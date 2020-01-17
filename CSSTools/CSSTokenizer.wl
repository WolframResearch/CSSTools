(* ::Package:: *)

(* Wolfram Language Package *)

(* CSS 3.0 Tokenizer *)
(* 
	We follow the level 3 syntax https://www.w3.org/TR/css-syntax-3/ where we can.
	Some exceptions exist because WL's StringSplit is not as judicious as reading character-by-character.
	Every exception most likely leads to slower parsing speeds, but that's life.
	Exceptions include:
		+ determining nesting structures
			CSS tokenizing strictly leaves brackets e.g. {([ and other structures alone. We mostly follow
			this via the tokenizeFlat function. 
			CSSTokenize performs a subsequent step via nestTokens in that we determine the scope of each 
			blocking structure and create non-conformant block tokens of types "{}", "()", "[]", 
			"function", "bad-url", and "bad-string".
			Each block token contain a "Children" key. The children are the list of tokens within that
			scoping block. The child tokens can themselves include block tokens with their own children. 
			These block tokens make parsing to WL options much easier so this divergence from the CSS
			specification is justified given the intent of this package: the FE is not reading and using
			CSS in real time; we are importing static CSS to analyze its structure and translate to WL.
		+ scientific notation and regular numbers
			CSS lumps these two together in the same regular expression definition. We find that we
			must separate them for StringSplit to not accidentally see 1E1px as a 1E dimension token
			followed by a 1px dimension token; it should be a px dimension token with value 1E1. 
		+ escaped characters
			We added \\\\ to the regex for RE["escape"] to handle possible trailing escapes, especially
			corner cases of \\ at the end of the file being parsed. These corner cases also make the 
			nestTokens algorithm more complicated, calling removeEscapedEndOfFile in certain places.
			This has bloated the nestTokens function significantly, but it passes all tokenization tests.
		+ URL tokens
			Forthcoming syntax allows for URL modifiers e.g. url("path" identMod functionMod(a)).
			Though no CSS module uses this syntax yet, we're trying to future proof the tokenizer. Modifiers
			are only allowed in quoted URLs. This forces us to treat quoted and unquoted URLs differently.
			Moreover, a bad URL aggressively consumes all characters until an ending paren ) is reached.
			Thus in nestTokens we are forced to break apart any string token in quoted URLs if the string 
			token contains an unescaped ending paren. 
			Any URLs with detected modifiers are put into the "Modifiers" key of the URL token. 
			This has bloated the nestTokens function significantly, but it passes all tokenization tests.*)
			
(* This package is a utility package for the rest of CSSTools. *)

BeginPackage["CSSTools`CSSTokenizer`", {"GeneralUtilities`"}] 

SetUsage[RE,                  "RE[string$] returns the corresponding regular expression macro for CSS token patterns."];
SetUsage[CSSNormalizeEscapes, "CSSNormalizeEscapes[string$] converts CSS escapes \\ such as code points to characters."];
SetUsage[CSSToken,            "CSSToken[<|$$|>] represents a CSS token."];
SetUsage[CSSTokenQ,           "Returns True$ if the expression is a valid CSS token."];
SetUsage[CSSTokenize,         "CSSTokenize[string$] converts string$ into a list of CSS tokens."];
SetUsage[CSSUntokenize,       "CSSUntokenize[{CSSToken$...}] serializes CSS tokens into a string."];

SetUsage[TokenTypeIs,      "TokenTypeIs[string$, CSSToken$] gives True if the type of the CSS token matches string$."];
SetUsage[TokenTypeIsNot,   "TokenTypeIsNot[string$, CSSToken$] gives True if the type of the CSS token does not match string$."];
SetUsage[TokenStringIs,    "TokenStringIs[string$, CSSToken$] gives True if the string content of the CSS token matches string$; case is ignored and escape sequences normalized."];
SetUsage[TokenStringIsNot, "TokenStringIsNot[string$, CSSToken$] gives True if the string content of the CSS token does not match string$; case is ignored and escape sequences normalized."];
SetUsage[TokenUnitIs,      "TokenUnitIs[string$, CSSToken$] gives True if the unit content of the CSS token matches string$; case is ignored and escape sequences normalized."];
SetUsage[TokenUnitIsNot,   "TokenUnitIsNot[string$, CSSToken$] gives True if the unit content of the CSS token does not match string$; case is ignored and escape sequences normalized."];
SetUsage[TokenPatternString, "TokenPatternString[string$, type$] is a CSSToken pattern of CSSToken type type$ and the case-insensitive string string$."]

SetUsage[AdvancePosAndSkipWhitespace,      "AdvancePosAndSkipWhitespace[pos$, l$, CSSTokens$] increments pos$, then increments pos$ further if any whitespace tokens are detected."];
SetUsage[RetreatPosAndSkipWhitespace,      "RetreatPosAndSkipWhitespace[pos$, l$, CSSTokens$] decrements pos$, then decrements pos$ further if any whitespace tokens are detected."];
SetUsage[AdvancePosToNextDelimiter,        "AdvancePosToNextDelimiter[pos$, l$, CSSTokens$] increments pos$ until a delimiter CSS token is reached."];
SetUsage[AdvancePosToNextSemicolon,        "AdvancePosToNextSemicolon[pos$, l$, CSSTokens$] increments pos$ until a semicolon CSS token is reached."];
SetUsage[AdvancePosToNextSemicolonOrBlock, "AdvancePosToNextSemicolonOrBlock[pos$, l$, CSSTokens$] increments pos$ until a semicolon or block CSS token is reached."];
SetUsage[AdvancePosToNextSemicolonOrComma, "AdvancePosToNextSemicolonOrComma[pos$, l$, CSSTokens$] increments pos$ until a semicolon or comma CSS token is reached."];
SetUsage[AdvancePosToNextBlock,            "AdvancePosToNextBlock[pos$, l$, CSSTokens$] increments pos$ until a block CSS token is reached."];
SetUsage[TrimWhitespaceTokens,             "TrimWhitespaceTokens[pos$, l$, CSSTokens$] removes any whitespace CSS tokens from the ends of the CSS token sequence."]


Begin["`Private`"]

(* ::Section::Closed:: *)
(*CSS 3.0 Grammar via RegularExpression macros*)


(* Notes on strings in WL and regular expressions:
	WL strings use the backslash \ as the escape character. 
	WL string characters that require escaping consist of double quote and backslash i.e. "\
	REs are defined using strings and have their own additional rules:
		The complete list of characters that need to be escaped in a RE consists of .\?(){}[]^$*+|
		Inside a RE character class [...], the complete list of escaped characters is ^-\[]
	Special cases to watch out for:
		\\\\ defines a literal backslash (all backslashes must be escaped in RE and backslash is also the WL escape character)
		\\.  defines a literal period
	Though double quotes do not need to be escaped in the RE, WL strings still need to have the character escaped.
	Tabs and newlines are escaped in both WL and RE so do not need additional escaping, e.g. \t\r\n\f.
	CSS is case-insensitive, so A-Z ranges are included alongside a-z ranges, even though this is not in the specification.
	For clarity, all macros are surround by parenthesis to keep them isolated from other RE patterns.
*)

(* The following regular expressions are not in the spec, but useful for further definitions *)
(* Letter patterns for normalizing keywords and other idents. *)
RE["A"] = "(a|A|\\\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?|\\\\a|\\\\A)";
RE["B"] = "(b|B|\\\\0{0,4}(42|62)(\r\n|[ \t\r\n\f])?|\\\\b|\\\\B)";
RE["C"] = "(c|C|\\\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?|\\\\c|\\\\C)";
RE["D"] = "(d|D|\\\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?|\\\\d|\\\\D)";
RE["E"] = "(e|E|\\\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?|\\\\e|\\\\E)";
RE["F"] = "(f|F|\\\\0{0,4}(46|66)(\r\n|[ \t\r\n\f])?|\\\\f|\\\\F)";
RE["G"] = "(g|G|\\\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\\\g|\\\\G)";
RE["H"] = "(h|H|\\\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\\\h|\\\\H)";
RE["I"] = "(i|I|\\\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\\\i|\\\\I)";
RE["J"] = "(j|J|\\\\0{0,4}(4a|6a|4A|6A)(\r\n|[ \t\r\n\f])?|\\\\j|\\\\J)";
RE["K"] = "(k|K|\\\\0{0,4}(4b|6b|4B|6B)(\r\n|[ \t\r\n\f])?|\\\\k|\\\\K)";
RE["L"] = "(l|L|\\\\0{0,4}(4c|6c|4C|6C)(\r\n|[ \t\r\n\f])?|\\\\l|\\\\L)";
RE["M"] = "(m|M|\\\\0{0,4}(4d|6d|4D|6D)(\r\n|[ \t\r\n\f])?|\\\\m|\\\\M)";
RE["N"] = "(n|N|\\\\0{0,4}(4e|6e|4E|6E)(\r\n|[ \t\r\n\f])?|\\\\n|\\\\N)";
RE["O"] = "(o|O|\\\\0{0,4}(4f|6f|4F|6F)(\r\n|[ \t\r\n\f])?|\\\\o|\\\\O)";
RE["P"] = "(p|P|\\\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\\\p|\\\\P)";
RE["Q"] = "(q|Q|\\\\0{0,4}(51|71)(\r\n|[ \t\r\n\f])?|\\\\q|\\\\Q)";
RE["R"] = "(r|R|\\\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\\\r|\\\\R)";
RE["S"] = "(s|S|\\\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\\\s|\\\\S)";
RE["T"] = "(t|T|\\\\0{0,4}(44|74)(\r\n|[ \t\r\n\f])?|\\\\t|\\\\T)";
RE["U"] = "(u|U|\\\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\\\u|\\\\U)";
RE["V"] = "(v|V|\\\\0{0,4}(56|76)(\r\n|[ \t\r\n\f])?|\\\\v|\\\\V)";
RE["W"] = "(w|W|\\\\0{0,4}(57|77)(\r\n|[ \t\r\n\f])?|\\\\w|\\\\W)";
RE["X"] = "(x|X|\\\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\\\x|\\\\X)";
RE["Y"] = "(y|Y|\\\\0{0,4}(59|79)(\r\n|[ \t\r\n\f])?|\\\\y|\\\\Y)";
RE["Z"] = "(z|Z|\\\\0{0,4}(5a|7a|5A|7A)(\r\n|[ \t\r\n\f])?|\\\\z|\\\\Z)";

RE["identStart"]  = "((--)|(-?(([_a-zA-Z]|[^[:ascii:]])|" ~~ RE["escape"] ~~ ")))";    (* -- is from CSS Variables Module 1 *)
RE["identBody"]   = "((([_a-zA-Z0-9\\-]|[^[:ascii:]])|" ~~ RE["escape"] ~~ ")*)";  
RE["integerSCI"]  = "([+\\-]?[0-9]+[Ee][+\\-]?[0-9]+)"; 
RE["numberSCI"]   = "([+\\-]?[0-9]*\\.[0-9]+[Ee][+\\-]?[0-9]+)";
RE["numSCI"]      = "(" ~~ RE["numberSCI"] ~~ "|" ~~ RE["integerSCI"] ~~ ")"; (* pattern match against reals before integers *)
RE["integer"]     = "([\\-+]?[0-9]+)";
RE["number"]      = "([+\\-]?[0-9]*\\.[0-9]+)"; 
RE["num"]         = "(" ~~ RE["number"] ~~ "|" ~~ RE["integer"] ~~ ")";
RE["string1"]     = "(\"([^\n\r\f\"\\\\]|(\\\\" ~~ RE["newline"] ~~ ")|" ~~ RE["escape"] ~~ ")*\")"; 
RE["string2"]     = "('([^\n\r\f'\\\\]|(\\\\" ~~ RE["newline"] ~~ ")|" ~~ RE["escape"] ~~ ")*')";    
RE["uniRange"]    = "(" ~~ RE["hex digit"] ~~ "{1,6}-" ~~ RE["hex digit"] ~~ "{1,6})";
RE["uni"]         = "([0-9a-fA-F]{1,6})";
(* 
	According to CSS Fonts 3, only up to 5 trailing wildcards are allowed in a unicode-range token.
	If only wildcards are present, then the range is interpreted as if it has a leading zero 0?????. 
	However, the W3C validator allows for 6 wildcards (and no preceeding hex characters), so we do too. *)
RE["uni?"]        = "(([\\?]{1,6})|([0-9a-fA-F]{1,1}[\\?]{1,5})|([0-9a-fA-F]{2,2}[\\?]{1,4})|([0-9a-fA-F]{3,3}[\\?]{1,3})|([0-9a-fA-F]{4,4}[\\?]{1,2})|([0-9a-fA-F]{5,5}[\\?]{1,1}))";
RE["urlhead"]     = RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["ws*"];
RE["urlbody"]     = "(((" ~~ RE["url-unquoted"] ~~ "|" ~~ RE["string-token"] ~~ ")" ~~ RE["ws*"] ~~ ")?)";

(* The following regular expressions are in the spec *)
(* The RE["escape"] has an additional \\\\ pattern to catch dangling escapes; these later parse to \uFFFD *)
RE["comment"]      = "(/\\*[^*]*\\*+([^/*][^*]*\\*+)*/)";
RE["newline"]      = "(\n|\r\n|\r|\f)";
RE["whitespace"]   = "( |\t|" ~~ RE["newline"] ~~ ")";
RE["hex digit"]    = "([0-9a-fA-F])";
RE["escape"]       = "((\\\\((" ~~ RE["hex digit"] ~~ "{1,6}" ~~ RE["whitespace"] ~~ "?)|([^\n\r\f0-9a-fA-F])))|(\\\\))";
RE["ws*"]          = "(" ~~ RE["whitespace-token"] ~~ "*)";
RE["url-unquoted"] = "(([^\"'()\n\f\r\t \\\\]|" ~~ "(\\\\((" ~~ RE["hex digit"] ~~ "{1,6}" ~~ RE["whitespace"] ~~ "?)|([^\n\r\f0-9a-fA-F])))" ~~ ")+)"; 

(* 
	The following regular expression tokens are in the spec;
	The number-token is not used because of confusion between dimensions and scientific notation *)
RE["whitespace-token"]      = "(" ~~ RE["whitespace"] ~~ "+)";
RE["ident-token"]           = "(" ~~ RE["identStart"] ~~ RE["identBody"] ~~ ")";
RE["function-token"]        = "(" ~~ RE["ident-token"] ~~ "\\()";
RE["at-keyword-token"]      = "(@" ~~ RE["ident-token"] ~~ ")";
RE["hash-token"]            = "(#" ~~ "(([_a-zA-Z0-9\\-]|[^[:ascii:]])|" ~~ RE["escape"] ~~ ")+)";
RE["string-token"]          = "(" ~~ RE["string1"] ~~ "|" ~~ RE["string2"] ~~ ")";
RE["url-token"]             = "(" ~~ RE["urlhead"] ~~ RE["urlbody"] ~~ "\\))";
(*RE["number-token"]          = "(..........)";*)
RE["dimension-token"]       = "(" ~~ RE["num"] ~~ RE["ident-token"] ~~ ")";
RE["percentage-token"]      = "(" ~~ RE["num"] ~~ "%)";
RE["unicode-range-token"]   = "([uU]\\+" ~~ "(" ~~ RE["uniRange"] ~~ "|" ~~ RE["uni?"] ~~ "|" ~~ RE["uni"] ~~ ")" ~~ ")";
RE["include-match-token"]   = "~=";
RE["dash-match-token"]      = "\\|=";
RE["prefix-match-token"]    = "\\^=";
RE["suffix-match-token"]    = "\\$=";
RE["substring-match-token"] = "\\*=";
RE["column-token"]          = "\\|\\|";
RE["CDO-token"]             = "<!--";
RE["CDC-token"]             = "-->";

(* 
	CSS is case-insensitive, but some document types like XML are not.
	The main culprets are id and class names in CSS selectors.
	These normalizing functions remove escaped characters. Converting case is done separately if at all. *)

(* If WL can't render the character code, return unicode code point U+FFFD *)
fromHexCharacterCode[x_String] :=
	With[{n = FromDigits[StringTrim @ StringDrop[x, 1], 16]},
		If[n === 0, 
			"\[UnknownGlyph]"
			,
			With[{code = Quiet @ FromCharacterCode @ n}, 
				If[Head[code] === FromCharacterCode || (* surrogate pairs *)55296 <= n <= 57343 || (* maximum code point *)n > 1114111, 
					"\[UnknownGlyph]"
					, 
					code]]]]
					
replaceBadHexCharacterCode[x_String] :=
	With[{n = FromDigits[StringTrim @ StringDrop[x, 1], 16]},
		If[n === 0, 
			"\[UnknownGlyph]"
			,
			With[{code = Quiet @ FromCharacterCode @ n}, 
				If[Head[code] === FromCharacterCode || (* surrogate pairs *)55296 <= n <= 57343 || (* maximum code point *)n > 1114111, 
					"\[UnknownGlyph]"
					, 
					x]]]]
			
characterNormalizationRules = MapThread[RegularExpression[RE[#1]] :> #2 &, {CharacterRange["A", "Z"], CharacterRange["a", "z"]}];

CSSNormalizeEscapes[x_String] := 
	StringReplace[x, 
		{
			(* Leave most javascript code points untouched, but convert undisplayable to \uFFFD *)
			RegularExpression["\\\\[uU]((0000)|([fF][fF][fF][dD]))"] :> "\\uFFFD",
			s:RegularExpression["\\\\[uU][0-9a-fA-F]{4,4}"] :> s,
			(* convert CSS code points, if possible, otherwise return code point U+FFFD *)
			s:RegularExpression["\\\\[0-9a-fA-F]{1,6}" ~~ RE["whitespace"] ~~ "?"] :> fromHexCharacterCode[s],
			(* normalize whitespace *)
			RegularExpression["\\\\[ \n\r\f]"] :> " ",
			(* convert single character escapes *)
			"\\" ~~ s:RegularExpression["[^\"]"] :> s,
			(* corner case: trailing escapes display as code point U+FFFD *)
			"\\" -> "\[UnknownGlyph]"}]


(* ::Section:: *)
(*Tokenize String*)


(* ::Subsection:: *)
(*CSSTokenize, CSSTokenQ*)


CSSToken /: CSSTokenQ[token:CSSToken[_?AssociationQ]] := 
	MatchQ[token["Type"],  
		Alternatives[
			"string", "ident", "at-keyword", "hash", 
			"number" , "percentage", "dimension",
			"{", "}", "(", ")", "[", "]", 
			"function", "{}", "()", "[]",
			"comma", "colon", "semicolon",
			"unicode-range", "url", "urlhead", "newline", "error",
			"whitespace", "delim",
			"CDC", "CDO", "column",
			"dash-match", "prefix-match", "include-match", "suffix-match", "substring-match"]] 
CSSTokenQ[___] := False

CSSTokenize[x_String] := nestTokens @ tokenizeFlat @ x
 
 
(* ::Subsection::Closed:: *)
(*Token access*)


CSSToken[a_?AssociationQ][key_] := a[key] 


(* ::Subsection::Closed:: *)
(*Flat tokenizer*)


tokenizeFlat[x_String] := 
	Replace[
		StringSplit[StringReplace[x, re:RegularExpression["\\\\(" ~~ RE["hex digit"] ~~ "{1,6}" ~~ RE["whitespace"] ~~ "?)"] :> replaceBadHexCharacterCode[re]], 
			{
				s:RegularExpression @ RE["comment"] :> Nothing(*CSSToken[<|"Type" -> "comment", "String" -> s|>]*),
				s:RegularExpression @ RE["at-keyword-token"] :> 
					CSSToken[<|
						"Type"   -> "at-keyword", 
						"String" -> StringDrop[s, 1]|>],
				s:RegularExpression @ RE["url-token"] :> 
					With[{v = stripURL @ s},
						CSSToken[<|
							"Type"   -> "url", 
							"String" -> First @ v,
							"Quotes" -> Last @ v|>]],
				s:RegularExpression @ RE["urlhead"] :> 
					CSSToken[<|
						"Type"   -> "urlhead", 
						"String" -> "url("|>],
				s:RegularExpression @ RE["string-token"] :> 
					CSSToken[<|
						"Type"   -> "string", 
						"String" -> StringTake[s, {2, -2}],
						"Quotes" -> StringTake[s, 1]|>],
				s:RegularExpression @ RE["function-token"] :> 
					CSSToken[<|
						"Type"   -> "function", 
						"String" -> StringDrop[s, -1]|>],
				s:RegularExpression @ RE["hash-token"] :> 
					With[{v = idHash @ StringDrop[s, 1]},
						CSSToken[<|
							"Type"   -> "hash",
							"String" -> First @ v,
							"Flag"   -> Last @ v|>]],
				s:RegularExpression @ RE["unicode-range-token"] :> 
					With[{range = First @ calculateUnicodeRange @ s},
						CSSToken[<|
							"Type"  -> "unicode-range", 
							"Start" -> First @ range,
							"Stop"  -> Last @ range|>]],
							
				(* because of CSS Variables we need to catch this CDC token before parsing idents *)
				"-->" :> CSSToken[<|"Type" -> "CDC", "String" -> "-->"|>], 
				
				(* 
					It would be awesome if we could use ToLowerCase on the ident "String" key, but we can't.
					1. CSS variables are case-sensitive idents
					2. CSS selectors are case-insensitive, but may apply to case-sensitive documents. *)
				s:RegularExpression @ RE["ident-token"] :> 
					CSSToken[<|
						"Type"   -> "ident", 
						"String" -> s|>],
				
				(* scientific notation is tricky; it can look like a dimension but can also be part of a dimension *)
				num:RegularExpression[RE["numSCI"]] ~~ "%" :> 
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "percentage", 
							"String"    -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]]|>]],
				num:RegularExpression[RE["numSCI"]] ~~ unit:RegularExpression[RE["ident-token"]] :>	
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "dimension", 
							"String"    -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]],
							"Unit"      -> unit|>]],
				num:RegularExpression[RE["numSCI"]] :> 
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "number", 
							"String"    -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]]|>]],
								
				s:RegularExpression @ RE["percentage-token"] :> 
					With[{p = tokenizePercentage @ s},
						CSSToken[<|
							"Type"      -> "percentage", 
							"String"    -> p[[1]],
							"Value"     -> p[[2]],
							"ValueType" -> p[[3]]|>]],
				s:RegularExpression @ RE["dimension-token"] :> 
					With[{d = tokenizeDimension @ s},
						CSSToken[<|
							"Type"      -> "dimension", 
							"String"    -> d[[1]],
							"Value"     -> d[[2]],
							"ValueType" -> d[[3]],
							"Unit"      -> d[[4]]|>]],
				s:RegularExpression @ RE["num"] :> 
					With[{n = tokenizeNumber@ s},
						CSSToken[<|
							"Type"      -> "number", 
							"String"    -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]]|>]],
				
				"-->"  :> CSSToken[<|"Type" -> "CDC", "String" -> "-->"|>], 
				"<!--" :> CSSToken[<|"Type" -> "CDO", "String" -> "<!--"|>], 
				
				"||" :> CSSToken[<|"Type" -> "column", "String" -> "||"|>], 
				
				"|=" :> CSSToken[<|"Type" -> "dash-match",      "String" -> "|="|>], 
				"^=" :> CSSToken[<|"Type" -> "prefix-match",    "String" -> "^="|>], 
				"~=" :> CSSToken[<|"Type" -> "include-match",   "String" -> "~="|>], 
				"$=" :> CSSToken[<|"Type" -> "suffix-match",    "String" -> "$="|>], 
				"*=" :> CSSToken[<|"Type" -> "substring-match", "String" -> "*="|>], 
				
				"," :> CSSToken[<|"Type" -> "comma",     "String" -> ","|>], 
				":" :> CSSToken[<|"Type" -> "colon",     "String" -> ":"|>], 
				";" :> CSSToken[<|"Type" -> "semicolon", "String" -> ";"|>], 
				
				s:Alternatives["(", ")", "{", "}", "[", "]"] :> CSSToken[<|"Type" -> s, "String" -> s|>],
					
				(* not technically a token, but useful for finding blocks *)
				s:RegularExpression @ RE["newline"] :> CSSToken[<|"Type" -> "newline", "String" -> s|>],
				
				s:Whitespace :> CSSToken[<|"Type" -> "whitespace", "String" -> " "|>],
				s_ :> CSSToken[<|"Type" -> "delim", "String" -> s|>]
				
				
			}],
		{
			CSSToken[KeyValuePattern[{"Type" -> "ident", "String" -> "\\"}]] :> CSSToken[<|"Type" -> "delim", "String" -> "\\"|>],
			"" :> Nothing},
		{1}]


tokenizePercentage[x_String] := Flatten @ StringCases[x, num:RegularExpression[RE["num"]] ~~ unit:"%" :> tokenizeNumber @ num]

tokenizeDimension[x_String] := Flatten @ StringCases[x, num:RegularExpression[RE["num"]] ~~ unit___ :> {tokenizeNumber @ num, unit}]

Clear[tokenizeNumber]
tokenizeNumber[x_String] := 
	If[StringMatchQ[x, RegularExpression["[+\\-]?[0-9]+([Ee][+\\-]?[0-9]+)?"]], 
		{x, Round @ Internal`StringToDouble[x], "integer"}
		, 
		{x, Internal`StringToDouble[x], "number"}]
		
idHash[x_String] := 
	If[StringMatchQ[x, RegularExpression[RE["ident-token"]]] && !StringMatchQ[x, RegularExpression["[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6,6}|[0-9a-fA-F]{8,8}"]], 
		{x, "id"}
		,
		{x, "unrestricted"}]

calculateUnicodeRange[x_String] :=
	StringCases[x,
		{
			s1:RegularExpression["[a-fA-F0-9]{1,6}"] ~~ "-" ~~ s2:RegularExpression["[a-fA-F0-9]{1,6}"] :> {FromDigits[s1, 16], FromDigits[s2, 16]},
			s:RegularExpression["[a-fA-F0-9]{0,6}[\\?]{1,6}"] :> {FromDigits[StringReplace[s, "?" -> "0"], 16], FromDigits[StringReplace[s, "?" -> "F"], 16]},
			s:RegularExpression["[a-fA-F0-9]{1,6}"] :> {FromDigits[s, 16], FromDigits[s, 16]}}]
		
stripURL[x_String] := 
	With[{noURL = First[StringCases[x, RegularExpression[RE["urlhead"]] ~~ s__ ~~ ")" :> StringTrim @ s], ""]},
		If[StringMatchQ[noURL, RegularExpression[RE["string-token"]]], 
			{StringTake[noURL, {2, -2}], StringTake[noURL, 1]}
			, 
			{noURL, None}]] 
			
padWithEndingParen[tokens:{__?CSSTokenQ}] := Append[tokens, CSSToken[<|"Type" -> "delim", "String" -> ")"|>]]

parseURLwithModifier[{t_?CSSTokenQ}, inQuotedURL:(True | False)] /; TokenTypeIs["url", t] := t 

parseURLwithModifier[inputTokens:{__?CSSTokenQ}, inQuotedURL:(True | False)] :=
	Module[{pos = 1, l, tokens = inputTokens, quote, string, modifierStart},
		(* first remove any tokens flagged for removal *)
		tokens = DeleteCases[tokens, CSSToken[KeyValuePattern[{"Type" -> "error", "String" -> "REMOVE" | "extra-ws"}]],	Infinity];
		(* if any other error tokens exist, exit as generic bad-url *)
		If[Cases[tokens, CSSToken[KeyValuePattern[{"Type" -> "error"}]]] =!= {},
			Return @ CSSToken[<|"Type" -> "error", "String" -> "bad-url", "Children" -> padWithEndingParen @ tokens|>]				
		];
		(* if not in a quoted URL then you can't be a modified URL *)
		If[!inQuotedURL, Return @ CSSToken[<|"Type" -> "error", "String" -> "bad-url", "Children" -> padWithEndingParen @ tokens|>]];
		
		(* parse after performing pre-checks *)
		l = Length[tokens];
		TrimWhitespaceTokens[pos, l, tokens];
		
		(* first token must be a string *)
		If[TokenTypeIs["string", tokens[[pos]]],
			string = tokens[[pos]]["String"];
			quote = tokens[[pos]]["Quotes"];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Return @ CSSToken[<|"Type" -> "error", "String" -> "bad-url", "Children" -> padWithEndingParen @ tokens[[pos ;; l]]|>]
		];
		(* remaining tokens must be idents or functions *)
		If[pos > l, Return @ CSSToken[<|"Type" -> "url", "String" -> string,"Quotes" -> quote|>]];
		modifierStart = pos;
		While[pos < l,
			If[TokenTypeIs["ident" | "function", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Return @ CSSToken[<|"Type" -> "error", "String" -> "bad-url", "Children" -> padWithEndingParen @ tokens[[pos ;; l]]|>]
			]
		];
		CSSToken[<|"Type" -> "url", "String" -> string, "Quotes" -> quote, "Modifiers" -> tokens[[modifierStart ;; l]]|>]	
	]

Attributes[markTokensForRemoval] = {HoldAll};
markTokensForRemoval[pos_, tokens_, brackets_, depth_] :=
	Module[{},
		Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
		brackets[[depth]] = {0, 0};
		depth--
	]
	
removeEscapedEndOfFile[CSSToken[kvp:KeyValuePattern[{"String" -> x_, "Unit" -> u_}]]] :=
	CSSToken[<|kvp, "Unit" -> StringReplace[x, "\\" ~~ EndOfString -> "\[UnknownGlyph]"]|>]

removeEscapedEndOfFile[CSSToken[kvp:KeyValuePattern[{"String" -> x_}]]] :=
	CSSToken[<|kvp, "String" -> StringReplace[x, "\\" ~~ EndOfString -> "\[UnknownGlyph]"]|>]
	


(* ::Subsection::Closed:: *)
(*Nest tokens*)


(* 
	The main nesting algorithm tracks the depth of the nesting, increasing with every open bracket.
	The location and type of the open brackets are tracked.
	Every closing bracket checks that it belongs to a matching open bracket.
	If it does not match, then the bracket is in error; 
	else it is a match and 
		1. group all tokens into a "block" token with appropriate label at the open bracket position;
		2. mark tokens for removal that are not at the open bracket positions since they are now in the block token
	The tokens are marked for removal as opposed to immediate removal because we must maintain the same
	length of tokens.
	This algorithm scans the token list only once.
	
	:: URLs ::
	URL heads are an exception to the nesting. They indicate a possible improper URL.
	It is possible that the URL has a modifier function (one or more <function-token> or <ident> instances) with a closing paren.
	Only quoted URLs can have such modifier functions; unquoted URLs cannot. 
	In a questionable URL, all tokens including " and ' are skipped until the questionable-url is closed by a closing paren.
	Once closed, the url must be checked for being OK in regards to one or more url-modifier tokens.
	A corner case is if the URL is unclosed at the end, an implicit closing paren is added; 
	this ending URL must be rechecked whether it is a valid URL format.
	
	:: strings ::
	Strings are another exception to the nesting. Isolated " and ' tokens indicate an improper string.
	While in a bad string, all tokens are skipped until a new line token is found.
	A corner case is if the string is unclosed at the end, an implicit closing quote is added;
	this ending string must be rechecked whether it is a valid string format. 
	
	:: comments ::
	Comments like /**/ are another candidate for an exception to the nesting. If an isolated /* is found,
	then it suggests a closing comment */ is missing. The CSS validator https://jigsaw.w3.org/css-validator/validator
	does not allow unclosed comments (nor does the CSS specification) so neither do we. *)		
nestTokens[inputTokens:{__?CSSTokenQ}] :=
	Module[
		{
			pos = 1, l = Length[inputTokens], depth = 0, brackets, tokens = inputTokens, newTokens,
			inBadURL = False, inBadString = False, inWS = False, inQuotedURL = False
		},
		(* The upper limit of nestings is the number of open brackets and quotation marks *)
		brackets = 
			ConstantArray[
				{0, 0}, 
				Count[
					tokens, 
					CSSToken[
						Alternatives[
							KeyValuePattern[{"Type" -> "delim", "String" -> "\"" | "'"}], 
							KeyValuePattern[{"Type" -> "function" | "urlhead" | "{" | "[" | "("}]]]]];
		
		While[pos <= l,
			Switch[tokens[[pos]]["Type"],
				"{" | "[" | "(", 
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos]], pos}];
					inWS = False,
				"urlhead",  
					If[!inBadString && !inBadURL, 
						depth++; brackets[[depth]] = {tokens[[pos]], pos}; inBadURL = True;
						(* peak ahead to see if in a quoted URL *)
						inQuotedURL = 
							And[
								pos < l,
								Or[
									TokenTypeIs["delim", tokens[[pos + 1]]] && TokenStringIs["\"" | "'", tokens[[pos + 1]]],
									TokenTypeIs["string", tokens[[pos + 1]]]]]];
					inWS = False,
				"string",
					(* in the case of a bad URL, strings need to be broken down into smaller tokens since an ending paren has higher precedence *)
					(* also closing a bad URL closes any bad string, too *)
					If[inBadURL && StringContainsQ[tokens[[pos]]["String"], RegularExpression["[^\\\\]\\)"]],
						newTokens = tokenizeFlat @ StringDrop[CSSUntokenize[tokens[[pos ;; ]]], 1]; (* make the quote from the start of the string into a delim token *)
						tokens = 
							Join[
								tokens[[ ;; pos - 1]], 
								{CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], CSSToken[<|"Type" -> "delim", "String" -> tokens[[pos]]["Quotes"]|>]}, 
								newTokens];
						l = Length[tokens];
						brackets = Join[brackets, {{0, 0}, {0, 0}}]];
					inWS = False,
				"function", 
					If[!inBadString && Not[inBadURL && !inQuotedURL], depth++; brackets[[depth]] = {tokens[[pos]], pos}]; 
					inWS = False,
				"delim",
					If[TokenStringIs["\"" | "'", tokens[[pos]]],      
						If[!inBadString, inBadString = True;
						depth++; brackets[[depth]] = {tokens[[pos]], pos}]];
					inWS = False,
				"newline", 
					If[inBadString, 
						inBadString = False;
						inQuotedURL = False;
						tokens[[brackets[[depth, 2]]]] = 
							CSSToken[<|
								"Type" -> "error", 
								"String" -> "bad-string", 
								"Children" -> tokens[[brackets[[depth, 2]] ;; pos]]|>];
						markTokensForRemoval[pos, tokens, brackets, depth]
						,
						If[!inBadURL, 
							If[inWS, 
								tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> "extra-ws"|>]
								, 
								inWS = True;
								tokens[[pos]] = CSSToken[<|"Type" -> "whitespace", "String" -> " "|>]]]],
				"whitespace", 
					If[inWS, tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> "extra-ws"|>], inWS = True],
				"}",
					If[inBadString || inBadURL, 
						Null
						,
						If[depth == 0 || TokenTypeIsNot["{", brackets[[depth, 1]]],
							tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> tokens[[pos]]["String"]|>]
							,
							tokens[[brackets[[depth, 2]]]] = CSSToken[<|"Type" -> "{}", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>];
							markTokensForRemoval[pos, tokens, brackets, depth]]];
					inWS = False,
				")", 
					If[inBadURL, 
						(* CLOSING PAREN: the closing of a bad URL also closes a bad string *)
						If[inBadString, 
							(* close bad string first *)
							inBadString = False; 
							tokens[[brackets[[depth, 2]]]] = 
								CSSToken[<|
									"Type"     -> "error", 
									"String"   -> "bad-string", 
									"Children" -> tokens[[brackets[[depth, 2]] ;; pos - 1]]|>];
							markTokensForRemoval[pos, tokens, brackets, depth];
							(* then also close bad URL *)
							inBadURL = False;
							tokens[[brackets[[depth, 2]]]] = 
								CSSToken[<|
									"Type"     -> "error", 
									"String"   -> "bad-url", 
									"Children" -> padWithEndingParen @ tokens[[brackets[[depth, 2]] + 1 ;; pos]]|>];
							markTokensForRemoval[pos, tokens, brackets, depth]
							, 
							(* CLOSING PAREN: if not in a bad string, then you might be in a function-token or legitimate closing of a modified URL *)
							Which[
								depth > 0 && TokenTypeIs["function", brackets[[depth, 1]]],
									tokens[[brackets[[depth, 2]]]] =
										CSSToken[<|
											"Type"      -> "function",
											"String"    -> brackets[[depth, 1]]["String"],  
											"Children"  -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>];
									markTokensForRemoval[pos, tokens, brackets, depth],
								depth > 0 && TokenTypeIs["urlhead", brackets[[depth, 1]]],
									inBadURL = False;
									tokens[[brackets[[depth, 2]]]] = parseURLwithModifier[tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]], inQuotedURL];
									inQuotedURL = False;
									markTokensForRemoval[pos, tokens, brackets, depth],
								True,
									tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> tokens[[pos]]["String"]|>]
							];
						];
						, (* CLOSING PAREN: not in a bad url, but still could be in a bad string *)
						If[!inBadString,
							If[depth == 0 || TokenTypeIsNot["function" | "(", brackets[[depth, 1]]], 
								tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> tokens[[pos]]["String"]|>]
								,
								tokens[[brackets[[depth, 2]]]] =
									If[TokenTypeIs["(", brackets[[depth, 1]]], 
										CSSToken[<|
											"Type"     -> "()", 
											"Children" -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>]
										,
										CSSToken[<|
											"Type"     -> "function",
											"String"   -> brackets[[depth, 1]]["String"],  
											"Children" -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>]];
								markTokensForRemoval[pos, tokens, brackets, depth]]]
					];
					inWS = False, 
				"]",  
					If[inBadString || inBadURL,
						Null
						,
						If[depth == 0 || TokenTypeIsNot["[", brackets[[depth, 1]]], 
							tokens[[pos]] = CSSToken[<|"Type" -> "error", "String" -> tokens[[pos]]["String"]|>]
							, 
							tokens[[brackets[[depth, 2]]]] = 
								CSSToken[<|
									"Type"     -> "[]",
									"Children" -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>]; 
							markTokensForRemoval[pos, tokens, brackets, depth]]];
					inWS = False, 
				_, inWS = False];
			pos++];
		pos = l;
		
		(* 
			CSS allows open brackets without a matching closing bracket.
			Any remaining open brackets are closed with an assumed matching bracket at the very end.
			The corner case of an escape at the end of the file \\<EOF> needs to be made an \[UnknownGlyph] unless it's within a string. *)
		While[depth > 0,
			tokens[[brackets[[depth, 2]]]] = 
				Which[
					inBadString, 
						inBadString = False; 
						(* try adding a matching quote to the end and see if it is a valid string in this case *)
						With[{try = StringJoin[CSSUntokenize @ tokens[[brackets[[depth, 2]] ;; ]], CSSUntokenize @ tokens[[brackets[[depth, 2]]]]]}, 
							If[StringMatchQ[try, RegularExpression[RE["string-token"]]], 
								CSSToken[<|
									"Type"   -> "string", 
									"String" -> StringTake[try, {2, -2}], 
									"Quotes" -> StringTake[try, 1]|>]
								,
								CSSToken[<|
									"Type"     -> "error", 
									"String"   -> "bad-string", 
									"Children" -> tokens[[brackets[[depth, 2]] ;; ]]|>]]],
					brackets[[depth, 1]]["Type"] == "function",
						If[TokenTypeIsNot["string", tokens[[-1]]] && StringEndsQ[tokens[[-1]]["String"], "\\"], 
							tokens[[-1]] = removeEscapedEndOfFile[tokens[[-1]]]];
						CSSToken[<|
							"Type"     -> "function", 
							"String"   -> tokens[[brackets[[depth, 2]]]]["String"], 
							"Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>],
					inBadURL, 
						inBadURL = False; 
						Block[{try = CSSUntokenize @ tokens[[brackets[[depth, 2]] + 1 ;; ]]},
							If[StringEndsQ[try, "\\"], 
								CSSToken[<|"Type" -> "error", "String" -> "bad-url", "Children" -> CSSTokenize @ try|>]
								, 
								try = StringJoin["url(", try, ")"];
								Which[
									StringMatchQ[try, RegularExpression[RE["url-token"]]], 
										With[{s = stripURL @ try}, CSSToken[<|"Type" -> "url", "String" -> First @ s, "Quotes" -> Last @ s|>]]
									,
									True,
										parseURLwithModifier[CSSTokenize @ try, inQuotedURL]]]],
					True,
						If[TokenTypeIsNot["string", tokens[[-1]]] && StringEndsQ[tokens[[-1]]["String"], "\\"], 
							tokens[[-1]] = removeEscapedEndOfFile[tokens[[-1]]]];
						Switch[brackets[[depth, 1]]["Type"], 
							"{", CSSToken[<|"Type" -> "{}", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>], 
							"(", CSSToken[<|"Type" -> "()", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>], 
							"[", CSSToken[<|"Type" -> "[]", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>]]];
			markTokensForRemoval[pos, tokens, brackets, depth]];
		
		(* very odd corner case where hash token is flagged as "id" type if it ends with an escaped EOF *)
		If[TokenTypeIs["hash", tokens[[-1]]] && StringEndsQ[tokens[[-1]]["String"], "\\"], 
			tokens[[-1]] = 
				Replace[
					tokens[[-1]], 
					CSSToken[kvp:KeyValuePattern[{"String" -> x_}]] :> CSSToken[<|kvp, "Flag" -> "unrestricted", "String" -> StringReplace[x, "\\" ~~ EndOfString -> "\[UnknownGlyph]"]|>]]];
		(* very odd corner case where dimension would have "\" as the unit if parser ends with an escaped EOF *)
		If[TokenTypeIs["dimension", tokens[[-1]]] && StringEndsQ[tokens[[-1]]["Unit"], "\\"], 
			tokens[[-1]] = 
				Replace[
					tokens[[-1]], 
					CSSToken[kvp:KeyValuePattern[{"Unit" -> x_}]] :> CSSToken[<|kvp, "Unit" -> StringReplace[x, "\\" ~~ EndOfString -> "\[UnknownGlyph]"]|>]]];
		If[TokenTypeIsNot["string" | "error" | "unicode-range", tokens[[-1]]] && StringEndsQ[tokens[[-1]]["String"], "\\"], 
			tokens[[-1]] = removeEscapedEndOfFile[tokens[[-1]]]];
		
		(* remove all tokens that were marked for removal *)	
		DeleteCases[
			tokens, 
			CSSToken[KeyValuePattern[{"Type" -> "error", "String" -> "REMOVE" | "extra-ws"}]], 
			Infinity]
	]
nestTokens[{}] := {}


(* ::Section:: *)
(*Untokenize*)


(*
	It may be useful to return to the original string from a set of CSS tokens.
	This is not entirely possible as some tokens are normalized during tokenization:
		+ errors like bad strings and bad urls are removed
		+ whitespace is normalized to a single space 
		+ originally unclosed quotes and blocks have closings *)

CSSUntokenize[{}] := ""
CSSUntokenize[tokens:{___?CSSTokenQ}] := StringJoin[Table[untokenize[tokens[[i]], tokens[[i+1]]], {i, Length[tokens] - 1}], untokenize @ tokens[[-1]]]
CSSUntokenize[token_?CSSTokenQ] := untokenize @ token

untokenize[___] := Failure["BadToken", <|"Message" -> "Unrecognized CSS token."|>]

CSSToken /: untokenize[token:CSSToken[a_?AssociationQ]] := 
	With[{t = token["Type"], s = token["String"], u = token["Unit"], c = token["Children"]},
		Switch[t,
			"string",        With[{q = token["Quotes"]}, q <> s <> q],
			"function",      s <> "(" <> Which[MissingQ[c], "", c === {}, ")", True, StringJoin[CSSUntokenize @ c, ")"]],
			"at-keyword",    "@" <> s,
			"percentage",    s <> "%",
			"dimension",     s <> u,
			"hash",          "#" <> s,
			"{}",            "{" <> If[c === {}, "", CSSUntokenize @ c] <> "}",
			"()",            "(" <> If[c === {}, "", CSSUntokenize @ c] <> ")",
			"[]",            "[" <> If[c === {}, "", CSSUntokenize @ c] <> "]",
			"url",           With[{q = token["Quotes"]}, If[q === None, "url(" <> s <> ")", "url(" <> q <> s <> q <> ")"]],
			"error",         Switch[s, "REMOVE", "", "bad-string", CSSUntokenize @ c, "bad-url", "url(" <> CSSUntokenize @ c, _, s],
			"unicode-range", untokenizeUnicodeRange[a["Start"], a["Stop"]],
			_,               If[MissingQ[s], s, s] (* number, ident, newline, delim, colon, semicolon, {, }, (, ), [, ]*)
		]
	]
	

(* Some sequences of tokens must serialize with empty comments in between in order to unambiguate the result. *)
untokenize[first_?CSSTokenQ, second_?CSSTokenQ] :=
	Switch[first["Type"],
		"ident",      
			If[
				Or[
					TokenTypeIs["ident" | "function" | "url" | "number" | "percentage" | "dimension" | "CDC" | "()", second],
					TokenTypeIs["delim", second] && TokenStringIs["-", second]],
				{untokenize @ first, "/**/"}
				,
				untokenize @ first],
		"at-keyword" | "hash" | "dimension", 
			If[
				Or[
					TokenTypeIs["ident" | "function" | "url" | "number" | "percentage" | "dimension" | "CDC", second],
					TokenTypeIs["delim", second] && TokenStringIs["-", second]],
				{untokenize @ first, "/**/"}
				,
				untokenize @ first],
		"number",
			If[
				Or[
					TokenTypeIs["ident" | "function" | "url" | "number" | "percentage" | "dimension", second],
					TokenTypeIs["delim", second] && TokenStringIs["%", second]],
				{untokenize @ first, "/**/"}
				,
				untokenize @ first],
		"delim",
			Switch[first["String"],
				"#" | "-", 
					If[
						Or[
							TokenTypeIs["ident" | "function" | "url" | "number" | "percentage" | "dimension", second],
							TokenTypeIs["delim", second] && TokenStringIs["-", second]],
						{untokenize @ first, "/**/"}
						,
						untokenize @ first],
				"@", 
					If[
						Or[
							TokenTypeIs["ident" | "function" | "url", second],
							TokenTypeIs["delim", second] && TokenStringIs["-", second]],
						{untokenize @ first, "/**/"}
						,
						untokenize @ first],
				"^" | "~" | "$" | "*", 
					If[TokenTypeIs["delim", second] && TokenStringIs["=", second], 
						{untokenize @ first, "/**/"}
						,
						untokenize @ first],
				"|", 
					If[TokenTypeIs["delim", second] && TokenStringIs["=" | "|", second], 
						{untokenize @ first, "/**/"}
						,
						untokenize @ first],
				"." | "+", 
					If[TokenTypeIs["number" | "percentage" | "dimension", second], 
						{untokenize @ first, "/**/"}
						, 
						untokenize @ first],
				"/",
					If[TokenTypeIs["delim", second] && TokenStringIs["*", second], 
						{untokenize @ first, "/**/"}
						, 
						untokenize @ first],
				_, If[StringContainsQ[first["String"], "\\"], {untokenize @ first, "\n"}, untokenize @ first]
			],
		_, untokenize @ first	
	]


untokenizeUnicodeRange[start_?NumericQ, stop_?NumericQ] :=
	Module[{t, startString, stopString, l},
		t = IntegerString[#, 16]& /@ {start, stop};
		{startString, stopString} = StringPadLeft[#, Max[StringLength /@ t], "0"] & /@ t;
		l = Intersection[StringPosition[startString, "0"], StringPosition[stopString, "f"]];
		Which[
			Length[l] > 0,                    "u+" <> StringReplacePart[startString, "?", l],
			TrueQ[startString == stopString], "u+" <> startString,
			True,                             "u+" <> startString <> "-" <> stopString]]


(* ::Subsection::Closed:: *)
(*Utilities*)


(* 
	The token string and unit utilities are necessary because the tokenizer does not convert any escape sequences. 
	These utilities perform a string match test on a "normalized" CSS token string. 
	The CSSToken "Type" is created by this package so does not require normalization. *)
TokenTypeIs[s_,    CSSToken[KeyValuePattern["Type" -> t_?StringQ]]] :=  StringMatchQ[t, s, IgnoreCase -> False]
TokenTypeIsNot[s_, CSSToken[KeyValuePattern["Type" -> t_?StringQ]]] := !StringMatchQ[t, s, IgnoreCase -> False]
TokenTypeIs[___] := False
TokenTypeIsNot[___] := False

TokenStringIs[s_,    CSSToken[KeyValuePattern["String" -> t_?StringQ]]] :=  StringMatchQ[CSSNormalizeEscapes @ t, s, IgnoreCase -> True]
TokenStringIsNot[s_, CSSToken[KeyValuePattern["String" -> t_?StringQ]]] := !StringMatchQ[CSSNormalizeEscapes @ t, s, IgnoreCase -> True]
TokenStringIs[___] := False
TokenStringIsNot[___] := False

TokenUnitIs[s_,    CSSToken[KeyValuePattern["Unit" -> t_?StringQ]]] :=  StringMatchQ[CSSNormalizeEscapes @ t, s, IgnoreCase -> True]
TokenUnitIsNot[s_, CSSToken[KeyValuePattern["Unit" -> t_?StringQ]]] := !StringMatchQ[CSSNormalizeEscapes @ t, s, IgnoreCase -> True]
TokenUnitIs[___] := False
TokenUnitIsNot[___] := False

TokenPatternString[s_String?StringQ, type_] := CSSToken[KeyValuePattern[{"Type" -> type, "String" -> _String?(StringQ[#] && StringMatchQ[CSSNormalizeEscapes @ #, s, IgnoreCase -> True]&)}]]

(* 
	The utilities are assumed to be used within "consume" functions where pos, l, and tokens are defined. 
	They are expected to run quickly so do no type checking. *)
SetAttributes[
	{
		AdvancePosAndSkipWhitespace, RetreatPosAndSkipWhitespace, AdvancePosToNextDelimiter,
		AdvancePosToNextSemicolon, AdvancePosToNextSemicolonOrBlock, AdvancePosToNextSemicolonOrComma,
		AdvancePosToNextBlock}, 
	HoldFirst];

AdvancePosAndSkipWhitespace[pos_, l_, tokens_] := (pos++; While[pos < l && TokenTypeIs["whitespace", tokens[[pos]]], pos++])
RetreatPosAndSkipWhitespace[pos_, l_, tokens_] := (pos--; While[pos > 1 && TokenTypeIs["whitespace", tokens[[pos]]], pos--])

AdvancePosToNextDelimiter[pos_, l_, tokens_] :=        While[pos < l && TokenTypeIsNot["delim",               tokens[[pos]]], pos++]
AdvancePosToNextSemicolon[pos_, l_, tokens_] :=        While[pos < l && TokenTypeIsNot["semicolon",           tokens[[pos]]], pos++]
AdvancePosToNextSemicolonOrBlock[pos_, l_, tokens_] := While[pos < l && TokenTypeIsNot["{}" | "semicolon",    tokens[[pos]]], pos++]
AdvancePosToNextSemicolonOrComma[pos_, l_, tokens_] := While[pos < l && TokenTypeIsNot["comma" | "semicolon", tokens[[pos]]], pos++]

AdvancePosToNextBlock[pos_, l_, tokens_] := While[pos < l && !MatchQ[tokens[[pos]]["Type"], "{}"], pos++]


SetAttributes[TrimWhitespaceTokens, HoldAll];
(* Adjust 'pos' and 'l' such that whitespace tokens are effectively trimmed. *)
TrimWhitespaceTokens[pos_, l_, tokens_] := (
	pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
	pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];)

(* ::Section:: *)
(*Package End*)


End[];
EndPackage[];