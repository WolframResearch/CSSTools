(* ::Package:: *)

(* Wolfram Language Package *)

(* CSS 3.0 Tokenizer *)
(* We follow the level 3 syntax https://www.w3.org/TR/css-syntax-3/ where we can. *)
(* Some exceptions exist because StringSplit is not as judicious as reading character-by-character. *)
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
RE["url-unquoted"] = "(([^\"'()\n\f\r\t \\\\]|" ~~ RE["escape"] ~~ ")+)"; 

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

(* If WL can't render the character code, return javascript unicode code point FFFD *)
fromHexCharacterCode[x_String] :=
	With[{n = FromDigits[StringTrim @ StringDrop[x, 1], 16]},
		If[n === 0, 
			"\\uFFFD"
			,
			With[{code = Quiet @ FromCharacterCode @ n}, If[Head[code] === FromCharacterCode, "\\uFFFD", code]]]]
			
characterNormalizationRules = MapThread[RegularExpression[RE[#1]] :> #2 &, {CharacterRange["A", "Z"], CharacterRange["a", "z"]}];

CSSNormalizeEscapes[x_String] := 
	StringReplace[x, 
		{
			(* Leave most javascript code points untouched, but convert undisplayable to \uFFFD *)
			RegularExpression["\\\\[uU]((0000)|([fF][fF][fF][dD]))"] :> "\\uFFFD",
			s:RegularExpression["\\\\[uU][0-9a-fA-F]{4,4}"] :> s,
			(* convert CSS code points, if possible, otherwise return javascript code point \uFFFD *)
			s:RegularExpression["\\\\[0-9a-fA-F]{1,6}" ~~ RE["whitespace"] ~~ "?"] :> fromHexCharacterCode[s],
			(* normalize whitespace *)
			RegularExpression["\\\\[ \n\r\f]"] :> " ",
			(* convert single character escapes *)
			"\\" ~~ s:RegularExpression["[^\"]"] :> s,
			(* corner case: trailing escapes display as javascript code point \uFFFD *)
			"\\" -> "\\uFFFD"}]


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
		StringSplit[x, 
			{
				s:RegularExpression @ RE["comment"] :> Nothing(*CSSToken[<|"Type" -> "comment", "String" -> s|>]*),
				s:RegularExpression @ RE["at-keyword-token"] :> 
					CSSToken[<|
						"Type"      -> "at-keyword", 
						"String"    -> CSSNormalizeEscapes @ ToLowerCase @ StringDrop[s, 1],
						"RawString" -> StringDrop[s, 1]|>],
				s:RegularExpression @ RE["url-token"] :> 
					With[{v = stripURL @ s},
						CSSToken[<|
							"Type"      -> "url", 
							"String"    -> If[Last @ v === None, CSSNormalizeEscapes @ First @ v, First @ v], (* only normalize escapes if the URL was unquoted *)
							"RawString" -> First @ v,
							"Quotes"    -> Last @ v|>]],
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
						"Type"      -> "function", 
						"RawString" -> StringDrop[s, -1],
						"String"    -> CSSNormalizeEscapes @ ToLowerCase @ StringDrop[s, -1]|>],
				s:RegularExpression @ RE["hash-token"] :> 
					With[{v = idHash @ StringDrop[s, 1]},
						CSSToken[<|
							"Type"      -> "hash",
							"String"    -> CSSNormalizeEscapes @ First @ v, (* no ToLowerCase because HTML id tags are case-sensitive *)
							"RawString" -> First @ v,
							"Flag"      -> Last @ v|>]],
				s:RegularExpression @ RE["unicode-range-token"] :> 
					With[{range = First @ calculateUnicodeRange @ s},
						CSSToken[<|
							"Type" -> "unicode-range", 
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
						"Type"      -> "ident", 
						"String"    -> If[s == "\\", s, CSSNormalizeEscapes @ s],
						"RawString" -> s|>],
				
				(* scientific notation is tricky; it can look like a dimension but can also be part of a dimension *)
				num:RegularExpression[RE["numSCI"]] ~~ "%" :> 
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "percentage", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ n[[1]],
							"RawString" -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]]|>]],
				num:RegularExpression[RE["numSCI"]] ~~ unit:RegularExpression[RE["ident-token"]] :>	
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "dimension", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ n[[1]],
							"RawString" -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]],
							"Unit"      -> CSSNormalizeEscapes @ ToLowerCase @ unit,
							"RawUnit"   -> unit|>]],
				num:RegularExpression[RE["numSCI"]] :> 
					With[{n = tokenizeNumber @ num},
						CSSToken[<|
							"Type"      -> "number", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ n[[1]],
							"RawString" -> n[[1]],
							"Value"     -> n[[2]],
							"ValueType" -> n[[3]]|>]],
								
				s:RegularExpression @ RE["percentage-token"] :> 
					With[{p = tokenizePercentage @ s},
						CSSToken[<|
							"Type"      -> "percentage", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ p[[1]],
							"RawString" -> p[[1]],
							"Value"     -> p[[2]],
							"ValueType" -> p[[3]]|>]],
				s:RegularExpression @ RE["dimension-token"] :> 
					With[{d = tokenizeDimension @ s},
						CSSToken[<|
							"Type"      -> "dimension", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ d[[1]],
							"RawString" -> d[[1]],
							"Value"     -> d[[2]],
							"ValueType" -> d[[3]],
							"Unit"      -> CSSNormalizeEscapes @ ToLowerCase @ d[[4]],
							"RawUnit"   -> d[[4]]|>]],
				s:RegularExpression @ RE["num"] :> 
					With[{n = tokenizeNumber@ s},
						CSSToken[<|
							"Type"      -> "number", 
							"String"    -> CSSNormalizeEscapes @ ToLowerCase @ n[[1]],
							"RawString" -> n[[1]],
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
	If[StringMatchQ[x, RegularExpression["[+\\-]?[0-9]+"]], 
		{x, Round @ Internal`StringToDouble[x](*Interpreter["Integer"][x]*), "integer"}
		, 
		{x, Internal`StringToDouble[x](*Interpreter["Number"][x]*), "number"}]
		
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
	URL heads are an exception to the nesting. They indicate an improper URL.
	While in a bad URL, all tokens including " and ' are skipped until the bad-url is closed by a closing paren.
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
			pos = 1, l = Length[inputTokens], depth = 0, brackets, tokens = inputTokens, 
			inBadURL = False, inBadString = False, inWS = False
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
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos]], pos}; inBadURL = True];
					inWS = False,
				"function", 
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos]], pos}];
					inWS = False,
				"delim",
					If[TokenStringIs["\"" | "'", tokens[[pos]]],      
						If[!inBadString, inBadString = True];
						depth++; brackets[[depth]] = {tokens[[pos]], pos}];
					inWS = False,
				"newline", 
					If[inBadString, 
						inBadString = False;
						tokens[[brackets[[depth, 2]]]] = 
							CSSToken[<|
								"Type" -> "error", 
								"String" -> "bad-string", 
								"Children" -> tokens[[brackets[[depth, 2]] ;; pos]]|>];
						Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = {0, 0};
						depth--
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
							Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = {0, 0};
							depth--]];
					inWS = False,
				")", 
					If[inBadURL, 
						(* the closing of a bad URL also closes a bad string *)
						If[inBadString, 
							inBadString = False; 
							tokens[[brackets[[depth, 2]]]] = 
								CSSToken[<|
									"Type"     -> "error", 
									"String"   -> "bad-string", 
									"Children" -> tokens[[brackets[[depth, 2]] ;; pos]]|>];
							Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = {0, 0}; depth--];
						inBadURL = False;
						tokens[[brackets[[depth, 2]]]] = 
							CSSToken[<|
								"Type"     -> "error", 
								"String"   -> "bad-url", 
								"Children" -> tokens[[brackets[[depth, 2]] ;; pos]]|>];
						Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = {0, 0};
						depth--
						,
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
											"Type"      -> "function",
											"String"    -> CSSNormalizeEscapes @ ToLowerCase @ brackets[[depth, 1]]["String"],  
											"RawString" -> brackets[[depth, 1]]["String"],  
											"Children"  -> tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]|>]];
								Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
								brackets[[depth]] = {0, 0};
								depth--]]];
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
							Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = {0, 0};
							depth--]];
					inWS = False, 
				_, inWS = False];
			pos++];
		pos = l;
		
		(* 
			CSS allows open brackets a without matching closing bracket.
			Any remaining open brackets are closed with an assumed matching bracket at the very end.*)
		While[depth > 0,
			Which[
				inBadString, 
					inBadString = False; 
					With[{try = StringJoin[CSSUntokenize @ tokens[[brackets[[depth, 2]] ;; ]], CSSUntokenize @ tokens[[brackets[[depth, 2]]]]]}, 
						tokens[[brackets[[depth, 2]]]] = 
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
				inBadURL, 
					inBadURL = False; 
					With[{try = StringJoin[CSSUntokenize @ tokens[[brackets[[depth, 2]] ;; ]], ")"]}, 
						tokens[[brackets[[depth, 2]]]] = 
							If[StringMatchQ[try, RegularExpression[RE["url-token"]]], 
								With[{s = stripURL @ try}, 
									CSSToken[<|
										"Type"      -> "url", 
										"String"    -> If[Last @ s === None, CSSNormalizeEscapes @ First @ s, First @ s], (* only normalize escapes if the URL was unquoted *)
										"RawString" -> First @ s,
										"Quotes"    -> Last @ s|>]]
								,
								CSSToken[<|
									"Type"     -> "error", 
									"String"   -> "bad-url", 
									"Children" -> tokens[[brackets[[depth, 2]] ;; ]]|>]]],
				True,
					tokens[[brackets[[depth, 2]]]] = 
						Switch[brackets[[depth, 1]]["Type"], 
							"{", CSSToken[<|"Type" -> "{}", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>], 
							"(", CSSToken[<|"Type" -> "()", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>], 
							"[", CSSToken[<|"Type" -> "[]", "Children" -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>],
							"function", 
								CSSToken[<|
									"Type"      -> "function", 
									"String"    -> CSSNormalizeEscapes @ ToLowerCase @ tokens[[brackets[[depth, 2]]]]["String"], 
									"RawString" -> tokens[[brackets[[depth, 2]]]]["String"], 
									"Children"  -> tokens[[brackets[[depth, 2]] + 1 ;; ]]|>]]];
			Do[tokens[[i]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>], {i, brackets[[depth, 2]] + 1, pos, 1}];
			brackets[[depth]] = {0, 0};
			depth--];
		
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
	With[
		{
			t = token["Type"], rs = token["RawString"], s = token["String"],
			ru = token["RawUnit"], c = token["Children"]
		},
		Switch[t,
			"string",        With[{q = token["Quotes"]}, q <> s <> q],
			"function",      rs <> "(" <> Which[MissingQ[c], "", c === {}, ")", True, StringJoin[CSSUntokenize @ c, ")"]],
			"at-keyword",    "@" <> rs,
			"percentage",    rs <> "%",
			"dimension",     rs <> ru,
			"hash",          "#" <> rs,
			"{}",            "{" <> If[c === {}, "", CSSUntokenize @ c] <> "}",
			"()",            "(" <> If[c === {}, "", CSSUntokenize @ c] <> ")",
			"[]",            "[" <> If[c === {}, "", CSSUntokenize @ c] <> "]",
			"url",           With[{q = token["Quotes"]}, If[q === None, "url(" <> rs <> ")", "url(" <> q <> s <> q <> ")"]],
			"error",         Switch[s, "REMOVE", "", "bad-string" | "bad-url", CSSUntokenize @ c, _, s],
			"unicode-range", untokenizeUnicodeRange[a["Start"], a["Stop"]],
			_,               If[MissingQ[rs], s, rs] (* number, ident, newline, delim, colon, semicolon, {, }, (, ), [, ]*)
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

TokenStringIs[s_,    CSSToken[KeyValuePattern["String" -> t_?StringQ]]] :=  StringMatchQ[t, s, IgnoreCase -> True]
TokenStringIsNot[s_, CSSToken[KeyValuePattern["String" -> t_?StringQ]]] := !StringMatchQ[t, s, IgnoreCase -> True]
TokenStringIs[___] := False
TokenStringIsNot[___] := False

TokenUnitIs[s_,    CSSToken[KeyValuePattern["Unit" -> t_?StringQ]]] :=  StringMatchQ[t, s, IgnoreCase -> True]
TokenUnitIsNot[s_, CSSToken[KeyValuePattern["Unit" -> t_?StringQ]]] := !StringMatchQ[t, s, IgnoreCase -> True]
TokenUnitIs[___] := False
TokenUnitIsNot[___] := False

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