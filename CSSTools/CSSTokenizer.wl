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
SetUsage[CSSTokenType,        "CSSTokenType[CSSToken$] returns the type of CSSToken$."];
SetUsage[CSSTokenString,      "CSSTokenString[CSSToken$] returns the main string of the CSSToken$."];
SetUsage[CSSTokenValue,       "CSSTokenValue[CSSToken$] returns the interpreted numeric value of the numeric CSSToken$."];
SetUsage[CSSTokenValueType,   "CSSTokenValueType[CSSToken$] returns the type of the CSSToken$ value e.g. \"number\" or \"integer\"."];
SetUsage[CSSTokenUnit,        "CSSTokenUnit[CSSToken$] returns the dimension of a dimension CSSToken$ e.g. \"em\" or \"px\"."];
SetUsage[CSSTokenChildren,    "CSSTokenChildren[CSSToken$] returns the CSS tokens nested within CSSToken$."];

SetUsage[TokenTypeIs,      "TokenTypeIs[string$, pos$, {CSSToken$$}] gives True if the type of the CSS token at position pos$ matches string$."];
SetUsage[TokenTypeIsNot,   "TokenTypeIsNot[string$, pos$, {CSSToken$$}] gives True if the type of the CSS token at position pos$ does not match string$."];
SetUsage[TokenStringIs,    "TokenStringIs[string$, pos$, {CSSToken$$}] gives True if the string content of the CSS token at position pos$ matches string$; case is ignored."];
SetUsage[TokenStringIsNot, "TokenStringIsNot[string$, pos$, {CSSToken$$}] gives True if the string content of the CSS token at position pos$ does not match string$; case is ignored."];

SetUsage[AdvancePosAndSkipWhitespace,      "AdvancePosAndSkipWhitespace[pos$, l$, CSSTokens$] increments pos$, then increments pos$ further if any whitespace tokens are detected."];
SetUsage[RetreatPosAndSkipWhitespace,      "RetreatPosAndSkipWhitespace[pos$, l$, CSSTokens$] decrements pos$, then decrements pos$ further if any whitespace tokens are detected."];
SetUsage[AdvancePosToNextSemicolon,        "AdvancePosToNextSemicolon[pos$, l$, CSSTokens$] increments pos$ until a semicolon CSS token is reached."];
SetUsage[AdvancePosToNextSemicolonOrBlock, "AdvancePosToNextSemicolonOrBlock[pos$, l$, CSSTokens$] increments pos$ until a semicolon or block CSS token is reached."];
SetUsage[AdvancePosToNextSemicolonOrComma, "AdvancePosToNextSemicolonOrComma[pos$, l$, CSSTokens$] increments pos$ until a semicolon or comma CSS token is reached."];
SetUsage[AdvancePosToNextBlock,            "AdvancePosToNextBlock[pos$, l$, CSSTokens$] increments pos$ until a block CSS token is reached."];


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

RE["identStart"]  = "((--)|(-?(([_a-zA-Z]|[^[:ascii:]])|" ~~ RE["escape"] ~~ ")))";    (* -- is from CSS Variables module *)
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


CSSTokenQ[x_] := 
	MatchQ[x, 
		Alternatives[
			_?StringQ,
			{"string"|"ident"|"at-keyword", _?StringQ},
			{"function", _?StringQ, ___?CSSTokenQ},
			{"number"|"percentage", _?StringQ, _?NumericQ, "number"|"integer"},
			{"dimension", _?StringQ, _?NumericQ, "number"|"integer", _?StringQ},
			{"hash", _?StringQ, "id"|"unrestricted"},
			{"{}"|"()"|"[]", ___?CSSTokenQ},
			{"unicode-range", _?NumericQ, _?NumericQ},
			{"url", _?StringQ},
			{"newline", "\n" | "\r\n" | "\r" | "\f"},
			{"error", _}]] 

CSSTokenize[x_String] := nestTokens @ tokenizeFlat @ x


(* ::Subsection::Closed:: *)
(*Token access*)


CSSToken /: CSSTokenType[     CSSToken[a_?AssociationQ]] := a["Type"]
CSSToken /: CSSTokenString[   CSSToken[a_?AssociationQ]] := a["String"]
CSSToken /: CSSTokenValue[    CSSToken[a_?AssociationQ]] := If[KeyExistsQ[a, "Value"],     a["Value"],     None]
CSSToken /: CSSTokenValueType[CSSToken[a_?AssociationQ]] := If[KeyExistsQ[a, "ValueType"], a["ValueType"], None]
CSSToken /: CSSTokenUnit[     CSSToken[a_?AssociationQ]] := If[KeyExistsQ[a, "Unit"],      a["Unit"],      None]
CSSToken /: CSSTokenChildren[ CSSToken[a_?AssociationQ]] := If[KeyExistsQ[a, "Children"],  a["Children"],  None]


(* ::Subsection::Closed:: *)
(*Flat tokenizer*)


tokenizeFlat[x_String] := 
	Replace[
		StringSplit[x, 
			{
				s:RegularExpression @ RE["comment"] :> Nothing(*{"comment", s}*),
				s:RegularExpression @ RE["at-keyword-token"] :> 
					CSSToken[<|
						"Type"   -> "at-keyword", 
						"String" -> CSSNormalizeEscapes @ StringDrop[s, 1]|>],
				s:RegularExpression @ RE["url-token"] :> 
					CSSToken[<|
						"Type"   -> "url", 
						"String" -> stripURL @ s|>],
				s:RegularExpression @ RE["urlhead"] :> 
					CSSToken[<|
						"Type"   -> "urlhead", 
						"String" -> ""|>],
				s:RegularExpression @ RE["string-token"] :> 
					CSSToken[<|
						"Type"   -> "string", 
						"String" -> StringTake[s, {2, -2}]|>],
				s:RegularExpression @ RE["function-token"] :> 
					CSSToken[<|
						"Type"   -> "function", 
						"String" -> CSSNormalizeEscapes @ ToLowerCase @ StringDrop[s, -1]|>],
				s:RegularExpression @ RE["hash-token"] :> 
					With[{v = idHash @ StringDrop[s, 1]},
						CSSToken[<|
							"Type"   -> "hash",
							"String" -> First @ v,
							"Flag"   -> Last @ v|>]],
				s:RegularExpression @ RE["unicode-range-token"] :> 
					With[{range = calculateUnicodeRange @ s},
						CSSToken[<|
							"Type" -> "unicode-range", 
							"Start" -> First @ range,
							"Stop"  -> Last @ range|>]],
							
				(* because of CSS Variables we need to catch this CDC token before parsing idents *)
				"-->" :> CSSToken[<|"Type" -> "CDC", "String " -> "-->"|>], 
				
				s:RegularExpression @ RE["ident-token"] :> 
					CSSToken[<|
						"Type"   -> "ident", 
						"String" -> If[s == "\\", s, CSSNormalizeEscapes @ s]|>],
				
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
							"Unit"      -> CSSNormalizeEscapes @ ToLowerCase @ unit|>]],
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
				
				"-->"  :> CSSToken[<|"Type" -> "CDC" "String" -> "-->"|>], 
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
				
				s:Whitespace :> CSSToken[<|"Type" -> " ", "String" -> " "|>],
				s_ :> CSSToken[<|"Type" -> "delim", "String" -> s|>]
				
				
			}],
		{
			{"ident", "\\"} :> "\\",
			"" :> Nothing},
		{1}]


tokenizePercentage[x_String] := StringSplit[x, num:RegularExpression[RE["num"]] ~~ unit:"%" :> tokenizeNumber @ num]

tokenizeDimension[x_String] := 
	StringSplit[x, 
		num:RegularExpression[RE["num"]] ~~ unit___ :> 
			Flatten @ {tokenizeNumber @ num, CSSNormalizeEscapes @ ToLowerCase @ unit}]

tokenizeNumber[x_String] := (* cache the Interpreter calls *)
	tokenizeNumber[x] = 
		If[StringMatchQ[x, RegularExpression["[+\\-]?[0-9]+"]], 
			{x, Interpreter["Integer"][x], "integer"}
			, 
			{x, Interpreter["Number"][x], "number"}]
		
idHash[x_String] := 
	If[StringMatchQ[x, RegularExpression[RE["ident-token"]]] && !StringMatchQ[x, RegularExpression["[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6,6}|[0-9a-fA-F]{8,8}"]], 
		{CSSNormalizeEscapes @ x, "id"}
		, 
		{CSSNormalizeEscapes @ x, "unrestricted"}]

calculateUnicodeRange[x_String] :=
	StringCases[x,
		{
			s1:RegularExpression["[a-fA-F0-9]{1,6}"] ~~ "-" ~~ s2:RegularExpression["[a-fA-F0-9]{1,6}"] :> {FromDigits[s1, 16], FromDigits[s2, 16]},
			s:RegularExpression["[a-fA-F0-9]{0,6}[\\?]{1,6}"] :> {FromDigits[StringReplace[s, "?" -> "0"], 16], FromDigits[StringReplace[s, "?" -> "F"], 16]},
			s:RegularExpression["[a-fA-F0-9]{1,6}"] :> {FromDigits[s, 16], FromDigits[s, 16]}}]
		
stripURL[x_String] := 
	With[{noURL = First[StringCases[x, RegularExpression[RE["urlhead"]] ~~ s__ ~~ ")" :> StringTrim @ s], ""]},
		If[StringMatchQ[noURL, RegularExpression[RE["string-token"]]], 
			StringTake[noURL, {2, -2}]
			, 
			(* only normalize escapes if the URL was unquoted *)
			CSSNormalizeEscapes @ noURL]] 


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
		brackets = ConstantArray[0, Count[tokens, "{" | "[" | "(" | "\"" | "'" | {"function", _} | {"urlhead", _}]];
		
		While[pos <= l,
			Switch[CSSTokenType @ tokens[[pos]],
				"{" | "[" | "(", 
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos]], pos}];
					inWS = False,
				"urlhead",  
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos, 1]], pos}; inBadURL = True];
					inWS = False,
				"function", 
					If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {tokens[[pos, 1]], pos}];
					inWS = False,
				"\"" | "'",      
					If[!inBadString, inBadString = True];
					depth++; brackets[[depth]] = {tokens[[pos]], pos};
					inWS = False,
				"newline", 
					If[inBadString, 
						inBadString = False;
						tokens[[brackets[[depth, 2]]]] = {"error", "bad-string"};
						Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = 0;
						depth--];
					If[inWS, tokens[[pos]] = {"error", "extra-ws"}, inWS = True],
				" ", If[inWS, tokens[[pos]] = {"error", "extra-ws"}, inWS = True],
				"}",
					If[inBadString || inBadURL, 
						Null
						,
						If[depth == 0 || brackets[[depth, 1]] != "{",
							tokens[[pos]] = {"error", tokens[[pos]]}
							,
							tokens[[brackets[[depth, 2]]]] = Prepend[tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]], "{}"];
							Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = 0;
							depth--]];
					inWS = False,
				")", 
					If[inBadURL, 
						(* the closing of a bad URL also closes a bad string *)
						If[inBadString, inBadString = False; brackets[[depth]] = 0; depth--];
						inBadURL = False;
						tokens[[brackets[[depth, 2]]]] = {"error", "bad-url"};
						Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = 0;
						depth--
						,
						If[inBadString,
							Null
							,
							If[depth == 0 || !MatchQ[brackets[[depth, 1]], "function"|"("], 
								tokens[[pos]] = {"error", tokens[[pos]]}
								,
								tokens[[brackets[[depth, 2]]]] = 
									Join[
										If[brackets[[depth, 1]] == "(", 
											{"()"}
											,
											tokens[[brackets[[depth, 2]]]]],
										tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]]];
								Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
								brackets[[depth]] = 0;
								depth--]]];
					inWS = False, 
				"]",  
					If[inBadString || inBadURL,
						Null
						,
						If[depth == 0 || brackets[[depth, 1]] != "[", 
							tokens[[pos]] = {"error", tokens[[pos]]}
							, 
							tokens[[brackets[[depth, 2]]]] = Prepend[tokens[[brackets[[depth, 2]] + 1 ;; pos - 1]], "[]"];
							Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = 0;
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
					With[{
						try = 
							StringJoin[
								tokens[[brackets[[depth, 2]]]], 
								untokenize /@ tokens[[brackets[[depth, 2]] + 1 ;; ]], 
								tokens[[brackets[[depth, 2]]]]]}, 
						tokens[[brackets[[depth, 2]]]] = 
							If[StringMatchQ[try, RegularExpression[RE["string-token"]]], 
								{"string", StringTake[try, {2, -2}]}
								,
								{"error", "bad-string"}]],
				inBadURL, 
					inBadURL = False; 
					With[{try = StringJoin["url(", untokenize /@ tokens[[brackets[[depth, 2]] + 1 ;; ]], ")"]}, 
						tokens[[brackets[[depth, 2]]]] = 
							If[StringMatchQ[try, RegularExpression[RE["url-token"]]], 
								{"url", stripURL @ try}
								,
								{"error", "bad-url"}]],
				True,
					tokens[[brackets[[depth, 2]]]] = 
						Join[
							Switch[brackets[[depth, 1]], 
								"{", {"{}"}, 
								"(", {"()"}, 
								"function", tokens[[brackets[[depth, 2]]]],
								"[", {"[]"}],
							tokens[[brackets[[depth, 2]] + 1 ;; ]]]];
			Do[tokens[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
			brackets[[depth]] = 0;
			depth--];
		
		(* remove all tokens that were marked for removal *)	
		DeleteCases[tokens /. {"newline", _} -> " ", {"error", "REMOVE" | "extra-ws"}, Infinity]
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

CSSUntokenize[tokens:{___?CSSTokenQ}] := StringJoin[untokenize /@ tokens]

CSSToken /: untokenize[token:CSSToken[a_?AssociationQ]] := 
	With[
		{
			t = CSSTokenType @ token, s = CSSTokenString @ token, 
			u = CSSTokenUnit @ token, c = CSSTokenChildren @ token
		},
		Switch[t,
			"string",     If[StringStartsQ[s, "'"], "\"" <> s <> "\"", "'" <> s <> "'"],
			"function",   s <> "(" <> If[c === {}, "", untokenize /@ c] <> ")",
			"at-keyword", "@" <> s,
			"number",     s,
			"percentage", s <> "%",
			"dimension",  s <> u,
			"hash",       "#" <> s,
			"ident",      s,
			"newline",    s,
			"{}",         "{" <> If[c === {}, "", untokenize /@ c] <> "}",
			"()",         "(" <> If[c === {}, "", untokenize /@ c] <> ")",
			"[]",         "[" <> If[c === {}, "", untokenize /@ c] <> "]",
			"url",        "url(" <> s <> ")",
			"error",      Switch[s, "REMOVE" | "bad-string", "", s],
			"unicode-range", untokenizeUR[a["Start"], a["Stop"]],
			_,            s				
		]
	]

untokenizeUR[start_?NumericQ, stop_?NumericQ] :=
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
	The utilities are assumed to be used within "consume" functions where pos, l, and tokens are defined. 
	They are expected to run quickly so do no type checking. *)
SetAttributes[
	{
		AdvancePosAndSkipWhitespace, RetreatPosAndSkipWhitespace, 
		AdvancePosToNextSemicolon, AdvancePosToNextSemicolonOrBlock, AdvancePosToNextSemicolonOrComma,
		AdvancePosToNextBlock}, 
	HoldFirst];

TokenTypeIs[s_, pos_, tokens_] := StringMatchQ[CSSTokenType @ tokens[[pos]], s, IgnoreCase -> False]
TokenTypeIsNot[s_, pos_, tokens_] := Not @ TokenTypeIs[s, pos, tokens]

TokenStringIs[s_, pos_, tokens_] := StringMatchQ[CSSTokenString @ tokens[[pos]], s, IgnoreCase -> True]
TokenStringIsNot[s_, pos_, tokens_] := Not @ TokenStringIs[s, pos, tokens]

AdvancePosAndSkipWhitespace[pos_, l_, tokens_] := (pos++; While[pos < l && CSSTokenType @ tokens[[pos]] == " ", pos++])
RetreatPosAndSkipWhitespace[pos_, l_, tokens_] := (pos--; While[pos > 1 && CSSTokenType @ tokens[[pos]] == " ", pos--])

AdvancePosToNextSemicolon[pos_, l_, tokens_] := While[pos < l && CSSTokenType @ tokens[[pos]] != ";", pos++]
AdvancePosToNextSemicolonOrBlock[pos_, l_, tokens_] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "{}" | ";"], pos++]
AdvancePosToNextSemicolonOrComma[pos_, l_, tokens_] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "," | ";"], pos++]

AdvancePosToNextBlock[pos_, l_, tokens_] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "{}"], pos++]


(* ::Section:: *)
(*Package End*)


End[];
EndPackage[];