(* ::Package:: *)

(* Wolfram Language Package *)

(* CSS 3.0 Tokenizer *)
(* We follow the level 3 syntax https://www.w3.org/TR/css-syntax-3/ where we can. *)
(* Some exceptions exist because StringSplit is not as judicious as reading character-by-character. *)
(* This package is a utility package for the rest of CSSTools. *)

BeginPackage["CSSTools`CSSTokenizer`", {"GeneralUtilities`"}] 

SetUsage[RE,                  "RE[string$] returns the corresponding regular expression macro for CSS token patterns."];
SetUsage[CSSNormalizeEscapes, "CSSNormalizeEscapes[string$] converts CSS escapes \\ such as code points to characters."];
SetUsage[CSSTokenQ,           "Returns True$ if the expression is a valid CSS token."];
SetUsage[CSSTokenize,         "CSSTokenize[string$] converts string$ into a list of CSS tokens."];
SetUsage[CSSUntokenize,       "CSSUntokenize[{CSSToken$...}] serializes CSS tokens into a string."];
SetUsage[CSSTokenType,        "CSSTokenType[CSSToken$] returns the type of CSSToken$."];
SetUsage[CSSTokenString,      "CSSTokenString[CSSToken$] returns the main string of the CSSToken$."];
SetUsage[CSSTokenValue,       "CSSTokenValue[CSSToken$] returns the interpreted numeric value of the numeric CSSToken$."];
SetUsage[CSSTokenValueType,   "CSSTokenValueType[CSSToken$] returns the type of the CSSToken$ value e.g. \"number\" or \"integer\"."];
SetUsage[CSSDimensionUnit,    "CSSDimensionUnit[CSSToken$] returns the dimension of a dimension CSSToken$ e.g. \"em\" or \"px\"."];

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
			{"newline", "\n"},
			{"error", _}]] 

CSSTokenize[x_String] := nestTokens @ tokenizeFlat @ x


(* ::Subsection::Closed:: *)
(*Token access*)


CSSTokenType[x_String] := x
CSSTokenType[{type_String, ___}] := type

CSSTokenString[x_String] := x
CSSTokenString[{type_String, string_String, ___}] := string

CSSTokenValue[{"number"|"dimension"|"percentage", string_String, value_?NumericQ, ___}] := value
CSSTokenValue[___] := None

CSSTokenValueType[{"number"|"dimension"|"percentage", string_String, value_?NumericQ, valueType_String, ___}] := valueType
CSSTokenValueType[___] := None

CSSDimensionUnit[{"dimension", string_String, value_?NumericQ, valueType_String, dimension_String}] := dimension
CSSDimensionUnit[___] := None


(* ::Subsection::Closed:: *)
(*Flat tokenizer*)


tokenizeFlat[x_String] := 
	Replace[
		StringSplit[x, 
			{
				s:RegularExpression @ RE["comment"]             :> Nothing(*{"comment", s}*),
				s:RegularExpression @ RE["at-keyword-token"]    :> {"at-keyword", CSSNormalizeEscapes @ StringDrop[s, 1]},
				s:RegularExpression @ RE["url-token"]           :> {"url", stripURL @ s},
				s:RegularExpression @ RE["urlhead"]             :> {"urlhead", ""},
				s:RegularExpression @ RE["string-token"]        :> {"string", StringTake[s, {2, -2}]},
				s:RegularExpression @ RE["function-token"]      :> {"function", CSSNormalizeEscapes @ ToLowerCase @ StringDrop[s, -1]},
				s:RegularExpression @ RE["hash-token"]          :> Flatten @ {"hash", idHash @ StringDrop[s, 1]},
				s:RegularExpression @ RE["unicode-range-token"] :> Flatten @ {"unicode-range", calculateUnicodeRange @ s},
				
				"-->" :> "-->", (* because of CSS Variables we need to catch this CDC token before parsing idents *)
				
				s:RegularExpression @ RE["ident-token"]         :> {"ident", If[s == "\\", s, CSSNormalizeEscapes @ s]},
				
				(* scientific notation is tricky; it can look like a dimension but can also be part of a dimension *)
				num:RegularExpression[RE["numSCI"]] ~~ "%"      :> Flatten @ {"percentage", tokenizeNumber @ num},
				num:RegularExpression[RE["numSCI"]] ~~ unit:RegularExpression[RE["ident-token"]] :>	
					Flatten @ {"dimension", tokenizeNumber[num], CSSNormalizeEscapes @ ToLowerCase @ unit},
				num:RegularExpression[RE["numSCI"]]             :> Flatten @ {"number", tokenizeNumber @ num},
								
				s:RegularExpression @ RE["percentage-token"]    :> Flatten @ {"percentage", tokenizePercentage @ s},
				s:RegularExpression @ RE["dimension-token"]     :> Flatten @ {"dimension",  tokenizeDimension @ s},
				s:RegularExpression @ RE["num"]                 :> Flatten @ {"number",     tokenizeNumber @ s},
				
				s:Alternatives[
					";", ":", ",", "(", ")", "{", "}", "[", "]", 
					"~=", "|=", "^=", "$=", "*=", "||", "<!--", "-->", 
					"/", "@", "*", ".", "#"] :> s,
				s:RegularExpression @ RE["newline"] :> {"newline", s},
				(*"\\" -> "\\uFFFD", (* trailing slashes are effectively undisplayable??? *)*)
				s:Whitespace :> " "}],
		{
			{"ident", "\\"} :> "\\",
			"" :> Nothing},
		{1}]
		
		
tokenizePercentage[x_String] := StringSplit[x, num:RegularExpression[RE["num"]] ~~ unit:"%" :> tokenizeNumber @ num]

tokenizeDimension[x_String] := StringSplit[x, num:RegularExpression[RE["num"]] ~~ unit___ :> {tokenizeNumber @ num, CSSNormalizeEscapes @ ToLowerCase @ unit}]

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
nestTokens[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], depth = 0, brackets, t = tokens, inBadURL = False, inBadString = False},
		(* The upper limit of nestings is the number of open brackets and quotation marks *)
		brackets = ConstantArray[0, Count[tokens, "{" | "[" | "(" | "\"" | "'" | {"function", _} | {"urlhead", _}]];
		
		While[pos <= l,
			Switch[t[[pos]],
				"{" | "[" | "(", If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {t[[pos]], pos}],
				{"urlhead", _},  If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {t[[pos, 1]], pos}; inBadURL = True],
				{"function", _}, If[!inBadString && !inBadURL, depth++; brackets[[depth]] = {t[[pos, 1]], pos}],
				"\"" | "'",      
					If[!inBadString, inBadString = True];
					depth++; brackets[[depth]] = {t[[pos]], pos},
				{"newline", _}, 
					If[inBadString, 
						inBadString = False;
						t[[brackets[[depth, 2]]]] = {"error", "bad-string"};
						Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = 0;
						depth--],
				"}",
					If[inBadString || inBadURL, 
						Null
						,
						If[depth == 0 || brackets[[depth, 1]] != "{",
							t[[pos]] = {"error", t[[pos]]}
							,
							t[[brackets[[depth, 2]]]] = Prepend[t[[brackets[[depth, 2]] + 1 ;; pos - 1]], "{}"];
							Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = 0;
							depth--]],
				")", 
					If[inBadURL, 
						(* the closing of a bad URL also closes a bad string *)
						If[inBadString, inBadString = False; brackets[[depth]] = 0; depth--];
						inBadURL = False;
						t[[brackets[[depth, 2]]]] = {"error", "bad-url"};
						Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
						brackets[[depth]] = 0;
						depth--
						,
						If[inBadString,
							Null
							,
							If[depth == 0 || !MatchQ[brackets[[depth, 1]], "function"|"("], 
								t[[pos]] = {"error", t[[pos]]}
								,
								t[[brackets[[depth, 2]]]] = 
									Join[
										If[brackets[[depth, 1]] == "(", 
											{"()"}
											,
											t[[brackets[[depth, 2]]]]],
										t[[brackets[[depth, 2]] + 1 ;; pos - 1]]];
								Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
								brackets[[depth]] = 0;
								depth--]]], 
				"]",  
					If[inBadString || inBadURL,
						Null
						,
						If[depth == 0 || brackets[[depth, 1]] != "[", 
							t[[pos]] = {"error", t[[pos]]}
							, 
							t[[brackets[[depth, 2]]]] = Prepend[t[[brackets[[depth, 2]] + 1 ;; pos - 1]], "[]"];
							Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
							brackets[[depth]] = 0;
							depth--]], 
				_, Null];
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
								t[[brackets[[depth, 2]]]], 
								untokenize /@ t[[brackets[[depth, 2]] + 1 ;; ]], 
								t[[brackets[[depth, 2]]]]]}, 
						t[[brackets[[depth, 2]]]] = 
							If[StringMatchQ[try, RegularExpression[RE["string-token"]]], 
								{"string", StringTake[try, {2, -2}]}
								,
								{"error", "bad-string"}]],
				inBadURL, 
					inBadURL = False; 
					With[{try = StringJoin["url(", untokenize /@ t[[brackets[[depth, 2]] + 1 ;; ]], ")"]}, 
						t[[brackets[[depth, 2]]]] = 
							If[StringMatchQ[try, RegularExpression[RE["url-token"]]], 
								{"url", stripURL @ try}
								,
								{"error", "bad-url"}]],
				True,
					t[[brackets[[depth, 2]]]] = 
						Join[
							Switch[brackets[[depth, 1]], 
								"{", {"{}"}, 
								"(", {"()"}, 
								"function", t[[brackets[[depth, 2]]]],
								"[", {"[]"}],
							t[[brackets[[depth, 2]] + 1 ;; ]]]];
			Do[t[[i]] = {"error", "REMOVE"}, {i, brackets[[depth, 2]] + 1, pos, 1}];
			brackets[[depth]] = 0;
			depth--];
		
		(* remove all tokens that were marked for removal *)	
		DeleteCases[t /. {"newline", _} -> " ", {"error", "REMOVE"}, Infinity]
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

untokenize[x_String] := x
untokenize[{"string", s_?StringQ}] := If[StringContainsQ[s, "'"], "\"" <> s <> "\"", "'" <> s <> "'"]
untokenize[{"function", s_?StringQ, tokens__?CSSTokenQ}] := s <> "(" <> untokenize /@ {tokens} <> ")"
untokenize[{"function", s_?StringQ}] := s <> "()"
untokenize[{"at-keyword", s_?StringQ}] := "@" <> s
untokenize[{"number", s_?StringQ, __}] := s
untokenize[{"percentage", s_?StringQ, __}] := s <> "%"
untokenize[{"dimension", num_?StringQ, _?NumericQ, "integer"|"number", dim_?StringQ}] := num <> dim
untokenize[{"hash", s_?StringQ, ___}] := "#" <> s
untokenize[{"ident", s_?StringQ}] := s
untokenize[{"newline", s_?StringQ}] := s
untokenize[{"error", "REMOVE"}] := ""
untokenize[{"error", "bad-string"}] := ""
untokenize[{"error", s_}] := s
untokenize[{"{}", s___?CSSTokenQ}] := "{" <> untokenize /@ {s} <> "}"
untokenize[{"()", s___?CSSTokenQ}] := "(" <> untokenize /@ {s} <> ")"
untokenize[{"[]", s___?CSSTokenQ}] := "[" <> untokenize /@ {s} <> "]"
untokenize[{"url", s_?StringQ}] := "url(" <> s <> ")"
untokenize[{"unicode-range", start_?NumericQ, stop_?NumericQ}] :=
	Module[{t, startString, stopString, l},
		t = IntegerString[#, 16]& /@ {start, stop};
		{startString, stopString} = StringPadLeft[#, Max[StringLength /@ t], "0"] & /@ t;
		l = Intersection[StringPosition[startString, "0"], StringPosition[stopString, "f"]];
		Which[
			Length[l] > 0,                    "u+" <> StringReplacePart[startString, "?", l],
			TrueQ[startString == stopString], "u+" <> startString,
			True,                             "u+" <> startString <> "-" <> stopString]]


(* ::Section::Closed:: *)
(*Identify @import components*)


(*tokenizeAtImportKeyword[x_String] := 
	Replace[
		StringSplit[x,
			{
				s:RegularExpression @ T["IMPORT_SYM"] :> {"import", s},
				s:RegularExpression @ T["URI"]        :> {"uri",    s},
				s:RegularExpression @ T["STRING"]     :> {"string", s},
				s:RegularExpression @ P["medium"]     :> {"medium", s},
				"," -> ",", 
				";" -> ";"}],
		{
			s_String /; StringMatchQ[s, Whitespace | ""] :> Nothing, (* whitespace is OK to remove here *)
			s_String /; StringLength[s] > 1              :> {"other", s}},
		{1}]*)


(*parse["atImportKeyword", tokens:{__?CSSTokenQ}] :=
	Module[{pos = 2 (* first token must be @import *), l = Length[tokens], path, mediaStart, mediums = {}, data},
		(* skip any whitespace before URL *)
		If[CSSTokenType @ tokens[[pos]] == " ", skipWhitespace[pos, l, tokens]];
		
		(* next token must be URL or string path to file *)
		path = 
			Switch[CSSTokenType @ tokens[[pos]],
				"uri",    CSSTokenString @ tokens[[pos]],
				"string", CSSTokenString @ tokens[[pos]],
				_,        "" (* shouldn't be able to reach this *)];
		skipWhitespace[pos, l, tokens]; 	
		
		(* anything else is a comma-delimited set of media queries *)
		(*TODO: implement proper media queries *)
		While[CSSTokenType @ tokens[[pos]] != ";",
			mediaStart = pos;
			While[!MatchQ[CSSTokenType @ tokens[[pos]], "," | ";"], pos++];
			AppendTo[mediums, StringJoin[CSSTokenString /@ tokens[[mediaStart, pos - 1]]]];
			If[CSSTokenType @ tokens[[pos]] == ";", Break[]]
		];
		
		(* import without interpretation *)
		data = Import[Echo[path, "@import"], "Text"];
		If[FailureQ[data], Return @ {}, data = processRulesets[data]];
		If[mediums =!= {}, data[[All, "Condition"]] = ConstantArray[mediums, Length[data]]];
		data	
	]*)


(* ::Section:: *)
(*Package End*)


End[];
EndPackage[];