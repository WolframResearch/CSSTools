(* ::Package:: *)

(* ::Text:: *)
(*Get-able CSS EBNF for use with FunctionalParser package*)


(* ::Section::Closed:: *)
(*Media Types*)


(* ::Text:: *)
(*'Aural' media type is deprecated. 'Speech' should be used instead.*)


(*MediaTypes = {"braille", "embossed", "handheld", "print", "projection", "screen", "speech", "tty", "tv"};
MediaTypeGroups = {"all", "continuous", "paged", "visual", "audio", "speech", "tactile", "grid", "bitmap", "interactive", "static"};
MediaTypeGroup["all"] = MediaTypes;
MediaTypeGroup["continuous"] =  MediaTypes[[{1, 3, 6, 7, 8, 9}]];
MediaTypeGroup["paged"] =       MediaTypes[[{2, 3, 4, 5, 9}]];
MediaTypeGroup["visual"] =      MediaTypes[[{3, 4, 5, 6, 8, 9}]];
MediaTypeGroup["audio"] =       MediaTypes[[{3, 6, 9}]];
MediaTypeGroup["speech"] =      MediaTypes[[{3, 7}]];
MediaTypeGroup["tactile"] =     MediaTypes[[{1, 2}]];
MediaTypeGroup["grid"] =        MediaTypes[[{1, 2, 3, 8}]];
MediaTypeGroup["bitmap"] =      MediaTypes[[{3, 4, 5, 6, 9}]];
MediaTypeGroup["interactive"] = MediaTypes[[{1, 3, 5, 6, 7, 8, 9}]];
MediaTypeGroup["static"] =      MediaTypes[[{1, 2, 3, 4, 6, 7, 8, 9}]];*)


(* ::Section::Closed:: *)
(*CSS Grammar*)


(* ::Subsection::Closed:: *)
(*Regular expression macros (alphabetical)*)


(*
	WL strings use the backslash \ as the escape character. 
	WL string characters that require escaping consist of double quote and backslash i.e. "\
	REs are defined using strings and have their own additional rules:
		The complete list of characters that need to be escaped in a RE consists of .\?(){}[]^$*+|
		Inside a RE character class [...], the complete list of escaped characters is ^-\[]
	Special cases to watch out for:
		\\\\ defines a literal backslash
		\\.  defines a literal period
	Though double quotes do not need to be escaped in the RE, WL strings still need to have the character escaped.
	CSS is case-insensitive, so A-Z ranges are included alongside a-z ranges, even though this is not in the specification.
	For clarity, all macros are surround by parenthesis to keep them isolated from other RE patterns.
*)


RE["badcomment"]  = "(" ~~ RE["badcomment1"] ~~ "|" ~~ RE["badcomment2"] ~~ ")";
RE["badcomment1"] = "(\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*)"; (* literal asterisks in RE only outside of RE character class [] *)
RE["badcomment2"] = "(\\/\\*[^*]*(\\*+[^/*][^*]*)*)";
RE["badstring"]   = "(" ~~ RE["badstring1"] ~~ "|" ~~ RE["badstring2"] ~~ ")";
RE["badstring1"]  = "(\\\"([^\n\r\f\\\"]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\\?)"; 
RE["badstring2"]  = "(\\'([^\n\r\f\\']|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\\?)"; 
RE["baduri"]      = "(" ~~ RE["baduri1"] ~~ "|" ~~ RE["baduri2"] ~~ "|" ~~ RE["baduri3"] ~~ ")";
RE["baduri1"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ "([!#$%&*-\\\[\\\]-~]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*" ~~ RE["w"] ~~ ")"; (* literal ( after url; CharacterRange["*", "~"]*)
RE["baduri2"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["string"] ~~ RE["w"] ~~ ")";
RE["baduri3"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["badstring"] ~~ ")";
RE["comment"]     = "(\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/)";
RE["h"]           = "([0-9a-fA-F])";
RE["ident"]       = "(-?" ~~ RE["nmstart"] ~~ RE["nmchar"] ~~ "*)";
RE["escape"]      = "(" ~~ RE["unicode"] ~~ "|\\\\[^\n\r\f0-9a-fA-F])"; 
RE["name"]        = "(" ~~ RE["nmchar"] ~~ "+)";
RE["nl"]          = "(\n|\r\n|\r|\f)";
RE["nmchar"]      = "([_a-zA-Z0-9\\-]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["nmstart"]     = "([_a-zA-Z]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["nonascii"]    = (*FIXME?*) "([^[:ascii:]])";
RE["num"]         = "([\\-+]?[0-9]*\\.[0-9]+|[\\-+]?[0-9]+)"; (* \\. literal dot for RE; pattern match against reals before integers *)
RE["s"]           = "([ \t\r\n\f]+)";
RE["string"]      = "(" ~~ RE["string1"] ~~ "|" ~~ RE["string2"] ~~ ")";
RE["string1"]     = "(\\\"([^\n\r\f\\\"]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\")"; (* [] contains escaped double quote *)
RE["string2"]     = "(\\'([^\n\r\f\\']|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\')"; (* [] contains escaped single quote *)
RE["unicode"]     = "(\\\\" ~~ RE["h"] ~~ "{1,6}(\r\n|[ \n\r\t\f])?)";
RE["url"]         = "(([!#$%&*-~]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*)";
RE["w"]           = "((" ~~ RE["s"] ~~ ")?)";


RE["A"] = "(a|\\\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?)";
RE["C"] = "(c|\\\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?)";
RE["D"] = "(d|\\\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?)";
RE["E"] = "(e|\\\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?)";
RE["G"] = "(g|\\\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\\\g)";
RE["H"] = "(h|\\\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\\\h)";
RE["I"] = "(i|\\\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\\\i)";
RE["K"] = "(k|\\\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\\\k)";
RE["L"] = "(l|\\\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?|\\\\l)";
RE["M"] = "(m|\\\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\\\m)";
RE["N"] = "(n|\\\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\\\n)";
RE["O"] = "(o|\\\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\\\o)";
RE["P"] = "(p|\\\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\\\p)";
RE["R"] = "(r|\\\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\\\r)";
RE["S"] = "(s|\\\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\\\s)";
RE["T"] = "(t|\\\\0{0,4}(44|74)(\r\n|[ \t\r\n\f])?|\\\\t)";
RE["U"] = "(u|\\\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\\\u)";
RE["X"] = "(x|\\\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\\\x)";
RE["Z"] = "(z|\\\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\\\z)";


(* ::Subsection::Closed:: *)
(*Tokens (alphabetical)*)


T["ANGLE"] = 
	StringExpression[
		"(" ~~ RE["num"] ~~ RE["D"] ~~ RE["E"] ~~ RE["G"] ~~ ")" ~~ "|", 
		"(" ~~ RE["num"] ~~ RE["R"] ~~ RE["A"] ~~ RE["D"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["G"] ~~ RE["R"] ~~ RE["A"] ~~ RE["D"] ~~ ")"];

T["BADSTRING"] = RE["badstring"];
T["BADURI"]    = RE["baduri"];

T["CDC"]         = "(-->)";
T["CDO"]         = "(<!--)";
T["CHARSET_SYM"] = "(@charset )";

T["DASHMATCH"] = "(\\|=)";
T["DIMENSION"] = "(" ~~ RE["num"] ~~ RE["ident"] ~~ ")";

T["EMS"] = "(" ~~ RE["num"] ~~ RE["E"] ~~ RE["M"] ~~ ")";
T["EXS"] = "(" ~~ RE["num"] ~~ RE["E"] ~~ RE["X"] ~~ ")";

T["FREQ"]     = "((" ~~ RE["num"] ~~ RE["H"] ~~ RE["Z"] ~~ ")|(" ~~ RE["num"] ~~ RE["K"] ~~ RE["H"] ~~ RE["Z"] ~~ "))";
T["FUNCTION"] = "(" ~~ RE["ident"] ~~ "\\()";

T["HASH"] = "(#" ~~ RE["name"] ~~ ")";

T["IDENT"]         = RE["ident"];
T["IMPORT_SYM"]    = "(@" ~~ RE["I"] ~~ RE["M"] ~~ RE["P"] ~~ RE["O"] ~~ RE["R"] ~~ RE["T"] ~~ ")";
T["IMPORTANT_SYM"] = "(!(" ~~ RE["w"] ~~ "|" ~~ RE["comment"] ~~ ")*" ~~ RE["I"] ~~ RE["M"] ~~ RE["P"] ~~ RE["O"] ~~ RE["R"] ~~ RE["T"] ~~ RE["A"] ~~ RE["N"] ~~ RE["T"] ~~ ")";
T["INCLUDES"]      = "(~=)";

T["LENGTH"] = 
	StringExpression[
		"(" ~~ RE["num"] ~~ RE["P"] ~~ RE["X"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["C"] ~~ RE["M"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["M"] ~~ RE["M"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["I"] ~~ RE["N"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["P"] ~~ RE["T"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["P"] ~~ RE["C"] ~~ ")"];

T["MEDIA_SYM"] = "(@" ~~ RE["M"] ~~ RE["E"] ~~ RE["D"] ~~ RE["I"] ~~ RE["A"] ~~ ")";

T["NUMBER"] = RE["num"];

T["PAGE_SYM"]   = "(@" ~~ RE["P"] ~~ RE["A"] ~~ RE["G"] ~~ RE["E"] ~~ ")";
T["PERCENTAGE"] = "(" ~~ RE["num"] ~~ "%)";

T["S"]      = RE["s"];
T["S*"]     = "(" ~~ RE["s"] ~~ "*" ~~ ")"; (* this is not in the spec, but makes productions cleaner *)
T["STRING"] = RE["string"];

T["TIME"] = "((" ~~ RE["num"] ~~ RE["M"] ~~ RE["S"] ~~ ")|(" ~~ RE["num"] ~~ RE["H"] ~~ RE["Z"] ~~ "))";

T["URI"] =
	StringExpression[
		"(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["string"] ~~ RE["w"] ~~ "\\)" ~~ ")" ~~ "|",
		"(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["url"] ~~ RE["w"] ~~ "\\)" ~~ ")"];


(* ::Subsection::Closed:: *)
(*Productions (TODO, not all used)*)


P["declaration"] := "(" ~~ P["property"] ~~ ":" ~~ T["S*"] ~~ P["expr"] ~~ P["prio"] ~~ "?)";
P["prio"]        := "(" ~~ T["IMPORTANT_SYM"] ~~ T["S*"] ~~ ")";
P["expr"]        := "(" ~~ P["term"] ~~ "(" ~~ P["operator"] ~~ "?" ~~ P["term"] ~~ ")*)";
P["property"]    := "(" ~~ T["IDENT"] ~~ T["S*"] ~~ ")";
P["operator"]    := "((/" ~~ T["S*"] ~~ ")|(," ~~ T["S*"] ~~ "))";

P["term"] := 
	StringExpression[
		"(" ~~ T["STRING"]   ~~ T["S*"] ~~ ")" ~~ "|",
		"(" ~~ T["URI"]      ~~ T["S*"] ~~ ")" ~~ "|",
		(*"(" ~~ P["function"] ~~ T["S*"] ~~ ")" ~~ "|",*) (* avoid recursion by only looking for function head *)
		"(" ~~ T["FUNCTION"] ~~ T["S*"] ~~ ")" ~~ "|",
		"(" ~~ T["IDENT"]    ~~ T["S*"] ~~ ")" ~~ "|",
		"(" ~~ P["hexcolor"] ~~ T["S*"] ~~ ")" ~~ "|",
		"(" ~~ 
			"(" ~~ T["TIME"]        ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["LENGTH"]      ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["FREQ"]        ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["ANGLE"]       ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["EMS"]         ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["EXS"]         ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["PERCENTAGE"]  ~~ T["S*"] ~~ ")" ~~ "|" ~~
			"(" ~~ T["NUMBER"]      ~~ T["S*"] ~~ ")" ~~ 
		")"];
		
P["function"] := "(" ~~ T["FUNCTION"] ~~ T["S*"] ~~ P["expr"] ~~ "\\)" ~~ T["S*"] ~~ ")";
P["hexcolor"] := "(" ~~ T["HASH"] ~~ T["S*"] ~~ ")";


(*StringMatchQ["rect(1px,1px,1px,1px)", RegularExpression[
T["FUNCTION"] ~~ T["S*"] ~~ 
	"(" ~~
		"(" ~~ T["LENGTH"] ~~ T["S"] ~~ T["LENGTH"] ~~ T["S"] ~~ T["LENGTH"] ~~ T["S"] ~~ T["LENGTH"] ~~ T["S*"] ~~ ")" ~~ "|" ~~
		"(" ~~ 
			T["LENGTH"] ~~ T["S*"] ~~ "," ~~ T["S*"] ~~ 
			T["LENGTH"] ~~ T["S*"] ~~ "," ~~ T["S*"] ~~ 
			T["LENGTH"] ~~ T["S*"] ~~ "," ~~ T["S*"] ~~ 
			T["LENGTH"] ~~ T["S*"] ~~ ")" ~~ 
	")" ~~ "\)"]]*)


(* ::Section::Closed:: *)
(*Parse File*)


(* ::Text:: *)
(*May need to consider @charset at start of document, or always assume UTF-8? (Default is UTF-8)*)


(*StringToByteArray["@charset \"\";"] //Normal // BaseForm[#, 16]&*)


(* ::Subsection::Closed:: *)
(*Identify rulesets (selector + block declarations)*)


(* DeleteCases removes any comment-like token. Perhaps we should allow an option flag to keep comments...? *)
parseBlock[x_String] := 
	DeleteCases[
		Map[{label["block", (*StringTrim @*) #], (*StringTrim @*) #}&, 
			StringSplit[x, 
				s:Alternatives[
					RegularExpression @ RE["comment"],
					RegularExpression @ RE["badcomment"],
					RegularExpression @ T["URI"],
					RegularExpression @ T["BADURI"],
					RegularExpression @ T["STRING"],
					RegularExpression @ T["BADSTRING"],
					"{", "}"
				] :> s]],
		{"comment" | "badcomment", _} | {"other", ""}]		


label["block", x_String] := 
	Which[
		StringMatchQ[x, RegularExpression @ RE["comment"]],    "comment",
		StringMatchQ[x, RegularExpression @ RE["badcomment"]], "badcomment",
		StringMatchQ[x, RegularExpression @ T["URI"]],         "uri",
		StringMatchQ[x, RegularExpression @ T["BADURI"]],      "baduri",
		StringMatchQ[x, RegularExpression @ T["STRING"]],      "string",
		StringMatchQ[x, RegularExpression @ T["BADSTRING"]],   "badstring",
		StringMatchQ[x, "{" | "}"], x,
		True, "other"
	]


(* ::Subsection::Closed:: *)
(*Identify declarations*)


parseDeclaration[x_String] := 
	DeleteCases[
		Map[{label["term", #], #}&, 
			StringSplit[x, 
				s:Alternatives[
					RegularExpression @ T["STRING"],
					RegularExpression @ T["URI"],
					RegularExpression @ T["FUNCTION"],
					RegularExpression @ T["IDENT"],
					RegularExpression @ P["hexcolor"],
					RegularExpression @ T["TIME"],
					RegularExpression @ T["LENGTH"],
					RegularExpression @ T["FREQ"],
					RegularExpression @ T["ANGLE"],
					RegularExpression @ T["EMS"],
					RegularExpression @ T["EXS"],
					RegularExpression @ T["PERCENTAGE"],
					RegularExpression @ T["NUMBER"],
					RegularExpression @ T["IMPORTANT_SYM"],
					":", ";", ",", "/", ")"
				] :> s]],
		{"other", ""}]		


label["term", x_String] := 
	Which[
		StringMatchQ[x, RegularExpression @ T["STRING"]],        "string",
		StringMatchQ[x, RegularExpression @ T["URI"]],           "uri",
		StringMatchQ[x, RegularExpression @ T["FUNCTION"]],      "function",
		StringMatchQ[x, RegularExpression @ T["IDENT"]],         "ident",
		StringMatchQ[x, RegularExpression @ P["hexcolor"]],      "hexcolor",
		StringMatchQ[x, RegularExpression @ T["TIME"]],          "time",
		StringMatchQ[x, RegularExpression @ T["LENGTH"]],        "length",
		StringMatchQ[x, RegularExpression @ T["FREQ"]],          "freq",
		StringMatchQ[x, RegularExpression @ T["ANGLE"]],         "angle",
		StringMatchQ[x, RegularExpression @ T["EMS"]],           "ems",
		StringMatchQ[x, RegularExpression @ T["EXS"]],           "exs",
		StringMatchQ[x, RegularExpression @ T["PERCENTAGE"]],    "percentage",
		StringMatchQ[x, RegularExpression @ T["NUMBER"]],        "number",
		StringMatchQ[x, RegularExpression @ T["IMPORTANT_SYM"]], "important",
		StringMatchQ[x, "," | "/"],                              "operator",
		StringMatchQ[x, WhitespaceCharacter..],                  "whitespace",
		StringMatchQ[x, ":" | ";" | ")"], x,
		True, "other"
	]


(* ::Section:: *)
(*Parse Properties*)


(*
	Color: All typesetting (including FrameBox) follows the FontColor option value. 
	Within Style, graphics directives are first converted to options.
	
	For border/margin properties, converting the number of provided CSS values to WL {{left, right}, {bottom, top}}
		1: applies to all sides                         {{1, 1}, {1, 1}}
		2: 1 -> top + bottom, 2 -> right + left         {{2, 2}, {1, 1}}
		3: 1 -> top, 2 -> right + left, 3 -> bottom     {{2, 2}, {3, 1}}
		4: 1 -> top, 2 -> right, 3 -> bottom, 4 -> left {{4, 2}, {3, 1}}
		
	Some properties rely on inheritance of other options names. 
	For example, a length given as "2em" translates to 2*current-font-size. 
	Perhaps we can use CurrentValue[{StyleDefinitions, <style>, FontSize}] to get this value, 
	but what named style should we inherit from? Output? Graphics?
	
	Also there are 2 universal keywords in CSS level 2: 'initial', 'inherit'. Other future keywords:
		'unset'  is CSS level 3: computed to 'inherit' if it's an inheritable prop, or 'initial' if not.
		'revert' is CSS level 4: only supported in Safari. Resets the cascade to the user stylesheet. Not to be confused with 'initial'.
*)


noValueFailure[prop_String] :=             Failure["UnexpectedParse", <|"Message" -> "No " <> prop <> " property value."|>];
tooManyTokensFailure[tokens_List] :=       Failure["UnexpectedParse", <|"Message" -> "Too many tokens.", "Tokens" -> tokens[[All, 1]]|>];
tooManyPropValuesFailure[props_List] :=    Failure["UnexpectedParse", <|"Message" -> "Too many property values provided.", "Props" -> props|>];
repeatedPropValueFailure[prop_] :=         Failure["UnexpectedParse", <|"Message" -> "Repeated property value type.", "Prop" -> prop|>];
unrecognizedKeyWordFailure[prop_String] := Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " keyword."|>];
unrecognizedValueFailure[prop_String] :=   Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " value."|>];
negativeLengthFailure[prop_String] :=      Failure["UnexpectedParse", <|"Message" -> prop <> "length must be non-negative."|>];
positiveLengthFailure[prop_String] :=      Failure["UnexpectedParse", <|"Message" -> prop <> "length must be positive."|>];
invalidFunctionFailure[function_string] := Failure["UnexpectedParse", <|"Message" -> "Invalid function.", "Function" -> function|>];
couldNotImportFailure[uri_String] :=       Failure["UnexpectedParse", <|"Message" -> "Failed to import from URI.", "URI" -> uri|>];
notAnImageFailure[uri_String] :=           Failure["UnexpectedParse", <|"Message" -> "Asset is not an image.", "URI" -> uri|>]


(* ::Subsection:: *)
(*initial values*)


initialValues = <|
	"background-color" -> None, (* 'transparent' *)
	"border-collapse"  -> Missing["Not supported."], (* 'separated' *)
	"border-top-color"    -> Dynamic @ CurrentValue[FontColor], 
	"border-right-color"  -> Dynamic @ CurrentValue[FontColor],
	"border-bottom-color" -> Dynamic @ CurrentValue[FontColor],
	"border-left-color"   -> Dynamic @ CurrentValue[FontColor],
	"border-color"        -> Dynamic @ CurrentValue[FontColor],
	"border-top-style"    -> None, 
	"border-right-style"  -> None,
	"border-bottom-style" -> None,
	"border-left-style"   -> None,
	"border-style"        -> None,
	"border-top-width"    -> Thickness[Medium], 
	"border-right-width"  -> Thickness[Medium],
	"border-bottom-width" -> Thickness[Medium],
	"border-left-width"   -> Thickness[Medium],
	"border-width"        -> Thickness[Medium],
	"border-top"     -> None, (* 'none' is the CSS default, but it is ignored if inside a Directive, e.g. FrameStyle \[Rule] Directive[None] *)
	"border-right"   -> None, (* Thus, if the initial value of any 'border-*' property is requested, also ignore thickness and color *)
	"border-bottom"  -> None, 
	"border-left"    -> None,
	"border"         -> None,
	"border-spacing" -> 0,
	"clip"           -> Missing["Not supported."],
	"color"          -> Black,    (* no set CSS specification, so use reasonable setting *)
	"font-family"    :> CurrentValue[{StyleDefinitions, "Text", FontFamily}], (* no set CSS specification, so use reasonable setting *)
	"font-size"      -> Medium,   (* 'medium' *)
	"font-style"     -> Plain,    (* 'normal' *)
	"font-variant"   -> "Normal", (* 'normal' *)
	"font-weight"    -> Plain,    (* 'normal' *)
	"height"         -> Automatic, (* 'auto' *)
	"line-height"    -> {1.2, 0}, (* 'normal' *)
	"list-style-image"    -> None,
	"list-style-type"     -> "\[FilledCircle]",
	"list-style-position" -> Automatic,
	"list-style"          -> None,
	"margin-top"     -> 0,
	"margin-bottom"  -> 0,
	"margin-left"    -> 0,
	"margin-right"   -> 0,
	"margin"         -> 0, (* sets all 4 sides *)
	"max-height"     -> Infinity,
	"max-width"      -> Infinity,
	"min-height"     -> 0,
	"min-width"      -> 0,
	"overflow"       -> Missing["Not supported."], (* 'visible' so content can overflow area *)
	"padding-top"    -> 0,
	"padding-bottom" -> 0,
	"padding-left"   -> 0,
	"padding-right"  -> 0,
	"padding"        -> 0, (* sets all 4 sides *)
	"vertical-align" -> Baseline, (* 'baseline' *)
	"width"          -> Automatic (* 'auto' *)
	|>;


(* ::Subsection::Closed:: *)
(*<color>*)


parseSingleColor[prop_String, tokens:{{_String, _String}..}] :=
	If[Length[tokens] == 1 && MatchQ[tokens[[1, 1]], "ident"],
		Switch[ToLowerCase @ tokens[[1, 2]],
			"initial",      initialValues @ prop,
			"inherit",      Inherited,
			"currentcolor", Dynamic @ CurrentValue[FontColor], 
			"transparent",  None, (* this 'ident' is interpreted as GrayLevel[0, 0] by Interpreter["Color"] *)
			_,              Interpreter["Color"][StringJoin @ tokens[[1, 2]]] (* keyword e.g. blue *)
		],
		Interpreter["Color"][StringJoin @ tokens[[All, 2]]] (* could be hexcolor or function e.g. rgb(_,_,_) or hsl(_,_,_) *)
	]


(* ::Subsection::Closed:: *)
(*<length>*)


parseLength[s_String, inFontSize_:False] := 
	Module[{val, unit, dpi = "Resolution" /. First[SystemInformation["Devices", "ScreenInformation"], "Resolution" -> 72]},
		(* check for case where length is 0 and unitless *)
		val = Interpreter["Number"] @ s;
		If[TrueQ[val == 0], Return @ val];
		If[!FailureQ[val], Return @ Failure["UnexpectedParse", <|"Message" -> "Non-zero length has missing units."|>]];
		
		(* separate the number from the unit *)
		{val, unit} = StringSplit[s, x:RegularExpression @ RE["num"] :> x];
	
		val = Interpreter["Number"] @ val;
		If[FailureQ[val], Return @ unrecognizedValueFailure @ "length"];
		If[val == 0,      Return @ 0];
	
		(* parse units 
			The following conversions to pixels are based on SVG length specs and DPI.
			'em' and 'ex' are relative values. If within the 'font-size' property, then first inherit from the parent.
			If an 'em' or 'ex' length is given outside the 'font-size' property, then it's a function of the current FontSize.
		*)
		Switch[ToLowerCase @ unit, 
			"em", If[inFontSize, val*Inherited,     With[{v = val}, Dynamic[v*CurrentValue[FontSize]]]],
			"ex", If[inFontSize, val*0.5*Inherited, With[{v = val}, Dynamic[v*0.5*CurrentValue[FontSize]]]],
			"in", val*dpi,
			"cm", val/2.54*dpi,
			"mm", val/10/2.54*dpi,
			"pt", val,
			"pc", 12*val,
			"px", 0.75*val,
			_, Failure["UnexpectedParse", <|"Message" -> "Unrecognized length unit."|>]
		]
	]


parseEmNonRelative[s_String] := Interpreter["Number"] @ StringDrop[s, -2]


negativeQ[n_, prop_String, default_] :=
	Which[
		FailureQ[n], n, 
		Negative[n], negativeLengthFailure @ prop, 
		True,        default
	]


(* ::Subsection::Closed:: *)
(*<percentage>*)


parsePercentage[s_String] := Interpreter["Number"] @ StringDrop[s, -1]


(* ::Subsection::Closed:: *)
(*background (TODO)*)


(* ::Subsubsection:: *)
(*background-attachment (TODO)*)


(* ::Subsubsection::Closed:: *)
(*background-color*)


(* 
	Effectively the same as color, except a successful parse returns as a rule Background \[Rule] value.
	Also 'currentColor' value needs to know the current value of 'color', instead of inherited from the parent.	
*) 
parse[prop:"background-color", tokens:{{_String, _String}..}] := 
	Module[{value},
		value = parseSingleColor[prop, tokens];
		If[FailureQ[value], value, Background -> value]
	]


(* ::Subsubsection:: *)
(*background-image (TODO)*)


(* ::Subsubsection:: *)
(*background-position (TODO)*)


(* ::Subsubsection:: *)
(*background-repeat (TODO)*)


(* ::Subsection::Closed:: *)
(*border*)


(*
	WL specified border style (dashing), width, and color all at once via Directive[].
	Post-processing is required to combine individual properties correctly.
*)


(* ::Subsubsection::Closed:: *)
(*border-collapse*)


parse[prop:"border-collapse", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		Switch[tokens[[pos, 1]],
			"ident", 
				Switch[ToLowerCase @ tokens[[pos, 2]],
					"inherit",  {},
					"initial",  initialValues @ prop,
					"separate", Missing["Not supported."],
					"collapse", {}, (* this is all Mathematica supports *)
					_,          unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-color*)


(* Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time. *)
parse[prop:"border-top-color"|"border-right-color"|"border-bottom-color"|"border-left-color", tokens:{{_String, _String}..}] := 
	Module[{value, wrapper},
		value = parseSingleColor[prop, tokens];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-color", Top, "border-right-color", Right, "border-bottom-color", Bottom, "border-left-color", Left];
			{FrameStyle -> wrapper[value], Cell[CellFrameColor -> value]}
		]
	]	

(* sets all 4 border/frame edges at once *)
parse[prop:"border-color", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}, start, stop},
		While[pos <= l,
			start = pos; If[tokens[[pos, 1]] == "function", While[pos < l && tokens[[pos, 1]] != ")", pos++]]; stop = pos;
			value = parseSingleColor[prop, tokens[[start ;; stop]]];
			If[FailureQ[value], 
				Return @ value
				, 
				AppendTo[results, value]; pos++; skipWhitespace[pos, l, tokens];
			];
		];
		Switch[Length[results],
			1, {FrameStyle -> {{results[[1]], results[[1]]}, {results[[1]], results[[1]]}}, Cell[CellFrameColor -> First @ results]},
			2, {FrameStyle -> {{results[[2]], results[[2]]}, {results[[1]], results[[1]]}}, Cell[CellFrameColor -> First @ results]},
			3, {FrameStyle -> {{results[[2]], results[[2]]}, {results[[3]], results[[1]]}}, Cell[CellFrameColor -> First @ results]},
			4, {FrameStyle -> {{results[[4]], results[[2]]}, {results[[3]], results[[1]]}}, Cell[CellFrameColor -> First @ results]},
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-spacing*)


(* 
	'border-spacing' isn't a 1-to-1 match with WL Grid's Spacings option, but it's close.
	WL Grid does not allow gaps between items. Spacings is already in units of the current FontSize.
	Also Spacings applies to the outer margins as well, but 'border-spacing' is internal only.
	Because each item is padded on either side, divide the result in half.
*)
parse[prop:"border-spacing", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l == 1 && MatchQ[tokens[[1, 1]], "ident"], 
			Return @ 
				Switch[ToLowerCase @ tokens[[1, 2]],
					"inherit", Spacings -> Inherited,
					"initial", Spacings -> initialValues @ prop,
					_          unrecognizedKeyWordFailure @ prop
				]
		];
		While[pos <= l,
			value = 
				Switch[tokens[[pos, 1]],
					"ems" | "exs",       With[{n = parseEmNonRelative @ tokens[[pos, 2]]}, negativeQ[n, prop, n/2]],
					"number" | "length", With[{n = parseLength @ tokens[[pos, 2]]},        negativeQ[n, prop, Dynamic[n/CurrentValue[FontSize]]]],
					_,                   unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			pos++; skipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1 | 2, Spacings -> results,
			_,     tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-style*)


parseSingleBorderStyle[prop_String, token:{_String, _String}] :=
	If[MatchQ[token[[1]], "ident"],
		Switch[ToLowerCase @ token[[2]],
			"initial", initialValues @ prop,
			"inherit", Inherited,
			"none",    None, 
			"hidden",  None, (* 'hidden' is technically different from 'none', but I don't think the difference matters in WL *)
			"dotted",  Dotted,
			"dashed",  Dashed,
			"solid",   Dashing[{}],
			"double" | "groove" | "ridge" | "inset" | "outset",  Missing["Not supported."],
			_,         unrecognizedKeyWordFailure @ prop
		]
		,
		unrecognizedValueFailure @ prop
	]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-style"|"border-right-style"|"border-bottom-style"|"border-left-style", tokens:{{_String, _String}..}] := 
	Module[{value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderStyle[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-style", Top, "border-right-style", Right, "border-bottom-style", Bottom, "border-left-style", Left];
			FrameStyle -> wrapper[value]
		]
	]	
	
parse[prop:"border-style", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			skipWhitespace[pos, l, tokens];
			value = parseSingleBorderStyle[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; pos++];
		];
		Switch[Length[results],
			1, FrameStyle -> {{results[[1]], results[[1]]}, {results[[1]], results[[1]]}},
			2, FrameStyle -> {{results[[2]], results[[2]]}, {results[[1]], results[[1]]}},
			3, FrameStyle -> {{results[[2]], results[[2]]}, {results[[3]], results[[1]]}}, 
			4, FrameStyle -> {{results[[4]], results[[2]]}, {results[[3]], results[[1]]}},
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-width*)


(* WL FrameStyle thickness is best given as a calculated AbsoluteThickness for numerical values. *)
parseSingleBorderWidth[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"thin",    Thickness[Small],
				"medium",  Thickness[Medium],
				"thick",   Thickness[Large],
				_,         unrecognizedKeyWordFailure @ prop
			],
		"length" | "number", With[{n = parseLength @ token[[2]]}, negativeQ[n, prop, AbsoluteThickness[n]]],
		"ems" | "exs",       With[{n = parseLength @ token[[2]]}, negativeQ[First @ n, prop, AbsoluteThickness[n]]],
		_, unrecognizedValueFailure @ prop
	]


convertToCellThickness[x_] := Switch[x, AbsoluteThickness[_], First[x], Thickness[_], First[x] /. {Small -> 1, Medium -> 2, Large -> 4}, _, x]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-width"|"border-right-width"|"border-bottom-width"|"border-left-width", tokens:{{_String, _String}..}] := 
	Module[{value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderWidth[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			Switch[prop, "border-top-width", Top, "border-right-width",  Right, "border-bottom-width", Bottom, "border-left-width", Left];
			{FrameStyle -> wrapper[value], Cell[CellFrame -> wrapper[convertToCellThickness @ value]]}
		]
	]	
	
(* sets all frame edge thickness at once *)
parse[prop:"border-width", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			skipWhitespace[pos, l, tokens];
			value = parseSingleBorderWidth[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; pos++];
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {{results[[1]], results[[1]]}, {results[[1]], results[[1]]}},
				2, {{results[[2]], results[[2]]}, {results[[1]], results[[1]]}},
				3, {{results[[2]], results[[2]]}, {results[[3]], results[[1]]}}, 
				4, {{results[[4]], results[[2]]}, {results[[3]], results[[1]]}},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{FrameStyle -> results, Cell[CellFrame -> Map[convertToCellThickness, results, {2}]]}
	]


(* ::Subsubsection::Closed:: *)
(*border (-top, -bottom, -right, -left)*)


(* 
	Shorthand for border-*-width/style/color. 
	This effectively resets all edges because any property not specified takes on its default value. 
	'border' by itself sets all 4 edges to be the same.
*)
parse[prop:"border"|"border-top"|"border-right"|"border-bottom"|"border-left", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], p, value, init, dirAll, dir, start, stop, acquiredColor = False, acquiredDashing = False, acquiredThickness = False},
		init = <|"color" -> None, "dashing" -> None, "thickness" -> None|>; 
		
		If[l == 1, (* if only one token is present, then it should be a keyword *)
			Return @  
				Switch[ToLowerCase @ tokens[[1, 2]],
					"inherit", Inherited,
					"initial", None, (* because border-style is 'none', all properties are reset *)
					"none",    None,
					_,         unrecognizedKeyWordFailure @ prop
				]
		];
		
		(* Ignore 'inherit' and 'initial' keywords because they are ambiguous. Other keywords are unique. *)
		While[pos <= l,
			If[!MatchQ[ToLowerCase @ tokens[[pos, 2]], "initial" | "inherit" | "none"],
				start = pos; If[tokens[[pos, 1]] == "function", While[pos < l && tokens[[pos, 1]] != ")", pos++]]; stop = pos;
				value = {
					parseSingleColor[prop, tokens[[start ;; stop]]],
					parseSingleBorderStyle[prop, First @ tokens[[start ;; stop]]],
					parseSingleBorderWidth[prop, First @ tokens[[start ;; stop]]]};
				p = FirstPosition[value, Except[_?FailureQ], Return @ unrecognizedValueFailure @ prop, {1}, Heads -> False][[1]];
				Switch[p, 
					1, If[acquiredColor,     Return @ repeatedPropValueFailure @ "color", init["color"] = value[[p]];     acquiredColor = True], 
					2, If[acquiredDashing,   Return @ repeatedPropValueFailure @ "style", init["dashing"] = value[[p]];   acquiredDashing = True], 
					3, If[acquiredThickness, Return @ repeatedPropValueFailure @ "width", init["thickness"] = value[[p]]; acquiredThickness = True]
				];
				,
				Return @ unrecognizedValueFailure @ prop
			];
			pos++; skipWhitespace[pos, l, tokens];
		];
		(* reset all sides to their initial values *)
		{
			FrameStyle -> (
				dirAll = Map[initialValues, {{"border-left", "border-right"}, {"border-bottom", "border-top"}}, {2}];
				dir = Directive @@ {If[acquiredColor, init["color"], Nothing], If[acquiredDashing, init["dashing"], Nothing], If[acquiredThickness, init["thickness"], Nothing]};
				Switch[prop,
					"border",        dirAll = {{dir, dir}, {dir, dir}},
					"border-top",    dirAll[[2, 2]] = dir,
					"border-right",  dirAll[[1, 2]] = dir,
					"border-bottom", dirAll[[2, 1]] = dir,
					"border-left",   dirAll[[1, 1]] = dir
				];
				dirAll),
			
			Cell[
				CellFrameColor -> If[acquiredColor, init["color"], Black],
				CellFrame -> (
					dirAll = Map[initialValues, {{"border-left", "border-right"}, {"border-bottom", "border-top"}}, {2}];
					dir = If[acquiredThickness, convertToCellThickness @ init["thickness"], None];
					Switch[prop,
						"border",        dirAll = {{dir, dir}, {dir, dir}},
						"border-top",    dirAll[[2, 2]] = dir,
						"border-right",  dirAll[[1, 2]] = dir,
						"border-bottom", dirAll[[2, 1]] = dir,
						"border-left",   dirAll[[1, 1]] = dir
					];
					dirAll)]
		}
	]


(* ::Subsection::Closed:: *)
(*clip*)


parse[prop:"clip", tokens:{{_String, _String}..}] := 
	Module[{value, pos = 1, l = Length[tokens], start, stop},
		If[l == 1, (* if only one token is present, then it should be a keyword *)
			Switch[tokens[[1, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[1, 2]],
						"initial",  Missing["Not supported."],
						"inherit",  Missing["Not supported."],
						"auto",     Missing["Not supported."],
						_, unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			]
			,
			If[tokens[[1, 1]] == "function", 
				start = pos; While[pos < l && tokens[[pos, 1]] != ")", pos++]; stop = pos; 
				If[pos < l, Return @ tooManyTokensFailure @ tokens]; (* only one rect(...) is allowed *)
				value = parseRect @ tokens[[start ;; stop]]; (* FIXME: actually parse this? is it even worth it since it's not supported? *)
				If[FailureQ[value], value, Missing["Not supported."]]
				,
				unrecognizedValueFailure @ prop
			]
		]
	]


(* ::Subsection::Closed:: *)
(*color*)


(* 
	The color interpreter appears to mostly follow the CSS-color-3 module.
	It would be nice if it gave a more detailed failure message, but let's not reinvent the wheel.
*)
parse[prop:"color", tokens:{{_String, _String}..}] := parseSingleColor[prop, tokens]


(* ::Subsection::Closed:: *)
(*font *)


(* ::Subsubsection::Closed:: *)
(*font*)


(*
	Short-hand for other font properties. If not a keyword, then font-size and font-family are required, in that order.
	[['font-style' || 'font-variant' || 'font-weight' ]? 'font-size' [ / 'line-height' ]? 'font-family' ]
	All font properties are reset to their initial values, then the listed properties are calculated.
*)
parse[prop:"font", tokens:{{_String, _String}..}] :=
	Module[{pos = 1, l = Length[tokens], value, newValue = {}, temp},
		(* reset font properties *)
		value = {
			FontFamily  -> initialValues["font-family"], 
			FontSize    -> initialValues["font-size"], 
			FontSlant   -> initialValues["font-style"],
			FontVariations -> {"CapsType" -> initialValues["font-variant"]},
			FontWeight  -> initialValues["font-weight"],
			LineSpacing -> initialValues["line-height"]};
		
		(* parse and assign new font values *)
		If[l == 1, (* if only one token is present, then it should be a keyword that represents a system font *)
			newValue = 
				Switch[ToLowerCase @ tokens[[1, 2]],
					"caption" | "icon" | "menu" | "small-caption", 
						{FontFamily :> CurrentValue["ControlsFontFamily"], FontSize :> CurrentValue["ControlsFontSize"]},
					"message-box" | "status-bar", 
						{FontFamily :> CurrentValue["PanelFontFamily"], FontSize :> CurrentValue["PanelFontSize"]},
					"inherit", (* not sure why you would use this since font properties are inherited anyway...? *)
						{
							FontFamily  -> Inherited, 
							FontSize    -> Inherited, 
							FontSlant   -> Inherited,
							FontVariations -> {"CapsType" -> Inherited},
							FontWeight  -> Inherited,
							LineSpacing -> Inherited},
					"initial", {} (* keep reset values *),
					_, unrecognizedKeyWordFailure @ prop
				];
			Return @ If[FailureQ[newValue], newValue, DeleteDuplicates[Join[newValue, value], SameQ[First[#1], First[#2]]&]]
		];
		
		(* 
			font-style, font-variant, and font-weight can appear in any order, but are optional.
			"normal" values are skipped since "normal" is the initial value of these properties.
			Besides "normal", the keywords of each property are unique (ignoring 'inherit').
		*)
		(* FIXME: could check that property is not duplicated like we do in e.g. border-top *)
		While[pos <= l && FailureQ[temp = parse["font-size", {tokens[[pos]]}]],
			If[!MatchQ[ToLowerCase @ tokens[[pos, 2]], "normal" | "initial" | "inherit"],
				AppendTo[newValue, FirstCase[parse[#, {tokens[[pos]]}]& /@ {"font-style", "font-variant", "font-weight"}, _Rule, Nothing]]
			];
			pos++; skipWhitespace[pos, l, tokens];
		];
		
		(* font-size must appear next *)
		If[pos > l, Return @ noValueFailure["font-size"], AppendTo[newValue, temp]; pos++];
		
		(* an optional line-height property can immediately follow font-size with a '/' in between (no whitespace allowed) *)
		If[pos > l, Return @ noValueFailure["font-family"]];
		If[MatchQ[tokens[[pos]], {"operator", "/"}],
			pos++; 
			temp = parse["line-height", {tokens[[pos]]}]; 
			If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]; pos++; skipWhitespace[pos, l, tokens]];
		];
		
		(* everything else must be a font-family *)
		temp = parse["font-family", tokens[[pos ;;]]];
		If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]];
		
		(* overwrite old with any new values *)
		DeleteDuplicates[Join[newValue, value], SameQ[First[#1], First[#2]]&]
	]


(* ::Subsubsection::Closed:: *)
(*font-family*)


parse[prop:"font-family", tokens:{{_String, _String}..}] := 
	Module[{fontTokens, parsed, result},
		fontTokens = DeleteCases[SplitBy[tokens, MatchQ[{"operator", ","}]], {{"operator", ","}}];
		parsed = parseSingleFontFamily /@ fontTokens;
		result = FirstCase[parsed, _Failure, None];
		If[FailureQ[result], Return @ result];
		FirstCase[parsed, _Rule, Failure["UnexpectedParse", <|"Message" -> "No font-family found."|>]]
	]

parseSingleFontFamily[tokens:{{_String, _String}..}] :=
	Module[
	{
		value, l, pos = 1, font, tokensNoWS,
		generic = {"serif", "sans-serif", "monospace", "fantasy", "cursive"},
		fail = Failure["UnexpectedParse", <|"Message" -> "Font family syntax error."|>]
	},
		tokensNoWS = DeleteCases[tokens, {"whitespace", _}];
		l = Length[tokensNoWS];
		value =
			Switch[tokensNoWS[[pos, 1]],
				"ident", (* all other tokens must be 'ident' (or whitespace); it could be only be a single 'ident' *)
					Which[
						!AllTrue[tokensNoWS[[All, 1]], StringMatchQ["ident"]], fail,
						l == 1 && MemberQ[generic, ToLowerCase @ tokensNoWS[[pos, 2]]], parseFontFamilySingleIdent @ tokensNoWS[[pos, 2]],
						True, 
							font = StringJoin @ Riffle[tokensNoWS[[All, 2]], " "];
							If[MemberQ[$FontFamilies, font], font, Missing["FontAbsent", font]]
					],
				"string", (* must only have a single string token up to the delimiting comma *)
					Which[
						l > 1, fail,
						True,
							font = StringTake[tokensNoWS[[All, 2]], {2, -2}];
							If[MemberQ[$FontFamilies, font], font, Missing["FontAbsent", font]]
					],
				_, fail
			];
		If[FailureQ[value] || MissingQ[value], value, FontFamily -> value]
	]
	
(*TODO:
	Perhaps as an option we should allow the user to define default fonts, e.g.
		GenericFontFamily \[Rule] {"fantasy" \[Rule] "tribal-dragon"}
	but always default to Automatic if it can't be found on the system.
*)
parseFontFamilySingleIdent[s_String] := 
	Switch[ToLowerCase @ s,
		"inherit",    Inherited,
		"initial",    initialValues["font-family"], 
		"serif",      "Times New Roman",
		"sans-serif", "Arial",
		"cursive",    
			If[MemberQ[$FontFamilies, "French Script MT"], 
				"French Script MT" (* I have a preference for this, but 'Brush Script' should be available on Mac, unsure on Linux. *)
				, 
				First[
					Select[$FontFamilies, StringContainsQ[#, "script", IgnoreCase -> True] && !StringContainsQ[#, "bold", IgnoreCase -> True]&],
					Automatic]
			],
		"fantasy",    Automatic, (* no system has a default 'fantasy' font *)
		"monospace",  
			Which[
				MemberQ[$FontFamilies, "Source Code Pro"],  "Source Code Pro",
				MemberQ[$FontFamilies, "Consolas"],         "Consolas",
				MemberQ[$FontFamilies, "Courier"],          "Courier",
				True, Automatic
			],
		_, unrecognizedKeyWordFailure @ "font-family"
	]


(* ::Subsubsection::Closed:: *)
(*font-size*)


parse[prop:"font-size", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[1, 2]],
						"initial",  initialValues[prop],
						"inherit",  Inherited,
						"larger",   Larger,
						"smaller",  Smaller,
						"xx-small", Tiny (*6*),
						"x-small",  8,
						"small",    Small (*9*),
						"medium",   Medium (*12*),
						"large",    Large (*24*),
						"x-large",  30,
						"xx-large", 36,
						_, unrecognizedKeyWordFailure @ prop
					],
				"percentage",        With[{n = parsePercentage @ tokens[[1, 2]]}, negativeQ[n, prop, Scaled[n/100]]],
				"length" | "number", With[{n = parseLength[#, True]& @ tokens[[1, 2]]}, negativeQ[n, prop, n]],
				"ems" | "exs",       With[{n = parseLength[#, True]& @ tokens[[1, 2]]}, negativeQ[First @ n, prop, n]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontSize -> value]
	]


(* ::Subsubsection::Closed:: *)
(*font-style*)


parse[prop:"font-style", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"inherit", Inherited,
						"initial", initialValues @ prop, 
						"normal",  Plain,
						"italic",  Italic,
						"oblique", "Oblique",
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontSlant -> value]
	]


(* ::Subsubsection::Closed:: *)
(*font-variant*)


parse[prop:"font-variant", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"inherit",    Inherited,
						"initial",    initialValues @ prop,
						"normal",     "Normal",
						"small-caps", "SmallCaps",
						_,            unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontVariations -> {"CapsType" -> value}]
	]


(* ::Subsubsection::Closed:: *)
(*font-weight*)


(*
	"lighter" and "bolder" are not supported.
	Weight mappings come from https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight.
*)
parse[prop:"font-weight", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"inherit", Inherited,
						"initial", initialValues[prop], 
						"normal"|"book"|"plain"|"regular"|"roman", Plain,
						"bold", Bold,
						"medium", "Medium",
						"heavy", "Heavy",
						"demi"|"demibold"|"semibold", "SemiBold",
						"black"|"ultra"|"ultrablack"|"extrablack"|"extrabold"|"ultrabold", "Black",
						"fat"|"obese", "Fat",
						"hairline"|"thin", "Thin",
						"extralight"|"ultralight"|"light", "Light",
						"lighter" | "bolder", Missing["Not supported."],
						_, unrecognizedKeyWordFailure @ prop
					],
				"number", 
					First[
						Nearest[
							{
								100 -> "Thin", (* Hairline *)
								(* 200 \[Rule] "Extra Light", *) (* Ultra Light *)
								300 -> "Light", 
								400 -> Plain, 
								500 -> "Medium", 
								600 -> "SemiBold", (* Demi Bold *)
								700 -> Bold,
								(* 800 \[Rule] "Extra Bold", *) (* Ultra Bold *)
								900 -> "Black" (* Heavy *)}, 
							Clip[Interpreter["Number"][tokens[[1, 2]]], {1, 1000}]], 
						Automatic],
				_, Failure["UnexpectedParse", <|"Message" -> "Unrecognized font weight."|>]
			];
		If[FailureQ[value], value, FontWeight -> value]
	]


(* ::Subsection::Closed:: *)
(*height, width (max/min)*)


(* 
	CSS height/width   --> WL ImageSize 
	CSS max/min-height --> WL ImageSize with UpTo (only max) or {{wmin, wmax}, {hmin, hmax}} (both max and min)
	CSS overflow       --> WL ImageSizeAction
*)
parseSingleSize[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"auto",    Automatic, (* let Mathematica decide what to do *)
				"none",    If[!StringMatchQ[prop, "max-height" | "max-width"], unrecognizedKeyWordFailure @ prop, Infinity],
				_,         unrecognizedKeyWordFailure @ prop
			],
		"length" | "number", With[{n = parseLength @ token[[2]]}, negativeQ[n, prop, n]],
		"ems" | "exs",       With[{n = parseLength @ token[[2]]}, negativeQ[First @ n, prop, n]],
		"percentage",        Scaled[(parsePercentage @ token[[2]])/100], (* should be percentage of height of containing block; not possible in WL *)
		_, unrecognizedValueFailure @ prop
	]
	
(* min-width and max-width override width property *)
parse[prop:"width"|"max-width"|"min-width", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			ImageSize -> 
				Switch[prop,
					"width",     value,
					"max-width", {UpTo[value], Automatic},
					"min-width", {{value, Automatic}, Automatic}
				]
		]
	]

parse[prop:"height"|"max-height"|"min-height", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			ImageSize -> 
				Switch[prop,
					"height",     {Automatic, value},
					"max-height", {Automatic, UpTo[value]},
					"min-height", {Automatic, {value, Automatic}}
				]
		]
	]


(* ::Subsection::Closed:: *)
(*line-height*)


(* Similar to WL LineSpacing, but LineSpacing already takes FontSize into account, so intercept number before 'ems' returns CurrentValue[FontSize] *)
parse[prop:"line-height", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"inherit", Inherited,
						"initial", initialValues[prop], 
						"normal",  {1.2, 0},
						_,         unrecognizedKeyWordFailure @ prop
					],
				"number",      With[{n = Interpreter["Number"] @ tokens[[1, 2]]}, negativeQ[n, prop, {n, 0}]],
				"length",      With[{n = parseLength @ tokens[[1, 2]]},           negativeQ[n, prop, {n, 0}]],
				"ems" | "exs", With[{n = parseEmNonRelative @ tokens[[1, 2]]},    negativeQ[n, prop, {n, 0}]],
				"percentage",  With[{n = parsePercentage @ tokens[[1, 2]]},       negativeQ[n, prop, {n/100, 0}]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, LineSpacing -> value]
	]


(* ::Subsection::Closed:: *)
(*list-style*)


(* ::Subsubsection::Closed:: *)
(*list-style-image*)


parseSingleListStyleImage[prop_String, token:{_String, _String}] := 
	Switch[token[[1]],
		"ident",
			Switch[ToLowerCase @ token[[2]],
				"inherit", Inherited,
				"initial", initialValues @ prop, 
				"none",    None,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"uri", 
			With[{im = Import[token[[2]]]}, 
				Which[
					FailureQ[im], couldNotImportFailure @ token[[2]], 
					!ImageQ[im],  notAnImageFailure @ token[[2]],
					_,            ToBoxes @ Dynamic @ Image[im, ImageSize -> CurrentValue[FontSize]]
				]
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-image", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleImage[prop, First @ tokens];
		If[FailureQ[value], value, Cell[CellDingbat -> value]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-position*)


(* 
	CellDingbat position is always outside the cell content and aligned with the first line of content.
	Though the following validates the CSS, Mathematica does not include any position option.
*)
parseSingleListStylePosition[prop_String, token:{_String, _String}] := 
	Switch[token[[1]],
		"ident",
			Switch[ToLowerCase @ token[[2]],
				"inherit", Inherited,
				"initial", initialValues @ prop, 
				"inside",  Missing["Not supported."],
				"outside", Automatic,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-position", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStylePosition[prop, First @ tokens];
		If[FailureQ[value], value, Cell[Missing["Not supported."]]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-type*)


parseSingleListStyleType[prop_String, token:{_String, _String}] := 
	Switch[token[[1]],
		"ident",
			Switch[ToLowerCase @ token[[2]],
				"inherit",              Inherited,
				"initial",              initialValues @ prop, 
				"disc",                 "\[FilledCircle]",
				"circle",               "\[EmptyCircle]",
				"square",               "\[FilledSquare]",
				"decimal",              Cell[TextData[{CounterBox["Item"], "."}]],
				"decimal-leading-zero", Cell[TextData[{CounterBox["Item", CounterFunction :> (FEPrivate`If[FEPrivate`Greater[#, 9], #, FEPrivate`StringJoin["0", FEPrivate`ToString[#]]]&)], "."}]],
				"lower-roman",          Cell[TextData[{CounterBox["Item", CounterFunction :> FrontEnd`RomanNumeral], "."}]],
				"upper-roman",          Cell[TextData[{CounterBox["Item", CounterFunction :> FrontEnd`CapitalRomanNumeral], "."}]],
				"lower-greek",          Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["\[Alpha]", "\[Omega]"], #]&)], "."}]],
				"lower-latin",          Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["a", "z"], #]&)], "."}]],
				"upper-latin",          Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)], "."}]],
				"armenian",             Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["\:0531", "\:0556"], #]&)], "."}]],
				"georgian",             Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["\:10d0", "\:10fa"], #]&)], "."}]],
				"lower-alpha",          Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["a", "z"], #]&)], "."}]],
				"upper-alpha",          Cell[TextData[{CounterBox["Item", CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)], "."}]],
				"none",                 None,
				_,                      unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-type", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleType[prop, First @ tokens];
		If[FailureQ[value], value, Cell[CellDingbat -> value]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style*)


(* short-hand for list-style-image/position/type properties given in any order *)
parse[prop:"list-style", tokens:{{_String, _String}..}] :=
	Module[{pos = 1, l = Length[tokens], value, p, noneCount = 0, acquiredImage = False, acquiredPos = False, acquiredType = False},
		(* parse and assign new font values *)
		If[l == 1, 
			value = 
				Switch[ToLowerCase @ tokens[[1, 2]],
					(* not sure why you would use this since list-style properties are inherited anyway...? *)
					"inherit", Inherited,
					"initial", None,
					"none",    None,
					_,         unrecognizedKeyWordFailure @ prop
				];
			Return @ If[FailureQ[value], value, Cell[CellDingbat -> value]]
		];
		
		(* 
			li-image, li-position, and li-type can appear in any order.
			A value of 'none' sets whichever of li-type and li-image are not otherwise specified to 'none'. 
			If both are specified, then an additional 'none' is an error.
		*)
		value = <|"image" -> None, "pos" -> Missing["Not available."], "type" -> None|>;
		While[pos <= l,
			If[TrueQ[ToLowerCase @ tokens[[pos, 2]] == "none"], 
				noneCount++
				,
				value = {
					parseSingleListStyleImage[prop, First @ tokens[[pos]]],
					parseSingleListStylePosition[prop, First @ tokens[[pos]]],
					parseSingleListStyleType[prop, First @ tokens[[pos]]]};
					p = FirstPosition[value, Except[_?FailureQ], Return @ unrecognizedValueFailure @ prop, {1}, Heads -> False][[1]];
				Switch[p, 
					1, If[acquiredImage, Return @ repeatedPropValueFailure @ "image",    value["image"] = value[[p]]; acquiredImage = True], 
					2, If[acquiredPos,   Return @ repeatedPropValueFailure @ "position", value["pos"] = value[[p]];   acquiredPos = True], 
					3, If[acquiredType,  Return @ repeatedPropValueFailure @ "type",     value["type"] = value[[p]];  acquiredType = True]
				];
			];
			pos++; skipWhitespace[pos, l, tokens];
		];
		Which[
			acquiredImage && acquiredType && noneCount > 0, repeatedPropValueFailure @ "none",
			acquiredImage, Cell[CellDingbat -> value["image"]], (* default to Image if it could be found *)
			acquiredType,  Cell[CellDingbat -> value["type"]],
			True,          Cell[CellDingbat -> None]]
		]
	]


(* ::Subsection::Closed:: *)
(*margin(-left, -right, -top, -bottom)*)


parseSingleMargin[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"auto",    Automatic, (* let Mathematica decide what to do *)
				_,         unrecognizedKeyWordFailure @ prop
			],
		"length" | "number" | "ems" | "exs", parseLength @ token[[2]] (* can be positive, negative, or 0 *),
		"percentage", Scaled[(parsePercentage @ token[[2]])/100],
		_, unrecognizedValueFailure @ prop
	]


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
parse[prop:"margin-top"|"margin-right"|"margin-bottom"|"margin-left", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], wrapper, value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleMargin[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "margin-left", Left, "margin-right", Right, "margin-bottom", Bottom, "margin-top", Top];
			{ImageMargins -> wrapper[value], Cell[CellMargins -> wrapper[value]]}
		]
	]
		
parse[prop:"margin", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleMargin[prop, tokens[[pos]]];
			If[FailureQ[value], 
				Return @ value
				, 
				AppendTo[results, value]; pos++; skipWhitespace[pos, l, tokens];
			];
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {{results[[1]], results[[1]]}, {results[[1]], results[[1]]}},
				2, {{results[[2]], results[[2]]}, {results[[1]], results[[1]]}},
				3, {{results[[2]], results[[2]]}, {results[[3]], results[[1]]}}, 
				4, {{results[[4]], results[[2]]}, {results[[3]], results[[1]]}},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{ImageMargins -> results, Cell[CellMargins -> results]}
	]


(* ::Subsection::Closed:: *)
(*overflow*)


parse[prop:"overflow", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens]},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[ToLowerCase @ tokens[[1, 2]],
			"visible", Missing["Not supported."],
			"hidden",  {ImageSizeAction -> "Clip", Scrollbars -> False},
			"scroll",  {ImageSizeAction -> "Clip", Scrollbars -> True},
			"auto",    ImageSizeAction -> "Scrollable",
			"inherit", ImageSizeAction -> Inherited,
			"initial", initialValues @ prop,
			_, unrecognizedKeyWordFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*padding(-left, -right, -top, -bottom)*)


parseSinglePadding[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"initial", initialValues @ prop,
				"inherit", Inherited,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"length" | "number", With[{n = parseLength @ token[[2]]},     negativeQ[n, prop, n]],
		"ems" | "exs",       With[{n = parseLength @ token[[2]]},     negativeQ[First @ n, prop, n]],
		"percentage",        With[{n = parsePercentage @ token[[2]]}, negativeQ[n, prop, Scaled[n/100]]],
		_, unrecognizedValueFailure @ prop
	]


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
parse[prop:"padding-top"|"padding-right"|"padding-bottom"|"padding-left", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], wrapper, value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSinglePadding[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "padding-left", Left, "padding-right", Right, "padding-bottom", Bottom, "padding-top", Top];
			{FrameMargins -> wrapper[value], Cell[CellFrameMargins -> wrapper[value]]}
		]
	]
		
parse[prop:"padding", tokens:{{_String, _String}..}] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSinglePadding[prop, tokens[[pos]]];
			If[FailureQ[value], 
				Return @ value
				, 
				AppendTo[results, value]; pos++; skipWhitespace[pos, l, tokens];
			];
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {{results[[1]], results[[1]]}, {results[[1]], results[[1]]}},
				2, {{results[[2]], results[[2]]}, {results[[1]], results[[1]]}},
				3, {{results[[2]], results[[2]]}, {results[[3]], results[[1]]}}, 
				4, {{results[[4]], results[[2]]}, {results[[3]], results[[1]]}},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{FrameMargins -> results, Cell[CellFrameMargins -> results]}
	]


(* ::Subsection::Closed:: *)
(*vertical-align*)


(* 
	Applies to in-line elements or rows in a table. WL's BaselinePosition is similar. 
	Lengths and percentages are w.r.t. the baseline of the surrounding element.
	Percentages are relative to the font-size i.e. 100% is one line-height upward.
	CellBaseline is limited and always aligns the top of the inline cell to the Bottom/Top/Etc of the parent
*)
parse[prop:"vertical-align", tokens:{{_String, _String}..}] :=
	Module[{value1, value2},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value1 = parseBaseline[prop, tokens];
		If[FailureQ[value1], Return @ value1];
		value2 = parseCellBaseline[prop, tokens];
		If[FailureQ[value2], Return @ value2];
		AmbiguityList[{value1, value2}]
	]

(* this is effectively for RowBox alignment *)
parseBaseline[prop:"vertical-align", tokens:{{_String, _String}..}] := 
	Module[{value (* for Baseline *), value2 (* for CellBaseline *)},
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"baseline",    Baseline -> Baseline,
						"sub",         Baseline -> Bottom,
						"super",       Baseline -> Axis, (* maybe not the best approximation *)
						"top",         Top -> Top, 
						"text-top",    Top -> Scaled[1],
						"middle",      Center -> Scaled[0.66], (* center plus half an "x" height, i.e. 50%+33%/2 = 66% *)
						"bottom",      Bottom -> Bottom,
						"text-bottom", Bottom -> Scaled[0],
						"inherit",     Inherited,
						"initial",     initialValues @ prop,
						_,             unrecognizedKeyWordFailure @ prop
					],
				"length" | "number", Baseline -> With[{v = parseLength @ tokens[[1, 2]]},        Scaled @ Dynamic[v/CurrentValue[FontSize]]], (* could be zero *)
				"ems" | "exs" ,      Baseline -> With[{v = parseEmNonRelative @ tokens[[1, 2]]}, Scaled @ v],
				"percentage",        Baseline -> With[{n = parsePercentage @ tokens[[1, 2]]},    Scaled[n/100]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, BaselinePosition -> value]
	]

(* it's unfortunate that CellBaseline is so limited *)
parseCellBaseline[prop:"vertical-align", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"baseline", Center,
						"middle",   Baseline,
						"super" | "sub", Missing["Not available."],
						"top" | "text-top", Missing["Not available."], (* because top of in-line is at baseline of cell *)
						"bottom" | "text-bottom", Bottom,
						"inherit", Inherited,
						"initial", Baseline,
						_, unrecognizedKeyWordFailure @ prop
					],
				"length" | "number", parseLength @ tokens[[1, 2]], (* w.r.t. the top of the in-line cell *)
				"ems" | "exs" | "percentage", Missing["Not available."],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Cell[CellBaseline -> value]]
	]


(* ::Subsection::Closed:: *)
(*FALL THROUGH *)


parse[prop_String, {}] := noValueFailure @ prop


(* ::Section::Closed:: *)
(*Process *)


(* ::Subsection::Closed:: *)
(*Utilities*)


findOpeningBracketPosition[positionIndex_Integer, openBracket_String, tokens:{{_String, _String}..}] :=
	Module[{p = positionIndex, depth = 1, l = Length[tokens]},
		While[tokens[[p, 1]] != openBracket, p++];
		p
	]


findClosingBracketPosition[positionIndex_Integer, openBracket_String, closeBracket_String, tokens:{{_String, _String}..}]:=
	Module[{p = positionIndex, depth = 1, l = Length[tokens]},
		p++;
		While[depth > 0 && p <= l,
			Switch[tokens[[p ,1]],
				openBracket,  depth++; p++,
				closeBracket, depth--; p++,
				_, p++
			]
		];
		p-1
	]


Attributes[skipWhitespace] = {HoldFirst};
skipWhitespace[positionIndex_, length_Integer, tokens_List] := 
	While[positionIndex < length && tokens[[positionIndex, 1]] == "whitespace", positionIndex++]


(* ::Subsection::Closed:: *)
(*Process rulesets ---> {{selector, declaration block}, ...}*)


processRulesets[s_String] :=
	Module[{i, pos, startPosBlock, stopPosBlock, l, lRulesets, relevantTokens, rulesets},
	
		relevantTokens = parseBlock[s];
		(* TODO: need to handle @import and other preprocessing before rulesets *)
	
		pos = 1;
		l = Length[relevantTokens];
		i = 1;
		(* 
			We're looking for blocks indicated by curly brackets. 
			The last declaration block may not have a closing bracket, so count only open brackets as an upper limit to the number of possible blocks.
		*)
		lRulesets = Count[relevantTokens, {"{", _}];
		rulesets = ConstantArray[0, lRulesets];
		While[pos < l && i <= lRulesets,
			startPosBlock = findOpeningBracketPosition[pos, "{", relevantTokens];
			(*
				'stopPosBlock' could reach the end of the token list, but not necessarily be a closed bracket '}'.
			*)
			stopPosBlock  = findClosingBracketPosition[startPosBlock, "{", "}", relevantTokens];
			rulesets[[i]] = (*FIXME: using associations too soon. Keep using nested lists for now*)
				<|
					"Selector" -> StringJoin @ relevantTokens[[pos ;; startPosBlock-1, 2]], 
					"Block" -> relevantTokens[[startPosBlock+1 ;; If[relevantTokens[[stopPosBlock, 1]] == "}", stopPosBlock-1, stopPosBlock]]]|>;
			pos = stopPosBlock + 1;
			i++
		];
		(* remove possible excess blocks *)
		DeleteCases[rulesets, 0, {1}]
	]


(* ::Subsection::Closed:: *)
(*Process declarations (within all blocks)*)


processDeclarations[rulesets:{_Association..}] :=
	Module[{aCopy, temp},
		aCopy = rulesets;
		temp = aCopy[[All, "Block"]];
		temp = temp /. {"other", x_String} :> Sequence @@ parseDeclaration[x];
		temp = processDeclarationBlock /@ temp;
		aCopy[[All, "Block"]] = temp;
		aCopy
	]		


processDeclarationBlock[tokens:{{_String, _String}..}] :=
	Module[{pos, l, lDeclarations, i, propertyPosition, valueStartPosition, valueStopPosition, declarations, important},
		pos = 1;
		l = Length[tokens];
		i = 1;
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[tokens, {":", _}];
		declarations = ConstantArray[0, lDeclarations];
		While[pos < l && i <= lDeclarations,
			skipWhitespace[pos, l, tokens];
			If[tokens[[pos, 1]] == "ident",
				propertyPosition = pos; pos++; skipWhitespace[pos, l, tokens];
				If[tokens[[pos, 1]] == ":",
					pos++; skipWhitespace[pos, l, tokens];
					valueStartPosition = pos;
					(* check for 'important' token, which would be the last token before ';' *)
					While[!MatchQ[tokens[[pos, 1]], ";" | "important"] && pos < l, pos++];
					Switch[tokens[[pos, 1]],
						"important", 
							important = True; 
							valueStopPosition = pos-1;
							pos++; skipWhitespace[pos, l, tokens];
							If[tokens[[pos, 1]] == ";", 
								pos++
								, 
								(* syntax error; reset values and let a further parser flag the error *)
								valueStopPosition = pos; important = False; While[tokens[[pos, 1]] != ";" && pos < l, pos++]
							];,
						";", 
							important = False;
							valueStopPosition = pos-1;,
						_,
							important = False;
							valueStopPosition = pos;
					];
					While[tokens[[valueStopPosition, 1]] == "whitespace", valueStopPosition--]; (* trim whitespace from the end of the value *)
					declarations[[i]] = <|
						"Important" -> important,
						"Property" -> ToLowerCase @ tokens[[propertyPosition, 2]], 
						"Value" -> (*check for empty property*)If[valueStopPosition < valueStartPosition, {}, tokens[[valueStartPosition ;; valueStopPosition]]],
						"Interpretation" -> None|>;
					pos++;
					,
					While[tokens[[pos, 1]] != ";" && pos < l, pos++];
				];
				,
				While[tokens[[pos, 1]] != ";" && pos < l, pos++];
			];
			i++;
		];					
		(* remove possible excess declarations *)
		DeleteCases[declarations, 0, {1}]
	]


(* ::Subsection::Closed:: *)
(*Process Properties*)


process[property_String, a:{__Association}] :=
	Module[{valuePositions, interpretationPositions, tokens, values, interpretations},
		{valuePositions, interpretationPositions} = getPropertyPositions[property, a];
		tokens = Extract[a, valuePositions];
		{values, interpretations} = {StringJoin /@ tokens[[All, All, 2]], parse[property, #]& /@ tokens};
		ReplacePart[a, Join[Thread[valuePositions -> values], Thread[interpretationPositions -> interpretations]]]
	]


getPropertyPositions[property_String, a:{__Association}] :=
	Module[{propertyPositions, valuePositions, interpretationPositions},
		propertyPositions = Position[a, s_String /; StringMatchQ[s, property, IgnoreCase -> True]];
		propertyPositions = Cases[propertyPositions, {_Integer, Key["Block"], _Integer, Key["Property"]}, {1}];
		
		valuePositions = propertyPositions; 
		valuePositions[[All, -1]] = Key["Value"];
		
		interpretationPositions = propertyPositions; 
		interpretationPositions[[All, -1]] = Key["Interpretation"];
		
		{valuePositions, interpretationPositions}
	]


(* ::Subsubsection:: *)
(*other*)


(*TODO
processUnknowns[a:{__Association}] :=
	Module[{p, valuePositions, interpretationPositions, tokens},
		p = Position[a, s_String /; StringMatchQ[s, property, IgnoreCase -> True]];
		valuePositions = p; valuePositions[[All, -1]] = Key["Value"];
		interpretationPositions = p; interpretationPositions[[All, -1]] = Key["Interpretation"];
		
		tokens = Extract[a, valuePositions];
		
		ReplacePart[a, 
			Join[
				Thread[valuePositions -> StringJoin /@ tokens[[All, All, 2]]], 
				Thread[interpretationPositions -> (parse[property, #]& /@ tokens)]]]
	]*)


(* ::Section:: *)
(*Properties*)


(* ::Subsection:: *)
(*List of properties*)


(* ::Subsubsection:: *)
(*Strictly only CSS2.1 properties.*)


(*pTable = Import["https://www.w3.org/TR/2011/REC-CSS2-20110607/propidx.html", "Data"];
pTable = StringReplace[pTable[[2;;,1]], "'" -> ""] // StringSplit // Join // Flatten // Union;*)


(* ::Text:: *)
(*Aural:*)


{
	"azimuth", 
	"cue", "cue-after", "cue-before", 
	"elevation", 
	"pause", "pause-after", "pause-before", 
	"pitch", "pitch-range", 
	"play-during", "richness", 
	"speak", "speak-header", "speak-numeral", "speak-punctuation", 
	"speech-rate", "stress", "voice-family", "volume"};


(* ::Text:: *)
(*Visual:*)


visual = {
	"background", "background-attachment", (*"background-color",*) "background-image", "background-position", "background-repeat", 
	(*"border", *)
	(*"border-collapse", "border-spacing", *)
	(*"border-left", "border-right", "border-top", "border-bottom", *)
	(*"border-color", "border-left-color", "border-right-color", "border-top-color", "border-bottom-color", *)
	(*"border-style", "border-left-style", "border-right-style", "border-top-style", "border-bottom-style", *)
	(*"border-width", "border-left-width", "border-right-width", "border-top-width", "border-bottom-width", *)
	"bottom", "caption-side", "clear", (*"clip", *)
	(*"color", *)
	"direction", 
	"empty-cells", "float", 
	(*"font", "font-family", "font-size", "font-style", "font-variant", "font-weight",*) 
	(*"height", *)"left", "letter-spacing", (*"line-height", *)
	(*"list-style", "list-style-image", "list-style-position", "list-style-type",*) 
	(*"margin", "margin-bottom", "margin-left", "margin-right", "margin-top", *)
	(*"max-height", "max-width", "min-height", "min-width", *)
	(*"overflow", *)
	(*"padding", "padding-bottom", "padding-left", "padding-right", "padding-top", *)
	"position", "quotes", "right", "table-layout", 
	"text-align", "text-decoration", "text-indent", "text-transform", 
	"top", "unicode-bidi", (*"vertical-align",*) 
	"visibility", "white-space", "width", "word-spacing", "z-index"};


Length[visual]


(* ::Text:: *)
(*Visual + Interactive*)


{
	"cursor", 
	"outline", "outline-color", "outline-style", "outline-width"};


(* ::Text:: *)
(*Visual + Paged*)


{
	"orphans", 
	"page-break-after", "page-break-before", "page-break-inside", 
	"widows"};


(* ::Text:: *)
(*All:*)


{"content", "counter-increment", "counter-reset", "display"};


(* ::Subsubsection::Closed:: *)
(*Some already recommended CSS3 properties*)


(*pTableAll = Association /@ Import["https://www.w3.org/Style/CSS/all-properties.en.json", "JSON"];
allProps = Select[pTableAll, #status == "REC"&][[All, "property"]] // Union;
Complement[allProps, pTable]*)


{
	"box-sizing", "caret-color", 
	"font-feature-settings", "font-kerning", "font-size-adjust", "font-stretch", "font-synthesis", "font-variant-caps", 
	"font-variant-east-asian", "font-variant-ligatures", "font-variant-numeric", "font-variant-position", 
	"opacity", "outline-offset", "resize", "text-overflow"};


(* ::Subsection::Closed:: *)
(*Valid functions*)


(*table = Import["https://developer.mozilla.org/en-US/docs/Web/CSS/Reference", "Data"];
Select[Flatten[table[[5, 1]]], StringEndsQ[#, "()"]&]*)


(* CSS 2.1 function *)
(*validFunction21 = {
	"attr()", "rgb()", "counter()", "counters()"};*)

(*validFunctions = {
	"annotation()", "attr()", "blur()", "brightness()", "calc()", 
	"character-variant()", "circle()", "contrast()", "cross-fade()", "cubic-bezier()", 
	"drop-shadow()", "element()", "ellipse()", "fit-content()", "format()", 
	"frames()", "grayscale()", "hsl()", "hsla()", "hue-rotate()", 
	"image()", "image-set()", "inset()", "invert()", "leader()", 
	"linear-gradient()", "local()", "matrix()", "matrix3d()", "minmax()", 
	"opacity()", "ornaments()", "perspective()", "polygon()", "radial-gradient()", 
	"rect()", "repeat()", "repeating-linear-gradient()", "repeating-radial-gradient()", "rgb()", 
	"rgba()", "rotate()", "rotate3d()", "rotateX()", "rotateY()", 
	"rotateZ()", "saturate()", "scale()", "scale3d()", "scaleX()", 
	"scaleY()", "scaleZ()", "sepia()", "skew()", "skewX()", 
	"skewY()", "steps()", "styleset()", "stylistic()", "swash()", 
	"symbols()", "target-counter()", "target-counters()", "target-text()", "translate()", 
	"translate3d()", "translateX()", "translateY()", "translateZ()", "url()", 
	"var()"};*)


(*{
	"cross-fade()", 
	"image-set()", "leader()", 
	"local()", "minmax()", 
	"perspective()", 
	"repeat()", 
	"symbols()", "target-counter()", "target-counters()", "target-text()", 
	"url()", 
	"var()"}*)


(* ::Subsubsection::Closed:: *)
(*DEPRECATED*)


(* rect(), *)


(* ::Subsubsection::Closed:: *)
(*attr*)


(* CSS21: limited to 'content' prop, alwasy returns string 
	content:attr(IDENT);	
*)


(* ::Subsubsection::Closed:: *)
(*basic shape*)


(*
	circle([<shape-radius>]? [at <position>]? )
	ellipse([<shape-radius>{2}]? [at <position>]?)
	inset( <shape-arg>{1,4} [round <border-radius>]? )
	polygon([<fill-rule>,]? [<shape-arg> <shape-arg>]#)
*)


(* ::Subsubsection::Closed:: *)
(*calc (CSS3)*)


(* perform simple arithmetic on lengths, e.g. calc(1em + 10%) 
	spaces around +- required, encouraged for */
*)


(* ::Subsubsection::Closed:: *)
(*color*)


(*
hsl(ANGLE, PERCENTAGE, PERCENTAGE[, NUMBER | PERCENTAGE]),
hsla(ANGLE, PERCENTAGE, PERCENTAGE, NUMBER | PERCENTAGE),
rgb(NUMBER | PERCENTAGE, NUMBER | PERCENTAGE, NUMBER | PERCENTAGE[, NUMBER | PERCENTAGE] ), (* NUMBER \[LessEqual] 255 for rgb, 0<x<1 for a*)
rgba,
*)


(* ::Subsubsection::Closed:: *)
(*element (CSS3 Experimental)*)


(* experimental CSS *)


(* ::Subsubsection::Closed:: *)
(*filter-function*)


(*
blur(LENGTH);
brightness(NUMBER OR PERCENTAGE);
contrast(NUMBER OR PERCENTAGE);
drop-shadow(LENGTH LENGTH [LENGTH LENGTH COLOR]);
grayscale(NUMBER OR PERCENTAGE);
hue-rotate(ANGLE);
invert(NUMBER OR PERCENTAGE);
opacity(NUMBER OR PERCENTAGE);
saturate(NUMBER OR PERCENTAGE);
sepia(NUMBER OR PERCENTAGE);*)


(* ::Subsubsection::Closed:: *)
(*fit-content  (CSS3 Experimental)*)


(* experimental CSS *)


(* ::Subsubsection::Closed:: *)
(*@font-face (CSS3 format())*)


(* only used in @font-face *)


(* ::Subsubsection::Closed:: *)
(*font-variant-alternates*)


(*
annotation(IDENT);
character-variant(IDENT);
ornaments(IDENT);
styleset(IDENT);
stylistic(IDENT);
swash(IDENT);
swash(IDENT) annotation(IDENT);*)


(* ::InheritFromParent:: *)
(* *)


(* ::Subsubsection::Closed:: *)
(*gradients*)


(* linear-gradient(), "radial-gradient()", "repeating-linear-gradient()", "repeating-radial-gradient()", *)


(* ::Subsubsection:: *)
(*image*)


(* ::Subsubsection::Closed:: *)
(*single-transition-timing-function (CSS WD)*)


(*
cubic-bezier(NUMBER{4}#), 
frames(INTEGER),  --> not implemented in CSS
steps(INTEGER, start|end) *)


(* ::Subsubsection::Closed:: *)
(*transformations*)


(* "matrix()", "matrix3d()", "rotate()", "rotate3d()", "rotateX()", "rotateY()", 
	"rotateZ()", "scale()", "scale3d()", "scaleX()", "scaleY()", "scaleZ()", 
	"skew()", "skewX()", "skewY()", "translate()", "translate3d()", "translateX()", "translateY()", "translateZ()" *)


(* ::Section::Closed:: *)
(*Notes*)


(* ::Subsection::Closed:: *)
(*Check matching ()[]{}*)


(* ::Text:: *)
(*I shouldn't have to do this. CSS is very flat in its syntax in that curly brackets and square brackets cannot be nested (as far as I can tell!). Parentheses can be nested, but only for functions e.g. rgba(0,0,0,calc(1+1)) but would still be rare.*)
(*After tokenization, I should only have to consume tokens until the relevant super-structure is consumed, i.e. at-rules and rule sets. Then parse the super-structures.*)


(*findBlocks[tokens_List] := 
	Module[{labels1 = {}, result, start, stop, pos, uuids},
		result = (* use stack algorithm to match bracket tokens *)
			Switch[First[#], 
				"{", With[{id = CreateUUID[]}, AppendTo[labels1, id]; {"{", id, "{"}], 
				"}", With[{id = Last[labels1]}, labels1 = Most[labels1]; {"}", id, "}"}], 
				_, #
			]& /@ tokens;
		
		(* get UUID of each bracket pair *)
		uuids = Select[result, StringMatchQ[#[[1]], "{"]&][[All, 2]];
		
		(* run over UUIDs and replace bracketed expressions with List to indicated scope *)	
		Do[
			{start, stop} = Position[result, {_, i, _}];
			pos = Sequence @@ Most[start]; (* brackets could be nested; all but last position indices indicate depth *)
			start = Last[start];
			stop = Last[stop];
			result = 
				ReplacePart[result, 
					Join[
						{
							{pos, start} -> {Switch[result[[pos, start, 1]], "{", "{}"], result[[pos, start + 1 ;; stop - 1]]}}, 
						Table[{pos, i} -> Nothing, {i, start + 1, stop}]]]
			,
			{i, uuids}];
			
		result
	]*)


(*createBracketBlocks[tokens_List] := 
	Module[{labels1 = {}, labels2 = {}, labels3 = {}, result, start, stop, pos, uuids},
		result = (* use stack algorithm to match bracket tokens *)
			Switch[First[#], 
				"{", With[{id = CreateUUID[]}, AppendTo[labels1, id]; {"{", id, "{"}], 
				"}", With[{id = Last[labels1]}, labels1 = Most[labels1]; {"}", id, "}"}], 
				
				(* the FUNCTION token includes an opening (, and its IDENT must be kept, too *)
				"FUNCTION", With[{id = CreateUUID[]}, AppendTo[labels2, id]; {"FUNCTION", id, Last[#]}],
				"(", With[{id = CreateUUID[]}, AppendTo[labels2, id]; {"(", id, "("}], 
				")", With[{id = Last[labels2]}, labels2 = Most[labels2]; {")", id, ")"}], 
				
				"[", With[{id = CreateUUID[]}, AppendTo[labels3, id]; {"[", id, "["}], 
				"]", With[{id = Last[labels3]}, labels3 = Most[labels3]; {"]", id, "]"}], 
			
				_, #
			]& /@ tokens;
		
		(* get UUID of each bracket pair *)
		uuids = Select[result, StringMatchQ[#[[1]], "{" | "(" | "[" | "FUNCTION"]&][[All, 2]];
		
		(* run over UUIDs and replace bracketed expressions with List to indicated scope *)	
		Do[
			{start, stop} = Position[result, {_, i, _}];
			pos = Sequence@@ Most[start]; (* brackets could be nested; all but last position indices indicate depth *)
			start = Last[start];
			stop = Last[stop];
			result = 
				ReplacePart[result, 
					Join[
						{
							{pos, start} -> {
								Switch[result[[pos, start, 1]], 
									"{", "{}", 
									"[", "[]", 
									"(", "()", 
									"FUNCTION", {"FUNCTION", StringDrop[result[[pos, start, -1]], -1]}
								], 
								result[[pos, start + 1 ;; stop - 1]]}}, 
						Table[{pos, i} -> Nothing, {i, start + 1, stop}]]]
			,
			{i, uuids}];
			
		result
	]*)


(* ::Subsection::Closed:: *)
(*Inheritance (graph)*)


(*(*
	Usage:
	{UUIDtaggedXML, $Elements, $DocumentGraph, $DocumentRoot, attributeTable} = createInheritenceGraphAndAttributeData[XML];
	inheritencePath = FindShortestPath[graph, root, "XML-9247b8a1-4848-40e2-8e4e-162fc6d473f4"] // Reverse;
	Does attributeTable[inheritencePath[[1]]] have an attribute you need?
		Yes \[Rule] stop
		No \[Rule] attributeTable[inheritencePath[[2]]]
	Repeat until the attribute is inherited or you run out of parents and the default is used.
*)
createInheritenceGraphAndAttributeData[parentXML_] := 
Module[{tagged, raw, eCount, rules, parentChild, edges, root, graph, attributeTable},
	tagged = parentXML //. XMLElement[elem_, attr_, children_] :> xmlElement[elem, attr, children, CreateUUID["XML-"]];
	raw = Cases[tagged, xmlElement[x___] :> xmlElement[x], Infinity];
	eCount = Length[raw];
	rules = ConstantArray[0, eCount];
	parentChild = ConstantArray[0, eCount];
	Do[
		With[{replaced = raw[[i]] //. rules[[;;i-1]]},
			parentChild[[i]] = Thread[Last[replaced] -> replaced[[-2]]];
			rules[[i]] = replaced -> Last[replaced];
		],
		{i,eCount}];
	edges = Flatten[rules /. {Rule[xmlElement[_, _, children:{__}, _], y_] :> Thread[y -> children], Rule[xmlElement[_, _, children:{}, _], y_] :> Nothing}];
	edges = DeleteCases[edges, Rule[_String, x_String] /; !MemberQ[rules[[All, 2]], x]];
	root = First[Last[edges]];
	graph = Graph[edges, VertexLabels -> Placed["Name", Tooltip], GraphLayout -> {"LayeredEmbedding", "RootVertex" -> root}];
	attributeTable = Association[Reverse /@ rules /. Rule[a_String, xmlElement[_, attr_, children_, _]] :> Rule[a, <|attr|>]];
	{tagged, raw, graph, root, attributeTable}
]*)


(*
	Style Inheritence:
	It is not required that the User Agent (Mathematica) support the 'style' attribute or CSS stylesheets.
	If it does, then styles are overridden by the ones that come below them in this order:
	 1. User Agent styles (Mathematica defaults) 
	 2. presentation attributes (within SVG element)
	 3. external stylesheets
	 4. document styles (<style> elements in the doc)
	 5. inline styles (style attribute)
	 6. animation
	 7. override styles (!important)
	 8. computed styles
*)
