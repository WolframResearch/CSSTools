(* ::Package:: *)

(* ::Title:: *)
(*CSS 2.1 Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20190321*)
(*Version: 1*)


(* ::Section:: *)
(*Package Header*)


BeginPackage["CSS21Parser`", {"GeneralUtilities`", "Selectors3`"}];

(* Selectors3` needed for Selector function *)

SetUsage[HeightMin, "\
HeightMin[value$] indicates value$ is to be interpreted as a minimum height taken from a CSS property."];
SetUsage[HeightMax, "\
HeightMax[value$] indicates value$ is to be interpreted as a maximum height taken from a CSS property."];
SetUsage[WidthMin, "\
WidthMin[value$] indicates value$ is to be interpreted as a minimum width taken from a CSS property."];
SetUsage[WidthMax, "\
WidthMax[value$] indicates value$ is to be interpreted as a maximum width taken from a CSS property."];

SetUsage[ResolveCSSCascade, "\
ResolveCSSCascade[type$, CSSData$, {selectors$, $$}] combines options that were interpreted from the CSS importer. \
CSS styles are merged following the CSS cascade and the resulting options are filtered by type$."];

SetUsage[ExtractCSSFromXML, "\
ExtractCSSFromXML[XMLObject$] imports the CSS declarations within XMLObject$."];

SetUsage[ApplyCSSToXML, "\
ApplyCSSToXML[XMLObject$, CSSData$] applies the CSSData$ to the symbolic XML, \
returning the CSSData$ with additional position and specificity information."];

SetUsage[ResolveCSSInterpretations, "\
ResolveCSSInterpretations[type$, CSSInterpretations$] combines options that were interpreted from the CSS importer. \
Any Left/Right/Bottom/Top and Min/Max values are merged."];

SetUsage[ResolveCSSInheritance, "\
ResolveCSSInheritance[target$, CSSData$] calculates the properties of the element at target$ including any inherited CSS properties."];

System`CellFrameStyle; (* needed in System` context *)
System`Box;

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Notes*)


(* ::Subsection::Closed:: *)
(*Outline*)


(* ::Text:: *)
(*Purpose: Import Cascading Style Sheet (.css) files, interpreting CSS styles as Wolfram Desktop options.*)
(*Approach:*)
(*	1. import CSS file as a string*)
(*	2. tokenize following the CSS grammar specification*)
(*	3. parse token sequences into available Wolfram Desktop options *)
(*Notes: *)
(*Step (1) is generally fast and assumes readable characters.*)
(*In step (2), comments, URIs, and main blocks are identified and separated using StringSplit with specific patterns. Comments are removed. Anything between declaration blocks is assumed to be a selector. Remaining strings are further split via more specific patterns.*)
(*The main bottleneck is step (3) due to the large amount of interpretation necessary of the token sequences. The basic "data types" i.e. length, color, percentage etc. are cached to improve import speed. We justify the caching because websites often stick with particular color schemes and layouts which results in a large amount of reusing colors, styles and lengths. *)


(* ::Subsection::Closed:: *)
(*Properties*)


(* ::Subsubsection::Closed:: *)
(*Strictly only CSS2.1 properties.*)


(*
(* Properties can be found from W3.org *)
pTable = Import["https://www.w3.org/TR/2011/REC-CSS2-20110607/propidx.html", "Data"];
pTable = StringReplace[pTable[[2;;,1]], "'" -> ""] // StringSplit // Join // Flatten // Union;
*)


(* ::Text:: *)
(*Aural (ignored):*)


aural = {
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
	"background", "background-attachment", "background-color", "background-image", "background-position", "background-repeat", 
	"border", 
	"border-collapse", "border-spacing", 
	"border-left", "border-right", "border-top", "border-bottom", 
	"border-color", "border-left-color", "border-right-color", "border-top-color", "border-bottom-color", 
	"border-style", "border-left-style", "border-right-style", "border-top-style", "border-bottom-style", 
	"border-width", "border-left-width", "border-right-width", "border-top-width", "border-bottom-width", 
	"bottom", "caption-side", "clear", "clip", 
	"color", 
	"direction", 
	"empty-cells", "float", 
	"font", "font-family", "font-size", "font-style", "font-variant", "font-weight", 
	"height", "left", "letter-spacing", "line-height", 
	"list-style", "list-style-image", "list-style-position", "list-style-type", 
	"margin", "margin-bottom", "margin-left", "margin-right", "margin-top", 
	"max-height", "max-width", "min-height", "min-width", 
	"overflow", 
	"padding", "padding-bottom", "padding-left", "padding-right", "padding-top", 
	"position", "quotes", "right", "table-layout", 
	"text-align", "text-decoration", "text-indent", "text-transform", 
	"top", "unicode-bidi", "vertical-align", 
	"visibility", "white-space", "width", "word-spacing", "z-index"};


(* ::Text:: *)
(*Visual + Interactive*)


interactive = {
	"cursor", 
	"outline", "outline-color", "outline-style", "outline-width"};


(* ::Text:: *)
(*Visual + Paged*)


paged = {
	"orphans", 
	"page-break-after", "page-break-before", "page-break-inside", 
	"widows"};


(* ::Text:: *)
(*All:*)


all = {"content", "counter-increment", "counter-reset", "display"};


(* ::Subsubsection::Closed:: *)
(*Some already recommended CSS3 properties*)


(*
(* If we are to extend the importer beyond CSS2.1, then start with CSS3. *)
pTableAll = Association /@ Import["https://www.w3.org/Style/CSS/all-properties.en.json", "JSON"];
allProps = Select[pTableAll, #status == "REC"&][[All, "property"]] // Union;
Complement[allProps, pTable]
*)


css3 = {
	"box-sizing", "caret-color", 
	"font-feature-settings", "font-kerning", "font-size-adjust", "font-stretch", 
	"font-synthesis", "font-variant-caps", "font-variant-east-asian", 
	"font-variant-ligatures", "font-variant-numeric", "font-variant-position", 
	"opacity", "outline-offset", "resize", "text-overflow"};


(* ::Subsection::Closed:: *)
(*Functions*)


(*
(* Complete list of CSS functions *)
table = Import["https://developer.mozilla.org/en-US/docs/Web/CSS/Reference", "Data"];
Select[Flatten[table[[5, 1]]], StringEndsQ[#, "()"]&]
*)


(* CSS 2.1 function *)
(*validFunction21 = {
	"attr()", "rgb()", "counter()", "counters()"};*)

(* more beyond CSS 2.1 *)
(*validFunctions = {
	"annotation()", "blur()", "brightness()", "calc()", 
	"character-variant()", "circle()", "contrast()", "cross-fade()", "cubic-bezier()", 
	"drop-shadow()", "element()", "ellipse()", "fit-content()", "format()", 
	"frames()", "grayscale()", "hsl()", "hsla()", "hue-rotate()", 
	"image()", "image-set()", "inset()", "invert()", "leader()", 
	"linear-gradient()", "local()", "matrix()", "matrix3d()", "minmax()", 
	"opacity()", "ornaments()", "perspective()", "polygon()", "radial-gradient()", 
	"rect()", "repeat()", "repeating-linear-gradient()", "repeating-radial-gradient()",  
	"rgba()", "rotate()", "rotate3d()", "rotateX()", "rotateY()", 
	"rotateZ()", "saturate()", "scale()", "scale3d()", "scaleX()", 
	"scaleY()", "scaleZ()", "sepia()", "skew()", "skewX()", 
	"skewY()", "steps()", "styleset()", "stylistic()", "swash()", 
	"symbols()", "target-counter()", "target-counters()", "target-text()", "translate()", 
	"translate3d()", "translateX()", "translateY()", "translateZ()", "url()", 
	"var()"};*)


(* ::Section::Closed:: *)
(*CSS 2.1 Grammar*)


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
	Tabs and newlines are escaped in both WL and RE so do not need additional escaping, e.g. \t\r\n\f.
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
RE["baduri1"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ "([!#$%&*-\\[\\]-~]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*" ~~ RE["w"] ~~ ")"; (* literal ( after url; CharacterRange["*", "~"]*)
RE["baduri2"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["string"] ~~ RE["w"] ~~ ")";
RE["baduri3"]     = "(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["badstring"] ~~ ")";
RE["comment"]     = "(\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/)";
RE["h"]           = "([0-9a-fA-F])";
RE["ident"]       = "(((--)|(-?" ~~ RE["nmstart"] ~~ "))" ~~ RE["nmchar"] ~~ "*)";
RE["integer"]     = "([\\-+]?[0-9]+)"; (* not part of the CSS 2.1 spec, but useful *)
RE["integerSCI"]  = "([\\-+]?[0-9]+[Ee][\\-+]?[0-9]+)"; (* not part of the CSS 2.1 spec, but useful *)
RE["escape"]      = "(" ~~ RE["unicode"] ~~ "|\\\\[^\n\r\f0-9a-fA-F])"; 
RE["name"]        = "(" ~~ RE["nmchar"] ~~ "+)";
RE["nl"]          = "(\n|\r\n|\r|\f)";
RE["nmchar"]      = "([_a-zA-Z0-9\\-]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["nmstart"]     = "([_a-zA-Z]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["nonascii"]    = (*FIXME?*) "([^[:ascii:]])";
RE["num"]         = "(" ~~ RE["number"] ~~ "|" ~~ RE["integer"] ~~ ")";       (* pattern match against reals before integers *)
RE["numSCI"]      = "(" ~~ RE["numberSCI"] ~~ "|" ~~ RE["integerSCI"] ~~ ")"; (* pattern match against reals before integers *)
RE["number"]      = "([\\-+]?[0-9]*\\.[0-9]+)"; (* literal dot *) (* not part of the CSS 2.1 spec, but useful *)
RE["numberSCI"]   = "([\\-+]?[0-9]*\\.[0-9]+[Ee][\\-+]?[0-9]+)"; (* not part of the CSS 2.1 spec, but useful *)
RE["s"]           = "([ \t\r\n\f]+)";
RE["string"]      = "(" ~~ RE["string1"] ~~ "|" ~~ RE["string2"] ~~ ")";
RE["string1"]     = "(\\\"([^\n\r\f\\\"]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\")"; (* [] contains escaped double quote *)
RE["string2"]     = "(\\'([^\n\r\f\\']|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\\')"; (* [] contains escaped single quote *)
RE["unicode"]     = "(\\\\" ~~ RE["h"] ~~ "{1,6}(\r\n|[ \n\r\t\f])?)";
RE["url"]         = "(([!#$%&*-~]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*)";
RE["w"]           = "(" ~~ RE["s"] ~~ "?)";

(* 
	Strictly following the specification leaves out some of the following letter patterns. 
	We include them all here for easier pattern matching of corner cases. *)

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

characterNormalizationRules = MapThread[RegularExpression[RE[#1]] :> #2 &, {CharacterRange["A", "Z"], CharacterRange["a", "z"]}];
normalizeKeyWord[s_String] := StringReplace[s, characterNormalizationRules]

(* ::Subsection::Closed:: *)
(*Tokens (alphabetical)*)


T["ANGLE"] = 
	StringExpression[
		"(" ~~ RE["num"] ~~ RE["D"] ~~ RE["E"] ~~ RE["G"] ~~ ")" ~~ "|", 
		"(" ~~ RE["num"] ~~ RE["R"] ~~ RE["A"] ~~ RE["D"] ~~ ")" ~~ "|",
		"(" ~~ RE["num"] ~~ RE["G"] ~~ RE["R"] ~~ RE["A"] ~~ RE["D"] ~~ ")"];

T["BAD_COMMENT"] = RE["badcomment"]; 
T["BAD_STRING"]  = RE["badstring"];
T["BAD_URI"]     = RE["baduri"];

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

T["UNICODE-RANGE"] = "(([uU]\\+[0-9A-Fa-f]{1,6}[?]*)|([uU]\\+[0-9A-Fa-f]{1,6}-[0-9A-Fa-f]{1,6}))"; (* FIXME: not sure if correct *)
T["URI"] =
	StringExpression[
		"(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["string"] ~~ RE["w"] ~~ "\\)" ~~ ")" ~~ "|",
		"(" ~~ RE["U"] ~~ RE["R"] ~~ RE["L"] ~~ "\\(" ~~ RE["w"] ~~ RE["url"] ~~ RE["w"] ~~ "\\)" ~~ ")"];


(* ::Subsection::Closed:: *)
(*Productions (not all used)*)


(* charset isn't a real production, but it's convenient to define here *)
P["charset"]     := "(" ~~ T["CHARSET_SYM"] ~~ T["STRING"] ~~ ";" ~~ ")";
P["declaration"] := "(" ~~ P["property"] ~~ ":" ~~ T["S*"] ~~ P["expr"] ~~ P["prio"] ~~ "?)";
P["prio"]        := "(" ~~ T["IMPORTANT_SYM"] ~~ T["S*"] ~~ ")";
P["expr"]        := "(" ~~ P["term"] ~~ "(" ~~ P["operator"] ~~ "?" ~~ P["term"] ~~ ")*)";
P["property"]    := "(" ~~ T["IDENT"] ~~ T["S*"] ~~ ")";
P["operator"]    := "((/" ~~ T["S*"] ~~ ")|(," ~~ T["S*"] ~~ "))";

P["media"]      := "(" ~~ T["MEDIA_SYM"] ~~ T["S*"] ~~ P["media_list"] ~~ "{" ~~ T["S*"] ~~ __ ~~ "}" ~~ T["S*"] ~~ ")";
P["media_list"] := "(" ~~ P["medium"] ~~ "(," ~~ T["S*"] ~~ P["medium"] ~~ ")*" ~~ ")";
P["medium"]     := "(" ~~ T["IDENT"] ~~ T["S*"] ~~ ")";

P["import"] := "(" ~~ T["IMPORT_SYM"] ~~ T["S*"] ~~ "(" ~~ T["STRING"] ~~ "|" ~~ T["URI"] ~~ ")" ~~ T["S*"] ~~ P["media_list"] ~~ "?" ~~ ";" ~~ T["S*"] ~~ ")";

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
			"(" ~~ T["DIMENSION"]   ~~ T["S*"] ~~ ")" ~~ "|" ~~ (* dimension is a catch-all *)
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
(*Tokenize File*)


validTokenQ[x_] := MatchQ[x, _?StringQ | {_?StringQ, Repeated[_?NumberQ | _?StringQ]}]
 
tokenType[x_String]    := x
tokenType[x_List]      := x[[1]] (* assumes already valid token as input *)
tokenString[x_List]    := x[[2]] (* assumes already valid token as input *)
tokenValue[x_List]     := x[[3]] (* assumes already valid number or dimension token as input *)
tokenValueType[x_List] := x[[4]] (* assumes already valid number or dimension token as input *)

dimensionUnit[x_List]  := x[[5]] (* assumes already valid dimension token as input *)
  

(*Identify @charset, @import, and rulesets (selector + block declarations)*)
tokenizeFirstPass[x_String] := 
	Replace[
		StringSplit[x, 
			{
				s:RegularExpression @ RE["comment"]    :> Nothing,
				s:RegularExpression @ RE["badcomment"] :> Nothing,
				s:RegularExpression @ P["charset"]     :> {"charset",   s},
				s:RegularExpression @ P["import"]      :> {"import",    s},
				(*s:RegularExpression @ T["MEDIA_SYM"] :> {"@media", s}, (* we don't parse media queries, but we recognize the symbol *)*)
				s:RegularExpression @ T["URI"]         :> {"uri",       s},
				s:RegularExpression @ T["BAD_URI"]     :> {"baduri",    s},
				s:RegularExpression @ T["STRING"]      :> {"string",    s},
				s:RegularExpression @ T["BAD_STRING"]  :> {"badstring", s},
				"{" -> "{", 
				"}" -> "}"}],
		s_String /; StringLength[s] > 1 :> {"other", s},
		{1}]


(* ::Subsection::Closed:: *)
(*Identify @import components*)


tokenizeAtImportKeyword[x_String] := 
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
		{1}]


parse["atImportKeyword", tokens:{__?validTokenQ}] :=
	Module[{pos = 2 (* first token must be @import *), l = Length[tokens], path, mediums = {}, data},
		(* second token must be URL or string path to file *)
		path = 
			Switch[tokenType @ tokens[[pos]],
				"uri", 
					path = StringTake[tokenString @ tokens[[pos]], {5, -2}]; (* strip off url() *)
					If[StringMatchQ[path, RegularExpression @ T["STRING"]], StringTake[path, {2, -2}], path], (* strip off string quotes *)
				"string", StringTake[tokenString @ tokens[[pos]], {2, -2}],
			_, (* shouldn't be able to reach this *) ""];
		skipWhitespace[pos, l, tokens]; 	
		
		(* anything else is a media condition *)
		While[tokenType @ tokens[[pos]] != ";",
			If[tokenType @ tokens[[pos]] == "medium", 
				AppendTo[mediums, tokenString @ tokens[[pos]]]; skipWhitespace[pos, l, tokens]
				,
				Return @ Failure["UnexpectedParse", <|"Message" -> "Expected @import media type."|>]];
			If[tokenType @ tokens[[pos]] == ";", Break[]];
			If[tokenType @ tokens[[pos]] == ",", 
				skipWhitespace[pos, l, tokens]
				,
				Return @ Failure["UnexpectedParse", <|"Message" -> "Expected @import media delimiter."|>]]];
		mediums = ToLowerCase @ mediums;
		
		(* import without interpretation *)
		data = Import[Echo[path, "@import"], "Text"];
		If[FailureQ[data], Return @ {}, data = processRulesets[data]];
		If[mediums =!= {}, data[[All, "Condition"]] = ConstantArray[mediums, Length[data]]];
		data	
	]


(* ::Subsection::Closed:: *)
(*Identify declarations*)


tokenizeDeclaration[x_String] := 
	Replace[
		StringSplit[x, 
			{
				s:RegularExpression @ T["STRING"]        :> {"string", s},
				s:RegularExpression @ T["URI"]           :> {"uri", s},
				s:RegularExpression @ T["FUNCTION"]      :> {"function", s},
				s:RegularExpression @ T["UNICODE-RANGE"] :> {"unicode-range", s},
				s:RegularExpression @ T["IDENT"]         :> {"ident", s},
				s:RegularExpression @ P["hexcolor"]      :> {"hexcolor", StringTrim @ s},
				
				(* scientific notation is tricky; it can look like a dimension but can also be part of a dimension *)
				num:RegularExpression[RE["numSCI"]] ~~ "%" :> Flatten @ {"percentage", tokenizeNumber[num]},
				num:RegularExpression[RE["numSCI"]] ~~ unit:RegularExpression[RE["ident"]] :> Flatten @ {"dimension", tokenizeNumber[num], normalizeKeyWord @ unit},
				num:RegularExpression[RE["numSCI"]] :> Flatten @ {"number", tokenizeNumber[num]},
				
				s:RegularExpression @ T["PERCENTAGE"]    :> Flatten @ {"percentage", tokenizePercentage[s]},
				s:RegularExpression @ T["DIMENSION"]     :> Flatten @ {"dimension",  tokenizeDimension[s]},
				s:RegularExpression @ T["NUMBER"]        :> Flatten @ {"number",     tokenizeNumber[s]},
				s:RegularExpression @ T["IMPORTANT_SYM"] :> {"important", s},
				":" -> ":", 
				";" -> ";",
				"," -> ",", 
				"/" -> "/", 
				")" -> ")",
				s:Whitespace :> " "}],
		{	
			s_String /; StringMatchQ[s, ""] -> Nothing,
			s_String /; StringLength[s] > 1 :> {"other", s}},
		{1}]


tokenizePercentage[x_String] := StringSplit[x, num:RegularExpression[RE["num"]] ~~ unit:"%" :> tokenizeNumber @ num]
tokenizeDimension[x_String] :=  StringSplit[x, num:RegularExpression[RE["num"]] ~~ unit___ :> {tokenizeNumber @ num, normalizeKeyWord @ unit}]

tokenizeNumber[x_String] := (* cache the Interpreter calls *)
	tokenizeNumber[x] = If[StringMatchQ[x, RegularExpression[RE["integer"]]], {x, Interpreter["Integer"][x], "integer"}, {x, Interpreter["Number"][x], "number"}]
			


(* ::Section::Closed:: *)
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


couldNotImportFailure[uri_String] :=       Failure["ImportFailure",   <|"URI" -> uri|>];
illegalIdentifierFailure[ident_String] :=  Failure["UnexpectedParse", <|"Message" -> "Illegal identifier.", "Ident" -> ident|>];
invalidFunctionFailure[function_String] := Failure["InvalidFunction", <|"Function" -> function|>];
negativeIntegerFailure[prop_String] :=     Failure["BadLength",       <|"Message" -> prop <> "integer must be non-negative."|>];
negativeLengthFailure[prop_String] :=      Failure["BadLength",       <|"Message" -> prop <> "length must be non-negative."|>];
notAnImageFailure[uri_String] :=           Failure["NoImageFailure",  <|"URI" -> uri|>];
noValueFailure[prop_String] :=             Failure["UnexpectedParse", <|"Message" -> "No " <> prop <> " property value."|>];
positiveLengthFailure[prop_String] :=      Failure["BadLength",       <|"Message" -> prop <> "length must be positive."|>];
repeatedPropValueFailure[prop_] :=         Failure["UnexpectedParse", <|"Message" -> "Repeated property value type.", "Prop" -> prop|>];
tooManyPropValuesFailure[props_List] :=    Failure["UnexpectedParse", <|"Message" -> "Too many property values provided.", "Props" -> props|>];
tooManyTokensFailure[tokens_List] :=       Failure["UnexpectedParse", <|"Message" -> "Too many tokens.", "Tokens" -> tokenType /@ tokens|>];
unrecognizedKeyWordFailure[prop_String] := Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " keyword."|>];
unrecognizedValueFailure[prop_String] :=   Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " value."|>];
unsupportedValueFailure[prop_String] :=    Failure["UnsupportedProp", <|"Property" -> prop|>]


(* ::Subsection::Closed:: *)
(*Property Data Table*)


(* 
	Some of these are shorthand properties that set one or more other properties. 
	As such, the shorthand initial values would never be directly required.
*)
CSSPropertyData = <|
	"background" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A",  (* shorthand property *)
		"WDInitialValue" -> Automatic|>,
	"background-attachment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "scroll",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"background-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "transparent",
		"WDInitialValue" -> None|>, 
	"background-image" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>, 
	"background-position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0% 0%",
		"WDInitialValue" -> {0, 0}|>, 
	"background-repeat" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "repeat",
		"WDInitialValue" -> "Repeat"|>, 
	"border-collapse" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "separate",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"border-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* shorthand property, sets all 4 border sides *)
		"WDInitialValue" -> Dynamic @ CurrentValue[FontColor]|>, 
	"border-top-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"WDInitialValue" -> Dynamic @ CurrentValue[FontColor]|>, 
	"border-right-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"WDInitialValue" -> Dynamic @ CurrentValue[FontColor]|>,
	"border-bottom-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"WDInitialValue" -> Dynamic @ CurrentValue[FontColor]|>,
	"border-left-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"WDInitialValue" -> Dynamic @ CurrentValue[FontColor]|>,
	"border-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none", (* shorthand property, sets all 4 sides *)
		"WDInitialValue" -> None|>, 
	"border-top-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>, 
	"border-right-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>,
	"border-bottom-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>,
	"border-left-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>,
	"border-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium", (* shorthand property, sets all 4 border sides *)
		"WDInitialValue" -> Thickness[Medium]|>, 
	"border-top-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Thickness[Medium]|>, 
	"border-right-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Thickness[Medium]|>,
	"border-bottom-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Thickness[Medium]|>,
	"border-left-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Thickness[Medium]|>,
	"border" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand property, sets all 4 border sides color/style/width*)
		"WDInitialValue" -> Automatic|>, (* not actually used; parsing uses individual border-color/style/width property values *)
	"border-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>,
	"border-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"caption-side" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "top",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"clear" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> Missing["Not supported."]|>,		
	"clip" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Missing["Not supported."]|>,
	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"WDInitialValue" -> Black|>,(* no set CSS specification, so use reasonable setting *)
	"content" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Normal|>,    
	"counter-increment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> {}|>,
	"counter-reset" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> {}|>,   
	"cursor" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"direction" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "ltr",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"display" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "inline",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"empty-cells" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "show",
		"WDInitialValue" -> Missing["Not supported."]|>, 	
	"float" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"font" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"WDInitialValue" -> Automatic|>, 
	"font-family" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"WDInitialValue" :> CurrentValue[{StyleDefinitions, "Text", FontFamily}]|>, 
	"font-size" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Medium|>,   
	"font-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Plain|>,    
	"font-variant" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> "Normal"|>, 
	"font-weight" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Plain|>, 
	"height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"letter-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> "Plain"|>, 
	"line-height" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> {1.2, 0}|>, 
	"list-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"WDInitialValue" -> None|>, 
	"list-style-image" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "",
		"WDInitialValue" -> None|>,
	"list-style-position" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "outside",
		"WDInitialValue" -> Automatic|>,
	"list-style-type" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "disc",
		"WDInitialValue" -> "\[FilledCircle]"|>,
	"margin" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 margins *)
		"WDInitialValue" -> Automatic|>, 
	"margin-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"margin-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"margin-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"margin-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"max-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> Infinity|>,
	"max-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> Infinity|>,
	"min-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"min-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"orphans" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"outline" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand property, sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"outline-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "invert",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"outline-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"outline-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"overflow" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "visible",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"padding" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 sides *)
		"WDInitialValue" -> 0|>, 
	"padding-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"padding-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"padding-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"padding-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"page-break-after" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"page-break-before" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"page-break-inside" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "static",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"quotes" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"WDInitialValue" -> Missing["Not supported."]|>,  
	"right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"table-layout" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Missing["Not supported."]|>, 		
	"text-align" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* a nameless value that acts as 'left' if 'direction' is 'ltr', 'right' if 'direction' is 'rtl' *)
		"WDInitialValue" -> Automatic|>, 
	"text-decoration" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> {}|>,       
	"text-indent" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"WDInitialValue" -> 0|>,
	"text-transform" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"WDInitialValue" -> None|>,  
	"top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"unicode-bidi" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"vertical-align" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "baseline",
		"WDInitialValue" -> Baseline|>,  
	"visibility" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visible",
		"WDInitialValue" -> True|>, 
	"white-space" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"widows" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Automatic|>, 
	"word-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"WDInitialValue" -> Missing["Not supported."]|>, 
	"z-index" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"WDInitialValue" -> Missing["Not supported."]|>
	|>;
	
initialValues[prop_String] := CSSPropertyData[prop, "WDInitialValue"]


(* ::Subsection::Closed:: *)
(*<angle>*)


(* mostly used for HSLA color function; non-degree units are converted to degrees *)
parseAngle[token:{"dimension", n_String, val_, type:"integer"|"number", "deg"}] := val 
parseAngle[token:{"dimension", n_String, val_, type:"integer"|"number", "grad"}] := val*360/400
parseAngle[token:{"dimension", n_String, val_, type:"integer"|"number", "rad"}] := val*360/2/Pi
parseAngle[token:{"dimension", n_String, val_, type:"integer"|"number", "turn"}] := val*360


(* ::Subsection::Closed:: *)
(*<color>*)


(* parseSingleColor is defined in CSSColors4.wl *)


(* ::Subsection::Closed:: *)
(*<counter>*)


parseCounter[prop_String, tokens:{___?validTokenQ}] := parseCounter[prop, tokens] =
	Module[{pos = 1, l = Length[tokens], style, listtype = "decimal"},
		Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
			"counter(",
				skipWhitespace[pos, l, tokens];
				If[pos <= l && tokenType @ tokens[[pos]] == "ident", 
					style = tokenString @ tokens[[pos]]
					, 
					Return @ invalidFunctionFailure @ StringJoin[tokenString /@ tokens]
				];
				skipWhitespace[pos, l, tokens];
				If[pos > l, Return @ invalidFunctionFailure @ StringJoin[tokenString /@ tokens]];
				Switch[tokenType @ tokens[[pos]],
					"ident", listtype = tokenString @ tokens[[pos]],
					")",     Null,
					_,       tooManyTokensFailure @ tokens
				];
				parseSingleListStyleType["list-style-type", {"ident", listtype}, style],
			"counters(", Return @ Missing["Not supported."],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*<integer> and <number>*)


parseNumber[token:{"number", n_String, val_, type:"integer"|"number"}] := val
parseZero[token:{"number", n_String, val_, type:"integer"|"number"}] := 
	If[TrueQ[val == 0], 0, Failure["UnexpectedParse", <|"Message" -> "Non-zero length has missing units."|>]]


(* ::Subsection::Closed:: *)
(*<length>*)

	
parseLength[token:{"dimension", n_String, val_, type:"integer"|"number", unit_String}, inFontSize_:False] := 
	Module[{dpi = "Resolution" /. First[SystemInformation["Devices", "ScreenInformation"], "Resolution" -> 72]},
		If[TrueQ[val == 0], Return @ 0];
		(* parse units 
			The following conversions to pixels are based on SVG length specs and DPI.
			'em' and 'ex' are relative values. If within the 'font-size' property, then first inherit from the parent.
			If an 'em' or 'ex' length is given outside the 'font-size' property, then it's a function of the current FontSize.
		*)
		Switch[unit, 
			"em", If[inFontSize, val*Inherited,     With[{v = val}, Dynamic[v*CurrentValue[FontSize]]]],
			"ex", If[inFontSize, val*0.5*Inherited, With[{v = val}, Dynamic[v*CurrentValue["FontXHeight"]]]],
			"in", val*dpi,
			"cm", val/2.54*dpi,
			"mm", val/10/2.54*dpi,
			"pt", val,
			"pc", 12*val,
			"px", 0.75*val,
			_,    Failure["UnexpectedParse", <|"Message" -> "Unrecognized length unit."|>]
		]
	]

parseLengthNonRelative[token:{"dimension", n_String, val_, type:"integer"|"number", "em"}] := val 
parseLengthNonRelative[token:{"dimension", n_String, val_, type:"integer"|"number", "ex"}] := val/2
(*parseLengthNonRelative[token:{"dimension", n_String, val_, type:"integer"|"number", _}] := parseLength[token]*)


negativeQ[n_, prop_String, default_] :=
	Which[
		FailureQ[n],         n, 
		TrueQ @ Negative[n], negativeLengthFailure @ prop, 
		True,                default
	]


(* ::Subsection::Closed:: *)
(*<percentage>*)


parsePercentage[token:{"percentage", n_String, val_, type:"integer"|"number"}] := Scaled[val/100]


(* ::Subsection::Closed:: *)
(*<uri>*)


parseURI[uri_String] := 
	Module[{p, s = StringTake[uri, {5, -2}], start, rest},
		If[StringStartsQ[s, "data:", IgnoreCase -> True],
			(* string split on the first comma *)
			p = StringPosition[s, ",", 1][[1, 1]];
			start = StringTake[s, {1, p-1}];
			rest = StringTake[s, {p+1, -1}];
			
			(* interpret data type and import *)
			Which[
				StringMatchQ[start, StartOfString ~~ "data:" ~~ EndOfString, IgnoreCase -> True], 
					ImportString[URLDecode @ rest, "String"],
				StringMatchQ[start, StartOfString ~~ "data:" ~~ ___ ~~ ";base64" ~~ EndOfString, IgnoreCase -> True],
					ImportString[rest, "Base64"],
				StringMatchQ[start, StartOfString ~~ "data:" ~~ ___ ~~ EndOfString, IgnoreCase -> True],
					start = StringReplace[start, StartOfString ~~ "data:" ~~ x___ ~~ EndOfString :> x];
					rest = URLDecode @ rest;
					Switch[ToLowerCase @ start,
						"" | "text/plain", ImportString[rest, "String"],
						"text/css",        ImportString[rest, "String"],
						"text/html",       ImportString[rest, "HTML"],
						"text/javascript", ImportString[rest, "String"],
						"image/gif",       ImportString[rest, "GIF"],
						"image/jpeg",      ImportString[rest, "JPEG"],
						"image/png",       ImportString[rest, "PNG"],
						"image/svg+xml",   ImportString[rest, "XML"],
						"image/x-icon"|"image/vnd.microsoft.icon", ImportString[rest, "ICO"],
						"audio/wave"|"audio/wav"|"audio/x-wav"|"audio/x-pn-wav", Audio[rest],
						_,                 Missing["Not supported."]
					]
			]
			,
			(* else attempt a generic import *)
			If[StringStartsQ[s, "\""|"\'"], s = StringTake[s, {2, -1}]];
			If[StringEndsQ[s, "\""|"\'"], s = StringTake[s, {1, -2}]];
			With[{i = Quiet @ Import[s]}, If[i === $Failed, couldNotImportFailure[s], i]]
		]
	]


(* ::Subsection::Closed:: *)
(*background*)


(* ::Subsubsection::Closed:: *)
(*background*)


(*TODO: parse multiple background specs but only take the first valid one*)
parse[prop:"background", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)parseSingleBG[prop, tokens]

(* 
	Shorthand for all background properties.
	Any properties not set by this are reset to their initial values.
	Commas separate background layers, so split on comma, but FE only supports one image so take last...?
*)
parseSingleBG[prop_String, tokens:{__?validTokenQ}] := 
	Module[
		{
			pos = 1, l = Length[tokens], value, start, 
			values = <|
				"a" -> initialValues @ "background-attachment", 
				"c" -> initialValues @ "background-color",
				"i" -> initialValues @ "background-image",
				"p" -> initialValues @ "background-position",
				"r" -> initialValues @ "background-repeat"|>,
			hasAttachment = False, hasColor = False, hasImage = False, hasPosition = False, hasRepeat = False,
			i1 = 0, i2 = 0
		},
		While[pos <= l, 
			Which[
				tokenType @ tokens[[pos]] == "function", (* only color can be a function; eventually should support gradients *)
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"rgb(" | "rgba(" | "hsl(" | "hsla(", 
							If[hasColor, Return @ repeatedPropValueFailure @ "background-color"]; 
							hasColor = True; values["c"] = parseSingleColor[prop, consumeFunction[pos, l, tokens]];,
						_, (* linear- or radial-gradient *)
							If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
							hasImage = True; values["i"] = Missing["Not supported."]; consumeFunction[pos, l, tokens] (* advances pos but return value is not used *)
					],
					
				StringMatchQ[tokenString @ tokens[[pos]], "inherit", IgnoreCase -> True], i1++,
				StringMatchQ[tokenString @ tokens[[pos]], "initial", IgnoreCase -> True], i2++,
								
				!FailureQ[value = parseSingleBGAttachment[prop, tokens[[pos]]]],
					If[hasAttachment, Return @ repeatedPropValueFailure @ "background-attachment"];
					hasAttachment = True; values["a"] = value,
					
				!FailureQ[value = parseSingleColor[prop, {tokens[[pos]]}]], (* color can also be hex or keyword *)
					If[hasColor, Return @ repeatedPropValueFailure @ "background-color"];
					hasColor = True; values["c"] = value,
					
				!FailureQ[value = parseSingleBGImage[prop, tokens[[pos]]]], 
					If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
					hasImage = True; values["i"] = value,
					
				!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
					If[hasPosition, Return @ repeatedPropValueFailure @ "background-position"];
					hasPosition = True; values["p"] = {value, Center};
					(* check for a pair of position values; they must be sequential *)
					start = pos; skipWhitespace[start, l, tokens];
					If[!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
						values["p"] = {values["p"][[1]], value};
						pos = start;
					];
					values["p"] = parseSingleBGPositionPair[values["p"], {"", #}& /@ values["p"]],
					
				!FailureQ[value = parseSingleBGRepeat[prop, tokens[[pos]]]], 
					If[hasRepeat, Return @ repeatedPropValueFailure @ "background-repeat"];
					hasRepeat = True; values["r"] = value,
				
				True, unrecognizedValueFailure @ prop						
			];
			skipWhitespace[pos, l, tokens]
		];
		(* 
			attachment: not supported, 
			color: Background, 
			image: System`BackgroundAppearance, 
			position: System`BackgroundAppearanceOptions, 
			repeat: System`BackgroundAppearanceOptions *)
		Which[
			l==1 && i1==1, Background -> Inherited,
			l==1 && i2==1, Background -> initialValues @ "background",
			i1>1, tooManyPropValuesFailure @ "inherit",
			i2>1, tooManyPropValuesFailure @ "initial",
			
			hasColor && Not[hasAttachment || hasImage || hasPosition || hasRepeat], 
				Background -> values["c"],
			
			True,
				{
					System`BackgroundAppearanceOptions ->
						Which[
							values["p"] === {0,0}    && values["r"] === "NoRepeat", "NoRepeat",
							values["p"] === "Center" && values["r"] === "NoRepeat", "Center",
							values["p"] === {0,0},                                  values["r"],
							True,                                                     Missing["Not supported."]
						],
					System`BackgroundAppearance -> values["i"],
					Background -> values["c"]}
		]
]


(* ::Subsubsection::Closed:: *)
(*background-attachment*)


parseSingleBGAttachment[prop_String, token_?validTokenQ] := parseSingleBGAttachment[prop, token] =
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"scroll",  Missing["Not supported."],
				"fixed",   Automatic, (* FE's only allowed value *)
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]

parse[prop:"background-attachment", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGAttachment[prop, tokens[[1]]];
		If[FailureQ[value] || MissingQ[value], value, Missing["Only fixed supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*background-color*)


(* 
	Effectively the same as color, except a successful parse returns as a rule Background -> value.
	Also 'currentColor' value needs to know the current value of 'color', instead of inherited from the parent.	
*) 
parse[prop:"background-color", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value},
		value = parseSingleColor[prop, tokens];
		If[FailureQ[value], value, Background -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-image*)


parseSingleBGImage[prop_String, token_?validTokenQ] := 
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"none",    None,
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"uri", parseURI @ tokenString @ token,
		_,     unrecognizedValueFailure @ prop
	]

parse[prop:"background-image", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		value = 
			Switch[tokenType @ tokens[[pos]], 
				"function", (* possible linear- or radial-gradient *)
					consumeFunction[pos, l, tokens]; Missing["Not supported."], (* advances pos but return value is not used *)
				_, parseSingleBGImage[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, System`BackgroundAppearance -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-position*)


parseSingleBGPosition[prop_String, token_?validTokenQ] :=
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"left",    Left,
				"center",  Center,
				"right",   Right,
				"top",     Top,
				"bottom",  Bottom,
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"percentage", parsePercentage @ token,
		"dimension",  parseLength @ token,
		"number",     parseZero @ token,
		_,            unrecognizedValueFailure @ prop
	]

parseSingleBGPositionPair[values:{__}, tokens:{__?validTokenQ}] :=
	Switch[Length[values],
		1, 
			Switch[values[[1]],
				Center,    "Center",
				Inherited, Inherited,
				_,         Missing["Not supported."]
			],
		2, 
			Switch[values,
				{Center, Center},                   "Center", 
				{Top, Left} | {Left, Top} | {0, 0}, "NoRepeat",
				{Inherited, _} | {_, Inherited},    illegalIdentifierFailure @ "inherit",
				_,                                  Missing["Not supported."]
			],
		_, tooManyTokensFailure @ tokens
	]

(* only "Center" is supported by the FE *)
parse[prop:"background-position", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value = parseSingleBGPosition[prop, tokens[[1]]];
			If[FailureQ[value], Return @ value, AppendTo[values, value]];
			skipWhitespace[pos, l, tokens]
		];
		value = parseSingleBGPositionPair[values, tokens];		
		If[FailureQ[value], value, System`BackgroundAppearanceOptions -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-repeat*)


parseSingleBGRepeat[prop_String, token_?validTokenQ] :=
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"no-repeat", "NoRepeat",
				"repeat-x",  "RepeatX",
				"repeat-y",  "RepeatY",
				"repeat",    "Repeat",
				"inherit",   Inherited,
				"initial",   initialValues @ prop,
				_,           unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]

parse[prop:"background-repeat", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGRepeat[prop, tokens[[1]]];
		If[FailureQ[value], value, System`BackgroundAppearanceOptions -> value]
	]


(* ::Subsection::Closed:: *)
(*border*)


(*
	WL specifies border style (dashing), width, and color all at once via Directive[].
	Post-processing is required to combine individual properties correctly.
	WL Grid only allows collapsed borders. However, each item can cloud wrapped in Frame.
		Grid[
			Map[
				Framed[#,ImageSize->Full,Alignment->Center]&, 
				{{"Client Name","Age"},{"",25},{"Louise Q.",""},{"Owen",""},{"Stan",71}}, 
				{2}],
			Frame->None,
			ItemSize->{{7,3}}]
	but this is beyond the scope of CSS importing.
*)


(* ::Subsubsection::Closed:: *)
(*border-collapse*)


parse[prop:"border-collapse", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		Switch[tokenType @ tokens[[pos]],
			"ident", 
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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
parse[prop:"border-top-color"|"border-right-color"|"border-bottom-color"|"border-left-color", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, wrapper},
		value = parseSingleColor[prop, tokens];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-color", Top, "border-right-color", Right, "border-bottom-color", Bottom, "border-left-color", Left];
			# -> wrapper[value]& /@ {FrameStyle, CellFrameStyle}
		]
	]	

(* sets all 4 border/frame edges at once *)
parse[prop:"border-color", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleColor[prop, If[tokenType @ tokens[[pos]] == "function", consumeFunction[pos, l, tokens], {tokens[[pos]]}]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			skipWhitespace[pos, l, tokens];
		];
		results =
			Switch[Length[results],
				1, {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]},
				2, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]},
				3, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
				4, {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
				_, tooManyTokensFailure @ tokens
			];
		{FrameStyle -> results, CellFrameStyle -> results}
	]


(* ::Subsubsection::Closed:: *)
(*border-spacing*)


(* 
	'border-spacing' isn't a 1-to-1 match with WL Grid's Spacings option, but it's close.
	WL Grid does not allow gaps between items. 
	WL Spacings is already in units of the current FontSize so the parsing of lengths is backwards
	in that relative lengths become non-relative, and non-relative become relative to FontSize. 
	Also WL Spacings applies to the outer margins as well, but 'border-spacing' is internal only.
	Because each item is padded on either side, divide the result in half.
*)
parse[prop:"border-spacing", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l == 1 && tokenType @ tokens[[1]] == "ident", 
			Return @ 
				Switch[normalizeKeyWord @ tokenString @ tokens[[1]],
					"inherit", Spacings -> Inherited,
					"initial", Spacings -> initialValues @ prop,
					_,         unrecognizedKeyWordFailure @ prop
				]
		];
		While[pos <= l,
			value = 
				Switch[tokenType @ tokens[[pos]],
					"dimension", 
						If[tokenValue @ tokens[[pos]] < 0,
							negativeLengthFailure @ prop
							,
							Switch[dimensionUnit @ tokens[[pos]],
								"em"|"ex", (parseLengthNonRelative @ tokens[[pos]])/2,
								_,         With[{n = parseLength @ tokens[[pos]]}, Dynamic[n/CurrentValue[FontSize]]]
							]
						],
					"number", parseZero @ tokens[[pos]],
					_,        unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			skipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, Spacings -> {First @ results, First @ results}, (* if only one length, then it specifies both hor and ver *)
			2, Spacings -> results,
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-style*)


parseSingleBorderStyle[prop_String, token_?validTokenQ] :=
	Switch[tokenType @ token,
		"ident",
			Switch[normalizeKeyWord @ tokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"none",    None, 
				"hidden",  None, (* 'hidden' is technically different from 'none', but I don't think the difference matters in WL *)
				"dotted",  Dotted,
				"dashed",  Dashed,
				"solid",   Dashing[{}],
				"double" | "groove" | "ridge" | "inset" | "outset",  Missing["Not supported."],
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-style"|"border-right-style"|"border-bottom-style"|"border-left-style", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderStyle[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-style", Top, "border-right-style", Right, "border-bottom-style", Bottom, "border-left-style", Left];
			# -> wrapper[value]& /@ {FrameStyle, CellFrameStyle}
		]
	]	
	
parse[prop:"border-style", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleBorderStyle[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			skipWhitespace[pos, l, tokens]
		];
		Switch[Length[results],
			1, # -> {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			2, # -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			3, # -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			4, # -> {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-width*)


(* WL FrameStyle thickness is best given as a calculated AbsoluteThickness for numerical values. *)
parseSingleBorderWidth[prop_String, token_?validTokenQ] :=
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"thin",    Thickness[Small],
				"medium",  Thickness[Medium],
				"thick",   Thickness[Large],
				_,         unrecognizedKeyWordFailure @ prop
			],
		"dimension", If[tokenValue @ token < 0, negativeLengthFailure @ prop, AbsoluteThickness[parseLength @ token]],
		"number",    parseZero @ token,
		_,           unrecognizedValueFailure @ prop
	]


convertToCellThickness[x_] := Switch[x, AbsoluteThickness[_], First[x], Thickness[_], First[x] /. {Small -> 1, Medium -> 2, Large -> 4}, _, x]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-width"|"border-right-width"|"border-bottom-width"|"border-left-width", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderWidth[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-width", Top, "border-right-width",  Right, "border-bottom-width", Bottom, "border-left-width", Left];
			{FrameStyle -> wrapper[value], CellFrame -> wrapper[convertToCellThickness @ value]}
		]
	]	
	
(* sets all frame edge thickness at once *)
parse[prop:"border-width", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleBorderWidth[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			skipWhitespace[pos, l, tokens]
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]},
				2, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]},
				3, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}, 
				4, {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{FrameStyle -> results, CellFrame -> Map[convertToCellThickness, results, {2}]}
	]


(* ::Subsubsection::Closed:: *)
(*border (-top, -bottom, -right, -left)*)


(* 
	Shorthand for border-*-width/style/color. 
	This effectively resets all edge properties because any property not specified takes on its default value. 
	'border' by itself sets all 4 edges to be the same.
*)
parse[prop:"border"|"border-top"|"border-right"|"border-bottom"|"border-left", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[
	{
		pos = 1, l = Length[tokens], value, 
		wrapper = Switch[prop, "border-left", Left, "border-right", Right, "border-top", Top, "border-bottom", Bottom, _, Through[{Left, Right, Top, Bottom}[#]]&],
		values = <|
			"c" -> initialValues[prop <> "-color"], 
			"s" -> initialValues[prop <> "-style"], 
			"w" -> initialValues[prop <> "-width"]|>,
		hasColor = False, hasStyle = False, hasWidth = False
	},
		
		(* if only one token is present, then check that it is a universal keyword *)
		If[l == 1,
			Switch[normalizeKeyWord @ tokenString @ tokens[[1]],
				"inherit", Return @ {FrameStyle -> wrapper[Inherited], CellFrame -> wrapper[Inherited], CellFrameStyle -> wrapper[Inherited]},
				"initial", Return @ {
					FrameStyle -> wrapper[Directive[Values @ values]], 
					CellFrameStyle -> wrapper[Directive[values["c"], values["s"]]], 
					CellFrame -> wrapper[convertToCellThickness @ values["w"]]}, 
				_, Null
			]
		];
		
		(* Ignore 'inherit' and 'initial' universal keywords. Other keywords are unique. *)
		While[pos <= l,
			Which[
				tokenType @ tokens[[pos]] == "function", (* only color can be a function *)
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")]; 
					hasColor = True; values["c"] = parseSingleColor[prop, consumeFunction[pos, l, tokens]],
					
				!FailureQ[value = parseSingleColor[prop, {tokens[[pos]]}]],
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")];
					hasColor = True; values["c"] = value,
				
				!FailureQ[value = parseSingleBorderStyle[prop, tokens[[pos]]]],
					If[hasStyle, Return @ repeatedPropValueFailure @ (prop <> "-style")];
					hasStyle = True; values["s"] = value,
					
				!FailureQ[value = parseSingleBorderWidth[prop, tokens[[pos]]]],
					If[hasWidth, Return @ repeatedPropValueFailure @ (prop <> "-width")];
					hasWidth = True; values["w"] = value,
				
				True, unrecognizedValueFailure @ prop						
			];
			skipWhitespace[pos, l, tokens];
		];
		
		{
			FrameStyle -> wrapper[Directive[Values @ values]], 
			CellFrameStyle -> wrapper[Directive[values["c"], values["s"]]], 
			CellFrame -> wrapper[convertToCellThickness @ values["w"]]}
	]


(* ::Subsection::Closed:: *)
(*clip*)


parse[prop:"clip", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l == 1, (* if only one token is present, then it should be a keyword *)
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"initial",  Missing["Not supported."],
						"inherit",  Missing["Not supported."],
						"auto",     Missing["Not supported."],
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			]
			,
			If[tokenType @ tokens[[pos]] == "function", 
				value = parseRect @ consumeFunction[pos, l, tokens]; (* FIXME: actually parse this? is it even worth it since it's not supported? *)
				Which[
					FailureQ[value], value, 
					pos < l,         tooManyTokensFailure @ prop,
					True,            Missing["Not supported."]
				]
				,
				unrecognizedValueFailure @ prop
			]
		]
	]


(* ::Subsection::Closed:: *)
(*color*)


parse[prop:"color", tokens:{__?validTokenQ}] := FontColor -> parseSingleColor[prop, tokens]


(* ::Subsection::Closed:: *)
(*content, lists, and quotes*)


(* ::Subsubsection::Closed:: *)
(*content*)


(* only used to add content before or after element, so let's restrict this to Cells' CellDingbat or CellLabel *)
parse[prop:"content", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, parsedValues = {}},
		While[pos <= l,
			value = 
				Switch[tokenType @ tokens[[pos]],
					"ident", 
						Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
							"normal",         CellDingbat->"\[FilledCircle]",
							"none",           CellDingbat->None,
							"inherit",        CellDingbat->Inherited,
							"initial",        initialValues @ prop,
							"open-quote",     Missing["Not supported."],
							"close-quote",    Missing["Not supported."],
							"no-open-quote",  Missing["Not supported."],
							"no-close-quote", Missing["Not supported."],
							_,                unrecognizedKeyWordFailure @ prop
						],
					"string", CellLabel -> tokenString @ tokens[[pos]], (* is this even doing this option justice? *)
					"uri",    With[{i = parseURI @ tokenString @ tokens[[pos]]}, If[FailureQ[i] || MissingQ[i], notAnImageFailure @ tokenString @ tokens[[pos]], CellDingbat -> i]],
					"function", 
						Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
							"counter(" | "counters(", CellDingbat -> parseCounter[prop, consumeFunction[pos, l, tokens]],
							"attr(",                  (*TODO*)parseAttr[prop, consumeFunction[pos, l, tokens]],
							_,                        unrecognizedValueFailure @ prop
						],
					_, unrecognizedValueFailure @ prop
				];
			AppendTo[parsedValues, value];
			skipWhitespace[pos, l, tokens];
		];
		Which[
			Count[parsedValues, Inherited] > 1, Return @ repeatedPropValueFailure @ "inherit",
			Count[parsedValues, None] > 1,      Return @ repeatedPropValueFailure @ "none",
			Count[parsedValues, Normal] > 1,    Return @ repeatedPropValueFailure @ "normal",
			True, parsedValues
		]
	]


(* ::Subsubsection::Closed:: *)
(*counter-increment*)


(* In WL each style must be repeated n times to get an increment of n *)
parse[prop:"counter-increment", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v, values = {}, next},
		While[pos <= l,
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = initialValues @ prop],
						_,         
							If[pos == l, 
								(* if the end is an ident then it simply adds itself once to the list of styles to increment *)
								values = Join[values, {tokenString @ tokens[[pos]]}]
								,
								(* otherwise check for a non-negative integer and add that style name n times *)
								v = tokenString @ tokens[[pos]]; next = pos; skipWhitespace[next, l, tokens];
								With[{n = Interpreter["Integer"][tokenString @ tokens[[next]]]}, 
									If[IntegerQ[n],
										If[n < 0, 
											Return @ negativeIntegerFailure @ prop
											,
											values = Join[values, ConstantArray[v, n]]; pos = next]
										,
										values = Join[values, {v}]]]]
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
			skipWhitespace[pos, l, tokens];
		];
		If[FailureQ[values], values, CounterIncrements -> values]
	]


(* ::Subsubsection::Closed:: *)
(*counter-reset*)


parse[prop:"counter-reset", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v = {}, values = {}, next},
		While[pos <= l,
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = initialValues @ prop],
						_,         
							If[pos == l, 
								(* if the end is an ident then it has 0 as its counter assignment *)
								AppendTo[values, {tokenString @ tokens[[pos]], 0}]
								,
								(* otherwise check for an integer *)
								v = tokenString @ tokens[[pos]]; next = pos; skipWhitespace[next, l, tokens];
								With[{n = Interpreter["Integer"][tokenString @ tokens[[next]]]}, 
									If[IntegerQ[n], (* if integer exists, use it and skip ahead, otherwise use 0 and don't increment pos *)
										AppendTo[values, {v, n}]; pos = next
										,
										AppendTo[values, {v, 0}]]]]
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
			skipWhitespace[pos, l, tokens];
		];
		If[FailureQ[values], values, CounterAssignments -> values]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-image*)


parseSingleListStyleImage[prop_String, token_?validTokenQ] := 
	Switch[tokenType @ token,
		"ident",
			Switch[normalizeKeyWord @ tokenString @ token,
				"inherit", Inherited,
				"initial", initialValues @ prop, 
				"none",    None,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"uri", 
			With[{im = parseURI @ tokenString @ token}, 
				Which[
					FailureQ[im], tokenString @ token, 
					MissingQ[im], tokenString @ token,
					!ImageQ[im],  notAnImageFailure @ tokenString @ token,
					True,         ToBoxes @ Dynamic @ Image[im, ImageSize -> CurrentValue[FontSize]]
				]
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-image", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleImage[prop, First @ tokens];
		If[FailureQ[value], value, CellDingbat -> value]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-position*)


(* 
	CellDingbat position is always outside the cell content and aligned with the first line of content.
	Though the following validates the CSS, Mathematica does not include any position option.
*)
parseSingleListStylePosition[prop_String, token_?validTokenQ] := 
	Switch[tokenType @ token,
		"ident",
			Switch[normalizeKeyWord @ tokenString @ token,
				"inherit", Inherited,
				"initial", initialValues @ prop, 
				"inside",  Missing["Not supported."],
				"outside", Automatic,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-position", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStylePosition[prop, First @ tokens];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-type*)


parseSingleListStyleType[prop_String, token_?validTokenQ, style_String:"Item"] := 
	Switch[tokenType @ token,
		"ident",
			Switch[normalizeKeyWord @ tokenString @ token,
				"inherit",              Inherited,
				"initial",              initialValues @ prop, 
				"disc",                 "\[FilledCircle]",
				"circle",               "\[EmptyCircle]",
				"square",               "\[FilledSquare]",
				"decimal",              Cell[TextData[{CounterBox[style], "."}]],
				"decimal-leading-zero", Cell[TextData[{CounterBox[style, CounterFunction :> (FEPrivate`If[FEPrivate`Greater[#, 9], #, FEPrivate`StringJoin["0", FEPrivate`ToString[#]]]&)], "."}]],
				"lower-roman",          Cell[TextData[{CounterBox[style, CounterFunction :> FrontEnd`RomanNumeral], "."}]],
				"upper-roman",          Cell[TextData[{CounterBox[style, CounterFunction :> FrontEnd`CapitalRomanNumeral], "."}]],
				"lower-greek",          Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["\[Alpha]", "\[Omega]"], #]&)], "."}]],
				"lower-latin",          Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["a", "z"], #]&)], "."}]],
				"upper-latin",          Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)], "."}]],
				"armenian",             Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["\:0531", "\:0556"], #]&)], "."}]],
				"georgian",             Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["\:10d0", "\:10fa"], #]&)], "."}]],
				"lower-alpha",          Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["a", "z"], #]&)], "."}]],
				"upper-alpha",          Cell[TextData[{CounterBox[style, CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)], "."}]],
				"none",                 None,
				_,                      unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-type", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleType[prop, First @ tokens];
		If[FailureQ[value], value, CellDingbat -> value]
	]


(* ::Subsubsection::Closed:: *)
(*list-style*)


(* short-hand for list-style-image/position/type properties given in any order *)
parse[prop:"list-style", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, values, p, noneCount = 0, hasImage = False, hasPos = False, hasType = False},
		(* 
			li-image, li-position, and li-type can appear in any order.
			A value of 'none' sets whichever of li-type and li-image are not otherwise specified to 'none'. 
			If both are specified, then an additional 'none' is an error.
		*)
		values = <|"image" -> None, "pos" -> Missing["Not supported."], "type" -> None|>;
		While[pos <= l,
			If[TrueQ[normalizeKeyWord @ tokenString @ tokens[[pos]] == "none"], 
				noneCount++
				,
				value = Through[{parseSingleListStyleImage, parseSingleListStylePosition, parseSingleListStyleType}[prop, tokens[[pos]]]];
				p = FirstPosition[value, Except[_?FailureQ], Return @ unrecognizedValueFailure @ prop, {1}, Heads -> False][[1]];
				Switch[p, 
					1, If[hasImage, Return @ repeatedPropValueFailure @ "image",    values["image"] = value[[p]]; hasImage = True], 
					2, If[hasPos,   Return @ repeatedPropValueFailure @ "position", values["pos"] = value[[p]];   hasPos = True], 
					3, If[hasType,  Return @ repeatedPropValueFailure @ "type",     values["type"] = value[[p]];  hasType = True]
				];
			];
			skipWhitespace[pos, l, tokens];
		];
		Which[
			hasImage && hasType && noneCount > 0, repeatedPropValueFailure @ "none",
			hasImage, CellDingbat -> values["image"], (* default to Image if it could be found *)
			hasType,  CellDingbat -> values["type"],
			True,     CellDingbat -> None]
	]


(* ::Subsubsection::Closed:: *)
(*quotes*)


(*
	Quotes could be implemented using DisplayFunction -> (RowBox[{<open-quote>,#,<close-quote>}]&),
	but only a handful of boxes accept this WL option e.g. DynamicBoxOptions, ValueBoxOptions, and a few others.
	There's also ShowStringCharacters, but this only hides/shows the double quote.
	We treat this then as not available, but we validate the form anyway.
*)
parse[prop:"quotes", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v, values = {}, next},
		While[pos <= l,
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokenString @ tokens[[pos]], values = initialValues @ prop]
					],
				"string",
					v = tokenString @ tokens[[pos]]; next = pos; skipWhitespace[next, l, tokens];
					If[next <= l && tokenType @ tokens[[next]] == "string", 
						AppendTo[values, {v, tokenString @ tokens[[next]]}]; pos = next
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Expected pairs of strings."|>]
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
			skipWhitespace[pos, l, tokens]
		];
		If[FailureQ[values], values, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*cursor*)


(* WL uses a TagBox[..., MouseAppearanceTag[""]] instead of an option to indicate mouse appearance *)
parse[prop:"cursor", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit",       Inherited,
						"initial",       initialValues @ prop,
						"auto",          Automatic,
						"copy",          "DragAndDrop",
						"crosshair",     "Crosshair",
						"default",       "Arrow",
						"help",          "Help",
						"move",          "FrameMove",
						"pointer",       "LinkHand",
						"progress",      ProgressIndicator[Appearance -> "Necklace"],
						"text",          "Edit",
						"vertical-text", "MathEdit90",
						"wait",          ProgressIndicator[Appearance -> "Necklace"],
						"e-resize"|"w-resize",   "FrameLRResize",
						"n-resize"|"s-resize",   "FrameTBResize",
						"nw-resize"|"se-resize", "FrameFallingResize",
						"ne-resize"|"sw-resize", "FrameRisingResize",
						_,               unrecognizedKeyWordFailure @ prop
					],
				"uri", parseURI @ tokenString @ tokens[[pos]],
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, With[{v = value}, MouseAppearance[#, v]&]]
	]		


(* ::Subsection::Closed:: *)
(*display*)


(*
	WL automatically lays out blocks either inline or as nested boxes. 
	If a Cell appears within TextData or BoxData then it is considered inline.
*)
parse[prop:"display", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inline",             Automatic, (* wrap in Cell[]? *)
						"block",              Automatic,
						"list-item",          Automatic,
						"inline-block",       Automatic,
						"table",              Automatic,
						"inline-table",       Automatic,
						"table-row-group",    Automatic,
						"table-header-group", Automatic,
						"table-footer-group", Automatic,
						"table-row",          Automatic,
						"table-column-group", Automatic,
						"table-column",       Automatic,
						"table-cell",         Automatic,
						"table-caption",      Automatic,
						"none",               Automatic,
						"inherit",            Inherited,
						"initial",            initialValues @ prop,
						_,                    unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*float, clear*)


(* WL does not support the flow of boxes around other boxes. *)
parse[prop:"float", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"left",    Automatic,
						"right",   Automatic,
						"none",    Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


parse[prop:"clear", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"left",    Automatic,
						"right",   Automatic,
						"both",    Automatic,
						"none",    Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*font *)


(* ::Subsubsection::Closed:: *)
(*font*)


(*
	Short-hand for other font properties. If not a keyword, then font-size and font-family are required, in that order.
	[['font-style' || 'font-variant' || 'font-weight' ]? 'font-size' [ / 'line-height' ]? 'font-family' ]
	All font properties are reset to their initial values, then the listed properties are calculated.
*)
parse[prop:"font", tokens:{__?validTokenQ}] :=
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
		If[l == 1, (* if only one token is present, then it should be a keyword that represents a system font (or font style?) *)
			newValue = 
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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
					"initial",          {} (* keep reset values *),
					"italic"|"oblique", {parse["font-style", tokens]},
					"small-caps",       {parse["font-variant", tokens]},
					_,                  unrecognizedKeyWordFailure @ prop
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
			If[!MatchQ[normalizeKeyWord @ tokenString @ tokens[[pos]], "normal" | "initial" | "inherit"],
				AppendTo[newValue, FirstCase[parse[#, {tokens[[pos]]}]& /@ {"font-style", "font-variant", "font-weight"}, _Rule, Nothing]]
			];
			skipWhitespace[pos, l, tokens];
		];
		
		(* font-size must appear next *)
		If[pos > l, Return @ noValueFailure["font-size"], AppendTo[newValue, temp]; pos++];
		
		(* an optional line-height property can immediately follow font-size with a '/' in between (no whitespace allowed) *)
		If[pos > l, Return @ noValueFailure["font-family"]];
		If[MatchQ[tokens[[pos]], {"operator", "/"}],
			pos++; 
			temp = parse["line-height", {tokens[[pos]]}]; 
			If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]; skipWhitespace[pos, l, tokens]];
		];
		
		(* everything else must be a font-family *)
		temp = parse["font-family", tokens[[pos ;;]]];
		If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]];
		
		(* overwrite old with any new values *)
		DeleteDuplicates[Join[newValue, value], SameQ[First[#1], First[#2]]&]
	]


(* ::Subsubsection::Closed:: *)
(*font-family*)


parse[prop:"font-family", tokens:{__?validTokenQ}] :=
	Module[{fontTokens, parsed, result},
		fontTokens = DeleteCases[SplitBy[tokens, MatchQ[{"operator", ","}]], {{"operator", ","}}];
		parsed = parseSingleFontFamily /@ fontTokens;
		result = FirstCase[parsed, _Failure, None]; (* FIXME: perhaps use FontSubstitutions here? *)
		If[FailureQ[result], Return @ result];
		FirstCase[parsed, _Rule, Failure["UnexpectedParse", <|"Message" -> "No font-family found."|>]]
	]

parseSingleFontFamily[tokens:{__?validTokenQ}] := parseSingleFontFamily[tokens] =
	Module[
	{
		value, l, pos = 1, font, tokensNoWS,
		(* first two generic font names are actually non-font keywords *)
		generic = {"inherit", "initial", "serif", "sans-serif", "monospace", "fantasy", "cursive"},
		fail = Failure["UnexpectedParse", <|"Message" -> "Font family syntax error."|>]
	},
		tokensNoWS = DeleteCases[tokens, " ", {1}];
		l = Length[tokensNoWS];
		value =
			Switch[tokenType @ tokensNoWS[[pos]],
				"ident", (* all other tokens must be 'ident' (or whitespace); it could be only be a single 'ident' *)
					Which[
						!AllTrue[tokenType /@ tokensNoWS, StringMatchQ["ident"]], fail,
						l == 1 && MemberQ[generic, normalizeKeyWord @ tokenString @ tokensNoWS[[pos]]], parseFontFamilySingleIdent @ tokenString @ tokensNoWS[[pos]],
						True, 
							font = StringJoin @ Riffle[tokenString /@ tokensNoWS, " "];
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
					],
				"string", (* must only have a single string token up to the delimiting comma *)
					Which[
						l > 1, fail,
						True,
							font = StringTake[tokenString /@ tokensNoWS, {2, -2}];
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
					],
				_, fail
			];
		If[FailureQ[value] || MissingQ[value], value, FontFamily -> value]
	]
	
(*
	Perhaps as an option we should allow the user to define default fonts, e.g.
		GenericFontFamily -> {"fantasy" -> "tribal-dragon"}
	but always default to Automatic if it can't be found on the system.
*)
parseFontFamilySingleIdent[s_String] := 
	Switch[normalizeKeyWord @ s,
		"inherit",    Inherited,
		"initial",    initialValues["font-family"], 
		"serif",      "Times New Roman",
		"sans-serif", "Arial",
		"fantasy",    Automatic, (* no system has a default 'fantasy' font *)
		"cursive",    
			If[MemberQ[$FontFamilies, "French Script MT"], 
				"French Script MT" (* I have a preference for this, but 'Brush Script' should be available on Mac, unsure on Linux. *)
				, 
				First[
					Select[$FontFamilies, StringContainsQ[#, "script", IgnoreCase -> True] && !StringContainsQ[#, "bold", IgnoreCase -> True]&],
					Automatic]
			],
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


parse[prop:"font-size", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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
				"dimension",  If[tokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, parseLength[tokens[[pos]], True]],
				"percentage", If[tokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, parsePercentage @ tokens[[pos]]],
				"number",     parseZero @ tokens[[pos]],
				_,            unrecognizedValueFailure @ prop 
			];
		If[FailureQ[value], value, FontSize -> value]
	]


(* ::Subsubsection::Closed:: *)
(*font-style*)


parse[prop:"font-style", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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


parse[prop:"font-variant", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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
parse[prop:"font-weight", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
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
								(* 200 -> "Extra Light", *) (* Ultra Light *)
								300 -> "Light", 
								400 -> Plain, 
								500 -> "Medium", 
								600 -> "SemiBold", (* Demi Bold *)
								700 -> Bold,
								(* 800 -> "Extra Bold", *) (* Ultra Bold *)
								900 -> "Black" (* Heavy *)}, 
							Clip[tokenValue @ tokens[[pos]], {1, 1000}]], 
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
parseSingleSize[prop_String, token_?validTokenQ] := parseSingleSize[prop, token] =
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"auto",    Automatic, (* let Mathematica decide what to do *)
				"none",    If[!StringMatchQ[prop, "max-height" | "max-width"], unrecognizedKeyWordFailure @ prop, Infinity],
				_,         unrecognizedKeyWordFailure @ prop
			],
		"dimension",  If[tokenValue @ token < 0, negativeLengthFailure @ prop, parseLength @ token],
		"number",     parseZero @ token,
		"percentage", parsePercentage @ token, (* should be percentage of height of containing block; not possible in WL *)
		_,            unrecognizedValueFailure @ prop
	]
	
(* min-width and max-width override width property *)
parse[prop:"width"|"max-width"|"min-width", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"width",     {WidthMin[value], WidthMax[value]},
					"max-width", WidthMax[value],
					"min-width", WidthMin[value]
				]
		]
	]

parse[prop:"height"|"max-height"|"min-height", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"height",     {HeightMin[value], HeightMax[value]},
					"max-height", HeightMax[value],
					"min-height", HeightMin[value]
				]
		]
	]


(* ::Subsection::Closed:: *)
(*line-height*)


(* 
	Similar to WL LineSpacing, but LineSpacing already takes FontSize into account.
	Thus, we intercept 'em' and 'ex' before getting a dynamic FontSize and we keep 
	the percentage from being wrapped in Scaled. *)
parse[prop:"line-height", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues[prop], 
						"normal",  {1.2, 0},
						_,         unrecognizedKeyWordFailure @ prop
					],
				"dimension", 
					If[tokenValue @ tokens[[pos]] < 0, 
						negativeLengthFailure @ prop
						,
						Switch[dimensionUnit @ tokens[[pos]],
							"em"|"ex", {parseLengthNonRelative @ tokens[[pos]], 0},
							_,         {parseLength @ tokens[[pos]], 0}
						]
					],
				"number",     If[tokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, {tokenValue @ tokens[[pos]], 0}],
				"percentage", If[tokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, {(tokenValue @ tokens[[pos]])/100, 0}],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, LineSpacing -> value]
	]


(* ::Subsection::Closed:: *)
(*margin(-left, -right, -top, -bottom)*)


parseSingleMargin[prop_String, token_?validTokenQ] := parseSingleMargin[prop, token] = 
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"auto",    Automatic, (* let FE decide what to do *)
				_,         unrecognizedKeyWordFailure @ prop
			],
		"dimension",  parseLength @ token,
		"number",     tokenValue @ token,
		"percentage", parsePercentage @ token,
		_,            unrecognizedValueFailure @ prop
	]
	
(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
parse[prop:"margin-top"|"margin-right"|"margin-bottom"|"margin-left", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleMargin[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "margin-left", Left, "margin-right", Right, "margin-bottom", Bottom, "margin-top", Top];
			{ImageMargins -> wrapper[value], CellMargins -> wrapper[value]}
		]
	]
		
parse[prop:"margin", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleMargin[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			skipWhitespace[pos, l, tokens];
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]},
				2, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]},
				3, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}, 
				4, {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{ImageMargins -> results, CellMargins -> results}
	]


(* ::Subsection::Closed:: *)
(*outline*)


(* 
	FE does not really have an option to do outlines that take up no space. 
	E.g. WL Tooltip always appears off to the side and would otherwise cover the original box.
	E.g. An attached cell overlaying a box would also prevent interaction with the original box.
	Wrapping an expressing in Framed addes a FrameBox which takes up space.
*)


(* ::Subsubsection::Closed:: *)
(*outline-color*)


parse[prop:"outline-color", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l == 1 && tokenType @ tokens[[1]] == "ident" && normalizeKeyWord @ tokenString @ tokens[[1]] == "invert",
			CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], ColorNegate @ CurrentValue[CellFrameColor], Inherited]]
			,
			While[pos <= l,
				value = parseSingleColor[prop, If[tokenType @ tokens[[pos]] == "function", consumeFunction[pos, l, tokens], {tokens[[pos]]}]];
				If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
				skipWhitespace[pos, l, tokens];
			];
			Switch[Length[results],
				1, Missing["Not supported."](*With[{c = First @ results}, CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], c, Inherited]]]*),
				_, tooManyTokensFailure @ tokens
			]
		]
	]


(* ::Subsubsection::Closed:: *)
(*outline-style*)


(* only a solid border is allowed for cells; 'hidden' is not allowed here *)
parse[prop:"outline-style", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value =
			If[tokenType @ tokens[[pos]] == "ident" && tokenString @ tokens[[pos]] == "hidden",
				unrecognizedKeyWordFailure @ prop
			,
				parseSingleBorderStyle[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-width*)


parse[prop:"outline-width", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = parseSingleBorderWidth[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			(*With[{t = Round @ convertToCellThickness @ value},
				{
					CellFrame -> Dynamic[If[CurrentValue["MouseOver"], t, Inherited]],
					CellFrameMargins -> Dynamic[If[CurrentValue["MouseOver"], -t, Inherited]]
			}*)
			Missing["Not supported."]
		]
	]


(* ::Subsubsection::Closed:: *)
(*outline*)


(* Shorthand for outline-width/style/color. 'outline' always sets all 4 edges to be the same. *)
parse[prop:"outline", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[
	{
		pos = 1, l = Length[tokens], value, 
		values = <|
			"c" -> initialValues[prop <> "-color"], 
			"s" -> initialValues[prop <> "-style"], 
			"w" -> initialValues[prop <> "-width"]|>,
		hasColor = False, hasStyle = False, hasWidth = False
	},
		
		(* if only one token is present, then check that it is a universal keyword *)
		If[l == 1,
			Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
				"inherit", Return @ Missing["Not supported."],
				"initial", Return @ Missing["Not supported."], 
				_, Null
			]
		];
		
		(* Ignore 'inherit' and 'initial' universal keywords. Other keywords are unique. *)
		While[pos <= l,
			Which[
				tokenType @ tokens[[pos]] == "function", (* only color can be a function *)
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")]; 
					hasColor = True; values["c"] = parseSingleColor[prop, consumeFunction[pos, l, tokens]],
					
				!FailureQ[value = parseSingleColor[prop, {tokens[[pos]]}]],
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")];
					hasColor = True; values["c"] = value,
				
				!FailureQ[value = parseSingleBorderStyle[prop, tokens[[pos]]]],
					If[hasStyle, Return @ repeatedPropValueFailure @ (prop <> "-style")];
					hasStyle = True; values["s"] = value,
					
				!FailureQ[value = parseSingleBorderWidth[prop, tokens[[pos]]]],
					If[hasWidth, Return @ repeatedPropValueFailure @ (prop <> "-width")];
					hasWidth = True; values["w"] = value,
				
				True, unrecognizedValueFailure @ prop						
			];
			skipWhitespace[pos, l, tokens];
		];
		
		Missing["Not supported."]
	]


(* ::Subsection::Closed:: *)
(*overflow*)


(* 
	This would mostly be found with WL Pane expression as PaneBox supports scrollbars. 
	Other boxes may support ImageSizeAction.
*)
parse[prop:"overflow", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokenType @ tokens[[pos]],
			"ident",
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
					"visible", Missing["Not supported."],
					"hidden",  {ImageSizeAction -> "Clip", Scrollbars -> False},
					"scroll",  {ImageSizeAction -> "Clip", Scrollbars -> True},
					"auto",    ImageSizeAction -> "Scrollable",
					"inherit", ImageSizeAction -> Inherited,
					"initial", initialValues @ prop,
					_,         unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*padding(-left, -right, -top, -bottom)*)


parseSinglePadding[prop_String, token_?validTokenQ] := parseSinglePadding[prop, token] = 
	Switch[tokenType @ token,
		"ident", 
			Switch[normalizeKeyWord @ tokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"dimension",  If[tokenValue @ token < 0, negativeLengthFailure @ prop, parseLength @ token],
		"percentage", If[tokenValue @ token < 0, negativeLengthFailure @ prop, parsePercentage @ token],
		_,            unrecognizedValueFailure @ prop
	]


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
parse[prop:"padding-top"|"padding-right"|"padding-bottom"|"padding-left", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSinglePadding[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "padding-left", Left, "padding-right", Right, "padding-bottom", Bottom, "padding-top", Top];
			{FrameMargins -> wrapper[value], CellFrameMargins -> wrapper[value]}
		]
	]
		
parse[prop:"padding", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSinglePadding[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			skipWhitespace[pos, l, tokens];
		];
		(* expand out results  to {{L,R},{B,T}} *)
		results = 
			Switch[Length[results],
				1, {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]},
				2, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]},
				3, {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}, 
				4, {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
				_, Return @ tooManyTokensFailure @ tokens
			];
		{FrameMargins -> results, CellFrameMargins -> results}
	]


(* ::Subsection::Closed:: *)
(*page breaks*)


(* ::Subsubsection::Closed:: *)
(*orphans/widows*)


(* 
	FE uses LinebreakAdjustments with a blackbox algorithm. 
	AFAIK there's no way to directly prevent orphans/widows.
*)
parse[prop:"orphans"|"widows", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"number", 
					If[tokenValue @ tokens[[pos]] < 1 || tokenValueType @ tokens[[pos]] != "integer", 
						positiveLengthFailure @ prop
						, 
						tokenValue @ tokens[[pos]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-after/before*)


parse[prop:("page-break-after"|"page-break-before"), tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"auto",    Automatic,
						"always",  True,
						"avoid",   False,
						"left",    Missing["Not supported."],
						"right",   Missing["Not supported."],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], 
			value
			, 
			Switch[prop, "page-break-after", PageBreakBelow, "page-break-before", PageBreakAbove] -> value
		]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-inside*)


parse[prop:"page-break-inside", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"auto",    Automatic, 
						"avoid",   False,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], 
			value
			, 
			{PageBreakWithin -> value, GroupPageBreakWithin -> value}
		]
	]


(* ::Subsection::Closed:: *)
(*position (and top, left, bottom, right)*)


(*
	'position' is most like WL's Alignment, but only relative to the parent box.
	WL does not support absolute positioning of cells and boxes.
	Attached cells can be floated or positioned absolutely, but are ephemeral and easily invalidated.
	Moreover, attached cells aren't an option, but rather a cell.
	Perhaps can use NotebookDynamicExpression -> Dynamic[..., TrackedSymbols -> {}] where "..." includes a list of attached cells.
*)
parse[prop:"position", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"static",   Automatic, (* normal layout, ignoring any left/right/top/bottom offsets *)
						"relative", Automatic, (* normal layout, offset relative to normal position and floats above "siblings" *)
						"absolute", Automatic, (* non-normal layout, attached cell attached to a parent box with absolute offset *)
						"fixed",    Automatic, (* non-normal layout, notebook-attached-cell in Working mode (ignores scrolling), appears on each page in Printout mode (header/footer?) *)
						"inherit",  Automatic,
						"initial",  initialValues @ prop,
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


parse[prop:"left"|"right"|"top"|"bottom", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"auto",    Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"percentage", 
					With[{n = parsePercentage @ tokens[[pos]]}, 
						If[n > 100 || n < 0, Missing["Out of range."], Rescale[n, {0, 100}, Switch[prop, "left"|"bottom", {-1, 1}, "right"|"top", {1, -1}]]]
					],
				"number", 
					With[{n = parseZero @ tokens[[pos]]}, 
						If[TrueQ[n == 0], Switch[prop, "left", Left, "right", Right, "top", Top, "bottom", Bottom], Missing["Not supported."]]
					],
				"dimension", 
					With[{n = parseLength @ tokens[[pos]]}, 
						If[TrueQ[n == 0], Switch[prop, "left", Left, "right", Right, "top", Top, "bottom", Bottom], Missing["Not supported."]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Alignment -> Switch[prop, "left"|"right", {value, Automatic}, "top"|"bottom", {Automatic, value}]]
	]


(* ::Subsection::Closed:: *)
(*table*)


(* ::Subsubsection::Closed:: *)
(*caption-side*)


(* WL Grid does not support an option to have a grid caption. *)
parse[prop:"caption-side", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"top",     Automatic,
						"bottom",  Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*empty-cells*)


(* There is no WL equivalent because FE uses only 'border-collapse' in Grid.*)
parse[prop:"empty-cells", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"show",    Automatic,
						"hide",    Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*table-layout*)


(* The FE does its own formatting passes depending on the column width settings and content. *)
parse[prop:"table-layout", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"auto",    Automatic,
						"fixed",   Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*text*)


(* ::Subsubsection::Closed:: *)
(*direction*)


parse[prop:"direction", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"ltr",     Automatic,
						"rtl",     Missing["Not supported."],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*text-align*)


(* WL distinguishes between alignment and justification, but CSS does not *)
parse[prop:"text-align", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokenType @ tokens[[pos]],
			"ident",
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
					"left",    TextAlignment -> Left,
					"right",   TextAlignment -> Right,
					"center",  TextAlignment -> Center,
					"justify", TextJustification -> 1,
					"inherit", TextAlignment -> Automatic,
					"initial", TextAlignment -> initialValues @ prop,
					_,         unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*text-indent*)


parse[prop:"text-indent", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"dimension",
					Switch[dimensionUnit @ tokens[[pos]],
						"em"|"ex", parseLengthNonRelative @ tokens[[pos]],
						_,         parseLength @ tokens[[pos]]
					],
				"number",     parseZero @ tokens[[pos]],
				"percentage", Missing["Not supported."],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, {LineIndent -> value, ParagraphIndent -> value}]
	]


(* ::Subsubsection::Closed:: *)
(*text-decoration*)


(* WL distinguishes between alignment and justification, but CSS does not *)
parse[prop:"text-decoration", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value =
				Switch[tokenType @ tokens[[pos]],
					"ident",
						Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
							"none",         If[pos > 1, tooManyTokensFailure @ "none",    Nothing],
							"inherit",      If[pos > 1, tooManyTokensFailure @ "inherit", Inherited],
							"initial",      If[pos > 1, tooManyTokensFailure @ "initial", Nothing],
							"underline",    "Underline" -> True,
							"overline",     "Overline" -> Missing["Not supported."], (* OverBar is a function, not an option in WL *)
							"line-through", "StrikeThrough" -> True,
							"blink",        "Blink" -> Missing["Not supported."],
							_,              unrecognizedKeyWordFailure @ prop
						],
					_, unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[values, value]];
			skipWhitespace[pos, l, tokens];
		];
		FontVariations -> values
	]


(* ::Subsubsection::Closed:: *)
(*text-transform*)


parse[prop:"text-transform", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokenType @ tokens[[pos]],
			"ident",
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
					"capitalize", Missing["Not supported."], (* Not by the FE at least, but see WL Capitalize[..., "AllWords"] *)
					"uppercase",  FontVariations -> {"CapsType" -> "AllCaps"},
					"lowercase",  FontVariations -> {"CapsType" -> "AllLower"},
					"none",       FontVariations -> {"CapsType" -> "Normal"},
					"inherit",    FontVariations -> {"CapsType" -> Inherited},
					"initial",    FontVariations -> {"CapsType" -> initialValues @ prop},
					_,            unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*letter-spacing*)


(* 
	General letter and word spacing is controlled by the Mathematica Front End.
	The FontTracking options gives some additional control, but appears to be mis-appropriated to CSS font-stretch.
	Not to be confused with CSS font-stretch.
*)
parse[prop:"letter-spacing", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						"normal",  "Plain",
						_,         unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]


(*(* 
	This was originally in CSS 2, but removed in CSS 2.1 due to lack of UA support.
	Added back in Level 3. CSS Fonts Module Level 4 supports percentages as well.
	Mathematica supports both level 3 and 4 features in FontTracking.
*)
parse[prop:"font-stretch", tokens:{__?validTokenQ}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident", 
					Switch[normalizeKeyWord @ tokenString @ tokens[[1]],
						"initial",         initialValues @ prop,
						"inherit",         Inherited,
						"ultra-condensed", "Narrow",        (* CSSFM4 50% *)
						"extra-condensed", "Narrow",        (* CSSFM4 62.5% *)
						"condensed",       "Condensed",     (* CSSFM4 75% *)
						"semi-condensed",  "SemiCondensed", (* CSSFM4 87.5% *)
						"normal",          Plain,           (* CSSFM4 100% *)
						"semi-expanded",   "Extended",      (* CSSFM4 112.5% *)
						"expanded",        "Extended",      (* CSSFM4 125% *)
						"extra-expanded",  "Wide",          (* CSSFM4 150% *)
						"ultra-expanded",  "Wide",          (* CSSFM4 200% *)
						_,                 unrecognizedKeyWordFailure @ prop
					],
				"percentage", If[tokenValue @ tokens[[1]] < 0, negativeValueFailure @ prop, (tokenValue @ n)/100],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]*)


(* ::Subsubsection::Closed:: *)
(*unicode-bidi*)


parse[prop:"unicode-bidi", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"normal",        Automatic,
						"embed",         Missing["Not supported."],
						"bidi-override", Missing["Not supported."],
						"inherit",       Inherited,
						"initial",       initialValues @ prop,
						_,               unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*word-spacing*)


(* 
	General letter and word spacing is controlled by the Mathematica Front End. 
	The CSS is still validated.
*)
parse[prop:"word-spacing", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						"normal",  "Plain",
						_,         unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*white-space*)


(* Whitespace is controlled by the Mathematica Front End. The CSS is still validated. *)
parse[prop:"white-space", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokenType @ tokens[[pos]],
			"ident",
				Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
					"inherit",  Missing["Not supported."],
					"initial",  initialValues @ prop,
					"normal",   Missing["Not supported."],
					"pre",      Missing["Not supported."],
					"nowrap",   Missing["Not supported."],
					"pre-wrap", Missing["Not supported."],
					"pre-line", Missing["Not supported."],
					_,          unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*vertical-align*)


(* 
	Applies to in-line elements or rows in a table. WL's BaselinePosition is similar. 
	WL BaselinePosition takes a rule as its value e.g. BaselinePosition -> Baseline -> Bottom.
	Lengths and percentages are w.r.t. the baseline of the surrounding element.
	Percentages are relative to the font-size i.e. 100% is one line-height upward.
	CellBaseline is limited and always aligns the top of the inline cell to the Bottom/Top/Etc of the parent
*)
parse[prop:"vertical-align", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{value1, value2},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value1 = parseBaseline[prop, First @ tokens];
		If[FailureQ[value1], Return @ value1];
		value2 = parseCellBaseline[prop, First @ tokens];
		If[FailureQ[value2], Return @ value2];
		{value1, value2}
	]

(* this is effectively for RowBox alignment *)
parseBaseline[prop:"vertical-align", token_?validTokenQ] := parseBaseline[prop, token] = 
	Module[{value (* for Baseline *)},
		(* tooManyTokens failure check occurs in higher-level function parse["vertical-align",...]*)
		value = 
			Switch[tokenType @ token,
				"ident",
					Switch[normalizeKeyWord @ tokenString @ token,
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
				"dimension",
					Switch[dimensionUnit @ token,
						"em"|"ex", Baseline -> Scaled[parseLengthNonRelative @ token],
						_,         Baseline -> With[{v = parseLength @ token}, Scaled @ Dynamic[v/CurrentValue[FontSize]]]
					],
				"percentage", Baseline -> parsePercentage @ token,
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, BaselinePosition -> value]
	]

(* it's unfortunate that CellBaseline is so limited *)
parseCellBaseline[prop:"vertical-align", token_?validTokenQ] := parseCellBaseline[prop, token] = 
	Module[{value},
		value = 
			Switch[tokenType @ token,
				"ident",
					Switch[normalizeKeyWord @ tokenString @ token,
						"baseline", Center,
						"middle",   Baseline,
						"super" | "sub", Missing["Not supported."],
						"top" | "text-top", Missing["Not supported."], (* because top of in-line is at baseline of cell *)
						"bottom" | "text-bottom", Bottom,
						"inherit", Inherited,
						"initial", Baseline,
						_, unrecognizedKeyWordFailure @ prop
					],
				"dimension", 
					Switch[dimensionUnit @ token,
						"em"|"ex", Missing["Not supported."],
						_,         parseLength @ token (* w.r.t. the top of the in-line cell *)
					],
				"number", parseZero @ token,
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, CellBaseline -> value]
	]


(* ::Subsection::Closed:: *)
(*visibility*)


(* 
	WL option ShowContents is only applicable within a StyleBox.
	Often this is implemented using the Invisible function.
*)
parse[prop:"visibility", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"visible",  True,
						"hidden",   False,
						"collapse", False,
						"inherit",  Inherited,
						"initial",  initialValues @ prop,
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, ShowContents -> value]
	]


(* ::Subsection::Closed:: *)
(*z-index*)


(* 
	The FE does its own depth ordering of boxes. 
	Attached cells are ordered by their creation order.
*)
parse[prop:"z-index", tokens:{__?validTokenQ}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokenType @ tokens[[pos]],
				"ident",
					Switch[normalizeKeyWord @ tokenString @ tokens[[pos]],
						"auto",     Automatic,
						"inherit",  Inherited,
						"initial",  initialValues @ prop,
						_,          unrecognizedKeyWordFailure @ prop
					],
				"number", 
					If[tokenValueType @ tokens[[pos]] != "integer", 
						Failure["BadValue", <|"Message" -> "Expected value is an integer."|>]
						, 
						tokenValue @ tokens[[pos]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*FALL THROUGH *)


parse[prop_String, {}] := noValueFailure @ prop


parse[prop_String, _] := unsupportedValueFailure @ prop


(* ::Section::Closed:: *)
(*Process *)


(* ::Subsection::Closed:: *)
(*Utilities*)


findOpeningBracketPosition[positionIndex_Integer, openBracket_String, tokens:{__?validTokenQ}] :=
	Module[{p = positionIndex},
		While[tokenType @ tokens[[p]] != openBracket, p++];
		p
	]


findClosingBracketPosition[positionIndex_Integer, openBracket_String, closeBracket_String, tokens:{__?validTokenQ}]:=
	Module[{p = positionIndex, depth = 1, l = Length[tokens]},
		p++;
		While[depth > 0 && p <= l,
			Switch[tokenType @ tokens[[p]],
				openBracket,  depth++; p++,
				closeBracket, depth--; p++,
				_, p++
			]
		];
		p-1
	]


Attributes[skipWhitespace] = {HoldFirst};
skipWhitespace[positionIndex_, length_Integer, tokens:{__?validTokenQ}] := (
	positionIndex++;
	While[positionIndex < length && tokenType @ tokens[[positionIndex]] == " ", positionIndex++])


Attributes[consumeFunction] = {HoldFirst};
consumeFunction[positionIndex_, l_, tokens:{__?validTokenQ}] :=
	Module[{start = positionIndex, i = 1},
		If[tokenType @ tokens[[positionIndex]] != "function", 
			$Failed
			,
			positionIndex++;
			While[positionIndex < l && i>0,
				Switch[normalizeKeyWord @ tokenType @ tokens[[positionIndex]],
					"function", i++; positionIndex++,
					")",        i--; If[i==0, Break[], positionIndex++],
					_,          positionIndex++
				]
			]; 
			tokens[[start;;positionIndex]]
		]
	]					


(* ::Subsection::Closed:: *)
(*Process rulesets ---> {{selector, declaration block}, ...}*)


processRulesets[s_String] :=
	Module[{i = 1, pos = 1, blockStartPos, blockStopPos, atBlockStartPos, atBlockStopPos, l, lRulesets, relevantTokens, rulesets, imports={}},
	
		relevantTokens = tokenizeFirstPass[s]; (* identifies curly-braces, strings, URIs, @import, @charset. Removes comments. *)
		l = Length[relevantTokens];
		
		(*TODO: handle charset statement *)
		If[tokenType @ relevantTokens[[pos]] == "charset", skipWhitespace[pos, l, relevantTokens]];
		If[tokenType @ relevantTokens[[pos]] == " ",       skipWhitespace[pos, l, relevantTokens]];
		While[tokenType @ relevantTokens[[pos]] == "import", 
			AppendTo[imports, parse["atImportKeyword", tokenizeAtImportKeyword[tokenString @ relevantTokens[[pos]]]]]; 
			skipWhitespace[pos, l, relevantTokens]
		];
		imports = Join @@ imports;
		
		(* 
			We're looking for blocks indicated by curly brackets. 
			The last declaration block may not have a closing bracket, so count only open brackets as an upper limit to the number of possible blocks.
			@keywords cannot be nested in CSS 2.1.
		*)
		lRulesets = Count[relevantTokens, "{"];
		rulesets = ConstantArray[0, lRulesets];
		While[pos < l && i <= lRulesets,
			(* pos indicates first non-whitespace token *)
			blockStartPos = findOpeningBracketPosition[pos, "{", relevantTokens];
			Which[
				(* no selector *)
				pos == blockStartPos,
					blockStopPos  = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
					skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos,
			
				(* @page *)
				StringStartsQ[tokenString @ relevantTokens[[pos]], RegularExpression[RE["w"] ~~ T["PAGE_SYM"]]],
					blockStopPos  = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
					rulesets[[i]] = 
						<|
							"Selector" -> StringTrim @ StringJoin[tokenString /@ relevantTokens[[pos ;; blockStartPos-1]]], 
							"Condition" -> StyleData[All, "Printout"],
							"Block" -> relevantTokens[[blockStartPos+1 ;; If[tokenType @ relevantTokens[[blockStopPos]] == "}", blockStopPos-1, blockStopPos]]]|>;
					skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos;
					i++,
					
				(* @media *)
				StringStartsQ[tokenString @ relevantTokens[[pos]], RegularExpression[RE["w"] ~~ T["MEDIA_SYM"]]],
					atBlockStartPos = blockStartPos;
					atBlockStopPos = findClosingBracketPosition[atBlockStartPos, "{", "}", relevantTokens];
					pos = atBlockStartPos; skipWhitespace[pos, l, relevantTokens];
					While[pos < atBlockStopPos && i <= lRulesets,
						blockStartPos = findOpeningBracketPosition[pos, "{", relevantTokens];
						Which[
							(* no selector *)
							pos == blockStartPos,
							blockStopPos  = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
							skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos,
							
							(* skip unknown "at rules" including @import *)
							StringStartsQ[tokenString @ relevantTokens[[pos]], RegularExpression[RE["w"] ~~ "@"]],
							blockStopPos = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
							skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos,
					
							(* normal case *)
							True,
							(* 'blockStopPos' could reach the end of the token list, but not necessarily be a closed bracket '}' *)
							blockStopPos  = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
							rulesets[[i]] = 
								<|
									"Selector" -> StringTrim @ StringJoin[tokenString /@ relevantTokens[[pos ;; blockStartPos-1]]], 
									"Condition" -> None,
									"Block" -> relevantTokens[[blockStartPos+1 ;; If[tokenType @ relevantTokens[[blockStopPos]] == "}", blockStopPos-1, blockStopPos]]]|>;
							skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos;
						];
						i++
					];
					skipWhitespace[atBlockStopPos, l, relevantTokens]; pos = atBlockStopPos,
					
				(* skip unknown "at rules" including @import *)
				StringStartsQ[tokenString @ relevantTokens[[blockStartPos-1]], RegularExpression[RE["w"] ~~ "@"]],
					atBlockStartPos = blockStartPos;
					atBlockStopPos = findClosingBracketPosition[atBlockStartPos, "{", "}", relevantTokens];
					skipWhitespace[atBlockStopPos, l, relevantTokens]; pos = atBlockStopPos,
				
				(* general case of correct rule syntax *)
				True,
					(* 'blockStopPos' could reach the end of the token list, but not necessarily be a closed bracket '}' *)
					blockStopPos  = findClosingBracketPosition[blockStartPos, "{", "}", relevantTokens];
					rulesets[[i]] = 
						<|
							"Selector" -> StringTrim @ StringJoin[tokenString /@ relevantTokens[[pos ;; blockStartPos-1]]], 
							"Condition" -> None,
							"Block" -> relevantTokens[[blockStartPos+1 ;; If[tokenType @ relevantTokens[[blockStopPos]] == "}", blockStopPos-1, blockStopPos]]]|>;
					skipWhitespace[blockStopPos, l, relevantTokens]; pos = blockStopPos;
					i++
			];
		];
		(* remove possible excess blocks *)
		Join[imports, DeleteCases[rulesets, 0, {1}]]
	]


(* ::Subsection::Closed:: *)
(*Process declarations (within all blocks)*)


processDeclarations[rulesets:{_Association..}] :=
	Module[{aCopy, temp},
		aCopy = rulesets;
		temp = aCopy[[All, "Block"]];
		temp = temp /. {"other", x_String} :> Sequence @@ tokenizeDeclaration[x];
		temp = processDeclarationBlock /@ temp;
		aCopy[[All, "Block"]] = temp;
		aCopy
	]		
(* fallthrough case where no rules are found from processRulesetes *)
processDeclarations[{}] := {} 


processDeclarationBlock[tokens:{__?validTokenQ}] :=
	Module[{pos, l, lDeclarations, i, propertyPosition, valueStartPosition, valueStopPosition, declarations, important},
		pos = 1;
		l = Length[tokens];
		i = 1;
		If[tokenType @ tokens[[pos]] == " ", skipWhitespace[pos, l, tokens]]; (* skip any initial whitespace *)
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[tokens, ":"];
		declarations = ConstantArray[0, lDeclarations];
		While[pos < l && i <= lDeclarations,
			If[tokenType @ tokens[[pos]] == "ident",
				propertyPosition = pos; skipWhitespace[pos, l, tokens];
				If[tokenType @ tokens[[pos]] == ":",
					skipWhitespace[pos, l, tokens];
					valueStartPosition = pos;
					(* check for 'important' token, which would be the last token before ';' *)
					While[!MatchQ[tokenType @ tokens[[pos]], ";" | "important"] && pos < l, pos++];
					Switch[tokenType @ tokens[[pos]],
						"important", 
							important = True; 
							valueStopPosition = pos-1;
							skipWhitespace[pos, l, tokens];
							Which[
								(* do nothing extra as 'important' must be on the last declaration that also is missing a semi-colon *)
								pos > l, Null, 
								(* skip over the semi-colon *)
								tokenType @ tokens[[pos]] == ";", pos++,  
								(* syntax error; reset values and let a further parser flag the error *)
								True, valueStopPosition = pos; important = False; While[tokenType @ tokens[[pos]] != ";" && pos < l, pos++]
							],
						";", 
							important = False;
							valueStopPosition = pos-1,
						_, (* case of no 'important' ident and no semi-colon after last declaration in block *)
							important = False;
							valueStopPosition = pos;
					];
					While[tokenType @ tokens[[valueStopPosition]] == " ", valueStopPosition--]; (* trim whitespace from the end of the value *)
					declarations[[i]] = <|
						"Important" -> important,
						"Property" -> normalizeKeyWord @ tokenString @ tokens[[propertyPosition]], 
						"Value" -> (*check for empty property*)If[valueStopPosition < valueStartPosition, {}, tokens[[valueStartPosition ;; valueStopPosition]]],
						"Interpretation" -> None|>;
					skipWhitespace[pos, l, tokens];
					,
					(* ELSE failed to find colon in declaration, so skip to next declaration by looking for nearest declaration end *)
					While[tokenType @ tokens[[pos]] != ";" && pos < l, pos++];
				];
				,
				(* ELSE failed to find initial identifier in declaration, so skip to next declaration by looking for nearest declaration end *)
				While[tokenType @ tokens[[pos]] != ";" && pos < l, pos++];
			];
			i++ (* increment number of successfully parsed declarations *)
		];					
		(* remove possible excess declarations *)
		DeleteCases[declarations, 0, {1}]
	]
	
processDeclarationBlock[{}] := {}


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


(* ::Subsection::Closed:: *)
(*Merge Properties*)


expectedMainKeys = {"Selector", "Condition", "Block"};
expectedMainKeysFull = {"Selector", "Specificity", "Targets", "Condition", "Block"};
expectedBlockKeys = {"Important", "Property", "Value", "Interpretation"};

validCSSDataBareQ[data:{__Association}] := 
	And[
		AllTrue[Keys /@ data, MatchQ[expectedMainKeys]],
		AllTrue[Keys /@ Flatten @ data[[All, "Block"]], MatchQ[expectedBlockKeys]]]
validCSSDataFullQ[data:{__Association}] := 
	And[
		AllTrue[Keys /@ data, MatchQ[expectedMainKeysFull]],
		AllTrue[Keys /@ Flatten @ data[[All, "Block"]], MatchQ[expectedBlockKeys]]]
validCSSDataQ[data:{__Association}] := validCSSDataBareQ[data] || validCSSDataFullQ[data]
validCSSDataQ[___] := False

(* these include all inheritable options that make sense to pass on in a Notebook environment *)
notebookLevelOptions = 
	{
		Background, BackgroundAppearance, BackgroundAppearanceOptions, 
		FontColor, FontFamily, FontSize, FontSlant, FontTracking, FontVariations, FontWeight, 
		LineIndent, LineSpacing, ParagraphIndent, ShowContents, TextAlignment};
		
(* these include all options (some not inheritable in the CSS sense) that make sense to set at the Cell level *)
cellLevelOptions = 
	{
		Background, 
		CellBaseline, CellDingbat, CellMargins, 
		CellFrame, CellFrameColor, CellFrameLabelMargins, CellFrameLabels, CellFrameMargins, CellFrameStyle, 
		CellLabel, CellLabelMargins, CellLabelPositioning, CellLabelStyle, 
		CounterIncrements, CounterAssignments,
		FontColor, FontFamily, FontSize, FontSlant, FontTracking, FontVariations, FontWeight, 
		LineIndent, LineSpacing, ParagraphIndent, ShowContents, TextAlignment,
		PageBreakBelow, PageBreakAbove, PageBreakWithin, GroupPageBreakWithin};
		
(* these are options that are expected to be Notebook or Cell specific *)
optionsToAvoidAtBoxLevel = 
	{
		BackgroundAppearance, BackgroundAppearanceOptions, 
		CellBaseline, CellDingbat, CellMargins, 
		CellFrame, CellFrameColor, CellFrameLabelMargins, CellFrameLabels, CellFrameMargins, CellFrameStyle, 
		CellLabel, CellLabelMargins, CellLabelPositioning, CellLabelStyle, 
		ParagraphIndent};
		
validBoxes =
	{
		ActionMenuBox, AnimatorBox, ButtonBox, CheckboxBox, ColorSetterBox, 
		DynamicBox, DynamicWrapperBox, FrameBox, Graphics3DBox, GraphicsBox, 
		GridBox, InputFieldBox, InsetBox, ItemBox, LocatorBox, 
		LocatorPaneBox, OpenerBox, OverlayBox, PaneBox, PanelBox, 
		PaneSelectorBox, PopupMenuBox, ProgressIndicatorBox, RadioButtonBox,
		SetterBox, Slider2DBox, SliderBox, TabViewBox, TogglerBox, TooltipBox};	
validExpressions =
	{
		ActionMenu, Animator, Button, Checkbox, ColorSetter, 
		Dynamic, DynamicWrapper, Frame, Graphics3D, Graphics, 
		Grid, InputField, Inset, Item, Locator, 
		LocatorPane, Opener, Overlay, Pane, Panel, 
		PaneSelector, PopupMenu, ProgressIndicator, RadioButton,
		Setter, Slider2D, Slider, TabView, Toggler, Tooltip};	
validBoxOptions =
	{
		Alignment, Appearance, Background, Frame, FrameMargins, FrameStyle, 
		FontTracking, ImageMargins, ImageSize, ImageSizeAction, Spacings, Scrollbars};
validBoxesQ = MemberQ[Join[validBoxes, validExpressions], #]&;

removeBoxOptions[allOptions_, boxes:{__?validBoxesQ}] :=
	Module[{currentOpts, optNames = allOptions[[All, 1]]},
		Join[
			Cases[allOptions, Rule[Background, _] | Rule[FontTracking, _], {1}],
			DeleteCases[allOptions, Alternatives @@ (Rule[#, _]& /@ validBoxOptions)],
			DeleteCases[
				Table[
					currentOpts = Intersection[Options[i][[All, 1]], optNames];
					Symbol[SymbolName[i] <> "Options"] -> Cases[allOptions, Alternatives @@ (Rule[#, _]& /@ currentOpts), {1}],
					{i, boxes}],
				_ -> {}, 
				{1}]]	
	]
(* ResolveCSSInterpretations:
	1. Remove Missing and Failure interpretations.
	2. Filter the options based on Notebook/Cell/Box levels.
	3. Merge together Left/Right/Bottom/Top and Width/Height options.  *)
ResolveCSSInterpretations[type:(Cell|Notebook|Box|All), interpretationList_Dataset] :=
	ResolveCSSInterpretations[type, Normal @ interpretationList]
	
ResolveCSSInterpretations[type:(Cell|Notebook|Box|All), interpretationList_] := 
	Module[{valid, initialSet},
		valid = DeleteCases[Flatten @ interpretationList, _?FailureQ | _Missing, {1}];
		valid = Select[valid, 
			Switch[type, 
				Cell,      MemberQ[cellLevelOptions, #[[1]]]&, 
				Notebook,  MemberQ[notebookLevelOptions, #[[1]]]&,
				Box,      !MemberQ[optionsToAvoidAtBoxLevel, #[[1]]]&,
				All,       True&]];
		(* assemble options *)
		initialSet = assemble[#, valid]& /@ Union[First /@ valid];
		If[type === Box || type === All,
			removeBoxOptions[initialSet, validBoxes]
			,
			initialSet]
	]

ResolveCSSInterpretations[box:_?validBoxesQ, interpretationList_Dataset] := ResolveCSSInterpretations[{box}, Normal @ interpretationList]
ResolveCSSInterpretations[box:_?validBoxesQ, interpretationList_] := ResolveCSSInterpretations[{box}, interpretationList]	
ResolveCSSInterpretations[boxes:{__?validBoxesQ}, interpretationList_] := 
	Module[{valid, initialSet},
		valid = DeleteCases[Flatten @ interpretationList, _?FailureQ | _Missing, {1}];
		valid = Select[valid, !MemberQ[optionsToAvoidAtBoxLevel, #[[1]]]&];
		(* assemble options *)
		initialSet = assemble[#, valid]& /@ Union[First /@ valid];
		removeBoxOptions[initialSet, boxes /. Thread[validExpressions -> validBoxes]]				
	]


thicknessQ[_Thickness | _AbsoluteThickness] := True
thicknessQ[_] := False
dashingQ[_Dashing | _AbsoluteDashing] := True
dashingQ[_] := False
dynamicColorQ[x_Dynamic] := If[Position[x, _?ColorQ | FontColor, Infinity] === {}, False, True]
dynamicColorQ[_] := False
dynamicThicknessQ[x_Dynamic] := If[Position[x, Thickness | AbsoluteThickness | FontSize, Infinity] === {}, False, True]
dynamicThicknessQ[_] := False

(* 
	Merging directives like the following is pretty naive. 
	We should probably revisit this, but it works for now because the CSS interpretations are in a simple form. *)

mergeDirectives[dNew_Directive, dOld_Directive] := 
	Module[{c1, t1, d1, c2, t2, d2, dirNew, dirOld},
		dirNew = If[MatchQ[dNew, Directive[_List]], Directive @@ dNew[[1]], dNew];
		dirOld = If[MatchQ[dOld, Directive[_List]], Directive @@ dOld[[1]], dOld];
		c1 = Last[Cases[dirOld, _?ColorQ | _?dynamicColorQ], {}];
		t1 = Last[Cases[dirOld, _?thicknessQ | _?dynamicThicknessQ], {}];
		d1 = Last[Cases[dirOld, _?dashingQ], {}];
		c2 = Last[Cases[dirNew, _?ColorQ | _?dynamicColorQ], {}];
		t2 = Last[Cases[dirNew, _?thicknessQ | _?dynamicThicknessQ], {}];
		d2 = Last[Cases[dirNew, _?dashingQ], {}];
		Directive[
			Last[Flatten[{c1, c2}], Unevaluated[Sequence[]]],
			Last[Flatten[{t1, t2}], Unevaluated[Sequence[]]],
			Last[Flatten[{d1, d2}], Unevaluated[Sequence[]]]]
	]

mergeDirectives[c2_?(ColorQ[#]||dynamicColorQ[#]&), dOld_Directive] := 
	Module[{t1, d1,dirOld},
		dirOld = If[MatchQ[dOld, Directive[_List]], Directive @@ dOld[[1]], dOld];
		t1 = Last[Cases[dirOld, _?thicknessQ | _?dynamicThicknessQ], Unevaluated[Sequence[]]];
		d1 = Last[Cases[dirOld, _?dashingQ], Unevaluated[Sequence[]]];
		Directive[c2, t1, d1]
	]

mergeDirectives[t2_?(thicknessQ[#]||dynamicThicknessQ[#]&), dOld_Directive] := 
	Module[{c1, d1, dirOld},
		dirOld = If[MatchQ[dOld, Directive[_List]], Directive @@ dOld[[1]], dOld];
		c1 = Last[Cases[dirOld, _?ColorQ | _?dynamicColorQ], Unevaluated[Sequence[]]];
		d1 = Last[Cases[dirOld, _?dashingQ], Unevaluated[Sequence[]]];
		Directive[c1, t2, d1]
	]
	
mergeDirectives[d2_?dashingQ, dOld_Directive] := 
	Module[{c1, t1, dirOld},
		dirOld = If[MatchQ[dOld, Directive[_List]], Directive @@ dOld[[1]], dOld];
		c1 = Last[Cases[dirOld, _?ColorQ | _?dynamicColorQ], Unevaluated[Sequence[]]];
		t1 = Last[Cases[dirOld, _?thicknessQ | _?dynamicThicknessQ], Unevaluated[Sequence[]]];
		Directive[c1, t1, d2]
	]

mergeDirectives[c_?(ColorQ[#]||dynamicColorQ[#]&), _?(ColorQ[#]||dynamicColorQ[#]&)] := c
mergeDirectives[t_?(thicknessQ[#]||dynamicThicknessQ[#]&), _?(thicknessQ[#]||dynamicThicknessQ[#]&)] := t
mergeDirectives[d_?dashingQ, _?dashingQ] := d
mergeDirectives[newDir_, Automatic] := newDir
mergeDirectives[newDir_, oldDir_] := Directive[oldDir, newDir]


assembleLRBTDirectives[x_List] := 
	Module[{r = {{Automatic, Automatic}, {Automatic, Automatic}}},
		Map[
			With[{v = First[#]}, 
				Switch[Head[#], 
					Bottom, r[[2, 1]] = mergeDirectives[v, r[[2, 1]]],
					Top,    r[[2, 2]] = mergeDirectives[v, r[[2, 2]]],
					Left,   r[[1, 1]] = mergeDirectives[v, r[[1, 1]]],
					Right,  r[[1, 2]] = mergeDirectives[v, r[[1, 2]]]]
			]&,
			Flatten[x]];
		r]
		
assembleLRBT[x_List] := 
	Module[{r = {{Automatic, Automatic}, {Automatic, Automatic}}},
		Map[
			With[{v = First[#]}, 
				Switch[Head[#], 
					Bottom | HeightMin, r[[2, 1]] = v,
					Top |    HeightMax, r[[2, 2]] = v,
					Left |   WidthMin,  r[[1, 1]] = v,
					Right |  WidthMax,  r[[1, 2]] = v]
			]&,
			Flatten[x]];
		r]
		
moveDynamicToHead[{{l_, r_}, {b_, t_}}] := 
	If[AnyTrue[{l, r, b, t}, MatchQ[#, _Dynamic]&], 
		Replace[Dynamic[{{l, r}, {b, t}}], HoldPattern[Dynamic[x__]] :> x, {3}]
		, 
		{{l, r}, {b, t}}
	]

Clear[assemble]
assemble[opt:FrameStyle|CellFrameStyle, rules_List] := opt -> assembleLRBTDirectives @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]
assemble[opt:FrameMargins|ImageMargins, rules_List] := opt -> assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]
assemble[opt:ImageSize, rules_List] := opt -> Replace[assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], {x_, x_} :> x, {1}] 
assemble[opt:CellFrame, rules_List] := opt -> Replace[assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], Automatic -> True, {2}]
assemble[opt:CellMargins|CellFrameMargins, rules_List] := opt -> moveDynamicToHead @ assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]
assemble[opt:CellFrameColor, rules_List] := opt -> Last @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]
assemble[opt_, rules_List] := Last @ Cases[rules, HoldPattern[opt -> _], {1}]
assemble[opt:FontVariations, rules_List] := opt -> DeleteDuplicates[Flatten @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], First[#1] === First[#2]&]


(* ::Section::Closed:: *)
(*Main Functions*)


(* ::Subsection::Closed:: *)
(*ResolveCSSCascade*)


(* ResolveCSSCascade:
	1. Select the entries in the CSS data based on the provided selectors
	2. order the selectors based on specificity and importance (if those options are on)
	3. merge resulting list of interpreted options *)
Options[ResolveCSSCascade] = {"IgnoreSpecificity" -> False, "IgnoreImportance" -> False};

ResolveCSSCascade[box:_?validBoxesQ, CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] := 
	ResolveCSSCascade[{box}, CSSData, selectorList, opts]	

ResolveCSSCascade[boxes:{__?validBoxesQ}, CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] :=
	ResolveCSSCascade[boxes, CSSData, selectorList, opts]
	
ResolveCSSCascade[type:(Cell|Notebook|Box|All), CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] :=
	ResolveCSSCascade[type, Normal @ CSSData, selectorList, opts]

ResolveCSSCascade[type:(Cell|Notebook|Box|All), CSSData:{__Association} /; validCSSDataQ[CSSData], selectorList:{__String}, opts:OptionsPattern[]] :=
	Module[{interpretationList, specificities},
		(* start by filtering the data by the given list of selectors; ordering is maintained *)
		interpretationList = Select[CSSData, MatchQ[#Selector, Alternatives @@ selectorList]&];
		
		If[TrueQ @ OptionValue["IgnoreSpecificity"],
			(* if ignoring specificity, then leave the user-supplied selector list alone *)
			interpretationList = Flatten @ interpretationList[[All, "Block"]]
			,
			(* otherwise sort based on specificity but maintain order of duplicates; this is what should happen based on the CSS specification *)
			specificities = Selector["", #][["Specificity"]]& /@ interpretationList[[All, "Selector"]];
			interpretationList = Flatten @ interpretationList[[Ordering[specificities]]][[All, "Block"]];
		];
		
		(* Following CSS cascade spec:
			Move !important CSS properties to the end since they should override all other properties, but maintain their ordering.
		*)
		interpretationList = 
			Flatten @ 
				If[TrueQ @ OptionValue["IgnoreImportance"],
					interpretationList[[All, "Interpretation"]]
					,
					Join[Select[interpretationList, #Important == False&], Select[interpretationList, #Important == True&]][[All, "Interpretation"]]
				];
				
		(* now that the styles are all sorted, merge them *)
		ResolveCSSInterpretations[type, interpretationList]
	]

ResolveCSSCascade[___] := Failure["BadCSSData", <||>]


(* ::Subsection::Closed:: *)
(*Read styles from XMLObject*)


linkElementPattern :=
	XMLElement[
		x_String | {_, x_String} /; StringMatchQ[x, "link", IgnoreCase -> True], 
		Alternatives[
			{
				___, 
				(attr1_String | {_, attr1_String} /; StringMatchQ[attr1, "rel", IgnoreCase -> True]) -> 
					(attrVal_String /; StringMatchQ[attrVal, "stylesheet", IgnoreCase -> True]), 
				___, 
				(attr2_String | {_, attr2_String} /; StringMatchQ[attr2, "href", IgnoreCase -> True]) -> loc_, 
				___},
			{
				___, 
				(attr2_String | {_, attr2_String} /; StringMatchQ[attr2, "href", IgnoreCase -> True]) -> loc_, 
				___, 
				(attr1_String | {_, attr1_String} /; StringMatchQ[attr1, "rel", IgnoreCase -> True]) -> 
					(attrVal_String /; StringMatchQ[attrVal, "stylesheet", IgnoreCase -> True]), 
				___}],
		___
	] :> loc

styleElementPattern :=
	XMLElement[
		x_String | {_, x_String} /; StringMatchQ[x, "style", IgnoreCase -> True], 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "type", IgnoreCase -> True]) -> 
				(attrVal_String /; StringMatchQ[attrVal, "text/css", IgnoreCase -> True]), 
			___}, 
		{css_String}
	] :> css
		
styleAttributePattern :=
	XMLElement[
		_, 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "style", IgnoreCase -> True]) -> css_, 
			___}, 
		___
	] :> css


ApplyCSSToXML[doc:XMLObject["Document"][___], CSSData_Dataset, wrapInDataset_:True] := ApplyCSSToXML[doc, Normal @ CSSData, wrapInDataset]
ApplyCSSToXML[doc:XMLObject["Document"][___], CSSData_?validCSSDataQ, wrapInDataset_:True] :=
	If[TrueQ @ wrapInDataset, Dataset, Identity][
		With[{t = Selector[doc, #Selector]}, 
			<|"Selector" -> #Selector, "Specificity" -> t[["Specificity"]], "Targets" -> t[["Elements"]], "Condition" -> #Condition, "Block" -> #Block|>
		]& /@ CSSData]
		
ApplyCSSToXML[_, CSSData_?validCSSDataQ, ___] := Failure["BadDoc", <|"Message" -> "Invalid XML document."|>]
ApplyCSSToXML[doc:XMLObject["Document"][___], ___] := Failure["BadData", <|"Message" -> "Invalid CSS data."|>]


Options[ExtractCSSFromXML] = {"RootDirectory" -> Automatic};

ExtractCSSFromXML[doc:XMLObject["Document"][___], opts:OptionsPattern[]] :=
	Module[
		{
			currentDir, externalSSPositions, externalSSContent, internalSSPositions, internalSSContent, 
			directStylePositions, directStyleContent, all, uniqueStyles},
			
		currentDir = Directory[];
		SetDirectory[If[OptionValue["RootDirectory"] === Automatic, Directory[], OptionValue["RootDirectory"]]]; 
		
		(* process externally linked style sheets via <link> elements *)
		externalSSPositions = Position[doc, First @ linkElementPattern];
		externalSSContent = ExternalCSS /@ Cases[doc, linkElementPattern, Infinity];
		(* filter out files that weren't found *)
		With[{bools =  # =!= $Failed& /@ externalSSContent},
			externalSSPositions = Pick[externalSSPositions, bools];
			externalSSContent = Pick[externalSSContent, bools];];
		externalSSContent = ApplyCSSToXML[doc, #, False]& /@ externalSSContent;
				
		(* process internal style sheets given by <style> elements *)
		internalSSPositions = Position[doc, First @ styleElementPattern];
		internalSSContent = InternalCSS /@ Cases[doc, styleElementPattern, Infinity];
		internalSSContent = ApplyCSSToXML[doc, #, False]& /@ internalSSContent;
		
		(* process internal styles given by 'style' attributes *)
		directStylePositions = Position[doc, First @ styleAttributePattern];
		directStyleContent = Cases[doc, styleAttributePattern, Infinity];
		directStyleContent = 
			MapThread[
				<|
					"Selector" -> None, 
					"Specificity" -> {1, 0, 0, 0}, 
					"Targets" -> {#1}, 
					"Condition" -> None, 
					"Block" -> processDeclarationBlock @ tokenizeDeclaration @ #2|>&, 
				{directStylePositions, directStyleContent}];
		
		(* combine all CSS sources based on position in XMLObject *)
		all =
			Flatten @ 
				Part[
					Join[externalSSContent, internalSSContent, directStyleContent],
					Ordering @ Join[externalSSPositions, internalSSPositions, directStylePositions]];
		uniqueStyles = Union @ Flatten @ all[[All, "Block", All, "Property"]];
		all = Fold[process[#2, #1]&, all, uniqueStyles];
		SetDirectory[currentDir];
		Dataset @ all		
	]


parents[x:{__Integer}] := Most @ Reverse @ NestWhileList[Drop[#, -2]&, x, Length[#] > 2&]

inheritedProperties = Pick[Keys@ #, Values @ #]& @ CSSPropertyData[[All, "Inherited"]];

ResolveCSSInheritance[position_Dataset, CSSData_] := ResolveCSSInheritance[Normal @ position, CSSData]
ResolveCSSInheritance[position_, CSSData_Dataset] := ResolveCSSInheritance[position, Normal @ CSSData]

(* ResolveCSSInheritance
	Based on the position in the XMLObject, 
	1. look up all ancestors' positions
	2. starting from the most ancient ancestor, calculate the styles of each ancestor, including inherited properties
	3. with all inheritance resolved, recalculate the style at the XMLObject position *)
ResolveCSSInheritance[position:{___?IntegerQ}, CSSData_?validCSSDataFullQ] :=
	Module[{lineage, data = CSSData, a, temp, temp2, i},
		(* order data by specificity *)
		data = data[[Ordering[data[[All, "Specificity"]]]]];
		
		(* *)
		lineage = Append[parents[position], position];
		a = <|Map[# -> <|"All" -> None, "Inherited" -> None|>&, lineage]|>;
		Do[
			(* get all CSS data entries that target the input position *)
			temp = Pick[data, MemberQ[#, i]& /@ data[[All, "Targets"]]];
			temp = Flatten @ temp[[All, "Block", All, {"Important", "Property", "Interpretation"}]];
			
			(* prepend all inherited properties from ancestors, removing possible duplicated inheritance *)
			temp2 = Join @@ Values @ a[[Key /@ parents[i], "Inherited"]];
			a[[Key[i], "All"]] = With[{values = Join[temp2, temp]}, Reverse @ DeleteDuplicates @ Reverse @ values];
			
			(* pass on any inheritable properties, but reset their importance so they don't overwrite later important props *)
			a[[Key[i], "Inherited"]] = Select[a[[Key[i], "All"]], MemberQ[inheritedProperties, #Property]&];
			With[{values = a[[Key[i], "Inherited", All, "Important"]]}, 
				a[[Key[i], "Inherited", All, "Important"]] = ConstantArray[False, Length[values]]
			];,
			{i, lineage}];
			
		(* return computed properties, putting important properties last *)
		Join[
			Select[a[[Key @ position, "All"]], #Important == False&][[All, "Interpretation"]], 
			Select[a[[Key @ position, "All"]], #Important == True& ][[All, "Interpretation"]]]
	]
	
ResolveCSSInheritance[position:{___?IntegerQ}, _] := Failure["BadData", <|"Message" -> "Invalid CSS data. CSS data must include specificity and target."|>]


(* ::Subsection::Closed:: *)
(*Import*)


(* slightly faster than using ImportString *)
importText[path_String, encoding_:"UTF8ISOLatin1"] := 
	Module[{str, strm, bytes},
		strm = OpenRead[path];
		If[FailureQ[strm], Return[$Failed]];
		str = Read[strm, Record, RecordSeparators -> {}];
		If[str === $Failed, Quiet @ Close[strm]; Return @ $Failed];
		If[str === EndOfFile, Quiet @ Close[strm]; Return @ {{}}];
		Close[strm];
		bytes = ToCharacterCode @ str;
		Quiet @ 
			If[encoding === "UTF8ISOLatin1", 
				Check[FromCharacterCode[bytes, "UTF8"], FromCharacterCode[bytes, "ISOLatin1"]]
				, 
				FromCharacterCode[bytes, encoding]
			]
	]
	
ExternalCSS[filepath_String] := 
	If[FailureQ[FindFile[filepath]],
		Message[Import::nffil, "CSS extraction"]; $Failed
		,
		With[{i = importText[filepath]}, If[FailureQ[i], $Failed, processDeclarations @ processRulesets @ i]]]
		
InternalCSS[data_String] := processDeclarations @ processRulesets @ data

RawCSS[filepath_String, opts___] := 
	Module[{raw, mainItems, blocksSansValues, untokenizedValues, editedBlocks},
		raw = ExternalCSS[filepath];
		If[!validCSSDataBareQ[raw], Return @ Failure["BadCSSFile", <||>]];
		mainItems = raw[[All, {"Selector", "Condition"}]];
		blocksSansValues = raw[[All, "Block", All, {"Important", "Property"}]];
		untokenizedValues = Map["Value" -> StringJoin[#]&, raw[[All, "Block", All, "Value", All, 2]], {2}];
		editedBlocks = "Block" -> #& /@ (MapThread[<|#1, #2|>&, #]& /@ Transpose[{blocksSansValues, untokenizedValues}]);
		MapThread[<|#1, #2|>&, {mainItems, editedBlocks}]
	]

InterpretedCSS[filepath_String, opts___] := 
	Module[{raw, uniqueStyles},
		raw = ExternalCSS[filepath];
		If[!validCSSDataBareQ[raw], Return @ Failure["BadCSSFile", <||>]];
		
		uniqueStyles = Union @ Flatten @ raw[[All, "Block", All, "Property"]];
		
		Fold[process[#2, #1]&, raw, uniqueStyles]
	]

ProcessToStylesheet[filepath_String, opts___] :=
	Module[{raw, uniqueSelectors, allProcessed, uniqueStyles},
		raw = ExternalCSS[filepath];
		If[!validCSSDataBareQ[raw], Return @ Failure["BadCSSFile", <||>]];
		
		(* get all selectors preserving order, but favor the last entry of any duplicates *)
		uniqueSelectors = Reverse @ DeleteDuplicates[Reverse @ raw[[All, "Selector"]]];
		uniqueStyles = Union @ Flatten @ raw[[All, "Block", All, "Property"]];
		
		allProcessed = Fold[process[#2, #1]&, raw, uniqueStyles];
		allProcessed = ResolveCSSCascade[All, allProcessed, {#}]& /@ uniqueSelectors;
		(*TODO: convert options like FrameMargins to actual styles ala FrameBoxOptions -> {FrameMargins -> _}*)
		"Stylesheet" -> 
			NotebookPut @ 
				Notebook[
					MapThread[Cell[StyleData[#1], Sequence @@ #2]&, {uniqueSelectors, allProcessed}], 
					StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
	]

ImportExport`RegisterImport[
	"CSS",
	{
		"Elements" :> (("Elements" -> {"RawData", "Interpreted", "Stylesheet"})&),
		"Interpreted" :> (("Interpreted" -> Dataset @ CSS21Parser`Private`InterpretedCSS[#])&), (* same as default *)
		"RawData" :> (("RawData" -> Dataset @ CSS21Parser`Private`RawCSS[#])&),
		"Stylesheet" :> CSS21Parser`Private`ProcessToStylesheet,
		((Dataset @ CSS21Parser`Private`InterpretedCSS[#])&)},
	{},
	"AvailableElements" -> {"Elements", "RawData", "Interpreted", "Stylesheet"}]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
