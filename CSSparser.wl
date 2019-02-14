(* ::Package:: *)

(* ::Title:: *)
(*CSS 2.1 Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20190210*)
(*Version: 1*)


(* ::Section:: *)
(*Package Header*)


(*BeginPackage["CSSImport`", {"GeneralUtilities`"}];

(*ImportExport`RegisterImport["format",defaultFunction]\*)

Begin["`Private`"];*)


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


(* ::Subsection::Closed:: *)
(*CSS vs Wolfram Desktop (WD) Stylesheets*)


(* ::Subsubsection::Closed:: *)
(*Definitions*)


(* ::Text:: *)
(*CSS								WD*)
(*declaration (property:value)			option (name -> value)*)
(*declaration block ({p1:v1; p2:v2; ...})	Cell[StyleData["StyleName"], n1->v1, n2->v2, ...]*)
(*selector							"StyleName"*)


(* ::Subsubsection::Closed:: *)
(*Combining Styles*)


(* ::Text:: *)
(*CSS: styles are called declarations (property:value) and a group of them is a declaration block.*)
(*WD: styles are given as options (name -> value) and can be grouped into StyleData cells in a WD stylesheet.*)


(* ::Subsubsection::Closed:: *)
(*Targeting content with styles*)


(* ::Text:: *)
(*CSS: selectors specify which elements are targeted with the corresponding declarations. *)
(*WD: named styles can be applied to any box/cell/notebook level.*)


(* ::Subsubsection::Closed:: *)
(*CSS Inheritance*)


(* ::Text:: *)
(* Multiple selectors can target the same element. In that case the declarations merge via the following cascading order: *)
(*(https://www.w3.org/TR/2011/REC-CSS2-20110607/cascade.html#cascading-order)*)
(* 	ORIGIN							EXAMPLE*)
(* 	1. user agent declarations 			a web browser's default styles*)
(* 	2. user normal declarations 			a web browser's user preferences*)
(* 	3. author normal declarations		an HTML document's embedded styles*)
(* 	4. author important declarations 		an HTML document's embedded important styles*)
(* 	5. user important declarations		a web browser's user preference overrides*)
(*Sort by origin (1-5) and specificity of selector (https://www.w3.org/TR/2011/REC-CSS2-20110607/cascade.html#specificity) with higher specificity overriding lower. If two declarations have the same weight (important/normal), origin and specificity, the latter specified wins.*)


(* ::Subsubsection::Closed:: *)
(*WD Inheritance*)


(* ::Text:: *)
(*Named styles can appear in multiple stylesheets or used directly in a notebook, cell, or box (via the Style wrapper). In that case the options merge via the following cascading order:*)
(*	ORIGIN						EXAMPLE*)
(*	1. $FrontEnd					Options[$FrontEnd]*)
(*	2. Core.nb					$InstallationDirectory\SystemFiles\FrontEnd\StyleSheets*)
(*	3. Default.nb					$InstallationDirectory\SystemFiles\FrontEnd\StyleSheets*)
(*	4. Private stylesheet (if present)	*)
(*	5. stylesheet "Notebook" local style*)
(*	6. stylesheet Style environment 	"Working" or "Printout"*)
(*	7. Notebook-level option 		an embedded option like WindowSize *)
(*	8. Cell-level options 			an embedded option like CellFrame*)
(*	9. Box-level options			an embedded option like ImageSize*)
(*Moreover, any named styles within stylesheets found at *)
(*	$InstallationDirectory\SystemFiles\FrontEnd\StyleSheets *)
(*can be overridden by named styles within stylesheet located at*)
(*	$BaseDirector\SystemFiles\FrontEnd\StyleSheets*)
(*or*)
(*	$UserBaseDirectory\SystemFiles\FrontEnd\StyleSheets*)
(*but only if the stylesheets have the same filename. These act like intermediary cascade steps between (1) and (2), and (3) and (4) where $BaseDirectory is checked before $UserBaseDirectory.*)


(* ::Subsubsection::Closed:: *)
(*CSS vs WD inheritance origin *)


(* ::Text:: *)
(*This comparison is hand-wavy:*)
(*WD							CSS*)
(*(1-3)	$FrontEnd/Core/Default		(1) user agent declarations*)
(*($Base and $UserBase)			(2) user declarations*)
(*(4) Private Stylesheet			(3) author declarations*)
(*(7-9) Notebook/Cell/Box		(4-5) important styles; perhaps just more author declarations*)
(**)


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
RE["w"]           = "(" ~~ RE["s"] ~~ "?)";


RE["A"] = "(a|A|\\\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?)";
RE["C"] = "(c|C|\\\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?)";
RE["D"] = "(d|D|\\\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?)";
RE["E"] = "(e|E|\\\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?)";
RE["G"] = "(g|G|\\\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\\\g|\\\\G)";
RE["H"] = "(h|H|\\\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\\\h|\\\\H)";
RE["I"] = "(i|I|\\\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\\\i|\\\\I)";
RE["K"] = "(k|K|\\\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\\\k|\\\\K)";
RE["L"] = "(l|L|\\\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?|\\\\l|\\\\L)";
RE["M"] = "(m|M|\\\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\\\m|\\\\M)";
RE["N"] = "(n|N|\\\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\\\n|\\\\N)";
RE["O"] = "(o|O|\\\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\\\o|\\\\O)";
RE["P"] = "(p|P|\\\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\\\p|\\\\P)";
RE["R"] = "(r|R|\\\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\\\r|\\\\R)";
RE["S"] = "(s|S|\\\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\\\s|\\\\S)";
RE["T"] = "(t|T|\\\\0{0,4}(44|74)(\r\n|[ \t\r\n\f])?|\\\\t|\\\\T)";
RE["U"] = "(u|U|\\\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\\\u|\\\\U)";
RE["X"] = "(x|X|\\\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\\\x|\\\\X)";
RE["Z"] = "(z|Z|\\\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\\\z|\\\\Z)";


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


(* ::Subsection::Closed:: *)
(*Identify @charset, @import, and rulesets (selector + block declarations)*)


trimFront[s_String] := StringReplace[s, StartOfString ~~ Whitespace -> ""]


(* DeleteCases removes any comment-like token. Perhaps we should allow an option flag to keep comments...? *)
parseBlock[x_String] := 
	DeleteCases[
		Map[With[{l = label["block", #]}, {l, If[l!="other", trimFront @ #, #]}]&, 
			StringSplit[x, 
				s:Alternatives[
					RegularExpression @ RE["comment"],
					RegularExpression @ RE["badcomment"],
					RegularExpression @ P["charset"],
					RegularExpression @ P["import"],
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
		StringMatchQ[x, RegularExpression @ P["charset"]],     "charset",
		StringMatchQ[x, RegularExpression @ P["import"]],      "import",
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
invalidFunctionFailure[function_string] := Failure["InvalidFunction", <|"Function" -> function|>];
negativeIntegerFailure[prop_String] :=     Failure["BadLength",       <|"Message" -> prop <> "integer must be non-negative."|>];
negativeLengthFailure[prop_String] :=      Failure["BadLength",       <|"Message" -> prop <> "length must be non-negative."|>];
notAnImageFailure[uri_String] :=           Failure["NoImageFailure",  <|"URI" -> uri|>];
noValueFailure[prop_String] :=             Failure["UnexpectedParse", <|"Message" -> "No " <> prop <> " property value."|>];
positiveLengthFailure[prop_String] :=      Failure["BadLength",       <|"Message" -> prop <> "length must be positive."|>];
repeatedPropValueFailure[prop_] :=         Failure["UnexpectedParse", <|"Message" -> "Repeated property value type.", "Prop" -> prop|>];
tooManyPropValuesFailure[props_List] :=    Failure["UnexpectedParse", <|"Message" -> "Too many property values provided.", "Props" -> props|>];
tooManyTokensFailure[tokens_List] :=       Failure["UnexpectedParse", <|"Message" -> "Too many tokens.", "Tokens" -> tokens[[All, 1]]|>];
unrecognizedKeyWordFailure[prop_String] := Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " keyword."|>];
unrecognizedValueFailure[prop_String] :=   Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " value."|>];
unsupportedValueFailure[prop_String] :=    Failure["UnsupportedProp", <|"Property" -> prop|>]


(* ::Subsection::Closed:: *)
(*Property Data Table*)


(* 
	Some of these are shorthand properties that set one or more other properties. 
	As such, the shorthand initial values would never be directly required.
*)
propertyData = <|
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
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 border sides *)
		"WDInitialValue" -> Automatic|>, 
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
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 sides *)
		"WDInitialValue" -> Automatic|>, 
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
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 border sides *)
		"WDInitialValue" -> Automatic|>, 
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
		"CSSInitialValue" -> "N/A", (* shorthand property, sets all 4 border sides *)
		"WDInitialValue" -> Automatic|>, 
	"border-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand border-top sets color/style/width *)
		"WDInitialValue" -> Automatic|>, 
	"border-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", (* shorthand border-top sets color/style/width *)
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
	
initialValues[prop_String] := propertyData[prop, "WDInitialValue"]


(* ::Subsection::Closed:: *)
(*<color>*)


parseSingleColor[prop_String, tokens:{{_String, _String}..}] := parseSingleColor[prop, tokens] = 
	Which[
		Length[tokens] == 1 && MatchQ[tokens[[1, 1]], "ident"],
			Switch[ToLowerCase @ tokens[[1, 2]],
				"initial",      initialValues @ prop,
				"inherit",      Inherited,
				"currentcolor", Dynamic @ CurrentValue[FontColor], 
				"transparent",  None, (* this 'ident' is interpreted as GrayLevel[0, 0] by Interpreter["Color"] *)
				_,              Interpreter["Color"][StringJoin @ tokens[[1, 2]]] (* keyword e.g. blue *)
			],
		Length[tokens] == 1 && MatchQ[tokens[[1, 1]], "hexcolor"],
			Interpreter["Color"][tokens[[1, 2]]],
		Length[tokens] > 1 && MatchQ[tokens[[1, 1]], "function"],
			Interpreter["Color"][StringJoin @ tokens[[All, 2]]], (* rgb(_,_,_) or hsl(_,_,_) *)
		True, unrecognizedValueFailure @ prop
	]


(* ::Subsection::Closed:: *)
(*<counter>*)


parseCounter[prop_String, tokens:{{_String, _String}...}] := parseCounter[prop, tokens] =
	Module[{pos = 1, l = Length[tokens], style, stringAddOn = "", listtype = "decimal"},
		Switch[ToLowerCase @ tokens[[pos, 2]],
			"counter(",
				skipWhitespace[pos, l, tokens];
				If[pos <= l && tokens[[pos, 1]] == "ident", style = tokens[[pos, 2]], Return @ invalidFunctionFailure @ StringJoin @ tokens[[All, 2]]];
				skipWhitespace[pos, l, tokens];
				If[pos > l, Return @ invalidFunctionFailure @ StringJoin @ tokens[[All, 2]]];
				Switch[tokens[[pos, 1]],
					"ident", listtype = tokens[[pos, 2]], (* updates listtype *)
					")",     Null,
					_,       tooManyTokensFailure @ tokens
				];
				parseSingleListStyleType["list-style-type", {"ident", listtype}, style],
			"counters(", Return @ Missing["Not supported."],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*<length>*)


parseLength[s_String, inFontSize_:False] := parseLength[s, inFontSize] =
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


parseEmNonRelative[s_String] := parseEmNonRelative[s] = Interpreter["Number"] @ StringDrop[s, -2]


negativeQ[n_, prop_String, default_] :=
	Which[
		FailureQ[n],         n, 
		TrueQ @ Negative[n], negativeLengthFailure @ prop, 
		True,                default
	]


(* ::Subsection::Closed:: *)
(*<percentage>*)


parsePercentage[s_String] := parsePercentage[s] = Interpreter["Number"] @ StringDrop[s, -1]


(* ::Subsection::Closed:: *)
(*<uri>*)


parseURI[uri_String] := 
	Module[{p, mimetype = "text/plain", base64 = False, s = StringTake[uri, {5, -2}], start, rest},
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


(*TODO: parse multiple background specs but only take the first *)
parse[prop:"background", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)parseSingleBG[prop, tokens]

(* 
	Shorthand for all background properties.
	Any properties not set by this are reset to their initial values.
	Commas separate background layers, so split on comma, but FE only supports one image so take last...?
*)
parseSingleBG[prop_String, tokens:{{_String, _String}..}] := 
	Module[
		{
			pos = 1, l = Length[tokens], value, start, func,
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
				tokens[[pos, 1]] == "function", (* only color can be a function; eventually should support gradients *)
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"rgb(" | "rgba(" | "hsl(" | "hsla(", 
							If[hasColor, Return @ repeatedPropValueFailure @ "background-color"]; 
							hasColor = True; values["c"] = parseSingleColor[prop, consumeFunction[pos, l, tokens]];,
						_, (* linear- or radial-gradient *)
							If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
							hasImage = True; values["i"] = Missing["Not supported."]; consumeFunction[pos, l, tokens] (* advances pos but return value is not used *)
					],
					
				StringMatchQ[tokens[[pos, 2]], "inherit", IgnoreCase -> True], i1++,
				StringMatchQ[tokens[[pos, 2]], "initial", IgnoreCase -> True], i2++,
								
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
				Notebook[
					System`BackgroundAppearanceOptions ->
						Which[
							values["p"] === {0,0}    && values["r"] === "NoRepeat", "NoRepeat",
							values["p"] === "Center" && values["r"] === "NoRepeat", "Center",
							values["p"] === {0,0},                                  values["r"],
							True,                                                     Missing["Not supported."]
						],
					System`BackgroundAppearance -> values["i"],
					Background -> values["c"]]
		]
]


(* ::Subsubsection::Closed:: *)
(*background-attachment*)


parseSingleBGAttachment[prop_String, token:{_String, _String}] := parseSingleBGAttachment[prop, token] =
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"scroll",  Missing["Not supported."],
				"fixed",   Automatic, (* FE's only allowed value *)
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]

parse[prop:"background-attachment", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGAttachment[prop, tokens[[1]]];
		If[FailureQ[value] || MissingQ[value], value, Missing["Only fixed supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*background-color*)


(* 
	Effectively the same as color, except a successful parse returns as a rule Background \[Rule] value.
	Also 'currentColor' value needs to know the current value of 'color', instead of inherited from the parent.	
*) 
parse[prop:"background-color", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value},
		value = parseSingleColor[prop, tokens];
		If[FailureQ[value], value, Background -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-image*)


parseSingleBGImage[prop_String, token:{_String, _String}] := 
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"none",    None,
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"uri", parseURI @ token[[2]],
		_,     unrecognizedValueFailure @ prop
	]

parse[prop:"background-image", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		value = 
			Switch[tokens[[pos, 1]], 
				"function", (* possible linear- or radial-gradient *)
					consumeFunction[pos, l, tokens]; Missing["Not supported."], (* advances pos but return value is not used *)
				_, parseSingleBGImage[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, System`BackgroundAppearance -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-position*)


parseSingleBGPosition[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"left",    Left,
				"center",  Center,
				"right",   Right,
				"top",     Top,
				"bottom",  Bottom,
				"inherit", Inherited,
				"initial", initialValues @ prop,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"percentage",      With[{v = parsePercentage @ token[[2]]}, Scaled[v/100]],
		"length"|"number", parseLength @ token[[2]],
		_,                 unrecognizedValueFailure @ prop
	]

parseSingleBGPositionPair[values:{__}, tokens:{{_String, _String}..}] :=
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
parse[prop:"background-position", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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


parseSingleBGRepeat[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
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

parse[prop:"background-repeat", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
				Framed[#,ImageSize\[Rule]Full,Alignment\[Rule]Center]&, 
				{{"Client Name","Age"},{"",25},{"Louise Q.",""},{"Owen",""},{"Stan",71}}, 
				{2}],
			Frame\[Rule]None,
			ItemSize\[Rule]{{7,3}}]
	but this is beyond the scope of CSS importing.
*)


(* ::Subsubsection::Closed:: *)
(*border-collapse*)


parse[prop:"border-collapse", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
parse[prop:"border-top-color"|"border-right-color"|"border-bottom-color"|"border-left-color", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
parse[prop:"border-color", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleColor[prop, If[tokens[[pos, 1]] == "function", consumeFunction[pos, l, tokens], {tokens[[pos]]}]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			skipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, {FrameStyle -> {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]}, Cell[CellFrameColor -> First @ results]},
			2, {FrameStyle -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]}, Cell[CellFrameColor -> First @ results]},
			3, {FrameStyle -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}, Cell[CellFrameColor -> First @ results]},
			4, {FrameStyle -> {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}, Cell[CellFrameColor -> First @ results]},
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
parse[prop:"border-spacing", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
					"ems",               With[{n = parseEmNonRelative @ tokens[[pos, 2]]}, negativeQ[n, prop, n/2]],
					"exs",               With[{n = parseEmNonRelative @ tokens[[pos, 2]]}, negativeQ[n, prop, n/4]],
					"number" | "length", With[{n = parseLength @ tokens[[pos, 2]]},        negativeQ[n, prop, Dynamic[n/CurrentValue[FontSize]]]],
					_,                   unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			skipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1 | 2, Spacings -> results,
			_,     tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-style*)


parseSingleBorderStyle[prop_String, token:{_String, _String}] :=
	Switch[token[[1]],
		"ident",
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
			],
		_, unrecognizedValueFailure @ prop
	]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-style"|"border-right-style"|"border-bottom-style"|"border-left-style", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
	
parse[prop:"border-style", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSingleBorderStyle[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			skipWhitespace[pos, l, tokens]
		];
		Switch[Length[results],
			1, FrameStyle -> {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]},
			2, FrameStyle -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]},
			3, FrameStyle -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
			4, FrameStyle -> {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]},
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
		_,                   unrecognizedValueFailure @ prop
	]


convertToCellThickness[x_] := Switch[x, AbsoluteThickness[_], First[x], Thickness[_], First[x] /. {Small -> 1, Medium -> 2, Large -> 4}, _, x]


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	As a compromise set the other edges to Inherited.
*)
parse[prop:"border-top-width"|"border-right-width"|"border-bottom-width"|"border-left-width", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderWidth[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-width", Top, "border-right-width",  Right, "border-bottom-width", Bottom, "border-left-width", Left];
			{FrameStyle -> wrapper[value], Cell[CellFrame -> wrapper[convertToCellThickness @ value]]}
		]
	]	
	
(* sets all frame edge thickness at once *)
parse[prop:"border-width", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
		{FrameStyle -> results, Cell[CellFrame -> Map[convertToCellThickness, results, {2}]]}
	]


(* ::Subsubsection::Closed:: *)
(*border (-top, -bottom, -right, -left)*)


(* 
	Shorthand for border-*-width/style/color. 
	This effectively resets all edge properties because any property not specified takes on its default value. 
	'border' by itself sets all 4 edges to be the same.
*)
parse[prop:"border"|"border-top"|"border-right"|"border-bottom"|"border-left", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[
	{
		pos = 1, l = Length[tokens], p, value, dirAll, dir, 
		wrapper = Switch[prop, "border-left", Left, "border-right", Right, "border-top", Top, "border-bottom", Bottom, _, Through[{Left, Right, Top, Bottom}[#]]&],
		values = <|
			"c" -> initialValues[prop <> "-color"], 
			"s" -> initialValues[prop <> "-style"], 
			"w" -> initialValues[prop <> "-width"]|>,
		hasColor = False, hasStyle = False, hasWidth = False
	},
		
		(* if only one token is present, then check that it is a universal keyword *)
		If[l == 1,
			Switch[ToLowerCase @ tokens[[1, 2]],
				"inherit", Return @ {FrameStyle -> wrapper[Inherited], Cell[CellFrameColor -> Inherited, CellFrame -> wrapper[Inherited]]},
				"initial", Return @ {FrameStyle -> wrapper[Directive[Values @ values]], Cell[CellFrameColor -> values["c"], CellFrame -> wrapper[convertToCellThickness @ values["w"]]]}, 
				_, Null
			]
		];
		
		(* Ignore 'inherit' and 'initial' universal keywords. Other keywords are unique. *)
		While[pos <= l,
			Which[
				tokens[[pos, 1]] == "function", (* only color can be a function *)
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
		
		{FrameStyle -> wrapper[Directive[Values @ values]], Cell[CellFrameColor -> values["c"], CellFrame -> wrapper[convertToCellThickness @ values["w"]]]}
	]


(* ::Subsection::Closed:: *)
(*clip*)


parse[prop:"clip", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l == 1, (* if only one token is present, then it should be a keyword *)
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"initial",  Missing["Not supported."],
						"inherit",  Missing["Not supported."],
						"auto",     Missing["Not supported."],
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			]
			,
			If[tokens[[pos, 1]] == "function", 
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


(* 
	The color interpreter appears to mostly follow the CSS-color-3 module.
	It would be nice if it gave a more detailed failure message, but let's not reinvent the wheel.
*)
parse[prop:"color", tokens:{{_String, _String}..}] := parseSingleColor[prop, tokens]


(* ::Subsection::Closed:: *)
(*content, lists, and quotes*)


(* ::Subsubsection::Closed:: *)
(*content*)


(* only used to add content before or after element, so let's restrict this to Cells *)
parse[prop:"content", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, parsedValues = {}},
		While[pos <= l,
			value = 
				Switch[tokens[[pos, 1]],
					"ident", 
						Switch[ToLowerCase @ tokens[[pos, 2]],
							"normal",         Normal,
							"none",           None,
							"inherit",        Inherited,
							"initial",        initialValues @ prop,
							"open-quote",     Missing["Not supported."],
							"close-quote",    Missing["Not supported."],
							"no-open-quote",  Missing["Not supported."],
							"no-close-quote", Missing["Not supported."],
							_,                unrecognizedKeyWordFailure @ prop
						],
					"string", Cell[CellLabel -> tokens[[pos, 2]]], (* is this even doing this option justice? *)
					"uri",    With[{i = parseURI @ tokens[[pos, 2]]}, If[FailureQ[i] || MissingQ[i], notAnImageFailure @ tokens[[pos, 2]], Cell[CellDingbat -> i]]],
					"function", 
						Switch[ToLowerCase @ tokens[[pos, 2]],
							"counter(" | "counters(", Cell[CellDingbat -> parseCounter[prop, consumeFunction[pos, l, tokens]]],
							"attr(",                  parseAttr[prop, consumeFunction[pos, l, tokens]],
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
parse[prop:"counter-increment", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v, values = {}},
		While[pos <= l,
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = initialValues @ prop],
						_,         
							If[pos == l, 
								(* if the end is an ident then it simply adds itself once to the list of styles to increment *)
								values = Join[values, {tokens[[pos, 2]]}]; pos++
								,
								(* otherwise check for a non-negative integer and add that style name n times *)
								v = tokens[[pos, 2]]; skipWhitespace[pos, l, tokens];
								With[{i = Interpreter["Integer"][tokens[[pos, 2]]]}, 
									If[IntegerQ[i],
										If[i < 0, 
											Return @ negativeIntegerFailure @ prop
											,
											values = Join[values, ConstantArray[v, i]]; skipWhitespace[pos, l, tokens]
										]
										,
										values = Join[values, {v}];
									]
								];
							];
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
		];
		If[FailureQ[values], values, CounterIncrements -> values]
	]


(* ::Subsubsection::Closed:: *)
(*counter-reset*)


parse[prop:"counter-reset", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v = {}, values = {}},
		While[pos <= l,
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = initialValues @ prop],
						_,         
							If[pos == l, 
								(* if the end is an ident then it has 0 as its counter assignment *)
								AppendTo[values, {tokens[[pos, 2]], 0}]; pos++
								,
								(* otherwise check for an integer *)
								v = tokens[[pos, 2]]; skipWhitespace[pos, l, tokens];
								With[{i = Interpreter["Integer"][tokens[[pos, 2]]]}, 
									If[IntegerQ[i], (* if integer exists, use it and skip ahead, otherwise use 0 and don't increment pos *)
										AppendTo[values, {v, i}]; skipWhitespace[pos, l, tokens]
										,
										AppendTo[values, {v, 0}];
									]
								];
							];
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
		];
		If[FailureQ[values], values, CounterAssignments -> values]
	]


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
			With[{im = parseURI @ token[[2]]}, 
				Which[
					FailureQ[im], token[[2]], 
					MissingQ[im], token[[2]],
					!ImageQ[im],  notAnImageFailure @ token[[2]],
					_,            ToBoxes @ Dynamic @ Image[im, ImageSize -> CurrentValue[FontSize]]
				]
			],
		_, unrecognizedValueFailure @ prop
	]
	
parse[prop:"list-style-image", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
	
parse[prop:"list-style-position", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStylePosition[prop, First @ tokens];
		If[FailureQ[value], value, Cell[Missing["Not supported."]]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style-type*)


parseSingleListStyleType[prop_String, token:{_String, _String}, style_String:"Item"] := 
	Switch[token[[1]],
		"ident",
			Switch[ToLowerCase @ token[[2]],
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
	
parse[prop:"list-style-type", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleType[prop, First @ tokens];
		If[FailureQ[value], value, Cell[CellDingbat -> value]]
	]


(* ::Subsubsection::Closed:: *)
(*list-style*)


(* short-hand for list-style-image/position/type properties given in any order *)
parse[prop:"list-style", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, values, p, noneCount = 0, hasImage = False, hasPos = False, hasType = False},
		(* 
			li-image, li-position, and li-type can appear in any order.
			A value of 'none' sets whichever of li-type and li-image are not otherwise specified to 'none'. 
			If both are specified, then an additional 'none' is an error.
		*)
		values = <|"image" -> None, "pos" -> Missing["Not supported."], "type" -> None|>;
		While[pos <= l,
			If[TrueQ[ToLowerCase @ tokens[[pos, 2]] == "none"], 
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
			hasImage, Cell[CellDingbat -> values["image"]], (* default to Image if it could be found *)
			hasType,  Cell[CellDingbat -> values["type"]],
			True,     Cell[CellDingbat -> None]]
	]


(* ::Subsubsection::Closed:: *)
(*quotes*)


(*
	Quotes could be implemented using DisplayFunction \[Rule] (RowBox[{<open-quote>,#,<close-quote>}]&),
	but only a handful of boxes accept this WL option e.g. DynamicBoxOptions, ValueBoxOptions, and a few others.
	There's also ShowStringCharacters, but this only hides/shows the double quote.
	We treat this then as not available, but we validate the form anyway.
*)
parse[prop:"quotes", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], v, values = {}},
		While[pos <= l,
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"none",    If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = {}],
						"inherit", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = Inherited],
						"initial", If[l > 1, Return @ illegalIdentifierFailure @ tokens[[pos, 2]], values = initialValues @ prop],
					],
				"string",
					v = tokens[[pos, 2]]; skipWhitespace[pos, l, tokens];
					If[tokens[[pos, 1]] == "string", 
						AppendTo[values, {v, tokens[[pos, 2]]}]; skipWhitespace[pos, l, tokens]
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Expected pairs of strings."|>]
					],
				_, values = unrecognizedValueFailure @ prop; Break[]
			];
		];
		If[FailureQ[values], values, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*cursor*)


(* WL uses a TagBox[..., MouseAppearanceTag[""]] instead of an option to indicate mouse appearance *)
parse[prop:"cursor", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
				"uri", parseURI @ tokens[[pos, 2]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, With[{v = value}, MouseAppearance[#, v]&]]
	]		


(* ::Subsection::Closed:: *)
(*display*)


(*
	WL automatically lays out blocks either inline or as nested boxes. 
	If a Cell appears within TextData or BoxData then it is considered inline.
*)
parse[prop:"display", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"float", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"clear", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
		If[l == 1, (* if only one token is present, then it should be a keyword that represents a system font (or font style?) *)
			newValue = 
				Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"font-family", tokens:{{_String, _String}..}] :=
	Module[{fontTokens, parsed, result},
		fontTokens = DeleteCases[SplitBy[tokens, MatchQ[{"operator", ","}]], {{"operator", ","}}];
		parsed = parseSingleFontFamily /@ fontTokens;
		result = FirstCase[parsed, _Failure, None]; (* FIXME: perhaps use FontSubstitutions here? *)
		If[FailureQ[result], Return @ result];
		FirstCase[parsed, _Rule, Failure["UnexpectedParse", <|"Message" -> "No font-family found."|>]]
	]

parseSingleFontFamily[tokens:{{_String, _String}..}] := parseSingleFontFamily[tokens] =
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
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
					],
				"string", (* must only have a single string token up to the delimiting comma *)
					Which[
						l > 1, fail,
						True,
							font = StringTake[tokensNoWS[[All, 2]], {2, -2}];
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
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


parse[prop:"font-size", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
				"percentage",        With[{n = parsePercentage @ tokens[[pos, 2]]}, negativeQ[n, prop, Scaled[n/100]]],
				"length" | "number", With[{n = parseLength[#, True]& @ tokens[[pos, 2]]}, negativeQ[n, prop, n]],
				"ems" | "exs",       With[{n = parseLength[#, True]& @ tokens[[pos, 2]]}, negativeQ[Quiet @ First @ n, prop, n]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontSize -> value]
	]


(* ::Subsubsection::Closed:: *)
(*font-style*)


parse[prop:"font-style", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"font-variant", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"font-weight", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
							Clip[Interpreter["Number"][tokens[[pos, 2]]], {1, 1000}]], 
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
parseSingleSize[prop_String, token:{_String, _String}] := parseSingleSize[prop, token] =
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
parse[prop:"width"|"max-width"|"min-width", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"width",     {Wmin[value], Wmax[value]},
					"max-width", Wmax[value],
					"min-width", Wmin[value]
				]
		]
	]

parse[prop:"height"|"max-height"|"min-height", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"height",     {Hmin[value], Hmax[value]},
					"max-height", Hmax[value],
					"min-height", Hmin[value]
				]
		]
	]


(* ::Subsection::Closed:: *)
(*line-height*)


(* Similar to WL LineSpacing, but LineSpacing already takes FontSize into account, so intercept number before 'ems' returns CurrentValue[FontSize] *)
parse[prop:"line-height", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"inherit", Inherited,
						"initial", initialValues[prop], 
						"normal",  {1.2, 0},
						_,         unrecognizedKeyWordFailure @ prop
					],
				"number",      With[{n = Interpreter["Number"] @ tokens[[pos, 2]]}, negativeQ[n, prop, {n, 0}]],
				"length",      With[{n = parseLength @ tokens[[pos, 2]]},           negativeQ[n, prop, {n, 0}]],
				"ems",         With[{n = parseEmNonRelative @ tokens[[pos, 2]]},    negativeQ[n, prop, {n, 0}]],
				"exs",         With[{n = parseEmNonRelative @ tokens[[pos, 2]]},    negativeQ[n, prop, {n/2, 0}]],
				"percentage",  With[{n = parsePercentage @ tokens[[pos, 2]]},       negativeQ[n, prop, {n/100, 0}]],
				_,             unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, LineSpacing -> value]
	]


(* ::Subsection::Closed:: *)
(*margin(-left, -right, -top, -bottom)*)


parseSingleMargin[prop_String, token:{_String, _String}] := parseSingleMargin[prop, token] = 
	Switch[token[[1]],
		"ident", 
			Switch[ToLowerCase @ token[[2]],
				"initial", initialValues @ prop,
				"inherit", Inherited,
				"auto",    Automatic, (* let FE decide what to do *)
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
parse[prop:"margin-top"|"margin-right"|"margin-bottom"|"margin-left", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleMargin[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "margin-left", Left, "margin-right", Right, "margin-bottom", Bottom, "margin-top", Top];
			{ImageMargins -> wrapper[value], Cell[CellMargins -> wrapper[value]]}
		]
	]
		
parse[prop:"margin", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
		{ImageMargins -> results, Cell[CellMargins -> results]}
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


parse[prop:"outline-color", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, colorNegate, results = {}},
		If[l == 1 && tokens[[1, 1]] == "ident" && ToLowerCase @ tokens[[1, 2]] == "invert",
			Cell[CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], ColorNegate @ CurrentValue[CellFrameColor], Inherited]]]
			,
			While[pos <= l,
				value = parseSingleColor[prop, If[tokens[[pos, 1]] == "function", consumeFunction[pos, l, tokens], {tokens[[pos]]}]];
				If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
				skipWhitespace[pos, l, tokens];
			];
			Switch[Length[results],
				1, Missing["Not supported."](*With[{c = First @ results}, Cell[CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], c, Inherited]]]]*),
				_, tooManyTokensFailure @ tokens
			]
		]
	]


(* ::Subsubsection::Closed:: *)
(*outline-style*)


(* only a solid border is allowed for cells; 'hidden' is not allowed here *)
parse[prop:"outline-style", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value =
			If[tokens[[pos, 1]] == "ident" && tokens[[pos, 2]] == "hidden",
				unrecognizedKeyWordFailure @ prop
			,
				parseSingleBorderStyle[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-width*)


parse[prop:"outline-width", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = parseSingleBorderWidth[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			(*With[{t = Round @ convertToCellThickness @ value},
				Cell[
					CellFrame -> Dynamic[If[CurrentValue["MouseOver"], t, Inherited]],
					CellFrameMargins \[Rule] Dynamic[If[CurrentValue["MouseOver"], -t, Inherited]]
			]*)
			Missing["Not supported."]
		]
	]


(* ::Subsubsection::Closed:: *)
(*outline*)


(* Shorthand for outline-width/style/color. 'outline' always sets all 4 edges to be the same. *)
parse[prop:"outline", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[
	{
		pos = 1, l = Length[tokens], p, value, dirAll, dir, 
		values = <|
			"c" -> initialValues[prop <> "-color"], 
			"s" -> initialValues[prop <> "-style"], 
			"w" -> initialValues[prop <> "-width"]|>,
		hasColor = False, hasStyle = False, hasWidth = False
	},
		
		(* if only one token is present, then check that it is a universal keyword *)
		If[l == 1,
			Switch[ToLowerCase @ tokens[[pos, 2]],
				"inherit", Return @ Missing["Not supported."],
				"initial", Return @ Missing["Not supported."], 
				_, Null
			]
		];
		
		(* Ignore 'inherit' and 'initial' universal keywords. Other keywords are unique. *)
		While[pos <= l,
			Which[
				tokens[[pos, 1]] == "function", (* only color can be a function *)
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
parse[prop:"overflow", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos, 1]],
			"ident",
				Switch[ToLowerCase @ tokens[[pos, 2]],
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


parseSinglePadding[prop_String, token:{_String, _String}] := parseSinglePadding[prop, token] = 
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
		_,                   unrecognizedValueFailure @ prop
	]


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
parse[prop:"padding-top"|"padding-right"|"padding-bottom"|"padding-left", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSinglePadding[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "padding-left", Left, "padding-right", Right, "padding-bottom", Bottom, "padding-top", Top];
			{FrameMargins -> wrapper[value], Cell[CellFrameMargins -> wrapper[value]]}
		]
	]
		
parse[prop:"padding", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
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
		{FrameMargins -> results, Cell[CellFrameMargins -> results]}
	]


(* ::Subsection::Closed:: *)
(*page breaks*)


(* ::Subsubsection::Closed:: *)
(*orphans/widows*)


(* 
	FE uses LinebreakAdjustments with a blackbox algorithm. 
	AFAIK there's no way to directly prevent orphans/widows.
*)
parse[prop:"orphans"|"widows", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"number", With[{n = Interpreter["Integer"][tokens[[pos, 2]]]}, If[n < 0, negativeIntegerFailure @ prop, n]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-after/before*)


parse[prop:("page-break-after"|"page-break-before"), tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
			Cell[Switch[prop, "page-break-after", PageBreakBelow, "page-break-before", PageBreakAbove] -> value]
		]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-inside*)


parse[prop:"page-break-inside", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
			Cell[PageBreakWithin -> value, GroupPageBreakWithin -> value]
		]
	]


(* ::Subsection::Closed:: *)
(*position (and top, left, bottom, right)*)


(*
	'position' is most like WL's Alignment, but only relative to the parent box.
	WL does not support absolute positioning of cells and boxes.
	Attached cells can be floated or positioned absolutely, but are ephemeral and easily invalidated.
	Moreover, attached cells aren't an option, but rather a cell.
	Perhaps can use NotebookDynamicExpression \[Rule] Dynamic[..., TrackedSymbols \[Rule] {}] where "..." includes a list of attached cells.
*)
parse[prop:"position", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"left"|"right"|"top"|"bottom", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"auto",    Automatic,
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"percentage", 
					With[{n = parsePercentage @ tokens[[pos, 2]]}, 
						If[n > 100 || n < 0, Missing["Out of range."], Rescale[n, {0, 100}, Switch[prop, "left"|"bottom", {-1, 1}, "right"|"top", {1, -1}]]]
					],
				"length" | "ems" | "exs", 
					With[{n = parseLength @ tokens[[pos, 2]]}, 
						If[n == 0, Switch[prop, "left", Left, "right", Right, "top", Top, "bottom", Bottom], Missing["Not supported."]]
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
parse[prop:"caption-side", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"empty-cells", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"table-layout", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"direction", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"text-align", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos, 1]],
			"ident",
				Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"text-indent", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"length",      With[{n = parseLength @ tokens[[pos, 2]]},        n],
				"ems",         With[{n = parseEmNonRelative @ tokens[[pos, 2]]}, n],
				"exs",         With[{n = parseEmNonRelative @ tokens[[pos, 2]]}, n/2],
				"percentage",  Missing["Not supported."],
				_,             unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, {LineIndent -> value, Cell[ParagraphIndent -> value]}]
	]


(* ::Subsubsection::Closed:: *)
(*text-decoration*)


(* WL distinguishes between alignment and justification, but CSS does not *)
parse[prop:"text-decoration", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value =
				Switch[tokens[[pos, 1]],
					"ident",
						Switch[ToLowerCase @ tokens[[pos, 2]],
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


parse[prop:"text-transform", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos, 1]],
			"ident",
				Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"letter-spacing", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						"normal",  "Plain",
						_,         unrecognizedKeyWordFailure @ prop
					],
				"length" | "ems" | "exs", parseLength @ tokens[[pos, 2]],
				_,                        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]


(*(* 
	This was originally in CSS 2, but removed in CSS 2.1 due to lack of UA support.
	Added back in Level 3. CSS Fonts Module Level 4 supports percentages as well.
	Mathematica supports both level 3 and 4 features in FontTracking.
*)
parse[prop:"font-stretch", tokens:{{_String, _String}..}] := 
	Module[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident", 
					Switch[ToLowerCase @ tokens[[1, 2]],
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
				"percentage", With[{n = parsePercentage @ tokens[[1, 2]]}, negativeQ[n, prop, n/100]],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]*)


(* ::Subsubsection::Closed:: *)
(*unicode-bidi*)


parse[prop:"unicode-bidi", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"word-spacing", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						"normal",  "Plain",
						_,         unrecognizedKeyWordFailure @ prop
					],
				"length" | "ems" | "exs", parseLength @ tokens[[pos, 2]],
				_,                        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*white-space*)


(* Whitespace is controlled by the Mathematica Front End. The CSS is still validated. *)
parse[prop:"white-space", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos, 1]],
			"ident",
				Switch[ToLowerCase @ tokens[[pos, 2]],
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
	Lengths and percentages are w.r.t. the baseline of the surrounding element.
	Percentages are relative to the font-size i.e. 100% is one line-height upward.
	CellBaseline is limited and always aligns the top of the inline cell to the Bottom/Top/Etc of the parent
*)
parse[prop:"vertical-align", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{value1, value2},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value1 = parseBaseline[prop, tokens];
		If[FailureQ[value1], Return @ value1];
		value2 = parseCellBaseline[prop, tokens];
		If[FailureQ[value2], Return @ value2];
		AmbiguityList[{value1, value2}]
	]

(* this is effectively for RowBox alignment *)
parseBaseline[prop:"vertical-align", tokens:{{_String, _String}..}] := parseBaseline[prop, tokens] = 
	Module[{value (* for Baseline *), value2 (* for CellBaseline *)},
		(* tooManyTokens failure check occurs in higher-level function parse["vertical-align",...]*)
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
parseCellBaseline[prop:"vertical-align", tokens:{{_String, _String}..}] := parseCellBaseline[prop, tokens] = 
	Module[{value},
		(* tooManyTokens failure check occurs in higher-level function parse["vertical-align",...]*)
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[1, 2]],
						"baseline", Center,
						"middle",   Baseline,
						"super" | "sub", Missing["Not supported."],
						"top" | "text-top", Missing["Not supported."], (* because top of in-line is at baseline of cell *)
						"bottom" | "text-bottom", Bottom,
						"inherit", Inherited,
						"initial", Baseline,
						_, unrecognizedKeyWordFailure @ prop
					],
				"length" | "number", parseLength @ tokens[[1, 2]], (* w.r.t. the top of the in-line cell *)
				"ems" | "exs" | "percentage", Missing["Not supported."],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Cell[CellBaseline -> value]]
	]


(* ::Subsection::Closed:: *)
(*visibility*)


(* 
	WL option ShowContents is only applicable within a StyleBox.
	Often this is implemented using the Invisible function.
*)
parse[prop:"visibility", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
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
parse[prop:"z-index", tokens:{{_String, _String}..}] := (*parse[prop, tokens] = *)
	Module[{pos = 1, l = Length[tokens], value, wrapper},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos, 1]],
				"ident",
					Switch[ToLowerCase @ tokens[[pos, 2]],
						"auto",     Automatic,
						"inherit",  Inherited,
						"initial",  initialValues @ prop,
						_,          unrecognizedKeyWordFailure @ prop
					],
				"number", Interpreter["Integer"][tokens[[pos, 2]]],
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
skipWhitespace[positionIndex_, length_Integer, tokens_List] := (
	positionIndex++;
	While[positionIndex < length && tokens[[positionIndex, 1]] == "whitespace", positionIndex++])


Attributes[consumeFunction] = {HoldFirst};
consumeFunction[positionIndex_, l_, tokens:{{_String, _String}..}] :=
	Module[{start = positionIndex, i=1},
		If[tokens[[positionIndex, 1]] != "function", 
			$Failed
			,
			positionIndex++;
			While[positionIndex < l && i>0,
				Switch[ToLowerCase @ tokens[[positionIndex, 1]],
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
	Module[{i = 1, pos = 1, startPosBlock, stopPosBlock, startAtBlock, stopAtBlock, l, lRulesets, relevantTokens, rulesets, condition},
	
		relevantTokens = parseBlock[s]; (* identifies curly-braces, strings, URIs, @import, @charset. Removes comments. *)
		l = Length[relevantTokens];
		
		(*TODO: import other stylesheets*)
		If[MatchQ[relevantTokens[[pos, 1]], "charset"|"import"], pos++; skipWhitespace[pos, l, relevantTokens]];
		
		(* 
			We're looking for blocks indicated by curly brackets. 
			The last declaration block may not have a closing bracket, so count only open brackets as an upper limit to the number of possible blocks.
			@keywords cannot be nested in CSS 2.1.
		*)
		lRulesets = Count[relevantTokens, {"{", _}];
		rulesets = ConstantArray[0, lRulesets];
		While[pos < l && i <= lRulesets,
			startPosBlock = findOpeningBracketPosition[pos, "{", relevantTokens];
			Which[
				StringStartsQ[relevantTokens[[startPosBlock-1, 2]], RegularExpression @ T["PAGE_SYM"]],
					stopPosBlock  = findClosingBracketPosition[startPosBlock, "{", "}", relevantTokens];
					rulesets[[i]] = 
						<|
							"Selector" -> StringTrim @ StringJoin @ relevantTokens[[pos ;; startPosBlock-1, 2]], 
							"Condition" -> StyleData[All, "Printout"],
							"Block" -> relevantTokens[[startPosBlock+1 ;; If[relevantTokens[[stopPosBlock, 1]] == "}", stopPosBlock-1, stopPosBlock]]]|>;
					pos = stopPosBlock + 1;
					i++,
					
				StringStartsQ[relevantTokens[[startPosBlock-1, 2]], RegularExpression @ T["MEDIA_SYM"]],
					startAtBlock = startPosBlock;
					stopAtBlock = findClosingBracketPosition[startAtBlock, "{", "}", relevantTokens];
					pos = startAtBlock+1;
					While[pos < stopAtBlock && i <= lRulesets,
						startPosBlock = findOpeningBracketPosition[pos, "{", relevantTokens];
						stopPosBlock  = findClosingBracketPosition[startPosBlock, "{", "}", relevantTokens];
						rulesets[[i]] = 
							<|
								"Selector" -> StringTrim @ StringJoin @ relevantTokens[[pos ;; startPosBlock-1, 2]], 
								"Condition" -> First @ StringSplit[relevantTokens[[startAtBlock-1, 2]], RegularExpression["(" ~~ T["MEDIA_SYM"] ~~ ")"]],
								"Block" -> relevantTokens[[startPosBlock+1 ;; If[relevantTokens[[stopPosBlock, 1]] == "}", stopPosBlock-1, stopPosBlock]]]|>;
						pos = stopPosBlock + 1;
						i++
					];
					pos = stopAtBlock + 1,
					
				(* skip unknown @ rules, including @import *)
				StringStartsQ[relevantTokens[[startPosBlock-1, 2]], "@"],
					startAtBlock = startPosBlock;
					stopAtBlock = findClosingBracketPosition[startAtBlock, "{", "}", relevantTokens];
					pos = stopAtBlock + 1,
					
				True,
					(* 'stopPosBlock' could reach the end of the token list, but not necessarily be a closed bracket '}' *)
					stopPosBlock  = findClosingBracketPosition[startPosBlock, "{", "}", relevantTokens];
					rulesets[[i]] = 
						<|
							"Selector" -> StringTrim @ StringJoin @ relevantTokens[[pos ;; startPosBlock-1, 2]], 
							"Condition" -> None,
							"Block" -> relevantTokens[[startPosBlock+1 ;; If[relevantTokens[[stopPosBlock, 1]] == "}", stopPosBlock-1, stopPosBlock]]]|>;
					pos = stopPosBlock + 1;
					i++
			];
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
		If[tokens[[pos, 1]] == "whitespace", skipWhitespace[pos, l, tokens]]; (* skip any initial whitespace *)
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[tokens, {":", _}];
		declarations = ConstantArray[0, lDeclarations];
		While[pos < l && i <= lDeclarations,
			If[tokens[[pos, 1]] == "ident",
				propertyPosition = pos; skipWhitespace[pos, l, tokens];
				If[tokens[[pos, 1]] == ":",
					skipWhitespace[pos, l, tokens];
					valueStartPosition = pos;
					(* check for 'important' token, which would be the last token before ';' *)
					While[!MatchQ[tokens[[pos, 1]], ";" | "important"] && pos < l, pos++];
					Switch[tokens[[pos, 1]],
						"important", 
							important = True; 
							valueStopPosition = pos-1;
							skipWhitespace[pos, l, tokens];
							Which[
								(* do nothing extra as 'important' must be on the last declaration that also is missing a semi-colon *)
								pos > l, Null, 
								(* skip over the semi-colon *)
								tokens[[pos, 1]] == ";", pos++,  
								(* syntax error; reset values and let a further parser flag the error *)
								True, valueStopPosition = pos; important = False; While[tokens[[pos, 1]] != ";" && pos < l, pos++]
							],
						";", 
							important = False;
							valueStopPosition = pos-1,
						_, (* case of no 'important' ident and no semi-colon after last declaration in block *)
							important = False;
							valueStopPosition = pos;
					];
					While[tokens[[valueStopPosition, 1]] == "whitespace", valueStopPosition--]; (* trim whitespace from the end of the value *)
					declarations[[i]] = <|
						"Important" -> important,
						"Property" -> ToLowerCase @ tokens[[propertyPosition, 2]], 
						"Value" -> (*check for empty property*)If[valueStopPosition < valueStartPosition, {}, tokens[[valueStartPosition ;; valueStopPosition]]],
						"Interpretation" -> None|>;
					skipWhitespace[pos, l, tokens];
					,
					(* ELSE failed to find colon in declaration, so skip to next declaration by looking for nearest declaration end *)
					While[tokens[[pos, 1]] != ";" && pos < l, pos++];
				];
				,
				(* ELSE failed to find initial identifier in declaration, so skip to next declaration by looking for nearest declaration end *)
				While[tokens[[pos, 1]] != ";" && pos < l, pos++];
			];
			i++ (* increment number of successfully parsed declarations *)
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


(* ::Section:: *)
(*Main Functions*)


(* ::Section:: *)
(*Package Footer*)


(*End[];
EndPackage[];*)
