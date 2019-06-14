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
Needs["CSSTools`CSSTokenizer`"]; (* keep tokenizer utilities hidden *)

(* Selectors3` needed for Selector function *)

SetUsage[CSSHeightMin, "\
CSSHeightMin[value$] indicates value$ is to be interpreted as a minimum height taken from a CSS property."];
SetUsage[CSSHeightMax, "\
CSSHeightMax[value$] indicates value$ is to be interpreted as a maximum height taken from a CSS property."];
SetUsage[CSSWidthMin, "\
CSSWidthMin[value$] indicates value$ is to be interpreted as a minimum width taken from a CSS property."];
SetUsage[CSSWidthMax, "\
CSSWidthMax[value$] indicates value$ is to be interpreted as a maximum width taken from a CSS property."];
SetUsage[CSSBorderColor, "\
CSSBorderColor[value$] indicates value$ is an interpreted CSS color."];
SetUsage[CSSBorderStyle, "\
CSSBorderStyle[value$] indicates value$ is an interpreted CSS border style."];
SetUsage[CSSBorderWidth, "\
CSSBorderWidth[value$] indicates value$ is an interpreted CSS border width."];

CSSBorderStyle

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
tooManyTokensFailure[tokens_List] :=       Failure["UnexpectedParse", <|"Message" -> "Too many tokens.", "Tokens" -> CSSTokenType /@ tokens|>];
unrecognizedKeyWordFailure[prop_String] := Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " keyword."|>];
unrecognizedValueFailure[prop_String] :=   Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " value."|>];
unsupportedValueFailure[prop_String] :=    Failure["UnsupportedProp", <|"Property" -> prop|>]


(* ::Subsection::Closed:: *)
(*Property Data Table*)


(* 
	Some of these are shorthand properties that set one or more other properties. 
	As such, the shorthand initial values would never be directly required.
	The 'unset' keyword takes on the 
		'initial' value if "Inherited" -> False, or
		'inherit' value if "Inherited" -> True.
*)
CSSPropertyData = <|
	"background" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A",  (* shorthand property *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> Background -> Inherited,
			"initial" -> Background -> None|>|>,
	"background-attachment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "scroll",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>,
	"background-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "transparent",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Background -> Inherited,
			"initial" -> Background -> None|>|>,
	"background-image" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> System`BackgroundAppearance -> Inherited,
			"initial" -> System`BackgroundAppearance -> None|>|>, 
	"background-position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0% 0%",
		"InterpretedGlobalValues" -> <|
			(* Technically WL value should be {0,0}, but FE only supports keywords. *)
			"inherit" -> System`BackgroundAppearanceOptions -> Inherited,
			"initial" -> System`BackgroundAppearanceOptions -> "NoRepeat"|>|>, 
	"background-repeat" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "repeat",
		"InterpretedGlobalValues" -> <|
			"inherit" -> System`BackgroundAppearanceOptions -> Inherited,
			"initial" -> System`BackgroundAppearanceOptions -> "Repeat"|>|>, 
	"border-collapse" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "separate",
		"InterpretedGlobalValues" -> <|
			(* FE only follows the 'collapse' model within Grid but does not provide a modifiable option. *)
			"inherit" -> {},
			"initial" -> Missing["Not supported."]|>|>, 
	"border-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* shorthand property, sets all 4 border sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[Inherited]], 
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[CSSBorderColor[Dynamic @ CurrentValue[FontColor]]]], 
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[CSSBorderColor[Dynamic @ CurrentValue[FontColor]]]]}|>|>, 
	"border-top-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Top @ Inherited, 
				CellFrameStyle -> Top @ Inherited},
			"initial" -> {
				FrameStyle     -> Top @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]], 
				CellFrameStyle -> Top @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]]}|>|>, 
	"border-right-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Right @ Inherited, 
				CellFrameStyle -> Right @ Inherited},
			"initial" -> {
				FrameStyle     -> Right @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]], 
				CellFrameStyle -> Right @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]]}|>|>,
	"border-bottom-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Bottom @ Inherited, 
				CellFrameStyle -> Bottom @ Inherited},
			"initial" -> {
				FrameStyle     -> Bottom @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]], 
				CellFrameStyle -> Bottom @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]]}|>|>,
	"border-left-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Left @ Inherited, 
				CellFrameStyle -> Left @ Inherited},
			"initial" -> {
				FrameStyle     -> Left @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]], 
				CellFrameStyle -> Left @ CSSBorderColor[Dynamic @ CurrentValue[FontColor]]}|>|>,
	"border-style" -> <| (* AKA dashing *)
		"Inherited" -> False,
		"CSSInitialValue" -> "none", (* shorthand property, sets all 4 sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[Inherited]], 
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[CSSBorderStyle[None]]], 
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[CSSBorderStyle[None]]]}|>|>, 
	"border-top-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Top @ Inherited, 
				CellFrameStyle -> Top @ Inherited},
			"initial" -> {
				FrameStyle     -> Top @ CSSBorderStyle[None], 
				CellFrameStyle -> Top @ CSSBorderStyle[None]}|>|>, 
	"border-right-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Right @ Inherited, 
				CellFrameStyle -> Right @ Inherited},
			"initial" -> {
				FrameStyle     -> Right @ CSSBorderStyle[None], 
				CellFrameStyle -> Right @ CSSBorderStyle[None]}|>|>,
	"border-bottom-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Bottom @ Inherited, 
				CellFrameStyle -> Bottom @ Inherited},
			"initial" -> {
				FrameStyle     -> Bottom @ CSSBorderStyle[None], 
				CellFrameStyle -> Bottom @ CSSBorderStyle[None]}|>|>,
	"border-left-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Left @ Inherited, 
				CellFrameStyle -> Left @ Inherited},
			"initial" -> {
				FrameStyle     -> Left @ CSSBorderStyle[None], 
				CellFrameStyle -> Left @ CSSBorderStyle[None]}|>|>,
	"border-width" -> <| (* AKA thickness *)
		"Inherited" -> False,
		"CSSInitialValue" -> "medium", (* shorthand property, sets all 4 border sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle -> Through[{Left, Right, Bottom, Top}[Inherited]], 
				CellFrame  -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				FrameStyle -> Through[{Left, Right, Bottom, Top}[CSSBorderWidth[Thickness[Medium]]]], 
				CellFrame  -> Through[{Left, Right, Bottom, Top}[CSSBorderWidth[Thickness[Medium]]]]}|>|>, 
	"border-top-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle -> Top @ Inherited, 
				CellFrame  -> Top @ Inherited},
			"initial" -> {
				FrameStyle -> Top @ CSSBorderWidth[Thickness[Medium]], 
				CellFrame  -> Top @ CSSBorderWidth[Thickness[Medium]]}|>|>, 
	"border-right-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle -> Right @ Inherited, 
				CellFrame  -> Right @ Inherited},
			"initial" -> {
				FrameStyle -> Right @ CSSBorderWidth[Thickness[Medium]], 
				CellFrame  -> Right @ CSSBorderWidth[Thickness[Medium]]}|>|>,
	"border-bottom-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle -> Bottom @ Inherited, 
				CellFrame  -> Bottom @ Inherited},
			"initial" -> {
				FrameStyle -> Bottom @ CSSBorderWidth[Thickness[Medium]], 
				CellFrame  -> Bottom @ CSSBorderWidth[Thickness[Medium]]}|>|>,
	"border-left-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle -> Left @ Inherited, 
				CellFrame  -> Left @ Inherited},
			"initial" -> {
				FrameStyle -> Left @ CSSBorderWidth[Thickness[Medium]], 
				CellFrame  -> Left @ CSSBorderWidth[Thickness[Medium]]}|>|>,
	"border" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand property, sets all 4 border sides color/style/width*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[Inherited]], 
				CellFrame      -> Through[{Left, Right, Bottom, Top}[Inherited]],
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				FrameStyle     -> Through[{Left, Right, Bottom, Top}[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None], CSSBorderWidth[Thickness[Medium]]]], 
				CellFrame      -> Through[{Left, Right, Bottom, Top}[CSSBorderWidth[Thickness[Medium]]]],
				CellFrameStyle -> Through[{Left, Right, Bottom, Top}[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None]]]}|>|>, 
	"border-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Top @ Inherited, 
				CellFrame      -> Top @ Inherited,
				CellFrameStyle -> Top @ Inherited},
			"initial" -> {
				FrameStyle     -> Top[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None], CSSBorderWidth[Thickness[Medium]]], 
				CellFrame      -> Top[CSSBorderWidth[Thickness[Medium]]],
				CellFrameStyle -> Top[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None]]}|>|>, 
	"border-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Right @ Inherited, 
				CellFrame      -> Right @ Inherited,
				CellFrameStyle -> Right @ Inherited},
			"initial" -> {
				FrameStyle     -> Right[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None], CSSBorderWidth[Thickness[Medium]]], 
				CellFrame      -> Right[CSSBorderWidth[Thickness[Medium]]],
				CellFrameStyle -> Right[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None]]}|>|>, 
	"border-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Bottom @ Inherited, 
				CellFrame      -> Bottom @ Inherited,
				CellFrameStyle -> Bottom @ Inherited},
			"initial" -> {
				FrameStyle     -> Bottom[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None], CSSBorderWidth[Thickness[Medium]]], 
				CellFrame      -> Bottom[CSSBorderWidth[Thickness[Medium]]],
				CellFrameStyle -> Bottom[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None]]}|>|>, 
	"border-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameStyle     -> Left @ Inherited, 
				CellFrame      -> Left @ Inherited,
				CellFrameStyle -> Left @ Inherited},
			"initial" -> {
				FrameStyle     -> Left[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None], CSSBorderWidth[Thickness[Medium]]], 
				CellFrame      -> Left[CSSBorderWidth[Thickness[Medium]]],
				CellFrameStyle -> Left[CSSBorderColor[Dynamic @ CurrentValue[FontColor]], CSSBorderStyle[None]]}|>|>,
	"border-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Spacings -> Inherited,
			"initial" -> Spacings -> 0|>|>,
	"bottom" -> <|(* no equivalent FE option *)
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Alignment -> {Automatic, Inherited}, 
			"initial" -> Alignment -> {Automatic, Automatic}|>|>, 
	"caption-side" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "top",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {}, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>|>, 
	"clear" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {}, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>|>,		
	"clip" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {}, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>|>,
	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontColor -> Inherited,
			"initial" -> FontColor -> Black|>|>,(* no set CSS specification, so use reasonable setting *)
	"content" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				CellDingbat -> Inherited,
				CellLabel   -> Inherited},
			"initial" -> {
				CellDingbat -> None,
				CellLabel   -> None}|>|>,    
	"counter-increment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> CounterIncrements -> Inherited,
			"initial" -> CounterIncrements -> {}|>|>,
	"counter-reset" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
		
			"inherit" -> CounterAssignments -> Inherited,
			"initial" -> CounterAssignments -> {}|>|>,   
	"cursor" -> <|(* no FE option to control mouse appearance *)
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {},
			"initial" -> (TagBox[#, MouseAppearanceTag["Arrow"]]&)|>|>, 
	"direction" -> <|(* so far FE only has left-to-right *)
		"Inherited" -> True,
		"CSSInitialValue" -> "ltr",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"display" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "inline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"empty-cells" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "show",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 	
	"float" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"font" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FontFamily     -> Inherited,
				FontSize       -> Inherited,
				FontSlant      -> Inherited,
				FontVariations -> {"CapsType" -> Inherited},
				FontWeight     -> Inherited,
				LineSpacing    -> Inherited},
			"initial" -> {
				FontFamily     :> CurrentValue[$FrontEnd, {StyleDefinitions, "Text", FontFamily}, "Arial"],
				FontSize       -> Medium,
				FontSlant      -> Plain,
				FontVariations -> {"CapsType" -> "Normal"},
				FontWeight     -> Plain,
				LineSpacing    -> {1.2, 0}}|>|>, 
	"font-family" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontFamily -> Inherited,
			"initial" -> FontFamily :> CurrentValue[$FrontEnd, {StyleDefinitions, "Text", FontFamily}, "Arial"]|>|>, 
	"font-size" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontSize -> Inherited,
			"initial" -> FontSize -> Medium|>|>,   
	"font-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontSlant -> Inherited,
			"initial" -> FontSlant -> Plain|>|>,    
	"font-variant" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontVariations -> {"CapsType" -> Inherited},
			"initial" -> FontVariations -> {"CapsType" -> "Normal"}|>|>, 
	"font-weight" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontWeight -> Inherited,
			"initial" -> FontWeight -> Plain|>|>, 
	"height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> {CSSHeightMin[Inherited], CSSHeightMax[Inherited]},
			"initial" -> ImageSize -> {CSSHeightMin[Automatic], CSSHeightMax[Automatic]}|>|>, 
	"left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Alignment -> {Inherited, Automatic},
			"initial" -> Alignment -> {Automatic, Automatic}|>|>, 
	"letter-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontTracking -> Inherited,
			"initial" -> FontTracking -> "Plain"|>|>, 
	"line-height" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> LineSpacing -> Inherited,
			"initial" -> LineSpacing -> {1.2, 0}|>|>, 
	"list-style" -> <|(* short-hand for list-style-image/position/type *)
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> CellDingbat -> Inherited,
			"initial" -> CellDingbat -> "\[FilledCircle]"|>|>, 
	"list-style-image" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "",
		"InterpretedGlobalValues" -> <|
			"inherit" -> CellDingbat -> Inherited,
			"initial" -> CellDingbat -> None|>|>,
	"list-style-position" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "outside",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>,
	"list-style-type" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "disc",
		"InterpretedGlobalValues" -> <|
			"inherit" -> CellDingbat -> Inherited,
			"initial" -> CellDingbat -> "\[FilledCircle]"|>|>,
	"margin" -> <|(* shorthand property, sets all 4 margins *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				ImageMargins -> Through[{Left, Right, Bottom, Top}[Inherited]],
				CellMargins  -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				ImageMargins -> Through[{Left, Right, Bottom, Top}[0]],
				CellMargins  -> Through[{Left, Right, Bottom, Top}[0]]}|>|>, 
	"margin-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				ImageMargins -> Top @ Inherited,
				CellMargins  -> Top @ Inherited},
			"initial" -> {
				ImageMargins -> Top @ 0,
				CellMargins  -> Top @ 0}|>|>,
	"margin-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				ImageMargins -> Right @ Inherited,
				CellMargins  -> Right @ Inherited},
			"initial" -> {
				ImageMargins -> Right @ 0,
				CellMargins  -> Right @ 0}|>|>,
	"margin-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				ImageMargins -> Bottom @ Inherited,
				CellMargins  -> Bottom @ Inherited},
			"initial" -> {
				ImageMargins -> Bottom @ 0,
				CellMargins  -> Bottom @ 0}|>|>,
	"margin-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				ImageMargins -> Left @ Inherited,
				CellMargins  -> Left @ Inherited},
			"initial" -> {
				ImageMargins -> Left @ 0,
				CellMargins  -> Left @ 0}|>|>,
	"max-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> CSSHeightMax[Inherited],
			"initial" -> ImageSize -> CSSHeightMax[Infinity]|>|>,
	"max-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> CSSWidthMax[Inherited],
			"initial" -> ImageSize -> CSSWidthMax[Infinity]|>|>,
	"min-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> CSSHeightMin[Inherited],
			"initial" -> ImageSize -> CSSHeightMin[0]|>|>,
	"min-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> CSSWidthMin[Inherited],
			"initial" -> ImageSize -> CSSWidthMin[0]|>|>,
	"orphans" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"outline" -> <|(* shorthand property, sets color/style/width, FE does not support outlines *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"outline-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "invert",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"outline-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"outline-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"overflow" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "visible",(* not supported in FE*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSizeAction -> Inherited,
			"initial" -> Missing["Not supported."]|>|>, 
	"padding" -> <|(* shorthand property, sets all 4 sides *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameMargins     -> Through[{Left, Right, Bottom, Top}[Inherited]],
				CellFrameMargins -> Through[{Left, Right, Bottom, Top}[Inherited]]},
			"initial" -> {
				FrameMargins     -> Through[{Left, Right, Bottom, Top}[0]],
				CellFrameMargins -> Through[{Left, Right, Bottom, Top}[0]]}|>|>, 
	"padding-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameMargins     -> Top @ Inherited,
				CellFrameMargins -> Top @ Inherited},
			"initial" -> {
				FrameMargins     -> Top @ 0,
				CellFrameMargins -> Top @ 0}|>|>,
	"padding-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameMargins     -> Bottom @ Inherited,
				CellFrameMargins -> Bottom @ Inherited},
			"initial" -> {
				FrameMargins     -> Bottom @ 0,
				CellFrameMargins -> Bottom @ 0}|>|>,
	"padding-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameMargins     -> Left @ Inherited,
				CellFrameMargins -> Left @ Inherited},
			"initial" -> {
				FrameMargins     -> Left @ 0,
				CellFrameMargins -> Left @ 0}|>|>,
	"padding-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				FrameMargins     -> Right @ Inherited,
				CellFrameMargins -> Right @ Inherited},
			"initial" -> {
				FrameMargins     -> Right @ 0,
				CellFrameMargins -> Right @ 0}|>|>,
	"page-break-after" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> PageBreakBelow -> Inherited,
			"initial" -> PageBreakBelow -> Automatic|>|>, 
	"page-break-before" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> PageBreakAbove -> Inherited,
			"initial" -> PageBreakAbove -> Automatic|>|>, 
	"page-break-inside" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				PageBreakWithin      -> Inherited,
				GroupPageBreakWithin -> Inherited},
			"initial" -> {
				PageBreakWithin      -> Automatic,
				GroupPageBreakWithin -> Automatic}|>|>, 
	"position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "static",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"quotes" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>,  
	"right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Alignment -> {Inherited, Automatic},
			"initial" -> Alignment -> {Automatic, Automatic}|>|>, 
	"table-layout" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 		
	"text-align" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* a nameless value that acts as 'left' if 'direction' is 'ltr', 'right' if 'direction' is 'rtl' *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> TextAlignment -> Inherited,
			"initial" -> TextAlignment -> Automatic|>|>, 
	"text-decoration" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontVariations -> Inherited,
			"initial" -> FontVariations -> {}|>|>,       
	"text-indent" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				LineIndent      -> Inherited,
				ParagraphIndent -> Inherited},
			"initial" -> {
				LineIndent      -> 0,
				ParagraphIndent -> 0}|>|>,
	"text-transform" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontVariations -> {"CapsType" -> Inherited},
			"initial" -> FontVariations -> {"CapsType" -> "Normal"}|>|>,  
	"top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Alignment -> {Automatic, Inherited},
			"initial" -> Alignment -> {Automatic, Automatic}|>|>, 
	"unicode-bidi" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"vertical-align" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "baseline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				BaselinePosition -> Inherited,
				CellBaseline     -> Inherited},
			"initial" -> {
				BaselinePosition -> Baseline -> Baseline,
				CellBaseline     -> Center}|>|>,  
	"visibility" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visible",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ShowContents -> Inherited,
			"initial" -> ShowContents -> True|>|>, 
	"white-space" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"widows" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> ImageSize -> {CSSWidthMin[Inherited], CSSWidthMax[Inherited]},
			"initial" -> ImageSize -> {CSSWidthMin[Automatic], CSSWidthMax[Automatic]}|>|>, 
	"word-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>, 
	"z-index" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>|>
	|>;


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


(* counter() function *)
parseCounter[prop_String, tokens:{___?CSSTokenQ}] := parseCounter[prop, tokens] =
	Block[{pos = 3, l = Length[tokens], style = "Item", listtype = "decimal"},
		(* pos starts at 3 as this skips the function identifier and name *)
		If[pos <= l && tokenTypeIs[" "], advancePosAndSkipWhitespace[]];
		
		(* get custom identifier *)
		If[pos <= l && tokenTypeIs["ident"], 
			style = CSSTokenString @ tokens[[pos]]
			, 
			Return @ invalidFunctionFailure @ CSSUntokenize @ tokens
		];
		advancePosAndSkipWhitespace[];
		If[pos > l, Return @ parseSingleListStyleType["list-style-type", {"ident", listtype}, style]];
		
		(* get optional counter style *)
		If[pos <= l && tokenTypeIs["ident"],
			listtype = CSSTokenString @ tokens[[pos]]
			,
			Return @ invalidFunctionFailure @ CSSUntokenize @ tokens
		];
		advancePosAndSkipWhitespace[];
		If[pos > l, Return @ parseSingleListStyleType["list-style-type", {"ident", listtype}, style]];
		
		tooManyTokensFailure @ tokens
	]
	
(* counters() function *)
parseCounters[prop_String, tokens:{___?CSSTokenQ}] := Missing["Not supported."]


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


(* URI token is of the form {"url", location} where location is a string without the url() wrapper and string characters *)
parseURI[uri_String] := 
	Module[{p, start, rest},
		If[StringStartsQ[uri, "data:", IgnoreCase -> True],
			(* string split on the first comma *)
			p = StringPosition[uri, ",", 1][[1, 1]];
			start = StringTake[uri, {1, p-1}];
			rest = StringTake[uri, {p+1, -1}];
			
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
						"image/x-icon",    ImportString[rest, "ICO"],
						"image/vnd.microsoft.icon", ImportString[rest, "ICO"],
						"audio/wave",      Audio[rest],
						"audio/wav",       Audio[rest],
						"audio/x-wav",     Audio[rest],
						"audio/x-pn-wav",  Audio[rest],
						_,                 Missing["Not supported."]
					]
			]
			,
			(* else attempt a generic import *)
			With[{i = Quiet @ Import[uri]}, If[i === $Failed, couldNotImportFailure[uri], i]]
		]
	]


(* ::Subsection::Closed:: *)
(*global values*)


(* global values *)
consumeProperty[prop_String, {{"ident", x_?StringQ /; StringMatchQ[x, "inherit", IgnoreCase -> True]}}] := 
	CSSPropertyData[prop, "InterpretedGlobalValues", "inherit"]
	
consumeProperty[prop_String, {{"ident", x_?StringQ /; StringMatchQ[x, "initial", IgnoreCase -> True]}}] := 
	CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]
	
consumeProperty[prop_String, {{"ident", x_?StringQ /; StringMatchQ[x, "unset", IgnoreCase -> True]}}] := 
	If[CSSPropertyData[prop, "Inherited"], 
		CSSPropertyData[prop, "InterpretedGlobalValues", "inherit"]
		, 
		CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]
	]

initialValues[prop_String] := 
	Module[{val = CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]}, 
		val =
			Which[
				MissingQ[val],                   val, 
				MatchQ[val, {Rule[_, _List]..}], val[[1, 2, 1]],
				MatchQ[val, {Rule[_, _]..}],     val[[1, 2]],
				MatchQ[val, _Function],          val,
				True,                            val[[2]]
			];
		If[MatchQ[Head[val], Left | Right | Bottom | Top], First @ val, val]
	]


(* ::Subsection::Closed:: *)
(*background*)


(* ::Subsubsection::Closed:: *)
(*background*)


(* 
	Shorthand for all background properties.
	Any properties not set by this are reset to their initial values. (FE generally uses None.)
	
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and some new properties.
	We only support CSS 2.1 for now and it supports only a single value.
	For temporary forward compatibility, we split the token sequence on any commas and take the first of the split sequence.
*)
consumeProperty[prop:"background", inputTokens:{__?CSSTokenQ}] := 
	Module[{backgrounds, result},
		backgrounds = DeleteCases[SplitBy[inputTokens, ","], {","}];
		backgrounds = parseSingleBG[prop, #]& /@ backgrounds; 
		result = Cases[backgrounds, Except[_Failure], {1}];
		If[result === {}, 
			FirstCase[backgrounds, _Failure, Failure["BadBackground", <|"Message" -> "Could not parse background property."|>], {1}]
			,
			First @ result
		]
	]

parseSingleBG[prop_String, inputTokens:{__?CSSTokenQ}] := 
	Block[
		{
			pos = 1, l = Length[inputTokens], tokens = inputTokens, value, start, startToken, 
			values = <|
				"a" -> initialValues @ "background-attachment", 
				"c" -> initialValues @ "background-color",
				"i" -> initialValues @ "background-image",
				"p" -> initialValues @ "background-position",
				"r" -> initialValues @ "background-repeat"|>,
			hasAttachment = False, hasColor = False, hasImage = False, hasPosition = False, hasRepeat = False
		},
		While[pos <= l, 
			Which[
				tokenTypeIs["function"], 
					Switch[CSSTokenString @ tokens[[pos]],
						(* color *)
						"rgb" | "rgba" | "hsl" | "hsla", 
							If[hasColor, Return @ repeatedPropValueFailure @ "background-color"]; 
							hasColor = True; values["c"] = parseSingleColor[prop, tokens[[pos]]];,
						(*TODO support gradients *)
						"linear-gradient" | "repeating-linear-gradient" | "radial-gradient" | "repeating-radial-gradient" | "conic-gradient", 
							If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
							hasImage = True; values["i"] = Missing["Not supported."];,
						_,
							Return @ invalidFunctionFailure @ CSSTokenString @ tokens[[pos]]
					],
				
				(* scroll or fixed keyword *)
				!FailureQ[value = parseSingleBGAttachment[prop, tokens[[pos]]]],
					If[hasAttachment, Return @ repeatedPropValueFailure @ "background-attachment"];
					hasAttachment = True; values["a"] = value,
					
				(* color hex or color keyword *)
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]], (* color can also be hex or keyword *)
					If[hasColor, Return @ repeatedPropValueFailure @ "background-color"];
					hasColor = True; values["c"] = value,
				
				(* uri token or none keyword *)
				!FailureQ[value = parseSingleBGImage[prop, tokens[[pos]]]], 
					If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
					hasImage = True; values["i"] = value,
				
				(* one of the keywords repeat | repeat-x | repeat-y | no-repeat *)
				!FailureQ[value = parseSingleBGRepeat[prop, tokens[[pos]]]], 
					If[hasRepeat, Return @ repeatedPropValueFailure @ "background-repeat"];
					hasRepeat = True; values["r"] = value,
				
				(* one of the key words left | center | right | top | bottom, *)
				!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
					If[hasPosition, Return @ repeatedPropValueFailure @ "background-position"];
					hasPosition = True; values["p"] = {value, Center};
					(* check for a pair of position values; they must be sequential *)
					start = pos; startToken = tokens[[pos]]; advancePosAndSkipWhitespace[];
					If[!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
						values["p"] = {values["p"][[1]], value}
						,
						pos = start (* if this next token leads to a parse failure, then reset the position *)
					];
					values["p"] = parseSingleBGPositionPair[values["p"], {startToken, tokens[[pos]]}],
								
				True, unrecognizedValueFailure @ prop						
			];
			advancePosAndSkipWhitespace[]
		];
		(* 
			attachment: not supported, 
			color: Background, 
			image: System`BackgroundAppearance, 
			position: System`BackgroundAppearanceOptions, 
			repeat: System`BackgroundAppearanceOptions *)
		Which[
			hasColor && Not[hasAttachment || hasImage || hasPosition || hasRepeat], 
				Background -> values["c"],
			True,
				{
					System`BackgroundAppearanceOptions ->
						Which[
							values["p"] === {0,0}    && values["r"] === "NoRepeat", "NoRepeat",
							values["p"] === "Center" && values["r"] === "NoRepeat", "Center",
							values["p"] === {0,0},                                  values["r"],
							True,                                                   Missing["Not supported."]
						],
					System`BackgroundAppearance -> values["i"],
					Background -> values["c"]}
		]
]


(* ::Subsubsection::Closed:: *)
(*background-attachment*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and the 'local' value.
	We only support CSS 2.1 for now and it supports only a single value. *)
consumeProperty[prop:"background-attachment", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGAttachment[prop, tokens[[pos]]];
		If[FailureQ[value] || MissingQ[value], value, Missing["Only fixed supported."]]
	]

parseSingleBGAttachment[prop_String, token_?CSSTokenQ] := 
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"scroll", Missing["Not supported."],
				"fixed",  Automatic, (* FE follows this mode, but there's no FE option for it *)
				_,        unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*background-color*)


(* 
	Effectively the same as color, except a successful parse returns as a rule Background -> value.
	Also 'currentColor' value needs to know the current value of 'color', instead of inherited from the parent.	*) 
consumeProperty[prop:"background-color", inputTokens:{__?CSSTokenQ}] := 
	Module[{value},
		value = parseSingleColor[prop, inputTokens];
		If[FailureQ[value], value, Background -> value]
	]


(* ::Subsubsection::Closed:: *)
(*background-image*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and any CSS <image> data type.
	We only support CSS 2.1 for now and it supports only a single value. *)
consumeProperty[prop:"background-image", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGImage[prop, First @ tokens];
		If[FailureQ[value], value, System`BackgroundAppearance -> value]
	]

parseSingleBGImage[prop_String, token_?CSSTokenQ] := 
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"none", None,
				_,      unrecognizedKeyWordFailure @ prop
			],
		"function", 
			Switch[CSSTokenString @ token,
				Alternatives[
					"linear-gradient", "repeating-linear-gradient",
					"radial-gradient", "repeating-radial-gradient", "conic-gradient"
				],
				   Missing["Not supported."],
				_, invalidFunctionFailure @ CSSUntokenize @ token
			],
		"uri", parseURI @ CSSTokenString @ token,
		_,     unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*background-position*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and use a 4-value syntax.
	We only support CSS 2.1 for now and it can take up to 2 values. *)
consumeProperty[prop:"background-position", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, values = {}},
		While[pos <= l,
			value = parseSingleBGPosition[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[values, value]];
			advancePosAndSkipWhitespace[]
		];
		value = parseSingleBGPositionPair[values, tokens];		
		If[FailureQ[value], value, System`BackgroundAppearanceOptions -> value]
	]

parseSingleBGPosition[prop_String, token_?CSSTokenQ] :=
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"left",    Left,
				"center",  Center,
				"right",   Right,
				"top",     Top,
				"bottom",  Bottom,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"percentage", parsePercentage @ token,
		"dimension",  parseLength @ token,
		"number",     parseZero @ token,
		_,            unrecognizedValueFailure @ prop
	]

(* FE supports a limited number of background positioning and repeat specs. *)
parseSingleBGPositionPair[values:{__}, inputTokens:{__?CSSTokenQ}] :=
	Switch[Length[values],
		1, 
			Switch[values[[1]],
				Center, "Center",
				_,      Missing["Not supported."]
			],
		2, 
			Switch[values,
				Alternatives[
					{Top, Left}, {Left, Top}, 
					{0, 0},	{0, Top}, {Left, 0}
				],                              "NoRepeat",
				{Center, Center},               "Center", 
				_,                              Missing["Not supported."]
			],
		_, tooManyTokensFailure @ tokens
	]


(* ::Subsubsection::Closed:: *)
(*background-repeat*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds, use a 2-value syntax, and adds 'space' and 'round' keywords.
	We only support CSS 2.1 for now and it can take only a single value. *)
consumeProperty[prop:"background-repeat", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGRepeat[prop, tokens[[pos]]];
		If[FailureQ[value], value, System`BackgroundAppearanceOptions -> value]
	]
	

parseSingleBGRepeat[prop_String, token_?CSSTokenQ] :=
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"no-repeat", "NoRepeat",
				"repeat-x",  "RepeatX",
				"repeat-y",  "RepeatY",
				"repeat",    "Repeat",
				_,           unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]


(* ::Subsection::Closed:: *)
(*border*)


(*
	WL specifies border style (dashing), width, and color all at once via Directive[].
	Post-processing is required to combine individual properties correctly.
	WL Grid only allows collapsed borders. However, each item can be wrapped in Frame, but this is a hack.
		Grid[
			Map[
				Framed[#, ImageSize -> Full, Alignment -> Center]&, 
				{{"Client Name", "Age"}, {"", 25}, {"Louise Q.", ""}, {"Owen", ""}, {"Stan", 71}}, 
				{2}],
			Frame -> None,
			ItemSize -> {{7, 3}}]
	This is beyond the scope of CSS importing.
*)


(* ::Subsubsection::Closed:: *)
(*border-collapse*)


(* 
	FE only follows the 'collapse' model within Grid but and not provide a modifiable FE option. 
	As it cannot be modified, we parse for correct syntax but only allow through the 'collapse' value.
	The empty list return value of a successful parse is ignored during post-processing.*)
consumeProperty[prop:"border-collapse", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		Switch[CSSTokenType @ tokens[[pos]],
			"ident", 
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
					"separate", Missing["Not supported."],
					"collapse", {}, (* this is all Mathematica supports *)
					_,          unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-color*)


(* 
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time. 
	This requires post-processing of the parsed result. We use Top/Right/Bottom/Left wrappers to keep track of each edge.*)
consumeProperty[
	prop:"border-top-color" | "border-right-color" | "border-bottom-color" | "border-left-color", 
	inputTokens:{__?CSSTokenQ}
] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, wrapper},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = 
				Switch[prop, 
					"border-top-color",    Top, 
					"border-right-color",  Right, 
					"border-bottom-color", Bottom, 
					"border-left-color",   Left
				];
			# -> wrapper[CSSBorderColor @ value]& /@ {FrameStyle, CellFrameStyle}
		]
	]	

(* sets all 4 border/frame edges at once *)
consumeProperty[prop:"border-color", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, results = {}},
		While[pos <= l,
			value = parseSingleColor[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, CSSBorderColor @ value]]; 
			advancePosAndSkipWhitespace[];
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
consumeProperty[prop:"border-spacing", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, results = {}},
		While[pos <= l,
			value = 
				Switch[CSSTokenType @ tokens[[pos]],
					"dimension", 
						If[CSSTokenValue @ tokens[[pos]] < 0,
							negativeLengthFailure @ prop
							,
							Switch[CSSDimensionUnit @ tokens[[pos]],
								"em"|"ex", (parseLengthNonRelative @ tokens[[pos]])/2,
								_,         With[{n = parseLength @ tokens[[pos]]}, Dynamic[n/CurrentValue[FontSize]]]
							]
						],
					"number", parseZero @ tokens[[pos]],
					_,        unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			advancePosAndSkipWhitespace[];
		];
		Switch[Length[results],
			1, Spacings -> {First @ results, First @ results}, (* if only one length, then it specifies both horizontal and vertical *)
			2, Spacings -> results,
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-style*)


(* 
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time. 
	This requires post-processing of the parsed result. We use Top/Right/Bottom/Left wrappers to keep track of each edge. *)
consumeProperty[
	prop:"border-top-style" | "border-right-style" | "border-bottom-style" | "border-left-style", 
	inputTokens:{__?CSSTokenQ}
] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, wrapper},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderStyle[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-style", Top, "border-right-style", Right, "border-bottom-style", Bottom, "border-left-style", Left];
			# -> wrapper[CSSBorderStyle @ value]& /@ {FrameStyle, CellFrameStyle}
		]
	]	
	
(* sets all 4 border/frame edges at once *)
consumeProperty[prop:"border-style", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, results = {}},
		While[pos <= l,
			value = parseSingleBorderStyle[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, CSSBorderStyle @ value]];
			advancePosAndSkipWhitespace[]
		];
		Switch[Length[results],
			1, # -> {Left @ results[[1]], Right @ results[[1]], Bottom @ results[[1]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			2, # -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[1]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			3, # -> {Left @ results[[2]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			4, # -> {Left @ results[[4]], Right @ results[[2]], Bottom @ results[[3]], Top @ results[[1]]}& /@ {FrameStyle, CellFrameStyle},
			_, tooManyTokensFailure @ tokens
		]
	]

parseSingleBorderStyle[prop_String, token_?CSSTokenQ] :=
	Switch[CSSTokenType @ token,
		"ident",
			Switch[ToLowerCase @ CSSTokenString @ token,
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


(* ::Subsubsection::Closed:: *)
(*border-width*)


(*
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time.
	This requires post-processing of the parsed result. We use Top/Right/Bottom/Left wrappers to keep track of each edge. *)
consumeProperty[
	prop:"border-top-width" | "border-right-width" | "border-bottom-width" | "border-left-width", 
	inputTokens:{__?CSSTokenQ}
] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, wrapper},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderWidth[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "border-top-width", Top, "border-right-width",  Right, "border-bottom-width", Bottom, "border-left-width", Left];
			{FrameStyle -> wrapper[CSSBorderWidth @ value], CellFrame -> wrapper[CSSBorderWidth @ value]}
		]
	]	
	
(* sets all 4 frame edge thickness at once *)
consumeProperty[prop:"border-width", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, results = {}},
		While[pos <= l,
			value = parseSingleBorderWidth[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, CSSBorderWidth @ value]];
			advancePosAndSkipWhitespace[]
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
		{FrameStyle -> results, CellFrame -> results}
	]
	
(* WL FrameStyle thickness is best given as a calculated AbsoluteThickness for numerical values. *)
parseSingleBorderWidth[prop_String, token_?CSSTokenQ] :=
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"thin",   Thickness[Small],
				"medium", Thickness[Medium],
				"thick",  Thickness[Large],
				_,        unrecognizedKeyWordFailure @ prop
			],
		"dimension", If[CSSTokenValue @ token < 0, negativeLengthFailure @ prop, AbsoluteThickness[parseLength @ token]],
		"number",    parseZero @ token,
		_,           unrecognizedValueFailure @ prop
	]


convertToCellThickness[x_] := Switch[x, AbsoluteThickness[_], First[x], Thickness[_], First[x] /. {Small -> 1, Medium -> 2, Large -> 4}, _, x]


(* ::Subsubsection::Closed:: *)
(*border (-top, -bottom, -right, -left)*)


(* 
	Shorthand for border-*-width/style/color. 
	This effectively resets all edge properties because any property not specified takes on its default value. 
	'border' by itself sets all 4 edges to be the same. *)
consumeProperty[
	prop:"border" | "border-top" | "border-right" | "border-bottom" | "border-left", 
	inputTokens:{__?CSSTokenQ}
] := 
	Block[
		{
			pos = 1, l = Length[inputTokens], tokens = inputTokens, value, 
			wrapper = Switch[prop, "border-left", Left, "border-right", Right, "border-top", Top, "border-bottom", Bottom, _, Through[{Left, Right, Top, Bottom}[#]]&],
			values = <|
				"c" -> initialValues[prop <> "-color"], 
				"s" -> initialValues[prop <> "-style"], 
				"w" -> initialValues[prop <> "-width"]|>,
			hasColor = False, hasStyle = False, hasWidth = False
		},
		
		While[pos <= l,
			Which[
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]],
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")];
					hasColor = True; values["c"] = CSSBorderColor @ value,
				
				!FailureQ[value = parseSingleBorderStyle[prop, tokens[[pos]]]],
					If[hasStyle, Return @ repeatedPropValueFailure @ (prop <> "-style")];
					hasStyle = True; values["s"] = CSSBorderStyle @ value,
					
				!FailureQ[value = parseSingleBorderWidth[prop, tokens[[pos]]]],
					If[hasWidth, Return @ repeatedPropValueFailure @ (prop <> "-width")];
					hasWidth = True; values["w"] = CSSBorderWidth @ value,
				
				True, unrecognizedValueFailure @ prop						
			];
			advancePosAndSkipWhitespace[];
		];
		
		{
			FrameStyle -> wrapper[values["c"], values["s"], values["w"]], 
			CellFrameStyle -> wrapper[values["c"], values["s"]], 
			CellFrame -> wrapper[values["w"]]}
	]


(* ::Subsection::Closed:: *)
(*clip*)


(* deprecated, but we still mostly parse it for correctness *)
consumeProperty[prop:"clip", inputTokens:{__?CSSTokenQ}] := 
	Block[{value, pos = 1, l = Length[inputTokens], tokens = inputTokens},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"auto", Missing["Not supported."],
						_,      unrecognizedKeyWordFailure @ prop
					],
				"function", parseRect @ tokens[[pos]], (* FIXME: actually parse this? is it even worth it since it's not supported? *)
				_,          unrecognizedValueFailure @ prop
			];
		If[TrueQ @ FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*color*)


consumeProperty[prop:"color", inputTokens:{__?CSSTokenQ}] := 
	With[{value = parseSingleColor[prop, inputTokens]},
		If[FailureQ[value], value, FontColor -> parseSingleColor[prop, inputTokens]]]


(* ::Subsection::Closed:: *)
(*content, lists, and quotes*)


(* ::Subsubsection::Closed:: *)
(*content*)


(* only used to add content before or after element, so let's restrict this to Cells' CellDingbat or CellLabel *)
consumeProperty[prop:"content", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, parsedValues = {}},
		While[pos <= l,
			value = 
				Switch[CSSTokenType @ tokens[[pos]],
					"ident", 
						Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
							"normal",         CellDingbat -> "\[FilledCircle]",
							"none",           CellDingbat -> None,
							"open-quote",     Missing["Not supported."],
							"close-quote",    Missing["Not supported."],
							"no-open-quote",  Missing["Not supported."],
							"no-close-quote", Missing["Not supported."],
							_,                unrecognizedKeyWordFailure @ prop
						],
					"string", CellLabel -> CSSTokenString @ tokens[[pos]], (* is this even doing this option justice? *)
					"uri",    
						With[{i = parseURI @ CSSTokenString @ tokens[[pos]]}, 
							If[FailureQ[i] || MissingQ[i], 
								notAnImageFailure @ CSSTokenString @ tokens[[pos]]
								, 
								CellDingbat -> i
							]
						],
					"function", 
						Switch[CSSTokenString @ tokens[[pos]],
							"counter",  CellDingbat -> parseCounter[prop, tokens[[pos, 3;;]]],
							"counters", CellDingbat -> parseCounters[prop, tokens[[pos, 3;;]]],
							"attr",     (*TODO*)parseAttr[prop, tokens[[pos, 3;;]]],
							_,          unrecognizedValueFailure @ prop
						],
					_, unrecognizedValueFailure @ prop
				];
			AppendTo[parsedValues, value];
			advancePosAndSkipWhitespace[];
		];
		Which[
			Count[parsedValues, None] > 1,   repeatedPropValueFailure @ "none",
			Count[parsedValues, Normal] > 1, repeatedPropValueFailure @ "normal",
			True,                            parsedValues
		]
	]


(* ::Subsubsection::Closed:: *)
(*counter-increment*)


(* In WL each style must be repeated n times to get an increment of n *)
consumeProperty[prop:"counter-increment", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, v, values = {}, cPos, n},
		While[pos <= l,
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"none", 
							If[l > 1,  (* none must be the only identifier if it is used *)
								Return @ illegalIdentifierFailure @ CSSTokenString @ tokens[[pos]]
								, 
								values = {}
							],
						_,  
							v = CSSTokenString @ tokens[[pos]];
							(* check to see if identifier token is immediately followed by an integer; if so, consume the integer, too *)
							cPos = pos; advancePosAndSkipWhitespace[];
							If[pos <= l && CSSTokenType @ tokens[[pos]] == "number",
								If[CSSTokenValueType @ tokens[[pos]] != "integer", Return @ Failure["BadNumber", <|"Message" -> "Expected integer type."|>]];
								n = CSSTokenValue @ tokens[[pos]];
								If[n < 0, 
									Return @ negativeIntegerFailure @ prop (* FE can only do positive increments *)
									,
									values = Join[values, ConstantArray[v, n]]
								];
								,
								values = Join[values, {v}]; pos = cPos;
							];
					],
				_, Return @ unrecognizedValueFailure @ prop
			];
			advancePosAndSkipWhitespace[];
		];
		CounterIncrements -> values
	]


(* ::Subsubsection::Closed:: *)
(*counter-reset*)


consumeProperty[prop:"counter-reset", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, v, values = {}, cPos},
		While[pos <= l,
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"none", 
							If[l > 1, 
								Return @ illegalIdentifierFailure @ CSSTokenString @ tokens[[pos]]
								, 
								values = {}
							],
						_,        
							v = CSSTokenString @ tokens[[pos]];
							(* check to see if identifier token is immediately followed by an integer; if so, consume the integer, too *)
							cPos = pos; advancePosAndSkipWhitespace[];
							If[pos <= l && CSSTokenType @ tokens[[pos]] == "number",
								If[CSSTokenValueType @ tokens[[pos]] != "integer", Return @ Failure["BadNumber", <|"Message" -> "Expected integer type."|>]];
								AppendTo[values, {v, CSSTokenValue @ tokens[[pos]]}]
								,
								AppendTo[values, {v, 0}]; pos = cPos;
							];
					],
				_, Return @ unrecognizedValueFailure @ prop
			];
			advancePosAndSkipWhitespace[];
		];
		CounterAssignments -> values
	]


(* ::Subsubsection::Closed:: *)
(*list-style-image*)


consumeProperty[prop:"list-style-image", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleImage[prop, First @ tokens];
		If[FailureQ[value], value, CellDingbat -> value]
	]

parseSingleListStyleImage[prop_String, token_?CSSTokenQ] := 
	Switch[CSSTokenType @ token,
		"ident",
			Switch[ToLowerCase @ CSSTokenString @ token,
				"none", None,
				_,      unrecognizedKeyWordFailure @ prop
			],
		"uri", 
			With[{im = parseURI @ CSSTokenString @ token}, 
				Which[
					FailureQ[im], im, 
					MissingQ[im], im,
					!ImageQ[im],  notAnImageFailure @ CSSTokenString @ token,
					True,         ToBoxes @ Dynamic @ Image[im, ImageSize -> CurrentValue[FontSize]]
				]
			],
		_, unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*list-style-position*)


(* 
	CellDingbat position is always outside the cell content and aligned with the first line of content.
	Though the following validates the CSS, Mathematica does not include any position option.
*)
consumeProperty[prop:"list-style-position", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStylePosition[prop, First @ tokens];
		If[FailureQ[value], value, Missing["Not supported."]]
	]

parseSingleListStylePosition[prop_String, token_?CSSTokenQ] := 
	Switch[CSSTokenType @ token,
		"ident",
			Switch[ToLowerCase @ CSSTokenString @ token,
				"inside",  Missing["Not supported."],
				"outside", Automatic,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*list-style-type*)


consumeProperty[prop:"list-style-type", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleType[prop, First @ tokens];
		If[FailureQ[value], value, CellDingbat -> value]
	]
	
parseSingleListStyleType[prop_String, token_?CSSTokenQ, style_String:"Item"] := 
	Switch[CSSTokenType @ token,
		"ident",
			Switch[ToLowerCase @ CSSTokenString @ token,
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
	

(* ::Subsubsection::Closed:: *)
(*list-style*)


(* short-hand for list-style-image/position/type properties given in any order *)
consumeProperty[prop:"list-style", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, values, p, noneCount = 0, hasImage = False, hasPos = False, hasType = False},
		(* 
			li-image, li-position, and li-type can appear in any order.
			A value of 'none' sets whichever of li-type and li-image are not otherwise specified to 'none'. 
			If both are specified, then an additional 'none' is an error.
		*)
		values = <|
			"i" -> initialValues[prop <> "-image"], 
			"p" -> initialValues[prop <> "-position"], 
			"t" -> initialValues[prop <> "-type"]|>;
		While[pos <= l,
			Which[
				(* check for 'none' keyword *)
				ToLowerCase @ CSSTokenString @ tokens[[pos]] == "none", noneCount++,					
				
				(* check for list-style-image *)
				!FailureQ[value = parseSingleListStyleImage[prop, tokens[[pos]]]],
					If[hasImage, 
						Return @ repeatedPropValueFailure @ "image"
						, 
						values["i"] = value[[p]]; hasImage = True
					],
					
				(* check for list-style-position *)
				!FailureQ[value = parseSingleListStylePosition[prop, tokens[[pos]]]],
					If[hasPos, 
						Return @ repeatedPropValueFailure @ "position"
						, 
						values["p"] = value[[p]]; hasPos = True
					],
					
				(* check for list-style-type *)
				!FailureQ[value = parseSingleListStyleType[prop, tokens[[pos]]]],
					If[hasType, 
						Return @ repeatedPropValueFailure @ "type"
						, 
						values["t"] = value[[p]]; hasType = True
					],
					
				(* anything else is an error *)
				True, Return @ unrecognizedValueFailure @ prop
			];
			advancePosAndSkipWhitespace[];
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
consumeProperty[prop:"quotes", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, v, values = {}, cPos},
		While[pos <= l,
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"none", 
							If[l > 1, 
								Return @ illegalIdentifierFailure @ CSSTokenString @ tokens[[pos]]
								, 
								values = {}
							],
						_, Return @ unrecognizedKeyWordFailure @ prop
					],
				"string",
					v = CSSTokenString @ tokens[[pos]]; cPos = pos; advancePosAndSkipWhitespace[];
					If[pos <= l && CSSTokenType @ tokens[[pos]] == "string", 
						AppendTo[values, {v, CSSTokenString @ tokens[[pos]]}];
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Expected pairs of strings."|>]
					],
				_, Return @ unrecognizedValueFailure @ prop
			];
			advancePosAndSkipWhitespace[]
		];
		Missing["Not supported."]
	]


(* ::Subsection::Closed:: *)
(*cursor*)


(* WL uses a TagBox[..., MouseAppearanceTag[""]] instead of an option to indicate mouse appearance *)
consumeProperty[prop:"cursor", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
				"uri", parseURI @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"display", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
						_,                    unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*float, clear*)


(* 
	FE does not support the flow of boxes around other boxes. 
	AttachedCell exists, but is too ephemeral. *)
consumeProperty[prop:"float", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"left",  Automatic,
						"right", Automatic,
						"none",  Automatic,
						_,       unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


consumeProperty[prop:"clear", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"left",    Automatic,
						"right",   Automatic,
						"both",    Automatic,
						"none",    Automatic,
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
consumeProperty[prop:"font", inputTokens:{__?CSSTokenQ}] :=
	Block[
		{
			pos = 1, l = Length[inputTokens], tokens = inputTokens, v, value, newValue = {}, temp,
			hasFontFamily = False, hasFontSize = False, hasFontStyle = False, hasFontVariant = False, hasFontWeight = False, hasLineHeight = False	
		},
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
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
					"caption" | "icon" | "menu" | "small-caption", 
						{FontFamily :> CurrentValue["ControlsFontFamily"], FontSize :> CurrentValue["ControlsFontSize"]},
					"message-box" | "status-bar", 
						{FontFamily :> CurrentValue["PanelFontFamily"], FontSize :> CurrentValue["PanelFontSize"]},
					"italic" | "oblique", {consumeProperty["font-style", tokens]},
					"small-caps",         {consumeProperty["font-variant", tokens]},
					_,                    unrecognizedKeyWordFailure @ prop
				];
			Return @ If[FailureQ[newValue], newValue, DeleteDuplicates[Join[newValue, value], SameQ[First[#1], First[#2]]&]]
		];
		
		(* 
			font-style, font-variant, and font-weight can appear in any order, but are optional.
			"normal" values are skipped since "normal" is the initial value of these properties.
			Besides "normal" the keywords of each property are unique.
		*)
		(* FIXME: could check that property is not duplicated like we do in e.g. border-top *)
		While[pos <= l && FailureQ[temp = consumeProperty["font-size", {tokens[[pos]]}]],
			If[!MatchQ[ToLowerCase @ CSSTokenString @ tokens[[pos]], "normal"],
				Which[
					(* font-style *)
					!FailureQ[v = consumeProperty["font-style", {tokens[[pos]]}]], 
						If[hasFontStyle, Return @ repeatedPropValueFailure @ "font-style"
							,
							hasFontStyle = True; AppendTo[newValue, v] 
						],
						
					(* font-weight *)
					!FailureQ[v = consumeProperty["font-weight", {tokens[[pos]]}]], 
						If[hasFontWeight, Return @ repeatedPropValueFailure @ "font-weight"
							,
							hasFontWeight = True; AppendTo[newValue, v] 
						],
						
					(* font-variant *)
					!FailureQ[v = consumeProperty["font-variant", {tokens[[pos]]}]], 
						If[hasFontVariant, Return @ repeatedPropValueFailure @ "font-variant"
							,
							hasFontVariant = True; AppendTo[newValue, v] 
						],
						
					True, Return @ unrecognizedValueFailure @ prop
				];
			];
			advancePosAndSkipWhitespace[];
		];
		
		(* 
			font-size must appear next; 
			an optional line-height property can immediately follow font-size with a '/' in between (no whitespace allowed) *)
		If[pos > l, Return @ noValueFailure["font-size"], AppendTo[newValue, temp]; pos++];
		
		(* check for optional line-height *)
		If[pos > l, Return @ noValueFailure["font-family"]];
		If[CSSTokenType @ tokens[[pos]] == "/",
			pos++; 
			temp = consumeProperty["line-height", {tokens[[pos]]}]; 
			If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]; advancePosAndSkipWhitespace[]];
		];
		
		(* everything else must be a font-family *)
		temp = consumeProperty["font-family", tokens[[pos ;;]]];
		If[FailureQ[temp], Return @ temp, AppendTo[newValue, temp]];
		
		(* overwrite old with any new values *)
		DeleteDuplicates[Join[newValue, value], SameQ[First[#1], First[#2]]&]
	]


(* ::Subsubsection::Closed:: *)
(*font-family*)


consumeProperty[prop:"font-family", inputTokens:{__?CSSTokenQ}] :=
	Module[{fontTokens, parsed, result},
		fontTokens = DeleteCases[SplitBy[inputTokens, ","], {","}];
		parsed = parseSingleFontFamily /@ fontTokens;
		result = FirstCase[parsed, _Failure, None]; (* FIXME: perhaps use FontSubstitutions here? *)
		If[FailureQ[result], Return @ result];
		FirstCase[parsed, _Rule, Failure["UnexpectedParse", <|"Message" -> "No font-family found."|>]]
	]

parseSingleFontFamily[inputTokens:{__?CSSTokenQ}] := parseSingleFontFamily[inputTokens] =
	Block[
	{
		value, l, pos = 1, font, tokensNoWS,
		generic = {"serif", "sans-serif", "monospace", "fantasy", "cursive"},
		fail = Failure["UnexpectedParse", <|"Message" -> "Font family syntax error."|>]
	},
		tokensNoWS = DeleteCases[inputTokens, " ", {1}];
		l = Length[tokensNoWS];
		value =
			Switch[CSSTokenType @ tokensNoWS[[pos]],
				"ident", (* if first token is an identifier, then all other tokens must be as well; there could only be a single 'ident' *)
					Which[
						!AllTrue[CSSTokenType /@ tokensNoWS, StringMatchQ["ident"]], fail,
						l == 1 && MemberQ[generic, ToLowerCase @ CSSTokenString @ tokensNoWS[[pos]]], parseFontFamilySingleIdent @ CSSTokenString @ tokensNoWS[[pos]],
						True, 
							font = StringJoin @ Riffle[CSSTokenString /@ tokensNoWS, " "];
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
					],
				"string", (* must only have a single string token up to the delimiting comma *)
					Which[
						l > 1, fail,
						True,
							font = CSSTokenString /@ tokensNoWS;
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
	Switch[CSSNormalizeEscapes @ s,
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
				MemberQ[$FontFamilies, "Source Code Pro"], "Source Code Pro",
				MemberQ[$FontFamilies, "Consolas"],        "Consolas",
				MemberQ[$FontFamilies, "Courier"],         "Courier",
				True, Automatic
			],
		_, unrecognizedKeyWordFailure @ "font-family"
	]


(* ::Subsubsection::Closed:: *)
(*font-size*)


consumeProperty[prop:"font-size", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"larger",   Larger,
						"smaller",  Smaller,
						"xx-small", Tiny (*6*),
						"x-small",  8,
						"small",    Small (*9*),
						"medium",   Medium (*12*),
						"large",    Large (*24*),
						"x-large",  30,
						"xx-large", 36,
						_,          unrecognizedKeyWordFailure @ prop
					],
				"dimension",  If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, parseLength[tokens[[pos]], True]],
				"percentage", If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, parsePercentage @ tokens[[pos]]],
				"number",     parseZero @ tokens[[pos]],
				_,            unrecognizedValueFailure @ prop 
			];
		If[FailureQ[value], value, FontSize -> value]
	]


(* ::Subsubsection::Closed:: *)
(*font-style*)


consumeProperty[prop:"font-style", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


consumeProperty[prop:"font-variant", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"font-weight", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
							Clip[CSSTokenValue @ tokens[[pos]], {1, 1000}]], 
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
parseSingleSize[prop_String, token_?CSSTokenQ] := parseSingleSize[prop, token] =
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"auto", Automatic, (* let Mathematica decide what to do *)
				"none", If[!StringMatchQ[prop, "max-height" | "max-width"], unrecognizedKeyWordFailure @ prop, Infinity],
				_,      unrecognizedKeyWordFailure @ prop
			],
		"dimension",  If[CSSTokenValue @ token < 0, negativeLengthFailure @ prop, parseLength @ token],
		"number",     parseZero @ token,
		"percentage", parsePercentage @ token, (* should be percentage of height of containing block; not possible in WL *)
		_,            unrecognizedValueFailure @ prop
	]
	
(* min-width and max-width override width property *)
consumeProperty[prop:"width" | "max-width" | "min-width", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"width",     {CSSWidthMin[value], CSSWidthMax[value]},
					"max-width", CSSWidthMax[value],
					"min-width", CSSWidthMin[value]
				]
		]
	]

consumeProperty[prop:"height" | "max-height" | "min-height", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			ImageSize -> 
				Switch[prop,
					"height",     {CSSHeightMin[value], CSSHeightMax[value]},
					"max-height", CSSHeightMax[value],
					"min-height", CSSHeightMin[value]
				]
		]
	]


(* ::Subsection::Closed:: *)
(*line-height*)


(* 
	Similar to WL LineSpacing, but LineSpacing already takes FontSize into account.
	Thus, we intercept 'em' and 'ex' before getting a dynamic FontSize and we keep 
	the percentage from being wrapped in Scaled. *)
consumeProperty[prop:"line-height", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"normal", {1.2, 0},
						_,        unrecognizedKeyWordFailure @ prop
					],
				"dimension", 
					If[CSSTokenValue @ tokens[[pos]] < 0, 
						negativeLengthFailure @ prop
						,
						Switch[CSSDimensionUnit @ tokens[[pos]],
							"em"|"ex", {parseLengthNonRelative @ tokens[[pos]], 0},
							_,         {parseLength @ tokens[[pos]], 0}
						]
					],
				"number",     If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, {CSSTokenValue @ tokens[[pos]], 0}],
				"percentage", If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, {(CSSTokenValue @ tokens[[pos]])/100, 0}],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, LineSpacing -> value]
	]


(* ::Subsection::Closed:: *)
(*margin(-left, -right, -top, -bottom)*)


parseSingleMargin[prop_String, token_?CSSTokenQ] := parseSingleMargin[prop, token] = 
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"auto", Automatic, (* let FE decide what to do *)
				_,      unrecognizedKeyWordFailure @ prop
			],
		"dimension",  parseLength @ token,
		"number",     CSSTokenValue @ token,
		"percentage", parsePercentage @ token,
		_,            unrecognizedValueFailure @ prop
	]
	
(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
consumeProperty[prop:"margin-top" | "margin-right" | "margin-bottom" | "margin-left", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleMargin[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "margin-left", Left, "margin-right", Right, "margin-bottom", Bottom, "margin-top", Top];
			{ImageMargins -> wrapper[value], CellMargins -> wrapper[value]}
		]
	]
		
consumeProperty[prop:"margin", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value, results = {}},
		While[pos <= l,
			value = parseSingleMargin[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			advancePosAndSkipWhitespace[];
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


consumeProperty[prop:"outline-color", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[CSSTokenString @ tokens[[pos]],
						"invert", CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], ColorNegate @ CurrentValue[CellFrameColor], Inherited]],
						_,        unrecognizedKeyWordFailure @ prop
					],
				"function",
					Switch[CSSTokenString @ tokens[[pos]],
						"rgb" | "rgba" | "hsl" | "hsla", parseSingleColor[prop, tokens[[pos]]],
						_,                               invalidFunctionFailure @ CSSTokenString @ tokens[[pos]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-style*)


(* only a solid border is allowed for cells; 'hidden' is not allowed here *)
consumeProperty[prop:"outline-style", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value =
			If[CSSTokenType @ tokens[[pos]] == "ident" && CSSTokenString @ tokens[[pos]] == "hidden",
				unrecognizedKeyWordFailure @ prop
			,
				parseSingleBorderStyle[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-width*)


consumeProperty[prop:"outline-width", inputTokens:{__?CSSTokenQ}] := 
	Block[{pos = 1, l = Length[inputTokens], tokens = inputTokens, value},
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
consumeProperty[prop:"outline", inputTokens:{__?CSSTokenQ}] := 
	Block[
	{
		pos = 1, l = Length[inputTokens], tokens = inputTokens, value, 
		values = <|
			"c" -> initialValues[prop <> "-color"], 
			"s" -> initialValues[prop <> "-style"], 
			"w" -> initialValues[prop <> "-width"]|>,
		hasColor = False, hasStyle = False, hasWidth = False
	},
		While[pos <= l,
			Which[
				CSSTokenType @ tokens[[pos]] == "function", (* only color can be a function *)
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")]; 
					hasColor = True; values["c"] = parseSingleColor[prop, consumeFunction[pos, l, tokens]],
					
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]],
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
			advancePosAndSkipWhitespace[];
		];
		Missing["Not supported."]
	]


(* ::Subsection::Closed:: *)
(*overflow*)


(* 
	This would mostly be found with WL Pane expression as PaneBox supports scrollbars. 
	Other boxes may support ImageSizeAction.
*)
consumeProperty[prop:"overflow", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[CSSTokenType @ tokens[[pos]],
			"ident",
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


parseSinglePadding[prop_String, token_?CSSTokenQ] := parseSinglePadding[prop, token] = 
	Switch[CSSTokenType @ token,
		"ident", 
			Switch[ToLowerCase @ CSSTokenString @ token,
				"initial", initialValues @ prop,
				"inherit", Inherited,
				_,         unrecognizedKeyWordFailure @ prop
			],
		"dimension",  If[CSSTokenValue @ token < 0, negativeLengthFailure @ prop, parseLength @ token],
		"percentage", If[CSSTokenValue @ token < 0, negativeLengthFailure @ prop, parsePercentage @ token],
		_,            unrecognizedValueFailure @ prop
	]


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
consumeProperty[prop:"padding-top"|"padding-right"|"padding-bottom"|"padding-left", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{l = Length[tokens], wrapper, value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSinglePadding[prop, tokens[[1]]];
		If[FailureQ[value], 
			value
			, 
			wrapper = Switch[prop, "padding-left", Left, "padding-right", Right, "padding-bottom", Bottom, "padding-top", Top];
			{FrameMargins -> wrapper[value], CellFrameMargins -> wrapper[value]}
		]
	]
		
consumeProperty[prop:"padding", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = parseSinglePadding[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			advancePosAndSkipWhitespace[];
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
consumeProperty[prop:"orphans"|"widows", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"number", 
					If[CSSTokenValue @ tokens[[pos]] < 1 || CSSTokenValueType @ tokens[[pos]] != "integer", 
						positiveLengthFailure @ prop
						, 
						CSSTokenValue @ tokens[[pos]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-after/before*)


consumeProperty[prop:("page-break-after"|"page-break-before"), inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


consumeProperty[prop:"page-break-inside", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{value, pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"position", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


consumeProperty[prop:"left"|"right"|"top"|"bottom", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"caption-side", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"empty-cells", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"table-layout", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


consumeProperty[prop:"direction", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"text-align", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[CSSTokenType @ tokens[[pos]],
			"ident",
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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


consumeProperty[prop:"text-indent", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"inherit", Inherited,
						"initial", initialValues @ prop,
						_,         unrecognizedKeyWordFailure @ prop
					],
				"dimension",
					Switch[CSSDimensionUnit @ tokens[[pos]],
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
consumeProperty[prop:"text-decoration", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value =
				Switch[CSSTokenType @ tokens[[pos]],
					"ident",
						Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
			advancePosAndSkipWhitespace[];
		];
		FontVariations -> values
	]


(* ::Subsubsection::Closed:: *)
(*text-transform*)


consumeProperty[prop:"text-transform", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[CSSTokenType @ tokens[[pos]],
			"ident",
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"letter-spacing", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"font-stretch", inputTokens:{__?CSSTokenQ}] := 
	Block[{value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[1, 1]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[1]],
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
				"percentage", If[CSSTokenValue @ tokens[[1]] < 0, negativeValueFailure @ prop, (CSSTokenValue @ n)/100],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]*)


(* ::Subsubsection::Closed:: *)
(*unicode-bidi*)


consumeProperty[prop:"unicode-bidi", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"word-spacing", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"white-space", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[CSSTokenType @ tokens[[pos]],
			"ident",
				Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"vertical-align", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{value1, value2},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value1 = parseBaseline[prop, First @ tokens];
		If[FailureQ[value1], Return @ value1];
		value2 = parseCellBaseline[prop, First @ tokens];
		If[FailureQ[value2], Return @ value2];
		{value1, value2}
	]

(* this is effectively for RowBox alignment *)
parseBaseline[prop:"vertical-align", token_?CSSTokenQ] := parseBaseline[prop, token] = 
	Block[{value (* for Baseline *)},
		(* tooManyTokens failure check occurs in higher-level function consumeProperty["vertical-align",...]*)
		value = 
			Switch[CSSTokenType @ token,
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ token,
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
					Switch[CSSDimensionUnit @ token,
						"em"|"ex", Baseline -> Scaled[parseLengthNonRelative @ token],
						_,         Baseline -> With[{v = parseLength @ token}, Scaled @ Dynamic[v/CurrentValue[FontSize]]]
					],
				"percentage", Baseline -> parsePercentage @ token,
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, BaselinePosition -> value]
	]

(* it's unfortunate that CellBaseline is so limited *)
parseCellBaseline[prop:"vertical-align", token_?CSSTokenQ] := parseCellBaseline[prop, token] = 
	Block[{value},
		value = 
			Switch[CSSTokenType @ token,
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ token,
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
					Switch[CSSDimensionUnit @ token,
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
consumeProperty[prop:"visibility", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
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
consumeProperty[prop:"z-index", inputTokens:{__?CSSTokenQ}] := (*consumeProperty[prop, tokens] = *)
	Block[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident",
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"auto",     Automatic,
						"inherit",  Inherited,
						"initial",  initialValues @ prop,
						_,          unrecognizedKeyWordFailure @ prop
					],
				"number", 
					If[CSSTokenValueType @ tokens[[pos]] != "integer", 
						Failure["BadValue", <|"Message" -> "Expected value is an integer."|>]
						, 
						CSSTokenValue @ tokens[[pos]]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*FALL THROUGH *)


consumeProperty[prop_String, {}] := noValueFailure @ prop

consumeProperty[prop_String, _] := unsupportedValueFailure @ prop


(* ::Section::Closed:: *)
(*Consume Token Sequences*)


(* ::Subsection::Closed:: *)
(*Utilities*)


(* The utilities are assumed to be used within "consume" functions where pos, l, and tokens are defined. *)
tokenTypeIs[s:(_String | Alternatives[__String])] := StringMatchQ[CSSTokenType @ tokens[[pos]], s, IgnoreCase -> False]
tokenStringIs[s_String] := StringMatchQ[CSSTokenString @ tokens[[pos]], s, IgnoreCase -> True]

advancePosAndSkipWhitespace[] := (pos++; While[pos < l && CSSTokenType @ tokens[[pos]] == " ", pos++])
retreatPosAndSkipWhitespace[] := (pos--; While[pos > 1 && CSSTokenType @ tokens[[pos]] == " ", pos--])

advancePosToNextSemicolon[] := While[pos < l && CSSTokenType @ tokens[[pos]] != ";", pos++]
advancePosToNextSemicolonOrBlock[] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "{}" | ";"], pos++]
advancePosToNextSemicolonOrComma[] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "," | ";"], pos++]
advancePosToNextSemicolonOrExclamation[] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "!" | ";"], pos++]

advancePosToNextBlock[] := While[pos < l && !MatchQ[CSSTokenType @ tokens[[pos]], "{}"], pos++]


(* ::Subsection::Closed:: *)
(*Consume Style Sheet*)


(* Block is used such that the private variables pos, l, and tokens are known by any token consumer. *)
consumeStyleSheet[s_String] :=
	Block[{pos, l, tokens, imports = {}, i = 1, lRulesets, rulesets},
		tokens = CSSTokenize @ s;
		pos = 1; l = Length[tokens];
		Echo[l, "Token Length"];
		
		(* skip any leading whitespace (there shouldn't be any if @charset exists) *)
		If[tokenTypeIs[" "], advancePosAndSkipWhitespace[]];
		
		(* check for @charset rule *)
		If[tokenTypeIs["at-keyword"] && tokenStringIs["charset"], consumeAtCharsetKeyword[]];
		
		(* check for @import rules *)
		While[tokenTypeIs["at-keyword"] && tokenStringIs["import"], AppendTo[imports, consumeAtImportKeyword[]];];
		imports = Join @@ imports;
				
		lRulesets = Count[tokens, {"{}", ___}]; (* upper bound of possible rulesets *)
		rulesets = ConstantArray[0, lRulesets]; (* container for processed rulesets *)
		While[pos < l,
			Which[
				(* any at-rule *)
				tokenTypeIs["at-keyword"], (*TODO*)consumeAtRule[CSSTokenString @ tokens[[pos]]],
				
				(* bad ruleset: missing a selector *)
				tokenTypeIs["{}"], advancePosAndSkipWhitespace[], 
				
				(* anything else treated as a ruleset *)
				True, rulesets[[i]] = consumeRuleset[]; i++;
			];
		];
		{pos, l, tokens[[pos ;;]], imports, DeleteCases[rulesets, 0, {1}]}
	]


(* ::Subsection::Closed:: *)
(*Consume Style Sheet Preambles (charset, import)*)


(* The character set is assumed UTF-8 and any charset is ignored. *)
consumeAtCharsetKeyword[] :=
	Module[{},
		If[!tokenTypeIs["at-keyword"] || !tokenStringIs["charset"],
			Echo[Row[{"Expected @charset keyword. Had instead ", tokens[[pos]]}], "@charset error"];
			advancePosToNextSemicolon[]; advancePosAndSkipWhitespace[]; 
			Return @ Null;
		];
		pos++;
		If[MatchQ[tokens[[pos ;; pos + 2]], {" ", {"string", _}, ";"}],
			pos = pos + 3
			,
			(* invalid @charset *)
			advancePosToNextSemicolon[];
		];
		advancePosAndSkipWhitespace[];
	]; 


consumeAtImportKeyword[] :=  
	Module[{path, mediums, mediaStart, data},
		If[!tokenTypeIs["at-keyword"] || !tokenStringIs["import"],
			Echo[Row[{"Expected @import keyword. Had instead ", tokens[[pos]]}], "@import error"];
			advancePosToNextSemicolon[]; advancePosAndSkipWhitespace[]; Return @ {};
		];
		advancePosAndSkipWhitespace[];
		(* next token must be URL or string path to file *)
		If[!tokenTypeIs["url" | "string"],
			Echo["Expected URL not found.", "@import error"];
			advancePosToNextSemicolon[]; advancePosAndSkipWhitespace[]; Return @ {};
		];
		path = CSSTokenString @ tokens[[pos]];
		advancePosAndSkipWhitespace[]; 	
		
		(* anything else is a comma-delimited set of media queries *)
		(*TODO: implement proper media queries *)
		mediums = {};
		While[!tokenTypeIs[";"],
			mediaStart = pos;
			advancePosToNextSemicolonOrComma[];
			If[pos == l, Echo["Media query has no closing. Reached EOF.", "@import error"]; Return @ {}];
			AppendTo[mediums, CSSUntokenize @ tokens[[mediaStart, pos - 1]]];
			If[tokenTypeIs[";"],
				(* break out of media loop*)
				Break[] 
				, 
				(* skip comma only *)
				advancePosAndSkipWhitespace[] 
			]
		];
		advancePosAndSkipWhitespace[]; (* skip semicolon *)
				
		(* import without interpretation *)
		data = Import[Echo[path, "@import"], "Text"];
		If[FailureQ[data],
			Return @ {}
			, 
			data = consumeStyleSheet[data];
			If[mediums =!= {}, data[[All, "Condition"]] = ConstantArray[mediums, Length[data]]];
			Return @ data
		]
	]
	
	
(* ::Subsection::Closed:: *)
(*Consume Style Sheet Body (@rule, ruleset)*)


consumeAtRule[type_String] :=
	Which[
		(* @import not allowed so skip them *)
		tokenStringIs["import"], advancePosToNextSemicolon[]; advancePosAndSkipWhitespace[], 
			
		(* @page *)
		tokenStringIs["page"], 
			Null,
			
		(* @media *)
		tokenStringIs["media"], 
			Null,
			
		(* unrecognized @rule *)
		True, 
			Null]


consumeRuleset[] :=
	Module[{selectorStartPos = pos, ruleset},
		advancePosToNextBlock[];
		ruleset = 
			<|
				"Selector" -> StringTrim @ CSSUntokenize @ tokens[[selectorStartPos ;; pos - 1]], 
				"Condition" -> None,
				(* The block token is already encapsulated {{}, CSSTokens...} *)
				"Block" -> consumeDeclarationBlock @  If[Length[tokens[[pos]]] > 1, tokens[[pos, 2 ;; ]], {}]|>; 
		advancePosAndSkipWhitespace[];
		ruleset
	]

consumeDeclarationBlock[{}] := {} 

consumeDeclarationBlock[inputTokens:{__?CSSTokenQ}] :=
	Block[{pos, l, tokens = inputTokens, lDeclarations, i = 1, decStart, dec, validDeclarations},
		pos = 1; l = Length[tokens];
		
		(* skip any initial whitespace *)
		If[tokenTypeIs[" "], advancePosAndSkipWhitespace[]]; 
		
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[tokens, ":"];
		validDeclarations = ConstantArray[0, lDeclarations];
		While[pos < l && i <= lDeclarations,
			decStart = pos; advancePosToNextSemicolon[];
			dec = consumeDeclaration[tokens[[decStart ;; pos]]];
			If[!FailureQ[dec], validDeclarations[[i++]] = dec];
			advancePosAndSkipWhitespace[]
		];					
		(* remove possible excess declarations *)
		DeleteCases[validDeclarations, 0, {1}]
	]
	
(* a declaration is prop:val or prop:val !important with optional semicolon if it is the last declaration *)
consumeDeclaration[inputTokens:{__?CSSTokenQ}] :=
	Block[{pos, l, tokens = inputTokens, propertyPosition, valuePosition, important = False, declaration},
		pos = 1; l = Length[tokens];
		
		(* check for bad property *)
		If[!tokenTypeIs["ident"], Return @ $Failed];
		propertyPosition = pos; advancePosAndSkipWhitespace[];
		
		(* check for EOF or missing colon *)
		If[pos >= l  || !tokenTypeIs[":"], Return @ $Failed];
		advancePosAndSkipWhitespace[]; 
		valuePosition = pos;
		
		(* remove trailing whitespace *)
		pos = l;
		If[tokenTypeIs[";"], 
			retreatPosAndSkipWhitespace[]
			,
			While[pos > 1 && tokenTypeIs[" "], pos--]
		];
		
		(* check for !important token sequence *)
		If[tokenTypeIs["ident"] && tokenStringIs["important"], 
			retreatPosAndSkipWhitespace[];
			If[tokenTypeIs["!"], important = True; retreatPosAndSkipWhitespace[]]];
		
		declaration = <|
			"Important" -> important,
			"Property" -> CSSNormalizeEscapes @ ToLowerCase @ CSSTokenString @ tokens[[propertyPosition]], 
			"Value" -> (*check for empty property*)If[pos < valuePosition, {}, tokens[[valuePosition ;; pos]]],
			"Interpretation" -> None|>;
		advancePosAndSkipWhitespace[];
		declaration		
	]


(* ::Subsection::Closed:: *)
(*Process Properties*)


process[property_String, a:{__Association}] :=
	Module[{valuePositions, interpretationPositions, tokens, values, interpretations},
		{valuePositions, interpretationPositions} = getPropertyPositions[property, a];
		tokens = Extract[a, valuePositions];
		{values, interpretations} = {StringJoin /@ tokens[[All, All, 2]], consumeProperty[property, #]& /@ tokens};
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
					Bottom | CSSHeightMin, r[[2, 1]] = v,
					Top |    CSSHeightMax, r[[2, 2]] = v,
					Left |   CSSWidthMin,  r[[1, 1]] = v,
					Right |  CSSWidthMax,  r[[1, 2]] = v]
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
