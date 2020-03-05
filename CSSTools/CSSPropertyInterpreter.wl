(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSPropertyInterpreter`", {"CSSTools`"}]
(* Exported symbols added here with SymbolName::usage *)  

Needs["CSSTools`CSSTokenizer`"]; (* keep tokenizer utilities hidden *)

(* functions to expose to outside packages via Needs *)
CSSPropertyData;
consumeProperty;
tooManyTokensFailure;
unrecognizedKeyWordFailure;
unrecognizedValueFailure;
parseAngle;
parseCounter;
parseFrequency;
parseLength;
parseLengthNonRelative;
parseNumber;
parsePercentage;
parseResolution;
parseSingleColor;
parseTime;
parseURI;


Begin["`Private`"] (* Begin Private Context *) 

(* ::Section::Closed:: *)
(*Properties*)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
(*Consume Properties*)


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
tooManyTokensFailure[tokens_List] :=       Failure["UnexpectedParse", <|"Message" -> "Too many tokens.", "Tokens" -> Through[tokens["Type"]]|>];
unrecognizedKeyWordFailure[prop_String] := Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " keyword."|>];
unrecognizedValueFailure[prop_String] :=   Failure["UnexpectedParse", <|"Message" -> "Unrecognized " <> prop <> " value."|>];
unsupportedValueFailure[prop_String] :=    Failure["UnsupportedProp", <|"Property" -> prop|>]


Options[consumeProperty] = {"Namespaces" -> {}};


(* ::Subsection::Closed:: *)
(*Property Data Table*)


(* 
	Some of these are shorthand properties that set one or more other properties. 
	As such, the shorthand initial values would never be directly required.
	The 'unset' keyword takes on the 
		'initial' value if "Inherited" -> False, or
		'inherit' value if "Inherited" -> True.
	AssociateTo is used such that other packages can extend this symbol regardless of load order.
*)
If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, {
	"background" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A",  (* shorthand property for background-attachment/color/image/position/repeat *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "background-attachment", "Value" -> "inherit", "Interpretation" -> Missing["Not supported."]|>, 
				<|"Property" -> "background-color",      "Value" -> "inherit", "Interpretation" -> <|Background -> Inherited|>|>, 
				<|"Property" -> "background-image",      "Value" -> "inherit", "Interpretation" -> <|System`BackgroundAppearance -> Inherited|>|>, 
				<|"Property" -> "background-position",   "Value" -> "inherit", "Interpretation" -> <|System`BackgroundAppearanceOptions -> Inherited|>|>, 
				<|"Property" -> "background-repeat",     "Value" -> "inherit", "Interpretation" -> <|System`BackgroundAppearanceOptions -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "background-attachment", "Value" -> "initial", "Interpretation" -> Missing["Not supported."]|>, 
				<|"Property" -> "background-color",      "Value" -> "initial", "Interpretation" -> <|Background -> None|>|>, 
				<|"Property" -> "background-image",      "Value" -> "initial", "Interpretation" -> <|System`BackgroundAppearance -> None|>|>, 
				<|"Property" -> "background-position",   "Value" -> "initial", "Interpretation" -> <|System`BackgroundAppearanceOptions -> "NoRepeat"|>|>, 
				<|"Property" -> "background-repeat",     "Value" -> "initial", "Interpretation" -> <|System`BackgroundAppearanceOptions -> "Repeat"|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"scroll", "fixed", "<color>", "transparent", "<funciri>", "none", "<percentage>", "<length>", "left", "right", "center", "top", "bottom", "repeat", "repeat-x", "repeat-y", "no-repeat"},
		"AppliesTo" -> All|>,
	"background-attachment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "scroll",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"scroll", "fixed"},
		"AppliesTo" -> All|>,
	"background-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "transparent",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Background -> Inherited|>,
			"initial" -> <|Background -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"background-image" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|System`BackgroundAppearance -> Inherited|>,
			"initial" -> <|System`BackgroundAppearance -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"<funciri>", "none"},
		"AppliesTo" -> All|>,
	"background-position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0% 0%",
		"InterpretedGlobalValues" -> <|
			(* Technically WL value should be {0,0}, but FE only supports keywords. *)
			"inherit" -> <|System`BackgroundAppearanceOptions -> Inherited|>,
			"initial" -> <|System`BackgroundAppearanceOptions -> "NoRepeat"|>|>,
		"Animatable" -> True,
		"Values" -> {"<percentage>", "<length>", "left", "right", "center", "top", "bottom"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"background-repeat" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "repeat",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|System`BackgroundAppearanceOptions -> Inherited|>,
			"initial" -> <|System`BackgroundAppearanceOptions -> "Repeat"|>|>,
		"Animatable" -> False,
		"Values" -> {"repeat", "repeat-x", "repeat-y", "no-repeat"},
		"AppliesTo" -> All|>,
	"border-collapse" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "separate",
		"InterpretedGlobalValues" -> <|
			(* FE only follows the 'collapse' model within Grid but does not provide a modifiable option. *)
			"inherit" -> <||>,
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"collapse", "separate"},
		"AppliesTo" -> {"table", "inline-table"}|>,
	"border-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* shorthand property, sets all 4 border sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-left-color",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-right-color",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-bottom-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-top-color",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>}, 
			"initial" -> {
				<|"Property" -> "border-left-color",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-right-color",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-bottom-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-top-color",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"border-top-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> Dynamic @ CurrentValue[FontColor], 
				CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"border-right-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> Dynamic @ CurrentValue[FontColor], 
				CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"border-bottom-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> Dynamic @ CurrentValue[FontColor], 
				CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"border-left-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor", (* value of 'color' property*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> Dynamic @ CurrentValue[FontColor], 
				CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "transparent"},
		"AppliesTo" -> All|>,
	"border-style" -> <| (* AKA dashing *)
		"Inherited" -> False,
		"CSSInitialValue" -> "none", (* shorthand property, sets all 4 sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-left-style",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-right-style",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-top-style",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-left-style",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-right-style",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-top-style",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>}|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"border-top-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> None, 
				CellFrameStyle -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"border-right-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> None, 
				CellFrameStyle -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"border-bottom-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> None, 
				CellFrameStyle -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"border-left-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle     -> Inherited, 
				CellFrameStyle -> Inherited|>,
			"initial" -> <|
				FrameStyle     -> None, 
				CellFrameStyle -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"border-width" -> <| (* AKA thickness *)
		"Inherited" -> False,
		"CSSInitialValue" -> "medium", (* shorthand property, sets all 4 border sides *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-left-width",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-right-width",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-bottom-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-top-width",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-left-width",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-right-width",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-bottom-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-top-width",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
    	"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-top-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle -> Inherited, 
				CellFrame  -> Inherited|>,
			"initial" -> <|
				FrameStyle -> Thickness[Medium], 
				CellFrame  -> 2|>|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-right-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle -> Inherited, 
				CellFrame  -> Inherited|>,
			"initial" -> <|
				FrameStyle -> Thickness[Medium], 
				CellFrame  -> 2|>|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-bottom-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle -> Inherited, 
				CellFrame  -> Inherited|>,
			"initial" -> <|
				FrameStyle -> Thickness[Medium], 
				CellFrame  -> 2|>|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-left-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameStyle -> Inherited, 
				CellFrame  -> Inherited|>,
			"initial" -> <|
				FrameStyle -> Thickness[Medium], 
				CellFrame  -> 2|>|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand property, sets all 4 border sides color/style/width*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-left-color",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-right-color",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-bottom-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-top-color",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-left-style",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-right-style",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-top-style",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>,
				<|"Property" -> "border-left-width",   "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-right-width",  "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-bottom-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>, 
				<|"Property" -> "border-top-width",    "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-left-color",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-right-color",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-bottom-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>, 
				<|"Property" -> "border-top-color",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		 		<|"Property" -> "border-left-style",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-right-style",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>, 
				<|"Property" -> "border-top-style",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>,
				<|"Property" -> "border-left-width",   "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-right-width",  "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-bottom-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>, 
				<|"Property" -> "border-top-width",    "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "<color>", "transparent"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-top-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-top-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>,
				<|"Property" -> "border-top-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-top-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		 		<|"Property" -> "border-top-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>,
				<|"Property" -> "border-top-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "<color>", "transparent"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-right-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-right-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>,
				<|"Property" -> "border-right-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-right-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		 		<|"Property" -> "border-right-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>,
				<|"Property" -> "border-right-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "<color>", "transparent"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-bottom-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>,
				<|"Property" -> "border-bottom-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-bottom-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		 		<|"Property" -> "border-bottom-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>,
				<|"Property" -> "border-bottom-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "<color>", "transparent"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "currentColor none medium", (* shorthand border-top sets color/style/width *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "border-left-color", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>, 
				<|"Property" -> "border-left-style", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrameStyle -> Inherited|>|>,
				<|"Property" -> "border-left-width", "Value" -> "inherit", "Interpretation" -> <|FrameStyle -> Inherited, CellFrame -> Inherited|>|>},
			"initial" -> {
				<|"Property" -> "border-left-color", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Dynamic @ CurrentValue[FontColor], CellFrameStyle -> Dynamic @ CurrentValue[FontColor]|>|>,
		 		<|"Property" -> "border-left-style", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> None, CellFrameStyle -> None|>|>,
				<|"Property" -> "border-left-width", "Value" -> "initial", "Interpretation" -> <|FrameStyle -> Thickness[Medium], CellFrame -> 2|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "<color>", "transparent"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"border-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Spacings -> Inherited|>,
			"initial" -> <|Spacings -> 0|>|>,
		"Animatable" -> False,
		"Values" -> {"<length>"},
		"AppliesTo" -> {"table", "inline-table"},
		"NumericThreshold" -> 0|>,
	"bottom" -> <|(* no equivalent FE option *)
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Alignment -> {Automatic, Inherited}|>, 
			"initial" -> <|Alignment -> {Automatic, Automatic}|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "<length>", "<percentage>"},
		"AppliesTo" -> {"positioned elements"},
		"NumericThreshold" -> -Infinity|>,
	"caption-side" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "top",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"top", "bottom"},
		"AppliesTo" -> {"table-caption elements"}|>,
	"clear" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"none", "left", "right", "both"},
		"AppliesTo" -> {"block-level elements"}|>,		
	"clip" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"<shape>", "auto"},
		"AppliesTo" -> {"absolutely positioned elements"}|>,
	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontColor -> Inherited|>,
			"initial" -> <|FontColor -> Black|>|>,(* no set CSS specification|>, so use reasonable setting *)
		"Animatable" -> True,
		"Values" -> {"<color>"},
		"AppliesTo" -> All|>,
	"content" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|DisplayFunction -> Inherited|>,
			"initial" -> <|DisplayFunction -> Function[Identity]|>|>,
		"Animatable" -> False,
		"Values" -> {"normal", "none", "<string>", "<funciri>", "<counter>", "attr()", "open-quote", "close-quote", "no-open-quote", "no-close-quote"},
		"AppliesTo" -> {":before", ":after"}|>,
	"counter-increment" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|CounterIncrements -> Inherited|>,
			"initial" -> <|CounterIncrements -> {}|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "<identifier>", "<integer>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"counter-reset" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
		
			"inherit" -> <|CounterAssignments -> Inherited|>,
			"initial" -> <|CounterAssignments -> {}|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "<identifier>", "<integer>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"cursor" -> <|(* no FE option to control mouse appearance *)
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <|"MouseAppearance" -> (TagBox[#, MouseAppearanceTag["Arrow"]]&)|>|>,
		"Animatable" -> False,
		"Values" -> {"<funciri>", "auto", "crosshair", "default", "pointer", "move", "e-resize", "ne-resize", "nw-resize", "n-resize", "se-resize", "sw-resize", "s-resize", "w-resize", "text", "wait", "help", "progress"},
		"AppliesTo" -> All|>,
	"direction" -> <|(* so far FE only has left-to-right *)
		"Inherited" -> True,
		"CSSInitialValue" -> "ltr",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"ltr", "rtl"},
		"AppliesTo" -> All|>,
	"display" -> <|(* can set Style[_, Visible->False]*)
		"Inherited" -> False,
		"CSSInitialValue" -> "inline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"inline", "block", "list-item", "inline-block", "table", "inline-table", "table-row-group", "table-header-group", "table-footer-group", "table-row", "table-column-group", "table-column", "table-cell", "table-caption", "none"},
		"AppliesTo" -> All|>,
	"empty-cells" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "show",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"show", "hide"},
		"AppliesTo" -> {"table-cell elements"}|>,
	"float" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"left", "right", "none"},
		"AppliesTo" -> All|>,
	"font" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "font-family",  "Value" -> "inherit", "Interpretation" -> <|FontFamily     -> Inherited|>|>,
				<|"Property" -> "font-size",    "Value" -> "inherit", "Interpretation" -> <|FontSize       -> Inherited|>|>,
				<|"Property" -> "font-style",   "Value" -> "inherit", "Interpretation" -> <|FontSlant      -> Inherited|>|>,
				<|"Property" -> "font-variant", "Value" -> "inherit", "Interpretation" -> <|FontVariations -> <|"CapsType" -> Inherited|>|>|>,
				<|"Property" -> "font-weight",  "Value" -> "inherit", "Interpretation" -> <|FontWeight     -> Inherited|>|>,
				<|"Property" -> "line-height",  "Value" -> "inherit", "Interpretation" -> <|LineSpacing    -> Inherited|>|>}, 
			"initial" -> {
				<|"Property" -> "font-family",  "Value" -> "initial", "Interpretation" -> <|FontFamily     -> "Arial"|>|>,
				<|"Property" -> "font-size",    "Value" -> "initial", "Interpretation" -> <|FontSize       -> Medium|>|>,
				<|"Property" -> "font-style",   "Value" -> "initial", "Interpretation" -> <|FontSlant      -> Plain|>|>,
				<|"Property" -> "font-variant", "Value" -> "initial", "Interpretation" -> <|FontVariations -> <|"CapsType" -> "Normal"|>|>|>,
				<|"Property" -> "font-weight",  "Value" -> "initial", "Interpretation" -> <|FontWeight     -> Plain|>|>,
				<|"Property" -> "line-height",  "Value" -> "initial", "Interpretation" -> <|LineSpacing    -> {1.2, 0}|>|>}|>,
		"Animatable" -> True,
		"Values" -> {"<family-name>", "<length>", "<percentage>", "normal", "italic", "oblique", "bold", "bolder", "lighter", "100", "200", "300", "400", "500", "600", "700", "800", "900", "small-caps", "caption", "icon", "menu", "message-box", "small-caption", "status-bar", "serif", "sans-serif", "cursive", "fantasy", "monospace", "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "smaller", "larger"},
		"AppliesTo" -> All|>,
	"font-family" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "arial", (* depends on user agent aka WD, we choose "Arial" *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontFamily -> Inherited|>,
			"initial" -> <|FontFamily -> "Arial"|>|>,
		"Animatable" -> False,
		"Values" -> {"<family-name>"},
		"AppliesTo" -> All|>,
	"font-size" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontSize -> Inherited|>,
			"initial" -> <|FontSize -> Medium|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "smaller", "larger"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"font-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontSlant -> Inherited|>,
			"initial" -> <|FontSlant -> Plain|>|>,
		"Animatable" -> False,
		"Values" -> {"normal", "italic", "oblique"},
		"AppliesTo" -> All|>,
	"font-variant" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontVariations -> <|"CapsType" -> Inherited|>|>,
			"initial" -> <|FontVariations -> <|"CapsType" -> "Normal"|>|>|>,
		"Animatable" -> False,
		"Values" -> {"normal", "small-caps"},
		"AppliesTo" -> All|>,
	"font-weight" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontWeight -> Inherited|>,
			"initial" -> <|FontWeight -> Plain|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "bold", "bolder", "lighter", "100", "200", "300", "400", "500", "600", "700", "800", "900"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 100|>,
	"height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Height" -> <|"Min" -> Inherited, "Max" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Height" -> <|"Min" -> Automatic, "Max" -> Automatic|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Alignment -> {Inherited, Automatic}|>,
			"initial" -> <|Alignment -> {Automatic, Automatic}|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> {"positioned elements"},
		"NumericThreshold" -> -Infinity|>,
	"letter-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontTracking -> Inherited|>,
			"initial" -> <|FontTracking -> "Plain"|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"line-height" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|LineSpacing -> Inherited|>,
			"initial" -> <|LineSpacing -> {1.2, 0}|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "<number>", "<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"list-style" -> <|(* short-hand for list-style-image/position/type *)
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* shorthand property *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "list-style-image",    "Value" -> "inherit", "Interpretation" -> <|CellDingbat -> Inherited|>|>,
				<|"Property" -> "list-style-position", "Value" -> "inherit", "Interpretation" -> Missing["Not supported."]|>,
				<|"Property" -> "list-style-type",     "Value" -> "inherit", "Interpretation" -> <|CellDingbat -> Inherited|>|>}, 
			"initial" -> {
				<|"Property" -> "list-style-image",    "Value" -> "initial", "Interpretation" -> <|CellDingbat -> None|>|>,
				<|"Property" -> "list-style-position", "Value" -> "initial", "Interpretation" -> Missing["Not supported."]|>,
				<|"Property" -> "list-style-type",     "Value" -> "initial", "Interpretation" -> <|CellDingbat -> "\[FilledCircle]"|>|>}|>,
		"Animatable" -> False,
		"Values" -> {"inside", "outside", "none", "<funciri>", "disc", "circle", "square", "decimal", "decimal-leading-zero", "lower-roman", "upper-roman", "lower-greek", "lower-latin", "upper-latin", "armenian", "georgian", "lower-alpha", "upper-alpha"},
		"AppliesTo" -> {"list-item"}|>,
	"list-style-image" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|CellDingbat -> Inherited|>,
			"initial" -> <|CellDingbat -> None|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "<funciri>"},
		"AppliesTo" -> {"list-item"}|>,
	"list-style-position" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "outside",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"inside", "outside"},
		"AppliesTo" -> {"list-item"}|>,
	"list-style-type" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "disc",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|CellDingbat -> Inherited|>,
			"initial" -> <|CellDingbat -> "\[FilledCircle]"|>|>,
		"Animatable" -> False,
		"Values" -> {"none", "disc", "circle", "square", "decimal", "decimal-leading-zero", "lower-roman", "upper-roman", "lower-greek", "lower-latin", "upper-latin", "armenian", "georgian", "lower-alpha", "upper-alpha"},
		"AppliesTo" -> {"list-item"}|>,
	"margin" -> <|(* shorthand property, sets all 4 margins *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				ImageMargins    -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> Inherited],
				CellMargins     -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> Inherited],
				PrintingOptions -> <|"PrintingMargins" -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> Inherited]|>|>,
			"initial" -> <|
				ImageMargins    -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> 0],
				CellMargins     -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> 0],
				PrintingOptions -> <|"PrintingMargins" -> AssociationThread[{"Left", "Right", "Bottom", "Top"} -> 0]|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"margin-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				ImageMargins    -> <|"Top" -> Inherited|>,
				CellMargins     -> <|"Top" -> Inherited|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Top" -> Inherited|>|>|>,
			"initial" -> <|
				ImageMargins    -> <|"Top" -> 0|>,
				CellMargins     -> <|"Top" -> 0|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Top" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"margin-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				ImageMargins    -> <|"Right" -> Inherited|>,
				CellMargins     -> <|"Right" -> Inherited|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Right" -> Inherited|>|>|>,
			"initial" -> <|
				ImageMargins    -> <|"Right" -> 0|>,
				CellMargins     -> <|"Right" -> 0|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Right" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"margin-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				ImageMargins    -> <|"Bottom" -> Inherited|>,
				CellMargins     -> <|"Bottom" -> Inherited|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Bottom" -> Inherited|>|>|>,
			"initial" -> <|
				ImageMargins    -> <|"Bottom" -> 0|>,
				CellMargins     -> <|"Bottom" -> 0|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Bottom" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"margin-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				ImageMargins    -> <|"Left" -> Inherited|>,
				CellMargins     -> <|"Left" -> Inherited|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Left" -> Inherited|>|>|>,
			"initial" -> <|
				ImageMargins    -> <|"Left" -> 0|>,
				CellMargins     -> <|"Left" -> 0|>,
				PrintingOptions -> <|"PrintingMargins" -> <|"Left" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"max-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Height" -> <|"Max" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Height" -> <|"Max" -> Automatic|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"max-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Width" -> <|"Max" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Width" -> <|"Max" -> Automatic|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"min-height" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Height" -> <|"Min" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Height" -> <|"Min" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"min-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Width" -> <|"Min" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Width" -> <|"Min" -> 0|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"orphans" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"<integer>"},
		"AppliesTo" -> {"block container elements"}|>,
	"outline" -> <|(* shorthand property, sets color/style/width, FE does not support outlines *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "outline-color", "Value" -> "inherit", "Interpretation" -> Missing["Not supported."]|>, 
				<|"Property" -> "outline-style", "Value" -> "inherit", "Interpretation" -> Missing["Not supported."]|>,
				<|"Property" -> "outline-width", "Value" -> "inherit", "Interpretation" -> Missing["Not supported."]|>},
    		"initial" -> {
				<|"Property" -> "outline-color", "Value" -> "initial", "Interpretation" -> Missing["Not supported."]|>,
		 		<|"Property" -> "outline-style", "Value" -> "initial", "Interpretation" -> Missing["Not supported."]|>,
				<|"Property" -> "outline-width", "Value" -> "initial", "Interpretation" -> Missing["Not supported."]|>}|>,
			<|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "invert", "none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset", "thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"outline-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "invert",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"<color>", "invert"},
		"AppliesTo" -> All|>,
	"outline-style" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset"},
		"AppliesTo" -> All|>,
	"outline-width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"thin", "medium", "thick", "<length>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"overflow" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "visible",(* not supported in FE*)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSizeAction -> Inherited|>,
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"visible", "hidden", "scroll", "auto"},
		"AppliesTo" -> {"block containers"}|>,
	"padding" -> <|(* shorthand property, sets all 4 sides *)
		"Inherited" -> False,
		"CSSInitialValue" -> "N/A", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> {
				<|"Property" -> "padding-left",   "Value" -> "inherit", "Interpretation" -> Inherited|>, 
				<|"Property" -> "padding-right",  "Value" -> "inherit", "Interpretation" -> Inherited|>, 
				<|"Property" -> "padding-bottom", "Value" -> "inherit", "Interpretation" -> Inherited|>, 
				<|"Property" -> "padding-top",    "Value" -> "inherit", "Interpretation" -> Inherited|>},
			"initial" -> {
				<|"Property" -> "padding-left",   "Value" -> "initial", "Interpretation" -> 0|>, 
				<|"Property" -> "padding-right",  "Value" -> "initial", "Interpretation" -> 0|>, 
				<|"Property" -> "padding-bottom", "Value" -> "initial", "Interpretation" -> 0|>, 
				<|"Property" -> "padding-top",    "Value" -> "initial", "Interpretation" -> 0|>}|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"padding-top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameMargins     -> Inherited,
				CellFrameMargins -> Inherited|>,
			"initial" -> <|
				FrameMargins     -> 0,
				CellFrameMargins -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"padding-bottom" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameMargins     -> Inherited,
				CellFrameMargins -> Inherited|>,
			"initial" -> <|
				FrameMargins     -> 0,
				CellFrameMargins -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"padding-left" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameMargins     -> Inherited,
				CellFrameMargins -> Inherited|>,
			"initial" -> <|
				FrameMargins     -> 0,
				CellFrameMargins -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"padding-right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FrameMargins     -> Inherited,
				CellFrameMargins -> Inherited|>,
			"initial" -> <|
				FrameMargins     -> 0,
				CellFrameMargins -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"page-break-after" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|PageBreakBelow -> Inherited|>,
			"initial" -> <|PageBreakBelow -> Automatic|>|>,
		"Animatable" -> False,
		"Values" -> {"auto", "always", "avoid", "left", "right"},
		"AppliesTo" -> {"block-level elements"}|>,
	"page-break-before" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|PageBreakAbove -> Inherited|>,
			"initial" -> <|PageBreakAbove -> Automatic|>|>,
		"Animatable" -> False,
		"Values" -> {"auto", "always", "avoid", "left", "right"},
		"AppliesTo" -> {"block-level elements"}|>,
	"page-break-inside" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				PageBreakWithin      -> Inherited,
				GroupPageBreakWithin -> Inherited|>,
			"initial" -> <|
				PageBreakWithin      -> Automatic,
				GroupPageBreakWithin -> Automatic|>|>,
		"Animatable" -> False,
		"Values" -> {"auto", "avoid"},
		"AppliesTo" -> {"block-level elements"}|>,
	"position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "static",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"static", "relative", "absolute", "fixed"},
		"AppliesTo" -> All|>,
	"quotes" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* depends on user agent aka WD *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"<string>", "none"},
		"AppliesTo" -> All|>,
	"right" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Alignment -> {Inherited, Automatic}|>,
			"initial" -> <|Alignment -> {Automatic, Automatic}|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> {"positioned elements"},
		"NumericThreshold" -> -Infinity|>,
	"table-layout" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"auto", "fixed"},
		"AppliesTo" -> {"table", "inline-table"}|>, 		
	"text-align" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "N/A", (* a nameless value that acts as 'left' if 'direction' is 'ltr', 'right' if 'direction' is 'rtl' *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|TextAlignment -> Inherited|>,
			"initial" -> <|TextAlignment -> Automatic|>|>,
		"Animatable" -> False,
		"Values" -> {"left", "center", "right", "justify"},
		"AppliesTo" -> {"block containers"}|>,
	"text-decoration" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontVariations -> Inherited|>,
			"initial" -> <|FontVariations -> <||>|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "underline", "overline", "line-through", "blink"},
		"AppliesTo" -> All|>,
	"text-indent" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				LineIndent      -> Inherited,
				ParagraphIndent -> Inherited|>,
			"initial" -> <|
				LineIndent      -> 0,
				ParagraphIndent -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> {"block containers"},
		"NumericThreshold" -> -Infinity|>,
	"text-transform" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontVariations -> <|"CapsType" -> Inherited|>|>,
			"initial" -> <|FontVariations -> <|"CapsType" -> "Normal"|>|>|>,
		"Animatable" -> False,
		"Values" -> {"capitalize", "uppercase", "lowercase", "none"},
		"AppliesTo" -> All|>,
	"top" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|Alignment -> {Automatic, Inherited}|>,
			"initial" -> <|Alignment -> {Automatic, Automatic}|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>"},
		"AppliesTo" -> {"positioned elements"},
		"NumericThreshold" -> -Infinity|>,
	"unicode-bidi" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"normal", "embed", "bidi-override"},
		"AppliesTo" -> All|>,
	"vertical-align" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "baseline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				BaselinePosition -> Inherited,
				CellBaseline     -> Inherited|>,
			"initial" -> <|
				BaselinePosition -> Baseline -> Baseline,
				CellBaseline     -> Center|>|>,
		"Animatable" -> True,
		"Values" -> {"<percentage>", "<length>", "baseline", "sub", "super", "top", "text-top", "middle", "bottom", "text-bottom"},
		"AppliesTo" -> {"inline-level", "table-cell"},
		"NumericThreshold" -> -Infinity|>,
	"visibility" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visible",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ShowContents -> Inherited|>,
			"initial" -> <|ShowContents -> True|>|>,
		"Animatable" -> True,
		"Values" -> {"visible", "hidden", "collapse"},
		"AppliesTo" -> All|>,
	"white-space" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"normal", "pre", "nowrap", "pre-wrap", "pre-line"},
		"AppliesTo" -> All|>,
	"widows" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "2",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"<integer>"},
		"AppliesTo" -> {"block container elements"},
		"NumericThreshold" -> 1|>,
	"width" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSize -> <|"Width" -> <|"Min" -> Inherited, "Max" -> Inherited|>|>|>,
			"initial" -> <|ImageSize -> <|"Width" -> <|"Min" -> Automatic, "Max" -> Automatic|>|>|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "auto"},
		"AppliesTo" -> All,
		"NumericThreshold" -> 0|>,
	"word-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "normal"},
		"AppliesTo" -> All,
		"NumericThreshold" -> -Infinity|>,
	"z-index" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"<integer>", "auto"},
		"AppliesTo" -> {"positioned elements"},
		"NumericThreshold" -> -Infinity|>
}];


(* ::Subsection::Closed:: *)
(*<angle>*)


(* mostly used for HSLA color function; non-degree units are converted to degrees *)
parseAngle[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["deg",  t] := val
parseAngle[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["grad", t] := val*360/400
parseAngle[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["rad",  t] := val*360/2/Pi
parseAngle[___] := Failure["UnexpectedParse", <|"Message" -> "Expected CSS token of angle type."|>]


(* ::Subsection::Closed:: *)
(*<color>*)


(* 
	parseSingleColor is more extensively defined in CSSColors4.wl.
	The following definitions only follow the CSS 2.1 specification. *)

(* parse all color types *)
parseSingleColor[prop_String, token_?CSSTokenQ] := parseSingleColor[prop, token] = 
	Switch[token["Type"],
		"ident",    parseSingleColorKeyWord[prop, token["String"]],
		"hash",     parseSingleColorHex[prop, token["String"]],
		"function", parseSingleColorFunction[prop, token],
		_,          unrecognizedValueFailure @ prop
	]
		
parseSingleColor[prop_String, ___] := unrecognizedValueFailure @ prop

parseSingleColorKeyWord[prop_String, keyword_String] := 
	Switch[keyword,
		"aqua",    RGBColor[0, 1, 1], 
		"black",   RGBColor[0, 0, 0], 
		"blue",    RGBColor[0, 0, 1], 
		"fuchsia", RGBColor[1, 0, 1],
		"gray",    RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]],
		"green",   RGBColor[0, Rational[128, 255], 0],
		"lime",    RGBColor[0, 1, 0],
		"maroon",  RGBColor[Rational[128, 255], 0, 0],
		"navy",    RGBColor[0, 0, Rational[128, 255]],
		"olive",   RGBColor[Rational[128, 255], Rational[128, 255], 0],
		"orange",  RGBColor[1, Rational[11, 17], 0],
		"purple",  RGBColor[Rational[128, 255], 0, Rational[128, 255]],
		"red",     RGBColor[1, 0, 0],
		"silver",  RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]],
		"teal",    RGBColor[0, Rational[128, 255], Rational[128, 255]],
		"white",   RGBColor[1, 1, 1],
		"yellow",  RGBColor[1, 1, 0],
		_,         unrecognizedValueFailure @ prop
	]

(* Hex *)
$1XC = Repeated[RegularExpression[RE["hex digit"]], {1}];
$2XC = Repeated[RegularExpression[RE["hex digit"]], {2}];
fromhexdigits[s_] := FromDigits[s, 16]
(* 3 digits *) hexPattern3[] := StartOfString ~~ r:$1XC ~~ g:$1XC ~~ b:$1XC ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b} / 15);
(* 6 digits *) hexPattern6[] := StartOfString ~~ r:$2XC ~~ g:$2XC ~~ b:$2XC ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b} / 255);

parseSingleColorHex[prop_String, hexString_String] :=
	Which[
		StringMatchQ[hexString, First @ hexPattern3[]], First[StringCases[hexString, hexPattern3[]], unrecognizedValueFailure @ prop],
		StringMatchQ[hexString, First @ hexPattern6[]], First[StringCases[hexString, hexPattern6[]], unrecognizedValueFailure @ prop],
		True, Failure["UnexpectedParse", <|"Message" -> "Unrecognized hex color " <> hexString <> "."|>]]

(* RGB *)
(* The patterns assume all whitespace has been removed. *)
rgbPattern[] := 
	{
		CSSToken[KeyValuePattern[{"Type" -> v1:"number"|"percentage", "Value" -> r_}]],
		d:Repeated[CSSToken[KeyValuePattern["Type" -> "comma"]], {0, 1}], 
		CSSToken[KeyValuePattern[{"Type" -> v1:"number"|"percentage", "Value" -> g_}]],
		d:Repeated[CSSToken[KeyValuePattern["Type" -> "comma"]], {0, 1}], 
		CSSToken[KeyValuePattern[{"Type" -> v1:"number"|"percentage", "Value" -> b_}]]
	} :> Apply[RGBColor, {r, g, b}/If[v1 == "number", 255, 100.]]

parseSingleColorFunction[prop_String, token_?CSSTokenQ] :=
	Module[{relevantTokens},
		(* relevantTokens removes all whitespace tokens *)
		relevantTokens = DeleteCases[token["Children"], CSSToken[KeyValuePattern["Type" -> "whitespace"]], {1}];
		If[StringMatchQ[token["RawString"], RegularExpression[RE["R"] ~~ RE["G"] ~~ RE["B"]]],
			If[MatchQ[relevantTokens, First @ rgbPattern[]], 
				Replace[relevantTokens, rgbPattern[]]
				, 
				unrecognizedValueFailure @ prop
			]
			, 
			Failure["UnexpectedParse", <|"Message" -> "Unrecognized color function " <> token["String"] <> "."|>]
		]
	]


(* ::Subsection::Closed:: *)
(*<counter>*)


(* counter() function *)
parseCounter[prop_String, tokens:{___?CSSTokenQ}] := (*parseCounter[prop, tokens] =*)
	Module[{pos = 1, l = Length[tokens], style = "Item", listtype = "decimal"},
		(* assumes that the function identifier and name have been skipped *)
		If[pos <= l && TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* get custom identifier *)
		If[pos <= l && TokenTypeIs["ident", tokens[[pos]]], 
			style = tokens[[pos]]["String"]
			, 
			Return @ invalidFunctionFailure @ CSSUntokenize @ tokens
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos > l, Return @ parseSingleListStyleType["list-style-type", CSSToken[<|"Type" -> "ident", "String" -> listtype|>], style]];
		
		(* get optional counter style *)
		If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
			listtype = tokens[[pos]]["String"]
			,
			Return @ invalidFunctionFailure @ CSSUntokenize @ tokens
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos > l, Return @ parseSingleListStyleType["list-style-type", CSSToken[<|"Type" -> "ident", "String" -> listtype|>], style]];
		
		tooManyTokensFailure @ tokens
	]
	
(* counters() function *)
parseCounters[prop_String, tokens:{___?CSSTokenQ}] := Missing["Not supported."]


(* ::Subsection::Closed:: *)
(*<frequency>*)


parseFrequency[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["hz",  t] := val
parseFrequency[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["khz", t] := val*1000


(* ::Subsection::Closed:: *)
(*<integer> and <number>*)


parseNumber[CSSToken[KeyValuePattern[{"Type" -> "number", "Value" -> val_?NumericQ}]]] := val
parseNumber[___] := Failure["UnexpectedParse", <|"Message" -> "Expected CSS token of number type."|>]

parseZero[CSSToken[KeyValuePattern[{"Type" -> "number", "Value" -> val_?NumericQ}]]] := 
	If[TrueQ[val == 0], 0, Failure["UnexpectedParse", <|"Message" -> "Non-zero length has missing units."|>]]
parseZero[___] := Failure["UnexpectedParse", <|"Message" -> "Expected CSS token of number type."|>]


(* ::Subsection::Closed:: *)
(*<length>*)


(* CSS's DPI is fixed at 96 *)
parseLength[CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> unit_}]], inFontSize_:False] := 
	Module[{dpi = 96(*"Resolution" /. First[SystemInformation["Devices", "ScreenInformation"], "Resolution" -> 72]*)},
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
			"q",  val/40/2.54*dpi,
			_,    Failure["UnexpectedParse", <|"Message" -> "Unrecognized length unit."|>]
		]
	]


parseLengthNonRelative[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["em", t] := val 
parseLengthNonRelative[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["ex", t] := val/2
parseLengthNonRelative[___] := Failure["UnexpectedParse", <|"Message" -> "Expected CSS token of dimension type \"em\" or \"ex\"."|>]


(* ::Subsection::Closed:: *)
(*<percentage>*)


parsePercentage[CSSToken[KeyValuePattern[{"Type" -> "percentage", "Value" -> val_?NumericQ}]]] := Scaled[val/100]
parsePercentage[___] := Failure["UnexpectedParse", <|"Message" -> "Expected CSS token of percentage type."|>]


(* ::Subsection::Closed:: *)
(*<time>*)


parseTime[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["s",  t] := val
parseTime[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["ms", t] := val/1000


(* ::Subsection::Closed:: *)
(*<uri>*)


(* 
	URI token is of the form CSSToken[<|"Type" -> "url", "String" -> location, "Quotes" -> <<None or "\"" or "'">>|>] 
	where location is a string without the url() wrapper and string characters *)
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
					Switch[CSSNormalizeEscapes @ ToLowerCase @ start,
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
consumeProperty[prop_String, {t:CSSToken[KeyValuePattern[{"Type" -> "ident", "String" -> _?StringQ}]]}, opts:OptionsPattern[]] /; TokenStringIs["inherit", t] := 
	CSSPropertyData[prop, "InterpretedGlobalValues", "inherit"]
	
consumeProperty[prop_String, {t:CSSToken[KeyValuePattern[{"Type" -> "ident", "String" -> _?StringQ}]]}, opts:OptionsPattern[]] /; TokenStringIs["initial", t] := 
	CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]
	
consumeProperty[prop_String, {t:CSSToken[KeyValuePattern[{"Type" -> "ident", "String" -> _?StringQ}]]}, opts:OptionsPattern[]] /; TokenStringIs["unset", t] := 
	If[CSSPropertyData[prop, "Inherited"], 
		CSSPropertyData[prop, "InterpretedGlobalValues", "inherit"]
		, 
		CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]
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
consumeProperty[prop:"background", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{backgrounds, result},
		backgrounds = DeleteCases[SplitBy[tokens, MatchQ[CSSToken[KeyValuePattern["Type" -> "comma"]]]], {CSSToken[KeyValuePattern["Type" -> "comma"]]}];
		backgrounds = parseSingleBG[prop, #]& /@ backgrounds; 
		result = Cases[backgrounds, Except[_Failure], {1}];
		If[result === {}, 
			FirstCase[backgrounds, _Failure, Failure["BadBackground", <|"Message" -> "Could not parse background property."|>], {1}]
			,
			First @ result
		]
	]
	
(* perhaps how to combine FE options:
(* 
			attachment: not supported, 
			color: Background, 
			image: System`BackgroundAppearance, 
			position: System`BackgroundAppearanceOptions, 
			repeat: System`BackgroundAppearanceOptions *)
		Which[
			hasColor && Not[hasAttachment || hasImage || hasPosition || hasRepeat], 
				<|Background -> values["c"]|>,
			True,
				<|
					System`BackgroundAppearanceOptions ->
						Which[
							values["p"] === {0,0}    && values["r"] === "NoRepeat", "NoRepeat",
							values["p"] === "Center" && values["r"] === "NoRepeat", "Center",
							values["p"] === {0,0},                                  values["r"],
							True,                                                   Missing["Not supported."]
						],
					System`BackgroundAppearance -> values["i"],
					Background -> values["c"]|>
		]
*)

fillInitialShortcutProperty[prop_?StringQ] := <|
	"Property"       -> prop, 
	"Value"          -> CSSPropertyData[prop, "CSSInitialValue"],
	"Interpretation" -> CSSPropertyData[prop, "InterpretedGlobalValues", "initial"]|>

parseSingleBG[prop_String, tokens:{__?CSSTokenQ}] := 
	Module[
		{
			pos = 1, l = Length[tokens], value, start, startToken, 
			values = <|
				"a" -> fillInitialShortcutProperty["background-attachment"], (* Missing["Not supported."] *)
				"c" -> fillInitialShortcutProperty["background-color"],
				"i" -> fillInitialShortcutProperty["background-image"],
				"p" -> fillInitialShortcutProperty["background-position"],
				"r" -> fillInitialShortcutProperty["background-repeat"]|>,
			hasAttachment = False, hasColor = False, hasImage = False, hasPosition = False, hasRepeat = False
		},
		While[pos <= l, 
			Which[
				TokenTypeIs["function", tokens[[pos]]], 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						(* color *)
						"rgb" | "rgba" | "hsl" | "hsla", 
							If[hasColor, Return @ repeatedPropValueFailure @ "background-color"]; 
							hasColor = True; 
							values["c"] = <|
								"Property"       -> "background-color",
								"Value"          -> CSSUntokenize @ tokens[[pos]],
								"Interpretation" -> <|Background -> parseSingleColor[prop, tokens[[pos]]]|>|>;,
						(*TODO support gradients *)
						"linear-gradient" | "repeating-linear-gradient" | "radial-gradient" | "repeating-radial-gradient" | "conic-gradient", 
							If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
							hasImage = True; 
							values["i"] = <|
								"Property"       -> "background-image",
								"Value"          -> CSSUntokenize @ tokens[[pos]],
								"Interpretation" -> Missing["Not supported."]|>;,
						_,
							Return @ invalidFunctionFailure @ tokens[[pos]]["String"]
					],
				
				(* scroll or fixed keyword *)
				!FailureQ[value = parseSingleBGAttachment[prop, tokens[[pos]]]],
					If[hasAttachment, Return @ repeatedPropValueFailure @ "background-attachment"];
					hasAttachment = True; 
					values["a"] = <|
						"Property"       -> "background-attachment",
						"Value"          -> CSSUntokenize @ tokens[[pos]],
						"Interpretation" -> value|>, (*Missing["Not supported."*)
					
				(* color hex or color keyword *)
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]], (* color can also be hex or keyword *)
					If[hasColor, Return @ repeatedPropValueFailure @ "background-color"];
					hasColor = True; 
					values["c"] = <|
						"Property"       -> "background-color",
						"Value"          -> CSSUntokenize @ tokens[[pos]],
						"Interpretation" -> <|Background -> value|>|>,
				
				(* uri token or none keyword *)
				value = parseSingleBGImage[prop, tokens[[pos]]];
				Or[
					!FailureQ[value],
					FailureQ[value] && First[value] === "ImportFailure"
				], 
					If[hasImage, Return @ repeatedPropValueFailure @ "background-image"];
					hasImage = True; 
					values["i"] = <|
						"Property"       -> "background-image",
						"Value"          -> CSSUntokenize @ tokens[[pos]],
						"Interpretation" -> <|System`BackgroundAppearance -> value|>|>,
				
				(* one of the keywords repeat | repeat-x | repeat-y | no-repeat *)
				!FailureQ[value = parseSingleBGRepeat[prop, tokens[[pos]]]], 
					If[hasRepeat, Return @ repeatedPropValueFailure @ "background-repeat"];
					hasRepeat = True; 
					values["r"] = <|
						"Property"       -> "background-repeat",
						"Value"          -> CSSUntokenize @ tokens[[pos]],
						"Interpretation" -> <|System`BackgroundAppearanceOptions -> value|>|>,
				
				(* one of the key words left | center | right | top | bottom, *)
				!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
					If[hasPosition, Return @ repeatedPropValueFailure @ "background-position"];
					hasPosition = True; 
					values["p"] = <|
						"Property"       -> "background-position",
						"Value"          -> CSSUntokenize @ tokens[[pos]],
						"Interpretation" -> If[MissingQ[value], value, <|System`BackgroundAppearanceOptions -> {value, Center}|>]|>;
					(* check for a pair of position values; they must be sequential *)
					start = pos; startToken = tokens[[pos]]; AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[!FailureQ[value = parseSingleBGPosition[prop, tokens[[pos]]]], 
						values["p"] = <|
							"Property"       -> "background-position",
							"Value"          -> CSSUntokenize @ tokens[[start ;; pos]],
							"Interpretation" -> (
								value = parseSingleBGPositionPair[{values["p"][["Interpretation", 1, 1]], value}, tokens[[start ;; pos]]];
								If[FailureQ[value] || MissingQ[value], value, <|System`BackgroundAppearanceOptions -> value|>])|>
						,
						pos = start (* if this next token leads to a parse failure, then reset the position *)
					],
								
				True, Return @ unrecognizedValueFailure @ prop						
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		Values @ values
]


(* ::Subsubsection::Closed:: *)
(*background-attachment*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and the 'local' value.
	We only support CSS 2.1 for now and it supports only a single value. *)
consumeProperty[prop:"background-attachment", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGAttachment[prop, tokens[[pos]]];
		If[FailureQ[value] || MissingQ[value], value, Missing["Only fixed supported."]]
	]

parseSingleBGAttachment[prop_String, token_?CSSTokenQ] := 
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
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
consumeProperty[prop:"background-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value},
		value = parseSingleColor[prop, First @ tokens];
		If[FailureQ[value], value, <|Background -> value|>]
	]


(* ::Subsubsection::Closed:: *)
(*background-image*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and any CSS <image> data type.
	We only support CSS 2.1 for now and it supports only a single value. *)
consumeProperty[prop:"background-image", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{(*pos = 1, *)l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGImage[prop, First @ tokens];
		If[FailureQ[value], value, <|System`BackgroundAppearance -> value|>]
	]

parseSingleBGImage[prop_String, token_?CSSTokenQ] := 
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
				"none", None,
				_,      unrecognizedKeyWordFailure @ prop
			],
		"function", 
			Switch[CSSNormalizeEscapes @ ToLowerCase @ token["String"],
				Alternatives[
					"linear-gradient", "repeating-linear-gradient",
					"radial-gradient", "repeating-radial-gradient", "conic-gradient"
				],
				   Missing["Not supported."],
				_, invalidFunctionFailure @ CSSUntokenize @ token
			],
		"url", parseURI @ token["String"],
		_,     unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*background-position*)


(* 
	In CSS Background and Borders 3, this prop can take multiple comma-separated backgrounds and use a 4-value syntax.
	We only support CSS 2.1 for now and it can take up to 2 values. *)
consumeProperty[prop:"background-position", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value = parseSingleBGPosition[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[values, value]];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		value = parseSingleBGPositionPair[values, tokens];		
		If[FailureQ[value] || MissingQ[value], value, <|System`BackgroundAppearanceOptions -> value|>]
	]

parseSingleBGPosition[prop_String, token_?CSSTokenQ] :=
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
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
parseSingleBGPositionPair[values:{__}, tokens:{__?CSSTokenQ}] :=
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
					{0 | Scaled[0], Top}, {Left, 0 | Scaled[0]},
					{0 | Scaled[0], 0 | Scaled[0]}
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
consumeProperty[prop:"background-repeat", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBGRepeat[prop, tokens[[pos]]];
		If[FailureQ[value], value, <|System`BackgroundAppearanceOptions -> value|>]
	]
	

parseSingleBGRepeat[prop_String, token_?CSSTokenQ] :=
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
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
consumeProperty[prop:"border-collapse", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		Switch[tokens[[pos]]["Type"],
			"ident", 
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"separate", Missing["Not supported."],
					"collapse", <||>, (* this is all Mathematica supports *)
					_,          unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-color*)


(* 
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time. 
	This requires post-processing of the parsed result. *)
consumeProperty[
	prop:"border-top-color" | "border-right-color" | "border-bottom-color" | "border-left-color", 
	tokens:{__?CSSTokenQ}, 
	opts:OptionsPattern[]
] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			<|FrameStyle -> value, CellFrameStyle -> value|>
		]
	]	

(* sets all 4 border/frame edges at once *)
consumeProperty[prop:"border-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, rawResults = {}, results = {}},
		While[pos <= l,
			value = parseSingleColor[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; AppendTo[rawResults, CSSUntokenize @ tokens[[pos]]]]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, 
				{
					<|"Property" -> "border-left-color",   "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-right-color",  "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-bottom-color", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-color",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			2, 
				{
					<|"Property" -> "border-left-color",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-color",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-color", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-color",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			3, 
				{
					<|"Property" -> "border-left-color",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-color",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-color", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-color",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			4, 
				{
					<|"Property" -> "border-left-color",   "Value" -> rawResults[[4]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[4]]|>,
					<|"Property" -> "border-right-color",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-color", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-color",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			_, tooManyTokensFailure @ tokens
		]
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
consumeProperty[prop:"border-spacing", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		While[pos <= l,
			value = 
				Switch[tokens[[pos]]["Type"],
					"dimension", 
						If[tokens[[pos]]["Value"] < 0,
							negativeLengthFailure @ prop
							,
							Switch[tokens[[pos]]["Unit"],
								"em"|"ex", (parseLengthNonRelative @ tokens[[pos]])/2,
								_,         With[{n = parseLength @ tokens[[pos]]}, Dynamic[n/CurrentValue[FontSize]]]
							]
						],
					"number", parseZero @ tokens[[pos]],
					_,        unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[results, value]];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, <|Spacings -> {First @ results, First @ results}|>, (* if only one length, then it specifies both horizontal and vertical *)
			2, <|Spacings -> results|>,
			_, tooManyTokensFailure @ tokens
		]
	]


(* ::Subsubsection::Closed:: *)
(*border-style*)


(* 
	Setting a single border/frame is only possible in WL if all 4 edges are specified at the same time. 
	This requires post-processing of the parsed result. *)
consumeProperty[
	prop:"border-top-style" | "border-right-style" | "border-bottom-style" | "border-left-style", 
	tokens:{__?CSSTokenQ}, 
	opts:OptionsPattern[]
] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderStyle[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			<|FrameStyle -> value, CellFrameStyle -> value|>
		]
	]
	
(* sets all 4 border/frame edges at once *)
consumeProperty[prop:"border-style", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, rawResults = {}, results = {}},
		While[pos <= l,
			value = parseSingleBorderStyle[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; AppendTo[rawResults, StringTrim @ CSSUntokenize @ tokens[[pos]]]];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		Switch[Length[results],
			1, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			2, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			3, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			4, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[4]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[4]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrameStyle -> #|>& @ results[[1]]|>},
			_, tooManyTokensFailure @ tokens
		]
	]

parseSingleBorderStyle[prop_String, token_?CSSTokenQ] :=
	Switch[token["Type"],
		"ident",
			Switch[token["String"],
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
	This requires post-processing of the parsed result. *)
consumeProperty[
	prop:"border-top-width" | "border-right-width" | "border-bottom-width" | "border-left-width", 
	tokens:{__?CSSTokenQ}, 
	opts:OptionsPattern[]
] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleBorderWidth[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			<|FrameStyle -> value, CellFrame -> convertToCellThickness @ value|>
		]
	]
	
(* sets all 4 frame edge thickness at once *)
consumeProperty[prop:"border-width", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, rawResults = {}, results = {}},
		While[pos <= l,
			value = parseSingleBorderWidth[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; AppendTo[rawResults, StringTrim @ CSSUntokenize @ tokens[[pos]]]];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		(* expand out results  to {{L,R},{B,T}} *)
		Switch[Length[results],
			1, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>},
			2, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>},
			3, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[2]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>},
			4, 
				{
					<|"Property" -> "border-left-style",   "Value" -> rawResults[[4]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[4]]|>,
					<|"Property" -> "border-right-style",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[2]]|>,
					<|"Property" -> "border-bottom-style", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[3]]|>,
					<|"Property" -> "border-top-style",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameStyle -> #, CellFrame -> convertToCellThickness @ #|>& @ results[[1]]|>},
			_, tooManyTokensFailure @ tokens
		]
	]
	
(* WL FrameStyle thickness is best given as a calculated AbsoluteThickness for numerical values. *)
parseSingleBorderWidth[prop_String, token_?CSSTokenQ] :=
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
				"thin",   Thickness[Small],
				"medium", Thickness[Medium],
				"thick",  Thickness[Large],
				_,        unrecognizedKeyWordFailure @ prop
			],
		"dimension", If[token["Value"] < 0, negativeLengthFailure @ prop, AbsoluteThickness[parseLength @ token]],
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
	tokens:{__?CSSTokenQ}, 
	opts:OptionsPattern[]
] := 
	Module[{pos = 1, l = Length[tokens], value, side, values, rawValues, hasColor = False, hasStyle = False, hasWidth = False},
		side = Switch[prop, "border-left", "Left", "border-right", "Right", "border-top", "Top", "border-bottom", "Bottom", _, "Top"];
		values = <|
			"c" -> CSSPropertyData["border-top-color", "InterpretedGlobalValues", "initial", FrameStyle], 
			"s" -> CSSPropertyData["border-top-style", "InterpretedGlobalValues", "initial", FrameStyle], 
			"w" -> CSSPropertyData["border-top-width", "InterpretedGlobalValues", "initial", FrameStyle]|>;
		rawValues = <|"c" -> "currentColor", "s" -> "none", "w" -> "medium"|>;
		
		While[pos <= l,
			Which[
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]],
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")];
					hasColor = True; values["c"] = value; rawValues["c"] = CSSUntokenize @ tokens[[pos]],
				
				!FailureQ[value = parseSingleBorderStyle[prop, tokens[[pos]]]],
					If[hasStyle, Return @ repeatedPropValueFailure @ (prop <> "-style")];
					hasStyle = True; values["s"] = value; rawValues["s"] = CSSUntokenize @ tokens[[pos]],
					
				!FailureQ[value = parseSingleBorderWidth[prop, tokens[[pos]]]],
					If[hasWidth, Return @ repeatedPropValueFailure @ (prop <> "-width")];
					hasWidth = True; values["w"] = value; rawValues["w"] = CSSUntokenize @ tokens[[pos]],
				
				True, Return @ unrecognizedValueFailure @ prop						
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		If[prop === "border",
			{
				<|"Property" -> "border-left-color",   "Value" -> rawValues["c"], "Interpretation" -> <|FrameStyle -> values["c"], CellFrameStyle -> values["c"]|>|>, 
				<|"Property" -> "border-right-color",  "Value" -> rawValues["c"], "Interpretation" -> <|FrameStyle -> values["c"], CellFrameStyle -> values["c"]|>|>, 
				<|"Property" -> "border-bottom-color", "Value" -> rawValues["c"], "Interpretation" -> <|FrameStyle -> values["c"], CellFrameStyle -> values["c"]|>|>, 
				<|"Property" -> "border-top-color",    "Value" -> rawValues["c"], "Interpretation" -> <|FrameStyle -> values["c"], CellFrameStyle -> values["c"]|>|>, 
				<|"Property" -> "border-left-style",   "Value" -> rawValues["s"], "Interpretation" -> <|FrameStyle -> values["s"], CellFrameStyle -> values["s"]|>|>, 
				<|"Property" -> "border-right-style",  "Value" -> rawValues["s"], "Interpretation" -> <|FrameStyle -> values["s"], CellFrameStyle -> values["s"]|>|>, 
				<|"Property" -> "border-bottom-style", "Value" -> rawValues["s"], "Interpretation" -> <|FrameStyle -> values["s"], CellFrameStyle -> values["s"]|>|>, 
				<|"Property" -> "border-top-style",    "Value" -> rawValues["s"], "Interpretation" -> <|FrameStyle -> values["s"], CellFrameStyle -> values["s"]|>|>,
				<|"Property" -> "border-left-width",   "Value" -> rawValues["w"], "Interpretation" -> <|FrameStyle -> values["w"], CellFrame -> convertToCellThickness @ values["w"]|>|>, 
				<|"Property" -> "border-right-width",  "Value" -> rawValues["w"], "Interpretation" -> <|FrameStyle -> values["w"], CellFrame -> convertToCellThickness @ values["w"]|>|>, 
				<|"Property" -> "border-bottom-width", "Value" -> rawValues["w"], "Interpretation" -> <|FrameStyle -> values["w"], CellFrame -> convertToCellThickness @ values["w"]|>|>, 
				<|"Property" -> "border-top-width",    "Value" -> rawValues["w"], "Interpretation" -> <|FrameStyle -> values["w"], CellFrame -> convertToCellThickness @ values["w"]|>|>},
			{
				<|"Property" -> prop <> "-color", "Value" -> rawValues["c"], "Interpretation" -> <|FrameStyle -> values["c"], CellFrameStyle -> values["c"]|>|>, 
				<|"Property" -> prop <> "-style", "Value" -> rawValues["s"], "Interpretation" -> <|FrameStyle -> values["s"], CellFrameStyle -> values["s"]|>|>, 
				<|"Property" -> prop <> "-width", "Value" -> rawValues["w"], "Interpretation" -> <|FrameStyle -> values["w"], CellFrame -> convertToCellThickness @ values["w"]|>|>}
		]
	]


(* ::Subsection::Closed:: *)
(*clip*)


(* deprecated, but we still mostly parse it for correctness *)
consumeProperty[prop:"clip", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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


(* color is represented by a single token, either a named color, hex e.g. #fff, or function *)
consumeProperty[prop:"color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	With[{value = parseSingleColor[prop, First @ tokens]},
		If[FailureQ[value], value, <|FontColor -> value|>]]
		

(* ::Subsection::Closed:: *)
(*content, lists, and quotes*)


(* ::Subsubsection::Closed:: *)
(*content*)


(* 
	Used to add content before or after element, or add content to a page header/footer.
	It's too restrictive to assign this to just Cells' CellDingbat or CellLabel. *)
consumeProperty[prop:"content", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, parsedValues = {}},
		While[pos <= l,
			value = 
				Switch[tokens[[pos]]["Type"],
					"ident", 
						Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
							"normal",         If[pos > 1, Return @ tooManyTokensFailure @ tokens, Normal],
							"none",           If[pos > 1, Return @ tooManyTokensFailure @ tokens, None],
							"open-quote",     Missing["Not supported."],
							"close-quote",    Missing["Not supported."],
							"no-open-quote",  Missing["Not supported."],
							"no-close-quote", Missing["Not supported."],
							_,                unrecognizedKeyWordFailure @ prop
						],
					"string", tokens[[pos]]["String"],
					"url",    
						With[{i = parseURI @ tokens[[pos]]["String"]}, 
							If[FailureQ[i] || MissingQ[i], 
								notAnImageFailure @ tokens[[pos]]["String"]
								, 
								ToBoxes @ i
							]
						],
					"function", 
						Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
							"counter",  parseCounter[prop, tokens[[pos]]["Children"]],
							"counters", parseCounters[prop, tokens[[pos]]["Children"]],
							"attr",     (*TODO*)parseAttr[prop, tokens[[pos]]["Children"]],
							_,          unrecognizedValueFailure @ prop
						],
					_, unrecognizedValueFailure @ prop
				];
			AppendTo[parsedValues, value];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		With[{p = parsedValues}, <|DisplayFunction -> If[MatchQ[p, {Normal | None}], First @ p, Function[RowBox[p]]]|>]
	]


(* ::Subsubsection::Closed:: *)
(*counter-increment*)


(* In WL each style must be repeated n times to get an increment of n *)
consumeProperty[prop:"counter-increment", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], v, values = {}, cPos, n},
		While[pos <= l,
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", 
							If[l > 1,  (* none must be the only identifier if it is used *)
								Return @ illegalIdentifierFailure @ tokens[[pos]]["String"]
								, 
								values = {}
							],
						_,  
							v = tokens[[pos]]["String"];
							(* check to see if identifier token is immediately followed by an integer; if so, consume the integer, too *)
							cPos = pos; AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos <= l && TokenTypeIs["number", tokens[[pos]]],
								If[tokens[[pos]]["ValueType"] != "integer", Return @ Failure["BadNumber", <|"Message" -> "Expected integer type."|>]];
								n = tokens[[pos]]["Value"];
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
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		<|CounterIncrements -> values|>
	]


(* ::Subsubsection::Closed:: *)
(*counter-reset*)


consumeProperty[prop:"counter-reset", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], v, values = {}, cPos},
		While[pos <= l,
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", 
							If[l > 1, 
								Return @ illegalIdentifierFailure @ tokens[[pos]]["String"]
								, 
								values = {}
							],
						_,        
							v = tokens[[pos]]["String"];
							(* check to see if identifier token is immediately followed by an integer; if so, consume the integer, too *)
							cPos = pos; AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos <= l && TokenTypeIs["number", tokens[[pos]]],
								If[tokens[[pos]]["ValueType"] != "integer", Return @ Failure["BadNumber", <|"Message" -> "Expected integer type."|>]];
								AppendTo[values, {v, tokens[[pos]]["Value"]}]
								,
								AppendTo[values, {v, 0}]; pos = cPos;
							];
					],
				_, Return @ unrecognizedValueFailure @ prop
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		<|CounterAssignments -> values|>
	]


(* ::Subsubsection::Closed:: *)
(*list-style-image*)


consumeProperty[prop:"list-style-image", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{(*pos = 1, *)l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleImage[prop, First @ tokens];
		If[FailureQ[value], value, <|CellDingbat -> value|>]
	]

parseSingleListStyleImage[prop_String, token_?CSSTokenQ] := 
	Switch[token["Type"],
		"ident",
			Switch[token["String"],
				"none", None,
				_,      unrecognizedKeyWordFailure @ prop
			],
		"url", 
			With[{im = parseURI @ token["String"]}, 
				Which[
					FailureQ[im], im, 
					MissingQ[im], im,
					!ImageQ[im],  notAnImageFailure @ token["String"],
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
consumeProperty[prop:"list-style-position", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{(*pos = 1, *)l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStylePosition[prop, First @ tokens];
		If[FailureQ[value], value, Missing["Not supported."]]
	]

parseSingleListStylePosition[prop_String, token_?CSSTokenQ] := 
	Switch[token["Type"],
		"ident",
			Switch[token["String"],
				"inside",  Missing["Not supported."],
				"outside", Automatic,
				_,         unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]


(* ::Subsubsection::Closed:: *)
(*list-style-type*)


consumeProperty[prop:"list-style-type", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{(*pos = 1, *)l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleListStyleType[prop, First @ tokens];
		If[FailureQ[value], value, <|CellDingbat -> value|>]
	]
	
parseSingleListStyleType[prop_String, token_?CSSTokenQ, style_String:"Item"] := 
	Switch[token["Type"],
		"ident",
			Switch[token["String"],
				"disc",                 "\[FilledCircle]",
				"circle",               "\[EmptyCircle]",
				"square",               "\[FilledSquare]",
				"decimal",              CounterBox[style],
				"decimal-leading-zero", CounterBox[style, CounterFunction :> (FEPrivate`If[FEPrivate`Greater[#, 9], #, FEPrivate`StringJoin["0", FEPrivate`ToString[#]]]&)],
				"lower-roman",          CounterBox[style, CounterFunction :> FrontEnd`RomanNumeral],
				"upper-roman",          CounterBox[style, CounterFunction :> FrontEnd`CapitalRomanNumeral],
				"lower-greek",          CounterBox[style, CounterFunction :> (Part[CharacterRange["\[Alpha]", "\[Omega]"], #]&)],
				"lower-latin",          CounterBox[style, CounterFunction :> (Part[CharacterRange["a", "z"], #]&)],
				"upper-latin",          CounterBox[style, CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)],
				"armenian",             CounterBox[style, CounterFunction :> (Part[CharacterRange["\:0531", "\:0556"], #]&)],
				"georgian",             CounterBox[style, CounterFunction :> (Part[CharacterRange["\:10d0", "\:10fa"], #]&)],
				"lower-alpha",          CounterBox[style, CounterFunction :> (Part[CharacterRange["a", "z"], #]&)],
				"upper-alpha",          CounterBox[style, CounterFunction :> (Part[CharacterRange["A", "Z"], #]&)],
				"none",                 None,
				_,                      unrecognizedKeyWordFailure @ prop
			],
		_, unrecognizedValueFailure @ prop
	]
	



(* ::Subsubsection::Closed:: *)
(*list-style*)


(* short-hand for list-style-image/position/type properties given in any order *)
consumeProperty[prop:"list-style", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, values, rawValues, noneCount = 0, hasImage = False, hasPos = False, hasType = False},
		(* 
			li-image, li-position, and li-type can appear in any order.
			A value of 'none' sets whichever of li-type and li-image are not otherwise specified to 'none'. 
			If both are specified, then an additional 'none' is an error.
		*)
		values = <|"i" -> <|CellDingbat -> None|>, "p" -> Missing["Not supported."], "t" -> <|CellDingbat -> "\[FilledCircle]"|>|>;
		rawValues = <|"i" -> "none", "p" -> "outside", "t" -> "disc"|>;
		
		While[pos <= l,
			Which[
				(* check for 'none' keyword *)
				TokenStringIs["none", tokens[[pos]]], noneCount++,					
				
				(* check for list-style-image *)
				value = parseSingleBGImage[prop, tokens[[pos]]];
				Or[
					TokenTypeIs["url", tokens[[pos]]],
					!FailureQ[value],
					FailureQ[value] && First[value] === "ImportFailure"
				], 
					If[hasImage, 
						Return @ repeatedPropValueFailure @ "image"
						, 
						values["i"] = <|CellDingbat -> value|>; rawValues["i"] = CSSUntokenize @ tokens[[pos]]; hasImage = True
					],
					
				(* check for list-style-position *)
				!FailureQ[value = parseSingleListStylePosition[prop, tokens[[pos]]]],
					If[hasPos, 
						Return @ repeatedPropValueFailure @ "position"
						, 
						values["p"] = value; rawValues["p"] = CSSUntokenize @ tokens[[pos]]; hasPos = True
					],
					
				(* check for list-style-type *)
				!FailureQ[value = parseSingleListStyleType[prop, tokens[[pos]]]],
					If[hasType, 
						Return @ repeatedPropValueFailure @ "type"
						, 
						values["t"] = <|CellDingbat -> value|>; rawValues["t"] = CSSUntokenize @ tokens[[pos]]; hasType = True
					],
					
				(* anything else is an error *)
				True, Return @ unrecognizedValueFailure @ prop
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		{
			<|"Property" -> "list-style-image",    "Value" -> rawValues["i"], "Interpretation" -> values["i"]|>,
			<|"Property" -> "list-style-type",     "Value" -> rawValues["t"], "Interpretation" -> values["t"]|>,
			<|"Property" -> "list-style-position", "Value" -> rawValues["p"], "Interpretation" -> values["p"]|>}
	]


(* ::Subsubsection::Closed:: *)
(*quotes*)


(*
	Quotes could be implemented using DisplayFunction -> (RowBox[{<open-quote>,#,<close-quote>}]&),
	but only a handful of boxes accept this WL option e.g. DynamicBoxOptions, ValueBoxOptions, and a few others.
	There's also ShowStringCharacters, but this only hides/shows the double quote.
	We treat this then as not available, but we validate the form anyway.
*)
consumeProperty[prop:"quotes", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], v, values = {}, cPos},
		While[pos <= l,
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", 
							If[l > 1, 
								Return @ illegalIdentifierFailure @ tokens[[pos]]["String"]
								, 
								values = {}
							],
						_, Return @ unrecognizedKeyWordFailure @ prop
					],
				"string",
					v = tokens[[pos]]["String"]; cPos = pos; AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[pos <= l && TokenTypeIs["string", tokens[[pos]]], 
						AppendTo[values, {v, tokens[[pos]]["String"]}];
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Expected pairs of strings."|>]
					],
				_, Return @ unrecognizedValueFailure @ prop
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		Missing["Not supported."]
	]


(* ::Subsection::Closed:: *)
(*cursor*)


(* WL uses a TagBox[..., MouseAppearanceTag[""]] instead of an option to indicate mouse appearance *)
consumeProperty[prop:"cursor", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
				"url", parseURI @ tokens[[pos]]["String"],
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, With[{v = value}, <|"MouseAppearance" -> MouseAppearance[#, v]&|>]]
	]		


(* ::Subsection::Closed:: *)
(*display*)


(*
	WL automatically lays out blocks either inline or as nested boxes. 
	If a Cell appears within TextData or BoxData then it is considered inline.
*)
consumeProperty[prop:"display", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
consumeProperty[prop:"float", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"left",  Automatic,
						"right", Automatic,
						"none",  Automatic,
						_,       unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


consumeProperty[prop:"clear", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
consumeProperty[prop:"font", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] :=
	Module[{pos = 1, l = Length[tokens], v, value, temp, hasFontStyle = False, hasFontVariant = False, hasFontWeight = False},
		(* reset font properties to initial values *)
		value = <|
			"st" -> fillInitialShortcutProperty["font-style"], 
			"v"  -> fillInitialShortcutProperty["font-variant"],
			"w"  -> fillInitialShortcutProperty["font-weight"],
			"si" -> fillInitialShortcutProperty["font-size"],
			"f"  -> fillInitialShortcutProperty["font-family"],
			"l"  -> fillInitialShortcutProperty["line-height"]|>;
			
		(* parse and assign new font values *)
		If[l == 1, (* if only one token is present, then it should be a keyword that represents a system font (or font style?) *)
			If[TokenTypeIs["ident", tokens[[pos]]],
				v = 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"caption" | "icon" | "menu" | "small-caption", 
							value["f"]  = <|"Property" -> "font-family", "Value" -> tokens[[pos]]["String"], "Interpretation" -> Dynamic @ CurrentValue["ControlsFontFamily"]|>;
							value["si"] = <|"Property" -> "font-size",   "Value" -> tokens[[pos]]["String"], "Interpretation" -> Dynamic @ CurrentValue["ControlsFontSize"]|>;,
						"message-box" | "status-bar", 
							value["f"]  = <|"Property" -> "font-family", "Value" -> tokens[[pos]]["String"], "Interpretation" -> Dynamic @ CurrentValue["PanelFontFamily"]|>;
							value["si"] = <|"Property" -> "font-size",   "Value" -> tokens[[pos]]["String"], "Interpretation" -> Dynamic @ CurrentValue["PanelFontSize"]|>;,
						"italic" | "oblique", 
							value["st"] = <|"Property" -> "font-style",  "Value" -> tokens[[pos]]["String"], "Interpretation" -> consumeProperty["font-style", tokens, opts]|>;,
						"small-caps",
							value["v"] = <|"Property" -> "font-variant", "Value" -> tokens[[pos]]["String"], "Interpretation" -> consumeProperty["font-variant", tokens, opts]|>;,
						_, unrecognizedKeyWordFailure @ prop
					];
				Return @ Values @ value
				,
				Return @ unrecognizedValueFailure @ prop
			]
		];
		
		(* 
			font-style, font-variant, and font-weight can appear in any order, but are optional.
			"normal" values are skipped since "normal" is the initial value of these properties.
			Besides "normal" the keywords of each property are unique.
			Keep checking for font-size as it indicates the end of style/variant/weight values.
		*)
		While[pos <= l && FailureQ[temp = consumeProperty["font-size", {tokens[[pos]]}, opts]],
			If[Not[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["normal", tokens[[pos]]]],
				Which[
					(* font-style *)
					!FailureQ[v = consumeProperty["font-style", {tokens[[pos]]}, opts]], 
						If[hasFontStyle, Return @ repeatedPropValueFailure @ "font-style"
							,
							hasFontStyle = True; 
							value["st"] = <|"Property" -> "font-style", "Value" -> CSSUntokenize @ tokens[[pos]], "Interpretation" -> v|>;
						],
						
					(* font-weight *)
					!FailureQ[v = consumeProperty["font-weight", {tokens[[pos]]}, opts]], 
						If[hasFontWeight, Return @ repeatedPropValueFailure @ "font-weight"
							,
							hasFontWeight = True; 
							value["w"] = <|"Property" -> "font-weight", "Value" -> CSSUntokenize @ tokens[[pos]], "Interpretation" -> v|>;
						],
						
					(* font-variant *)
					!FailureQ[v = consumeProperty["font-variant", {tokens[[pos]]}, opts]], 
						If[hasFontVariant, Return @ repeatedPropValueFailure @ "font-variant"
							,
							hasFontVariant = True; 
							value["v"] = <|"Property" -> "font-variant", "Value" -> CSSUntokenize @ tokens[[pos]], "Interpretation" -> v|>;
						],
						
					True, Return @ unrecognizedValueFailure @ prop
				];
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		
		(* 
			font-size must appear next; 
			an optional line-height property can immediately follow font-size with a '/' in between (no whitespace allowed) *)
		If[pos > l, Return @ noValueFailure["font-size"], 
			value["si"] = <|"Property" -> "font-size", "Value" -> CSSUntokenize @ tokens[[pos]], "Interpretation" -> temp|>; 
			pos++
		];
		
		(* check for optional line-height *)
		If[pos > l, Return @ noValueFailure["font-family"]];
		If[TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["/", tokens[[pos]]],
			pos++; 
			v = consumeProperty["line-height", {tokens[[pos]]}, opts]; 
			If[FailureQ[v], 
				Return @ v
				, 
				value["l"] = <|"Property" -> "font-family", "Value" -> CSSUntokenize @ tokens[[pos]], "Interpretation" -> v|>; 
				AdvancePosAndSkipWhitespace[pos, l, tokens]];
		];
		
		(* everything else must be a font-family *)
		v = consumeProperty["font-family", tokens[[pos ;;]], opts];
		If[FailureQ[v], 
			Return @ v
			, 
			value["f"] = <|"Property" -> "font-family", "Value" -> StringTrim @ CSSUntokenize @ tokens[[pos ;;]], "Interpretation" -> v|>
		];
		
		Values @ value
	]


(* ::Subsubsection::Closed:: *)
(*font-family*)


consumeProperty[prop:"font-family", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] :=
	Module[{fontTokens, parsed, result},
		fontTokens = DeleteCases[SplitBy[tokens, MatchQ[CSSToken[KeyValuePattern["Type" -> "comma"]]]], {CSSToken[KeyValuePattern["Type" -> "comma"]]}];
		parsed = parseSingleFontFamily /@ fontTokens;
		result = FirstCase[parsed, _Failure, None]; (* FIXME: perhaps use FontSubstitutions here? *)
		If[FailureQ[result], Return @ result];
		result = FirstCase[parsed, _Rule, Failure["UnexpectedParse", <|"Message" -> "No font-family found."|>]];
		If[FailureQ[result], result, <|result|>]
	]

parseSingleFontFamily[tokens:{__?CSSTokenQ}] := parseSingleFontFamily[tokens] =
	Module[
	{
		value, l, pos = 1, font, tokensNoWS,
		generic = {"serif", "sans-serif", "monospace", "fantasy", "cursive"},
		fail = Failure["UnexpectedParse", <|"Message" -> "Font family syntax error."|>]
	},
		tokensNoWS = DeleteCases[tokens, CSSToken[KeyValuePattern["Type" -> "whitespace"]], {1}];
		l = Length[tokensNoWS];
		value =
			Switch[tokensNoWS[[pos]]["Type"],
				"ident", (* if first token is an identifier, then all other tokens must be as well; there could only be a single 'ident' *)
					Which[
						!AllTrue[Through[tokensNoWS["Type"]], StringMatchQ["ident"]], 
							fail,
						l == 1 && MemberQ[generic, tokensNoWS[[pos]]["String"]], 
							parseFontFamilySingleIdent @ tokensNoWS[[pos]]["String"],
						True, 
							font = StringJoin @ Riffle[Through[tokensNoWS["String"]], " "];
							First[Pick[$FontFamilies, StringMatchQ[$FontFamilies, font, IgnoreCase -> True]], Missing["FontAbsent", font]]
					],
				"string", (* must only have a single string token up to the delimiting comma *)
					Which[
						l > 1, fail,
						True,
							font = Through[tokensNoWS["String"]];
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


consumeProperty[prop:"font-size", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
				"dimension",  If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, parseLength[tokens[[pos]], True]],
				"percentage", If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, parsePercentage @ tokens[[pos]]],
				"number",     parseZero @ tokens[[pos]],
				_,            unrecognizedValueFailure @ prop 
			];
		If[FailureQ[value], value, <|FontSize -> value|>]
	]


(* ::Subsubsection::Closed:: *)
(*font-style*)


consumeProperty[prop:"font-style", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal",  Plain,
						"italic",  Italic,
						"oblique", "Oblique",
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|FontSlant -> value|>]
	]


(* ::Subsubsection::Closed:: *)
(*font-variant*)


(* In CSS Fonts Module Level 3 this becomes a shorthand for font-variation-* properties *)
consumeProperty[prop:"font-variant", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal",     "Normal",
						"small-caps", "SmallCaps",
						_,            unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|FontVariations -> <|"CapsType" -> value|>|>]
	]


(* ::Subsubsection::Closed:: *)
(*font-weight*)


(*
	"lighter" and "bolder" are not supported.
	Weight mappings come from https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight.
*)
consumeProperty[prop:"font-weight", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
							Clip[tokens[[pos]]["Value"], {1, 1000}]], 
						Automatic],
				_, Failure["UnexpectedParse", <|"Message" -> "Unrecognized font weight."|>]
			];
		If[FailureQ[value], value, <|FontWeight -> value|>]
	]


(* ::Subsection::Closed:: *)
(*height, width (max/min)*)


(* 
	CSS height/width   --> WL ImageSize 
	CSS max/min-height --> WL ImageSize with UpTo (only max) or {{wmin, wmax}, {hmin, hmax}} (both max and min)
	CSS overflow       --> WL ImageSizeAction
*)
parseSingleSize[prop_String, token_?CSSTokenQ] := parseSingleSize[prop, token] =
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
				"auto", Automatic, (* let Mathematica decide what to do *)
				"none", If[!StringMatchQ[prop, "max-height" | "max-width"], unrecognizedKeyWordFailure @ prop, Infinity],
				_,      unrecognizedKeyWordFailure @ prop
			],
		"dimension",  If[token["Value"] < 0, negativeLengthFailure @ prop, parseLength @ token],
		"number",     parseZero @ token,
		"percentage", parsePercentage @ token, (* should be percentage of height of containing block; not possible in WL *)
		_,            unrecognizedValueFailure @ prop
	]
	
(* min-width and max-width override width property *)
consumeProperty[prop:"width" | "max-width" | "min-width", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			<|ImageSize -> 
				Switch[prop,
					"width",     <|"Width" -> <|"Min" -> value, "Max" -> value|>|>,
					"max-width", <|"Width" -> <|"Max" -> value|>|>,
					"min-width", <|"Width" -> <|"Min" -> value|>|>
				]|>
		]
	]

consumeProperty[prop:"height" | "max-height" | "min-height", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleSize[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			If[NumericQ[value] && !IntegerQ[value], value = Round[value]];
			<|ImageSize -> 
				Switch[prop,
					"height",     <|"Height" -> <|"Min" -> value, "Max" -> value|>|>,
					"max-height", <|"Height" -> <|"Max" -> value|>|>,
					"min-height", <|"Height" -> <|"Min" -> value|>|>
				]|>
		]
	]


(* ::Subsection::Closed:: *)
(*line-height*)


(* 
	Similar to WL LineSpacing, but LineSpacing already takes FontSize into account.
	Thus, we intercept 'em' and 'ex' before getting a dynamic FontSize and we keep 
	the percentage from being wrapped in Scaled. *)
consumeProperty[prop:"line-height", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal", {1.2, 0},
						_,        unrecognizedKeyWordFailure @ prop
					],
				"dimension", 
					If[tokens[[pos]]["Value"] < 0, 
						negativeLengthFailure @ prop
						,
						Switch[tokens[[pos]]["Unit"],
							"em"|"ex", {parseLengthNonRelative @ tokens[[pos]], 0},
							_,         {parseLength @ tokens[[pos]], 0}
						]
					],
				"number",     If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, {tokens[[pos]]["Value"], 0}],
				"percentage", If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, {(tokens[[pos]]["Value"])/100, 0}],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|LineSpacing -> value|>]
	]


(* ::Subsection::Closed:: *)
(*margin(-left, -right, -top, -bottom)*)


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
consumeProperty[prop:"margin-top" | "margin-right" | "margin-bottom" | "margin-left", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleMargin[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			<|ImageMargins ->value, CellMargins -> value, PrintingOptions -> <|"PrintingMargins" -> value|>|>
		]
	]
		
consumeProperty[prop:"margin", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, rawResults = {}, results = {}},
		While[pos <= l,
			value = parseSingleMargin[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; AppendTo[rawResults, CSSUntokenize @ tokens[[pos]]]]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, 
				{
					<|"Property" -> "margin-left",   "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>,
					<|"Property" -> "margin-right",  "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>,
					<|"Property" -> "margin-bottom", "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>,
					<|"Property" -> "margin-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>},
			2, 
				{
					<|"Property" -> "margin-left",   "Value" -> rawResults[[2]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[2]]|>,
					<|"Property" -> "margin-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[2]]|>,
					<|"Property" -> "margin-bottom", "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>,
					<|"Property" -> "margin-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>},
			3, 
				{
					<|"Property" -> "margin-left",   "Value" -> rawResults[[2]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[2]]|>,
					<|"Property" -> "margin-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[2]]|>,
					<|"Property" -> "margin-bottom", "Value" -> rawResults[[3]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[3]]|>,
					<|"Property" -> "margin-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>},
			4, 
				{
					<|"Property" -> "margin-left",   "Value" -> rawResults[[4]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[4]]|>,
					<|"Property" -> "margin-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[2]]|>,
					<|"Property" -> "margin-bottom", "Value" -> rawResults[[3]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[3]]|>,
					<|"Property" -> "margin-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|ImageMargins -> #, CellMargins -> #, PrintingOptions -> <|"PrintingMargins" -> #|>|>& @ results[[1]]|>},
			_, tooManyTokensFailure @ tokens
		]
	]

parseSingleMargin[prop_String, token_?CSSTokenQ] := parseSingleMargin[prop, token] = 
	Switch[token["Type"],
		"ident", 
			Switch[token["String"],
				"auto", Automatic, (* let FE decide what to do *)
				_,      unrecognizedKeyWordFailure @ prop
			],
		"dimension",  parseLength @ token,
		"number",     parseZero @ token,
		"percentage", parsePercentage @ token,
		_,            unrecognizedValueFailure @ prop
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


consumeProperty[prop:"outline-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"invert", CellFrameColor -> Dynamic[If[CurrentValue["MouseOver"], ColorNegate @ CurrentValue[CellFrameColor], Inherited]],
						_,        unrecognizedKeyWordFailure @ prop
					],
				"function",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"rgb" | "rgba" | "hsl" | "hsla", parseSingleColor[prop, tokens[[pos]]],
						_,                               invalidFunctionFailure @ tokens[[pos]]["String"]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-style*)


(* only a solid border is allowed for cells; 'hidden' is not allowed here *)
consumeProperty[prop:"outline-style", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value =
			If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["hidden", tokens[[pos]]],
				unrecognizedKeyWordFailure @ prop
				,
				parseSingleBorderStyle[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*outline-width*)


consumeProperty[prop:"outline-width", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
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


(* currently this is not supported *)
(* Shorthand for outline-width/style/color. 'outline' always sets all 4 edges to be the same. *)
consumeProperty[prop:"outline", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[
	{
		pos = 1, l = Length[tokens], value, 
		values = <|"c" -> Missing["Not supported."], "s" -> Missing["Not supported."], "w" -> Missing["Not supported."]|>, 
		rawValues = <|"c" -> "invert", "s" -> "none", "w" -> "medium"|>, 
		hasColor = False, hasStyle = False, hasWidth = False
	},
		While[pos <= l,
			Which[
				!FailureQ[value = parseSingleColor[prop, tokens[[pos]]]],
					If[hasColor, Return @ repeatedPropValueFailure @ (prop <> "-color")];
					hasColor = True; values["c"] = value; rawValues["c"] = CSSUntokenize @ tokens[[pos]],
				
				!FailureQ[value = parseSingleBorderStyle[prop, tokens[[pos]]]],
					If[hasStyle, Return @ repeatedPropValueFailure @ (prop <> "-style")];
					hasStyle = True; values["s"] = value; rawValues["s"] = CSSUntokenize @ tokens[[pos]],
					
				!FailureQ[value = parseSingleBorderWidth[prop, tokens[[pos]]]],
					If[hasWidth, Return @ repeatedPropValueFailure @ (prop <> "-width")];
					hasWidth = True; values["w"] = value; rawValues["w"] = CSSUntokenize @ tokens[[pos]],
				
				True, unrecognizedValueFailure @ prop						
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		{
			<|"Property" -> "outline-width", "Value" -> rawValues["w"], "Interpretation" -> Missing["Not supported."]|>,
			<|"Property" -> "outline-style", "Value" -> rawValues["s"], "Interpretation" -> Missing["Not supported."]|>,
			<|"Property" -> "outline-color", "Value" -> rawValues["c"], "Interpretation" -> Missing["Not supported."]|>}
	]


(* ::Subsection::Closed:: *)
(*overflow*)


(* 
	This would mostly be found with WL Pane expression as PaneBox supports scrollbars. 
	Other boxes may support ImageSizeAction.
*)
consumeProperty[prop:"overflow", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"visible", Missing["Not supported."],
					"hidden",  <|ImageSizeAction -> "Clip", Scrollbars -> False|>,
					"scroll",  <|ImageSizeAction -> "Clip", Scrollbars -> True|>,
					"auto",    <|ImageSizeAction -> "Scrollable"|>,
					_,         unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsection::Closed:: *)
(*padding(-left, -right, -top, -bottom)*)


(* 
	CSS margins --> WL ImageMargins and CellMargins
	CSS padding --> WL FrameMargins and CellFrameMargins
*)
consumeProperty[
	prop:"padding-top" | "padding-right" | "padding-bottom" | "padding-left", 
	tokens:{__?CSSTokenQ}, 
	opts:OptionsPattern[]
] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSinglePadding[prop, tokens[[pos]]];
		If[FailureQ[value], 
			value
			, 
			<|FrameMargins -> value, CellFrameMargins -> value|>
		]
	]
		
consumeProperty[prop:"padding", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, rawResults = {}, results = {}},
		While[pos <= l,
			value = parseSinglePadding[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]; AppendTo[rawResults, CSSUntokenize @ tokens[[pos]]]]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		Switch[Length[results],
			1, 
				{
					<|"Property" -> "padding-left",   "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>,
					<|"Property" -> "padding-right",  "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>,
					<|"Property" -> "padding-bottom", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>,
					<|"Property" -> "padding-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>},
			2, 
				{
					<|"Property" -> "padding-left",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[2]]|>,
					<|"Property" -> "padding-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[2]]|>,
					<|"Property" -> "padding-bottom", "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>,
					<|"Property" -> "padding-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>},
			3, 
				{
					<|"Property" -> "padding-left",   "Value" -> rawResults[[2]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[2]]|>,
					<|"Property" -> "padding-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[2]]|>,
					<|"Property" -> "padding-bottom", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[3]]|>,
					<|"Property" -> "padding-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>},
			4, 
				{
					<|"Property" -> "padding-left",   "Value" -> rawResults[[4]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[4]]|>,
					<|"Property" -> "padding-right",  "Value" -> rawResults[[2]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[2]]|>,
					<|"Property" -> "padding-bottom", "Value" -> rawResults[[3]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[3]]|>,
					<|"Property" -> "padding-top",    "Value" -> rawResults[[1]], "Interpretation" -> <|FrameMargins -> #, CellFrameMargins -> #|>& @ results[[1]]|>},
			_, tooManyTokensFailure @ tokens
		]
	]


parseSinglePadding[prop_String, token_?CSSTokenQ] := (*parseSinglePadding[prop, token] = *)
	Switch[token["Type"],
		"number",     parseZero @ token,
		"dimension",  If[token["Value"] < 0, negativeLengthFailure @ prop, parseLength @ token],
		"percentage", If[token["Value"] < 0, negativeLengthFailure @ prop, parsePercentage @ token],
		_,            unrecognizedValueFailure @ prop
	]


(* ::Subsection::Closed:: *)
(*page breaks*)


(* ::Subsubsection::Closed:: *)
(*orphans/widows*)


(* 
	FE uses LinebreakAdjustments with a blackbox algorithm. 
	AFAIK there's no way to directly prevent orphans/widows.
*)
consumeProperty[prop:"orphans" | "widows", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", 
					If[tokens[[pos]]["Value"] < 1 || tokens[[pos]]["ValueType"] != "integer", 
						positiveLengthFailure @ prop
						, 
						tokens[[pos]]["Value"]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-after/before*)


consumeProperty[prop:("page-break-after" | "page-break-before"), tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",   Automatic,
						"always", True,
						"avoid",  False,
						"left",   Missing["Not supported."],
						"right",  Missing["Not supported."],
						_,        unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], 
			value
			, 
			<|Switch[prop, "page-break-after", PageBreakBelow, "page-break-before", PageBreakAbove] -> value|>
		]
	]


(* ::Subsubsection::Closed:: *)
(*page-break-inside*)


consumeProperty[prop:"page-break-inside", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",  Automatic, 
						"avoid", False,
						_,       unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], 
			value
			, 
			<|PageBreakWithin -> value, GroupPageBreakWithin -> value|>
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
consumeProperty[prop:"position", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"static",   Automatic, (* normal layout, ignoring any left/right/top/bottom offsets *)
						"relative", Automatic, (* normal layout, offset relative to normal position and floats above "siblings" *)
						"absolute", Automatic, (* non-normal layout, attached cell attached to a parent box with absolute offset *)
						"fixed",    Automatic, (* non-normal layout, notebook-attached-cell in Working mode (ignores scrolling), appears on each page in Printout mode (header/footer?) *)
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


consumeProperty[prop:"left" | "right" | "top" | "bottom", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value =
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", Automatic,
						_,      unrecognizedKeyWordFailure @ prop
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
		If[FailureQ[value] || MissingQ[value], 
			value
			, 
			<|Alignment -> Switch[prop, "left"|"right", {value, Automatic}, "top"|"bottom", {Automatic, value}]|>
		]
	]


(* ::Subsection::Closed:: *)
(*table*)


(* ::Subsubsection::Closed:: *)
(*caption-side*)


(* WL Grid does not support an option to have a grid caption. *)
consumeProperty[prop:"caption-side", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"top",    Automatic,
						"bottom", Automatic,
						_,        unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*empty-cells*)


(* There is no WL equivalent because FE uses only 'border-collapse' in Grid.*)
consumeProperty[prop:"empty-cells", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"show", Automatic,
						"hide", Automatic,
						_,      unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*table-layout*)


(* The FE does its own formatting passes depending on the column width settings and content. *)
consumeProperty[prop:"table-layout", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",  Automatic,
						"fixed", Automatic,
						_,       unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*text*)


(* ::Subsubsection::Closed:: *)
(*direction*)


consumeProperty[prop:"direction", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"ltr", Automatic,
						"rtl", Missing["Not supported."],
						_,     unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*text-align*)


(* WL distinguishes between alignment and justification, but CSS does not *)
consumeProperty[prop:"text-align", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"left",    <|TextAlignment -> Left|>,
					"right",   <|TextAlignment -> Right|>,
					"center",  <|TextAlignment -> Center|>,
					"justify", <|TextJustification -> 1|>,
					_,         unrecognizedKeyWordFailure @ prop
				],
			_, unrecognizedValueFailure @ prop
		]
	]


(* ::Subsubsection::Closed:: *)
(*text-indent*)


consumeProperty[prop:"text-indent", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"dimension",
					Switch[tokens[[pos]]["Unit"],
						"em"|"ex", parseLengthNonRelative @ tokens[[pos]],
						_,         parseLength @ tokens[[pos]]
					],
				"number",     parseZero @ tokens[[pos]],
				"percentage", Missing["Not supported."],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|LineIndent -> value, ParagraphIndent -> value|>]
	]


(* ::Subsubsection::Closed:: *)
(*text-decoration*)


(* WL distinguishes between alignment and justification, but CSS does not *)
(* In CSS Text Decoration Module Level 3 this becomes a shorthand property for text-decoration-color/style/line *)
consumeProperty[prop:"text-decoration", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, values = {}},
		While[pos <= l,
			value =
				Switch[tokens[[pos]]["Type"],
					"ident",
						Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
							"none",         If[pos > 1, tooManyTokensFailure @ "none", Nothing],
							"underline",    "Underline" -> True,
							"overline",     "Overline" -> Missing["Not supported."], (* OverBar is a function, not an option in WL *)
							"line-through", "StrikeThrough" -> True,
							"blink",        "Blink" -> Missing["Not supported."],
							_,              unrecognizedKeyWordFailure @ prop
						],
					_, unrecognizedValueFailure @ prop
				];
			If[FailureQ[value], Return @ value, AppendTo[values, value]];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		<|FontVariations -> <|values|>|>
	]


(* ::Subsubsection::Closed:: *)
(*text-transform*)


consumeProperty[prop:"text-transform", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"capitalize", Missing["Not supported."], (* Not by the FE at least, but see WL Capitalize[..., "AllWords"] *)
					"uppercase",  <|FontVariations -> <|"CapsType" -> "AllCaps"|>|>,
					"lowercase",  <|FontVariations -> <|"CapsType" -> "AllLower"|>|>,
					"none",       <|FontVariations -> <|"CapsType" -> "Normal"|>|>,
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
consumeProperty[prop:"letter-spacing", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal", "Plain",
						_,        unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|FontTracking -> value|>]
	]


(*(* 
	This was originally in CSS 2, but removed in CSS 2.1 due to lack of UA support.
	Added back in Level 3. CSS Fonts Module Level 4 supports percentages as well.
	Mathematica supports both level 3 and 4 features in FontTracking.
*)
consumeProperty[prop:"font-stretch", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
				"percentage", If[tokens[[pos]]["Value"] < 0, negativeValueFailure @ prop, tokens[[pos]]["Value"]/100],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, FontTracking -> value]
	]*)


(* ::Subsubsection::Closed:: *)
(*unicode-bidi*)


consumeProperty[prop:"unicode-bidi", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal",        Automatic,
						"embed",         Missing["Not supported."],
						"bidi-override", Missing["Not supported."],
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
consumeProperty[prop:"word-spacing", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"normal", "Plain",
						_,        unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsubsection::Closed:: *)
(*white-space*)


(* Whitespace is controlled by the Mathematica Front End. The CSS is still validated. *)
consumeProperty[prop:"white-space", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
consumeProperty[prop:"vertical-align", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value1, value2},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value1 = parseBaseline[prop, First @ tokens];
		If[FailureQ[value1], Return @ value1];
		value2 = parseCellBaseline[prop, First @ tokens];
		If[FailureQ[value2], Return @ value2];
		AssociateTo[value1, value2]
	]

(* this is effectively for RowBox alignment *)
parseBaseline[prop:"vertical-align", token_?CSSTokenQ] := parseBaseline[prop, token] = 
	Module[{value (* for Baseline *)},
		(* tooManyTokens failure check occurs in higher-level function consumeProperty["vertical-align",...]*)
		value = 
			Switch[token["Type"],
				"ident",
					Switch[token["String"],
						"baseline",    Baseline -> Baseline,
						"sub",         Baseline -> Bottom,
						"super",       Baseline -> Axis, (* maybe not the best approximation *)
						"top",         Top -> Top, 
						"text-top",    Top -> Scaled[1],
						"middle",      Center -> Scaled[0.66], (* center plus half an "x" height, i.e. 50%+33%/2 = 66% *)
						"bottom",      Bottom -> Bottom,
						"text-bottom", Bottom -> Scaled[0],
						_,             unrecognizedKeyWordFailure @ prop
					],
				"dimension",
					Switch[token["Unit"],
						"em"|"ex", Baseline -> Scaled[parseLengthNonRelative @ token],
						_,         Baseline -> With[{v = parseLength @ token}, Scaled @ Dynamic[v/CurrentValue[FontSize]]]
					],
				"percentage", Baseline -> parsePercentage @ token,
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|BaselinePosition -> value|>]
	]

(* it's unfortunate that CellBaseline is so limited *)
parseCellBaseline[prop:"vertical-align", token_?CSSTokenQ] := parseCellBaseline[prop, token] = 
	Module[{value},
		value = 
			Switch[token["Type"],
				"ident",
					Switch[token["String"],
						"baseline",               Center,
						"middle",                 Baseline,
						"super" | "sub",          Missing["Not supported."],
						"top" | "text-top",       Missing["Not supported."], (* because top of in-line is at baseline of cell *)
						"bottom" | "text-bottom", Bottom,
						_,                        unrecognizedKeyWordFailure @ prop
					],
				"dimension", 
					Switch[token["Unit"],
						"em"|"ex", Missing["Not supported."],
						_,         parseLength @ token (* w.r.t. the top of the in-line cell *)
					],
				"number", parseZero @ token,
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|CellBaseline -> value|>]
	]


(* ::Subsection::Closed:: *)
(*visibility*)


(* 
	WL option ShowContents is only applicable within a StyleBox.
	Often this is implemented using the Invisible function.
*)
consumeProperty[prop:"visibility", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"visible",  True,
						"hidden",   False,
						"collapse", False,
						_,          unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|ShowContents -> value|>]
	]


(* ::Subsection::Closed:: *)
(*z-index*)


(* 
	The FE does its own depth ordering of boxes. 
	Attached cells are ordered by their creation order.
*)
consumeProperty[prop:"z-index", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", Automatic,
						_,      unrecognizedKeyWordFailure @ prop
					],
				"number", 
					If[tokens[[pos]]["ValueType"] != "integer", 
						Failure["BadValue", <|"Message" -> "Expected value is an integer."|>]
						, 
						tokens[[pos]]["Value"]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*FALL THROUGH *)


consumeProperty[prop_String, {}, opts:OptionsPattern[]] := noValueFailure @ prop

consumeProperty[prop_String, _, opts:OptionsPattern[]] := unsupportedValueFailure @ prop


End[] (* End Private Context *)

EndPackage[]