(* Wolfram Language Package *)

BeginPackage["CSSTools`SVG11`", {"CSSTools`"}]
(* Exported symbols added here with SymbolName::usage *)  

Needs["CSSTools`CSSTokenizer`"]; (* keep tokenizer utilities hidden *)
Needs["CSSTools`CSSPropertyInterpreter`"];


Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *)
(*SVG 1.1 Presenter Attributes*)


(* All such attributes can be referenced by CSS. *)


(* SVG 1.1 elements by category *)
animationElements = Sequence["animate", "animateColor", "animateMotion", "animateTransform", "set"];
basicShapes = Sequence["circle", "ellipse", "line", "polyline", "polygon", "rect"];
containerElements = Sequence["a", "defs", "g", "glyph", "marker", "mask", "missing-glyph", "pattern", "svg", "switch", "symbol"];
descriptiveElements = Sequence["desc", "metadata", "title"];
filterPrimitiveElements = Sequence["feBlend", "feColorMatrix", "feComponentTransfer", "feComposite", "feConvolveMatrix", "feDiffuseLighting", "feDisplacementMap", "feFlood", "feGaussianBlur", "feImage", "feMerge", "feMorphology", "feOffset", "feSpecularLighting", "feTile", "feTurbulence"];
fontElements = Sequence["font", "font-face", "font-face-format", "font-face-name", "font-face-src", "font-face-uri", "hkern", "vkern"];
gradientElements = Sequence["linearGradient", "radialGradient"];
graphicsElements = Sequence["circle", "ellipse", "image", "line", "path", "polygon", "polyline", "rect", "text", "use"];
graphicsRefrencingElements = Sequence["image", "use"];
lightSourceElements = Sequence["feDistantLight", "fePointLight", "feSpotLight"];
shapeElements = Sequence["circle", "ellipse", "line", "path", "polygon", "polyline", "rect"];
structuralElements = Sequence["defs", "g", "svg", "symbol", "use"];
textContentChildElements = Sequence["altGlyph", "textPath", "tref", "tspan"];
textContentElements = Sequence["altGlyph", "textPath", "text", "tref", "tspan"];

(* >1.1 categories? *)
renderableElements = Sequence["a", "circle", "ellipse", "foreignObject", "g", "image", "line", "mesh", "path", "polygon", "polyline", "rect", "svg", "switch", "symbol", "text", "textPath", "tspan", "unknown", "use"];
textElements = Sequence["altGlyph", "altGlyphDef", "altGlyphItem", "glyph", "glyphRef", "textPath", "text", "tref", "tspan"];

(* properties already in CSS 2.1 *)
(* 
	The following properties should be identical between SVG 1.1 and CSS 2.1:
	clip, color, cursor, direction, display, 
	font, font-family, font-size, font-style, font-variant, font-weight,
	letter-spacing, overflow, text-decoration, unicode-bidi, visibility, word-spacing *)


SVGPresentationAttributesData = <|
	"alignment-baseline" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|BaselinePosition -> Inherited|>,
			"initial" -> <|BaselinePosition -> Baseline -> Baseline|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "baseline", "before-edge", "text-before-edge", "middle", "central", "after-edge", "text-after-edge", "ideographic", "alphabetic", "hanging", "mathematical"},
		"AppliesTo" -> {textContentChildElements}|>,
	"baseline-shift" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				BaselinePosition -> Inherited,
				CellBaseline     -> Inherited|>,
			"initial" -> <|
				BaselinePosition -> Baseline -> Baseline,
				CellBaseline     -> Center|>|>,  
		"Animatable" -> True,
		"Values" -> {"baseline", "sub", "super", "<percentage>", "<length>"},
		"AppliesTo" -> {textContentChildElements}|>,
 	"clip" -> <|(* depcrecated *)
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"rect()", "auto"},
		"AppliesTo" -> {"svg", "symbol", "image", "foreignObject", "pattern", "marker"}|>,
 	"clip-path" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGClipPath" -> Inherited|>,
			"initial" -> <|"SVGClipPath" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"<FuncIRI>", "none"},
		"AppliesTo" -> {containerElements, graphicsElements, "clipPath"}|>,
 	"clip-rule" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "nonzero",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGClipRule" -> Inherited|>,
			"initial" -> <|"SVGClipRule" -> "NonZero"|>|>,
		"Animatable" -> True,
		"Values" -> {"nonzero", "evenodd"},
		"AppliesTo" -> {graphicsElements (* within a clipPath element *)}|>,
 	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "black", (* depends on user agent; we choose black *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontColor -> Dynamic @ CurrentValue[FontColor]|>,
			"initial" -> <|FontColor -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> {"<color>"},
		"AppliesTo" -> All (* svg 2 recommendation *)|>,
 	"color-interpolation" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "sRGB",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGColorInterpolation" -> Inherited|>,
			"initial" -> <|"SVGColorInterpolation" -> RGBColor|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "sRGB", "linearRGB"},
		"AppliesTo" -> {containerElements, graphicsElements, "animate", "animateColor"}|>,
 	"color-interpolation-filters" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "linearRGB",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGColorInterpolationFilters" -> Inherited|>,
			"initial" -> <|"SVGColorInterpolationFilters" -> XYZColor|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "sRGB", "linearRGB"},
		"AppliesTo" -> {filterPrimitiveElements}|>,
 	"color-profile" -> <|(* deprecated since svg 2 *)
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ColorSpace -> Inherited|>,
			"initial" -> <|ColorSpace -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "sRGB", "<name>", "<funciri>"},
		"AppliesTo" -> {"image" (* ones that refer to raster images *)}|>,
 	"color-rendering" -> <|(* FE doesn't allow rendering options *)
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGColorRendering" -> Inherited|>,
			"initial" -> <|"SVGColorRendering" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "optimizeSpeed", "optimizeQuality"},
		"AppliesTo" -> {containerElements, graphicsElements, "animate", "animateColor"}|>,
 	"cursor" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <|"MouseAppearance" -> (TagBox[#, MouseAppearanceTag["Arrow"]]&)|>|>,
		"Animatable" -> True,
		"Values" -> {"<funciri>", "auto", "crosshair", "default", "pointer", "move", "e-resize", "ne-resize", "nw-resize", "n-resize", "se-resize", "sw-resize", "s-resize", "w-resize", "text", "wait", "help"},
		"AppliesTo" -> {containerElements, graphicsElements}|>,
 	"direction" -> <|(* so far FE only has left-to-right *)
		"Inherited" -> True,
		"CSSInitialValue" -> "ltr",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"ltr", "rtl"},
		"AppliesTo" -> {textContentElements}|>,
 	"display" -> <|(* can set Style[_, Visible -> False] *)
		"Inherited" -> False,
		"CSSInitialValue" -> "inline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"inline", "block", "list-item", "run-in", "compact", "marker", "table", "inline-table", "table-row-group", "table-header-group", "table-footer-group", "table-row", "table-column-group", "table-column", "table-cell", "table-caption", "none"},
		"AppliesTo" -> {"svg", "g", "switch", "a", "foreignObject", graphicsElements, textContentChildElements}|>,
 	"dominant-baseline" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				BaselinePosition -> Inherited,
				CellBaseline     -> Inherited|>,
			"initial" -> <|
				BaselinePosition -> Baseline -> Baseline,
				CellBaseline     -> Center|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "use-script", "no-change", "reset-size", "ideographic", "alphabetic", "hanging", "mathematical", "central", "middle", "text-after-edge", "text-before-edge"},
		"AppliesTo" -> {textContentElements}|>,
 	"enable-background" -> <|(* depcrecated since svg 2.0 *)
		"Inherited" -> False,
		"CSSInitialValue" -> "accumulate",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGEnableBackground" -> Inherited|>,
			"initial" -> <|"SVGEnableBackground" -> "Accumulate"|>|>,
		"Animatable" -> False,
		"Values" -> {"accumulate", "new"},
		"AppliesTo" -> {containerElements}|>,
 	"fill" -> <|(* special case of animation elements: "animate", "animateColor", "animateMotion", "animateTransform", "set" *)
		"Inherited" -> True,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFill" -> Inherited|>,
			"initial" -> <|"SVGFill" -> Black|>|>,
		"Animatable" -> True,
		"Values" -> {"<paint>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"fill-opacity" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFillOpacity" -> Inherited|>,
			"initial" -> <|"SVGFillOpacity" -> 1|>|>,
		"Animatable" -> True,
		"Values" -> {"<opacity-value>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"fill-rule" -> <|(* AFAIK the FE only uses even-odd rules by default *)
		"Inherited" -> True,
		"CSSInitialValue" -> "nonzero",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFillRule" -> Inherited|>,
			"initial" -> <|"SVGFillRule" -> "NonZero"|>|>,
		"Animatable" -> True,
		"Values" -> {"nonzero", "evenodd"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"filter" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFilter" -> Inherited|>,
			"initial" -> <|"SVGFilter" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"<funciri>", "none"},
		"AppliesTo" -> DeleteCases[Union @ {containerElements, graphicsElements}, "mask"]|>,
 	"flood-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFloodColor" -> Inherited|>,
			"initial" -> <|"SVGFloodColor" -> Black|>|>,
		"Animatable" -> True,
		"Values" -> {"currentColor", "<color>", "<icccolor>"},
		"AppliesTo" -> {"feFlood"}|>,
 	"flood-opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFloodOpacity" -> Inherited|>,
			"initial" -> <|"SVGFloodOpacity"-> 1|>|>,
		"Animatable" -> True,
		"Values" -> {"<opacity-value>"},
		"AppliesTo" -> {"feFlood"}|>,
 	"font" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "see individual properties",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				FontFamily     -> Inherited,
				FontSize       -> Inherited,
				FontSlant      -> Inherited,
				FontVariations -> <|"CapsType" -> Inherited|>,
				FontWeight     -> Inherited,
				LineSpacing    -> Inherited|>,
			"initial" -> <|
				FontFamily     -> Dynamic @ CurrentValue[$FrontEnd, {StyleDefinitions, "Text", FontFamily}, "Arial"],
				FontSize       -> Medium,
				FontSlant      -> Plain,
				FontVariations -> <|"CapsType" -> "Normal"|>,
				FontWeight     -> Plain,
				LineSpacing    -> {1.2, 0}|>|>,
		"Animatable" -> True, (* with conditions *)
		"Values" -> {"<family-name>", "<length>", "<percentage>", "normal", "italic", "oblique", "bold", "bolder", "lighter", "100", "200", "300", "400", "500", "600", "700", "800", "900", "small-caps", "caption", "icon", "menu", "message-box", "small-caption", "status-bar", "serif", "sans-serif", "cursive", "fantasy", "monospace", "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "smaller", "larger"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-family" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "arial", (* depends on user agent; we choose arial *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontFamily -> Inherited|>,
			"initial" -> <|FontFamily -> "Arial"|>|>,
		"Animatable" -> True,
		"Values" -> {"<family-name>", "serif", "sans-serif", "cursive", "fantasy", "monospace"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-size" -> <|
		"Inherited" -> "yes, the computed value is inherited",
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontSize -> Inherited|>,
			"initial" -> <|FontSize -> Medium|>|>,
		"Animatable" -> True,
		"Values" -> {"<length>", "<percentage>", "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "smaller", "larger"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-size-adjust" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGFontSizeAdjust" -> Inherited|>,
			"initial" -> <|"SVGFontSizeAdjust" -> None|>|>,
		"Animatable" -> True, (* with conditions *)
		"Values" -> {"<number>", "none"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-stretch" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontTracking -> Inherited|>,
			"initial" -> <|FontTracking -> Plain|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "wider", "narrower", "ultra-condensed", "extra-condensed", "condensed", "semi-condensed", "semi-expanded", "expanded", "extra-expanded", "ultra-expanded"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontSlant -> Inherited|>,
			"initial" -> <|FontSlant -> Plain|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "italic", "oblique"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-variant" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontVariations -> <|"CapsType" -> Inherited|>|>,
			"initial" -> <|FontVariations -> <|"CapsType" -> "Normal"|>|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "small-caps"},
		"AppliesTo" -> {textContentElements}|>,
 	"font-weight" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontWeight -> Inherited|>,
			"initial" -> <|FontWeight -> Plain|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "bold", "bolder", "lighter", "100", "200", "300", "400", "500", "600", "700", "800", "900"},
		"AppliesTo" -> {textContentElements}|>,
 	"glyph-orientation-horizontal" -> <|(* deprecated since svg 2.0 *)
		"Inherited" -> True,
		"CSSInitialValue" -> "0deg",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGGlyphOrientationHorizontal" -> Inherited|>,
			"initial" -> <|"SVGGlyphOrientationHorizontal" -> Quantity[0, "AngularDegrees"]|>|>,
		"Animatable" -> False,
		"Values" -> {"<angle>"},
		"AppliesTo" -> {textContentElements}|>,
 	"glyph-orientation-vertical" -> <|(* deprecated since svg 2.0 *)
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGGlyphOrientationVertical" -> Inherited|>,
			"initial" -> <|"SVGGlyphOrientationVertical" -> Automatic|>|>,
		"Animatable" -> False,
		"Values" -> {"auto", "<angle>"},
		"AppliesTo" -> {textContentElements}|>,
 	"image-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGImageRendering" -> Inherited|>,
			"initial" -> <|"SVGImageRendering" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "optimizeSpeed", "optimizeQuality"},
		"AppliesTo" -> {"image"}|>,
 	"kerning" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGKerning" -> Inherited|>,
			"initial" -> <|"SVGKerning" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "<length>"},
		"AppliesTo" -> {textContentElements}|>,
 	"letter-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontTracking -> Inherited|>,
			"initial" -> <|FontTracking -> "Plain"|>|>,
		"Animatable" -> True,
		"Values" -> {"normal", "<length>"},
		"AppliesTo" -> {textContentElements}|>,
 	"lighting-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "white",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGLightingColor" -> Inherited|>,
			"initial" -> <|"SVGLightingColor" -> White|>|>,
		"Animatable" -> True,
		"Values" -> {"currentColor", "<color>", "<icccolor>"},
		"AppliesTo" -> {"feDiffuseLighting", "feSpecularLightingelements"}|>,
 	"marker" -> <|(* shortand property for marker-*: controlled by Arrowheads[] function *)
		"Inherited" -> True,
		"CSSInitialValue" -> "see individual properties",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGMarkerEnd" -> Inherited, "SVGMarkerMid" -> Inherited, "SVGMarkerStart" -> Inherited|>,
			"initial" -> <|"SVGMarkerEnd" -> None, "SVGMarkerMid" -> None, "SVGMarkerStart" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<funciri>"},
		"AppliesTo" -> {"line", "path", "polygon", "polyline"}|>,
 	"marker-end" -> <|(* controlled by Arrowheads[] function *)
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGMarkerEnd" -> Inherited|>,
			"initial" -> <|"SVGMarkerEnd" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<funciri>"},
		"AppliesTo" -> {"line", "path", "polygon", "polyline"}|>,
 	"marker-mid" -> <|(* controlled by Arrowheads[] function *)
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGMarkerMid" -> Inherited|>,
			"initial" -> <|"SVGMarkerMid" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<funciri>"},
		"AppliesTo" -> {"line", "path", "polygon", "polyline"}|>,
 	"marker-start" -> <|(* controlled by Arrowheads[] function *)
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGMarkerStart" -> Inherited|>,
			"initial" -> <|"SVGMarkerStart" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "<funciri>"},
		"AppliesTo" -> {"line", "path", "polygon", "polyline"}|>,
 	"mask" -> <|(* shorthand for mask-* attributes in CSS Masking Module Level 1 *)
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGMask" -> Inherited|>,
			"initial" -> <|"SVGMask" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"<funciri>", "none"},
		"AppliesTo" -> {containerElements, graphicsElements}|>,
 	"opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGOpacity" -> Inherited|>,
			"initial" -> <|"SVGOpacity" -> 1|>|>,
		"Animatable" -> True,
		"Values" -> {"<opacity-value>"},
		"AppliesTo" -> DeleteCases[{containerElements, graphicsElements}, "mask"]|>,
 	"overflow" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "see prose",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ImageSizeAction -> Inherited|>,
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"visible", "hidden", "scroll", "auto"},
		"AppliesTo" -> {"svg", "symbol", "image", "foreignObject", "pattern", "marker"}|>,
 	"pointer-events" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visiblePainted",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGPointerEvents" -> Inherited|>,
			"initial" -> <|"SVGPointerEvents" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"visiblePainted", "visibleFill", "visibleStroke", "visible |<br>painted", "fill", "stroke", "all", "none"},
		"AppliesTo" -> {graphicsElements}|>,
 	"shape-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGShapeRendering" -> Inherited|>,
			"initial" -> <|"SVGShapeRendering" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "optimizeSpeed", "crispEdges", "geometricPrecision"},
		"AppliesTo" -> {shapeElements}|>,
 	"stop-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStopColor" -> Inherited|>,
			"initial" -> <|"SVGStopColor" -> Black|>|>,
		"Animatable" -> True,
		"Values" -> {"currentColor", "<color>", "<icccolor>"},
		"AppliesTo" -> {"stop"}|>,
 	"stop-opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStopOpacity" -> Inherited|>,
			"initial" -> <|"SVGStopOpacity" -> 1|>|>,
		"Animatable" -> True,
		"Values" -> {"<opacity-value>"},
		"AppliesTo" -> {"stop"}|>,
 	"stroke" -> <|(* shorthand for stroke-* properties *)
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStroke" -> Inherited|>,
			"initial" -> <|"SVGStroke" -> None|>|>,
		"Animatable" -> True,
		"Values" -> {"<paint>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-dasharray" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeDashArray" -> Inherited|>,
			"initial" -> <|"SVGStrokeDashArray" -> None|>|>,
		"Animatable" -> True, (* non-additive *)
		"Values" -> {"none", "<dasharray>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-dashoffset" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeDashOffset" -> Inherited|>,
			"initial" -> <|"SVGStrokeDashOffset" -> 0|>|>,
		"Animatable" -> True,
		"Values" -> {"<percentage>", "<length>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-linecap" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "butt",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeLineCap" -> Inherited|>,
			"initial" -> <|"SVGStrokeLineCap" -> CapForm["Butt"]|>|>,
		"Animatable" -> True,
		"Values" -> {"butt", "round", "square"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-linejoin" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "miter",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeLineJoin" -> Inherited|>,
			"initial" -> <|"SVGStrokeLineJoin" -> JoinForm["Miter"]|>|>,
		"Animatable" -> True,
		"Values" -> {"miter", "round", "bevel"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-miterlimit" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "4",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeMiterLimit" -> Inherited|>,
			"initial" -> <|"SVGStrokeMiterLimit" -> 4|>|>,
		"Animatable" -> True,
		"Values" -> {"<miterlimit>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-opacity" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeOpacity" -> Inherited|>,
			"initial" -> <|"SVGStrokeOpacity" -> 1|>|>,
		"Animatable" -> True,
		"Values" -> {"<opacity-value>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"stroke-width" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGStrokeWidth" -> Inherited|>,
			"initial" -> <|"SVGStrokeWidth" -> AbsoluteThickness[1]|>|>,
		"Animatable" -> True,
		"Values" -> {"<percentage>", "<length>"},
		"AppliesTo" -> {shapeElements, textContentElements}|>,
 	"text-anchor" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "start",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGTextAnchor" -> Inherited|>,
			"initial" -> <|"SVGTextAnchor" -> "Start"|>|>,
		"Animatable" -> True,
		"Values" -> {"start", "middle", "end"},
		"AppliesTo" -> textContentElements|>,
 	"text-decoration" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontVariations -> Inherited|>,
			"initial" -> <|FontVariations -> <||>|>|>,
		"Animatable" -> True,
		"Values" -> {"none", "underline", "overline", "line-through", "blink"},
		"AppliesTo" -> {textContentElements}|>,
 	"text-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGTextRendering" -> Inherited|>,
			"initial" -> <|"SVGTextRendering" -> Automatic|>|>,
		"Animatable" -> True,
		"Values" -> {"auto", "optimizeSpeed", "optimizeLegibility", "geometricPrecision"},
		"AppliesTo" -> {"text"}|>,
 	"unicode-bidi" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> False,
		"Values" -> {"normal", "embed", "bidi-override"},
		"AppliesTo" -> {textContentElements}|>,
 	"visibility" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visible",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|ShowContents -> Inherited|>,
			"initial" -> <|ShowContents -> True|>|>,
		"Animatable" -> True,
		"Values" -> {"visible", "hidden", "collapse"},
		"AppliesTo" -> {graphicsElements, textContentChildElements, "a"}|>,
 	"word-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"normal", "<length>"},
		"AppliesTo" -> {textContentElements}|>,
 	"writing-mode" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "lr-tb",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|"SVGWritingMode" -> Inherited|>,
			"initial" -> <|"SVGWritingMode" -> "LeftToRight"|>|>,
		"Animatable" -> False,
		"Values" -> {"lr-tb", "rl-tb", "tb-rl", "lr", "rl", "tb"},
		"AppliesTo" -> {"text"}|>|>;

If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, SVGPresentationAttributesData];
		
		
(* 
	Applies to alignment of graphics elements. WL's BaselinePosition is similar. 
	WL BaselinePosition takes a rule as its value e.g. BaselinePosition -> Baseline -> Bottom.
	Lengths and percentages are w.r.t. the baseline of the surrounding element.
	Percentages are relative to the font-size i.e. 100% is one line-height upward.
*)
consumeProperty[prop:"alignment-baseline", tokens_?CSSTokenQ, opts:OptionsPattern[]] :=
	Module[{pos = 1, (*l = Length[tokens], *)value},
		If[Length[tokens] > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"baseline" | "auto", Baseline -> Baseline,
						"sub",          Baseline -> Bottom,
						"super",        Baseline -> Axis, (* maybe not the best approximation *)
						"top",          Top -> Top, 
						"text-top" | "before-edge" | "text-before-edge", Top -> Scaled[1],
						"central" | "center", Center -> Scaled[0.5],
						"middle",       Center -> Scaled[0.66], (* center plus half an "x" height, i.e. 50%+33%/2 = 66% *)
						"bottom",       Bottom -> Bottom,
						"text-bottom",  Bottom -> Scaled[0],
						"ideographic",  Baseline -> Bottom,
						"alphabetic",   Baseline -> Baseline,
						"hanging",      Baseline -> Bottom,
						"mathematical", Baseline -> Baseline,					
						_,              unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|BaselinePosition -> value|>]
	]


(* baseline-shift is going to be deprecated in favor of vertical-align *)
consumeProperty[prop:"baseline-shift", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := consumeProperty["vertical-align", tokens]


(* "clip" is already handled in CSSPropertyInterpreter.wl and is mostly deprecated *)


consumeProperty[prop:"clip-path", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", None,
						_,      unrecognizedKeyWordFailure @ prop
					],
				"url", parseURI @ tokens[[pos]]["String"],
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGClipPath" -> value|>]
	]
	
	
consumeProperty[prop:"clip-rule", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"nonzero", "NonZero",
						"evenodd", "EvenOdd",
						_,         unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGClipRule" -> value|>]
	]
	

(* "color" is already handled in CSSPropertyInterpreter.wl *)


consumeProperty[prop:"color-interpolation", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",      RGBColor,
						"srgb",      RGBColor, (* default FE colorspace *)
						"linearrgb", XYZColor,
						_,           unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, <|"SVGColorInterpolation" -> value|>]
	]


consumeProperty[prop:"color-interpolation-filters", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",      XYZColor, 
						"srgb",      RGBColor, 
						"linearrgb", XYZColor,
						_,           unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, <|"SVGColorInterpolationFilters" -> value|>]
	]
	

(* DEPRECATED in CSS Color Module Level 3
	A named color profile is supposed to be defined in an @color-profile rule in the CSS file.
	Most imported images have the color profile already specified or embedded. *)
consumeProperty[prop:"color-profile", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", Automatic, 
						"srgb", "RGB", 
						_,      (*TODO: parse @color-profile*)"NAME"(*Import[tokens[[pos]]["String"], "ICC"]*)				
					],
				"url", parseURI @ tokens[[pos]]["String"], (*TODO: this will probably fail unless the file ends with .icc extension *)
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value] || MissingQ[value], value, <|ColorSpace -> value|>]
	]
	

(* If "Quality" then perform color correction in the given ColorSpace, otherwise use default RGB. *)
consumeProperty[prop:"color-rendering", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",            Automatic, 
						"optimizespeed",   "Speed",
						"optimizequality", "Quality",
						_,                 unrecognizedKeyWordFailure @ prop				
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGColorRendering" -> value|>]
	]
	

(* "color"     is already handled in CSSPropertyInterpreter.wl *)
(* "cursor"    is already handled in CSSPropertyInterpreter.wl *)
(* "direction" is already handled in CSSPropertyInterpreter.wl *)
(* "display"   is already handled in CSSPropertyInterpreter.wl 
	SVG 1.1 includes "marker" and "compact" values, but those are not part of CSS 2.1 *)
	
	
consumeProperty[prop:"dominant-baseline", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",             Automatic, 
						"use-script",       Baseline, 
						"no-change",        Inherited,
						"reset-size",       Baseline,
						"ideographic",      Baseline -> Bottom,
						"alphabetic",       Baseline,
						"hanging",          Top,
						"mathematical",     Bottom -> Scaled[0.66], (* bottom at top of x-height *)
						"central",          Axis,
						"middle",           Center -> Scaled[0.66],
						"text-after-edge",  Bottom -> Scaled[-0.16], (* bottom shifted 1/2 x-height downward *)
						"text-before-edge", Top -> Scaled[-0.16],    (* top shifted 1/2 x-height downward *)
						_,                  unrecognizedKeyWordFailure @ prop				
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|BaselinePosition -> value|>]
	]


(* don't put a lot of effort into this as no one but IE10 supports it*)
consumeProperty[prop:"enable-background", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := Missing["Deprecated."]
	

consumeProperty[prop:"fill", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"url", parseURI @ tokens[[pos]]["String"], (* can reference a gradient or other image as a Texture *)
				_,     parseSingleColor[prop, tokens[[pos]]]
			];
		If[FailureQ[value], value, <|"SVGFill" -> value|>]
	]
	
	
consumeProperty[prop:"fill-opacity", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGFillOpacity" -> Opacity[Clip[value, {0, 1}]]|>]
	]
	

consumeProperty[prop:"fill-rule", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"nonzero", "NonZero",
						"evenodd", "EvenOdd", 
						_,         unrecognizedKeyWordFailure @ prop				
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGFillRule" -> value|>]
	]
	

(* Filter effects are like blur, coloring, etc. that WL can apply via an Image transform applied to a raster. *)
consumeProperty[prop:"filter", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", None, 
						_,      unrecognizedKeyWordFailure @ prop				
					],
				"url", parseURI @ tokens[[pos]]["String"], (* can reference a pre-defined filter *)
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGFilter" -> value|>]
	]
	

(* only applies to feFlood elements *)
consumeProperty[prop:"flood-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], value, <|"SVGFloodColor" -> value|>]
	]


(* only applies to feFlood elements *)
consumeProperty[prop:"flood-opacity", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGFloodOpacity" -> Opacity[Clip[value, {0, 1}]]|>]
	]
	

(* "font"         is already handled in CSSPropertyInterpreter.wl *)
(* "font-family"  is already handled in CSSPropertyInterpreter.wl *)
(* "font-size"    is already handled in CSSPropertyInterpreter.wl *)
(* "font-style"   is already handled in CSSPropertyInterpreter.wl *)
(* "font-variant" is already handled in CSSPropertyInterpreter.wl *)
(* "font-weight"  is already handled in CSSPropertyInterpreter.wl *)


(* this is not part of the CSS 2.1 specification *)
consumeProperty[prop:"font-size-adjust", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", None, 
						_,      unrecognizedKeyWordFailure @ prop				
					],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGFontSizeAdjust" -> value|>]
	]
	

(* font-stretch
	This was originally in CSS 2, but removed in CSS 2.1 due to lack of UA support.
	Added back in Level 3. CSS Fonts Module Level 4 supports percentages as well.
	Mathematica supports both level 3 and 4 features in FontTracking. *)
consumeProperty[prop:"font-stretch", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
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
				"percentage", (* technically this is a SVG 2 feature *) 
					With[{p = parsePercentage @ tokens[[pos]]},
						If[!FailureQ[p] && First[p] < 0, Failure["BadPercentage", <|"Message" -> prop <> "percentage must be non-negative."|>], p]],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|FontTracking -> value|>]
	]
	

(* deprecated in SVG 2 *)
consumeProperty[prop:"glyph-orientation-horizontal", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"dimension", parseAngle @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGGlyphOrientationHorizontal" -> value|>]
	]
	

(* deprecated in SVG 2 *)
consumeProperty[prop:"glyph-orientation-vertical", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", Automatic,
						_,      unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseAngle @ tokens[[pos]],
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGGlyphOrientationVertical" -> value|>]
	]
	
	
consumeProperty[prop:"image-rendering", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",            Automatic,
						"optimizespeed",   "Speed",
						"optimizequality", "Quality",
						_,                 unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGImageRendering" -> value|>]
	]
	
	
(* deprecated in SVG 2 *)
consumeProperty[prop:"kerning", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", Automatic,
						_,      unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]],
				"number",    parseNumber @ tokens[[pos]], (* if no units are present, then the number represents units in the user coordinate system *)
				_,           unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGKerning" -> value|>]
	]
	
	
(* "letter-spacing" is already handled in CSSPropertyInterpreter.wl *)


(* only used in primitives feDiffuseLighting and feSpecularLighting *)
consumeProperty[prop:"lighting-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], value, <|"SVGLightingColor" -> value|>]
	]
	


parseSingleMarker[prop_String, tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens]},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"none", None, 
					_,      unrecognizedKeyWordFailure @ prop				
				],
			"url", parseURI @ tokens[[pos]]["String"], (* can reference a pre-defined PointMarker *)
			_,     unrecognizedValueFailure @ prop
		]
	]


consumeProperty[prop:"marker-end", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value},
		value = parseSingleMarker[prop, tokens];
		If[FailureQ[value], value, <|"SVGMarkerEnd" -> value|>]
	]

consumeProperty[prop:"marker-mid", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value},
		value = parseSingleMarker[prop, tokens];
		If[FailureQ[value], value, <|"SVGMarkerMid" -> value|>]
	]

consumeProperty[prop:"marker-start", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value},
		value = parseSingleMarker[prop, tokens];
		If[FailureQ[value], value, <|"SVGMarkerStart" -> value|>]
	]

consumeProperty[prop:"marker", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{value},
		value = parseSingleMarker[prop, tokens];
		If[FailureQ[value], value, <|"SVGMarkerEnd" -> value, "SVGMarkerMid" -> value, "SVGMarkerStart" -> value|>]
	]


consumeProperty[prop:"mask", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] :=
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"none", None, 
						_,      unrecognizedKeyWordFailure @ prop				
					],
				"url", parseURI @ tokens[[pos]]["String"], (* can reference a pre-defined PointMarker *)
				_,     unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGMask" -> value|>]
	]
	
	
consumeProperty[prop:"opacity", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGOpacity" -> Opacity[Clip[value, {0, 1}]]|>]
	]


(* "overflow" is already handled in CSSPropertyInterpreter.wl *)

	
consumeProperty[prop:"pointer-events", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"visiblePainted", Automatic,
						"visiblefill",    "VisibleFill",
						"visiblestroke",  "VisibleStroke",
						"visible",        "Visible",
						"painted",        "Painted",
						"fill",           "Fill",
						"stroke",         "SVGStroke",
						"all",            All,
						"none",           None,
						_,                unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGPointerEvents" -> value|>]
	]
	
	
consumeProperty[prop:"shape-rendering", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",               Automatic,
						"optimizespeed",      "OptimizeSpeed",
						"cripsedges",         "CrispEdges",
						"geometricprecision", "GeometricPrecision",
						_,                    unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGShapeRendering" -> value|>]
	]
	
	
(* only used in 'stop' elements *)
consumeProperty[prop:"stop-color", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], value, <|"SVGStopColor" -> value|>]
	]
	
	
(* only used in 'stop' elements *)
consumeProperty[prop:"stop-opacity", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStopOpacity" -> Opacity[Clip[value, {0, 1}]]|>]
	]
	
	
consumeProperty[prop:"stroke", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = parseSingleColor[prop, tokens[[pos]]];
		If[FailureQ[value], value, <|"SVGStroke" -> value|>]
	]


consumeProperty[prop:"stroke-dasharray", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value, results = {}},
		If[l == 1 && TokenTypeIs["ident", tokens[[pos]]],
			Return @ If[TokenStringIs["none", tokens[[pos]]], <|"SVGStrokeDashArray" -> {}|>], unrecognizedKeyWordFailure @ prop]; 
		While[pos <= l,
			value = parseSingleDashArray[prop, tokens[[pos]]];
			If[FailureQ[value], Return @ value, AppendTo[results, value]]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			If[pos < l && TokenTypeIs["comma", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]]
		];
		<|"SVGStrokeDashArray" -> If[AllTrue[results, TrueQ[# == 0]&], {}, results]|>
	]

parseSingleDashArray[prop_String, token_?CSSTokenQ] :=
	Switch[token["Type"],
		"number",     If[token["Value"] < 0, negativeLengthFailure @ prop, parseNumber @ token],
		"dimension",  If[token["Value"] < 0, negativeLengthFailure @ prop, parseLength @ token],
		"percentage", If[token["Value"] < 0, negativeLengthFailure @ prop, parsePercentage @ token],
		_,            unrecognizedValueFailure @ prop
	]
	
consumeProperty[prop:"stroke-dashoffset", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number",     parseNumber @ tokens[[pos]],
				"dimension",  parseLength @ tokens[[pos]],
				"percentage", parsePercentage @ tokens[[pos]],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeDashOffset" -> value|>]
	]
	
consumeProperty[prop:"stroke-linecap", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"butt",   CapForm["Butt"],
						"round",  CapForm["Round"],
						"square", CapForm["Square"],
						_,        unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeLineCap" -> value|>]
	]
	
consumeProperty[prop:"stroke-linejoin", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"miter", JoinForm["Miter"],
						"round", JoinForm["Round"],
						"bevel", JoinForm["Bevel"],
						_,       unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeLineJoin" -> value|>]
	]
	
consumeProperty[prop:"stroke-miterlimit", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", With[{n = parseNumber}, If[n < 1, Failure["BadNumber", <|"Message" -> prop <> "must be greater than 1."|>], n]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeMiterLimit" -> value|>]
	]
	
consumeProperty[prop:"stroke-opacity", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number", parseNumber @ tokens[[pos]],
				_,        unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeOpacity" -> Opacity[Clip[value, {0, 1}]]|>]
	]
	
consumeProperty[prop:"stroke-width", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] :=
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"number",     If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, parseNumber @ tokens[[pos]]],
				"dimension",  If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, parseLength @ tokens[[pos]]],
				"percentage", If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, parsePercentage @ tokens[[pos]]],
				_,            unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGStrokeWidth" -> value|>]
	]
	
	
consumeProperty[prop:"text-anchor", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] :=
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"start",  "Start",
						"middle", "Middle",
						"end",    "End",
						_,        unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGTextAnchor" -> value|>]
	]
	
	
(* "text-decoration" is already handled in CSSPropertyInterpreter.wl *)
	

consumeProperty[prop:"text-rendering", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",               Automatic,
						"optimizespeed",      "OptimizeSpeed",
						"optimizelegibility", "OptimizeLegibility",
						"geometricprecision", "GeometricPrecision",
						_,                    unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGTextRendering" -> value|>]
	]
	

(* "unicode-bidi" is already handled in CSSPropertyInterpreter.wl *)
(* "visibility"   is already handled in CSSPropertyInterpreter.wl *)
(* "word-spacing" is already handled in CSSPropertyInterpreter.wl *)


consumeProperty[prop:"writing-mode", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"lr-tb" | "lr", "LeftToRight",
						"rl-tb" | "rl", "RightToLeft",
						"tb-rl" | "tb", "TopToBottom",
						_,              unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, <|"SVGWritingMode" -> value|>]
	]
	
	
	
End[] (* End Private Context *)


EndPackage[]
