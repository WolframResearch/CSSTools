(* ::Package:: *)

(* ::Section:: *)
(*SVG 1.1 Presenter Attributes*)


(* All such attributes can be referenced by CSS. *)


SVGPresentationAttributesData = <|
	"alignment-baseline" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "see property description",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|
				BaselinePosition -> Inherited,
				CellBaseline     -> Inherited|>,
			"initial" -> <|
				BaselinePosition -> Baseline -> Baseline,
				CellBaseline     -> Center|>|>,
		"Animatable" -> True,
		"Values" -> "auto | baseline | before-edge | text-before-edge | middle | central | after-edge | text-after-edge | ideographic | alphabetic | hanging | mathematical",
		"AppliesTo" -> "tspan, tref, altGlyph, textPath elements"|>,
	"baseline-shift" -> <|
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
		"Values" -> "baseline | sub | super | <percentage>| <length>",
		"AppliesTo" -> "tspan, tref, altGlyph, textPath elements"|>,
 	"clip" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> "<shape>| auto",
		"AppliesTo" -> "elements which establish a new viewport, pattern elements and marker elements"|>,
 	"clip-path" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> "<funciri> | none",
		"AppliesTo" -> "container elements and graphics elements"|>,
 	"clip-rule" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "nonzero",
		"InterpretedGlobalValues" -> <|
			"inherit" -> Missing["Not supported."],
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> "nonzero | evenodd",
		"AppliesTo" -> "graphics elements within a clipPath element"|>,
 	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "depends on user agent",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <|FontColor -> Dynamic @ CurrentValue[FontColor]|>,
			"initial" -> <|FontColor -> Dynamic @ CurrentValue[FontColor]|>|>,
		"Animatable" -> True,
		"Values" -> "<color>",
		"AppliesTo" -> "elements to which properties fill, stroke, stop-color, flood-color, lighting-color apply"|>,
 	"color-interpolation" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "sRGB",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>, (*TODO: STARTHERE*)
		"Animatable" -> True,
		"Values" -> "auto | sRGB | linearRGB",
		"AppliesTo" -> "container elements, graphics elements and animateColor"|>,
 	"color-interpolation-filters" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "linearRGB",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | sRGB | linearRGB",
		"AppliesTo" -> "filter primitives"|>,
 	"color-profile" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | sRGB | <name> | <funciri>",
		"AppliesTo" -> "image elements that refer to raster images"|>,
 	"color-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | optimizeSpeed | optimizeQuality",
		"AppliesTo" -> "container elements, graphics elements and animateColor"|>,
 	"cursor" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "[ [<funciri>,]* [ auto | crosshair | default | pointer | move | e-resize | ne-resize | nw-resize | n-resize | se-resize | sw-resize | s-resize | w-resize| text | wait | help ] ]",
		"AppliesTo" -> "container elements and graphics elements"|>,
 	"direction" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "ltr",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "ltr | rtl",
		"AppliesTo" -> "text content elements"|>,
 	"display" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "inline",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "inline | block | list-item | run-in | compact | marker | table | inline-table | table-row-group | table-header-group | table-footer-group | table-row | table-column-group | table-column | table-cell | table-caption | none",
		"AppliesTo" -> "svg, g, switch, a, foreignObject, graphics elements (including the text element) and text sub-elements (i.e., tspan, tref, altGlyph, textPath)"|>,
 	"dominant-baseline" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | use-script | no-change | reset-size | ideographic | alphabetic | hanging | mathematical | central | middle | text-after-edge | text-before-edge",
		"AppliesTo" -> "text content elements"|>,
 	"enable-background" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "accumulate",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "accumulate | new [ <x> <y> <width><height> ]",
		"AppliesTo" -> "container elements"|>,
 	"fill" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<paint> (See Specifyingpaint)",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"fill-opacity" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<opacity-value>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"fill-rule" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "nonzero",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "nonzero | evenodd",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"filter" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<funciri> | none",
		"AppliesTo" -> "container elements and graphics elements"|>,
 	"flood-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "currentColor |<br><color>[<icccolor>]|<br>inherit",
		"AppliesTo" -> "feFlood elements"|>,
 	"flood-opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<opacity-value>",
		"AppliesTo" -> "feFlood elements"|>,
 	"font" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "see individual properties",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True, (* with conditions *)
		"Values" -> "[ [ font-style|| font-variant|| font-weight]? font-size[ /'line-height']? font-family] | caption | icon | menu | message-box | small-caption | status-bar",
		"AppliesTo" -> "text content elements"|>,
 	"font-family" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "depends on user agent",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "[[<family-name>|<generic-family>],]* [<family-name>|<generic-family>]",
		"AppliesTo" -> "text content elements"|>,
 	"font-size" -> <|
		"Inherited" -> "yes, the computed value is inherited",
		"CSSInitialValue" -> "medium",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<absolute-size>|<relative-size>| <length>|<percentage>",
		"AppliesTo" -> "text content elements"|>,
 	"font-size-adjust" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True, (* with conditions *)
		"Values" -> "<number>| none",
		"AppliesTo" -> "text content elements"|>,
 	"font-stretch" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | wider | narrower | ultra-condensed | extra-condensed | condensed | semi-condensed | semi-expanded | expanded | extra-expanded | ultra-expanded",
		"AppliesTo" -> "text content elements"|>,
 	"font-style" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | italic | oblique",
		"AppliesTo" -> "text content elements"|>,
 	"font-variant" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | small-caps",
		"AppliesTo" -> "text content elements"|>,
 	"font-weight" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | bold | bolder | lighter | 100 | 200 | 300 |400 | 500 | 600 | 700 | 800 | 900",
		"AppliesTo" -> "text content elements"|>,
 	"glyph-orientation-horizontal" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0deg",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "<angle>",
		"AppliesTo" -> "text content elements"|>,
 	"glyph-orientation-vertical" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "auto | <angle>",
		"AppliesTo" -> "text content elements"|>,
 	"image-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | optimizeSpeed | optimizeQuality",
		"AppliesTo" -> "images"|>,
 	"kerning" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | <length>",
		"AppliesTo" -> "text content elements"|>,
 	"letter-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | <length>",
		"AppliesTo" -> "text content elements"|>,
 	"lighting-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "white",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "currentColor |<br><color>[<icccolor>]|<br>inherit",
		"AppliesTo" -> "feDiffuseLighting andfeSpecularLightingelements"|>,
 	"marker" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "see individual properties",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "see individual properties",
		"AppliesTo" -> "path, line, polyline and polygon elements"|>,
 	"marker-end<br>marker-mid<br>marker-start" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "none |<br>inherit|<br><funciri>",
		"AppliesTo" -> "path, line, polyline and polygon elements"|>,
 	"mask" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<funciri> | none",
		"AppliesTo" -> "container elements and graphics elements"|>,
 	"opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<opacity-value>",
		"AppliesTo" -> "container elements and graphics elements"|>,
 	"overflow" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "see prose",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "visible | hidden | scroll | auto",
		"AppliesTo" -> "elementswhich establish a new viewport, pattern elements and marker elements"|>,
 	"pointer-events" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visiblePainted",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "visiblePainted | visibleFill | visibleStroke | visible |<br>painted | fill | stroke | all | none",
		"AppliesTo" -> "graphics elements"|>,
 	"shape-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | optimizeSpeed | crispEdges |<br>geometricPrecision",
		"AppliesTo" -> "shapes"|>,
 	"stop-color" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "black",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "currentColor |<br><color>[<icccolor>]|<br>inherit",
		"AppliesTo" -> "stop elements"|>,
 	"stop-opacity" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<opacity-value>",
		"AppliesTo" -> "stop elements"|>,
 	"stroke" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<paint> (See Specifyingpaint)",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-dasharray" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True, (* with conditions *)
		"Values" -> "none | <dasharray>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-dashoffset" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "0",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<percentage> | <length>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-linecap" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "butt",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "butt | round | square",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-linejoin" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "miter",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "miter | round | bevel",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-miterlimit" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "4",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<miterlimit>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-opacity" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<opacity-value>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"stroke-width" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "1",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "<percentage> | <length>",
		"AppliesTo" -> "shapes and text content elements"|>,
 	"text-anchor" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "start",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "start | middle | end",
		"AppliesTo" -> "text content elements"|>,
 	"text-decoration" -> <|
		"Inherited" -> "no (see prose)",
		"CSSInitialValue" -> "none",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "none | [ underline || overline || line-through | | blink ]",
		"AppliesTo" -> "text content elements"|>,
 	"text-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "auto | optimizeSpeed | optimizeLegibility |<br>geometricPrecision",
		"AppliesTo" -> "text elements"|>,
 	"unicode-bidi" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "normal | embed | bidi-override",
		"AppliesTo" -> "text content elements"|>,
 	"visibility" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "visible",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "visible | hidden | collapse",
		"AppliesTo" -> "graphics elements (including the text element) and textsub-elements (i.e., tspan, tref, altGlyph, textPath and a)"|>,
 	"word-spacing" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "normal",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> True,
		"Values" -> "normal | <length>",
		"AppliesTo" -> "text content elements"|>,
 	"writing-mode" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "lr-tb",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>,
			"initial" -> <||>|>,
		"Animatable" -> False,
		"Values" -> "lr-tb | rl-tb | tb-rl | lr | rl | tb",
		"AppliesTo" -> "text elements"|>|>;
