(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSPagedMedia3`"];

consumeAtPageRule;

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs, CSSTokenString
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSPropertyInterpreter` 
	---> defines CSS wrappers like CSSHeightMin
	---> defines consumeProperty and CSSPropertyData *)
(* CSSImport`
	---> defines consumeDeclaration *)

Needs["CSSTools`CSSTokenizer`"]; 
Needs["CSSTools`CSSPropertyInterpreter`"];
Needs["CSSTools`CSSImport`"]; 
Needs["CSSTools`"]; (* ResolveCSSInterpretations *)

(* Package Notes: implements https://drafts.csswg.org/css-page-3
	This package extends the CSS Tools to include CSS Paged Media Module Level 3 (currently only a CSS working draft). 
	It adds 4 properties to CSSPropretyInterpreter`:
		page:  references named page properties [FE not supported]
		size:  set the size of the page box's containing block i.e. the page sheet
		bleed: set whether page box content extends outside of the page box margins and into the page sheet [FE not supported]
		marks: set whether to show crop marks if the page box size is smaller than the page sheet size [FE partial support]
	TODO: It also modifies how the CSS page properties merge while resolving the CSS cascade. 
	All CSS @page options fall under the FE option "PrintingOptions" so must be carefully merged.
*)

Begin["`Private`"]; (* Begin Private Context *) 

(* we assume that the @page rule has already been tokenized *)


(* ::Section:: *)
(*Allowed CSS 2.1 Properties*)


$ApplicableCSSPageProperties = {
	(*bidi*)
	"direction",
	(*background*)
	"background-color", "background-image", "background-repeat", "background-attachment", "background-position", "background",
	(*border*)
	"border-top-width", "border-right-width", "border-bottom-width", "border-left-width", "border-width",
	"border-top-color", "border-right-color", "border-bottom-color", "border-left-color", "border-color",
	"border-top-style", "border-right-style", "border-bottom-style", "border-left-style", "border-short-style",
	"border-top", "border-right", "border-bottom", "border-left", "border",
	(*counter*)
	"counter-reset", "counter-increment",
	(*color*)
	"color",
	(*font*)
	"font-family", "font-size", "font-style", "font-variant", "font-weight", "font",
	(*height*)
	"height", "min-height", "max-height",
	(*line height*)
	"line-height",
	(*margins*)
	"margin-top", "margin-right", "margin-bottom", "margin-left", "margin",
	(*outlines*)
	"outline-width", "outline-style", "outline-color", "outline",
	(*padding*)
	"padding-top", "padding-right", "padding-bottom", "padding-left", "padding",
	(*quotes*)
	"quotes",
	(*text*)
	"letter-spacing", "text-align", "text-decoration", "text-indent", "text-transform", "white-space", "word-spacing",
	(*visibility*)
	"visibility",
	(*width*)
	"width", "min-width", "max-width"};


$ApplicableCSSPageMarginProperties = {
	(*bidi properties*)
	"direction", "unicode-bidi",
	(*background properties*)
	"background-color", "background-image", "background-repeat", "background-attachment", "background-position", "background",
	(*border properties*)
	"border-top-width", "border-right-width", "border-bottom-width", "border-left-width", "border-width", 
	"border-top-color", "border-right-color", "border-bottom-color", "border-left-color", "border-color",
	"border-top-style", "border-right-style", "border-bottom-style", "border-left-style", "border-short-style",
	"border-top", "border-right", "border-bottom", "border-left", "border",
	(*counter properties*)
	"counter-reset", "counter-increment", 
	(*content*)
	"content", 
	(*color*)
	"color",
	(*font properties*)
	"font-family", "font-size", "font-style", "font-variant", "font-weight", "font", 
	(*height properties*)
	"height", "min-height", "max-height", 
	(*line height*)
	"line-height", 
	(*margin properties*)
	"margin-top", "margin-right", "margin-bottom", "margin-left", "margin",
	(*outline properties*)	
	"outline-width", "outline-style", "outline-color", "outline", 
	(*overflow*)
	"overflow",
	(*padding properties*)
	"padding-top", "padding-right", "padding-bottom", "padding-left", "padding", 
	(*quotes*)
	"quotes",
	(*text properties*)
	"letter-spacing", "text-align", "text-decoration", "text-indent", "text-transform", "white-space", "word-spacing",
	(*vertical align*)
	"vertical-align",
	(*visibility*)
	"visibility",
	(*width properties*)
	"width", "min-width", "max-width",
	(*z index*)
	"z-index"};


$ValidPageMargins = {
	"top-left-corner", "top-left", "top-center", "top-right", "top-right-corner", 
	"bottom-left-corner", "bottom-left", "bottom-center", "bottom-right", "bottom-right-corner",
	"left-top", "left-middle", "left-bottom", 
	"right-top", "right-middle", "right-bottom"};


(* ::Section:: *)
(*New Properties*)


(* new CSS properties *)
If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, 
	{
		"page" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> {}, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>,
		"bleed" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> {}, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>,
		"marks" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "none",
			"InterpretedGlobalValues" -> <|
				"inherit" -> PrintingOptions -> {"PrintRegistrationMarks" -> Inherited}, 
				"initial" -> PrintingOptions -> {"PrintRegistrationMarks" -> False}|>|>,
		"size" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> 
					PrintingOptions -> {
						"PageSize"  -> Inherited,
						"PaperSize" -> Inherited},
				"initial" -> 
					PrintingOptions -> {
						"PageSize"  -> {Automatic, Automatic},
						"PaperSize" -> {Automatic, Automatic}}|>|>}];


(* ::Subsection:: *)
(*Size*)


(* new interpreters *)
(* Notes on page box size vs paper size
	The page consists of the page sheet (medium on which the content is printed) which has its own size. 
		WL: PrintingOptions -> {"PaperSize" -> {<<width>>, <<height>>}} where size is in points (72 points per inch)		
	The page box contains the page content and margins; its size can be separately specified. 
		WL: PrintingOptions -> {"PageSize" -> {<<width>>, <<height>>}} where size is in points (72 points per inch)		
	Normally these two are the same size, but they don't have to be. 
	AFAICT the CSS does not control the size of the page box, only the paper size. *)
consumeProperty[prop:"size", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], paperSizes, paperOrientations, value1 = {}, value2 = {}},
		paperSizes = {"a5", "a4", "a3", "b5", "b4", "jis-b5", "jis-b4", "letter", "legal", "ledger"};
		paperOrientations = {"landscape", "portrait"};
		Which[
			(* case of page size possibly followed by orientation *)
			TokenTypeIs["ident", pos, tokens] && TokenStringIs[Alternatives @@ paperSizes, pos, tokens],
				value1 = parseSinglePaperSize[prop, tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* check for orientation *)
				If[pos <= l && TokenTypeIs["ident", pos, tokens],
					value2 = parseSinglePaperOrientation[prop, tokens[[pos]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				];
			,
			(* case of orientation possibly followed by page size *)
			TokenTypeIs["ident", pos, tokens] && TokenStringIs[Alternatives @@ paperOrientations, pos, tokens],
				value1 = parseSinglePaperOrientation[prop, tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* check for paper size *)
				If[pos <= l && TokenTypeIs["ident", pos, l],
					value2 = parseSinglePaperSize[prop, tokens[[pos]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				];
			,
			(* case of single keyword *)
			TokenTypeIs["ident", pos, tokens],
				value1 = 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"auto", {"PageSize" -> {Automatic, Automatic}, "PaperSize" -> {Automatic, Automatic}},
						_,      unrecognizedKeyWordFailure @ prop
					]
			,
			(* case of lengths *)
			TokenTypeIs["dimension", pos, tokens],
				value1 = If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, convertDPItoPrintersPoints @ parseLength @ tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[FailureQ[value1], Return @ value1, value1 = {value1}];
				(* check for separate height value *)
				If[pos <= l && TokenTypeIs["dimension", pos, tokens],
					value2 = If[CSSTokenValue @ tokens[[pos]] < 0, negativeLengthFailure @ prop, convertDPItoPrintersPoints @ parseLength @ tokens[[pos]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[FailureQ[value2], Return @ value2, value2 = {value2}];
					,
					value2 = value1
				];
				value1 = {"PageSize" -> Join[value1, value2], "PaperSize" -> Join[value1, value2]};
				value2 = {};
			,
			(* fallthrough *)
			True, value1 = unrecognizedValueFailure @ prop
		];
		If[FailureQ[value1], Return @ value1];
		If[FailureQ[value2], Return @ value2];
		"PrintingOptions" -> Join[value1, value2]
	]
	
parseSinglePaperSize[prop_String, token_?CSSTokenQ] :=
	Switch[ToLowerCase @ CSSTokenString @ token,
		"a5",     {"PageSize" -> {419.528, 595.276}, "PaperSize" -> {419.528, 595.276}},
		"a4",     {"PageSize" -> {595.276, 841.89},  "PaperSize" -> {595.276, 841.89}},
		"a3",     {"PageSize" -> {841.89,  1190.55}, "PaperSize" -> {841.89,  1190.55}},
		"b5",     {"PageSize" -> {498.898, 708.661}, "PaperSize" -> {498.898, 708.661}},
		"b4",     {"PageSize" -> {708.661, 1000.63}, "PaperSize" -> {708.661, 1000.63}},
		"jis-b5", {"PageSize" -> {515.906, 728.504}, "PaperSize" -> {515.906, 728.504}},
		"jis-b4", {"PageSize" -> {728.504, 1031.81}, "PaperSize" -> {728.504, 1031.81}},
		"letter", {"PageSize" -> {612.,    792.},    "PaperSize" -> {612.,    792.}},
		"legal",  {"PageSize" -> {612.,    1008.},   "PaperSize" -> {612.,    1008.}},
		"ledger", {"PageSize" -> {792.,    1224.},   "PaperSize" -> {792.,    1224.}},
		_,        unrecognizedKeyWordFailure @ prop
	]
	
parseSinglePaperOrientation[prop_String, token_?CSSTokenQ] :=
	Switch[ToLowerCase @ CSSTokenString @ token,
		"portrait",  {"PaperOrientation" -> "Portrait"},
		"landscape", {"PaperOrientation" -> "Landscape"},
		_            unrecognizedKeyWordFailure @ prop
	]
	
convertDPItoPrintersPoints[val_] := 
	With[{dpi = "Resolution" /. First[SystemInformation["Devices", "ScreenInformation"], "Resolution" -> 72]},
		val/dpi*72
	]
convertDPItoPrintersPoints[Dynamic[val_]] := Dynamic[val] 


(* ::Subsection:: *)
(*Marks*)


(* 
	The FE does not support the 'cross' CSS value. 
	The FE does support 'crop', but only draws the crop marks in the upper-left corner. 
	Because cross and crop can be specified together, ignore the cross value unless there is a parsing error.*)
consumeProperty[prop:"marks", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		Switch[CSSTokenType @ tokens[[pos]],
			"ident",
				Switch[CSSTokenString @ tokens[[pos]],
					"none",  value = {"PrintRegistrationMarks" -> False},
					"crop",  
						value = {"PrintRegistrationMarks" -> True};
						(* check for 'cross' value*)
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						If[pos <= l && TokenTypeIs["ident", pos, tokens],
							If[TokenStringIsNot["cross", pos, tokens], value = unrecognizedKeyWordFailure @ prop]
							,
							value = unrecognizedValueFailure @ prop
						]
						,
					"cross", 
						value = Missing["Not supported."];
						(* check for 'crop' value*)
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						If[pos <= l && TokenTypeIs["ident", pos, tokens],
							If[TokenStringIsNot["crop", pos, tokens], value = {"PrintRegistrationMarks" -> True}]
							,
							value = unrecognizedValueFailure @ prop
						],
					_, value = unrecognizedKeyWordFailure @ prop
				],
			_, value = unrecognizedValueFailure @ prop
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l, Return @ tooManyTokensFailure @ tokens];
		If[FailureQ[value], value, PrintingOptions -> value]
	]


(* ::Subsection:: *)
(*Bleed*)


consumeProperty[prop:"bleed", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
						"auto", Missing["Not supported."], (* Computes to 6pt if marks has crop and to zero otherwise. *)
						_,      unrecognizedKeyWordFailure @ prop
					],
				"dimension", parseLength @ tokens[[pos]]
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection:: *)
(*Page*)


(* 
	The named page is case-sensitive, but the FE does not support named pages. 
	If this did work, then a named @page rule's declarations would apply, possibly introducing a page break.
	The only way to do this in WL (if at all) is via post-processing. *)
consumeProperty[prop:"page", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[CSSTokenType @ tokens[[pos]],
				"ident", 
					(* 
						If 'auto', the used value is the value specified on its nearest ancestor with a non-auto value. 
						When specified on the root element, the used value for auto is the empty string. *)
					If[StringMatchQ[CSSTokenString @ tokens[[pos]], "auto", IgnoreCase -> True], 
						Missing["Not supported."]
						,
						Missing["Not supported."]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Section:: *)
(*@page token sequence consumer*)


(* ::Subsection:: *)
(*Main @page rule*)


SetAttributes[{consumeAtPageRule}, HoldFirst];

consumeAtPageRule[pos_, l_, tokens_] := 
	Module[{selectorsStart, selectorsEnd, pageSelectors},
		(* check for valid start of @page token sequence *)
		If[TokenTypeIsNot["at-keyword", pos, tokens] || TokenStringIsNot["page", pos, tokens],
			Echo[Row[{"Expected @page keyword. Had instead ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
			,
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		
		(* 
			consume optional page selector list 
			Following CSS selectors, if any of the comma-delimited selectors do not match, all are ignored. *)
		If[TokenTypeIsNot["{}", pos, tokens], 
			selectorsStart = pos; AdvancePosToNextBlock[pos, l, tokens]; 
			selectorsEnd = pos; RetreatPosAndSkipWhitespace[selectorsEnd, l, tokens];
			pageSelectors = 
				If[selectorsEnd < selectorsStart,
					consumePageSelectorList[{}]
					,
					consumePageSelectorList[tokens[[selectorsStart ;; selectorsEnd]]]
				];
			,
			pageSelectors = consumePageSelectorList[{}];
		];
		Echo[pageSelectors, "pageSelectors"];
		
		(* consume @page block *)
		consumeAtPageBlock[Echo[tokens[[pos, 2 ;; ]], "block tokens"]]	
	]


(* The @page {...} block contains either additional page-margin at-rules or generic rules *)
consumeAtPageBlock[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], atMarginStart, atMarginEnd, dec, decStart, decEnd, declarations = {}},
		Echo["In page block"];
		(* skip any initial whitespace *)
		If[TokenTypeIs[" ", pos, tokens], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		
		While[pos <= l,
			Echo[tokens[[pos]], "token"];
			Which[
				(* page-margin at-rule *)
				TokenTypeIs["at-keyword", pos, tokens] && TokenStringIs[Alternatives @@ $ValidPageMargins, pos, tokens], 
					atMarginStart = atMarginEnd = pos; AdvancePosToNextBlock[atMarginEnd, l, tokens];
					dec = consumeAtPageMarginRule[tokens[[atMarginStart ;; atMarginEnd]]];
					Echo[dec];					
					If[!FailureQ[dec], AppendTo[declarations, dec]];
					pos = atMarginEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* CSS 2.1 property or new @Page properties size, marks, bleed, and page*)
				TokenTypeIs["ident", pos, tokens] && TokenStringIs[Alternatives @@ Join[$ApplicableCSSPageProperties, {"size", "marks", "bleed", "page"}], pos, tokens],
					decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
					dec = consumeDeclaration[tokens[[decStart ;; decEnd]]];
					If[!FailureQ[dec], AppendTo[declarations, dec]];
					pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* unrecognized rules are skipped *)
				True, AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		declarations
	]	
	

(* ::Subsection:: *)
(*Page-margin rules*)


(*
	FE page margins are not as flexible as in CSS, but we still get to decorate 
	the top-left/center/right and bottom-left/center/right areas. However, the FE
	implements these "margin areas" by overlaying three separate cells: 
		Cell 1 [LEFT]:   has left margin 0 and FE forces TextAlignment->Left
		Cell 2 [CENTER]: FE forces TextAlignment->Center
		Cell 3 [RIGHT]:  has right margin 0 and FE forces TextAlignment->Right
	Any specified TextAlignment option is ignored.
	Since each cell spans the entire page width, they appear to be left/center/right.
	But if you add CellFrame->True to any then you'll see that they indeed span the page width.
	This behavior is unavoidable and breaks any attempt at using the CSS border property within page margins.
	
	The cells are vertically positioned at the baseline of the cell content
	according to the value of PageHeaderMargins->{<<left page>>,<<right page>>} and
	similarly for PageFooterMargins. If the option value is Automatic, the vertical position is
	the cell content baseline anchored to the center of the margin area.	
	
	Per CSS, a page-margin box is generated if and only if the computed value of its content property is not 'none'.
	The FE leaves the cell content blank so overlaying an empty cell has no visible effect.
*)
consumeAtPageMarginRule[tokens:{___?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], horizontal, vertical, declarations},
		(* check for valid start of margin at-rule token sequence *)
		If[TokenTypeIsNot["at-keyword", pos, tokens] || TokenStringIsNot[Alternatives @@ $ValidPageMargins, pos tokens],
			Echo[Row[{"Expected a page-margin at-rule. Had instead ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		{horizontal, vertical} =
			Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
				"top-left",      {Left,   PageHeaders},
				"top-center",    {Center, PageHeaders},
				"top-right",     {Right,  PageHeaders},
				"bottom-left",   {Left,   PageFooters},
				"bottom-center", {Center, PageFooters},
				"bottom-right",  {Right,  PageFooters},
				_,               {None,   None}
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		(* check for valid block *)
		If[TokenTypeIsNot["{}", pos, tokens], 
			Echo[Row[{"Expected a page-margin block. Had instead ", tokens[[pos]]}], "@page-margin error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		declarations = consumeAtPageMarginBlock[tokens[[pos, 2 ;;]]];
		vertical -> horizontal[declarations]
	]


(* 
	An @page-margin {...} block contains only generic rules.
	However, these rules must be converted into Cell[...] expressions, 
	then passed on to the declaration format e.g.
		<|..., Interpretation->PageHeaders->Left[Cell[...]]|> *)
consumeAtPageMarginBlock[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], dec, decStart, decEnd, declarations = {}},
		Echo["In page margin block"];
		(* skip any initial whitespace *)
		If[TokenTypeIs[" ", pos, tokens], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		
		While[pos <= l,
			(* only a subset of CSS 2.1 properties are valid *)
			If[TokenTypeIs["ident", pos, tokens] && TokenStringIs[Alternatives @@ $ApplicableCSSPageMarginProperties, pos, tokens],
				decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
				dec = consumeDeclaration[tokens[[decStart ;; decEnd]]];
				If[!FailureQ[dec], AppendTo[declarations, dec]];
				pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		Echo[declarations, "Bfore resolve"];
		ResolveCSSInterpretations[Cell, Flatten[Flatten[declarations][[All, "Interpretation"]]]]
	]	


(* ::Subsection:: *)
(*Page selectors*)


consumePageSelectorList[tokens:{__?CSSTokenQ}] :=
	Module[{selectors},
		selectors = DeleteCases[SplitBy[tokens, ","], {","}];
		selectors = consumePageSelector /@ selectors;
		If[AnyTrue[selectors, MissingQ], Return @ Missing["Not supported."]];
		If[AnyTrue[selectors, FailureQ], Return @ FirstCase[selectors, _?FailureQ]];
		If[Length[selectors] > 1, Missing["Not supported."], First @ selectors]
	]
consumePageSelectorList[{}] := All


(* 
	The FE does not allow for page selection beyond the first printed page. 
	Even in the case of selecting for the first page, the FE only modifies the displayed headers and footers.
	The FE does not distinguish between Left/Right pages except for displayed headers and footers and left/right margins.
	The FE does not treat blank pages any differently than pages with content. *)
consumePageSelector[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], scope = All, page},
		(* skip potential leading whitespace *)
		If[TokenTypeIs[" ", pos, tokens], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* check for page identifier, which is not supported by FE *)
		If[pos <= l && TokenTypeIs["ident", pos, tokens], 
			page = CSSTokenString @ tokens[[pos]];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* until the FE supports this feature, bail out now as not supported *)
			Return @ Missing["Not supported."]
		];
		
		(* check for 0 or more pseudo-pages *)
		While[pos <= l,
			Switch[
				CSSTokenType @ tokens[[pos]],
				" ", Return @ Failure["UnexpectedParse", <|"Message" -> "@page pseudo-pages cannot contain whitespace."|>],
				":",
					pos++;
					If[pos > l, Return @ Failure["UnexpectedParse", <|"Message" -> "Incomplete @page pseudo-page selector."|>]];
					If[TokenTypeIs["ident", pos, tokens],
						Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]],
							"left",  scope = Left,
							"right", scope = Right,
							"first", scope = First,
							"blank", Missing["Not supported."],
							_,       Return @ Failure["UnexpectedParse", <|"Message" -> "Unrecognized @page pseudo-page " <> CSSTokenString @ tokens[[pos]] <> "."|>]
						]
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Invalid @page pseudo-page " <> CSSTokenString @ tokens[[pos]] <> "."|>]
					],
				_, Return @ Failure["UnexpectedParse", <|"Message" -> "Expected @page pseudo-page selector."|>]
			];
			pos++
		];
		scope
	]


(* ::Section:: *)
(*Package Footer*)


End[]; (* End Private Context *)
EndPackage[];
