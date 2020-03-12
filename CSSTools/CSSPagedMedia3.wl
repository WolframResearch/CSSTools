(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSPagedMedia3`", {"CSSTools`"}];

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSPropertyInterpreter` 
	---> defines consumeProperty and CSSPropertyData *)
(* CSSStyleSheetInterpreter`
	---> defines consumeDeclaration
	---> defines notebookLevelOptions
	---> defines consumeAtPageRule, consumeAtPageBlock, convertMarginsToPrintingOptions *)

Needs["CSSTools`CSSTokenizer`"]; 
Needs["CSSTools`CSSPropertyInterpreter`"];
Needs["CSSTools`CSSStyleSheetInterpreter`"]; 


(* Package Notes: implements https://drafts.csswg.org/css-page-3
	This package extends the CSS Tools to include CSS Paged Media Module Level 3 (currently only a CSS working draft). 
	It adds 4 properties to CSSPropretyInterpreter`:
		page:  references named page properties [FE not supported]
		size:  set the size of the page box's containing block i.e. the page sheet
		bleed: set whether page box content extends outside of the page box margins and into the page sheet [FE not supported]
		marks: set whether to show crop marks if the page box size is smaller than the page sheet size [FE partial support]
	TODO: It also modifies how the CSS page properties merge while resolving the CSS cascade. 
	All CSS @page options fall under the FE option PrintingOptions so must be carefully merged.
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
(*@page token sequence consumer*)


(* 
	It is assumed that the string input has already been tokenized into CSS tokens.
	The main token consumers have the HoldFirst attribute. 
	This allows the position variable to be tracked continuously through each token consumer.
	Some token consumers also advance the position.
	
	The tokenizer and token accessor functions are defined in CSSTools`CSSTokenizer.
	<<token>>["Type"]       canonical token type e.g. "ident", "dimension", etc.
	<<token>>["String"]     canonical string i.e. lower case and escape sequences are translated e.g. "\30 Red" --> "0red"
	<<token>>["RawString"]  original unaltered string
	<<token>>["Value"]      dimension value (already an interpreted number)
	<<token>>["Unit"]       canonical dimension unit i.e. lower case and escape sequences are translated e.g. "px"
	
	Function TokenTypeIs does not ignore case as the token types should already be canonicalized.
	Function TokenStringIs uses the canonical string for comparison and ignores case.
*)


(* ::Subsection:: *)
(*Main @page rule*)


SetAttributes[{consumeAtPageRule}, HoldFirst];

consumeAtPageRule[pos_, l_, tokens_, namespaces_] := 
	Module[{selectorsStart, selectorsEnd, pageSelectors, block},
		(* check for valid start of @page token sequence *)
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot["page", tokens[[pos]]],
			Echo[Row[{"Expected @page keyword. Had instead ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
			,
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		
		(* 
			consume optional page selector list 
			Following CSS selectors, if any of the comma-delimited selectors do not match, all are ignored. *)
		pageSelectors = 
			If[TokenTypeIsNot["{}", tokens[[pos]]], 
				selectorsStart = pos; AdvancePosToNextBlock[pos, l, tokens]; (* pos is now at block's position *)
				selectorsEnd = pos; RetreatPosAndSkipWhitespace[selectorsEnd, l, tokens];
				consumePageSelectorList[If[selectorsEnd < selectorsStart, {}, tokens[[selectorsStart ;; selectorsEnd]]]]
				,
				consumePageSelectorList[{}];
			];
		
		(* consume @page block *)
		block = consumeAtPageBlock[tokens[[pos]]["Children"], pageSelectors, namespaces];
		block[[All, "Condition"]] = Hold[CurrentValue[InputNotebook[], ScreenStyleEnvironment] === "Printout"];
		<|
			"Selector" -> "@page" <> If[NumericQ[selectorsStart], " " <> CSSUntokenize @ tokens[[selectorsStart ;; selectorsEnd]], ""],
			"Block"    -> block|>	
	]


(* The @page {...} block contains either additional page-margin at-rules or generic rules *)
consumeAtPageBlock[tokens:{___?CSSTokenQ}, scope_, namespaces_] :=
	Module[{pos = 1, l = Length[tokens], atMarginStart, atMarginEnd, dec, decStart, decEnd, declarations = {}},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		While[pos <= l,
			Which[
				(* page-margin at-rule *)
				TokenTypeIs["at-keyword", tokens[[pos]]] && TokenStringIs[Alternatives @@ $ValidPageMargins, tokens[[pos]]], 
					atMarginStart = atMarginEnd = pos; AdvancePosToNextBlock[atMarginEnd, l, tokens];
					dec = consumeAtPageMarginRule[tokens[[atMarginStart ;; atMarginEnd]], scope, namespaces];
					If[!FailureQ[dec], AppendTo[declarations, dec]];
					pos = atMarginEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* CSS 2.1 property or new @Page properties size, marks, bleed, and page*)
				TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs[Alternatives @@ Join[$ApplicableCSSPageProperties, {"size", "marks", "bleed", "page"}], tokens[[pos]]],
					decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
					dec = consumeDeclaration[tokens[[decStart ;; decEnd]], namespaces];
					If[TrueQ @ $Debug, Echo[dec, "@page declaration"]];
					If[!FailureQ[dec], 
						Which[
							TokenStringIs["margin" | "margin-top" | "margin-bottom" | "margin-left" | "margin-right", tokens[[pos]]],
								dec = convertMarginsToPrintingOptions[dec, scope]
							,
							True, Null
						];
						AppendTo[declarations, dec]
					];
					pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* unrecognized rules are skipped *)
				True, AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		If[TrueQ @ $Debug, Echo[declarations, "@page declarations"]];
		declarations
	]	
	
convertMarginsToPrintingOptions[declaration_?AssociationQ, scope_] :=
	Module[{value},
		If[
			Or[
				!KeyExistsQ[declaration, "Interpretation"],
				!KeyExistsQ[declaration["Interpretation"], PrintingOptions], 
				!KeyExistsQ[declaration["Interpretation", PrintingOptions, "PrintingMargins"]]],
			Return @ declaration
		];
		value = declaration["Interpretation", PrintingOptions, "PrintingMargins"];
		value = 
			KeyValueMap[
				Rule[
					#1,
					Which[
						MatchQ[#1, "Left" | "Right"], 
							Replace[#2, Scaled[x_] :> Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 1}]]],
						MatchQ[#1, "Top" | "Bottom"],
							Replace[#2, Scaled[x_] :> Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 2}]]],
						True,
							#2
					]
				]&,
				value
			];
		value = <|value|>;
		<|
			declaration, 
			"Interpretation" -> <|
				PrintingOptions -> <|
					Which[
						MatchQ[scope, Left | Right], <|"InnerOuterMargins" -> DeleteMissing @ {value["Left"], value["Right"]}|>,
						MatchQ[scope, First],        Missing["Not supported."],
						True,                        "PrintingMargins" -> <|value|>
					]|>|>|>
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
consumeAtPageMarginRule[tokens:{___?CSSTokenQ}, scope_, namespaces_] := 
	Module[{pos = 1, l = Length[tokens], horizontal, vertical, location},
		(* check for valid start of margin at-rule token sequence *)
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot[Alternatives @@ $ValidPageMargins, tokens[[pos]]],
			Echo[Row[{"Expected a page-margin at-rule. Had instead ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		{horizontal, vertical} =
			Switch[location = tokens[[pos]]["String"],
				"top-left",      {"Left",   PageHeaders},
				"top-center",    {"Center", PageHeaders},
				"top-right",     {"Right",  PageHeaders},
				"bottom-left",   {"Left",   PageFooters},
				"bottom-center", {"Center", PageFooters},
				"bottom-right",  {"Right",  PageFooters},
				_,               {None,   None}
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		(* check for valid block *)
		If[TokenTypeIsNot["{}", tokens[[pos]]], 
			Echo[Row[{"Expected a page-margin block. Had instead ", tokens[[pos]]}], "@page-margin error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		consumeAtPageMarginBlock[tokens[[pos]]["Children"], scope, "@" <> location, horizontal, vertical, namespaces]
	]


(* 
	An @page-margin {...} block contains only generic rules.
	However, these rules must be converted into Cell[...] expressions, 
	then passed on to the declaration format e.g.
		<|..., Interpretation->PageHeaders->Left[Cell[...]]|> *)
consumeAtPageMarginBlock[tokens:{___?CSSTokenQ}, scope_, location_String, horizontal_, vertical_, namespaces_] :=
	Module[{pos = 1, l = Length[tokens], dec, decStart, decEnd, declarations = {}, interpretation},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		
		While[pos <= l,
			(* only a subset of CSS 2.1 properties are valid *)
			If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs[Alternatives @@ $ApplicableCSSPageMarginProperties, tokens[[pos]]],
				decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
				dec = consumeDeclaration[tokens[[decStart ;; decEnd]], namespaces];
				If[!FailureQ[dec], AppendTo[declarations, dec]];
				pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		interpretation =
			With[{res = CSSCascade[All, Cell, {<|"Selector" -> CSSSelector["dummy"], "Block" -> declarations|>}, {"dummy"}]},
				With[{v = DisplayFunction /. res},
					If[MatchQ[v, Normal | None],
						None
						,
						Cell[
							TextData[Apply[Identity, v]], 
							If[StringContainsQ[location, "top"], "Header", "Footer"],
							Sequence @@ DeleteCases[res, _[DisplayFunction, _]]]
					]
				]
			];
		<|
			"Property"       -> location,
			"Value"          -> Dataset @ declarations,
			"Important"      -> False,
			"Interpretation" -> 
				Which[
					MatchQ[scope, Left | Right], <|vertical -> {<|"Page" -> ToString @ scope, "Position" -> horizontal, "Content" -> interpretation|>}|>,
					MatchQ[scope, First],        Missing["Not supported."],
					True,                        <|vertical -> {
						                            <|"Page" -> "Left",  "Position" -> horizontal, "Content" -> interpretation|>, 
						                            <|"Page" -> "Right", "Position" -> horizontal, "Content" -> interpretation|>}|>
				]|>
	]	


(* ::Subsection:: *)
(*Page selectors*)


consumePageSelectorList[tokens:{__?CSSTokenQ}] :=
	Module[{selectors},
		selectors = DeleteCases[SplitBy[tokens, MatchQ[CSSToken[KeyValuePattern["Type" -> "comma"]]]], {CSSToken[KeyValuePattern["Type" -> "comma"]]}];
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
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* check for page identifier, which is not supported by FE *)
		If[pos <= l && TokenTypeIs["ident", tokens[[pos]]], 
			page = tokens[[pos]]["String"];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* until the FE supports this feature, bail out now as not supported *)
			Return @ Missing["Not supported."]
		];
		
		(* check for 0 or more pseudo-pages *)
		While[pos <= l,
			Switch[
				tokens[[pos]]["Type"],
				"whitespace", Return @ Failure["UnexpectedParse", <|"Message" -> "@page pseudo-pages cannot contain whitespace."|>],
				"colon",
					pos++;
					If[pos > l, Return @ Failure["UnexpectedParse", <|"Message" -> "Incomplete @page pseudo-page selector."|>]];
					If[TokenTypeIs["ident", tokens[[pos]]],
						Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
							"left",  scope = Left,
							"right", scope = Right,
							"first", scope = First,
							"blank", Missing["Not supported."],
							_,       Return @ Failure["UnexpectedParse", <|"Message" -> "Unrecognized @page pseudo-page " <> tokens[[pos]]["String"] <> "."|>]
						]
						,
						Return @ Failure["UnexpectedParse", <|"Message" -> "Invalid @page pseudo-page " <> tokens[[pos]]["String"] <> "."|>]
					],
				_, Return @ Failure["UnexpectedParse", <|"Message" -> "Expected @page pseudo-page selector."|>]
			];
			pos++
		];
		scope
	]


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
				"inherit" -> <||>, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>,
			"Animatable" -> True,
			"Values" -> {"auto", "<identifier>"},
			"AppliesTo" -> "boxes that create class A break points"|>,
		"bleed" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> <||>, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>,
		"marks" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "none",
			"InterpretedGlobalValues" -> <|
				"inherit" -> <|PrintingOptions -> <|"PrintRegistrationMarks" -> Inherited|>|>, 
				"initial" -> <|PrintingOptions -> <|"PrintRegistrationMarks" -> False|>|>|>|>,
		"size" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> <|
					PrintingOptions -> <|
						"PageSize"  -> Inherited,
						"PaperSize" -> Inherited|>|>,
				"initial" -> <|
					PrintingOptions -> <|
						"PageSize"  -> {Automatic, Automatic},
						"PaperSize" -> {Automatic, Automatic}|>|>|>|>}];
						
Map[
	If[FreeQ[notebookLevelOptions, #], AppendTo[notebookLevelOptions, #]]&,
	{PrintingOptions, PageHeaders, PageFooters}];
	
assembleByFEOption[opt:(PageHeaders | PageFooters), CSSBlockData_?validCSSBlockFullQ] := 
	Module[{orderedRules = Flatten @ DeleteMissing[#[opt] & /@ CSSBlockData[[All, "Interpretation"]]]},
		opt -> {
			(* left page *)
			{
				First[Select[orderedRules, #Page === "Left" && #Position === "Left" &],   <|"Content" -> None|>]["Content"],
				First[Select[orderedRules, #Page === "Left" && #Position === "Center" &], <|"Content" -> None|>]["Content"],
				First[Select[orderedRules, #Page === "Left" && #Position === "Right" &],  <|"Content" -> None|>]["Content"]},
			(* right page *)
			{
				First[Select[orderedRules, #Page === "Right" && #Position === "Left" &],   <|"Content" -> None|>]["Content"],
				First[Select[orderedRules, #Page === "Right" && #Position === "Center" &], <|"Content" -> None|>]["Content"],
				First[Select[orderedRules, #Page === "Right" && #Position === "Right" &],  <|"Content" -> None|>]["Content"]}}
	]
			


(* ::Subsection:: *)
(*Size*)


(* new interpreters *)
(* Notes on page box size vs paper box
	The page consists of the paper box (medium on to which the content is printed) which has its own size. 
		WL: PrintingOptions -> {"PaperSize" -> {<<width>>, <<height>>}} where size is in points (72 points per inch)		
	The page box sits within the paper box and contains the page content and margins; its size can be separately specified. 
		WL: PrintingOptions -> {"PageSize" -> {<<width>>, <<height>>}} where size is in points (72 points per inch)		
	Normally these two are the same size, but they don't have to be. 
	AFAICT the CSS does not control the size of the page box, only the paper box. *)
consumeProperty[prop:"size", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], paperSizes, paperOrientations, value1 = {}, value2 = {}},
		paperSizes = {"a5", "a4", "a3", "b5", "b4", "jis-b5", "jis-b4", "letter", "legal", "ledger"};
		paperOrientations = {"landscape", "portrait"};
		Which[
			(* case of page size possibly followed by orientation *)
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs[Alternatives @@ paperSizes, tokens[[pos]]],
				value1 = parseSinglePaperSize[prop, tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* check for orientation *)
				If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
					value2 = parseSinglePaperOrientation[prop, tokens[[pos]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				];
			,
			(* case of orientation possibly followed by page size *)
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs[Alternatives @@ paperOrientations, tokens[[pos]]],
				value1 = parseSinglePaperOrientation[prop, tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* check for paper size *)
				If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
					value2 = parseSinglePaperSize[prop, tokens[[pos]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				];
			,
			(* case of single keyword *)
			TokenTypeIs["ident", tokens[[pos]]],
				value1 = 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto", {"PageSize" -> {Automatic, Automatic}, "PaperSize" -> {Automatic, Automatic}},
						_,      unrecognizedKeyWordFailure @ prop
					];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
			,
			(* case of lengths *)
			TokenTypeIs["dimension", tokens[[pos]]],
				value1 = If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, convertDPItoPrintersPoints @ parseLength @ tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[FailureQ[value1], Return @ value1, value1 = {value1}];
				(* check for separate height value *)
				If[pos <= l && TokenTypeIs["dimension", tokens[[pos]]],
					value2 = If[tokens[[pos]]["Value"] < 0, negativeLengthFailure @ prop, convertDPItoPrintersPoints @ parseLength @ tokens[[pos]]];
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
		If[pos <= l, Return @ tooManyTokensFailure @ tokens];
		If[FailureQ[value1], Return @ value1];
		If[FailureQ[value2], Return @ value2];
		<|PrintingOptions -> <|Join[value1, value2]|>|>
	]
	
parseSinglePaperSize[prop_String, token_?CSSTokenQ] :=
	Switch[token["String"],
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
	Switch[token["String"],
		"portrait",  {"PaperOrientation" -> "Portrait"},
		"landscape", {"PaperOrientation" -> "Landscape"},
		_,           unrecognizedKeyWordFailure @ prop
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
consumeProperty[prop:"marks", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"none",  value = {"PrintRegistrationMarks" -> False},
					"crop",  
						value = {"PrintRegistrationMarks" -> True};
						(* check for 'cross' value*)
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
							If[TokenStringIsNot["cross", tokens[[pos]]], value = unrecognizedKeyWordFailure @ prop]
							,
							value = unrecognizedValueFailure @ prop
						]
						,
					"cross", 
						value = Missing["Not supported."];
						(* check for 'crop' value*)
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
							If[TokenStringIsNot["crop", tokens[[pos]]], value = {"PrintRegistrationMarks" -> True}]
							,
							value = unrecognizedValueFailure @ prop
						],
					_, value = unrecognizedKeyWordFailure @ prop
				],
			_, value = unrecognizedValueFailure @ prop
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l, Return @ tooManyTokensFailure @ tokens];
		If[FailureQ[value], value, <|PrintingOptions -> <|value|>|>]
	]


(* ::Subsection:: *)
(*Bleed*)


consumeProperty[prop:"bleed", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
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
consumeProperty[prop:"page", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ tokens];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					(* 
						If 'auto', the used value is the value specified on its nearest ancestor with a non-auto value. 
						When specified on the root element, the used value for auto is the empty string. *)
					If[TokenStringIs["auto", tokens[[pos]]], 
						Missing["Not supported."]
						,
						Missing["Not supported."]
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Section:: *)
(*Package Footer*)


End[]; (* End Private Context *)
EndPackage[];
