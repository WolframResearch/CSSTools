(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSPagedMedia3`"];
Needs["CSSTools`CSSTokenizer`"];           (* keep tokenizer utilities hidden from $ContextPath *)
(*Needs["CSSTools`CSSPropertyInterpreter`"]; (* keep property interpreters hidden from $ContextPath *)*)

Begin["`Private`"]; (* Begin Private Context *) 

(* we assume that the @page rule has already been tokenized *)

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


(* new CSS properties *)
AssociateTo[
	CSSTools`CSSPropertyInterpreter`Private`CSSPropertyData, 
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
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> {}, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>,
		"size" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> {}, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>}];
		

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
		
		Echo["Here"];
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
		consumeAtPageBlock[tokens[[pos, 2 ;; ]]]	
	]

(* The @page {...} block contains either additional page-margin at-rules or generic rules *)
consumeAtPageBlock[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], atMarginStart, atMarginEnd, dec, decStart, decEnd, declarations = {}},
		While[pos <= l,
			Which[
				(* page-margin at-rule *)
				TokenTypeIs["at-keyword", pos, tokens] && TokenStringIs[Alternatives @@ $ValidPageMargins, pos, tokens], 
					atMarginStart = atMarginEnd = pos; AdvancePosToNextBlock[atMarginEnd, l, tokens];
					consumeAtPageMarginRule[tokens[[atMarginStart ;; atMarginEnd]]];
					pos = atMarginEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* CSS 2.1 property*)
				TokenTypeIs["ident", pos, tokens] && TokenStringIs[Alternatives @@ $ApplicableCSSPageProperties, pos, tokens],
					decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
					dec = CSSImport`Private`consumeDeclaration[tokens[[decStart ;; decEnd]]];
					If[TrueQ @ CSSImport`Private`$RawImport, 
						KeyDropFrom[dec, "Interpretation"]
						,
						AssociateTo[dec, "Interpretation" -> CSSTools`CSSPropertyInterpreter`Private`consumeProperty[dec["Property"], dec["Interpretation"]]]
					];
					AppendTo[declarations, dec];
					pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* Paged Media property: size, marks, bleed, and page *)
				TokenTypeIs["ident", pos, tokens],
					decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
					dec = CSSImport`Private`consumeDeclaration[tokens[[decStart ;; decEnd]]];
					If[TrueQ @ CSSImport`Private`$RawImport, 
						KeyDropFrom[dec, "Interpretation"]
						,
						AssociateTo[dec, "Interpretation" -> consumeProperty[dec["Property"], dec["Interpretation"]]]
					];
					AppendTo[declarations, dec];
					pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* unrecognized rules are skipped *)
				True, AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		declarations
	]
	
consumeProprety[prop:"size", ]
	
	
consumeAtPageMarginRule[tokens:{___?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens]},
		(* check for valid start of margin at-rule token sequence *)
		If[TokenTypeIsNot["at-keyword", pos, tokens],
			Echo[Row[{"Expected a margin at-rule. Had instead ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		Switch[ToLowerCase @ CSSTokenString @ tokens[[pos]], 
			"top-left",      Null,
			"top-center",    Null,
			"top-right",     Null,
			"bottom-left",   Null,
			"bottom-center", Null,
			"bottom-right",  Null,
			Alternatives[
				"top-left-corner", "top-right-corner", "bottom-left-corner", "bottom-right-corner", 
				"left-top", "left-middle", "left-bottom", "right-top", "right-middle", "right-bottom"
			],               Missing["Not supported."]
		];
	]
	
consumeAtPageMarginRuleTopLeft[] := {}

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
							"left",  Missing["Not supported."],
							"right", Missing["Not supported."],
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


End[]; (* End Private Context *)
EndPackage[];
