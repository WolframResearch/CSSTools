(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSMediaQueries4`", { "CSSTools`"}]

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSStyleSheetInterpreter`
	---> defines consumeMediaQuery *)

Needs["CSSTools`CSSTokenizer`"];   
Needs["CSSTools`CSSStyleSheetInterpreter`"]; 

Begin["`Private`"] (* Begin Private Context *) 

consumeMediaQuery[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], negate = False, env, conditions = {}},
		(* trim whitespace tokens from ends *)
		pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
		pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		If[TokenTypeIsNot["()", tokens[[pos]]], 
			(* if not a media condition, then check for the optional media type and 'not' keyword *)
			Switch[tokens[[pos]]["Type"], 
				"ident", 
					Switch[ToLowerCase @ tokens[[pos]]["String"],
						"only", (* must be followed by a media type *)
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
							env = consumeMediaType[pos, l, tokens],
						"not",  (* does not need a media type to follow as it could be <<media-not>> form of <<media-condition>> syntax *)
							negate = True; 
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
							If[isMediaType[tokens[[pos]]], env = consumeMediaType[pos, l, tokens], env = None],
						_, 
							If[isMediaType[tokens[[pos]]], 
								env = consumeMediaType[pos, l, tokens]
								, 
								Return @ Failure["BadMedia", <|"Message" -> "Unrecognized media type."|>]
							]
					],
				_, Return @ Failure["BadMedia", <|"Message" -> "Expected ident token in media query."|>]
			];
			If[FailureQ[env], Return @ env];
			(* it's possible, though unlikely, that we've reached the end of the media query *)
			If[pos > l, 
				Return @ env
				,
				(* else attempt to consume 'and (media condition)'; 'or' is not allowed *)
				If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and", tokens[[pos]]],
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
					AppendTo[conditions, consumeMediaCondition[tokens[[pos]]["Children"]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					,
					Return @ Failure["BadMedia", <|"Message" -> "Unexpected condition in media query."|>]
				]
			];
			, (* ELSE we just have the first media condition *)
			AppendTo[conditions, consumeMediaCondition[tokens[[pos]]["Children"]]]
		];
		While[pos <= l,
			Which[
				TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and", tokens[[pos]]],
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
					AppendTo[conditions, consumeMediaCondition[tokens[[pos]]["Children"]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* 'or' is not allowed unless there is a deeper nesting of conditions *)
				TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["or", tokens[[pos]]],
					Return @ Failure["BadMedia", <|"Message" -> "Cannot combine 'and' and 'or' at same level in a media query."|>]
				,
				True,
					Return @ Failure["BadMedia", <|"Message" -> "Unexpected condition in media query."|>]
			]
		];
		conditions
	]
	

isMediaType[token_?CSSTokenQ] := 
	And[
		TokenTypeIs["ident", token],
		TokenStringIs[
			Alternatives[
				"all", "print", "screen", "speech",
				"tty", "tv", "projection", "handheld", 
				"braille", "embossed", "aural"],
			token]]

SetAttributes[consumeMediaType, HoldFirst];
consumeMediaType[pos_, l_, tokens_] :=
	Module[{value},
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[ToLowerCase @ tokens[[pos]]["String"],
						"all",    None,
						"print",  ScreenStyleEnvironment -> "Printout",
						"screen", None,
						"speech", Missing["Not supported."],
						Alternatives["tty", "tv", "projection", "handheld", "braille", "embossed", "aural"], Missing["Not supported."],
						"or",     Failure["BadMedia", <|"Message" -> "Reserved keyword 'or' cannot be used."|>],
						"and",    Failure["BadMedia", <|"Message" -> "Reserved keyword 'and' cannot be used."|>],
						"not",    Failure["BadMedia", <|"Message" -> "Reserved keyword 'not' cannot be used."|>],
						"only",   Failure["BadMedia", <|"Message" -> "Reserved keyword 'only' cannot be used."|>],
						_,        Missing["Not supported."] (* unknown query type *)
					],
				_, Failure["BadMedia", <|"Message" -> "Expected ident token in media query."|>]
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		value
	]


(* 
	The media condition can be deeply recursive since parentheses are allowed to nest conditions.
	At no single level can 'and' and 'or' logical operators be combined; use parenthesis to indicate scope. *)
(*TODO*)
consumeMediaCondition[tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], negate = False},
		(* trim whitespace tokens from ends *)
		pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
		pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		Which[
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["not", tokens[[pos]]],
				negate = True; AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[TokenTypeIs["()", tokens[[pos]]],
					consumeMediaInParens[tokens[[pos]]["Children"]]
					,
					Throw @ Failure[]
				]
			,
			consumeMediaInParens[tokens[[pos]]["Children"]];
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[ToLowerCase @ tokens[[pos]]["String"],
						"and", Null,
						"or",  Null,
						_,     Null
					],
				_, Null
			]
		]
	]

consumeMediaInParens[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], negate = False},
		(* trim whitespace tokens from ends *)
		pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
		pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* consume possible negation which must always be followed by a parenthesized sequence *)
		If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["not", tokens[[pos]]],
			negate = True; AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		
		(* consume <<media-in-parens>> *)
		
		
		Which[
			pos == l, (* if only one token is present... then this must be the last in a chain of media conditions *)
				Which[
					TokenTypeIs["ident", tokens[[pos]]], 
						If[!StringStartsQ[tokens[[pos]]["String"], "min-" | "max-", IgnoreCase -> True],
							Return @ mediaFeatureBoolean[ToLowerCase @ tokens[[pos]]["String"]]
							,
							Return @ Failure["BadMedia", <|"Message" -> "Prefixes are not allowed for media features with no value."|>]
						]
					,
					TokenTypeIs["()", tokens[[pos]]], 
						consumeMediaCondition[tokens[[pos]]["Children"]]
					,
					Return @ Failure["BadMedia", <|"Message" -> "Unrecognized media feature."|>]
				]
			,
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["not", tokens[[pos]]],
				negate = True; AdvancePosAndSkipWhitespace[pos, l, tokens];
			True,
				Null
		];
		(* do media-in-parens check *)
		Which[
			isMediaFeaturePlain[pos, l, tokens], 
				consumeMediaFeaturePlain[pos, l, tokens]
			,
			True, 
				Null
		];
		
	]
	

isMediaFeaturePlain[pos_, l_, tokens_] := 
	Module[{p = pos},
		If[TokenTypeIs["ident", tokens[[p]]] && isMediaFeatureName[tokens[[p]]["String"]],
			AdvancePosAndSkipWhitespace[p, l, tokens]
			,
			Return @ False
		];
		If[p <= l && TokenTypeIs["colon", tokens[[p]]],
			AdvancePosAndSkipWhitespace[p, l, tokens]
			,
			Return @ False
		];
		p <= l && isMediaFeatureValue[p, l, tokens]
	]
	
isRatio[pos_, l_, tokens_] :=
	Module[{p = pos},
		If[TokenTypeIs["number", tokens[[p]]] && tokens[[p]]["ValueType"] === "integer",
			AdvancePosAndSkipWhitespace[p, l, tokens]
			,
			Return @ False
		];
		If[p <= l && TokenTypeIs["delim", tokens[[p]]] && TokenStringIs["/", tokens[[p]]],
			AdvancePosAndSkipWhitespace[p, l, tokens]
			,
			Return @ False
		];
		p <= l && TokenTypeIs["number", tokens[[p]]] && tokens[[p]]["ValueType"] === "integer"
	]
	
isMediaFeatureName[s_?StringQ] := 
	StringMatchQ[
		s, 
		Alternatives["width", "height", "aspect-ratio", "resolution", "color",
			"color-index", "monochrome", "device-width", "device-height", "device-aspect-ratio",
			"orientation", "scan", "grid", "update", "overflow-block", "overflow-inline",
			"color-gamut", "hover", "any-hover", "pointer", "any-pointer"],
		IgnoreCase -> True]
		
isMediaFeatureValue[pos_, l_, tokens_] :=
	Or[
		isRatio[pos, l, tokens],
		TokenTypeIs["dimension" | "ident" | "number", tokens[[pos]]]]
		
(*isMediaRange[pos_, l_, tokens_] :=
	Module[{p = pos},
		If[isMediaFeatureName[tokens[[p]]["String"]],
			AdvancePosAndSkipWhitespace[p, l, tokens];
			
			,
			Null(* mf-value etc. *)
		]
	]*)


(* ranged features *)
mediaFeatureBoolean["width"] := True (* WD has a positive minimum window width so this is always positive *)
mediaFeatureBoolean["height"] := True (* WD has a positive minimum window height so this is always positive *)
mediaFeatureBoolean["aspect-ratio"] := True (* always non-zero *)
mediaFeatureBoolean["resolution"] := True
mediaFeatureBoolean["color"] := True
mediaFeatureBoolean["color-index"] := True
mediaFeatureBoolean["monochrome"] := True
(* deprecated ranged features but must be supported *)
mediaFeatureBoolean["device-width"] := True
mediaFeatureBoolean["device-height"] := True
mediaFeatureBoolean["device-aspect-ratio"] := True

(* discrete features *)
mediaFeatureBoolean["orientation"] := True
mediaFeatureBoolean["scan"] := True
mediaFeatureBoolean["grid"] := True
mediaFeatureBoolean["update"] := True
mediaFeatureBoolean["overflow-block"] := True (* could be 'none'=False *)
mediaFeatureBoolean["overflow-inline"] := True (* could be 'none'=False *)
mediaFeatureBoolean["color-gamut"] := True
mediaFeatureBoolean["hover"] := True (* could be 'none'=False *)
mediaFeatureBoolean["any-hover"] := True (* could be 'none'=False *)
mediaFeatureBoolean["pointer"] := True (* could be 'none'=False *)
mediaFeatureBoolean["any-pointer"] := True (* could be 'none'=False *)

(*TODO: is this list necessary? *)
CSSMediaFeatureProperties = <|
	"width" -> <|
		"Value" -> "length",
		"Type"  -> "range"|>,
	"height" -> <|
		"Value" -> "length",
		"Type"  -> "range"|>,
	"aspect-ratio" -> <|
		"Value" -> "ratio",
		"Type"  -> "range"|>,
	"orientation" -> <|
		"Value" -> {"portrait", "landscape"},
		"Type"  -> "discrete"|>
|>

(*TODO: should media features be consumed similar to CSS properties? *)
consumeMediaFeature["width", _] := Null

End[] (* End Private Context *)

EndPackage[]