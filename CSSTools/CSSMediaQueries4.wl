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
	Module[{pos = 1, l = Length[tokens], negate = False, only = False, env, conditions = {}},
		(* trim whitespace tokens from ends *)
		pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
		pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* media type is optional; if missing it implies all media *)
		If[TokenTypeIsNot["()", tokens[[pos]]], 
			Switch[tokens[[pos]]["Type"], 
				"ident", 
					Switch[tokens[[pos]]["String"],
						"only", 
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
							env = consumeMediaType[pos, l, tokens],
						"not",  
							negate = True; 
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]];
							env = consumeMediaType[pos, l, tokens],
						Alternatives[
							"all", "print", "screen", "speech", 
							"tty", "tv", "projection", "handheld", 
							"braille", "embossed", "aural"
						],  
							env = consumeMediaType[pos, l, tokens],
						_, 
							Return @ Failure["BadMedia", <|"Message" -> "Expected a media type in the media query."|>]
					],
				_, Return @ Failure["BadMedia", <|"Message" -> "Expected ident token in media query."|>]
			];
			If[FailureQ[env], Return @ env];
			(* it's possible, though unlikely, that we've reached the end of the media query *)
			If[pos > l, 
				Return @ env
				,
				(* else attempt to consume 'and condition' *)
				If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and", tokens[[pos]]],
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]]
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
					If[pos > l, Return @ Failure["BadMedia", <|"Message" -> "Unexpected end of media query."|>]]
					AppendTo[conditions, consumeMediaCondition[tokens[[pos]]["Children"]]];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["or", tokens[[pos]]],
					Return @ Failure["BadMedia", <|"Message" -> "Cannot combine 'and' and 'or' at same level in a media query."|>]
				,
				True,
					Return @ Failure["BadMedia", <|"Message" -> "Unexpected condition in media query."|>]
			]
		];
		conditions
	]
	

SetAttributes[consumeMediaType, HoldFirst];
consumeMediaType[pos_, l_, tokens_] :=
	Module[{value},
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident",
					Switch[tokens[[pos]]["String"],
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
	Module[{},
		tokens
	]

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