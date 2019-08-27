(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSMediaQueries4`", { "CSSTools`"}]

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSStyleSheetInterpreter`
	---> defines consumeMediaQuery *)
(* CSSPropertyInterpreter`
	---> defines parseLength *)

Needs["CSSTools`CSSTokenizer`"];   
Needs["CSSTools`CSSStyleSheetInterpreter`"]; 
Needs["CSSTools`CSSPropertyInterpreter`"];

Begin["`Private`"] (* Begin Private Context *) 

consumeMediaQuery[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], hasMediaType = False, mediaType = All, mcStart = False, negate = False, value1, value2},
		TrimWhitespaceTokens[pos, l, tokens];
		(* 
			The start of a <media-query> is a little tricky. It can start with 'not', but we have to
			determine whether the 'not' is for a <media-not> or a <media-query>.
			Consume tokens as if it were a standard media query, 
			but exit early and try again as a <media-condition> if anything fails to parse. *)
		Which[
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["not", tokens[[pos]]],
				negate = True; 
				AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			,
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["only", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens];
			,
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and" | "or", tokens[[pos]]],
				Throw @ 
					Failure["BadMedia", <|
						"Message"  -> "Reserved keyword '" <> tokens[[pos]]["String"] <> "' cannot be used.",
						"Position" -> showError[pos, pos, tokens]|>]
			,
			isMediaType[tokens[[pos]]],
				hasMediaType = True;
				mediaType = consumeMediaType[pos, l, tokens];
			,
			True,
				mcStart = True
		];
		If[!mcStart,
			(* The next token must be an ident if one was not already consumed. *)
			If[!hasMediaType && pos < l, mediaType = consumeMediaType[pos, l, tokens]; hasMediaType = True];
			(* The next tokens, if any, are 'and' with <media-condition-without-or> *)
			While[pos < l,
				Which[
					TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and", tokens[[pos]]],
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						If[pos > l, 
							Throw @ 
								Failure["BadMedia", <|
									"Message"  -> "Unexpected end of media query.", 
									"Position" -> showError[pos, -1, tokens]|>]];
						If[TokenTypeIs["()" | "function", tokens[[pos]]],
							value2 = consumeMediaInParens[tokens[[pos]]];
							Which[
								!ValueQ[value1], 
									value1 = value2;
								,
								MatchQ[value1, Hold[And[___]]], 
									value2 = Replace[value2, Hold[x___] :> Hold[And[x]]];
									value1 = Join[value1, value2, 2];
								,
								True,
									value1 = Thread[And[value1, value2], Hold]
							];
							,
							Throw @ 
								Failure["BadMedia", <|
									"Message" -> "Expected <media-in-parens> after 'not'.",
									"Position" -> showError[1, -1, tokens]|>]
						];
						AdvancePosAndSkipWhitespace[pos, l, tokens];
					,
					TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["or", tokens[[pos]]],
						Throw @ 
							Failure["BadMedia", <|
								"Message"  -> "Logic level contains both 'and' and 'or'.",
								"Position" -> showError[1, -1, tokens]|>];
					,
					True, 
						Throw @ 
							Failure["BadMedia", <|
								"Message" -> "Expected 'and' token."|>]
				]
			]
			,
			(* attempt to consume entire token sequence as a <media-condition> *)
			value1 = Catch @ consumeMediaCondition[tokens];
			If[FailureQ[value1], Throw @ Failure[value1[[1]], If[KeyExistsQ[value1[[2]], "Parsed"], KeyDrop[value1[[2]], "Parsed"], value1[[2]]]]]
			
		];
		{mediaType, value1}
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
consumeMediaCondition[tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], negate = False, value1, value2, andSequence = False, orSequence = False},
		TrimWhitespaceTokens[pos, l, tokens];
		Which[
			(* <media-not> *)
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["not", tokens[[pos]]],
				negate = True; 
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[pos > l, 
					Throw @ 
						Failure["BadMedia", <|
							"Message"  -> "Unexpected end of media query.",
							"Position" -> showError[pos, -1, tokens],
							"Parsed"   -> 1.0|>]];
				If[TokenTypeIs["()" | "function", tokens[[pos]]],
					value1 = consumeMediaInParens[tokens[[pos]]];
					value1 = Replace[value1, Hold[x___] :> Hold[Not[x]]]
					,
					Throw @ 
						Failure["BadMedia", <|
							"Message" -> "Expected <media-in-parens> after 'not'.",
							"Parsed"  -> 1.0|>]
				];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[pos <= l, 
					Throw @ 
						Failure["BadMedia", <|
							"Message"  -> "Too many tokens in the media condition.",
							"Position" -> showError[pos, -1, tokens],
							"Parsed"   -> 1.0|>]]
			,
			(* <media-in-parens> *)
			TokenTypeIs["()" | "function", tokens[[pos]]], 
				value1 = consumeMediaInParens[tokens[[pos]]];
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* check for optionally repeating <media-and> or <media-or> *)
				While[pos < l,
					Which[
						TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["and", tokens[[pos]]],
							If[orSequence, 
								Throw @ 
									Failure["BadMedia", <|
										"Message"  -> "Logic level contains both 'and' and 'or'.",
										"Position" -> showError[1, -1, tokens],
										"Parsed"   -> 1.0|>]];
							andSequence = True;
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, 
								Throw @ 
								Failure["BadMedia", <|
									"Message"  -> "Unexpected end of media query.", 
									"Position" -> showError[pos, -1, tokens],
									"Parsed"   -> 1.0|>]];
							If[TokenTypeIs["()" | "function", tokens[[pos]]],
								value2 = consumeMediaInParens[tokens[[pos]]];
								If[MatchQ[value1, Hold[And[___]]], 
									value2 = Replace[value2, Hold[x___] :> Hold[And[x]]];
									value1 = Join[value1, value2, 2];
									,
									value1 = Thread[And[value1, value2], Hold]
								];
								,
								Throw @ 
									Failure["BadMedia", <|
										"Message" -> "Expected <media-in-parens> after 'not'.",
										"Position" -> showError[1, -1, tokens],
										"Parsed"  -> 1.0|>]
							];
							AdvancePosAndSkipWhitespace[pos, l, tokens];
						,
						TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["or", tokens[[pos]]],
							If[andSequence, 
								Throw @ 
									Failure["BadMedia", <|
										"Message"  -> "Logic level contains both 'and' and 'or'.",
										"Position" -> showError[1, -1, tokens],
										"Parsed"   -> 1.0|>]];
							orSequence = True;
							AdvancePosAndSkipWhitespace[pos, l, tokens];
							If[pos > l, 
								Throw @ 
									Failure["BadMedia", <|
										"Message"  -> "Unexpected end of media query.", 
										"Position" -> showError[pos, -1, tokens],
										"Parsed"   -> 1.0|>]];
							If[TokenTypeIs["()" | "function", tokens[[pos]]],
								value2 = consumeMediaInParens[tokens[[pos]]];
								If[MatchQ[value1, Hold[Or[___]]], 
									value2 = Replace[value2, Hold[x___] :> Hold[Or[x]]];
									value1 = Join[value1, value2, 2];
									,
									value1 = Thread[Or[value1, value2], Hold]
								];
								,
								Throw @ 
									Failure["BadMedia", <|
										"Message" -> "Expected <media-in-parens> after 'not'.",
										"Parsed"  -> 1.0|>]
							];
							AdvancePosAndSkipWhitespace[pos, l, tokens];
						,
						True, 
							Throw @ 
								Failure["BadMedia", <|
									"Message" -> "Expected 'and' or 'or' token.",
									"Parsed"  -> 1.0|>]
					]
				]
			,
			True,
				Throw @ 
					Failure["BadMedia", <|
						"Message" -> "Unexpected token in media condition.", 
						"Parsed"  -> 1.0|>]
		];
		value1
	]

(* can only be () or function token *)
consumeMediaInParens[token_?CSSTokenQ] :=
	Module[{pos = 1, l, tokens, value, booleanError, plainError, rangeError, conditionError, all},
		Switch[token["Type"],
			"()", 
				Which[
					(* attempt to consume as <mf-boolean> *)
					!FailureQ[value = booleanError = Catch @ consumeMediaFeatureBoolean[token["Children"]]],
						Null
					,
					(*Echo[booleanError, "bool"];*)
					(* attempt to consume as <mf-plain> *)
					!FailureQ[value = plainError = Catch @ consumeMediaFeaturePlain[token["Children"]]],
						Null
					,
					(*Echo[plainError, "plain"];*)
					(* attempt to consume as <mf-range> *)
					!FailureQ[value = rangeError = Catch @ consumeMediaFeatureRange[token["Children"]]],
						Null
					,
					(*Echo[rangeError, "range"];*)
					(* attempt to consume as <media-condition> *)
					!FailureQ[value = conditionError = Catch @ consumeMediaCondition[token["Children"]]],
						Null
					,
					(*Echo[conditionError, "cond"];*)
					(* attempt to consume as <ident> <any-value> *)
					True,
						tokens = token["Children"]; l = Length[tokens];
						TrimWhitespaceTokens[pos, l, tokens];
						If[TokenTypeIs["ident", tokens[[pos]]], 
							If[pos == l, 
								Throw @ booleanError
								,
								value = Hold[TrueQ @ False] (* valid but Indeterminate *)
							]
							,
							all = {booleanError, plainError, rangeError, conditionError};
							all = all[[FirstPosition[all[[All, 2, "Parsed"]], Max[all[[All, 2, "Parsed"]]]][[1]]]];
							Throw @ all(*Failure[all[[1]], KeyDrop[all[[2]], "Parsed"]]*)
						]
				]
			,
			"function", 
				value = Hold[TrueQ @ False], (* valid but Indeterminate *)
			_, 
				Throw @ 
					Failure["BadMedia", <|
						"Message"  -> "Expected <media-in-parens> production.",
						"Position" -> showError[pos, pos, {token}]|>]
		];
		value
	]


SetAttributes[consumeMediaFeatureRangeOperator, HoldFirst];
consumeMediaFeatureRangeOperator[pos_, l_, tokens_] :=
	Which[
		And[
			TokenTypeIs["delim", tokens[[pos]]],
			TokenStringIs["<", tokens[[pos]]],
			pos + 1 <= l,
			TokenTypeIs["delim", tokens[[pos + 1]]],
			TokenStringIs["=", tokens[[pos + 1]]]
		],
			pos++; AdvancePosAndSkipWhitespace[pos, l, tokens]; LessEqual
		,		
		And[
			TokenTypeIs["delim", tokens[[pos]]],
			TokenStringIs[">", tokens[[pos]]],
			pos + 1 <= l,
			TokenTypeIs["delim", tokens[[pos + 1]]],
			TokenStringIs["=", tokens[[pos + 1]]]
		],
			pos++; AdvancePosAndSkipWhitespace[pos, l, tokens]; GreaterEqual
		,
		TokenTypeIs["delim", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"<", AdvancePosAndSkipWhitespace[pos, l, tokens]; Less,
				">", AdvancePosAndSkipWhitespace[pos, l, tokens]; Greater,
				"=", AdvancePosAndSkipWhitespace[pos, l, tokens]; Equal,
				_,   Throw @ Failure["BadMFRange", <|"Message" -> "Unrecognized media feature range operator.", "Op" -> tokens[[pos]]["String"]|>]
			]
		,
		True,
			Throw @ Failure["BadMFRange", <|"Message" -> "Unrecognized media feature range operator.", "Op" -> tokens[[pos]]["String"]|>]
	]


consumeMediaFeatureRange[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], name, delimStart, inRegularOrder, valueEnd, value1, value2, op1, op2, op1P = {}, op2P = {}},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]] && isMediaFeatureName[tokens[[pos]]["String"]],
			(* case: <mf-name> <op> <mf-value> *)
			inRegularOrder = True;
			name = tokens[[pos]]["String"];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			op1 = Catch @ consumeMediaFeatureRangeOperator[pos, l, tokens];
			If[FailureQ[op1], Throw @ modifyError[op1, pos, -1, tokens, "Parsed" -> 0.33]];
			value1 = Catch @ consumeMediaFeature[name, tokens[[pos ;; l]]];
			If[FailureQ[value1], Throw @ modifyError[value1, pos, -1, tokens, "Parsed" -> 1.0]];
			,
			(* case: <mf-value1> <op1> <mf-name> and optional <op2> <mf-value2> *)
			inRegularOrder = False;
			delimStart = pos;      AdvancePosToNextDelimiter[delimStart, l, tokens];
			valueEnd = delimStart; RetreatPosAndSkipWhitespace[valueEnd, 1, tokens]; 
			value1 = tokens[[pos ;; valueEnd]];
			pos = delimStart;
			AppendTo[op1P, pos];
			op1 = Catch @ consumeMediaFeatureRangeOperator[pos, l, tokens]; 
			If[FailureQ[op1], Throw @ modifyError[op1, pos, -1, tokens, "Parsed" -> 0.33]];
			AppendTo[op1P, pos]; RetreatPosAndSkipWhitespace[op1P[[2]], 1, tokens];
			Which[
				StringStartsQ[tokens[[pos]]["String"], "min-" | "max-"],
					Throw @ 
						Failure["BadMFRange", <|
							"Message"  -> "Cannot use min/max prefixes in media range.", 
							"Position" -> showError[pos, pos, tokens],
							"Parsed"   -> 1.0|>]
				,
				!isRangedMediaFeature[tokens[[pos]]["String"]],
					Throw @ 
						Failure["BadMFRange", <|
							"Message"  -> "Media feature name is not of 'range' type.", 
							"Position" -> showError[pos, pos, tokens],
							"Parsed"   -> 1.0|>]
				,
				isMediaFeatureName[tokens[[pos]]["String"]], 
					name = tokens[[pos]]["String"];
					AdvancePosAndSkipWhitespace[pos, l, tokens];
					value1 = consumeMediaFeature[name, value1];
				,
				True,
					Throw @ 
						Failure["BadMFRange", <|
							"Message"  -> "Expected a supported media feature name.",
							"Position" -> showError[pos, pos, tokens],
							"Parsed"   -> 1.0|>]
			];
			If[pos < l,
				AppendTo[op2P, pos];
				op2 = consumeMediaFeatureRangeOperator[pos, l, tokens]; 
				AppendTo[op2P, pos]; RetreatPosAndSkipWhitespace[op2P[[2]], 1, tokens];
				Which[
					MatchQ[op1, Equal],
						Throw @ 
							Failure["BadMFRange", <|
								"Message"  -> "Double-range format does not allow '=' as an operator.",
								"Position" -> showError[op1P[[1]], op1P[[2]], tokens],
								"Parsed"   -> 1.0|>]
					,
					MatchQ[op2, Equal],
						Throw @ 
							Failure["BadMFRange", <|
								"Message"  -> "Double-range format does not allow '=' as an operator.",
								"Position" -> showError[op2P[[1]], op2P[[2]], tokens],
								"Parsed"   -> 1.0|>]
					,
					Or[
						MatchQ[op1, LessEqual | Less] && MatchQ[op2, GreaterEqual | Greater],
						MatchQ[op2, LessEqual | Less] && MatchQ[op1, GreaterEqual | Greater]
					],
						Throw @ 
							Failure["BadMFRange", <|
								"Message" -> "Double-range format has both greater and less operators.",
								"Op"      -> {op1, op2},
								"Parsed"  -> 1.0|>]
					,
					True,
						Null
				];
				value2 = consumeMediaFeature[name, tokens[[pos ;; l]]];
			];
		];
		If[ValueQ[op2],
			(* if there's a second value, then merge the two together *)
			value1 = 
				Switch[op1,
					LessEqual,    Replace[value1, Hold[Equal[x_, y_]] :> Hold[GreaterEqual[x, y]]],
					Less,         Replace[value1, Hold[Equal[x_, y_]] :> Hold[Greater[x, y]]],
					GreaterEqual, Replace[value1, Hold[Equal[x_, y_]] :> Hold[LessEqual[x, y]]],
					Greater,      Replace[value1, Hold[Equal[x_, y_]] :> Hold[Less[x, y]]],
					_,            "Unreachable state."
				];
			value2 = 
				Switch[op2,
					LessEqual,    Replace[value2, Hold[Equal[x_, y_]] :> Hold[LessEqual[x, y]]],
					Less,         Replace[value2, Hold[Equal[x_, y_]] :> Hold[Less[x, y]]],
					GreaterEqual, Replace[value2, Hold[Equal[x_, y_]] :> Hold[GreaterEqual[x, y]]],
					Greater,      Replace[value2, Hold[Equal[x_, y_]] :> Hold[Greater[x, y]]],
					_,            "Unreachable state."
				];
			value1 = Thread[And[value1, value2], Hold];
			,
			(* single values may be in a reversed order *)
			If[inRegularOrder,
				value1 = 
					Switch[op1,
						LessEqual,    Replace[value1, Hold[Equal[x_, y_]] :> Hold[LessEqual[x, y]]],
						Less,         Replace[value1, Hold[Equal[x_, y_]] :> Hold[Less[x, y]]],
						GreaterEqual, Replace[value1, Hold[Equal[x_, y_]] :> Hold[GreaterEqual[x, y]]],
						Greater,      Replace[value1, Hold[Equal[x_, y_]] :> Hold[Greater[x, y]]],
						_,            "Unreachable state."
					]
				,
				value1 = 
					Switch[op1,
						LessEqual,    Replace[value1, Hold[Equal[x_, y_]] :> Hold[GreaterEqual[x, y]]],
						Less,         Replace[value1, Hold[Equal[x_, y_]] :> Hold[Greater[x, y]]],
						GreaterEqual, Replace[value1, Hold[Equal[x_, y_]] :> Hold[LessEqual[x, y]]],
						Greater,      Replace[value1, Hold[Equal[x_, y_]] :> Hold[Less[x, y]]],
						_,            "Unreachable state."
					]
			]
			
		];
		value1
	]
	
consumeMediaFeaturePlain[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], name, value, s},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			s = ToLowerCase @ tokens[[pos]]["String"];
			If[isMediaFeatureName[s] || StringStartsQ[s, "min-" | "max-"],
				name = s;
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Throw @ 
					Failure["BadMFPlain", <|
						"Message"  -> "Expected a supported media feature name.",
						"Position" -> showError[pos, pos, tokens],
						"Parsed"   -> 0.0|>]
			]
		];
		If[pos <= l && TokenTypeIs["colon", tokens[[pos]]],
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw @ 
				Failure["BadMFPlain", <|
					"Message"  -> "Expected a colon token.",
					"Position" -> showError[pos, pos, tokens],
					"Parsed"   -> 0.33|>]
		];
		value = Catch @ consumeMediaFeature[name, tokens[[pos ;; l]]];
		If[FailureQ[value], 
			(* a colon implies a plain media feature was attempted *)
			Throw @ modifyError[value, pos, -1, tokens, "Parsed" -> 1.0] 
			,
			value
		]
	]

consumeMediaFeatureBoolean[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[pos < l, 
			Throw @ 
				Failure["BadMFBoolean", <|
					"Message"  -> "Too many tokens in the media feature boolean.", 
					"Position" -> showError[pos + 1, -1, tokens],
					"Parsed"   -> 0.0|>]
		];
		Which[
			TokenTypeIs["ident", tokens[[pos]]],
				If[!StringStartsQ[tokens[[pos]]["String"], "min-" | "max-", IgnoreCase -> True],
					If[isMediaFeatureName[tokens[[pos]]["String"]],
						value = mediaFeatureBoolean[ToLowerCase @ tokens[[pos]]["String"]]
						,
						Throw @ 
							Failure["BadMFBoolean", <|
								"Message"  -> "Expected a supported media feature name.",
								"Position" -> showError[pos, pos, tokens],
								"Parsed"   -> 1.0|>]
					]
					,
					Throw @ 
						Failure["BadMFBoolean", <|
							"Message"  -> "Prefixes are not allowed for media features with no value.",
							"Position" -> showError[pos, pos, tokens],
							"Parsed"   -> 1.0|>]
				]
			,
			TokenTypeIs["()" | "function", tokens[[pos]]],
				Throw @ 
					Failure["BadMFBoolean", <|
						"Message"  -> "Expected a media feature name.",
						"Position" -> showError[pos, pos, tokens],
						"Parsed"   -> 0.0|>] 
			,
			True,
				Throw @ 
					Failure["BadMFBoolean", <|
						"Message"  -> "Expected a supported media feature name.",
						"Position" -> showError[pos, pos, tokens],
						"Parsed"   -> 1.0|>] 
		];
		With[{v = value}, Hold[v]]
	]
	
(*isRatio[pos_, l_, tokens_] :=
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
	]*)
	
isMediaFeatureName[s_?StringQ] := 
	StringMatchQ[
		s, 
		Alternatives["width", "height", "aspect-ratio", "resolution", "color",
			"color-index", "monochrome", "device-width", "device-height", "device-aspect-ratio",
			"orientation", "scan", "grid", "update", "overflow-block", "overflow-inline",
			"color-gamut", "hover", "any-hover", "pointer", "any-pointer"],
		IgnoreCase -> True]
		
(*isMediaFeatureValue[pos_, l_, tokens_] :=
	Or[
		isRatio[pos, l, tokens],
		TokenTypeIs["dimension" | "ident" | "number", tokens[[pos]]]]*)

isRangedMediaFeature[s_?StringQ] :=
	StringMatchQ[
		s, 
		Alternatives[
			"width", "height", "aspect-ratio", "resolution", 
			"color", "color-index", "monochrome", 
			"device-width", "device-height", "device-aspect-ratio"], 
		IgnoreCase -> True]

isFalseInTheNegativeRange[s_?StringQ] :=
	StringMatchQ[
		s, 
		Alternatives[
			"width", "height", "resolution", 
			"color", "color-index", "monochrome", 
			"device-width", "device-height"], 
		IgnoreCase -> True]
		

(* ranged features *)
mediaFeatureBoolean["width"] := True (* WD has a positive minimum window width so this is always positive *)
mediaFeatureBoolean["height"] := True (* WD has a positive minimum window height so this is always positive *)
mediaFeatureBoolean["aspect-ratio"] := True (* always non-zero *)
mediaFeatureBoolean["resolution"] := True
mediaFeatureBoolean["color"] := True
mediaFeatureBoolean["color-index"] := True (* FE at least using SRGB *)
mediaFeatureBoolean["monochrome"] := False (* assume running on a color device *)
(* deprecated ranged features but must be supported *)
mediaFeatureBoolean["device-width"] := True
mediaFeatureBoolean["device-height"] := True
mediaFeatureBoolean["device-aspect-ratio"] := True

(* discrete features *)
mediaFeatureBoolean["orientation"] := True
mediaFeatureBoolean["scan"] := True
mediaFeatureBoolean["grid"] := False (* FE does not run on grid devices, only bitmap devices *)
mediaFeatureBoolean["update"] := True (* FE is a fast-updating device *)
mediaFeatureBoolean["overflow-block"] := True (* FE uses 'scroll' *)
mediaFeatureBoolean["overflow-inline"] := True (* could be 'none'=False *)
mediaFeatureBoolean["color-gamut"] := True
mediaFeatureBoolean["hover"] := True 
mediaFeatureBoolean["any-hover"] := True 
mediaFeatureBoolean["pointer"] := True (* FE runs on either computer+mouse or on a touchscreen device. *)
mediaFeatureBoolean["any-pointer"] := True 

mediaFeatureBoolean[_?StringQ] := False

(*TODO: is this list necessary? *)
(*CSSMediaFeatureProperties = <|
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
|>*)


consumeMediaFeature[name_?StringQ /; StringStartsQ[name, "min-" | "max-", IgnoreCase -> True], tokens:{__?CSSTokenQ}] := 
	Module[{s, value},
		s = StringDrop[name, 4];
		If[isMediaFeatureName[s],
			If[isRangedMediaFeature[s],
				value = consumeMediaFeature[s, tokens];
				value = 
					If[StringStartsQ[name, "min-", IgnoreCase -> True],
						Replace[value, Hold[Equal[x_, y_]] :> Hold[GreaterEqual[x, y]]]
						,
						(* should we test for "false in the negative range" here? *)
						Replace[value, Hold[Equal[x_, y_]] :> If[TrueQ @ Negative[y], Hold[False], Hold[LessEqual[x, y]]]]
					]
				,
				Throw @	Failure["BadMFName", <|"Message" -> "Media feature name does not support the min/max prefix."|>]
			]
			,
			Throw @	Failure["BadMFName", <|"Message" -> "Unrecognized media feature name."|>]
		]; 
		value
	]
	
consumeMediaFeature[name:"orientation", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens]},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[ToLowerCase @ tokens[[pos]]["String"],
				"portrait",  Hold[CurrentValue[InputNotebook[], {WindowSize, 1}] <= CurrentValue[InputNotebook[], {WindowSize, 2}]],
				"landscape", Hold[CurrentValue[InputNotebook[], {WindowSize, 1}] >  CurrentValue[InputNotebook[], {WindowSize, 2}]],
				_,           Throw @ Failure["BadMFValue", <|"Message" -> "Orientation must be portrait or landscape keyword."|>] 
			]
			,
			Throw @	Failure["BadMFValue", <|"Message" -> "Orientation must be portrait or landscape keyword."|>]
		]
	]

consumeMediaFeature[name:"aspect-ratio", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], numerator, denomenator},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]], 
			If[tokens[[pos]]["ValueType"] === "integer",
				numerator = parseNumber[tokens[[pos]]];
				If[numerator < 1, Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio numerator must be a positive integer."|>]]
				,
				Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio numerator is not an integer."|>]
			]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio must be a ratio of two integers."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["/", tokens[[pos]]],
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio must use '/' delimiter."|>]
		];
		If[TokenTypeIs["number", tokens[[pos]]], 
			If[tokens[[pos]]["ValueType"] === "integer",
				denomenator = parseNumber[tokens[[pos]]];
				If[denomenator < 1, Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio denomenator must be a positive integer."|>]]
				,
				Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio denomenator is not an integer."|>]
			]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Aspect-ratio must be a ratio of two integers."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ Failure["BadMFValue", <|"Message" -> "Too many tokens in the media feature value."|>]
			,
			With[{v = Rational[numerator, denomenator]}, 
				Hold[(CurrentValue[InputNotebook[], {WindowSize, 1}]/CurrentValue[InputNotebook[], {WindowSize, 2}]) == v]
			]
		]
	]

consumeMediaFeature[name:"width", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["dimension", tokens[[pos]]], 
			value = parseLength[tokens[[pos]]]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Width must be a length with dimensions."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[CurrentValue[InputNotebook[], {WindowSize, 1}] == v]]
		]
	]
	
consumeMediaFeature[name:"height", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["dimension", tokens[[pos]]], 
			value = parseLength[tokens[[pos]]]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Height must be a length with dimensions."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[CurrentValue[InputNotebook[], {WindowSize, 2}] == v]]
		]
	]


(* FIXME:
	When querying media with non-square pixels, resolution queries the density in the vertical dimension. 
	We can query the FE's detected devices, but it doesn't tell us which screen the notebook is on... *)
(* Printers may have a higher resolution than the screen. Is this a concern? *)
consumeMediaFeature[name:"resolution", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		Which[
			TokenTypeIs["dimension", tokens[[pos]]], 
				value = parseDimension[tokens[[pos]]]
			,
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["infinity", tokens[[pos]]],
				value = Infinity
			,
			True,
				Throw @ 
					Failure["BadMFValue", <|
						"Message"  -> "Resolution must be a dimensions with unit dpi, dppx, or dpcm.",
						"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			(* FIXME: this should be more descirption e.g. CurrentValue[InputNotebook[], "Resolution"] but not possible... *)
			With[{v = value}, Hold[First["Resolution" /. SystemInformation["Devices", "ScreenInformation"]] == v]]
		]
	]

(* 
	There's no method available to the FE to determine the monitor device's scan type. 
	Almost all devices use 'progressive'. *)
consumeMediaFeature[name:"scan", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"interlace",   value = False,
				"progressive", value = True,
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Scan must be either 'interlace' or 'progressive'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'interlace' or 'progressive' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]

(* Legacy feature. FE only runs on bitmap devices. *)
consumeMediaFeature[name:"grid", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]] && tokens[[pos]]["ValueType"] === "integer",
			value = parseNumber[tokens[[pos]]];
			If[value != 0 || value != 1,
				Throw @ 
					Failure["BadMFValue", <|
						"Message"  -> "Grid media feature must be either 0 or 1.",
						"Position" -> showError[pos, pos, tokens]|>]
			]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = MatchQ[value, 0]}, Hold[v]]
		]
	]
	
consumeMediaFeature[name:"update", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"none", value = False,
				"slow", value = False,
				"fast", value = True, (* FE only runs on "fast" displays. *)
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Update must be either 'none', 'slow' or 'fast'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'none', 'slow' or 'fast' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]

consumeMediaFeature[name:"overflow-block", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"none",           value = False,
				"scroll",         value = True,  
				"optional-paged", value = False, 
				"paged",          value = False,
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Update must be either 'none', 'scroll', 'optional-paged' or 'paged'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'none', 'scroll', 'optional-paged' or 'paged' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]
	
consumeMediaFeature[name:"overflow-inline", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"none",           value = False,
				"scroll",         value = True,  
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Update must be either 'none' or 'scroll'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'none' or 'scroll' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]

(* FIXME?: I'm assuming the device has a uniform bit depth and is not monochrome. *)
consumeMediaFeature[name:"color", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]] && tokens[[pos]]["ValueType"] === "integer",
			value = parseNumber[tokens[[pos]]];
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Color media feature must be an integer.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[First["BitDepth" /. SystemInformation["Devices", "ScreenInformation"]] == v]]
		]
	]

(* FIXME: can we even test for this? Default to True for now. *)
consumeMediaFeature[name:"color-index", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]] && tokens[[pos]]["ValueType"] === "integer",
			value = parseNumber[tokens[[pos]]];
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Color-index media feature must be an integer.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			Hold[True](*With[{v = value}, Hold[TrueQ[v]]]*)
		]
	]
	
(* FIXME: can we even test for this? Default to False for now as FE usually runs on color devices. *)
consumeMediaFeature[name:"monochrome", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]] && tokens[[pos]]["ValueType"] === "integer",
			value = parseNumber[tokens[[pos]]];
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Monochrome media feature must be an integer.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			Hold[False](*With[{v = value}, Hold[TrueQ[v]]]*)
		]
	]

(* FIXME?: not sure if we can test for this. We assume most devices are SRGB. *)
consumeMediaFeature[name:"color-gamut", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"srgb",    value = True,
				"p3",      value = False,
				"rec2020", value = False,  
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Color-gamut must be either 'srgb', 'p3' or 'rec2020'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'srgb', 'p3' or 'rec2020' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]

(* FE runs on either computer+mouse or on a touchscreen device which are 'fine' pointer devices. *)
consumeMediaFeature[name:"pointer" | "any-pointer", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"none",   value = False,
				"coarse", value = False,
				"fine",   value = True,  
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Pointer must be either 'none', 'coarse' or 'fine'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'none', 'coarse' or 'fine' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]
	
(* FIXME?
	FE runs on either computer+mouse (with hover) or on a touchscreen device (no hover). 
	For now assume hover is available. *)
consumeMediaFeature[name:"hover" | "any-hover", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[tokens[[pos]]["String"],
				"none",  value = False,
				"hover", value = True,
				_,             
					Throw @ 
						Failure["BadMFValue", <|
							"Message"  -> "Pointer must be either 'none' or 'hover'.",
							"Position" -> showError[pos, pos, tokens]|>]
			]
			,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Expected 'none' or 'hover' keyword.",
					"Position" -> showError[pos, pos, tokens]|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[v]]
		]
	]
	
(* FIXME: only detects the first monitor device... *)	
consumeMediaFeature[name:"device-width", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["dimension", tokens[[pos]]], 
			value = parseLength[tokens[[pos]]]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Device-width must be a length with dimensions."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[With[{w = First["FullScreenArea" /. SystemInformation["Devices", "ScreenInformation"]][[1]]}, w[[2]]-w[[1]]] == v]]
		]
	]

(* FIXME: only detects the first monitor device... *)	
consumeMediaFeature[name:"device-height", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], value},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["dimension", tokens[[pos]]], 
			value = parseLength[tokens[[pos]]]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Device-height must be a length with dimensions."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ 
				Failure["BadMFValue", <|
					"Message"  -> "Too many tokens in the media feature value.", 
					"Position" -> showError[pos, -1, tokens]|>]
			,
			With[{v = value}, Hold[With[{w = First["FullScreenArea" /. SystemInformation["Devices", "ScreenInformation"]][[2]]}, w[[2]]-w[[1]]] == v]]
		]
	]
	
consumeMediaFeature[name:"device-aspect-ratio", tokens:{__?CSSTokenQ}] := 
	Module[{pos = 1, l = Length[tokens], numerator, denomenator},
		TrimWhitespaceTokens[pos, l, tokens];
		If[TokenTypeIs["number", tokens[[pos]]], 
			If[tokens[[pos]]["ValueType"] === "integer",
				numerator = parseNumber[tokens[[pos]]];
				If[numerator < 1, Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio numerator must be a positive integer."|>]]
				,
				Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio numerator is not an integer."|>]
			]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio must be a ratio of two integers."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["/", tokens[[pos]]],
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio must use '/' delimiter."|>]
		];
		If[TokenTypeIs["number", tokens[[pos]]], 
			If[tokens[[pos]]["ValueType"] === "integer",
				denomenator = parseNumber[tokens[[pos]]];
				If[denomenator < 1, Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio denomenator must be a positive integer."|>]]
				,
				Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio denomenator is not an integer."|>]
			]
			,
			Throw @ Failure["BadMFValue", <|"Message" -> "Device-aspect-ratio must be a ratio of two integers."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l,
			Throw @ Failure["BadMFValue", <|"Message" -> "Too many tokens in the media feature value."|>]
			,
			With[{v = Rational[numerator, denomenator]}, 
				Hold[With[{w = First["FullScreenArea" /. SystemInformation["Devices", "ScreenInformation"]]}, (w[[1,2]]-w[[1,1]])/(w[[2,2]]-w[[2,1]])] == v]
			]
		]
	]

consumeMediaFeature[_, _] := Throw @ Failure["BadMFName", <|"Message" -> "Unrecognized media feature name."|>]


(* ::Subsection::Closed:: *)
(*Error Highlighting*)


untokenizeWithoutComments[tokens_] := StringReplace[CSSUntokenize @ tokens, "/**/" -> ""]

showError[startPos_, stopPos_, tokens_] :=
	If[Length[tokens] < 3,
		Style[
			untokenizeWithoutComments @ tokens,
			Background -> Yellow]
		,
		Row[{
			untokenizeWithoutComments @ tokens[[;; startPos - 1]],
			Style[
				untokenizeWithoutComments @ tokens[[startPos ;; stopPos]],
				Background -> Yellow],
			untokenizeWithoutComments @ tokens[[stopPos + 1 ;; ]]
		}]
	]

ClearAll[modifyError];
modifyError[fail_Failure, pos1_, pos2_, tokens_, rule_:Nothing] :=
	Failure[fail[[1]], <|
		"Message"  -> fail[[2, "Message"]], 
		"Position" -> showError[pos1, pos2, tokens],
		rule|>]

End[] (* End Private Context *)

EndPackage[]