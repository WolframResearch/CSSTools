(* Wolfram Language Package *)

(* Expose these to top level *)
CSSGradientConvertAngleToPositions;
CSSGradientMakeBlendFromColorStops;
CSSGradientConvertRadialPosition;
CSSGradientConvertRadialDataRange;

BeginPackage["CSSTools`CSSImages3`", {"CSSTools`"}]

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSPropertyInterpreter` 
	---> defines parseSingleColor, parseAngle *)

Needs["CSSTools`CSSTokenizer`"];
Needs["CSSTools`CSSPropertyInterpreter`"];


Begin["`Private`"] (* Begin Private Context *) 

$Debug = False;

expectedFunctionFailure[type_] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a `Type` function.", 
		"MessageParameters" -> <|"Type" -> type|>|>]
missingColorFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a <color>.", 
		"MessageParameters" -> <||>|>]
missingColorStopFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a <color-stop>.", 
		"MessageParameters" -> <||>|>]
missingCommaFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a <comma>.", 
		"MessageParameters" -> <||>|>]
missingDirectionFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a direction left, right, top or bottom.", 
		"MessageParameters" -> <||>|>]
missingDirectionOrLengthFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a direction left, right, top or bottom, or a length.", 
		"MessageParameters" -> <||>|>]
missingPositionFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a <position>.", 
		"MessageParameters" -> <||>|>]
negativeSizeFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Radial gradient size must be non-negative.", 
		"MessageParameters" -> <||>|>]
relativeSizeFailure[] := 
	Failure["BadParseAtStart", <|
		"MessageTemplate"   -> "A circular radial gradient cannot use a relative size for the radius.", 
		"MessageParameters" -> <||>|>]
mismatchedSizeFailure[] := 
	Failure["BadParseAtStart", <|
		"MessageTemplate"   -> "The radial gradient size is inconsistent with its shape.", 
		"MessageParameters" -> <||>|>]
notEnoughTokensFailure[type_?StringQ] := 
	Failure["IncompleteParse", <|
		"MessageTemplate"   -> "Incomplete function. Missing a `Type` argument.", 
		"MessageParameters" -> <|"Type" -> type|>|>]
notEnoughTokensFailure[type_?ListQ] := 
	Failure["IncompleteParse", <|
		"MessageTemplate"   -> "Incomplete function. Missing `Type` arguments.", 
		"MessageParameters" -> <|"Type" -> StringJoin @ Riffle[type, " and "]|>|>]
colorHintDuplicationFailure[] :=
	Failure["BadParse", <|
		"MessageTemplate"   -> "A <color-hint> must come between two <color-stop> instances.", 
		"MessageParameters" -> <||>|>]


(* ========== utilities ========== *)
SetAttributes[{consumeLengthPercentage, consumeColorStop, consumeColorStopList, consumeColorHint}, HoldFirst];

consumeLengthPercentage[pos_, l_, tokens_] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				(* a bad dimension/percentage/number is a critical failure *)
				"dimension",  With[{p = parseLength @ tokens[[pos]]},     If[FailureQ[p], Throw @ p, p]],
				"percentage", With[{p = parsePercentage @ tokens[[pos]]}, If[FailureQ[p], Throw @ p, p]],
				"number",     With[{p = parseZero @ tokens[[pos]]},       If[FailureQ[p], Throw @ p, p]],
				_,            Return @ $Failed (* soft failure *)
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];	
		try
	]		

consumeColorStop[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try, stopColor, stopPosition = None},
		(* stop position and stop color can occur in any order; stop position is optional *)
		Which[
			!FailureQ[try = parseSingleColor["gradient", tokens[[pos]]]],
				stopColor = try; 
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				(* attempt to get optional stop position *)
				If[pos <= l,
					try = consumeLengthPercentage[pos, l, tokens];
					If[!FailureQ[try], stopPosition = try];
				];		
				{stopPosition, stopColor},
				
			!FailureQ[try = consumeLengthPercentage[pos, l, tokens]],
				stopPosition = try;
				(* attempt to get required stop color *)
				If[pos > l, 
					Throw @ notEnoughTokensFailure["<color>"]
					,
					(* lack of a color here is a critical error *)
					try = parseSingleColor["gradient", tokens[[pos]]];
					If[FailureQ[try], 
						Throw @ missingColorFailure[]
						, 
						AdvancePosAndSkipWhitespace[pos, l, tokens];
						{stopPosition, try}
					]
				],
			
			True,
				$Failed (* soft failure *)			
		]
	]
	
consumeColorHint[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{hint, pos = inputPos},
		(* attempt to get the hint *)
		hint = consumeLengthPercentage[pos, l, tokens];
			
		(* set token position and return {hint, "Hint"} for later parsing *)
		If[FailureQ[hint],
			hint (* don't advance position, don't record anything *)
			, 
			(* a hint must always be followed by a comma and a color stop *)
			If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop>"}]];
			If[TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				Throw @ missingCommaFailure[]
			];
			inputPos = pos;
			{hint, "Hint"}
		]
	]
	
(* The color-stop-list must have at least 2 colors. *)
consumeColorStopList[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try, stopList = ConstantArray[None, l - pos + 1], i = 1, hintAllowed = True},
		(* get first <color-stop> *)
		try = consumeColorStop[pos, l, tokens];
		If[FailureQ[try], Throw @ missingColorFailure[], stopList[[i++]] = try];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DA"]];
		
		If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop>"}]];
		If[TokenTypeIs["comma", tokens[[pos]]], 
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw @ missingCommaFailure[]
		];
			
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DB"]];
		
		(* get remaining colors *)
		If[pos > l, Throw @ notEnoughTokensFailure["<color-stop>"]];
		While[pos <= l,
			(* A <color-hint> is optional and must be between two <color-stop> instances. *)
			Which[
				!FailureQ[try = consumeColorHint[pos, l, tokens]],
					If[!hintAllowed, pos--; Throw @ colorHintDuplicationFailure[]];
					stopList[[i++]] = try;
					
					If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DD" <> ToString[i]]];
		
					hintAllowed = False,
					
				!FailureQ[try = consumeColorStop[pos, l, tokens]],
					stopList[[i++]] = try;
		
					If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DC" <> ToString[i]]];
		
					hintAllowed = True;
					If[pos > l, Break[]]; (* <color-stop-list> must end on a <color-stop> *)
					If[TokenTypeIs["comma", tokens[[pos]]], 
						AdvancePosAndSkipWhitespace[pos, l, tokens]
						,
						Throw @ missingCommaFailure[]
					],
					
				True,
					Throw @ missingColorStopFailure[]
			];
		];
		DeleteCases[stopList, None, {1}]		
	]
	
performColorStopFixupFixValues[inputPair_, length_] := 
	Replace[
		inputPair, 
		{x_, y_} :> 
			Switch[x, 
				_Plus,      {Replace[x, {z_Scaled :> First[z], z_?NumericQ :> z/length}, {1}], y},
				_Scaled,    {First[x], y},
				_?NumericQ, {x/length, y},
				_,          {x, y}
			]
	]

(* These rules are described in the spec under section 3.4.3.
	We also convert all lengths to scaled units. For example, if a color-stop is given at 25px,
	and the gradient line length is 150px, then the color stop is at position 1/6 along the gradient. *)
performColorStopFixup[colorStopList_, gradientLineLength_] :=
	Module[{csl = colorStopList, runs, temp, h, hints},
		csl = performColorStopFixupFixValues[#, gradientLineLength]& /@ csl;
	
		(* if the first or last color stops do not have a position, set them as endpoints*)
		If[csl[[ 1, 1]] === None, csl[[ 1, 1]] = 0];
		If[csl[[-1, 1]] === None, csl[[-1, 1]] = 1];
		
		(* if a stop or hint has a position smaller than one before it, make it the largest of all the ones before it *)
		Do[csl[[i, 1]] = If[csl[[i, 1]] === None, csl[[i, 1]], Max[DeleteCases[csl[[ ;; i, 1]], None]]], {i, 1, Length[csl]}];
		
		(* any runs of stops that still don't have values are evenly spaced *)
		runs = Split[First /@ Position[csl, None], #1 === #2 - 1 &];
		Do[
			temp = With[{l = csl[[First[#] - 1, 1]], r = csl[[Last[#] + 1, 1]]}, Range[l, r, (r - l)/(Length[#] + 1)]]& @ i;
			If[Length[temp] == 1, temp = First[temp], temp = temp[[2 ;; -2]]];
			csl[[i, 1]] = temp
			,
			{i, runs}
		];
		
		(* These steps are described in the spec under section 3.4.2 
			LinearGradientImage only uses linear interpolation. 
			To mimic non-linear behavior for gradient hints, we use a linear interpolation of the exponential given in the spec. *)
		hints = First /@ Position[csl, "Hint"];
		Do[
			(* determine location of transition hint as a percentage of the distance between the surrounding color stops *)
			h = (csl[[i, 1]] - csl[[i - 1, 1]])/(csl[[i + 1, 1]] + csl[[i - 1, 1]]);
			
			(* create linear interpolation of exponential *)
			csl[[i]] = CSSGradientHint[h, csl[[i - 1]], csl[[i + 1]]];
			,
			{i, hints}
		];
		
		(* correct corner case:
			If the distance of the WL Blend function is 0, then the blend is only the left-most color.
			This differs from the CSS spec which states a discontinuous transition should take place.
			To modify the WL behavior, we prepend the first color slightly to the left of itself or zero. 
			*)
		If[csl[[-1, 1]] - csl[[1, 1]] == 0, PrependTo[csl, {Min[csl[[1, 1]] - 1, 0], csl[[1, 2]]}]];
		
		With[{c = csl}, Blend[c, #]&]
	]
	
CSSGradientHint[percent_, {lp_, lc_}, {rp_, rc_}] := Sequence @@ Table[{lp + j*(rp - lp), Blend[{lc, rc}, j^Log[percent, 0.5]]}, {j, 0, 1, 0.01}]



(* ========== linear-gradient ========== *)
SetAttributes[{consumeLinearGradientFunction, consumeAngle, consumeSideOrCorner}, HoldFirst];

consumeAngle[pos_, l_, tokens_] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				(* a bad dimension/number is a critical failure *)
				"dimension",  With[{p = parseAngle @ tokens[[pos]]}, If[FailureQ[p], Throw @ p, p]],
				"number",     With[{p = parseZero @ tokens[[pos]]},  If[FailureQ[p], Throw @ p, p]],
				_,            Return @ $Failed (* soft failure *)
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		try
	]

consumeSideOrCorner[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{lr = None, tb = None},
		(* It is assumed the "to" token has already been consumed and whitespace skipped *)
		(* Thus we _must_ have a direction token here *)
		If[TokenTypeIsNot["ident", tokens[[pos]]], Throw @ missingDirectionFailure[]]; 
		
		(* start parsing through the directions *)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
			"left",   lr = Left,
			"right",  lr = Right,
			"top",    tb = Top,
			"bottom", tb = Bottom,
			_,        Throw @ missingDirectionFailure[]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]];
		
		(* Attempt to parse an optional second side specification *)
		(* Only a valid, non-repeating ident token will move the position tracker forward *)
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
				"left",   If[lr === None, lr = Left;   AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"right",  If[lr === None, lr = Right;  AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"top",    If[tb === None, tb = Top;    AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"bottom", If[tb === None, tb = Bottom; AdvancePosAndSkipWhitespace[pos, l, tokens]],
				_,        Throw @ missingDirectionFailure[]
			]
		];
		
		(* Return parsed side spec. Translates CSS spec to WL LinearGradientImage direction spec *)
		Switch[{lr, tb},
			{Left,  None},   {Right,  Left},
			{Right, None},   {Left,   Right},
			{None,  Top},    {Bottom, Top},
			{None,  Bottom}, {Top,    Bottom},
			{Left,  Top},    {{Right, Bottom}, {Left,  Top}},
			{Right, Top},    {{Left,  Bottom}, {Right, Top}},
			{Left,  Bottom}, {{Right, Top},    {Left,  Bottom}},
			{Right, Bottom}, {{Left,  Top},    {Right, Bottom}},
			_,               Throw @ Failure["ImpossibleStateLGISOC", <|"MessageTemplate" -> "This error should not be possible."|>]
		]
	]

consumeLinearGradientFunction[pos_, length_, tokens:{___?CSSTokenQ}] :=
	Module[{l = length, try, direction = None, csl},
		(* pre-checks *)
		If[l == 0 || l == 1 && TokenTypeIs["whitespace", tokens[[1]]], 
			Throw @ notEnoughTokensFailure[{"<color-stop>", "<comma>", "<color-stop>"}]
		];
		TrimWhitespaceTokens[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], direction}, "A"]];
				
		(* get optional angle or to-side-or-corner syntax *)
		Which[
			!FailureQ[try = consumeAngle[pos, l, tokens]],
				direction = try Degree;
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], direction}, "BA"]],
				
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["to", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				direction = consumeSideOrCorner[pos, l, tokens];
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], direction}, "BB"]],
				
			True, 
				Null (* assume only color-stop-list is present *)	
		];
		If[direction =!= None,
			If[TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Throw @ missingCommaFailure[]
			]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], direction}, "C"]];
		
		(* get color-stop-list *)
		If[pos > l, Throw @ notEnoughTokensFailure["<color-stop-list>"]];
		csl = consumeColorStopList[pos, l, tokens];
		
		(* with every part parsed, combine results and interpret *)
		
		(* this is also a check for failure *)
		try = CSSGradientMakeBlendFromColorStops[direction, csl, {150,150}];
		
		(* 
			CSS has a special case when the gradient points to a corner. In this case, no matter the size
			of the gradient image, the opposite corners have a constant color line between them. We mimic
			this using the DataRange option. *)
		With[{d = direction, c = csl},
			Switch[d,
				None,       
					LinearGradientImage[{Top, Bottom} -> CSSGradientMakeBlendFromColorStops[d, c, #], #]&,
				
				_?NumericQ, 
					LinearGradientImage[CSSGradientConvertAngleToPositions[d, #] -> CSSGradientMakeBlendFromColorStops[d, c, #], #]&,
				
				{{Left|Right, Top|Bottom}, {Left|Right, Top|Bottom}}, 
					LinearGradientImage[d -> CSSGradientMakeBlendFromColorStops[d, c, #], #, DataRange -> {{0, 1}, {0, 1}}]&,
				
				_?ListQ,    
					LinearGradientImage[d -> CSSGradientMakeBlendFromColorStops[d, c, #], #]&,
				
				_, 
					Throw @ Failure["ImpossibleStateLGI", <|"MessageTemplate" -> "This error should not be possible."|>]
			]
		]
	]

parseLinearGradientFunction[prop_String, token_?CSSTokenQ] :=
	Module[{pos = 1, l, tokens, try},
		If[Not[TokenTypeIs["function", token] && TokenStringIs["linear-gradient", token]],
			Return @ expectedFunctionFailure["linear-gradient"]
		];
		
		(* prepare the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		try = Catch @ consumeLinearGradientFunction[pos, l, tokens];
		If[FailureQ[try], 
			If[First[try] === "IncompleteParse",
				Replace[try, 
					Failure[s_, a_?AssociationQ] :> 
						Failure[s, <|a, "Expr" -> token["String"] <> "(" <> CSSUntokenize[tokens] <> "\!\(\*StyleBox[\" ...\",Background->RGBColor[1,1,0]]\)" <> ")"|>]]
				,
				Replace[try, 
				Failure[s_, a_?AssociationQ] :> 
					Failure[s, <|a, "Expr" -> token["String"] <> "(" <> HighlightUntokenize[tokens, If[pos > l, {{pos-1}}, {{pos}}]] <> ")"|>]]
			] 
			,
			try
		]
	]


CSSGradientConvertAngleToPositions[angle_, imageSize:{w_, h_}] :=
	Module[{gradientLineVector},
		gradientLineVector = 1.*(Abs[w*Sin[angle]] + Abs[h*Cos[angle]])*{Sin[angle], Cos[angle]};
		{imageSize - gradientLineVector, imageSize + gradientLineVector}/2
	]
	
CSSGradientMakeBlendFromColorStops[direction_, colorstops_, imageSize:{w_, h_}] :=
	Module[{gradientLineLength},
		gradientLineLength = 
			Switch[direction,
				(* case: default top-to-bottom so use image height*)
				None, imageSize[[2]], 
				
				(* case: angle*Degree *)
				_?NumericQ,	EuclideanDistance @@ CSSGradientConvertAngleToPositions[direction, imageSize], 
				
				(* cases: to side or corner directions *)
				{Left | Right, Left | Right},                                 imageSize[[1]],
				{Top | Bottom, Top | Bottom},                                 imageSize[[2]],
				{{Right | Left, Top | Bottom}, {Right | Left, Top | Bottom}}, EuclideanDistance[{0, 0}, imageSize],
				
				(* case: point to point *)
				{{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}, EuclideanDistance @@ direction,
				
				(* fallthrough should not be possible *)
				_, Throw @ Failure["BadParse", <|"MessageTemplate" -> "Direction should be None, a number, or sides."|>]
			];
		performColorStopFixup[colorstops, gradientLineLength]
	]
	


(* ========== radial gradient ========== *)
SetAttributes[{
	consumeRadialGradientFunction, consumeRadialGradientEndingShape, consumeRadialGradientSize, 
	consumePosition, consumePositionType1, consumePositionType2, consumePositionType3}, HoldFirst];

consumeRadialGradientEndingShape[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"circle",  "circle",
						"ellipse", "ellipse",
						_,         Return @ $Failed (* soft error because <size> could also be an ident token *)
					],
				_, Return @ $Failed (* soft error *)
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; 
		try
	]

consumeRadialGradientSize[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try, size},
		try =
			If[TokenTypeIs["ident", tokens[[pos]]],
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"closest-corner",  "closest-corner",
					"closest-side",    "closest-side",
					"farthest-corner", "farthest-corner",
					"farthest-side",   "farthest-side",
					_,                 $Failed (* soft failure because <at-position> could follow with 'at' ident token *)
				]
				,
				consumeLengthPercentage[pos, l, tokens]
			];
		
		Which[
			FailureQ[try], try, 
			
			StringQ[try], 
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				try,
				
			True,
				size = {try};
				(* attempt to get another length *)
				If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]]; 
				try = consumeLengthPercentage[pos, l, tokens];
				If[FailureQ[try], size,	Append[size, try]]
		]
	]

(* This is a 1- or 2-argument format of only ident tokens; if a missing 2nd arg then Center is implied *)
(* [left|center|right] || [top|center|bottom] *)
consumePositionType1[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, leftright = None, topbottom = None},
		If[TokenTypeIsNot["ident", tokens[[pos]]], Return @ $Failed];
		
		(* get first of {x, y} pair *)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
			"left",   leftright = Left,
			"right",  leftright = Right,
			"center", leftright = Center, (* this assignment could change if a 2nd keyword is provided *)
			"top",    topbottom = Top,
			"bottom", topbottom = Bottom,
			_,        Return @ $Failed (* soft error *)
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* always advance if no error detected *)
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom}, "CPT1A"]];
		
		(* attempt to get optional 2nd position *)
		(* If a second value is duplicated, then assume we've hit the end of this <position> spec *)
		If[pos <= l && TokenTypeIs["ident", tokens[[pos]]], 
			Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
				"left",   
					Switch[leftright, 
						Center, AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Left; topbottom = Center,
						None,   AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Left,
						_,      Null
					],
				"right",  
					Switch[leftright, 
						Center, AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Right; topbottom = Center,
						None,   AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Right,
						_,      Null
					],
				"center",
					If[topbottom === None, 
						AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Center
						,
						AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Center
					],
				"top",    If[topbottom === None, AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Top],
				"bottom", If[topbottom === None, AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Bottom],
				_,        Null
			]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom}, "CPT1B"]];
		
		(* return parsed value and update position tracker *)
		inputPos = pos;
		{leftright, topbottom}
	]

(* This is a 1- or 2-argument format; if a missing 2nd arg then Center is implied *)
(* [left|center|right|<length-percentage>] [top|center|bottom|<length-percentage>]? *)
consumePositionType2[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, leftright, topbottom, lengthpercentage = None},
		(* must start with a left/center/right position ident token or a <length-percentage> *)
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"left",   leftright = Left,
					"right",  leftright = Right,
					"center", leftright = Center,
					_,        Return @ $Failed (* soft error *)
				],
			"number",     lengthpercentage = parseZero @ tokens[[pos]],
			"dimension",  lengthpercentage = parseLength @ tokens[[pos]],
			"percentage", lengthpercentage = parsePercentage @ tokens[[pos]],
			_,            Return @ $Failed (* soft error *)
		];
		If[FailureQ[lengthpercentage], Throw @ missingDirectionOrLengthFailure[] (* critical error *)];
		If[lengthpercentage =!= None, leftright = lengthpercentage; lengthpercentage = None];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* always advance if no error detected *)
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT2A"]];
		
		(* attemp to get optional second value; must be top/center/bottom or a <length-percentage> *)
		If[pos > l, Return @ {leftright, Center}];
		Switch[tokens[[pos]]["Type"],
			"ident",
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"center", topbottom = Center,
					"top",    topbottom = Top,
					"bottom", topbottom = Bottom,
					_,        inputPos = pos; Return @ {leftright, Center} 
				],
			"number",     lengthpercentage = parseZero @ tokens[[pos]],
			"dimension",  lengthpercentage = parseLength @ tokens[[pos]],
			"percentage", lengthpercentage = parsePercentage @ tokens[[pos]],
			_,            inputPos = pos; Return @ {leftright, Center} 
		];
		If[FailureQ[lengthpercentage], Throw @ missingDirectionOrLengthFailure[] (* critical error *)];
		If[lengthpercentage =!= None, topbottom = lengthpercentage];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* always advance if no error detected *)
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT2B"]];
		
		(* return parsed value and update position tracker *)
		inputPos = pos;
		{leftright, topbottom}		
	]
	
(* This is a 4-argument format *)
(* [[left|right] <length-percentage>] && [[top|bottom] <length-percentage>] *)
consumePositionType3[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, leftright = None, topbottom = None, lengthpercentage = None},
		(* must start with a position ident token excluding "center" *)
		If[TokenTypeIsNot["ident", tokens[[pos]]], Return @ $Failed];
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
			"left",   leftright = {Left},
			"right",  leftright = {Right},
			"top",    topbottom = {Top},
			"bottom", topbottom = {Bottom},
			_,        Return @ $Failed (* soft error *)
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT3A"]];
				
		(* each <ident> must be followed by a <length-percentage> *)
		If[pos > l, Return @ $Failed];
		lengthpercentage = consumeLengthPercentage[pos, l, tokens];
		If[FailureQ[lengthpercentage], 
			Return @ $Failed
			, 
			If[Length[leftright] == 1, AppendTo[leftright, lengthpercentage], AppendTo[topbottom, lengthpercentage]]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT3B"]];
				
		(* get mandatory 2nd spec *)
		If[pos > l, Return @ $Failed];
		If[TokenTypeIsNot["ident", tokens[[pos]]], Return @ $Failed];
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
			"left",   If[leftright === None, leftright = {Left},   Return @ $Failed],
			"right",  If[leftright === None, leftright = {Right},  Return @ $Failed],
			"top",    If[topbottom === None, topbottom = {Top},    Return @ $Failed],
			"bottom", If[topbottom === None, topbottom = {Bottom}, Return @ $Failed],
			_,        Return @ $Failed (* soft error *)
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT3C"]];
				
		(* each <ident> must be followed by a <length-percentage> *)
		If[pos > l, Return @ $Failed];
		lengthpercentage = consumeLengthPercentage[pos, l, tokens];
		If[FailureQ[lengthpercentage], 
			Return @ $Failed
			, 
			If[Length[leftright] == 1, AppendTo[leftright, lengthpercentage], AppendTo[topbottom, lengthpercentage]]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT3D"]];
				
		inputPos = pos;
		{leftright, topbottom}
	]

consumePosition[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try},
		Which[
			!FailureQ[try = consumePositionType3[pos, l, tokens]], try,
			!FailureQ[try = consumePositionType2[pos, l, tokens]], try,
			!FailureQ[try = consumePositionType1[pos, l, tokens]], try,
			True,                                                  Return @ $Failed
		]
	]

consumeRadialGradientFunction[pos_, length_, tokens:{___?CSSTokenQ}] :=
	Module[{try, csl, l = length, gradientSize = None, gradientPos = None, gradientEndingShape = None},
		(* pre-checks *)
		If[l == 0 || l == 1 && TokenTypeIs["whitespace", tokens[[1]]], 
			Throw @ notEnoughTokensFailure[{"<color-stop>", "<comma>", "<color-stop>"}]];
		TrimWhitespaceTokens[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "A"]];
		
		(* get optional <ending-shape> and/or <size> syntax *)
		Which[
			(* we check for the circle/ellipse ending shape first as its presence is the easiest to rule out *)
			!FailureQ[try = consumeRadialGradientEndingShape[pos, l, tokens]],
				gradientEndingShape = try;
				If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]];
				try = consumeRadialGradientSize[pos, l, tokens];
				If[!FailureQ[try], gradientSize = try],
				
			!FailureQ[try = consumeRadialGradientSize[pos, l, tokens]],
				gradientSize = try;
				If[pos > l, Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]];
				
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "AA"]];
				
				try = consumeRadialGradientEndingShape[pos, l, tokens];
				
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "AB"]];
				
				If[!FailureQ[try], gradientEndingShape = try],
				
			True, 
				Null (* assume <ending-shape> and/or <size> is not present *)	
		];
		
		(* negative size values are not allowed *)
		If[ListQ[gradientSize] && AnyTrue[gradientSize, If[MatchQ[#, Scaled[_]], Negative[First[#]], Negative[#]]&], Throw @ negativeSizeFailure[]];
		
		(* set ending shape if not specified, check shape for errors *)
		If[gradientEndingShape === None,
			gradientEndingShape = If[ListQ[gradientSize] && Length[gradientSize] == 1, "circle", "ellipse"]
		];
		If[gradientEndingShape === "circle" && ListQ[gradientSize],
			Which[
				MatchQ[gradientSize, {_Scaled}], pos--; Throw @ relativeSizeFailure[],
				MatchQ[gradientSize, {_, _}],    pos--; Throw @ mismatchedSizeFailure[]
			]
		];
		If[gradientEndingShape === "ellipse" && ListQ[gradientSize] && !MatchQ[gradientSize, {_, _}], pos--; Throw @ mismatchedSizeFailure[]];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "B"]];
		
		(* get optional 'at <position>' syntax *)
		If[(gradientEndingShape =!= None || gradientSize =!= None) && pos > l, 
			Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "BB"]];
				
		If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["at", tokens[[pos]]],
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* a position _must_ be consumed following the 'at' ident token *)
			If[pos > l, Throw @ notEnoughTokensFailure[{"<position>", "<comma>", "<color-stop-list>"}]]; 
			
			If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "BC"]];
			
			try = consumePosition[pos, l, tokens];
			If[FailureQ[try], Throw @ missingPositionFailure[], gradientPos = try]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "C"]];
		
		(* must have a comma before the color-stop-list *)
		If[gradientPos =!= None && pos > l, 
			Throw @ notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}]
		];
		If[gradientEndingShape =!= None || gradientSize =!= None || gradientPos =!= None, 
			If[TokenTypeIs["comma", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Throw @ missingCommaFailure[]
			]
		];
		
		
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "D"]];
	
		(* get color-stop-list *)
		If[pos > l, Throw @ notEnoughTokensFailure["<color-stop-list>"]];
		csl = consumeColorStopList[pos, l, tokens];
		
		
		
		{gradientPos, gradientSize, gradientEndingShape, csl}		
	]
	
parseRadialGradientFunction[token_?CSSTokenQ] :=
	Module[{try, pos = 1, l, tokens},
		If[Not[TokenTypeIs["function", token] && TokenStringIs["radial-gradient", token]],
			Return @ expectedFunctionFailure["radial-gradient"]
		];
		
		(* consume the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		try = Catch @ consumeRadialGradientFunction[pos, l, tokens];
		If[FailureQ[try], 
			Which[
				First[try] === "IncompleteParse",
					Replace[try, 
						Failure[s_, a_?AssociationQ] :> 
							Failure[s, <|a, "Expr" -> token["String"] <> "(" <> CSSUntokenize[tokens] <> "\!\(\*StyleBox[\" ...\",Background->RGBColor[1,1,0]]\)" <> ")"|>]]
				,
				First[try] === "BadParseAtStart",
					Replace[try, 
						Failure[s_, a_?AssociationQ] :> 
							Failure[s, <|a, "Expr" -> token["String"] <> "(" <> HighlightUntokenize[tokens, List /@ Range[If[pos > l, pos-1, pos]]] <> ")"|>]]
				,
				True,
					Replace[try, 
						Failure[s_, a_?AssociationQ] :> 
							Failure[s, <|a, "Expr" -> token["String"] <> "(" <> HighlightUntokenize[tokens, If[pos > l, {{pos-1}}, {{pos}}]] <> ")"|>]]
			] 
			,
			try
		]
	]


(* 4-arg case of <position> is a coordinate plus an offset; offset can be a percentage (scaled) *)
convertPositionSpec[position:{{lr_, xoffset_}, {tb_, yoffset_}}, imageSize:{w_, h_}] :=
	With[
		{
			x = If[MatchQ[xoffset, _Scaled], w*First[xoffset], xoffset], 
			y = If[MatchQ[yoffset, _Scaled], h*First[yoffset], yoffset]},
		{If[lr === Left, x, w - x], If[tb === Top, h - y, y]}
	]

(* 2-arg case of <position> *)
convertPositionSpec[position:{x_, y_}, imageSize:{w_, h_}] :=
	{
		Switch[x,
			Scaled[_], First[x]*w,
			Left,      0,
			Right,     w,
			Center,    w/2,
			_,         x],
		Switch[y,
			Scaled[_], First[y]*h,
			Top,       h,
			Bottom,    0,
			Center,    h/2,
			_,         y]}
			
getClosestSidePoint[pos:{x_, y_}, imageSize:{w_, h_}] :=
	First[Extract[{{0, y}, {x, 0}, {w, y}, {x, h}}, Position[{x, y, w-x, h-y}, Min[{x, y, w-x, h-y}]]]]
getFarthestSidePoint[pos:{x_, y_}, imageSize:{w_, h_}] :=
	First[Extract[{{0, y}, {x, 0}, {w, y}, {x, h}}, Position[{x, y, w-x, h-y}, Max[{x, y, w-x, h-y}]]]]
getClosestCornerPoint[pos:{x_, y_}, imageSize:{w_, h_}] := {If[x > w/2, w, 0], If[y > h/2, h, 0]}
getFarthestCornerPoint[pos:{x_, y_}, imageSize:{w_, h_}] := {If[x > w/2, 0, w], If[y > h/2, 0, h]}
		
getRadialStartAndEndPositions[position_, size_, imageSize:{w_, h_}] :=
	Module[{startPos, endPos},
		startPos = convertPositionSpec[position, imageSize];
		endPos =
			Switch[size,
				"closest-side",    getClosestSidePoint[startPos, imageSize],
				"farthest-side",   getFarthestSidePoint[startPos, imageSize],
				"closest-corner",  getClosestCornerPoint[startPos, imageSize],  (* not actually used *)
				"farthest-corner", getFarthestCornerPoint[startPos, imageSize], (* not actually used *)
				{_},               startPos + {size[[1]], 0}, (* cannot be a Scaled value from earlier checks *)
				{_, _},            convertPositionSpec[size, imageSize]
			];
		{startPos, endPos}
	]

CSSGradientConvertRadialPosition[position_, size_, imageSize:{w_, h_}] :=
	getRadialStartAndEndPositions[position, size, imageSize]
	
CSSGradientConvertRadialPosition[position_, "closest-corner", imageSize:{w_, h_}] :=
	{
		imageSize/2, 
		imageSize/2 + {EuclideanDistance @@ getRadialStartAndEndPositions[position, "closest-side", imageSize], 0}}

(* only used if gradient is detected to be an ellipse *)
(* elliptical gradients are always on axis, never rotated *)
CSSGradientConvertRadialDataRange[position_, size_, imageSize:{w_, h_}] :=
	Module[{dr = {{0, w}, {0, h}}, r, x1, y1, x2, y2, d, x0, y0, dx, dy, xp, yp},
		{{x1, y1}, {x2, y2}} = getRadialStartAndEndPositions[position, size, imageSize];
		r = EuclideanDistance[{x1, y1}, {x2, y2}];
		
		Switch[size,
			"closest-side",    
				(* assumes starting position is not on an edge (degenerate case); find the other sides *)
				Switch[{x2, y2},
					{0 | w, _}, (* left or right side *)
						If[y1 > h/2, 
							d = h-y1-r; dr[[2, 1]] -= d; dr[[2, 2]] -= d; dr[[2, 1]] += h*d/(d+r)
							,
							d = y1-r;   dr[[2, 1]] += d; dr[[2, 2]] += d; dr[[2, 2]] -= h*d/(d+r)
						], 
					{_, 0 | h}, (* top or bottom side *)
						If[x1 < w/2,
							d = x1-r;   dr[[1, 1]] += d; dr[[1, 2]] += d; dr[[1, 2]] -= w*d/(d+r)
							,
							d = w-x1-r; dr[[1, 1]] -= d; dr[[1, 2]] -= d; dr[[1, 1]] += w*d/(d+r);
						]
				];
				,
			"farthest-side",   (*TODO: incorrect, fix this *)
				Switch[{x2, y2},
					{0 | w, _}, (* left or right side *)
						If[y1 > h/2, 
							d = Abs[y1-r];   dr[[2, 1]] -= d; dr[[2, 2]] -= d; dr[[2, 2]] += h*d/(d+r)
							,
							d = Abs[r-h+y1]; dr[[2, 1]] += d; dr[[2, 2]] += d; dr[[2, 1]] -= h*d/(d+r)
						], 
					{_, 0 | h}, (* top or bottom side *)
						If[x1 < w/2,
							d = Abs[w-x1-r]; dr[[1, 1]] += d; dr[[1, 2]] += d; dr[[1, 1]] -= w*d/(d+r)
							,
							d = Abs[x1-r];   dr[[1, 1]] -= d; dr[[1, 2]] -= d; dr[[1, 2]] += w*d/(d+r)
						]
				],
			"closest-corner", 
				{{x1, y1}, {x2, y2}} = getRadialStartAndEndPositions[position, "closest-side", imageSize];
				r = EuclideanDistance[{x1, y1}, {x2, y2}]; 
				{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
				dx = Sqrt[2](w/2 - Abs[x0]) - r;
				dy = Sqrt[2](h/2 - Abs[y0]) - r;
				xp = Abs[-(w/2)dx/(r+dx)]1.;
				yp = Abs[-(h/2)dy/(r+dy)]1.;
				dr = {{xp - x0*(w - 2*xp)/w, w - xp - x0*(w - 2*xp)/w}, {yp - y0*(h - 2*yp)/h, h - yp - y0*(h - 2*yp)/h}},
			"farthest-corner", 
				{{x1, y1}, {x2, y2}} = getRadialStartAndEndPositions[position, "farthest-side", imageSize];
				r = EuclideanDistance[{x1, y1}, {x2, y2}]; 
				{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
				dx = Sqrt[2](w/2 + Abs[x0]) - r;
				dy = Sqrt[2](h/2 + Abs[y0]) - r;
				xp = Abs[-(w/2)dx/(r+dx)]1.;
				yp = Abs[-(h/2)dy/(r+dy)]1.;
				dr = {{xp(* - 2*x0*(w/2-xp)/w*), w - xp(* - 2*x0*(w/2-xp)/w*)}, {yp(* - y0*(h - 2*yp)/h*), h - yp(* - y0*(h - 2*yp)/h*)}},
			{_},               Null,
			{_, _},            Null
		];
		dr
	]

	

End[] (* End Private Context *)

EndPackage[]