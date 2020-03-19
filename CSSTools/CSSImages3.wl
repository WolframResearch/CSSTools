(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSImages3`", {"CSSTools`"}]

CSSGradientConvertAngleToPositions;
CSSGradientMakeBlendFromColorStops;

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSPropertyInterpreter` 
	---> defines parseSingleColor, parseAngle *)

Needs["CSSTools`CSSTokenizer`"];
Needs["CSSTools`CSSPropertyInterpreter`"]


Begin["`Private`"] (* Begin Private Context *) 


expectedFunctionFailure[type_] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a `Type` function.", 
		"MessageParameters" -> <|"Type" -> type|>|>]
missingColorFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a color.", 
		"MessageParameters" -> <||>|>]
missingCommaFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a comma.", 
		"MessageParameters" -> <||>|>]
missingDirectionFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Expected a direction left, right, top or bottom.", 
		"MessageParameters" -> <||>|>]
duplicateDirectionFailure[] := 
	Failure["BadParse", <|
		"MessageTemplate"   -> "Duplicated direction specification.", 
		"MessageParameters" -> <||>|>]

SetAttributes[{consumeLinearColorStop, consumeColorStopList, consumeLinearColorHint, consumeSideOrCorner}, HoldFirst];

consumeLinearColorStop[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, c, stopPosition = None},
		(* get the stop color *)
		c = parseSingleColor["linear-gradient", tokens[[pos]]];
		If[FailureQ[c], Throw @ c];
			
		(* get the optional stop position *)
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos <= l, 
			stopPosition = 
				Switch[tokens[[pos]]["Type"],
					"dimension",  parseLength @ tokens[[pos]],
					"percentage", parsePercentage @ tokens[[pos]],
					_,            None
				];
				
			(* set token position and return {color, stop-position} pair *)
			If[stopPosition =!= None, AdvancePosAndSkipWhitespace[pos, l, tokens]]
		];
	
		inputPos = pos;
		{stopPosition, c}
	]
	
consumeLinearColorHint[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, hint},
		(* get the hint *)
		hint = 
			Switch[tokens[[pos]]["Type"],
				"dimension",  parseLength @ tokens[[pos]],
				"percentage", parsePercentage @ tokens[[pos]],
				_,            None
			];
			
		(* set token position and return {"Hint", hint} for later parsing *)
		If[hint === None,
			Nothing (* don't advance position, don't record anything *)
			, 
			(* a hint must always be followed by a comma *)
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]; inputPos = pos;
				,
				inputPos = pos; Throw @ missingCommaFailure[]
			];
			{hint, "Hint"}
		]
	]
	
consumeSideOrCorner[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, lr = None, tb = None},
		(* it is assumed the "to" token has already been consumed and whitespace skipped *)
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
		If[pos > l, Throw @ missingCommaFailure[]];
		
		(* attempt to parse a second side specification *)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
			"left",   
				If[lr =!= None, 
					Throw @ duplicateDirectionFailure[]
					, 
					lr = Left; AdvancePosAndSkipWhitespace[pos, l, tokens];
				],
			"right",  
				If[lr =!= None, 
					Throw @ duplicateDirectionFailure[]
					, 
					lr = Right; AdvancePosAndSkipWhitespace[pos, l, tokens];
				],
			"top",    
				If[tb =!= None, 
					Throw @ duplicateDirectionFailure[]
					, 
					tb = Top; AdvancePosAndSkipWhitespace[pos, l, tokens];
				],
			"bottom", 
				If[tb =!= None, 
					Throw @ duplicateDirectionFailure[]
					, 
					tb = Bottom; AdvancePosAndSkipWhitespace[pos, l, tokens];
				],
			_, Null (* assuming then there is only one side spec so don't do anything *)
		];
		
		(* advance past expected comma *)
		If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
			AdvancePosAndSkipWhitespace[pos, l, tokens]; inputPos = pos
			,
			inputPos = pos; Throw @ missingCommaFailure[]
		];
		
		(* return parsed side spec *)
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

(* The color-stop-list must have at least 2 colors. *)
consumeColorStopList[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{stopList = {}},
		(* get first color-stop *)
		If[pos > l, Throw @ missingColorFailure[]];
		AppendTo[stopList, consumeLinearColorStop[pos, l, tokens]];
		If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw @ missingCommaFailure[]
		];
			
		(* get remaining colors *)
		While[pos <= l,
			AppendTo[stopList, consumeLinearColorHint[pos, l, tokens]];
			If[pos > l, Throw @ missingColorFailure[]];
			AppendTo[stopList, consumeLinearColorStop[pos, l, tokens]];
			If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Break[]
			];
		];
		stopList		
	]


parseLinearGradientFunction[prop_String, token_?CSSTokenQ] :=
	Module[{tokens, pos = 1, l, try, direction = None, csl},
		If[Not[TokenTypeIs["function", token] && TokenStringIs["linear-gradient", token]],
			Throw @ expectedFunctionFailure[prop]
		];
		
		(* prepare the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		If[l == 0 || l == 1 && TokenTypeIs["whitespace", tokens[[1]]], Throw @ missingColorFailure[]];
		TrimWhitespaceTokens[pos, l, tokens];
		
		(* get optional angle or side-to-corner syntax *)
		Which[
			!FailureQ[try = parseAngle[tokens[[pos]]]],
				direction = try Degree; 
				(* advance to color-stop-list *)
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
					AdvancePosAndSkipWhitespace[pos, l, tokens]
					,
					Throw @ missingCommaFailure[]
				],
				
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["to", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				direction = consumeSideOrCorner[pos, l, tokens];,
				
			True, 
				Null (* assume only color-stop-list is present *)	
		];
		
		(* get color-stop-list *)
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
				{Left | Right, _},                      imageSize[[1]],
				{Top | Bottom, _},                      imageSize[[2]],
				{{Right | Left, Top | Bottom}, {_, _}}, EuclideanDistance[{0, 0}, imageSize],
				
				(* fallthrough should not be possible *)
				_, Throw @ Failure["BadParse", <|"MessageTemplate" -> "Direction should be None, a number, or sides."|>]
			];
		performColorStopFixup[colorstops, gradientLineLength]
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


(* ========== radial gradient ========== *)

(*parseRadialGradientFunction[prop_String, token_?CSSTokenQ] :=
	Module[{tokens, pos = 1, l, try, direction = None, gradientLineLength, imageSize = {150, 150}},
		If[Not[TokenTypeIs["function", token] && TokenStringIs["linear-gradient", token]],
			Return @ expectedFunctionFailure[prop]
		];
		
		(* prepare the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		TrimWhitespaceTokens[pos, l, tokens];
		
		(* get optional angle or side-to-corner syntax *)
		Which[
			!FailureQ[try = parseAngle[tokens[[pos]]]],
				direction = try Degree; 
				(* advance to color-stop-list *)
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				If[pos <= l && TokenTypeIs["comma", tokens[[pos]]], 
					AdvancePosAndSkipWhitespace[pos, l, tokens]
					,
					Throw @ missingCommaFailure[]
				],
				
			TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["to", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				direction = consumeSideOrCorner[pos, l, tokens];,
				
			True, 
				Null (* assume only color-stop-list is present *)	
		];
		
		(* get color-stop-list *)
		try = consumeColorStopList[pos, l, tokens];
		
		(* with every part parsed, combine results and interpret *)
		gradientLineLength = 
			Switch[direction,
				None,       
					imageSize[[2]], (* use height since default gradient direction is top-to-bottom *)
				_?NumericQ, 
					With[{pts = CSSGradientConvertAngleToPositions[direction, imageSize]}, 
						Power[Plus @@ Power[pts[[2]] - pts[[1]], 2], 1/2]],
				_?ListQ, Power[Plus @@ Power[direction[[2]] - direction[[1]], 2], 1/2]
			];
		try = performColorStopFixup[try, gradientLineLength];
		
		(* 
			CSS has a special case when the gradient points to a corner. In this case, no matter the size
			of the gradient image, the opposite corners have a constant color line between them. We mimic
			this using the DataRange option. *)
		With[{d = direction, t = try},
			Switch[d,
				None,       LinearGradientImage[{Top, Bottom} -> t, #]&,
				_?NumericQ, LinearGradientImage[CSSGradientConvertAngleToPositions[d, #] -> t, #]&,
				{{Left|Right, Top|Bottom}, {Left|Right, Top|Bottom}}, LinearGradientImage[d -> t, #, DataRange -> {{0, 1}, {0, 1}}]&,
				_?ListQ,    LinearGradientImage[d -> t, #]&,
				_,          Throw @ Failure["ImpossibleStateLGI", <|"MessageTemplate" -> "This error should not be possible."|>]
			]
		]
	]*)

	

End[] (* End Private Context *)

EndPackage[]