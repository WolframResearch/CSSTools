(* ::Package:: *)

(* ::Section::Closed:: *)
(*Header*)


(* Wolfram Language Package *)

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
mismatchedSizeFailure[tag_:"BadParseAtStart"] := 
	Failure[tag, <|
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


(* ::Section::Closed:: *)
(*Utilities*)


SetAttributes[
	{
		consumeLengthPercentage, consumeColorStop, consumeColorStopList, consumeColorHint,
		consumePosition, consumePositionType1, consumePositionType2, consumePositionType3,
		consumeAngle}, 
	HoldFirst];


(* ::Subsection::Closed:: *)
(*Consume Angle*)


consumeAngle[pos_, l_, tokens_] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				(* a bad dimension/number is a critical failure *)
				"dimension",  With[{p = parseAngle @ tokens[[pos]]}, If[FailureQ[p], Throw[p, CSSImages3], p]],
				"number",     With[{p = parseZero @ tokens[[pos]]},  If[FailureQ[p], Throw[p, CSSImages3], p]],
				_,            Return @ $Failed (* soft failure *)
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		try
	]


(* ::Subsection::Closed:: *)
(*Consume Length-Percentage*)


consumeLengthPercentage[pos_, l_, tokens_] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				(* a bad dimension/percentage/number is a critical failure *)
				"dimension",  With[{p = parseLength @ tokens[[pos]]},     If[FailureQ[p], Throw[p, CSSImages3], p]],
				"percentage", With[{p = parsePercentage @ tokens[[pos]]}, If[FailureQ[p], Throw[p, CSSImages3], p]],
				"number",     With[{p = parseZero @ tokens[[pos]]},       If[FailureQ[p], Throw[p, CSSImages3], p]],
				_,            Return @ $Failed (* soft failure *)
			];
		AdvancePosAndSkipWhitespace[pos, l, tokens];	
		try
	]


(* ::Subsection::Closed:: *)
(*Consume Position*)


(* This is a 1- or 2-argument format of only ident tokens; if a missing 2nd arg then Center is implied *)
(* [left|center|right] || [top|center|bottom] *)
consumePositionType1[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, leftright = Automatic, topbottom = Automatic},
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
						Center,    AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Left; topbottom = Center,
						Automatic, AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Left,
						_,         Null
					],
				"right",  
					Switch[leftright, 
						Center,    AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Right; topbottom = Center,
						Automatic, AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Right,
						_,         Null
					],
				"center",
					If[topbottom === Automatic, 
						AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Center
						,
						AdvancePosAndSkipWhitespace[pos, l, tokens]; leftright = Center
					],
				"top",    If[topbottom === Automatic, AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Top],
				"bottom", If[topbottom === Automatic, AdvancePosAndSkipWhitespace[pos, l, tokens]; topbottom = Bottom],
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
	Module[{pos = inputPos, leftright, topbottom, lengthpercentage = Automatic},
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
		If[FailureQ[lengthpercentage], Throw[missingDirectionOrLengthFailure[], CSSImages3] (* critical error *)];
		If[lengthpercentage =!= Automatic, leftright = lengthpercentage; lengthpercentage = Automatic];
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
		If[FailureQ[lengthpercentage], Throw[missingDirectionOrLengthFailure[], CSSImages3] (* critical error *)];
		If[lengthpercentage =!= Automatic, topbottom = lengthpercentage];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* always advance if no error detected *)
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], leftright, topbottom, lengthpercentage}, "CPT2B"]];
		
		(* return parsed value and update position tracker *)
		inputPos = pos;
		{leftright, topbottom}		
	]
	
(* This is a 4-argument format *)
(* [[left|right] <length-percentage>] && [[top|bottom] <length-percentage>] *)
consumePositionType3[inputPos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = inputPos, leftright = Automatic, topbottom = Automatic, lengthpercentage = Automatic},
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
			"left",   If[leftright === Automatic, leftright = {Left},   Return @ $Failed],
			"right",  If[leftright === Automatic, leftright = {Right},  Return @ $Failed],
			"top",    If[topbottom === Automatic, topbottom = {Top},    Return @ $Failed],
			"bottom", If[topbottom === Automatic, topbottom = {Bottom}, Return @ $Failed],
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


(* ::Subsection::Closed:: *)
(*Consume Color Stops and Hints*)


consumeColorStop[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try, stopColor, stopPosition = Automatic},
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
					Throw[notEnoughTokensFailure["<color>"], CSSImages3]
					,
					(* lack of a color here is a critical error *)
					try = parseSingleColor["gradient", tokens[[pos]]];
					If[FailureQ[try], 
						Throw[missingColorFailure[], CSSImages3]
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
			If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop>"}], CSSImages3]];
			If[TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				Throw[missingCommaFailure[], CSSImages3]
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
		If[FailureQ[try], Throw[missingColorFailure[], CSSImages3], stopList[[i++]] = try];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DA"]];
		
		If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop>"}], CSSImages3]];
		If[TokenTypeIs["comma", tokens[[pos]]], 
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			Throw[missingCommaFailure[], CSSImages3]
		];
			
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], stopList}, "DB"]];
		
		(* get remaining colors *)
		If[pos > l, Throw[notEnoughTokensFailure["<color-stop>"], CSSImages3]];
		While[pos <= l,
			(* A <color-hint> is optional and must be between two <color-stop> instances. *)
			Which[
				!FailureQ[try = consumeColorHint[pos, l, tokens]],
					If[!hintAllowed, pos--; Throw[colorHintDuplicationFailure[], CSSImages3]];
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
						Throw[missingCommaFailure[], CSSImages3]
					],
					
				True,
					Throw[missingColorStopFailure[], CSSImages3]
			];
		];
		DeleteCases[stopList, None, {1}]		
	]


(* ::Subsection::Closed:: *)
(*Color Stop Fixup*)


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
		If[MatchQ[csl[[ 1, 1]], None | Automatic], csl[[ 1, 1]] = 0];
		If[MatchQ[csl[[-1, 1]], None | Automatic], csl[[-1, 1]] = 1];
		
		(* if a stop or hint has a position smaller than one before it, make it the largest of all the ones before it *)
		Do[csl[[i, 1]] = If[MatchQ[csl[[i, 1]], None | Automatic], csl[[i, 1]], Max[DeleteCases[csl[[ ;; i, 1]], None | Automatic]]], {i, 1, Length[csl]}];
		
		(* any runs of stops that still don't have values are evenly spaced *)
		runs = Split[First /@ Position[csl, None | Automatic], #1 === #2 - 1 &];
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
		
		With[{c = csl}, Blend[c, #]&]
	]


CSSGradientHint[percent_, {lp_, lc_}, {rp_, rc_}] := 
	Module[{samplePoints, j},
		If[percent == 0., Return @ {lp, rc}];
		If[percent == 1., Return @ {rp, lc}];
		
		(* use WL Plot to recursively sample the transition curve, adding detail at larger curvature *)
		samplePoints = Cases[Plot[j^Log[percent, 0.5], {j, 0, 1}], Line[x_] :> x, Infinity][[1, All, 1]];
		If[!MatchQ[samplePoints, {__?NumericQ}], samplePoints = Table[j, {j, 0, 1, 0.001}]];
		
		Sequence @@ Table[{lp + j*(rp - lp), Blend[{lc, rc}, j^Log[percent, 0.5]]}, {j, samplePoints}]
	]


(* ::Subsection::Closed:: *)
(*Other*)


capEndsOfShortCSL[blend_Function] := 
	Module[{distances, colors},
		distances = blend[[1, 1]][[All, 1]];
		colors = blend[[1, 1]][[All, 2]];
		If[First[distances] > 0, PrependTo[distances, 0]; PrependTo[colors, First[colors]]];
		If[Last[distances] < 1, AppendTo[distances, 1]; AppendTo[colors, Last[colors]]];
		With[{b = Thread[{distances, colors}]}, Blend[b, #]&]
	]


duplicateCSLBlocks[blend_Function, gradLength_, imageSize:{w_, h_}] :=
	Module[{distances, i, cslLength, cslList},
		(* convert color stop list to absolute lengths *)
		cslList = Thread[{blend[[1, 1]][[All, 1]]*gradLength, blend[[1, 1]][[All, 2]]}];
		
		distances = cslList[[All, 1]];
		cslLength = Last[distances] - First[distances];
		
		Join @@ Table[Plus[{i, 0}, #]& /@ cslList, {i, -Ceiling[First[distances]/cslLength]*cslLength, Sqrt[w*w+h*h], cslLength}]
	]


getGradientLineLength[direction_, imageSize:{w_, h_}] :=
	Switch[direction,
		(* case: default top-to-bottom so use image height*)
		None | Automatic, imageSize[[2]], 
		
		(* case: angle*Degree *)
		_?NumericQ,	EuclideanDistance @@ convertAngleToPositions[direction, imageSize], 
		
		(* cases: to side or corner directions *)
		{Left | Right, Left | Right},                                 imageSize[[1]],
		{Top | Bottom, Top | Bottom},                                 imageSize[[2]],
		{{Right | Left, Top | Bottom}, {Right | Left, Top | Bottom}}, EuclideanDistance[{0, 0}, imageSize],
		
		(* case: point to point *)
		{{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}, EuclideanDistance @@ direction,
		
		(* fallthrough should not be possible *)
		_, Throw[Failure["BadParse", <|"MessageTemplate" -> "Direction should be Automatic, a number, or sides."|>], CSSImages3]
	]


makeBlendFromColorStops[direction_, colorstops_, imageSize:{w_, h_}] :=
	performColorStopFixup[colorstops, getGradientLineLength[direction, imageSize]]


convertAngleToPositions[angle_, imageSize:{w_, h_}] :=
	Module[{gradientLineVector},
		gradientLineVector = 1.*(Abs[w*Sin[angle]] + Abs[h*Cos[angle]])*{Sin[angle], Cos[angle]};
		{imageSize - gradientLineVector, imageSize + gradientLineVector}/2
	]


(* ::Section:: *)
(*Linear Gradient*)


SetAttributes[{consumeLinearGradientFunction, consumeSideOrCorner}, HoldFirst];


(* ::Subsection::Closed:: *)
(*Token Consumers*)


consumeSideOrCorner[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{lr = Automatic, tb = Automatic},
		(* It is assumed the "to" token has already been consumed and whitespace skipped *)
		(* Thus we _must_ have a direction token here *)
		If[TokenTypeIsNot["ident", tokens[[pos]]], Throw[missingDirectionFailure[], CSSImages3]]; 
		
		(* start parsing through the directions *)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
			"left",   lr = Left,
			"right",  lr = Right,
			"top",    tb = Top,
			"bottom", tb = Bottom,
			_,        Throw[missingDirectionFailure[], CSSImages3]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]];
		
		(* Attempt to parse an optional second side specification *)
		(* Only a valid, non-repeating ident token will move the position tracker forward *)
		If[TokenTypeIs["ident", tokens[[pos]]],
			Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
				"left",   If[lr === Automatic, lr = Left;   AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"right",  If[lr === Automatic, lr = Right;  AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"top",    If[tb === Automatic, tb = Top;    AdvancePosAndSkipWhitespace[pos, l, tokens]],
				"bottom", If[tb === Automatic, tb = Bottom; AdvancePosAndSkipWhitespace[pos, l, tokens]],
				_,        Throw[missingDirectionFailure[], CSSImages3]
			]
		];
		
		(* Return parsed side spec. Translates CSS spec to WL LinearGradientImage direction spec *)
		Switch[{lr, tb},
			{Left,      Automatic}, {Right,  Left},
			{Right,     Automatic}, {Left,   Right},
			{Automatic, Top},       {Bottom, Top},
			{Automatic, Bottom},    {Top,    Bottom},
			
			{Left,  Top},    {{Right, Bottom}, {Left,  Top}},
			{Right, Top},    {{Left,  Bottom}, {Right, Top}},
			{Left,  Bottom}, {{Right, Top},    {Left,  Bottom}},
			{Right, Bottom}, {{Left,  Top},    {Right, Bottom}},
			
			_,               Throw[Failure["ImpossibleStateLGISOC", <|"MessageTemplate" -> "This error should not be possible."|>], CSSImages3]
		]
	]


consumeLinearGradientFunction[pos_, length_, tokens:{___?CSSTokenQ}] :=
	Module[{l = length, try, direction = Automatic, csl},
		(* pre-checks *)
		If[l == 0 || l == 1 && TokenTypeIs["whitespace", tokens[[1]]], 
			Throw[notEnoughTokensFailure[{"<color-stop>", "<comma>", "<color-stop>"}], CSSImages3]
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
		If[direction =!= Automatic,
			If[TokenTypeIs["comma", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Throw @ missingCommaFailure[]
			]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], direction}, "C"]];
		
		(* get color-stop-list *)
		If[pos > l, Throw[notEnoughTokensFailure["<color-stop-list>"], CSSImages3]];
		csl = consumeColorStopList[pos, l, tokens];
		
		{direction, csl}
	]


(* ::Subsection::Closed:: *)
(*Options*)


Options[CSSLinearGradientImage] = {"Repeating" -> False};
SyntaxInformation[CSSLinearGradientImage] = {"ArgumentsPattern" -> {_., _., _., OptionsPattern[]}};


(* ::Subsection::Closed:: *)
(*Failure Messages*)


linearDirectionFailure[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The direction argument must be either Automatic, a numerical degree, or side to side directions.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 
colorStopFailure1[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The color stop argument must be a list of at least two color stops.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>]
colorStopFailure2[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The color stop argument has an invalid color stop or color hint.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>]
colorStopFailure3[arg_, pos_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "A <color-hint> must come between two <color-stop> instances.", 
		"Input" -> Join[If[pos == 1, {}, arg[[ ;; pos - 1]]], {Style[arg[[pos]], Background -> Yellow]}, If[pos == Length[arg], {}, arg[[pos + 1 ;;]]]],
		"MessageParameters" -> <||>|>]
imageSizeFailure[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The image size argument must be either Automatic, a number, or a pair of numbers.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>]


(* ::Subsection::Closed:: *)
(*Parse from CSS*)


doParseLinearGradientFunction[prop_String, token_?CSSTokenQ, isRepeating:(True | False)] :=
	Module[{pos = 1, l, tokens, try, functionHead, function},
		functionHead = If[isRepeating, "repeating-", ""] <> "linear-gradient";
		If[Not[TokenTypeIs["function", token] && TokenStringIs[functionHead, token]],
			Return @ expectedFunctionFailure[functionHead]
		];
		
		(* prepare the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		try = Catch[consumeLinearGradientFunction[pos, l, tokens], CSSImages3];
		If[FailureQ[try], 
			Return @ 
				If[First[try] === "IncompleteParse",
					Replace[try, 
						Failure[s_, a_?AssociationQ] :> 
							Failure[s, <|a, "Expr" -> token["String"] <> "(" <> CSSUntokenize[tokens] <> "\!\(\*StyleBox[\" ...\",Background->RGBColor[1,1,0]]\)" <> ")"|>]]
					,
					Replace[try, 
					Failure[s_, a_?AssociationQ] :> 
						Failure[s, <|a, "Expr" -> token["String"] <> "(" <> HighlightUntokenize[tokens, If[pos > l, {{pos-1}}, {{pos}}]] <> ")"|>]]
				]
		];
		
		function = With[{d = If[ListQ[try[[1]]], Last[try[[1]]], try[[1]]], csl = try[[2]]}, CSSLinearGradientImage[csl, d, #]&];
		If[isRepeating, Insert[function, "Repeating" -> True, {1, -1}], function]
	]


parseLinearGradientFunction[prop_String, token_?CSSTokenQ] := doParseLinearGradientFunction[prop, token, False]
	
parseRepeatingLinearGradientFunction[prop_String, token_?CSSTokenQ] := doParseLinearGradientFunction[prop, token, True]


(* ::Subsection::Closed:: *)
(*CSSLinearGradientImage*)


(* upgrade short-hand notation to full spec *)
CSSLinearGradientImage[opts:OptionsPattern[]] := CSSLinearGradientImage[{{Scaled[0], Black}, {Scaled[1], White}}, {Top, Bottom}, {150, 150}, opts]

CSSLinearGradientImage[colorStopList_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, {Top, Bottom}, {150, 150}, opts]

CSSLinearGradientImage[colorStopList_, direction_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, direction, {150, 150}, opts]

CSSLinearGradientImage[colorStopList_, Left,   imageSize_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, {Right, Left}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, Right,  imageSize_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, {Left, Right}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, Top,    imageSize_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, {Bottom, Top}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, Bottom, imageSize_, opts:OptionsPattern[]] := CSSLinearGradientImage[colorStopList, {Top, Bottom}, imageSize, opts]

CSSLinearGradientImage[colorStopList_, {Left, Top} | {Top, Left}, imageSize_, opts:OptionsPattern[]] := 
	CSSLinearGradientImage[colorStopList, {{Right, Bottom}, {Left, Top}}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, {Left, Bottom} | {Bottom, Left}, imageSize_, opts:OptionsPattern[]] := 
	CSSLinearGradientImage[colorStopList, {{Right, Top}, {Left, Bottom}}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, {Right, Top} | {Top, Right}, imageSize_, opts:OptionsPattern[]] := 
	CSSLinearGradientImage[colorStopList, {{Left, Bottom}, {Right, Top}}, imageSize, opts]
CSSLinearGradientImage[colorStopList_, {Right, Bottom} | {Bottom, Right}, imageSize_, opts:OptionsPattern[]] := 
	CSSLinearGradientImage[colorStopList, {{Left, Top}, {Right, Bottom}}, imageSize, opts]


CSSLinearGradientImage[colorStopList_, direction_, imageSize_, opts:OptionsPattern[]] := 
	Module[{try, hintAllowed = True, is, blend, colors, convertedDirection, distances},
		(* === direction checks === *)
		If[
			!MatchQ[
				direction, 
				Alternatives[
					None | Automatic,
					Times[_?NumericQ, Degree] | _?NumericQ,
					{{Left, Top}, {Right, Bottom}} | {{Left, Bottom}, {Right, Top}} | {{Right, Bottom}, {Left, Top}} | {{Right, Top}, {Left, Bottom}},
					{Left, Right} | {Right, Left},
					{Top, Bottom} | {Bottom, Top}]],
			Return @ linearDirectionFailure[direction]
		];
		
		(* === color stop checks === *) 
		(* list length check *)
		If[Not[And[ListQ[colorStopList], Length[colorStopList] > 1]], Return @ colorStopFailure1[colorStopList]];
		
		(* list format checks *)
		try = 
			Position[
				colorStopList, 
				Except[{Scaled[_?NumericQ] | _?NumericQ | None | Automatic, _?ColorQ} | {Scaled[_?NumericQ] | _?NumericQ, "Hint"}], 
				{1}, 
				Heads -> False];
		If[Length[try] > 0, Return @ colorStopFailure2[First @ Extract[colorStopList, try]]];
		
		(* hint position check *)
		If[colorStopList[[1, 2]] === "Hint", Return @ colorStopFailure3[colorStopList, 1]];
		If[colorStopList[[-1, 2]] === "Hint", Return @ colorStopFailure3[colorStopList, Length[colorStopList]]];
		Do[
			If[colorStopList[[i, 2]] === "Hint", If[hintAllowed, hintAllowed = False, Return @ colorStopFailure3[colorStopList, i]]];
			If[ColorQ[colorStopList[[i, 2]]], hintAllowed = True];
			,
			{i, 2, Length[colorStopList] - 1, 1}
		];
				
		(* === image size checks === *)
		is = 
			Switch[imageSize,
				Automatic,                {150, 150},
				{_?NumericQ, _?NumericQ}, imageSize,
				_?NumericQ,               {imageSize, imageSize},
				_,                        Return @ imageSizeFailure[imageSize]
			];
		
		(* === all argument checks passed tests === *)
		(* 
			Repeating linear gradients must first check for accurate gradient lengths, which could be zero. 
			Implementation seems to be inconsistent across browsers, so we follow the degenerate radial case. *)
		blend = makeBlendFromColorStops[direction, colorStopList, is];
		If[blend[[1, 1, -1, 1]] - blend[[1, 1, 1, 1]] == 0, 
			If[TrueQ @ OptionValue["Repeating"],
				colors = DeleteCases[colorStopList, {_, "Hint"}][[All, 2]];
				Return @ ConstantImage[Blend[Join[{First[colors]}, Riffle[colors[[2 ;; -2]], colors[[2 ;; -2]]], {Last[colors]}]], is]
				,
				(* correct corner case:
					If the distance of the WL Blend function is 0, then the blend misses the coloring from zero to the first color stop.
					This differs from the CSS spec which states a discontinuous transition should take place.
					To modify the WL behavior, we repeat the first color, prepended with a color stop position further to the left. *)
				blend = With[{newBlend = Prepend[blend[[1, 1]], {blend[[1, 1, 1, 1]] - 1, blend[[1, 1, 1, 2]]}]}, Blend[newBlend, #]&];
			]
		];
		
		(*
		For repeating gradients, all calculations are the same except the color stop list is repeated.
		Quoting from the CSS Images 3 spec:
			When rendered the color-stops are repeated infinitely in both directions, with their positions 
			shifted by multiples of the difference between the last specified color-stop's position and the 
			first specified color-stop's position. For example, 
			  
			  repeating-linear-gradient(red 10px, blue 50px) 
			
			is equivalent to
			
			  linear-gradient(..., red -30px, blue 10px, red 10px, blue 50px, red 50px, blue 90px, ...). 
			
			Note that the last color-stop and first color-stop will always coincide at the boundaries of each 
			group, which will produce sharp transitions if the gradient does not start and end with the same 
			color.
		*)
		If[TrueQ @ OptionValue["Repeating"],
			(* REPEATING *)
			convertedDirection = 
				Switch[direction,
					_?NumericQ,       convertAngleToPositions[direction, is],
					None | Automatic, {{is[[1]]/2,   is[[2]]}, {is[[1]]/2,         0}}, (* same as {Top, Bottom} *)
					
					(* side-to-side *)
					{Left, Right},    {{        0, is[[2]]/2}, {  is[[1]], is[[2]]/2}},
					{Right, Left},    {{  is[[1]], is[[2]]/2}, {        0, is[[2]]/2}},
					{Top, Bottom},    {{is[[1]]/2,   is[[2]]}, {is[[1]]/2,         0}},
					{Bottom, Top},    {{is[[1]]/2,         0}, {is[[1]]/2,   is[[2]]}},
					
					{{Left, Top}, {Right, Bottom}}, convertAngleToPositions[Degree*(ArcTan[is[[1]]/is[[2]]]*180/Pi +  90), is],
					{{Right, Bottom}, {Left, Top}}, convertAngleToPositions[Degree*(ArcTan[is[[1]]/is[[2]]]*180/Pi + 270), is],
					{{Left, Bottom}, {Right, Top}}, convertAngleToPositions[Degree*(ArcTan[is[[2]]/is[[1]]]*180/Pi      ), is],
					{{Right, Top}, {Left, Bottom}}, convertAngleToPositions[Degree*(ArcTan[is[[2]]/is[[1]]]*180/Pi + 180), is],
					
					(* fallthrough assumes position-to-position *)
					_?ListQ,          direction
				];
						
			blend = With[{b = duplicateCSLBlocks[blend, getGradientLineLength[direction, is], is]}, Blend[b, #]&];
			distances = blend[[1, 1]][[All, 1]]; (* these are in absolute units *)
			colors = blend[[1, 1]][[All, 2]];
			
			distances = Rescale[distances, {0, EuclideanDistance @@ convertedDirection}];
			blend = With[{b = Thread[{distances, colors}]}, Blend[b, #]&];
			
			LinearGradientImage[N @ convertedDirection, is, ColorFunction -> blend]			
			,
			(* NOT REPEATING *)
			(* 
				CSS has a special case when the gradient points to a corner. In this case, no matter the size
				of the gradient image, the opposite corners have a constant color line between them. We mimic
				this using the DataRange option in the case of a non-repeating gradient. *)
			Switch[direction,
				None | Automatic,                                     LinearGradientImage[{Top, Bottom} -> blend, is],
				_?NumericQ,                                           LinearGradientImage[convertAngleToPositions[direction, is] -> blend, is],
				{{Left|Right, Top|Bottom}, {Left|Right, Top|Bottom}}, LinearGradientImage[direction -> blend, is, DataRange -> {{0, 1}, {0, 1}}],
				_?ListQ,                                              LinearGradientImage[direction -> blend, is]
			]
		]
	]


(* ::Section:: *)
(*Radial Gradient*)


SetAttributes[{consumeRadialGradientFunction, consumeRadialGradientEndingShape, consumeRadialGradientSize}, HoldFirst];


(* ::Subsection::Closed:: *)
(*Token Consumers*)


consumeRadialGradientEndingShape[pos_, l_, tokens:{__?CSSTokenQ}] :=
	Module[{try},
		try =
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"circle",  "Circle",
						"ellipse", "Ellipse",
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
					"closest-corner",  "ClosestCorner",
					"closest-side",    "ClosestSide",
					"farthest-corner", "FarthestCorner",
					"farthest-side",   "FarthestSide",
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
				If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]]; 
				try = consumeLengthPercentage[pos, l, tokens];
				If[FailureQ[try], size,	Append[size, try]]
		]
	]


consumeRadialGradientFunction[pos_, length_, tokens:{___?CSSTokenQ}] :=
	Module[{try, csl, l = length, gradientSize = Automatic, gradientPos = Automatic, gradientEndingShape = Automatic, needsColorStopComma = False},
		(* pre-checks *)
		If[l == 0 || l == 1 && TokenTypeIs["whitespace", tokens[[1]]], 
			Throw[notEnoughTokensFailure[{"<color-stop>", "<comma>", "<color-stop>"}], CSSImages3]];
		TrimWhitespaceTokens[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "A"]];
		
		(* get optional <ending-shape> and/or <size> syntax *)
		Which[
			(* we check for the circle/ellipse ending shape first as its presence is the easiest to rule out *)
			!FailureQ[try = consumeRadialGradientEndingShape[pos, l, tokens]],
				gradientEndingShape = try;
				needsColorStopComma = True;
				If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]];
				try = consumeRadialGradientSize[pos, l, tokens];
				If[!FailureQ[try], gradientSize = try],
				
			!FailureQ[try = consumeRadialGradientSize[pos, l, tokens]],
				gradientSize = try;
				needsColorStopComma = True;
				If[pos > l, Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]];
				
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "AA"]];
				
				try = consumeRadialGradientEndingShape[pos, l, tokens];
				
				If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "AB"]];
				
				If[!FailureQ[try], gradientEndingShape = try],
				
			True, 
				Null (* assume <ending-shape> and/or <size> is not present *)	
		];
		
		(* when consuming the gradientSize, the parsed result is Automatic or a list; either 1 value or 2 values, and the 2 values can be Scaled *)
		(* negative size values are not allowed; if no size is given then default to "FarthestCorner" *)
		If[ListQ[gradientSize] && AnyTrue[gradientSize, If[MatchQ[#, Scaled[_]], Negative[First[#]], Negative[#]]&], Throw[negativeSizeFailure[], CSSImages3]];
		If[gradientSize === Automatic, gradientSize = "FarthestCorner"];
		
		(* set ending shape if not specified, check shape for errors *)
		If[gradientEndingShape === Automatic,
			gradientEndingShape = If[ListQ[gradientSize] && Length[gradientSize] == 1, "Circle", "Ellipse"]
		];
		If[gradientEndingShape === "Circle" && ListQ[gradientSize],
			Which[
				MatchQ[gradientSize, {_Scaled}], pos--; Throw[relativeSizeFailure[], CSSImages3],
				MatchQ[gradientSize, {_, _}],    pos--; Throw[mismatchedSizeFailure[], CSSImages3],
				True,                            Null (* throw no errors *)
			]
		];
		If[gradientEndingShape === "Ellipse" && ListQ[gradientSize] && !MatchQ[gradientSize, {_, _}], pos--; Throw[mismatchedSizeFailure[], CSSImages3]];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "B"]];
		
		(* get optional 'at <position>' syntax *)
		If[(gradientEndingShape =!= Automatic || gradientSize =!= Automatic) && pos > l, 
			Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "BB"]];
				
		If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["at", tokens[[pos]]],
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* a position _must_ be consumed following the 'at' ident token *)
			If[pos > l, Throw[notEnoughTokensFailure[{"<position>", "<comma>", "<color-stop-list>"}], CSSImages3]]; 
			
			If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "BC"]];
			
			try = consumePosition[pos, l, tokens];
			If[FailureQ[try], Throw[missingPositionFailure[], CSSImages3], gradientPos = try];
			
			needsColorStopComma = True;
		];
		
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "C"]];
		
		(* must have a comma before the color-stop-list if any of the optional specs are present *)
		If[needsColorStopComma && pos > l, 
			Throw[notEnoughTokensFailure[{"<comma>", "<color-stop-list>"}], CSSImages3]
		];
		If[needsColorStopComma, 
			If[TokenTypeIs["comma", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens]
				,
				Throw[missingCommaFailure[], CSSImages3]
			]
		];
				
		If[TrueQ[$Debug], Echo[{pos, l, If[pos > l, "EOF", tokens[[pos]]], gradientPos, gradientSize, gradientEndingShape}, "D"]];
	
		(* get color-stop-list *)
		If[pos > l, Throw[notEnoughTokensFailure["<color-stop-list>"], CSSImages3]];
		csl = consumeColorStopList[pos, l, tokens];
		
		(* this is a quick check for failure *)
		try = makeBlendFromColorStops[{{0,0}, {1,0}}, csl, {150, 150}];
		
		{gradientPos, gradientEndingShape, gradientSize, csl}
	]


(* ::Subsection::Closed:: *)
(*Options*)


Options[CSSRadialGradientImage] = {"Repeating" -> False, "Extent" -> Automatic, "Shape" -> Automatic};
SyntaxInformation[CSSRadialGradientImage] = {"ArgumentsPattern" -> {_., _., _., OptionsPattern[]}};


(* ::Subsection::Closed:: *)
(*Failure Messages*)


radialPositionFailure[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The position argument must be a side position, offset from a side position, or a coordinate.", 
		"Input" -> arg,
		"Examples" -> Alternatives @@ {{Left, Top}, {20, Scaled[0.1]}, {{Left, 10}, {Bottom, Scaled[0.2]}}},
		"MessageParameters" -> <||>|>] 
		
radialShapeFailure[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The shape argument must be Automatic, \"Circle\" or \"Ellipse\".", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 

radialSizeFailure1[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "An extent argument keyword must be a either \"FarthestCorner\", \"ClosestCorner\", \"FarthestSide\", or \"ClosestSide\".", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 
		
radialSizeFailure2[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "A pair of numeric sizes are expected for elliptical gradients.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 

radialSizeFailure3[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "A circular gradient must use an absolute numerical extent.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 

wrongSizeFailure[arg_] :=
	Failure["CSSGradientFailure", <|
		"MessageTemplate"   -> "The extent argument must be either Automatic, a single size, or a pair of sizes.", 
		"Input" -> arg,
		"MessageParameters" -> <||>|>] 


(* ::Subsection::Closed:: *)
(*Parse from CSS*)


doParseRadialGradientFunction[prop_String, token_?CSSTokenQ, isRepeating:(True | False)] :=
	Module[{try, pos = 1, l, tokens, functionHead, function},
		functionHead = If[isRepeating, "repeating-", ""] <> "radial-gradient";
		If[Not[TokenTypeIs["function", token] && TokenStringIs[functionHead, token]],
			Return @ expectedFunctionFailure[functionHead]
		];
		
		(* consume the function's token sequence *)
		tokens = token["Children"]; l = Length[tokens];
		try = Catch[consumeRadialGradientFunction[pos, l, tokens], CSSImages3];
		If[FailureQ[try], 
			Return @ 
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
		];
		
		(* 
			CSS has the origin in the upper left corner with an inverted y-axis. 
			The starting position may need to be corrected *)
		function =
			Which[
				MatchQ[try[[1]], {_, Scaled[_]}], (* relative y *)
					With[{startPosX = try[[1, 1]], startPosY = Scaled[1 - First[try[[1, 2]]]], shape = try[[2]], size = try[[3]], colorStopList = try[[4]]},
						CSSRadialGradientImage[colorStopList, {startPosX, startPosY}, #, "Shape" -> shape, "Extent" -> size]&]
				,
				MatchQ[try[[1]], {_, _?NumericQ}], (* absolute y *)
					With[{startPosX = try[[1, 1]], startPosY = try[[1, 2]], shape = try[[2]], size = try[[3]], colorStopList = try[[4]]},
						CSSRadialGradientImage[colorStopList, {startPosX, #[[2]] - startPosY}, #, "Shape" -> shape, "Extent" -> size]&]
				,
				True, 
					With[{startPos = try[[1]], shape = try[[2]], size = try[[3]], colorStopList = try[[4]]},
						CSSRadialGradientImage[colorStopList, startPos, #, "Shape" -> shape, "Extent" -> size]&]
			];
		If[isRepeating, Insert[function, "Repeating" -> True, {1, -1}], function]
	]


parseRadialGradientFunction[prop_String, token_?CSSTokenQ] := doParseRadialGradientFunction[prop, token, False]
	
parseRepeatingRadialGradientFunction[prop_String, token_?CSSTokenQ] := doParseRadialGradientFunction[prop, token, True]


(* ::Subsection::Closed:: *)
(*CSSRadialGradientImage*)


(* 0-arg *)
CSSRadialGradientImage[opts:OptionsPattern[]] := 
	doCSSRadialGradientImage[
			{Center, Center}, 
			OptionValue[CSSRadialGradientImage, "Shape"], (* any bad options will cause messages here, but are suppressed in other arguments *)
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Extent"], 
			{{Scaled[0], Black}, {Scaled[1], White}}, 
			{150, 150}, 
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Repeating"]]

(* 1-arg *)
CSSRadialGradientImage[colorStopList_, opts:OptionsPattern[]] := 
	doCSSRadialGradientImage[
			{Center, Center}, 
			OptionValue[CSSRadialGradientImage, "Shape"], (* any bad options will cause messages here, but are suppressed in other arguments *)
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Extent"], 
			colorStopList, 
			{150, 150}, 
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Repeating"]]

(* 2-arg *)
CSSRadialGradientImage[colorStopList_, startPosition_, opts:OptionsPattern[]] := 
	doCSSRadialGradientImage[
			startPosition, 
			OptionValue[CSSRadialGradientImage, "Shape"], (* any bad options will cause messages here, but are suppressed in other arguments *)
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Extent"], 
			colorStopList, 
			{150, 150}, 
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Repeating"]]

(* 3-arg *)
CSSRadialGradientImage[colorStopList_, startPosition_, imageSize_, opts:OptionsPattern[]] := 
	doCSSRadialGradientImage[
			startPosition, 
			OptionValue[CSSRadialGradientImage, "Shape"], (* any bad options will cause messages here, but are suppressed in other arguments *)
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Extent"], 
			colorStopList, 
			imageSize, 
			OptionValue[CSSRadialGradientImage, FilterRules[{opts}, Options[CSSRadialGradientImage]], "Repeating"]]


(* 
	The radial gradient consists of a ray from starting some point {x0, y0} with concentric circles or ellipses out
	to the extent specified e.g. "FarthestCorner" or a numeric distance e.g. {rx, ry}.
	
	The color stops that define the gradient can extend less or more than the length of the gradient ray.
	
	For repeating gradients, all calculations are the same except the color stop list is repeated:
	"When rendered the color-stops are repeated infinitely in both directions, with their positions 
	"shifted by multiples of the difference between the last specified color-stop's position and the 
	"first specified color-stop's position. For example, 
	"  
	"  repeating-linear-gradient(red 10px, blue 50px) 
	"
	"is equivalent to
	"
	"  linear-gradient(..., red -30px, blue 10px, red 10px, blue 50px, red 50px, blue 90px, ...). 
	"
	"Note that the last color-stop and first color-stop will always coincide at the boundaries of each 
	"group, which will produce sharp transitions if the gradient does not start and end with the same 
	"color.
	
	Also, because we use DataRange to create the elliptical shape, any absolute distance color stop 
	must be scaled as well.
*)

doCSSRadialGradientImage[startPosition_, inputShape_, inputSize_, colorStopList_, imageSize_, isRepeating_] := 
	Module[{gradientPos, try, hintAllowed = True, is, shape, size, possibleEnds, convertedPosition, csl, dataRange, distances, colors},
		possibleEnds = "FarthestCorner" | "ClosestCorner" | "FarthestSide" | "ClosestSide";
		(* === starting position checks === *)
		gradientPos = If[startPosition === Automatic, {Center, Center}, startPosition];
		If[
			!MatchQ[
				gradientPos, 
				Alternatives[
					{{Left | Right, Scaled[_?NumericQ] | _?NumericQ}, {Top | Bottom, Scaled[_?NumericQ] | _?NumericQ}},
					Scaled[{_?NumericQ, _?NumericQ}],
					{Scaled[_?NumericQ] | _?NumericQ | Left | Center | Right, Scaled[_?NumericQ] | _?NumericQ | Top | Center | Bottom}]],
			Return @ radialPositionFailure[gradientPos]
		];
		If[MatchQ[gradientPos, Scaled[{_?NumericQ, _?NumericQ}]], gradientPos = Thread[gradientPos]];
		
		(* === shape and size check === *)
		Which[
			inputSize === Automatic, (* default shape determines size *) 
				size = "FarthestCorner"; 
				shape = 
					Switch[inputShape,
						Automatic | "Ellipse", "Ellipse",
						"Circle",              "Circle",
						_,                     Return @ radialShapeFailure[inputShape]
					]
			,
			inputShape === Automatic, (* size determines shape *)
				Switch[inputSize,
					_?NumericQ,         
						If[Negative[inputSize], Return @ negativeSizeFailure[]];
						shape = "Circle"; size = inputSize;
					,
					Scaled[_?NumericQ], 
						Return @ radialSizeFailure3[inputSize]
					,
					{_?NumericQ | Scaled[_?NumericQ], _?NumericQ | Scaled[_?NumericQ]} | Scaled[{_?NumericQ, _?NumericQ}],
						If[MatchQ[inputSize, Scaled[{_?NumericQ, _?NumericQ}]], size = Thread[inputSize], size = inputSize];
						If[AnyTrue[size, If[MatchQ[#, Scaled[_]], Negative[First[#]], Negative[#]]&], Return @ negativeSizeFailure[]];
						shape = "Ellipse";						
					,
					_?StringQ,
						If[!MatchQ[inputSize, possibleEnds], Return @ radialSizeFailure1[inputSize]];
						shape = "Ellipse"; size = inputSize;
					,
					True,                        
						wrongSizeFailure[inputSize]
				]
			,
			True, (* shape must match size *)
				Switch[inputShape,
					"Circle", 
						Switch[inputSize, 
							_?NumericQ,         If[Negative[inputSize], Return @ negativeSizeFailure[]],
							Scaled[_?NumericQ], Return @ radialSizeFailure3[inputSize],
							_?StringQ,          If[!MatchQ[inputSize, possibleEnds], Return @ radialSizeFailure1[inputSize]],
							_,                  Return @ mismatchedSizeFailure["BadSize"]				
						]
					,
					"Ellipse",
						Switch[inputSize,
							{_?NumericQ | Scaled[_?NumericQ], _?NumericQ | Scaled[_?NumericQ]},
								If[AnyTrue[inputSize, If[MatchQ[#, Scaled[_]], Negative[First[#]], Negative[#]]&], Return @ negativeSizeFailure[]],
							_?StringQ, If[!MatchQ[inputSize, possibleEnds], Return @ radialSizeFailure1[inputSize]],
							_,         Return @ mismatchedSizeFailure["BadSize"]
						]
					,
					_, 
						Return @ radialShapeFailure[inputShape]
				];
				shape = inputShape; size = inputSize;
		];
		
		(* === color stop checks === *) 
		(* list length check *)
		If[Not[And[ListQ[colorStopList], Length[colorStopList] > 1]], Return @ colorStopFailure1[colorStopList]];
		
		(* list format checks *)
		try = 
			Position[
				colorStopList, 
				Except[{Scaled[_?NumericQ] | _?NumericQ | None | Automatic, _?ColorQ} | {Scaled[_?NumericQ] | _?NumericQ, "Hint"}], 
				{1}, 
				Heads -> False];
		If[Length[try] > 0, Return @ colorStopFailure2[First @ Extract[colorStopList, try]]];
		
		(* hint position check *)
		If[colorStopList[[ 1, 2]] === "Hint", Return @ colorStopFailure3[colorStopList, 1]];
		If[colorStopList[[-1, 2]] === "Hint", Return @ colorStopFailure3[colorStopList, Length[colorStopList]]];
		Do[
			If[colorStopList[[i, 2]] === "Hint", If[hintAllowed, hintAllowed = False, Return @ colorStopFailure3[colorStopList, i]]];
			If[ColorQ[colorStopList[[i, 2]]], hintAllowed = True];
			,
			{i, 2, Length[colorStopList] - 1, 1}
		];
		
		(* === image size checks === *)
		is = 
			Switch[imageSize,
				Automatic,                {150, 150},
				{_?NumericQ, _?NumericQ}, imageSize,
				_?NumericQ,               {imageSize, imageSize},
				_,                        Return @ imageSizeFailure[imageSize]
			];
			
		(* === all checks passed so make gradient ===*)
		convertedPosition = CSSGradientConvertRadialPosition[gradientPos, shape, size, is];
		
		(* check for degenerate radial lengths; see section 3.2.3 of specification *)
		(* 
			In all cases (circle with 0 radius, ellipse with either zero width or zero height), the spec indicates
			we should keep the gradient effectively correct in case zoom is applied. But zoom is only applicable in 
			browsers, not the WL. 
			In WL the image is fixed, so this will end up looking like a solid color (the last color stop). 
			A repeating gradient will be a blend of all colors in the color stop list (with middle colors repeated). *)
		If[EuclideanDistance @@ convertedPosition == 0, 
			Return @ 
				If[TrueQ @ isRepeating,
					csl = DeleteCases[colorStopList, {_, "Hint"}][[All, 2]];
					ConstantImage[Blend @ Join[{First[csl]}, Riffle[csl[[2 ;; -2]], csl[[2 ;; -2]]], {Last[csl]}], is]
					,
					ConstantImage[colorStopList[[-1, 2]], is]
				]
		];
		
		If[shape === "Ellipse", 
			dataRange = CSSGradientConvertRadialDataRange[gradientPos, size, is];
			
			(* due to the DataRange changes, absolute distances along the color stop list must also be scaled *)
			csl = colorStopList;
			With[{scaling = (is[[1]] + 2*(dataRange[[1, 2]] - is[[1]]))/is[[1]]}, 
				csl = MapAt[If[NumericQ[#], #*scaling, #]&, csl, {All, 1}]];
			csl = makeBlendFromColorStops[convertedPosition, csl, is];
			,
			csl = makeBlendFromColorStops[convertedPosition, colorStopList, is];
		];
		
		If[TrueQ @ isRepeating,
			(* REPEATING *)
			(* convert the color stops to absolute units, then duplicate the color stop sequence as much as necessary *)
			csl = duplicateCSLBlocks[csl, getGradientLineLength[convertedPosition, is], is];
			
			(* set the gradient position to run from the original start to the end given by the updated color stop sequence *)
			convertedPosition = {convertedPosition[[1]], convertedPosition[[1]] + {csl[[-1, 1]], 0}};
			
			(* recalculate the Blend function given the updated color stop list and converted position *)
			csl = makeBlendFromColorStops[convertedPosition, csl, is];
			
			RadialGradientImage[convertedPosition, is, ColorFunction -> csl, If[shape === "Ellipse", DataRange -> dataRange, Unevaluated[Sequence[]]]]
			,
			
			(* NOT REPEATING *)
			(* check that the first and last stops are all the way at the end; if not, duplicate the first and last color *)
			csl = capEndsOfShortCSL[csl];
			
			(* 
				1. rescale the gradient ray to the length based on the calculated color stop positions. 
				2. rescale the color stop list to run from 0 to 1 *)
			convertedPosition = N @ {First[convertedPosition], First[convertedPosition] + {1, 0}*csl[[1, 1, -1, 1]]*getGradientLineLength[convertedPosition, is]};
			distances = csl[[1, 1]][[All, 1]];
			colors = csl[[1, 1]][[All, 2]];
			csl = With[{b = Thread[{Rescale[distances, {First[distances], Last[distances]}], colors}]}, Blend[b, #]&];
			
			RadialGradientImage[convertedPosition, is, ColorFunction -> csl, If[shape === "Ellipse", DataRange -> dataRange, Unevaluated[Sequence[]]]]
		]
	]


(* ::Subsection::Closed:: *)
(*Conversion from CSS to WD *GradientImage*)


(* ::Subsubsection::Closed:: *)
(*Utilities*)


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


(* ::Subsubsection::Closed:: *)
(*Circular Gradients*)


(* 
	The formulas involved assume that the origin is the center of the Image. 
	Use of absoulte values is a convenience to minimize the number of cases. *)

CSSGradientConvertRadialPosition[position_, "Circle", r_, imageSize:{w_, h_}] :=
	Module[{x1, y1},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{{x1, y1}, {x1, y1} + {r, 0}}
	]

CSSGradientConvertRadialPosition[position_, "Circle", "ClosestSide", imageSize:{w_, h_}] :=
	Module[{x0, y0, x1, y1, distanceToLeftRight, distanceToTopBottom},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 - Abs[x0]; distanceToTopBottom = h/2 - Abs[y0]; 
		If[distanceToLeftRight < distanceToTopBottom,
			{{x1, y1}, {x1, y1} + {distanceToLeftRight, 0}}
			,
			{{x1, y1}, {x1, y1} + {0, distanceToTopBottom}}
		]
	]
	
CSSGradientConvertRadialPosition[position_, "Circle", "FarthestSide", imageSize:{w_, h_}] :=
	Module[{x0, y0, x1, y1, distanceToLeftRight, distanceToTopBottom},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 + Abs[x0]; distanceToTopBottom = h/2 + Abs[y0]; 
		If[distanceToLeftRight < distanceToTopBottom,
			{{x1, y1}, {x1, y1} + {0, distanceToTopBottom}}
			,
			{{x1, y1}, {x1, y1} + {distanceToLeftRight, 0}}
		]
	]
	
CSSGradientConvertRadialPosition[position_, "Circle", "ClosestCorner", imageSize:{w_, h_}] :=
	Module[{x1, y1},
		{x1, y1} = convertPositionSpec[position, imageSize];
		Which[
			x1 >= w/2 && y1 >= h/2, {{x1, y1}, {w, h}},
			x1 >= w/2 && y1 <  h/2, {{x1, y1}, {w, 0}},
			x1 <  w/2 && y1 >= h/2, {{x1, y1}, {0, h}},
			x1 <  w/2 && y1 <  h/2, {{x1, y1}, {0, 0}}
		]
	]
	
CSSGradientConvertRadialPosition[position_, "Circle", "FarthestCorner", imageSize:{w_, h_}] :=
	Module[{x1, y1},
		{x1, y1} = convertPositionSpec[position, imageSize];
		Which[
			x1 >= w/2 && y1 >= h/2, {{x1, y1}, {0, 0}},
			x1 >= w/2 && y1 <  h/2, {{x1, y1}, {0, h}},
			x1 <  w/2 && y1 >= h/2, {{x1, y1}, {w, 0}},
			x1 <  w/2 && y1 <  h/2, {{x1, y1}, {w, h}}
		]
	]


(* ::Subsubsection::Closed:: *)
(*Elliptical Gradients*)


(* 
	Elliptical gradients are always on axis, never rotated. WL supports circular gradients as the default.
	To implement an elliptical gradient, then you must use the DataRange option to change the effective viewport.
	Because the ImageSize remains fixed, if the DataRange (i.e. PlotRange) shrinks, then the circle's radius is 
	stretched outward; if the PlotRange grows then the radius shrinks inward. This is a non-linear inverse relationship:
		
		To  move the radius R by a distance x, adjust the PlotRange by X: 
		X=(-size/2)*x/(x+R)
	
	This assumes that the position of the circle is in the center of the image so no displacement occurs, only dilation.
	'size' is either the horizontal 'w' (width) or vertical 'h' (height) of the ImageSize parameter.
	
	We must also position the circle. A consequence of first dilating the circle is that the x and/or y coordinates
	become dilated as well. So to move a distance X along a dilated coordinate is a simple scaling based on the amount
	of dilation. The scale factor is (size + 2*x)/size where x is the adjustment to the PlotRange (x could be negative).
	
	The closest-corner and farthest-corner cases are more involved. There are two constraints:
		1. The aspect ratio of the ellipse must be the same as in the "closest-..." cases
		2. The ellipse must pass through a corner of the box
		
		Fortunately the aspect ratio is easy to calculate when relative to the center of the box: 
		A = (h/2 +/- Abs[y0])/(w/2 +/- Abs[x0])
		
	where {x0, y0} is the circle position relative to the center of the box. 'h' and 'w' are the ImageSize (box size).
	The + sign is used for "farthest" and the - sign is used for "nearest". 
	
	Using the equation of an ellipse (x-x0)^2/rx^2 + (y-y0)^2/ry^2 == 1 and the aspect ratio, we solve for
	the two radii of the ellipse:
	
		rx = Sqrt[2]*(w/2 +/- Abs[x0])	ry = Sqrt[2]*(h/2 +/- Abs[y0])
		
	The + sign is used for "farthest" and the - sign is used for "nearest". 
		
		
	*)

CSSGradientConvertRadialPosition[position_, "Ellipse", size:{{_, _}, {_, _}} | {_, _}, imageSize:{w_, h_}] :=
	Module[{x0, y0, r, d, dilateVertically, pt},
		{d, r, {x0, y0}, dilateVertically} = radialGradientEllipseData[position, size, imageSize];
		pt = {w, h}/2;
		pt += If[dilateVertically, {x0, (h+2*d)/h*y0}, {(w+2*d)/w*x0, y0}];
		N @ {pt, pt + {r, 0}} (* bug in RadialGradientImage requires position to be numeric *)
	]

CSSGradientConvertRadialDataRange[position_, size:{{_, _}, {_, _}} | {_, _}, imageSize:{w_, h_}] :=
	Module[{plotRange = {{0, w}, {0, h}}, dilateVertically, d, r, startPos},
		{d, r, startPos, dilateVertically} = radialGradientEllipseData[position, size, imageSize];
		If[dilateVertically, 
			plotRange[[2, 1]] -= d; plotRange[[2, 2]] += d
			, 
			plotRange[[1, 1]] -= d; plotRange[[1, 2]] += d
		];
		N @ plotRange
	]
	
radialGradientEllipseData[position_, size:{{_, _}, {_, _}} | {_, _}, imageSize:{w_, h_}] :=
	Module[{x0, y0, x1, y1, rx, ry, r},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		{rx, ry} = convertPositionSpec[size, imageSize];
		
		If[rx > ry, 
			r = ry; (* ry is the limiting distance *)
			{r - rx, r, {x0, y0}, False}
			,
			r = rx; (* rx is the limiting distance *)
			{r - ry, r, {x0, y0}, True}
		]
	]
	
CSSGradientConvertRadialPosition[position_, "Ellipse", size:"ClosestSide" | "FarthestSide", imageSize:{w_, h_}] :=
	Module[{dilateVertically, d, r, pt, x0, y0},
		{d, r, {x0, y0}, dilateVertically} = radialGradientEllipseData[position, size, imageSize];
		pt = {w, h}/2;
		pt += If[dilateVertically, {x0, (h+2*d)/h*y0}, {(w+2*d)/w*x0, y0}];
		N @ {pt, pt + {r, 0}} (* bug in RadialGradientImage requires position to be numeric *)
	]
	
CSSGradientConvertRadialDataRange[position_, size:"ClosestSide" | "FarthestSide", imageSize:{w_, h_}] :=
	Module[{plotRange = {{0, w}, {0, h}}, dilateVertically, d, r, startPos},
		{d, r, startPos, dilateVertically} = radialGradientEllipseData[position, size, imageSize];
		If[dilateVertically, 
			plotRange[[2, 1]] -= d; plotRange[[2, 2]] += d
			, 
			plotRange[[1, 1]] -= d; plotRange[[1, 2]] += d
		];
		N @ plotRange
	]
	
radialGradientEllipseData[position_, "ClosestSide", imageSize:{w_, h_}] :=
	Module[{x0,y0,x1, y1, distanceToLeftRight, distanceToTopBottom, r, dx, dy, x, y},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 - Abs[x0]; distanceToTopBottom = h/2 - Abs[y0]; 
		If[distanceToLeftRight < distanceToTopBottom,
			(* need to dilate outwards in vertical direction *) 
			r = distanceToLeftRight; dy = distanceToTopBottom - r; y = -h/2*dy/(dy+r);
			{y, r, {x0, y0}, True}
			,
			(* need to dilate outwards in horizontal direction *) 
			r = distanceToTopBottom; dx = distanceToLeftRight - r; x = -w/2*dx/(dx+r);
			{x, r, {x0, y0}, False}
		]
	]

radialGradientEllipseData[position_, "FarthestSide", imageSize:{w_, h_}] :=
	Module[{x0,y0,x1, y1, distanceToLeftRight, distanceToTopBottom, r, dx, dy, x, y},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 + Abs[x0]; distanceToTopBottom = h/2 + Abs[y0]; 
		If[distanceToLeftRight < distanceToTopBottom,
			(* need to dilate inwards in horizontal direction *) 
			r = distanceToTopBottom; dx = distanceToLeftRight - r; x = -w/2*dx/(dx+r);
			{x, r, {x0, y0}, False}
			,
			(* need to dilate inwards in vertical direction *) 
			r = distanceToLeftRight; dy = distanceToTopBottom - r; y = -h/2*dy/(dy+r);
			{y, r, {x0, y0}, True}
		]
	]

CSSGradientConvertRadialPosition[position_, "Ellipse", size:"ClosestCorner" | "FarthestCorner", imageSize:{w_, h_}] :=
	Module[{dx, dy, r, pt, x0, y0},
		{{dx, dy}, r, {x0, y0}} = radialGradientEllipseData[position, size, imageSize];
		pt = {w, h}/2;
		pt += {(w+2*dx)/w*x0, (h+2*dy)/h*y0};
		N @ {pt, pt + {r, 0}} (* bug in RadialGradientImage requires position to be numeric *)
	]
	
CSSGradientConvertRadialDataRange[position_, size:"ClosestCorner" | "FarthestCorner", imageSize:{w_, h_}] :=
	Module[{plotRange = {{0, w}, {0, h}}, dx, dy, r, startPos},
		{{dx, dy}, r, startPos} = radialGradientEllipseData[position, size, imageSize];
		plotRange[[2, 1]] -= dy; plotRange[[2, 2]] += dy; plotRange[[1, 1]] -= dx; plotRange[[1, 2]] += dx;
		N @ plotRange
	]

radialGradientEllipseData[position_, "ClosestCorner", imageSize:{w_, h_}] :=
	Module[{x0, y0, x1, y1, distanceToLeftRight, distanceToTopBottom, r, dx, dy, x, y},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 - Abs[x0]; distanceToTopBottom = h/2 - Abs[y0]; 
		r = If[distanceToLeftRight < distanceToTopBottom, distanceToLeftRight, distanceToTopBottom];
		(* need to dilate outwards in both directions *) 
		dx = Sqrt[2]*distanceToLeftRight - r; x = -w/2*dx/(dx+r);
		dy = Sqrt[2]*distanceToTopBottom - r; y = -h/2*dy/(dy+r);
		{{x, y}, r, {x0, y0}}
	]
	
radialGradientEllipseData[position_, "FarthestCorner", imageSize:{w_, h_}] :=
	Module[{x0, y0, x1, y1, distanceToLeftRight, distanceToTopBottom, r, dx, dy, x, y},
		{x1, y1} = convertPositionSpec[position, imageSize];
		{x0, y0} = {x1, y1} - {w/2, h/2}; (* make start position relative to center of image *)
		
		distanceToLeftRight = w/2 + Abs[x0]; distanceToTopBottom = h/2 + Abs[y0]; 
		r = If[distanceToLeftRight < distanceToTopBottom, distanceToLeftRight, distanceToTopBottom];
		(* need to dilate outwards in both directions *) 
		dx = Sqrt[2]*distanceToLeftRight - r; x = -w/2*dx/(dx+r);
		dy = Sqrt[2]*distanceToTopBottom - r; y = -h/2*dy/(dy+r);
		{{x, y}, r, {x0, y0}}
	]	


(* ::Section::Closed:: *)
(*New CSS Property Data*)


(* ::Subsection::Closed:: *)
(*PropertyData*)


If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, {
	"object-fit" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "fill",
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True,
		"Values" -> {"fill", "contain", "cover", "none", "scale-down"},
		"AppliesTo" -> {"replaced elements"}|>,
	"object-position" -> <|
		"Inherited" -> False,
		"CSSInitialValue" -> "50% 50%", (* centered *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True, (* technically <position> is the possible value, but it consists of the following sub-values *)
		"Values" -> {"<position>", "<percentage>", "<length>", "left", "right", "center", "top", "bottom"},
		"AppliesTo" -> {"replaced elements"}|>,
	"image-orientation" -> <| (* deprecated property *)
		"Inherited" -> False,
		"CSSInitialValue" -> "from-image", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True, 
		"Values" -> {"from-image", "none", "<angle>", "flip"},
		"AppliesTo" -> All|>,
	"image-rendering" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "auto", 
		"InterpretedGlobalValues" -> <|
			"inherit" -> <||>, (* no equivalent FE option *)
			"initial" -> Missing["Not supported."]|>,
		"Animatable" -> True, 
		"Values" -> {"auto", "smooth", "high-quality", "crisp-edges", "pixelated"},
		"AppliesTo" -> All|>}]


(* ::Subsection::Closed:: *)
(*object-fit*)


(* object-fit should probably translate to ImageSizeAction, but WL Image expressions when wrapped in Pane ignore its values. *)
consumeProperty[prop:"object-fit", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"fill",       Automatic,
						"contain",    Automatic,
						"cover",      Automatic,
						"none",       Automatic,
						"scale-down", Automatic,
						_,            unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*object-position*)


consumeProperty[prop:"object-position", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		value = consumePosition[pos, l, tokens];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Subsection::Closed:: *)
(*image-orientation (deprecated)*)


(* 
	Deprecated as it only corrects for images that are not correctly orientated and serves no real other purpose.
	Other transformation properties can do much the same. *)
consumeProperty[prop:"image-orientation", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Missing["Not supported."]


(* ::Subsection::Closed:: *)
(*image-rendering*)


(* 
	The FE does its own thing with choosing how to render images at different magnifications. 
	Effectively it uses "pixelated" when the image is drawn larger than its actual size. *)
consumeProperty[prop:"image-rendering", tokens:{__?CSSTokenQ}, opts:OptionsPattern[]] := 
	Module[{pos = 1, l = Length[tokens], value},
		If[l > 1, Return @ tooManyTokensFailure @ prop];
		value = 
			Switch[tokens[[pos]]["Type"],
				"ident", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"auto",         Automatic,
						"smooth",       Automatic,
						"high-quality", Automatic,
						"crisp-edges",  Automatic,
						"pixelated",    Automatic,
						_,              unrecognizedKeyWordFailure @ prop
					],
				_, unrecognizedValueFailure @ prop
			];
		If[FailureQ[value], value, Missing["Not supported."]]
	]


(* ::Section::Closed:: *)
(*Footer*)


End[] (* End Private Context *)

EndPackage[]
