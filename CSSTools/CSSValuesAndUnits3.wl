(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSValuesAndUnits3`", {"CSSTools`"}]
(* Exported symbols added here with SymbolName::usage *)

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSToken, CSSTokenQ, CSSUntokenize *)
(* CSSPropertyInterpreter` 
	---> defines consumeProperty 
	---> provides CSSPropertyData *)

Needs["CSSTools`CSSTokenizer`"];   
Needs["CSSTools`CSSPropertyInterpreter`"];


Begin["`Private`"] (* Begin Private Context *) 


(* ::Subsection::Closed:: *)
(*calc() and attr() functions in CSS properties *)


(* attr() referencing of document attributes
	* an XML element's attribute value is referenced with the attr() function e.g. attr(<attr-name> <type-or-unit>? [, <attr-fallback>]?)
	* in CSS 2.1 this always returned a string, but this module extends the return types 
	* a fallback value is not required. If present, it must be of the same type if attr() is not the only token within the property value.
	+ Most attr() substitution happens at compute time during the cascade/inheritance:
		* some errors can be detected at parse time
		* any attr() in a property value effectively keeps the entire value from parsing
		* at compute time the value can fail to parse after substitution  
	*)
	
parseType[s_String] :=
	Switch[s, 
		"string",  "<string>",
		"color",   "<color>",
		"url",     "<funciri>", 
		"%",       "<percentage>",
		"number",  "<number>",
		"integer", "<integer>",
		"em" | "rem" | "ex" | "ch" | "vw" | "vh" | "vmin" | "vmax" | "in" | "cm" | "pc" | "mm" | "pt" | "px" | "q", "<length>",
		"hz" | "khz",                     "<frequency>",
		"s" | "ms",                       "<time>",
		"deg" | "grad" | "rad" | "turns", "<angle>",
		"dpi" | "dpcm" | "dppx",          "<resolution>",
		_,                                None
	]
	
typeCheck[_, x_Failure] := x	
typeCheck[prop_?StringQ, a_?AssociationQ] := 
	Module[{type},
		type = parseType[a["Type"]];
		If[MemberQ[CSSPropertyData[prop, "Values"], type], 
			a
			, 
			Failure["BadParse", <|
				"MessageTemplate" -> "Property `Prop` is not compatible with the data type.", 
				"Type" -> a["Type"],
				"MessageParameters" -> <|"Type" -> "Parse", "Prop" -> prop|>|>]
		]
	]
	
fallbackTypeCheck[_, x_Failure, _] := x	
fallbackTypeCheck[prop_?StringQ, a_?AssociationQ, length_] := 
	Module[{token, fallback, type, check1, check2},
		token = CSSTokenize[a["Fallback"]];
		If[Length[token] > 1, 
			Return @ 
				Failure["BadParse", <|
					"MessageTemplate" -> "Fallback has too many tokens.", 
					"Tokens" -> token,
					"MessageParameters" -> <|"Type" -> "Parse", "Prop" -> prop|>|>]];
		token = First[token];
		fallback =
			Which[
				TokenTypeIs["dimension", token], 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ token["Unit"],
						"em" | "rem" | "ex" | "ch" | "vw" | "vh" | "vmin" | "vmax" | "in" | "cm" | "pc" | "mm" | "pt" | "px" | "q", "<length>",
						"hz" | "khz",                     "<frequency>",
						"s" | "ms",                       "<time>",
						"deg" | "grad" | "rad" | "turns", "<angle>",
						"dpi" | "dpcm" | "dppx",          "<resolution>",
						_,                                None
					],
				TokenTypeIs["ident", token],  CSSNormalizeEscapes @ ToLowerCase @ token["String"],
				TokenTypeIs["string", token], "<string>",
				TokenTypeIs["url", token],    "<funciri>",
				True,                         None
			];
		(* check: fallback type is valid for that property *)
		check1 =
			Or[
				MemberQ[CSSPropertyData[prop, "Values"], fallback],
				MemberQ[CSSPropertyData[prop, "Values"], "<color>"] && !FailureQ[parseSingleColor[prop, token]],
				MemberQ[CSSPropertyData[prop, "Values"], "<image>"] && With[{i = parseURI @ token["String"]}, Not[FailureQ[i] || MissingQ[i]]]];
		If[!check1,
			Return @ 
				Failure["BadParse", <|
					"MessageTemplate"   -> "Property is not compatible with the fallback provided.",
					"Property"          -> prop,  
					"Fallback"          -> fallback,
					"MessageParameters" -> <|"Type" -> "Parse", "Prop" -> prop|>|>]
		];
		(* check: fallback type is matches stated type if there's more than one token in the property value *)
		type = parseType[a["Type"]];
		check2 = If[length === 1, True, type === fallback];
		If[!check2,
			Return @ 
				Failure["BadParse", <|
					"MessageTemplate"   -> "Declared type is not compatible with the fallback provided.",
					"Type"              -> type,  
					"Fallback"          -> fallback,
					"MessageParameters" -> <|"Type" -> "Parse", "Prop" -> prop|>|>]
		];
		a
	]


(* The DownValue for consuming attr() referencing and calc() in properties needs to come early in order to override existing behavior. *)
DownValues[consumeProperty] = 
	Join[
		{
			(* any use of attr() must be resolved at inheritance (CSSInheritance) unless a parse error is detected *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], !FreeQ[tokens, TokenPatternString["attr", "function"]]]
			] :>
				Module[{checkedEntries, failPosition, declaredNamespaces},
					(* check: parse failures; includes namespace prefix check and check for additional attr() in fallback *)
					(* recall Namespaces format is {<|"Prefix" -> "A", "Namespace" -> "www.A.com", "Default" -> False|>, ...}*)
					declaredNamespaces = OptionValue["Namespaces"];
					checkedEntries = parseAttrFunctionToken[#, declaredNamespaces]& /@ Extract[tokens, Position[tokens, TokenPatternString["attr", "function"]]];
					failPosition = FirstPosition[checkedEntries, _Failure, {}];
					If[failPosition =!= {}, Return @ Extract[checkedEntries, failPosition]];
					
					(* check: type is appropriate for the given property *)
					checkedEntries = typeCheck[prop, #]& /@ checkedEntries;
					failPosition = FirstPosition[checkedEntries, _Failure, {}];
					If[failPosition =!= {}, Return @ Extract[checkedEntries, failPosition]];
					
					(* check: fallback value is appropriate for the given property *)
					(* check: if the property value has more than just the attr() function, then the fallback value matches the type *)
					checkedEntries = fallbackTypeCheck[prop, #, Length[tokens]]& /@ checkedEntries;
					failPosition = FirstPosition[checkedEntries, _Failure, {}];
					If[failPosition =!= {}, Return @ Extract[checkedEntries, failPosition]];
					
					<|"CSSResolveValueAtComputeTime" -> <|
						"String"     -> CSSUntokenize[tokens], 
						"Property"   -> prop, 
						"Namespaces" -> declaredNamespaces|>|>					
				],
				
			(* calc() is usually resolved at compute time (CSSCascade/CSSInheritance) unless a parse error is detected or can be unambiguously made a number *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], !FreeQ[tokens, TokenPatternString["calc", "function"]]]
			] :>
				Module[{calcCheck},
					calcCheck = Catch[calcReduce /@ Extract[tokens, Position[tokens, TokenPatternString["calc", "function"]]]];
					Which[
						(* parse failure *)
						FailureQ[calcCheck], 
							calcCheck,
						
						(* parses directly to dimension|percentage|number *)
						(* consumeProperty eventually checks whether numeric values and/or percentages are compatible with property value *)
						Position[calcCheck, TokenPatternString["calc", "function"]] === {}, 
							consumeProperty[prop, calcCheck], 
						
						(* good parse that contains calc(% + dimension), but percentage is incompatible with property value *)
						Position[calcCheck, TokenPatternString["*", "percentage"]] =!= {} && !MemberQ[CSSPropertyData[prop, "Values"], "<percentage>"],
							Failure["UnexpectedParse", <|
								"MessageTemplate"   -> "Property `Prop` does not support percentages.", 
								"MessageParameters" -> <|"Prop" -> prop|>|>],
								
						True,
							<|"CSSResolveValueAtComputeTime" -> <|
								"String"     -> CSSUntokenize[calcCheck],
								"Property"   -> prop,
								"Namespaces" -> OptionValue["Namespaces"]|>|>
					]
				],
			
			(* any use of rem units must be resolved at compute time as it involves the root element of the XML document *)
			HoldPattern[
				Condition[
					consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], 
					And[
						!TrueQ[RemIsResolved],
						!FreeQ[tokens, CSSToken[KeyValuePattern[{"Type" -> "dimension", "Unit" -> _String?(StringQ[#] && StringMatchQ[CSSNormalizeEscapes @ #, "rem", IgnoreCase -> True]&)}]]]]]
			] :>
				<|"CSSResolveValueAtComputeTime" -> <|
					"String"     -> CSSUntokenize[tokens],
					"Property"   -> prop,
					"Namespaces" -> OptionValue["Namespaces"]|>|>
		},
		DownValues[consumeProperty]]


(* ::Subsection::Closed:: *)
(* new dimension definitions *)


(* CSS's DPI is fixed at 96 *)
parseLength[CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> unit_}]], inFontSize_:False] := 
	Module[{dpi = 96},
		If[TrueQ[val == 0], Return @ 0];
		(* parse units 
			If within the 'font-size' property, then relative lengths inherit from the parent.
			If a relative length is given outside the 'font-size' property, then it's a function of the current FontSize.
			The "ch" relative length is the advance measure of the "0" glyph. It cannot be determined easily in WL so we
			assume left-to-right text and use the fallback of 1/2 the calculated font size.
		*)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ unit, 
			(* relative units *)
			"em",   If[inFontSize, val*Inherited,     With[{v = val}, Dynamic[v*CurrentValue[FontSize]]]],
			"ex",   If[inFontSize, val*0.5*Inherited, With[{v = val}, Dynamic[v*CurrentValue["FontXHeight"]]]],
			"ch",   If[inFontSize, val*0.5*Inherited, Dynamic[0.5*CurrentValue[FontSize]]],
			"rem",  With[{v = val}, Dynamic[v*CurrentValue[$FrontEnd, FontSize]]], (* fallback if not resolved at compute-value time *)
			(* viewport units are a percentage of the window size *)
			"vw",   With[{v = val}, Dynamic[Round[v/100*CurrentValue[{"WindowSize", 1}]]]], 
			"vh",   With[{v = val}, Dynamic[Round[v/100*CurrentValue[{"WindowSize", 2}]]]],
			"vmin", With[{v = val}, Dynamic[Round[v/100*Min[CurrentValue["WindowSize"]]]]],
			"vmax", With[{v = val}, Dynamic[Round[v/100*Max[CurrentValue["WindowSize"]]]]],
			(* absolute units *)
			"in",   val*dpi,
			"cm",   val*dpi/2.54,
			"pc",   val*dpi/6,
			"mm",   val*dpi/25.4,
			"pt",   val*dpi/72,
			"px",   val*dpi/96,
			"q",    val*dpi/101.6,
			_,      Failure["UnexpectedParse", <|"Message" -> "Unrecognized length unit."|>]
		]
	]
	
(* deg, grad, rad are already defined in CSS 2.1 *)
parseAngle[t:CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> _}]]] /; TokenUnitIs["turn", t] := val*360

(* <frequency>, <time>, <percentage> etc. also already defined in CSS 2.1 *)


(* ::Subsection::Closed:: *)
(*<resolution>*)


(* FE uses points to indicate DPI *)
parseResolution[CSSToken[KeyValuePattern[{"Type" -> "dimension", "Value" -> val_?NumericQ, "Unit" -> unit_}]]] :=
	Module[{dpi = 96 (* fixed CSS value 1dppx = 96dpi *)},
		Switch[unit, 
			"dpi",  val,
			"dpcm", val/2.54,
			"dppx", val*dpi,
			_,      Failure["UnexpectedParse", <|"Message" -> "Unrecognized dimension unit."|>]
		]
	]
	
	
(* ::Subsection::Closed:: *)
(*Namespace*)


isNamespace[pos_, l_, tokens_] :=
	Module[{posCheck = pos},
		If[
			Or[
				TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["*", tokens[[posCheck]]], 
				TokenTypeIs["ident", tokens[[posCheck]]]]
			,
			posCheck++
			,
			Return @ False
		];
		If[posCheck <= l && TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["|", tokens[[posCheck]]],
			posCheck++
			,
			Return @ False
		];
		If[posCheck <= l, TokenTypeIs["ident", tokens[[posCheck]]], False]
	]
	
isNoNamespace[pos_, l_, tokens_] :=
	Module[{posCheck = pos},
		If[TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["|", tokens[[posCheck]]],
			posCheck++
			,
			Return @ False
		];
		If[posCheck <= l, TokenTypeIs["ident", tokens[[posCheck]]],	False]
	]
	
SetAttributes[{consumeNamespacePrefix, consumeDefaultNamespacePrefix}, HoldFirst]
consumeNamespacePrefix[pos_, l_, tokens_, namespaces_] := 
	Module[{ns},
		(* get case-sensitive namespace from declared namespaces *)
		With[{s = tokens[[pos]]["String"]}, 
			ns = FirstCase[namespaces, _?(Function[#Prefix === s])];
			(* an attr() function with an undeclared namespace prefix is invalid *)
			If[MissingQ[ns], 
				Return @ 
					Failure["BadNamespace", <|
						"Message"   -> "Not a declared namespace.",
						"Namespace" -> tokens[[pos]]["String"]|>]
				,
				ns = ns["Prefix"]
			];							
		];
		pos++; 
		pos++; (* skip "|" delim token *)
		ns
	]
	
consumeDefaultNamespacePrefix[pos_, l_, tokens_, namespaces_] :=
	Module[{ns},
		ns = FirstCase[namespaces, _?(Function[#Default === True])];
		If[MissingQ[ns], ns, ns["Prefix"]] (* could be Missing if not declared, which is an ambiguous state *)
	]


(* ::Subsection::Closed:: *)
(*calc()*)
	
	
(* this should probably go into Tokenize package *)
HighlightUntokenize[tokens:{__?CSSTokenQ}, highlightPositions_] := 
	Module[{highlights},
		highlights = 
			Replace[
				#,
				{
					CSSToken[kvp:KeyValuePattern[{"Type" -> "dimension", "String" -> s_, "Unit" -> u_}]] :> 
						CSSToken[<|kvp, 
							"String" -> "\!\(\*StyleBox[\"" <> s <> "\",Background->RGBColor[1,1,0]]\)",
							"Unit"   -> "\!\(\*StyleBox[\"" <> u <> "\",Background->RGBColor[1,1,0]]\)"|>],
					CSSToken[kvp:KeyValuePattern[{"Type" -> "percentage", "String" -> s_}]] :> 
						CSSToken[<|kvp, 
							"String" -> "\!\(\*StyleBox[\"" <> s <> "\",Background->RGBColor[1,1,0]]\)",
							"Unit"   -> "\!\(\*StyleBox[\"%\",Background->RGBColor[1,1,0]]\)"|>],
					CSSToken[kvp:KeyValuePattern[{"String" -> s_}]] :> CSSToken[<|kvp, "String" -> "\!\(\*StyleBox[\"" <> s <> "\",Background->RGBColor[1,1,0]]\)"|>]}
			]& /@ Extract[tokens, highlightPositions];
		CSSUntokenize @ ReplacePart[tokens, Thread[highlightPositions -> highlights]]			
	]
			
calcGetPositionOfDeepestMultiplicationOrDivision[tokens:{__?CSSTokenQ}] := 
	First[ReverseSortBy[Length] @ Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim", "String" -> "*" | "/"}]]], None]

$Debug = False;

calcCreateValue[inputToken_?CSSTokenQ] :=
	Module[{tokens, currentDepthPosition, tokensAtDepth, opPosition, tokenCheck, wrapperPosition, wrapperToken, pos, l},
		(* check that input token is a calc() function *)
		If[TokenTypeIs["function", inputToken] && TokenStringIs["calc", inputToken], 
			tokens = inputToken["Children"];
			,
			Throw @ 
				Failure["BadParse", <|
					"MessageTemplate" -> "Expected calc() function token.", 
					"MessageParameters" -> <||>, 
					"Token" -> inputToken|>]
		];
		
		tokenCheck = Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "dimension" | "percentage" | "number"}]]];
		(*TODO: finish this function to return value that FE understands *)
	]

calcReduce[inputToken_?CSSTokenQ] :=
	Module[{tokens, currentDepthPosition, tokensAtDepth, opPosition, tokenCheck, wrapperPosition, wrapperToken, pos, l},
		(* check that input token is a calc() function *)
		If[TokenTypeIs["function", inputToken] && TokenStringIs["calc", inputToken], 
			tokens = inputToken["Children"];
			,
			Throw @ 
				Failure["BadParse", <|
					"MessageTemplate" -> "Expected calc() function token.", 
					"MessageParameters" -> <||>, 
					"Token" -> inputToken|>]
		];
		If[$Debug, Echo["calc() function check"]];
		
		(* check for only valid operators *)
		opPosition = Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim"}]]];
		opPosition = Pick[opPosition, !MatchQ[#, "+" | "-" | "*" | "/"]& /@ Through[Extract[tokens, opPosition]["String"]]];
		If[opPosition =!= {}, 
			Throw @ 
				Failure["BadParse", <|
					"MessageTemplate" -> "calc() contains unexpected operators.", 
					"MessageParameters" -> <||>, 
					"Expr" -> HighlightUntokenize[tokens, opPosition]|>]];
		If[$Debug, Echo["calc() operator check"]];
		
		(* check for only valid values *)
		tokenCheck = Position[tokens, CSSToken[KeyValuePattern[{"Type" -> Except["delim" | "whitespace" | "percentage" | "number" | "dimension"]}]]];
		tokenCheck = Pick[tokenCheck, Not[Or[TokenTypeIs["function", #] && TokenStringIs["calc", #], TokenTypeIs["()", #]]]& /@ Extract[tokens, tokenCheck]];
		If[tokenCheck =!= {}, 
			Throw @ 
				Failure["BadParse", <|
					"MessageTemplate" -> "calc() contains unexpected tokens.", 
					"MessageParameters" -> <||>, 
					"Expr" -> HighlightUntokenize[tokens, opPosition]|>]];
		If[$Debug, Echo["calc() value check"]];
			
		(* perform reduction of calc() expression *)
		opPosition = calcGetPositionOfDeepestMultiplicationOrDivision[tokens];
		While[opPosition =!= None && currentDepthPosition =!= None,
			currentDepthPosition = Most @ opPosition;
			If[currentDepthPosition === {},
				tokensAtDepth = tokens;
				,
				tokensAtDepth = Extract[tokens, currentDepthPosition];
			];
			tokensAtDepth = calcResolveMultiplicationAndDivisionAtConstantTokenDepth[tokensAtDepth];
			If[$Debug, Echo[CSSUntokenize[tokensAtDepth], "MD"]];
			tokensAtDepth = calcResolveAdditionAndSubtractionAtConstantTokenDepth[tokensAtDepth];
			If[$Debug, Echo[CSSUntokenize[tokensAtDepth], "AS"]];
			If[currentDepthPosition =!= {},
				tokens = ReplacePart[tokens, currentDepthPosition -> tokensAtDepth];
				wrapperPosition = Drop[currentDepthPosition, -2];
				wrapperToken = Extract[tokens, wrapperPosition];
				If[TokenTypeIs["function", wrapperToken] && TokenStringIs["calc", wrapperToken],
					tokens = 
						ReplacePart[
							tokens, 
							wrapperPosition -> Replace[wrapperToken, CSSToken[kvp:KeyValuePattern[{"Children" -> c_}]] :> CreateParensToken[c]]];
				]
				,
				tokens = tokensAtDepth;
				currentDepthPosition = None (* stop While loop *)
			];
			If[$Debug, Echo[CSSUntokenize[tokens]]];
			opPosition = calcGetPositionOfDeepestMultiplicationOrDivision[tokens];
		];
		(* in case the calc expression is already flat and has no multiplication, do addition/subtraction *)
		tokens = calcResolveAdditionAndSubtractionAtConstantTokenDepth[tokens];
		
		(* reduce nested parens, normalize addition signs, and keep as calc() if reduction has more than one term *)
		tokens = calcReduceParens[tokens];
		tokens = calcNormalizeSigns[tokens];
		opPosition = Flatten @ Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim", "String" -> "+" | "-"}]]];
		If[Length[opPosition] > 0, 
			CSSToken[<|"Type" -> "function", "String" -> "calc", "Children" -> tokens|>]
			,
			pos = 1; l = Length[tokens]; TrimWhitespaceTokens[pos, l, tokens];
			tokens[[pos]]
		]
	]
	
calcReduceParens[inputTokens:{__?CSSTokenQ}] :=
	Module[{tokens = inputTokens, pos},
		(* replace calc() or () from the deepest instance to the most shallow and check for failures at each step *)
		(* Position is sorted depth-first, which is good because we should substitute the deepest calc() instances first. *)
		pos = FirstPosition[tokens, TokenPatternString["calc", "function"] | CSSToken[KeyValuePattern[{"Type" -> "()"}]], None];
		While[pos =!= None,
			(* do token replacement *)
			tokens = Flatten @ ReplacePart[tokens, pos -> Extract[tokens, pos]["Children"]];
			pos = FirstPosition[tokens, TokenPatternString["calc", "function"] | CSSToken[KeyValuePattern[{"Type" -> "()"}]], None];
		];
		tokens
	]
	
calcNormalizeSigns[inputTokens:{__?CSSTokenQ}] :=
	Module[{tokens = inputTokens, pos, l = Length[inputTokens]},
		(* replace "- -n" with "+ n" or "+ -n" with "- n" *)
		pos = Flatten @ Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim", "String" -> "+" | "-"}]]];
		Do[
			
			Which[
				TokenStringIs["+", tokens[[i]]] && i + 2 <= l && Negative[tokens[[i + 2]]["Value"]], 
					tokens[[i]] = CreateDelimToken["-"];
					tokens[[i + 2]] = calcChangeSignOfToken[tokens[[i + 2]]]
				,
				TokenStringIs["-", tokens[[i]]] && i + 2 <= l && Negative[tokens[[i + 2]]["Value"]], 
					tokens[[i]] = CreateDelimToken["+"];
					tokens[[i + 2]] = calcChangeSignOfToken[tokens[[i + 2]]]
				,
				True, Null
			];
			,
			{i, pos}
		];
		tokens
	]		

(* multiplication *)
calcMultiplyToString[v1_ ,v2_] := With[{v = v1*v2}, With[{vv = ToString[If[!IntegerQ[v], N[v], v]]}, If[StringEndsQ[vv, "."], vv <> "0", vv]]]

calcMultiplyNumberByNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcMultiplyToString[v1, v2]]
	
calcMultiplyNumberByPercentageToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcMultiplyToString[v1, v2] <> "%"]
	
calcMultiplyNumberByDimensionToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_, "Unit" -> u_}]]] := 
	First @ CSSTokenize[calcMultiplyToString[v1, v2] <> u]

(* in some cases we must distribute the multiplication through the parenthesized grouping *)
calcMultiplyNumberByParensToken[numberToken_, parenToken_] :=
	(* This function is called at a point where the paren contents have already been simplified as far as possible. *)
	CSSToken[<|
		"Type" -> "()", 
		"Children" -> (If[TokenTypeIs["dimension" | "percentage" | "number", #], calcResolveMultiplication[numberToken, #], #]& /@ parenToken["Children"])|>]
	

calcResolveMultiplication[leftToken_, rightToken_] :=
	Which[
		TokenTypeIs["number", leftToken] && TokenTypeIs["number", rightToken],     calcMultiplyNumberByNumberToken[leftToken, rightToken],
		TokenTypeIs["number", leftToken] && TokenTypeIs["percentage", rightToken], calcMultiplyNumberByPercentageToken[leftToken, rightToken],
		TokenTypeIs["percentage", leftToken] && TokenTypeIs["number", rightToken], calcMultiplyNumberByPercentageToken[leftToken, rightToken],
		TokenTypeIs["number", leftToken] && TokenTypeIs["dimension", rightToken],  calcMultiplyNumberByDimensionToken[leftToken, rightToken],
		TokenTypeIs["dimension", leftToken] && TokenTypeIs["number", rightToken],  calcMultiplyNumberByDimensionToken[rightToken, leftToken],
		TokenTypeIs["number", leftToken] && TokenTypeIs["()", rightToken],         calcMultiplyNumberByParensToken[leftToken, rightToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["number", rightToken],         calcMultiplyNumberByParensToken[rightToken, leftToken],
		True,                                                                      $Failed 				
	]
	
(* division *)
calcDivideToString[v1_ ,v2_] := With[{v = v1/v2}, With[{vv = ToString[If[!IntegerQ[v], N[v], v]]}, If[StringEndsQ[vv, "."], vv <> "0", vv]]]

calcDivideNumberByNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcDivideToString[v1, v2]]

calcDividePercentageByNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcDivideToString[v1, v2] <> "%"]
	
calcDivideDimensionByNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_, "Unit" -> u1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcDivideToString[v1, v2] <> u1]

(* in some cases we must distribute the division through the parenthesized grouping *)
calcDivideParensByNumberToken[parenToken:CSSToken[KeyValuePattern[{"Type" -> "()"}]], numberToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]]] :=
	(* This function is called at a point where the paren contents have already been simplified as far as possible. *)
	CSSToken[<|
		"Type" -> "()", 
		"Children" -> (If[TokenTypeIs["dimension" | "percentage" | "number", #], calcResolveDivision[#, numberToken], #]& /@ parenToken["Children"])|>]

calcResolveDivision[leftToken_, rightToken_] :=
	Which[
		TokenTypeIs["number", leftToken] && TokenTypeIs["number", rightToken],     calcDivideNumberByNumberToken[leftToken, rightToken],
		TokenTypeIs["percentage", leftToken] && TokenTypeIs["number", rightToken], calcDividePercentageByNumberToken[leftToken, rightToken],
		TokenTypeIs["dimension", leftToken] && TokenTypeIs["number", rightToken],  calcDivideDimensionByNumberToken[leftToken, rightToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["number", rightToken],         calcDivideParensByNumberToken[leftToken, rightToken],
		True,                                                                      $Failed 				
	]
	
calcResolveMultiplicationAndDivisionAtConstantTokenDepth[inputTokens:{__?CSSTokenQ}] :=
	Module[{tokens = inputTokens, opPositions, opPosition, numberOfSiblings, leftToken, leftTokenIndex, rightToken, rightTokenIndex, i, j, try},
		(*opPosition = calcGetPositionOfDeepestMultiplicationOrDivision[tokens];*)
		(* resolve deepest operators first *)
		opPositions = Reverse @ Flatten @ Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim", "String" -> "*" | "/"}]], {1}];
		numberOfSiblings = Length[inputTokens];
		Do[
			If[MatchQ[i, 1 | numberOfSiblings], 
				Throw @ 
					Failure["BadParse", <|
						"MessageTemplate" -> "calc() * and / operator must have a value on either side.", 
						"MessageParameters" -> <||>, 
						"Expr" -> HighlightUntokenize[tokens, {opPosition}]|>]
				, 
				j = i; RetreatPosAndSkipWhitespace[j, 1, tokens];
				leftTokenIndex = j; leftToken = tokens[[j]];
				j = i; AdvancePosAndSkipWhitespace[j, numberOfSiblings, tokens];
				rightTokenIndex = j; rightToken = tokens[[j]];
				If[TokenStringIs["*", tokens[[i]]],
					try = calcResolveMultiplication[leftToken, rightToken];
					If[FailureQ[try], 
						Throw @ 
							Failure["BadParse", <|
								"MessageTemplate" -> "calc() * operator must have at least one number on a side.", 
								"MessageParameters" -> <||>, 
								"Expr" -> HighlightUntokenize[tokens, Table[j, {j, leftTokenIndex, rightTokenIndex, 1}]]|>]
					]
					,
					try = calcResolveDivision[leftToken, rightToken];
					If[FailureQ[try], 
						Throw @ 
							Failure["BadParse", <|
								"MessageTemplate" -> "calc() / operator must have a number on the right side.", 
								"MessageParameters" -> <||>, 
								"Expr" -> HighlightUntokenize[tokens, Table[j, {j, leftTokenIndex, rightTokenIndex, 1}]]|>]
					]
				];
				tokens[[leftTokenIndex]] = try; tokens[[leftTokenIndex + 1 ;; rightTokenIndex]] = Nothing;
			]
			,
			{i, opPositions}
		];
		tokens		
	]
	
(* addition *)
(* unitless numbers added to dimensions are not allowed (only allowed as factors in multiplication and division)*)
calcAddToString[v1_ ,v2_] := With[{v = v1+v2}, With[{vv = ToString[If[!IntegerQ[v], N[v], v]]}, If[StringEndsQ[vv, "."], vv <> "0", vv]]]

calcAddNumberToNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcAddToString[v1, v2]]

calcAddPercentageToPercentageToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcAddToString[v1, v2] <> "%"]

(* Dimensions of matching type e.g. <length> such as px and cm can be added together as separate terms *)
calcAddDimensionToDimensionToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_, "Unit" -> u1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_, "Unit" -> u2_}]]] := 
	Which[
		StringMatchQ[u1, u2, IgnoreCase -> True], First @ CSSTokenize[calcAddToString[v1, v2] <> u1],
		parseType[u1] === parseType[u2],          
			CSSToken[<|
				"Type" -> "()", 
				"Children" -> {
					leftToken, 
					CSSToken[<|"Type" -> "whitespace", "String" -> " "|>], 
					CSSToken[<|"Type" -> "delim", "String" -> "+"|>], 
					CSSToken[<|"Type" -> "whitespace", "String" -> " "|>], 
					rightToken}|>],
		True, $Failed
	]

(* in some cases we must distribute the addition through the parenthesized grouping *)
(* This function is called at a point where the paren contents have already been simplified as far as possible, may contain both dimension and percentage tokens. *)
(* case z + (x + y - w) so must take sign into account and z can be negative *)
calcAddTokenToParensToken[numberToken_, parenToken_] :=
	Module[{pChildren = parenToken["Children"], typePosition},	
		(* add to matching type within paren's children *)
		(* first try for the matching type, then percentage/dimension combo *)
		Which[
			TokenTypeIs["dimension", numberToken],  
				typePosition = FirstPosition[pChildren, _?(TokenTypeIs["dimension", #] && StringMatchQ[numberToken["Unit"], #["Unit"], IgnoreCase -> True]&), {None}, {1}][[1]];
				If[typePosition =!= None, 
					Return @ 
						CreateParensToken @ 
							ReplacePart[
								pChildren, 
								{
									typePosition -> 
										If[calcTokenIsSubtracted[typePosition, pChildren], 
											calcSubtractDimensionFromDimensionToken[numberToken, Extract[pChildren, typePosition]]
											, 
											calcAddDimensionToDimensionToken[numberToken, Extract[pChildren, typePosition]]
										],
									(typePosition - 2) -> CreateDelimToken["+"]
								}]];
				
				typePosition = FirstPosition[pChildren, _?(TokenTypeIs["dimension", #]&), {None}, {1}][[1]];
				If[typePosition =!= None,
					Return @
						If[parseType[numberToken["Unit"]] === parseType[Extract[pChildren, typePosition]["Unit"]],          
							CreateParensToken @ Join[pChildren, {CreateWhitespaceToken[], CreateDelimToken["+"], CreateWhitespaceToken[], numberToken}]
							,
							$Failed]];
				
				typePosition = FirstPosition[pChildren, _?(TokenTypeIs["percentage", #]&), {None}, {1}][[1]];
				If[typePosition =!= None, 
					Return @ CreateParensToken @ Join[pChildren, {CreateWhitespaceToken[], CreateDelimToken["+"], CreateWhitespaceToken[], numberToken}]];
					
				$Failed
			,
			TokenTypeIs["percentage", numberToken], 
				typePosition = FirstPosition[pChildren, _?(TokenTypeIs["percentage", #]&), {None}, {1}][[1]];
				If[typePosition =!= None, 
					Return @ 
						CreateParensToken @ 
							ReplacePart[
								pChildren, 
								{
									typePosition -> 
										If[calcTokenIsSubtracted[typePosition, pChildren], 
											calcSubtractPercentageFromPercentageToken[numberToken, Extract[pChildren, typePosition]]
											, 
											calcAddPercentageToPercentageToken[numberToken, Extract[pChildren, typePosition]]
										],
									(typePosition - 2) -> CreateDelimToken["+"]
								}]];
				
				typePosition = FirstPosition[pChildren, _?(TokenTypeIs["dimension", #]&), {None}, {1}][[1]];
				If[typePosition =!= None,
					Return @ CreateParensToken @ Join[pChildren, {CreateWhitespaceToken[], CreateDelimToken["+"], CreateWhitespaceToken[], numberToken}]];
					
				$Failed
			,
			True, $Failed
		]
	]

calcAddParensTokenToParensToken[parenTokenLeft_, parenTokenRight_] :=
	CreateParensToken @ Join[parenTokenLeft["Children"], {CreateWhitespaceToken[], CreateDelimToken["+"], CreateWhitespaceToken[]}, parenTokenRight["Children"]]
		
calcResolveAddition[leftToken_, rightToken_] :=
	Which[
		TokenTypeIs["number", leftToken] && TokenTypeIs["number", rightToken],                          calcAddNumberToNumberToken[leftToken, rightToken],
		TokenTypeIs["percentage", leftToken] && TokenTypeIs["percentage", rightToken],                  calcAddPercentageToPercentageToken[leftToken, rightToken],
		TokenTypeIs["dimension", leftToken] && TokenTypeIs["dimension", rightToken],                    calcAddDimensionToDimensionToken[leftToken, rightToken],
		TokenTypeIs["dimension" | "percentage" | "number", leftToken] && TokenTypeIs["()", rightToken], calcAddTokenToParensToken[leftToken, rightToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["dimension" | "percentage" | "number", rightToken], calcAddTokenToParensToken[rightToken, leftToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["()", rightToken],                                  calcAddParensTokenToParensToken[leftToken, rightToken],
		True,                                                                                           $Failed
	]

(* subtraction *)
calcChangeSignOfToken[t_?CSSTokenQ] :=
	Replace[
		t, 
 		CSSToken[kvp:KeyValuePattern[{"String" -> s_, "Value" -> v_}]] :> 
 			CSSToken[<|kvp, "String" -> Which[StringStartsQ[s, "-"], StringDrop[s, 1], StringStartsQ[s, "+"], "-" <> StringDrop[s, 1], True, "-" <> s], "Value" -> -v|>]]

calcTokenIsSubtracted[tokenPos_, tokens:{__?CSSTokenQ}] :=
	Module[{pos = tokenPos},
		(* look left for first delim token *)
		RetreatPosAndSkipWhitespace[pos, 1, tokens];
		Which[
			pos < 1,                                                                  False,
			TokenTypeIs["whitespace", tokens[[pos]]],                                 False,
			TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["-", tokens[[pos]]], True,
			TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["+", tokens[[pos]]], False,
			True,                                     
				Throw @ 
					Failure["BadParse", <|
						"MessageTemplate" -> "calc() expression contains unrecognized operator.", 
						"MessageParameters" -> <||>, 
						"Expr" -> HighlightUntokenize[tokens, {1}]|>]
		]
	]

calcSubtractToString[v1_ ,v2_] := With[{v = v1-v2}, With[{vv = ToString[If[!IntegerQ[v], N[v], v]]}, If[StringEndsQ[vv, "."], vv <> "0", vv]]]

calcSubtractNumberFromNumberToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcSubtractToString[v1, v2]]

calcSubtractPercentageFromPercentageToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_}]]] := 
	First @ CSSTokenize[calcSubtractToString[v1, v2] <> "%"]

calcSubtractDimensionFromDimensionToken[leftToken:CSSToken[KeyValuePattern[{"Value" -> v1_, "Unit" -> u1_}]], rightToken:CSSToken[KeyValuePattern[{"Value" -> v2_, "Unit" -> u2_}]]] := 
	Which[
		StringMatchQ[u1, u2, IgnoreCase -> True], First @ CSSTokenize[calcSubtractToString[v1, v2] <> u1],
		parseType[u1] === parseType[u2],          
			CSSToken[<|
				"Type" -> "()", 
				"Children" -> {
					leftToken, 
					CSSToken[<|"Type" -> "whitespace", "String" -> " "|>], 
					CSSToken[<|"Type" -> "delim", "String" -> "-"|>], 
					CSSToken[<|"Type" -> "whitespace", "String" -> " "|>], 
					rightToken}|>],
		True, $Failed
	]

(* in some cases we must distribute the addition through the parenthesized grouping *)
(* case (x + y - w) - z *)
calcSubtractTokenFromParensToken[numberToken_, parenToken_] := 
	Module[{pChildren = parenToken["Children"]},	
		(* change sign of token then add *)
		pChildren[[1]] = calcChangeSignOfToken[pChildren[[1]]];
		calcAddTokenToParensToken[numberToken, CreateParensToken @ pChildren]
	]

(* case z - (x + y - w) *)
calcSubtractParensTokenFromToken[numberToken_, parenToken_] :=
	Module[{pChildren = parenToken["Children"]},	
		(* distribute the sign then add *)
		pChildren = 
			Replace[
				pChildren, 
				{
					CSSToken[<|"Type" -> "delim", "String" -> "-"|>] -> CSSToken[<|"Type" -> "delim", "String" -> "+"|>],
					CSSToken[<|"Type" -> "delim", "String" -> "+"|>] -> CSSToken[<|"Type" -> "delim", "String" -> "-"|>]}, 
				{1}];
		pChildren[[1]] = calcChangeSignOfToken[pChildren[[1]]];
 		calcAddTokenToParensToken[numberToken, CreateParensToken @ pChildren]		
	]
	
calcSubtractParensTokenFromParensToken[parenTokenLeft_, parenTokenRight_] :=
	Module[{pChildren = parenTokenRight["Children"]},	
		(* distribute the sign then add *)
		pChildren = 
			Replace[
				pChildren, 
				{
					CSSToken[<|"Type" -> "delim", "String" -> "-"|>] -> CSSToken[<|"Type" -> "delim", "String" -> "+"|>],
					CSSToken[<|"Type" -> "delim", "String" -> "+"|>] -> CSSToken[<|"Type" -> "delim", "String" -> "-"|>]}, 
				{1}];
		pChildren[[1]] = calcChangeSignOfToken[pChildren[[1]]];
 		calcAddParensTokenToParensToken[parenTokenLeft, CreateParensToken @ pChildren]		
	]

calcResolveSubtraction[leftToken_, rightToken_] :=
	Which[
		TokenTypeIs["number", leftToken] && TokenTypeIs["number", rightToken],                          calcSubtractNumberFromNumberToken[leftToken, rightToken],
		TokenTypeIs["percentage", leftToken] && TokenTypeIs["percentage", rightToken],                  calcSubtractPercentageFromPercentageToken[leftToken, rightToken],
		TokenTypeIs["dimension", leftToken] && TokenTypeIs["dimension", rightToken],                    calcSubtractDimensionFromDimensionToken[leftToken, rightToken],
		TokenTypeIs["dimension" | "percentage" | "number", leftToken] && TokenTypeIs["()", rightToken], calcSubtractParensTokenFromToken[leftToken, rightToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["dimension" | "percentage" | "number", rightToken], calcSubtractTokenFromParensToken[leftToken, rightToken],
		TokenTypeIs["()", leftToken] && TokenTypeIs["()", rightToken],                                  calcSubtractParensTokenFromParensToken[leftToken, rightToken],
		True,                                                                                           $Failed 		
	]

calcResolveAdditionAndSubtractionAtConstantTokenDepth[inputTokens:{__?CSSTokenQ}] :=
	Module[{tokens = inputTokens, opPositions, numberOfSiblings, leftToken, leftTokenIndex, rightToken, rightTokenIndex, i, j, try, nOps, nTerms, termPattern},
		opPositions = Flatten @ Position[tokens, CSSToken[KeyValuePattern[{"Type" -> "delim", "String" -> "+" | "-"}]], {1}];
		numberOfSiblings = Length[inputTokens];
		termPattern = x_?CSSTokenQ /; Or[TokenTypeIs["function", x] && TokenStringIs["calc", x], TokenTypeIs["dimension" | "percentage" | "number" | "()", x]];
		
		(* check that operators appear between numbers/percentages/dimensions *)
		nOps = Count[tokens, x_?CSSTokenQ /; TokenTypeIs["delim", x] && TokenStringIs["+"|"-", x]];
		nTerms = Count[tokens, termPattern];
		If[nTerms - 1 != nOps,
			Throw @
				Failure["BadParse", <|
					"MessageTemplate" -> If[nTerms - 1 > nOps, "calc() appears to be missing an operator.", "calc() has too many operators."], 
					"MessageParameters" -> <||>, 
					"Expr" -> HighlightUntokenize[tokens, Position[tokens, termPattern]]|>]
		];
		
		(* loop over opPositions *)
		Do[
			If[MatchQ[i, 1 | numberOfSiblings] || TokenTypeIsNot["whitespace", inputTokens[[i - 1]]] || TokenTypeIsNot["whitespace", inputTokens[[i + 1]]], 
				Throw @ 
					Failure["BadParse", <|
						"MessageTemplate" -> "calc() + and - operator must have whitespace on either side.", 
						"MessageParameters" -> <||>, 
						"Expr" -> HighlightUntokenize[tokens, {i}]|>]
				, 
				j = i; RetreatPosAndSkipWhitespace[j, 1, tokens];
				leftTokenIndex = j; leftToken = tokens[[j]];
				j = i; AdvancePosAndSkipWhitespace[j, numberOfSiblings, tokens];
				rightTokenIndex = j; rightToken = tokens[[j]];
				(* leave the addition/subtraction intact if trying to add percentage and dimension, or two dimensions of matching type but not exact unit match *)
				Which[
					Or[
						TokenTypeIs["percentage", leftToken] && TokenTypeIs["dimension", rightToken], 
						TokenTypeIs["dimension", leftToken] && TokenTypeIs["percentage", rightToken],
						And[
							TokenTypeIs["dimension", leftToken], TokenTypeIs["dimension", rightToken],
							parseType[leftToken["Unit"]] === parseType[rightToken["Unit"]],
							!StringMatchQ[leftToken["Unit"], rightToken["Unit"], IgnoreCase -> True]]],
					Null,
					
					And[
						TokenTypeIs["dimension", leftToken], TokenTypeIs["dimension", rightToken],
						parseType[leftToken["Unit"]] =!= parseType[rightToken["Unit"]]],
					Throw @ 
						Failure["BadParse", <|
							"MessageTemplate" -> "calc() cannot add or subtract incompatible types.", 
							"MessageParameters" -> <||>, 
							"Expr" -> HighlightUntokenize[tokens, {{leftTokenIndex}, {rightTokenIndex}}]|>],  
					
					True,
					Which[
						TokenStringIs["+", tokens[[i]]],
							try = calcResolveAddition[leftToken, rightToken];
							If[FailureQ[try], 
								Throw @ 
									Failure["BadParse", <|
										"MessageTemplate" -> "calc() + operator must have compatible terms on each side.", 
										"MessageParameters" -> <||>, 
										"Expr" -> HighlightUntokenize[tokens, {{leftTokenIndex}, {rightTokenIndex}}]|>]
							]
						,
						TokenStringIs["-", tokens[[i]]],
							try = calcResolveSubtraction[leftToken, rightToken];
							If[FailureQ[try], 
								Throw @ 
									Failure["BadParse", <|
										"MessageTemplate" -> "calc() - operator must have a compatible terms on each side.", 
										"MessageParameters" -> <||>, 
										"Expr" -> HighlightUntokenize[tokens, {{leftTokenIndex}, {rightTokenIndex}}]|>]
							]
						,
						True,
							Throw @ 
								Failure["BadParse", <|
									"MessageTemplate" -> "calc() contains unknown operator.", 
									"MessageParameters" -> <||>, 
									"Expr" -> HighlightUntokenize[tokens, i]|>]
					];
					tokens[[rightTokenIndex]] = try; tokens[[leftTokenIndex ;; rightTokenIndex - 1]] = CSSToken[<|"Type" -> "error", "String" -> "REMOVE"|>];
				]
			]
			,
			{i, opPositions}
		];
		DeleteCases[tokens, CSSToken[KeyValuePattern[{"Type" -> "error", "String" -> "REMOVE"}]], {1}]		
	]
	
	
(* ::Subsection::Closed:: *)
(*rem unit*)


getPositionOfNearestRemDimensionToken[tokens_] := 
	FirstPosition[
		tokens, 
		CSSToken[KeyValuePattern[{"Type" -> "dimension", "Unit" -> _String?(StringQ[#] && StringMatchQ[CSSNormalizeEscapes @ #, "rem", IgnoreCase -> True]&)}]],
		None]

replaceRemDimensionWithTokens[tokensInput:{__?CSSTokenQ}, rootFontSizeValue_, rootFontSizeDimension_] :=
	Module[{tokens = tokensInput, remPosition, remToken},
		(* If the root does not define FontSize, then wait to use fallback rem value when parsing lengths *)
		If[rootFontSizeValue === None || rootFontSizeDimension === None, Return @ tokens];
		
		(* with a root FontSize, then replace throughout *)
		remPosition = getPositionOfNearestRemDimensionToken[tokens];
		While[remPosition =!= None,
			remToken = Extract[tokens, remPosition];
			remToken = 
				Replace[
					remToken, 
					CSSToken[kvp:KeyValuePattern[{"Value" -> v_}]] :> 
						With[{new = v*rootFontSizeValue}, 
							CSSToken[<|kvp, "Unit" -> rootFontSizeDimension, "String" -> ToString[new], "Value" -> new, "ValueType" -> If[IntegerQ[new], "integer", "number"]|>]]];
			
			tokens = ReplacePart[tokens, remPosition -> remToken];
			remPosition = getPositionOfNearestRemDimensionToken[tokens];
		];
		tokens
	]
	
replaceRemDimensionsInDeclarationList[declarationsInput_?ListQ, rootFontSizeValue_, rootFontSizeDimension_] :=
	Module[{check, itemsToResolve, declarations = declarationsInput},
		check = declarations[[All, "Interpretation"]];
		itemsToResolve = Flatten @ Position[(AssociationQ[#] && KeyExistsQ[#, "CSSResolveValueAtComputeTime"])& /@ check, True];
		Do[
			declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]] = 
				CSSUntokenize @ 
					replaceRemDimensionWithTokens[
							CSSTokenize @ declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]], 
							rootFontSizeValue,
							rootFontSizeDimension];
			,
			{i, itemsToResolve}];
		declarations
	]


(* ::Subsection::Closed:: *)
(*attr()*)


ClearAll[attrFailureBadAttrName, attrFailureBadNamespace, attrFailureUnknownUnitType];
attrFailureNotAttrFunction = 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Expected an attr() function token.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
		
attrFailureBadAttrName[token_] := 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Expected an ident token for the attribute name.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>,
		"Type"              -> token|>];
		
attrFailureBadNamespace[ns_] := 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Undeclared namespace.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>,
		"Namespace"         -> ns|>];
		
attrFailureUnknownUnitType[unit_] := 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Unknown attribute unit or type.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>,
		"Type"              -> unit|>];
		
attrFailureNoComma =
	Failure["UnexpectedToken", <|
		"MessageTemplate" -> "Expected a comma token before the fallback value.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];

attrFailureMissingFallback =
	Failure["UnexpectedToken", <|
		"MessageTemplate" -> "Missing fallback value.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];

attrFailureInvalidFallback =
	Failure["UnexpectedToken", <|
		"MessageTemplate" -> "Invalid fallback value.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
		
attrFailureFallbackContainsAttrFunctions =
	Failure["UnexpectedToken", <|
		"MessageTemplate" -> "Fallback cannot contain additional attr() functions.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];


(*attributeFallbackDefaults[type_String] :=
	Switch[type,
		"string",  "",
		"color",   Dynamic[CurrentValue[FontColor]],
		"url",     CSSToken[<|"Type" -> "url", "String" -> "", "Quotes" -> "\""|>],
		"%",       CSSToken[<|"Type" -> "percentage", "String" -> "0",   "Value" -> 0,  "ValueType" -> "integer"|>],
		"number",  CSSToken[<|"Type" -> "number",     "String" -> "0.0", "Value" -> 0., "ValueType" -> type|>],
		"integer", CSSToken[<|"Type" -> "number",     "String" -> "0",   "Value" -> 0,  "ValueType" -> type|>],
		_,         CSSToken[<|"Type" -> "dimension",  "String" -> "0",   "Value" -> 0,  "ValueType" -> "integer", "Unit" -> type|>]
	]*)
	
attributeFallbackDefaults[type_String] :=
	Switch[type,
		"string",  "", (* to be interpreted later as "" *)
		"color",   "currentColor",
		"url",     "\"\"", (* to be interpreted later as url("") *)
		"%",       "0%",
		"number",  "0.0",
		"integer", "0",
		_,         "0" <> type
	]

parseAttrFunctionToken[t_?CSSTokenQ, namespaces_] :=
	Module[{pos, l, tokens, ns, attribName, typeOrUnit, fallback, defaultNS},
		If[TokenTypeIsNot["function", t] || TokenStringIsNot["attr", t], Return @ attrFailureNotAttrFunction];
		
		tokens = t["Children"];
		pos = 1; 
		l = Length[tokens];
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* ==== get attr-name which may have a namespace ==== *)
		ns = Missing["NotFound"];
		Switch[tokens[[pos]]["Type"],
			"ident",
				(* check whether ident is a namespace *)
				If[isNamespace[pos, l, tokens], ns = consumeNamespacePrefix[pos, l, tokens, namespaces]];
				If[FailureQ[ns], Return @ ns],
			"delim",
				Switch[tokens[[pos]]["String"],
					"*", 
						(* check whether universal selector is the namespace *)
						If[isNamespace[pos, l, tokens], pos++; pos++; ns = All, Return @ attrFailureBadAttrName["*"]],
					"|", 
						(* check whether the namespace declaration is followed by ident *)
						If[isNoNamespace[pos, l, tokens], pos++; ns = None,	Return @ attrFailureBadAttrName["|"]],
					_, 
						Return @ attrFailureBadAttrName[tokens[[pos]]["String"]]
				],
			_, Return @ attrFailureBadAttrName[tokens[[pos]]["String"]]
		];
		
		(* check attr() namespace against declared namespaces *)
		defaultNS = FirstCase[namespaces, _?(Function[#Default === True])];
		Which[
			MissingQ[ns] &&  MissingQ[defaultNS], Null, (* no namespace in attr() and no namespace declared *) (* pretty common *)
			MissingQ[ns] && !MissingQ[defaultNS], Null, (* attr() takes on default namespace *)
			MatchQ[ns, None | All],               Null, (* None or All attr() namespace is always allowed *)
			True, 
				If[MissingQ[FirstCase[namespaces, _?(Function[#Prefix === ns])]], Return @ attrFailureBadNamespace[ns]]
		]; 
		
		(* get attribute name *)
		If[pos > l || TokenTypeIsNot["ident", tokens[[pos]]], Return @ attrFailureBadAttrName[tokens[[pos]]["Type"]]];
		attribName = tokens[[pos]]["String"];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		(* ==== get optional type or unit ==== *)
		If[pos <= l && TokenTypeIs["ident", tokens[[pos]]],
			typeOrUnit = CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			,
			typeOrUnit = "string"
		];
		If[!StringMatchQ[
			typeOrUnit, 
			Alternatives[
				"string", "color", "url", "integer", "number", "length", 
				"em", "ex", "ch", "rem", "vw", "vh", "vmin", "vmax", "in", "cm", "mm", "px", "pc", "pt", "q",
				"frequency", "hz", "khz",	"time", "s", "ms", "angle", "deg", "grad", "rad", "turns", "%"]],
			Return @ attrFailureUnknownUnitType[typeOrUnit]
		];
		
		(* ==== check for fallback syntax, which could still lead to a failure ==== *)
		(* if nothing but whitespace remains, then the value is good to go with a default fallback value*)
		If[(pos > l) || (pos == l && TokenTypeIs["whitespace", tokens[[pos]]]), 
			fallback = attributeFallbackDefaults[typeOrUnit];
			Return @ <|"NamespacePrefix" -> ns, "AttributeName" -> attribName, "Type" -> typeOrUnit, "Fallback" -> fallback|>
		];
		
		(* comma must be present for a non-default fallback to exist *)
		If[TokenTypeIsNot["comma", tokens[[pos]]], Return @ attrFailureNoComma];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; 
		If[(pos > l) || (pos == l && TokenTypeIs["whitespace", tokens[[pos]]]), Return @ attrFailureMissingFallback];
		If[!FreeQ[tokens[[pos ;; ]], CSSToken[KeyValuePattern[{"Type" -> "function", "String" -> "attr"}]]],
			Return @ attrFailureFallbackContainsAttrFunctions
		];
		fallback = tokens[[pos]];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		(* success if only whitespace remains, otherwise it's an invalid fallback *)
		If[(pos > l) || (pos == l && TokenTypeIs["whitespace", tokens[[pos]]]), 
			<|"NamespacePrefix" -> ns, "AttributeName" -> attribName, "Type" -> typeOrUnit, "Fallback" -> CSSUntokenize @ fallback|>
			,
			attrFailureInvalidFallback
		]		
	]
	

badAttrFailure[type_, value_] := 
	Failure["BadAttr", <|
		"MessageTemplate" -> "Attribute value `Value` does not match provided type `Type`.", 
		"MessageParameters" -> <|"Type" -> type, "Value" -> value|>|>]

allLengths = Alternatives["em", "rem", "ex", "ch", "vw", "vh", "vmin", "vmax", "in", "cm", "pc", "mm", "pt", "px", "q"];

parseAttrFromType[type_, value_] :=
	Module[{t},
		Switch[CSSNormalizeEscapes @ ToLowerCase @ type,
			"string",  "\"" <> value <> "\"",
			t = First[CSSTokenize[value]];
			"color",   If[FailureQ[parseSingleColor["color", t]], badAttrFailure[type, value], value],
			(* TODO: relative urls must be made absolute according to the input doc *)
			"url",     If[FailureQ[parseURI["uri", First @ CSSTokenize["url(" <> value <> ")"]]], badAttrFailure[type, value], value],
			"integer", If[TokenTypeIs["number", t] && t["ValueType"] === "integer", value, badAttrFailure[type, value]],
			"number",  If[TokenTypeIs["number", t] && t["ValueType"] === "number",  value, badAttrFailure[type, value]],
			"%",       If[TokenTypeIs["number", t], value <> "%", badAttrFailure[type, value]],
			"em" | "rem" | "ex" | "ch" | "vw" | "vh" | "vmin" | "vmax" | "in" | "cm" | "pc" | "mm" | "pt" | "px" | "q",
				If[TokenTypeIs["number", t], StringTrim @ value <> type, badAttrFailure[type, value]],
			"hz" | "khz",
				If[TokenTypeIs["number", t], StringTrim @ value <> type, badAttrFailure[type, value]],
			"s" | "ms",
				If[TokenTypeIs["number", t], StringTrim @ value <> type, badAttrFailure[type, value]],
			"deg" | "grad" | "rad" | "turns",
				If[TokenTypeIs["number", t], StringTrim @ value <> type, badAttrFailure[type, value]],
			"dpi" | "dpcm" | "dppx",
				If[TokenTypeIs["number", t], StringTrim @ value <> type, badAttrFailure[type, value]],
			"length", 
				If[TokenTypeIs["dimension", t] && TokenUnitIs[allLengths, t], value, badAttrFailure[type, value]],
			"angle",
				If[TokenTypeIs["dimension", t] && TokenUnitIs["deg" | "grad" | "rad" | "turns", t], value, badAttrFailure[type, value]],
			"time",
				If[TokenTypeIs["dimension", t] && TokenUnitIs["s" | "ms", t], value, badAttrFailure[type, value]],
			"frequency", 
				If[TokenTypeIs["dimension", t] && TokenUnitIs["hz" | "khz", t], value, badAttrFailure[type, value]]
		]
	]


replaceAttrFunctionsWithTokens[tokensInput:{__?CSSTokenQ}, element_?CSSTargetQ, ssNamespaces_] :=
	Module[{tokens = tokensInput, attrPosition, attrToken, attrCheck, attrNS, attrValue, elementAttributeNamePosition, elementAttributeNamespace, elementAttributeValue, temp},
		
		(* replace attr() from the deepest instance to the most shallow and check for failures at each step *)
		(* Position is sorted depth-first, which is good because we should substitute the deepest attr() instances first. *)
		attrPosition = FirstPosition[tokens, TokenPatternString["attr", "function"], None];
		While[attrPosition =!= None,
			attrToken = Extract[tokens, attrPosition];
			
			(* parse attr() function to <|"NamespacePrefix" -> _, "AttributeName" -> _, "Type" -> _, "Fallback" -> _|> *)
			attrCheck = parseAttrFunctionToken[attrToken, ssNamespaces]; 
			If[FailureQ[attrCheck], Return @ attrCheck];
			
			(* check that attribute name exists *)
			elementAttributeNamePosition = FirstPosition[Keys @ element["Attributes"], _?(StringMatchQ[#, attrCheck["AttributeName"], IgnoreCase -> !element["CaseSensitive"]["AttributeName"]]&), {None}, {1}, Heads -> False][[1]];
			If[elementAttributeNamePosition === None, 
				(* if not, get attr() fallback value *)
				temp = parseAttrFromType[attrCheck["Type"], attrCheck["Fallback"]];
				If[FailureQ[temp], Return @ temp, attrValue = temp];
				,
				(* else, since attribute name exists, now check that namespaces match *)
				{elementAttributeNamespace, elementAttributeValue} = {#Namespace, #Value}& @ element["Attributes"][Keys[element["Attributes"]][[elementAttributeNamePosition]]];
				attrNS = FirstCase[ssNamespaces, KeyValuePattern[{"Prefix" -> attrCheck["NamespacePrefix"]}]]["Namespace"];
				If[
					Or[
						attrCheck["NamespacePrefix"] === All,
						(MissingQ[attrCheck["NamespacePrefix"]] || attrCheck["NamespacePrefix"] === None) && elementAttributeNamespace === None,
						FirstCase[ssNamespaces, KeyValuePattern[{"Prefix" -> attrCheck["NamespacePrefix"]}]]["Namespace"] === elementAttributeNamespace
					]
					,
					(* namespaces match so use element's attribute value *)
					temp = parseAttrFromType[attrCheck["Type"], elementAttributeValue];
					If[FailureQ[temp], 
						(* element's attribute value failed to parse, so use attr() fallback value *)
						temp = parseAttrFromType[attrCheck["Type"], attrCheck["Fallback"]];
						If[FailureQ[temp], Return @ temp, attrValue = temp];
						,
						attrValue = temp
					]
					,
					(* namespaces do not match so use fallback attr() value *)
					temp = parseAttrFromType[attrCheck["Type"], attrCheck["Fallback"]];
					If[FailureQ[temp], Return @ temp, attrValue = temp];
				];
			];
			
			tokens = ReplacePart[tokens, attrPosition -> Unevaluated[Sequence @@ CSSTokenize[attrValue]]];
			attrPosition = FirstPosition[tokens, TokenPatternString["attr", "function"], None];
		];
		tokens
	];
	
replaceAttrFunctionsInDeclarationList[declarationsInput_?ListQ, element_?CSSTargetQ] :=
	Module[{check, itemsToResolve, declarations = declarationsInput, ssNamespaces, try},
		check = declarations[[All, "Interpretation"]];
		itemsToResolve = Flatten @ Position[(AssociationQ[#] && KeyExistsQ[#, "CSSResolveValueAtComputeTime"])& /@ check, True];
		Do[
			ssNamespaces = declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "Namespaces"]];
			try = 
				replaceAttrFunctionsWithTokens[
					CSSTokenize @ declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]], 
					element,
					ssNamespaces];
			If[FailureQ[try],
				declarations[[i, "Interpretation"]] = try
				,
				declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]] =	CSSUntokenize @ try
			];
			,
			{i, itemsToResolve}];
		declarations
	]
	
	
End[] (* End Private Context *)

EndPackage[]