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
				
			(* any use of calc() must be resolved at compute time (CSSCascade/CSSInheritance) unless a parse error is detected *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], !FreeQ[tokens, TokenPatternString["calc", "function"]]]
			] :>
				Module[{calcCheck, failPosition},
					calcCheck = parseCalcFunctionToken[#, {}]& /@ Extract[tokens, Position[tokens, TokenPatternString["calc", "function"]]];
					failPosition = FirstPosition[Replace[calcCheck, x_Failure :> x["MessageParameters"]["Type"], 1], "Parse", {}, 1];
					If[failPosition =!= {},
						Extract[calcCheck, failPosition]
						,
						<|"CSSResolveValueAtComputeTime" -> <|
							"String"     -> CSSUntokenize[tokens],
							"Property"   -> prop,
							"Namespaces" -> OptionValue["Namespaces"]|>|>
					]
				]},
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
			
			Viewport widths vh, vw, vmin, vmax
		*)
		Switch[CSSNormalizeEscapes @ ToLowerCase @ unit, 
			(* relative units *)
			"em",   If[inFontSize, val*Inherited,     With[{v = val}, Dynamic[v*CurrentValue[FontSize]]]],
			"ex",   If[inFontSize, val*0.5*Inherited, With[{v = val}, Dynamic[v*CurrentValue["FontXHeight"]]]],
			"ch",   If[inFontSize, val*0.5*Inherited, Dynamic[0.5*CurrentValue[FontSize]]],
			"rem",  If[inFontSize, val*0.5*Inherited, Dynamic[0.5*CurrentValue[FontSize]]],
			(* viewport units *)
			"vw",   Null, (* TODO *)
			"vh",   Null,
			"vmin", Null,
			"vmax", Null,
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
(*Parsing*)


(* ========== Parsing attr() function ========== *)
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
	
	
(* ========== Parsing calc() function ========== *)
ClearAll[calcFailureNoDefinition, calcFailureRecursion];
calcFailureNotCalcFunction = 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Expected a calc() function token.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];

reduceNestedCalcFunctionsToParens[t_?CSSTokenQ] :=
	Module[{tokens, calcToken, calcPosition, calcCheck, l},
		If[TokenTypeIsNot["function", t] || TokenStringIsNot["calc", t], Return @ calcFailureNotCalcFunction];
			
		tokens = t["Children"];
		(* replace calc() from the deepest instance to the most shallow and check for failures at each step *)
		(* Position is sorted depth-first, which is good because we should substitute the deepest calc() instances first. *)
		calcPosition = FirstPosition[tokens, TokenPatternString["calc", "function"], None];
		l = Length[tokens];
		While[calcPosition =!= None,
			calcToken = Extract[tokens, calcPosition];
			
			(* do token replacement *)
			calcCheck = CSSToken[<|"Type" -> "()", "Children" -> calcToken["Children"]|>];
			tokens = ReplacePart[tokens, calcPosition -> calcCheck];
			calcPosition = FirstPosition[tokens, TokenPatternString["calc", "function"], None];
			l = Length[tokens];
		];
		Replace[t, CSSToken[kvp:KeyValuePattern[{"Children" -> _}]] :> CSSToken[<|kvp, "Children" -> tokens|>]]
	]
	

parseCalcFunctionToken[t_?CSSTokenQ] :=
	Module[{pos = 1, tokens, l, leftValue, rightValue, operator},
		If[Not[(TokenTypeIs["function", t] && TokenStringIs["calc", t]) || TokenTypeIs["()", t]], Return @ calcFailureNotCalcFunction];
		
		tokens = Length[t["Children"]];
		l = Length[tokens];
		pos = 1;
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		 
		Switch[tokens[[pos]]["Type"],
			"dimension",  Null,
			"number",     Null,
			"percentage", Null,
			"()",         parseCalcFunctionToken[tokens[[pos]]],
			"function",   parseCalcFunctionToken[tokens[[pos]]],
			_,            Null
		]; 
		
	]
	
	
(* ::Subsection::Closed:: *)
(*Token Replacing*)


(* ========== attr() replacment ========== *)
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
	Module[{tokens = tokensInput, attrPosition, attrToken, attrCheck, attrNS, attrValue, l, elementAttributeNamePosition, elementAttributeNamespace, elementAttributeValue, temp},
		
		(* replace attr() from the deepest instance to the most shallow and check for failures at each step *)
		(* Position is sorted depth-first, which is good because we should substitute the deepest attr() instances first. *)
		attrPosition = FirstPosition[tokens, TokenPatternString["attr", "function"], None];
		l = Length[tokens];
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
					]
					,
					(* namespaces do not match so use fallback attr() value *)
					temp = parseAttrFromType[attrCheck["Type"], attrCheck["Fallback"]];
					If[FailureQ[temp], Return @ temp, attrValue = temp];
				];
			];
			
			tokens = ReplacePart[tokens, attrPosition -> Unevaluated[Sequence @@ CSSTokenize[attrValue]]];
			attrPosition = FirstPosition[tokens, TokenPatternString["attr", "function"], None];
			l = Length[tokens];
		];
		tokens
	];
	
replaceAttrFunctionsInDeclarationList[declarationsInput_?ListQ, element_?CSSTargetQ] :=
	Module[{check, itemsToResolve, declarations = declarationsInput, ssNamespaces},
		check = declarations[[All, "Interpretation"]];
		itemsToResolve = Flatten @ Position[(AssociationQ[#] && KeyExistsQ[#, "CSSResolveValueAtComputeTime"])& /@ check, True];
		Do[
			ssNamespaces = declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "Namespaces"]];
			declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]] = 
				CSSUntokenize @ 
					replaceAttrFunctionsWithTokens[
							CSSTokenize @ declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]], 
							element,
							ssNamespaces];
			,
			{i, itemsToResolve}];
		declarations
	]

	

	
	
	
End[] (* End Private Context *)

EndPackage[]