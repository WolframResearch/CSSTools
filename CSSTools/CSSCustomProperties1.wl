(* Wolfram Language Package *)

BeginPackage["CSSTools`CSSCustomProperties1`"]
(* Exported symbols added here with SymbolName::usage *)  

(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSToken, CSSTokenQ, CSSUntokenize *)
(* CSSPropertyInterpreter` 
	---> defines consumeProperty *)
(* CSSStyleSheetInterpreter` 
	---> defines inheritedPropertyRules *)

Needs["CSSTools`CSSTokenizer`"];   
Needs["CSSTools`CSSPropertyInterpreter`"];
Needs["CSSTools`CSSStyleSheetInterpreter`"];


Begin["`Private`"] (* Begin Private Context *) 


(* ::Subsection::Closed:: *)
(*custom properties*)


(* Custom Property Definition
	A custom CSS property definition is defined by having a property name that starts with two dashes.
	The values of a custom property definition are very permissive:
		* CSS Syntax 3 trims whitespace around property values; effectively substituting Nothing
		* any token sequence is allowed (bad tokens like bad-url or unmatched brackets still fail)
		* the 'initial' ident token is replaced with a "guaranteed-failure" value
		* the 'inherit' or 'unset' ident tokens are effectively ignored since all custom properties inherit
	
	Custom property definitions follow CSS cascade rules: the most important and highest specificity variable definition found in the CSS is used *)

(* var() referencing of custom property definitions
	* A custom CSS property definition is referenced with the var() function e.g. var(--prop, fallback)
	* var() can only appear in the property value
	* var() is parsed as a token sequence, not string substitution e.g. var(--prop)px is <token-prop><token-px> not <token-prop-px> 
	* The fallback is not required. If present, it can be any token sequence, including comma tokens or more var() substitutions.
	* Care must be taken to avoid generation of large token sequences. A cutoff token length is suggested but arbitrary.
	+ All var() substitution happens at compute time during the cascade/inheritance:
		* any var() in a property value effectively keeps the entire value from parsing
		* at compute time the value can fail to parse after substitution  
		* recursive definitions are checked and, if found, generates a "guaranteed-failure" value
	*)
	
(* The DownValue for consuming var() referencing in properties needs to come early in order to override existing behavior *)
(* 
	The DownValue position for defining custom properties is not as particular since it does not conflict with vendor prefixes e.g. -moz-*, -webkit-* etc.
	We put it at the start of the DownValues list as well for no good reason. *)
DownValues[consumeProperty] = 
	Join[
		{
			(* corner case of no property value is a valid custom property definition *)
			HoldPattern[
				Condition[consumeProperty[prop_String, {}, opts:OptionsPattern[]], StringQ[prop] && StringStartsQ[prop, "--"]]
			] :>
				<|"CSSCustomPropertyDefinition" -> <|
					"Name"  -> prop, 
					"Value" -> Nothing,
					"Namespaces" -> OptionValue["Namespaces"]|>|>,
					
			(* normal custom property definition *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], StringQ[prop] && StringStartsQ[prop, "--"]]
			] :> 
				<|"CSSCustomPropertyDefinition" -> <|
					"Name"  -> prop, 
					"Value" -> 
						If[Length[tokens] == 1 && TokenTypeIs["ident", tokens[[1]]],
							Which[
								TokenStringIs["initial", tokens[[1]]],           Failure["GuaranteedInvalidValue", <|"Message" -> "No custom property found."|>],
								TokenStringIs["inherit" | "unset", tokens[[1]]], Inherited,
								True,                                            CSSUntokenize[tokens]
							]
							,
							CSSUntokenize[tokens]
						],
					"Namespaces" -> OptionValue["Namespaces"]|>|>,
						
			(* any use of var() must be resolved at compute time (CSSCascade/CSSInheritance) unless a parse error is detected *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}, opts:OptionsPattern[]], !FreeQ[tokens, TokenPatternString["var", "function"]]]
			] :>
				Module[{varCheck, failPosition},
					varCheck = parseVarFunctionToken[#, {}]& /@ Extract[tokens, Position[tokens, TokenPatternString["var", "function"]]];
					failPosition = FirstPosition[Replace[varCheck, x_Failure :> x["MessageParameters"]["Type"], 1], "Parse", {}, 1];
					If[failPosition =!= {},
						Extract[varCheck, failPosition]
						,
						<|"CSSResolveValueAtComputeTime" -> <|
							"String"     -> CSSUntokenize[tokens],
							"Property"   -> prop,
							"Namespaces" -> OptionValue["Namespaces"]|>|>
					]
				]},
		DownValues[consumeProperty]]
		

(* all custom property definitions inherit *)
inheritedPropertyRules = Append[inheritedPropertyRules, StringStartsQ[#, "--"]&];
			

(* ========== Parsing var() function ========== *)

ClearAll[varFailureNoDefinition, varFailureRecursion];
varFailureNotVarFunction = 
	Failure["UnexpectedToken", <|
		"MessageTemplate"   -> "Expected a var() function token.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
varFailureNoName =
	Failure["GuaranteedInvalidValue", <|
		"MessageTemplate" -> "Missing custom property name.", 
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
varFailureBadName =
	Failure["GuaranteedInvalidValue", <|
		"MessageTemplate" -> "Invalid custom property name.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
varFailureNoDefinition[def_] :=
	Failure["GuaranteedInvalidValue", <|
		"MessageTemplate" -> "Failed to find custom property definition.",
		"Prop" -> def,
		"MessageParameters" -> <|"Type" -> "Compute"|>|>];
varFailureNoComma =
	Failure["GuaranteedInvalidValue", <|
		"MessageTemplate" -> "Expected a comma token before the fallback value.",
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];
varFailureRecursion[cycles_] :=
	Failure["GuaranteedInvalidValue", <|
		"MessageTemplate" -> "Recursive custom property definition.",
		"Loops" -> cycles,
		"MessageParameters" -> <|"Type" -> "Parse"|>|>];

Attributes[getCustomPropertyNameFromVarFunction] = {HoldRest};
getCustomPropertyNameFromVarFunction[token_?CSSTokenQ] :=
	Module[{pos = 1, l, tokens},
		tokens = token["Children"];
		l = Length[tokens];
		getCustomPropertyNameFromVarFunction[token, pos, l, tokens]
	]

getCustomPropertyNameFromVarFunction[token_?CSSTokenQ, pos_, l_, tokens_] :=
	Module[{},
		If[TokenTypeIsNot["function", token] || TokenStringIsNot["var", token], Return @ varFailureNotVarFunction];
		
		(* case of var() with no arguments *)
		If[l == 0, Return @ varFailureNoName]; 
		
		(* var() function could have leading whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		If[pos > l, Return @ varFailureNoName];
		
		(* next token string must start with "--" *)
		If[TokenTypeIs["ident", tokens[[pos]]] && StringStartsQ[tokens[[pos]]["String"], "--"], tokens[[pos]]["String"], varFailureBadName]
	]

parseVarFunctionToken[token_?CSSTokenQ, replacements_] :=
	Module[{pos = 1, l, tokens, name},
		tokens = token["Children"];
		l = Length[tokens];
		name = getCustomPropertyNameFromVarFunction[token, pos, l, tokens];
		If[FailureQ[name], Return @ name];
		
		(* ==== check for fallback syntax, which could still lead to a failure ==== *)
		
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		
		(* if nothing but whitespace remains, then the value is good to go *)
		If[(pos > l) || (pos == l && TokenTypeIs["whitespace", tokens[[pos]]]), 
			If[MemberQ[replacements[[All, 1]], name],
				Return @ CSSTokenize[name /. replacements]
				,
				Return @ varFailureNoDefinition[name]
			]
		];
		
		(* comma must be present for a fallback to exist *)
		If[TokenTypeIsNot["comma", tokens[[pos]]], Return @ varFailureNoComma];
		pos++; (* everything after the comma is valid, even whitespace tokens *)
		If[MemberQ[replacements[[All, 1]], name],
			CSSTokenize[name /. replacements]
			,
			If[pos > l, Nothing, tokens[[pos ;; ]]]
		]			
	]
	

(* ========== var() function substitution ========== *)
customPropertyRelationshipGraph[replacements:{Rule[_?StringQ, _?StringQ]...}] := 
	Module[{reps, newRules},
		(* find all var() token instances in the custom property replacement rules; get the names; ignore Failures as these should be removed during other parsing *)
		reps = CSSTokenize /@ replacements[[All, 2]];
		newRules = Map[getCustomPropertyNameFromVarFunction, Extract[#, Position[#, TokenPatternString["var", "function"]]]]& /@ reps;
		newRules = 
			With[{choices = FreeQ[#, Failure]& /@ newRules},
				Flatten[Thread /@ Thread[Rule[Pick[replacements[[All, 1]], choices], Pick[newRules, choices]]]]];
		Graph[newRules]
	]


(* check for recursive definitions; return cycles/self-loops if any found *)
customPropertyDefinitionCycles[replacements:{Rule[_?StringQ, _?StringQ]...}] :=
	Module[{g},
		(* make a graph and check for looping structures *)
		g = customPropertyRelationshipGraph[replacements];
		Join[(*cycles*)FindCycle[g, Infinity, All], (*self loops*)EdgeList[g, _?(#[[1]] === #[[2]]&)]]
	]

replaceVarFunctionWithTokens[tokensInput:{__?CSSTokenQ}, replacements_] :=
	Module[{cycles, tokens = tokensInput, varPosition, varToken, varCheck, name, l},
		(* get any cyclic dependencies in the replacement rules *)
		cycles = customPropertyDefinitionCycles[replacements];
		
		(* replace var() from the deepest instance to the most shallow and check for failures at each step *)
		(* Position is sorted depth-first, which is good because we should substitute the deepest var() instances first. *)
		varPosition = FirstPosition[tokens, TokenPatternString["var", "function"], None];
		l = Length[tokens];
		While[varPosition =!= None && l < 10^6,
			varToken = Extract[tokens, varPosition];
			
			(* check for recursion *)
			name = getCustomPropertyNameFromVarFunction[varToken];
			If[!FreeQ[cycles, name], Return @ varFailureRecursion[Pick[cycles, !FreeQ[#, name]& /@ cycles]]];
			
			(* do token replacement *)
			varCheck = parseVarFunctionToken[varToken, replacements];
			If[FailureQ[varCheck], Return @ varCheck, tokens = ReplacePart[tokens, varPosition -> Unevaluated[Sequence @@ varCheck]]];
			varPosition = FirstPosition[tokens, TokenPatternString["var", "function"], None];
			l = Length[tokens];
		];
		tokens //. HoldPattern[{a___, Sequence[b__], c___}] :> {a, b, c} (* flatten any Sequence held within Association *)
	]


(* ================= CSSCustomPropertyDefinition replacement if they contain other functions ================= *)
(*
	CSS is almost frustratingly flexible. A CSS custom property declaration e.g. --varName:varValue can itself have
	other resolvable functions in its property value. Moreover, @media declarations can re-define the property
	based on some condition. So what do we do?
	
	We need to resolve the property definitions before we can substitute them elsewhere in the CSS.
	
	Resolving calc() and attr() (from CSSValuesAndUnits) is relatively simple to do in comparison to resolving other var() functions.
	Recall that var() functions can contain dependencies that could lead to infinite recursion. Thus, we create
	a dependency graph and check it for loops. If no loops exist, then the substitution order is determined by   
	following the directed graph.
	
	If there are multiple property declarations with the same property name (recall the prop name is case sensitive),
	then we follow each definition as it makes substitutions into subsequent custom properties. Any conditions on 
	said definition are joined with any other conditions that are encountered along the way.
	
	In a sense we have a multi-verse scenario: any custom property definition is resolved to its logical conclusion, 
	but there could be many such conclusions. After each branch in the chain is fully resolved, we merge them all
	back together. It often ends up duplicating the custom property definitions, but each one is also often unique in its 
	stated conditions. Even if there is no uniqueness of condition, the cascade-order has not been affected by the duplications,
	so the correct value can be found by assembling all definitions together with their conditions.
	
	Assembling duplicate props with conditions is normally handled by CSSTools`CSSStyleSheetInterpreter`Private`assembleWithConditions.
	
	There are further consequences to this approach. Now that we have duplicate custom properties with varying conditions, 
	any substitution into a var() function will ultimately pass those conditions on. This bloat will slow down the FE
	as it attempts to resolve all the conditions, but that is the world we live in. In practice it is better to work with
	"simplified" CSS in that all media conditions have first been excised before running CSSCascade or CSSInheritance.
*)
Options[resolveCSSCustomPropertyDefinition] = {"PropertyIsCaseSensitive" -> False}
resolveCSSCustomPropertyDefinition[CSSDataSubset_, scope_, opts:OptionsPattern[]] :=
	Module[{replacements, cycles, localSubset = CSSDataSubset, graph, customProp, localDecs, propLoop, result, tmp, pos},
		(* check for dependency loops. Remove any dependent properties, but issue a warning. *)
		replacements = getAllVarReplacementRules[localSubset];
		graph = Graph[customPropertyRelationshipGraph[replacements], VertexLabels -> Automatic];
		cycles = Join[(*cycles*)FindCycle[graph, Infinity, All], (*self loops*)EdgeList[graph, _?(#[[1]] === #[[2]]&)]];
		If[cycles =!= {}, Echo[varFailureRecursion /@ cycles, "CSS custom property recursion detected:"]];
		Do[
			If[!ListQ[cycles[[i]]], cycles[[i]] = {cycles[[i]]}];
			pos = Position[localSubset, KeyValuePattern[{"Property" -> Alternatives @@ cycles[[i]][[All, 1]]}]];
			Do[localSubset[[Sequence @@ j]] = <|localSubset[[Sequence @@ j]], "Interpretation" -> varFailureRecursion[cycles[[i]]]|>, {j, pos}]
			,
			{i, 1, Length[cycles], 1}
		];
		replacements = getAllVarReplacementRules[localSubset];
		graph = Graph[customPropertyRelationshipGraph[replacements], VertexLabels -> Automatic];
		
		(* Which property should we evaluate first? Assuming there's no dependency loops, start with the property that is the source of all others *)
		customProp = First[Pick[VertexList[graph], 0 === #& /@ VertexOutDegree[graph]], None];
		
		(* get all cascade-ordered declarations, remove ones with parse failures *)
		localDecs = CSSTools`CSSStyleSheetInterpreter`Private`cssCascadeDeclarations[scope, localSubset, All, opts];
		localDecs = DeleteCases[localDecs, KeyValuePattern[{"Interpretation" -> _?FailureQ}]];
		
		(* tokenize CSSCustomPropertyDefinition instances and add tokens as a new key *)
		Do[
			tmp = localDecs[[i]];
			If[KeyExistsQ[tmp["Interpretation"], "CSSCustomPropertyDefinition"], 
				AssociateTo[tmp["Interpretation", "CSSCustomPropertyDefinition"], "Tokens" -> CSSTokenize[tmp["Value"]]]];
			localDecs[[i]] = tmp
			,
			{i, Length[localDecs]}];
			
		(* 
			If there are multiple prop definitions, then we need to follow each to its end state.
			This could incur recursive function calls, which is OK as each duplication makes a same-length copy of the declarations. 
			To keep cascade-ordering, transpose all same-length "chains" together and delete duplicate declarations. *)
		propLoop = Flatten @ Position[localDecs, KeyValuePattern[{"Property" -> customProp}], {1}];
		Do[localDecs[[i]] = reduceDeclarationWithoutVar[localDecs[[i]]], {i, propLoop}];
		If[propLoop === {}, 
			result = localDecs
			,
			result = followChainToEnd[graph, localDecs, #]& /@ propLoop;
			result = Transpose[result] // Flatten // DeleteDuplicates;
		];
		
		(* clean up any modifications made to the declarations in the scope of this function *)
		Do[
			If[KeyExistsQ[result[[i, "Interpretation"]], "CSSCustomPropertyDefinition"], 
				tmp = CSSUntokenize @ result[[i, "Interpretation", "CSSCustomPropertyDefinition", "Tokens"]];
				result[[i, "Interpretation", "CSSCustomPropertyDefinition", "Value"]] = tmp;
				result[[i, "Value"]] = tmp;
			]
			, 
			{i, Length[result]}];
		Do[
			If[ListQ[result[[i, "Condition"]]], 
				result[[i, "Condition"]] = 
					Replace[
						Thread[DeleteCases[result[[i, "Condition"]], None], Hold], 
						{
							Hold[{x___}] :> If[Length[{x}] > 1, Hold[And[x]], Hold[x]], 
							{} -> None}]]
			, 
			{i, Length[result]}];
		tmp = Select[result, KeyExistsQ[#Interpretation, "CSSCustomPropertyDefinition"]&];
		{result, <|
			"Property" -> #["Interpretation", "CSSCustomPropertyDefinition", "Name"], 
			"Value" -> #["Interpretation", "CSSCustomPropertyDefinition", "Value"], 
			"Condition" -> #Condition|> & /@ tmp}
]

getAllVarReplacementRules[CSSDataSubset_] :=
	Module[{customPropertyDefinitions},
		customPropertyDefinitions = Flatten[CSSDataSubset[[All, "Block"]]][[All, "Interpretation"]];
		customPropertyDefinitions = Pick[customPropertyDefinitions, (AssociationQ[#] && KeyExistsQ[#, "CSSCustomPropertyDefinition"])& /@ customPropertyDefinitions];
		(#Name -> #Value)& /@ Flatten @ Values @ customPropertyDefinitions
	]

followChainToEnd[currentGraph_Graph, currentDeclarations_, propPosition_] :=
	Module[{replaceParts, localDecs = currentDeclarations, replaceLoop, parts, graph = currentGraph, customProp, propLoop = {}, tokens, result = currentDeclarations},
		customProp = localDecs[[propPosition]]["Property"];
		
		(* 
			Find positions where we need to replace the custom prop with new values.
			Each declaration may call the same var() function multiple times, so group them under the same declaration index. *)
		replaceParts = 
			GroupBy[
				Position[
					localDecs, 
					Condition[
						CSSToken[KeyValuePattern[{"Type" -> "function", "String" -> s_?StringQ /; StringMatchQ[s, "var", IgnoreCase -> True], "Children" -> c_}]],
						Position[c, CSSToken[KeyValuePattern[{"Type" -> "ident", "String" -> customProp}]]] =!= {}]], 
				First -> Rest];
		
		(* 
			Only continue down the chain if there's a valid substitution to make.
			If at any time we detect that no more var() functions appear in the property value, reduce all other calc() and attr() functions. *)
		If[replaceParts =!= <||>,
			(* reduce property value if no var() functions are found in its value *)
			localDecs[[propPosition]] = reduceDeclarationWithoutVar[localDecs[[propPosition]]];
			
			replaceLoop = Keys[replaceParts];
			tokens = localDecs[[propPosition, "Interpretation", "CSSCustomPropertyDefinition", "Tokens"]];
			
			(* every substitution location gets replaced by the same token sequence *)
			parts = 
				Table[
					reduceDeclarationWithoutVar[
						ReplacePart[localDecs[[replaceLoopIndex]],
							Join[
								{{Key["Condition"]} -> combineConditions[localDecs[[replaceLoopIndex, "Condition"]], localDecs[[propPosition, "Condition"]]]},
								Thread[replaceParts[replaceLoopIndex] -> Unevaluated @ Sequence @@ tokens]]
						] //. HoldPattern[{a___, Sequence[b__], c___}] :> {a, b, c}]
					,
					{replaceLoopIndex, replaceLoop}];
			localDecs = Flatten @ ReplacePart[localDecs, Thread[replaceLoop -> parts]];
			
			(* move backwards along the custom prop replacement graph *)
			graph = VertexDelete[graph, customProp];
			customProp = First[Pick[VertexList[graph], 0 === #& /@ VertexOutDegree[graph]], None];
			propLoop = Flatten @ Position[localDecs, KeyValuePattern[{"Property" -> customProp}], {1}];
			Do[localDecs[[i]] = reduceDeclarationWithoutVar[localDecs[[i]]], {i, propLoop}];
			
			(* recursively continue this process until reaching the end of the chain *)
			result = followChainToEnd[graph, localDecs, #]& /@ propLoop;
			result = Transpose[result] // Flatten // DeleteDuplicates;
		];
		result
	]
	
reduceDeclarationWithoutVar[dec_?AssociationQ] :=
	Module[{d = dec, funcPositions, token, try},
		If[Position[d, t_?CSSTokenQ /; TokenStringIs["var", t]] === {},
			funcPositions = Position[d, t_?CSSTokenQ /; TokenStringIs["attr" | "calc", t]];
			Do[
				token = Extract[d, funcPos];
				try = Catch @ 
					Which[
						TokenStringIs["calc", token], CSSTools`CSSValuesAndUnits3`Private`replaceCalcFunctionsWithTokens[{token}, "prop"],
						TokenStringIs["attr", token], CSSTools`CSSValuesAndUnits3`Private`replaceAttrFunctionsWithTokens[{token}, d["Element"], d["CustomPropertyDefinition", "Namespaces"]],
						True,                         Failure["BadParse", <|"MessageTemplate" -> "Could not find expected function token."|>]
					];
				If[FailureQ[try], 
					d["Interpretation"] = try
					,
					d = ReplacePart[d, funcPos -> Unevaluated[Sequence @@ try]] //. HoldPattern[{a___, Sequence[b__], c___}] :> {a, b, c}
				]
				,
				{funcPos, funcPositions}]
		];
		d
	]

combineConditions[cOld_, cAdd_] := DeleteDuplicates @ Sort @ Flatten @ {cOld, cAdd}


End[] (* End Private Context *)

EndPackage[]