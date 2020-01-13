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
				Condition[consumeProperty[prop_String, {}],	StringQ[prop] && StringStartsQ[prop, "--"]]
			] :>
				<|"CSSCustomPropertyDefinition" -> <|
					"Name"  -> prop, 
					"Value" -> Nothing|>|>,
					
			(* normal custom property definition *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}], StringQ[prop] && StringStartsQ[prop, "--"]]
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
						]|>|>,
						
			(* any use of var() must be resolved at compute time (CSSInheritance) unless a parse error is detected *)
			HoldPattern[
				Condition[consumeProperty[prop_String, tokens:{__?CSSTokenQ}], !FreeQ[tokens, TokenPatternString["var", "function"]]]
			] :>
				Module[{varCheck, failPosition},
					varCheck = parseVarFunctionToken[#, {}]& /@ Extract[tokens, Position[tokens, TokenPatternString["var", "function"]]];
					failPosition = FirstPosition[Replace[varCheck, x_Failure :> x["MessageParameters"]["Type"], 1], "Parse", {}, 1];
					If[failPosition =!= {},
						Extract[varCheck, failPosition]
						,
						<|"CSSResolveValueAtComputeTime" -> <|
							"String" -> CSSUntokenize[tokens],
							"Property" -> prop|>|>
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

(* check for recursive definitions; return cycles/self-loops if any found *)
customPropertyDefinitionCycles[replacements:{Rule[_?StringQ, _?StringQ]...}] :=
	Module[{reps, newRules, graph},
		(* find all var() token instances in the custom property replacement rules; get the names; ignore Failures as these should be removed during other parsing *)
		reps = CSSTokenize /@ replacements[[All, 2]];
		newRules = Map[getCustomPropertyNameFromVarFunction, Extract[#, Position[#, TokenPatternString["var", "function"]]]]& /@ reps;
		newRules = 
			With[{choices = FreeQ[#, Failure]& /@ newRules},
				Flatten[Thread /@ Thread[Rule[Pick[replacements[[All, 1]], choices], Pick[newRules, choices]]]]];
				
		(* make a graph and check for looping structures *)
		With[{g = Graph[newRules]}, Join[(*cycles*)FindCycle[g, Infinity, All], (*self loops*)EdgeList[g, _?(#[[1]] === #[[2]]&)]]]
	]

replaceVarFunctionWithTokens[tokensInput:{__?CSSTokenQ}, replacements_] :=
	Module[{cycles, tokens = tokensInput, varPosition, varToken, varCheck, name, failPosition},
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
		tokens
	]
	
replaceVarFunctionsInDeclarationList[declarationsInput_?ListQ] :=
	Module[{customPropertyDefinitions, itemsToResolve, declarations = declarationsInput},
		customPropertyDefinitions = (#Name -> #Value)& /@ DeleteMissing[declarations[[All, "Interpretation", "CSSCustomPropertyDefinition"]]];
		itemsToResolve = Flatten @ Position[declarations[[All, "Interpretation", "CSSResolveValueAtComputeTime"]], _Association?AssociationQ, 1];
		Do[
			declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]] = 
				CSSUntokenize @ 
					replaceVarFunctionWithTokens[
							CSSTokenize @ declarations[[i, "Interpretation", "CSSResolveValueAtComputeTime", "String"]], 
							customPropertyDefinitions];
			,
			{i, itemsToResolve}];
		declarations
		
	]

	



End[] (* End Private Context *)

EndPackage[]