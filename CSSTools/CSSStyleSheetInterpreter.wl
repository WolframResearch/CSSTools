(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["CSSTools`CSSStyleSheetInterpreter`", {"CSSTools`"}];

consumeDeclaration;
consumeAtPageRule;
consumeAtPageBlock;
consumeMediaQuery;
convertMarginsToPrintingOptions;
notebookLevelOptions;
validCSSBlockFullQ;
assemble;
assembleByFEOption;
$Debug;
inheritedPropertyRules;

(* CSSTools`
	---> defines wrappers like CSSHeightMax *)
(* Selectors3` 
	---> defines CSSSelector function, consumeCSSSelector, convertXMLPositionToCSSTarget *)
(* CSSTokenizer`
	---> various tokenizer functions e.g. CSSTokenQ. TokenTypeIs
	---> token position modifiers e.g. AdvancePosAndSkipWhitespace *)
(* CSSPropertyInterpreter` 
	---> defines consumeProperty and CSSPropertyData *)

Needs["CSSTools`CSSTokenizer`"];   
Needs["CSSTools`CSSSelectors3`"];
Needs["CSSTools`CSSPropertyInterpreter`"];


Begin["`Private`"];


(* ::Section:: *)
(*Notes*)


(* ::Subsection::Closed:: *)
(*Outline*)


(* ::Text:: *)
(*Purpose: Import Cascading Style Sheet (.css) files, interpreting CSS styles as Wolfram Desktop options.*)
(*Approach:*)
(*	1. import CSS file as a string*)
(*	2. tokenize following the CSS grammar specification*)
(*	3. parse token sequences into available Wolfram Desktop options *)
(*Notes: *)
(*Step (1) is generally fast and assumes readable characters.*)
(*Step (2) uses a single-pass StringSplit. Comments are removed.*)
(*The main bottleneck is step (3) due to the large amount of interpretation necessary of the token sequences. The basic "data types" i.e. length, color, percentage etc. are cached to improve import speed. We justify the caching because websites often stick with particular color schemes and layouts which results in a large amount of reusing colors, styles and lengths. *)


(* ::Section:: *)
(*Consume Token Sequences*)


(* ::Subsection::Closed:: *)
(*Notes*)


(* 
	It is assumed that the string input has already been tokenized into CSS tokens.
	The main token consumers have the HoldFirst attribute. 
	This allows the position variable to be tracked continuously through each token consumer.
	Some token consumers also advance the position.
	
	The tokenizer and token accessor functions are defined in CSSTools`CSSTokenizer.
	<<token>>["Type"]       canonical token type e.g. "ident", "dimension", etc.
	<<token>>["String"]     original unaltered string
	<<token>>["Value"]      dimension value (already an interpreted number)
	<<token>>["Unit"]       canonical dimension unit i.e. lower case and escape sequences are translated e.g. "px"
	
	Function TokenTypeIs does not ignore case as the token types should already be canonicalized.
	Function TokenStringIs uses the canonical string for comparison and ignores case.
*)


(* ::Subsection::Closed:: *)
(*Consume Style Sheet*)


consumeStyleSheet[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], imports = {}, namespaces = {}, rulesets},
		If[TrueQ @ $Debug, Echo[l, "Token Length"]];
		
		(* skip any leading whitespace (there shouldn't be any if @charset exists) *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		If[TrueQ @ $Debug, Echo[pos, "position"]];
		
		(* check for @charset rule *)
		If[TokenTypeIs["at-keyword", tokens[[pos]]] && TokenStringIs["charset", tokens[[pos]]], consumeAtCharsetKeyword[pos, l, tokens]];
		If[TrueQ @ $Debug, Echo[pos, "position after @charset check"]];
		
		(* check for @import rules *)
		While[TokenTypeIs["at-keyword", tokens[[pos]]] && TokenStringIs["import", tokens[[pos]]], 
			AppendTo[imports, consumeAtImportKeyword[pos, l, tokens]];
		];
		imports = Join @@ imports;
		If[TrueQ @ $Debug, Echo[pos, "position after @import check"]];
		
		(* check for @namespace rules *)
		(* These must appear after the @charset and @import rules and before rule sets *)
		While[TokenTypeIs["at-keyword", tokens[[pos]]] && TokenStringIs["namespace", tokens[[pos]]], 
			AppendTo[namespaces, consumeAtNamespaceKeyword[pos, l, tokens]]
		];
		If[TrueQ @ $Debug, Echo[namespaces, "detected namespaces"]];
		If[AnyTrue[namespaces, FailureQ], Return @ FirstCase[namespaces, _Failure, Failure["BadNamespace", <||>]]];
		(* Having duplicate default namespaces or dupliate prefixes is nonconforming, but not an error. Remove them. *)
		namespaces = Reverse @ DeleteDuplicatesBy[Reverse @ namespaces, #Default&];
		namespaces = Reverse @ DeleteDuplicatesBy[Reverse @ namespaces, #Prefix&];
		
		(* consume rulesets *)
		rulesets = Flatten @ consumeRulesets[pos, l, tokens, namespaces];
		
		(* combine all stylesheets *)
		Join[imports, rulesets]
	]

SetAttributes[consumeRulesets, HoldFirst];
consumeRulesets[pos_, l_, tokens_, namespaces_, allowAtRule_:True] :=
	Module[{lRulesets, rulesets, i = 1},
		lRulesets = Count[tokens, CSSToken[KeyValuePattern["Type" -> "{}"]], {1}]; (* upper bound of possible rulesets *)
		If[TrueQ @ $Debug, Echo[lRulesets, "number of rule sets in block"]];
		rulesets = ConstantArray[0, lRulesets]; (* container for processed rulesets *)
		If[TrueQ @ $Debug, Echo[tokens, "tokens in declaration block"]];
		If[lRulesets == 0,
			Return @ {}
			,
			While[pos < l,
				If[TrueQ @ $Debug, Echo[pos, "position before rule"]];
				Which[
					(* any at-rule *)
					allowAtRule && TokenTypeIs["at-keyword", tokens[[pos]]], 
						If[TrueQ @ $Debug, Echo[tokens[[pos]], "consuming at rule"]]; 
						rulesets[[i]] = consumeAtRule[pos, l, tokens, namespaces];
						i++,
					
					(* bad ruleset: missing a selector *)
					TokenTypeIs["{}", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens], 
					
					(* anything else treated as a ruleset *)
					True, rulesets[[i]] = consumeRuleset[pos, l, tokens, namespaces]; i++;
				];
			];
		];
		DeleteCases[rulesets, 0, {1}]
	]


(* ::Subsection::Closed:: *)
(*Consume Style Sheet Preambles (charset, import, namespace)*)


SetAttributes[{consumeAtCharsetKeyword, consumeAtImportKeyword, consumeAtNamespaceKeyword}, HoldFirst];

(* The character set is assumed UTF-8 and any charset is ignored. *)
consumeAtCharsetKeyword[pos_, l_, tokens_] :=
	Module[{},
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot["charset", tokens[[pos]]],
			Echo[Row[{"Expected @charset keyword. Had instead ", tokens[[pos]]}], "@charset error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ Null;
		];
		pos++;
		If[TokenTypeIsNot["whitespace", tokens[[pos]]], 
			AdvancePosToNextSemicolon[pos, l, tokens]
			,
			pos++;
			If[TokenTypeIsNot["string", tokens[[pos]]], 
				AdvancePosToNextSemicolon[pos, l, tokens]
				,
				pos++;
				If[TokenTypeIsNot["semicolon", tokens[[pos]]], 
					AdvancePosToNextSemicolon[pos, l, tokens]
					,
					pos++]]];
			
		AdvancePosAndSkipWhitespace[pos, l, tokens];
	]; 


consumeAtImportKeyword[pos_, l_, tokens_] :=  
	Module[{path, mediums, mediaStart, data},
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot["import", tokens[[pos]]],
			Echo[Row[{"Expected @import keyword. Had instead ", tokens[[pos]]}], "@import error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; Return @ {};
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		(* next token must be URL or string path to file *)
		If[TokenTypeIsNot["url" | "string", tokens[[pos]]],
			Echo["Expected URL not found.", "@import error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; Return @ {};
		];
		path = tokens[[pos]]["String"];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; 	
		If[TrueQ @ $Debug, Echo[pos, "position before @import media check"]];
		
		(* anything else is a comma-delimited set of media queries *)
		mediums = {};
		While[TokenTypeIsNot["semicolon", tokens[[pos]]],
			mediaStart = pos;
			AdvancePosToNextSemicolonOrComma[pos, l, tokens];
			If[pos == l, Echo["Media query has no closing. Reached EOF.", "@import error"]; Return @ {}];
			AppendTo[mediums, tokens[[mediaStart ;; pos - 1]]];
			If[TokenTypeIs["semicolon", tokens[[pos]]],
				(* break out of media loop*)
				Break[] 
				, 
				(* skip comma only *)
				AdvancePosAndSkipWhitespace[pos, l, tokens] 
			]
		];
		mediums = consumeMediaQuery /@ mediums; 
		If[AnyTrue[mediums, _?FailureQ], Return @ {}];
		(* a media query list is true of any component is true, and false only if all are false, so combine into Or *)
		mediums = Thread[Or @@ mediums, Hold];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* skip semicolon *)
				
		(* import without interpretation *)
		data = 
			With[{loc = FindFile[path]},
				If[FailureQ[loc], 
					Import[Echo[FileNameJoin[{Directory[], path}], "@import"], "Text"]
					,
					Import[loc, "Text"]
				]
			];
		If[FailureQ[data],
			Return @ {}
			, 
			data = consumeStyleSheet @ CSSTokenize @ data;
			If[mediums =!= None, data[[All, "Block", All, "Condition"]] = mediums];
			Return @ data
		]
	]


consumeAtNamespaceKeyword[pos_, l_, tokens_] :=
	Module[{prefix, namespace, default = False},
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot["namespace", tokens[[pos]]],
			Return @ Failure["BadNamespace", <|"Message" -> "Expected @namespace keyword. Had instead " <> tokens[[pos]]|>]; (* bad stylesheet *)
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		(* ident token after @namespace is optional. If missing, the declared namespace is the default namespace. *)
		If[TokenTypeIs["ident", tokens[[pos]]], 
			prefix = tokens[[pos]]["String"]; (* case-sensitive *)
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			prefix = None; default = True;
		];
		(* next token must be a string or URI*)
		Switch[tokens[[pos]]["Type"],
			"string", namespace = tokens[[pos]]["String"],
			"url",    namespace = tokens[[pos]]["String"],
			_,        Return @ Failure["BadNamespace", <|"Message" -> "Namespace declaration " <> tokens[[pos]]["String"] <> " is an incorrect format."|>]; (* bad stylesheet *)
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		(* token sequence must close with a semi-colon *)
		If[TokenTypeIsNot["semicolon", tokens[[pos]]] || TokenStringIsNot[";", tokens[[pos]]], 
			Return @ Failure["BadNamespace", <|"Message" -> "Namespace declaration has missing semicolon."|>]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		<|"Prefix" -> prefix, "Namespace" -> namespace, "Default" -> default|>
	]; 


(* ::Subsection::Closed:: *)
(*Consume Style Sheet Body (@rule, ruleset)*)


(* ::Subsubsection::Closed:: *)
(*main*)


SetAttributes[{consumeAtRule, consumeRuleset, consumeAtPageRule, consumeAtMediaRule}, HoldFirst];

consumeAtRule[pos_, l_, tokens_, namespaces_] :=
	Which[
		(* @import is not allowed after the top of the stylesheet, so skip them *)
		TokenStringIs["import", tokens[[pos]]], 
			AdvancePosToNextSemicolon[pos, l, tokens]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			{}, 
			
		(* @page *)
		TokenStringIs["page", tokens[[pos]]], consumeAtPageRule[pos, l, tokens, namespaces],
			
		(* @media *)
		TokenStringIs["media", tokens[[pos]]], 
			If[TrueQ @ $Debug, Echo["consuming at-media rule"]];
			consumeAtMediaRule[pos, l, tokens, namespaces],
			
		(* unrecognized @rule *)
		True, 
			AdvancePosToNextSemicolonOrBlock[pos, l, tokens]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			{}
	] 


(* ::Subsubsection::Closed:: *)
(*@media*)


consumeAtMediaRule[pos_, l_, tokens_, namespaces_] := 
	Module[{queries, values, queryStart},
		Which[
			(* Media queries are used in a number of places so don't re-check their validity. Instead skip any @media sequence. *)
			TokenTypeIs["at-keyword", tokens[[pos]]] && TokenStringIs["media", tokens[[pos]]], 
				AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			(* if no @media sequence then skip possible whitespace *)
			TokenTypeIs["whitespace", tokens[[pos]]],
				AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			True, Null
		];
		(* medias can be a comma-separated list *)
		queryStart = pos; AdvancePosToNextBlock[pos, l, tokens];
		queries = 
			DeleteCases[
				SplitBy[tokens[[queryStart ;; pos - 1]], MatchQ[CSSToken[KeyValuePattern["Type" -> "comma"]]]], 
				{CSSToken[KeyValuePattern["Type" -> "comma"]]}];
		queries = consumeMediaQuery /@ queries;
		If[AnyTrue[queries, _?FailureQ], Return @ FirstCase[queries, _?FailureQ]];
		(* a media query list is true if any component is true, and false only if all are false, so combine into Or *)
		queries = Thread[Or @@ queries, Hold];
		
		If[TokenTypeIsNot["{}", tokens[[pos]]], Return @ Failure["BadMedia", <|"Message" -> "Expected @media block."|>]];
		values = consumeAtMediaBlock[tokens[[pos]]["Children"], namespaces];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		values[[All, "Block", All, "Condition"]] = queries;
		values
	]
	
consumeMediaQuery[tokens:{___?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens]},
		(* trim whitespace tokens from ends *)
		pos = l; If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]]; l = pos;
		pos = 1; If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		(* first token should be the media type *)
		Switch[tokens[[pos]]["Type"],
			"ident", 
				Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
					"all",        None,
					"braille",    Missing["Not supported."],
					"embossed",   Missing["Not supported."],
					"handheld",   None,
					"print",      ScreenStyleEnvironment -> "Printout",
					"projection", None,
					"screen",     None,
					"speech",     Missing["Not supported."],
					"tty",        Missing["Not supported."],
					"tv",         None,
					_,            Missing["Not supported."] (* unknown query type *)
				],
			_, Failure["BadMedia", <|"Message" -> "Expected ident token in media query."|>]
		]
		
		(* in future, media conditions follow immediately after the media type, if any *)
	]
	
consumeAtMediaBlock[tokens:{___?CSSTokenQ}, namespaces_] :=
	Module[{pos = 1, l = Length[tokens]},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		(* consume rulesets, but do not allow additional @rules *)
		consumeRulesets[pos, l, tokens, namespaces, False (* other @rules are not allowed *)]
	]
	



(* ::Subsubsection::Closed:: *)
(*@page*)


consumeAtPageRule[pos_, l_, tokens_, namespaces_] := 
	Module[{pageSelectors, block},
		(* check for valid start of @page token sequence *)
		If[TokenTypeIsNot["at-keyword", tokens[[pos]]] || TokenStringIsNot["page", tokens[[pos]]],
			Echo[Row[{"Expected @page keyword instead of ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
			,
			AdvancePosAndSkipWhitespace[pos, l, tokens]
		];
		
		(* consume optional page selector :left, :right, or :first *)
		pageSelectors =
			If[TokenTypeIsNot["{}", tokens[[pos]]], 
				If[TokenTypeIsNot["colon", tokens[[pos]]], 
					Echo[Row[{"Expected @page pseudopage instead of ", tokens[[pos]]}], "@page error"];
					AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
					Return @ {}
				];
				pos++; (* skip colon *)
				If[TokenTypeIs["ident", tokens[[pos]]],
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"left",  Left,
						"right", Right,
						"first", Missing["Not supported."],
						_,       Echo[Row[{"Expected @page pseudopage instead of ", tokens[[pos]]}], "@page error"]; $Failed
					] 
				];
				,
				All
			];
		If[FailureQ[pageSelectors] || MissingQ[pageSelectors], Return @ {}];
		If[TokenTypeIsNot["{}", tokens[[pos]]], 
			Echo[Row[{"Expected @page block instead of ", tokens[[pos]]}], "@page error"];
			AdvancePosToNextBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ {}
		];
		
		(* consume @page block *)
		block = consumeAtPageBlock[tokens[[pos]]["Children"], pageSelectors, namespaces];
		block[[All, "Condition"]] = Hold[CurrentValue[InputNotebook[], ScreenStyleEnvironment] === "Printout"];
		<|
			"Selector" -> "@page",
			"Block"    -> block|>	
	]


(* The @page {...} block contains only margin rules; CSS 2.1 does not allow specifying page size *)
consumeAtPageBlock[tokens:{___?CSSTokenQ}, scope_, namespaces_] :=
	Module[{pos = 1, l = Length[tokens], dec, decStart, decEnd, declarations = {}},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		
		While[pos <= l,
			If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["margin" | "margin-top" | "margin-bottom" | "margin-left" | "margin-right", tokens[[pos]]],
				decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
				dec = consumeDeclaration[tokens[[decStart ;; decEnd]], namespaces];
				If[!FailureQ[dec], 
					dec = convertMarginsToPrintingOptions[dec, scope];
					AppendTo[declarations, dec]
				];
				pos = decEnd; AdvancePosAndSkipWhitespace[pos, l, tokens];
				,
				(* unrecognized rules are skipped *)
				AdvancePosToNextSemicolonOrBlock[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens];
			]
		];
		declarations
	]


convertMarginsToPrintingOptions[declaration_?AssociationQ, scope_] :=
	Module[{value},
		If[
			Or[
				!KeyExistsQ[declaration, "Interpretation"],
				!KeyExistsQ[declaration["Interpretation"], PrintingOptions], 
				!KeyExistsQ[declaration["Interpretation", PrintingOptions, "PrintingMargins"]]],
			Return @ declaration
		];
		value = declaration["Interpretation", PrintingOptions, "PrintingMargins"];
		(* CSS 2.1 does not allow ex or em lengths *)
		If[!FreeQ[value, FontSize | "FontXHeight"], Return @ Failure["BadLength", <|"Message" -> "Page margins cannot us 'em' or 'ex' units."|>]];
		value = 
			KeyValueMap[
				Rule[
					#1,
					Which[
						MatchQ[#1, "Left" | "Right"], 
							Replace[#2, Scaled[x_] :> Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 1}]]],
						MatchQ[#1, "Top" | "Bottom"],
							Replace[#2, Scaled[x_] :> Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 2}]]],
						True,
							#2
					]
				]&,
				value
			];
		value = <|value|>;
		<|
			declaration, 
			"Interpretation" -> <|
				PrintingOptions -> <|
					Which[
						MatchQ[scope, Left | Right], <|"InnerOuterMargins" -> DeleteMissing @ {value["Left"], value["Right"]}|>,
						MatchQ[scope, First],        Missing["Not supported."],
						True,                        "PrintingMargins" -> <|value|>
					]|>|>|>
	]


(* ::Subsubsection::Closed:: *)
(*ruleset*)


consumeRuleset[pos_, l_, tokens_, namespaces_] :=
	Module[{selectorStartPos = pos, ruleset},
		AdvancePosToNextBlock[pos, l, tokens];
		If[TrueQ @ $Debug, Echo[{pos, tokens}, "pos + tokens"]];
		If[TrueQ @ $Debug, Echo[namespaces, "namespaces"]];
		ruleset = 
			<|
				"Selector" -> consumeCSSSelector[tokens[[selectorStartPos ;; pos - 1]], namespaces], 
				(* The block token is already encapsulated CSSToken[<|"Type" -> {}, "Children" -> {CSSTokens...}|>] *)
				"Block" -> consumeDeclarationBlock[If[Length[tokens[[pos]]["Children"]] > 1, tokens[[pos]]["Children"], {}], namespaces]|>; 
		(* return the formatted ruleset, but first make sure to skip the block *)
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		ruleset
	]


consumeDeclarationBlock[{}, _] := {} 

consumeDeclarationBlock[blockTokens:{__?CSSTokenQ}, namespaces_] :=
	Module[{blockPos = 1, blockLength = Length[blockTokens], lDeclarations, i = 1, decStart, dec, validDeclarations},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", blockTokens[[blockPos]]], AdvancePosAndSkipWhitespace[blockPos, blockLength, blockTokens]]; 
		
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[blockTokens, CSSToken[KeyValuePattern["Type" -> "colon"]]];
		validDeclarations = ConstantArray[0, lDeclarations];
		While[blockPos < blockLength && i <= lDeclarations,
			decStart = blockPos; AdvancePosToNextSemicolon[blockPos, blockLength, blockTokens];
			dec = consumeDeclaration[blockTokens[[decStart ;; blockPos]], namespaces];
			If[!FailureQ[dec], validDeclarations[[i++]] = dec];
			(* skip over semi-colon *)
			AdvancePosAndSkipWhitespace[blockPos, blockLength, blockTokens]
		];					
		(* remove possible excess declarations *)
		DeleteCases[validDeclarations, 0, {1}]
	]


(* a declaration is "prop:val" or "prop:val !important" with optional semicolon if it is the last declaration *)
consumeDeclaration[decTokens:{__?CSSTokenQ}, namespaces_] :=
	Module[{decPos = 1, decLength = Length[decTokens], propertyPosition, valuePosition, endPosition, important = False, declaration},
		(* check for bad property *)
		If[TokenTypeIsNot["ident", decTokens[[decPos]]], Return @ $Failed];
		propertyPosition = decPos; AdvancePosAndSkipWhitespace[decPos, decLength, decTokens];
		
		(* check for EOF or missing colon *)
		If[decPos >= decLength || TokenTypeIsNot["colon", decTokens[[decPos]]], Return @ $Failed];
		AdvancePosAndSkipWhitespace[decPos, decLength, decTokens]; 
		valuePosition = decPos;
		
		(* remove trailing whitespace and possible trailing semi-colon*)
		decPos = decLength;
		If[TrueQ @ $Debug, Echo[decTokens // Column, "dec tokens"]; Echo[decPos, "pos"]];
		If[TokenTypeIs["semicolon", decTokens[[decPos]]], 
			RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]
			,
			While[decPos > 1 && TokenTypeIs["whitespace", decTokens[[decPos]]], decPos--];
			If[TokenTypeIs["semicolon", decTokens[[decPos]]], RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]];
		];
		
		(* check for !important token sequence *)
		endPosition = decPos;
		If[TokenTypeIs["ident", decTokens[[decPos]]] && TokenStringIs["important", decTokens[[decPos]]], 
			RetreatPosAndSkipWhitespace[decPos, decLength, decTokens];
			If[TokenTypeIs["delim", decTokens[[decPos]]] && TokenStringIs["!", decTokens[[decPos]]], 
				important = True; RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]
				,
				(* else leave all trailing tokens alone *)
				decPos = endPosition;
			]
		];
		
		declaration =
			With[
				{
					prop = decTokens[[propertyPosition]]["String"],
					(*check for empty property*)
					valueTokens = If[decPos < valuePosition, {}, decTokens[[valuePosition ;; decPos]]]
				},
				<|
					"Property"       -> prop,
					"Value"          -> CSSUntokenize @ valueTokens,
					"Important"      -> important,
					"Interpretation" -> valueTokens,
					"Condition"      -> None|>
			];
		If[TrueQ @ $RawImport, 
			KeyDropFrom[declaration, "Interpretation"]
			,
			AssociateTo[
				declaration, 
				"Interpretation" -> 
					With[{p = declaration["Property"]},
						consumeProperty[
							If[StringStartsQ[p, "--"], p, CSSNormalizeEscapes @ ToLowerCase @ p], 
							declaration["Interpretation"], 
							"Namespaces" -> namespaces]]]
		]		
	]


(* ::Section:: *)
(*Merge Properties*)


(* ::Subsection::Closed:: *)
(*Valid boxes, options, and expressions for merging*)


expectedMainKeys      = {"Selector", "Block"};
expectedMainKeysFull  = {"Selector", "Targets", "Block"};
expectedBlockKeys     = {"Property", "Value", "Important", "Condition"};
expectedBlockKeysFull = {"Property", "Value", "Important", "Interpretation", "Condition"};


validCSSBlockQ[{}(*block can be empty*)] := True
validCSSBlockQ[data:{__Association}]     := AllTrue[Keys /@ data, (Sort[expectedBlockKeys]===Sort[#])&]
validCSSBlockFullQ[{}]                   := True
validCSSBlockFullQ[data:{__Association}] := AllTrue[Keys /@ data, (Sort[expectedBlockKeysFull]===Sort[#])&]

validCSSDataRawQ[data:{__Association}]  := And[AllTrue[Keys /@ data, (Sort[expectedMainKeys]===Sort[#])&],     validCSSBlockQ[Flatten @ data[[All, "Block"]]]]
validCSSDataBareQ[data:{__Association}] := And[AllTrue[Keys /@ data, (Sort[expectedMainKeys]===Sort[#])&],	    validCSSBlockFullQ[Flatten @ data[[All, "Block"]]]]
validCSSDataFullQ[data:{__Association}] := And[AllTrue[Keys /@ data, (Sort[expectedMainKeysFull]===Sort[#])&],	validCSSBlockFullQ[Flatten @ data[[All, "Block"]]]]

validCSSDataQ[data:{__Association}] := validCSSDataBareQ[data] || validCSSDataFullQ[data]
validCSSDataQ[___] := False


(* these include all inheritable options that make sense to pass on in a Notebook environment *)
notebookLevelOptions = 
	{
		Background, BackgroundAppearance, BackgroundAppearanceOptions, 
		FontColor, FontFamily, FontSize, FontSlant, FontTracking, FontVariations, FontWeight, 
		LineIndent, LineSpacing, ParagraphIndent, PrintingOptions, ShowContents, TextAlignment};
		
(* these include all options (some not inheritable in the CSS sense) that make sense to set at the Cell level *)
cellLevelOptions = 
	{
		Background, 
		CellBaseline, CellDingbat, CellMargins, 
		CellFrame, CellFrameColor, CellFrameLabelMargins, CellFrameLabels, CellFrameMargins, CellFrameStyle, 
		CellLabel, CellLabelMargins, CellLabelPositioning, CellLabelStyle, 
		CounterIncrements, CounterAssignments, DisplayFunction, (* used to hold cell content *)
		FontColor, FontFamily, FontSize, FontSlant, FontTracking, FontVariations, FontWeight, 
		LineIndent, LineSpacing, ParagraphIndent, ShowContents, TextAlignment,
		PageBreakBelow, PageBreakAbove, PageBreakWithin, GroupPageBreakWithin};
		
(* these are options that are expected to be Notebook or Cell specific *)
optionsToAvoidAtBoxLevel = 
	{
		BackgroundAppearance, BackgroundAppearanceOptions, 
		CellBaseline, CellDingbat, CellMargins, 
		CellFrame, CellFrameColor, CellFrameLabelMargins, CellFrameLabels, CellFrameMargins, CellFrameStyle, 
		CellLabel, CellLabelMargins, CellLabelPositioning, CellLabelStyle, 
		ParagraphIndent};


validBoxes =
	{
		ActionMenuBox, AnimatorBox, ButtonBox, CheckboxBox, ColorSetterBox, 
		DynamicBox, DynamicWrapperBox, FrameBox, Graphics3DBox, GraphicsBox, 
		GridBox, InputFieldBox, InsetBox, ItemBox, LocatorBox, 
		LocatorPaneBox, OpenerBox, OverlayBox, PaneBox, PanelBox, 
		PaneSelectorBox, PopupMenuBox, ProgressIndicatorBox, RadioButtonBox,
		SetterBox, Slider2DBox, SliderBox, TabViewBox, TemplateBox, TogglerBox, TooltipBox};	
validBoxGenerators =
	{
		ActionMenu, Animator, Button, Checkbox, ColorSetter, 
		Dynamic, DynamicWrapper, Frame, Graphics3D, Graphics, 
		Grid, InputField, Inset, Item, Locator, 
		LocatorPane, Opener, Overlay, Pane, Panel, 
		PaneSelector, PopupMenu, ProgressIndicator, RadioButton,
		Setter, Slider2D, Slider, TabView, TemplateBox, Toggler, Tooltip};	
validBoxOptions =
	{
		Alignment, Appearance, Background, DisplayFunction, Frame, FrameMargins, FrameStyle, 
		FontTracking, ImageMargins, ImageSize, ImageSizeAction, Spacings, Scrollbars};
validBoxesQ = With[{all = Join[validBoxes, validBoxGenerators]}, MemberQ[all, #]&];

movePseudoOptionsIntoBoxOptions[allOptions_, boxes:{__?validBoxesQ}] :=
	Module[{currentOpts, optNames = allOptions[[All, 1]]},
		Join[
			Cases[allOptions, Rule[Background, _] | Rule[FontTracking, _], {1}],
			DeleteCases[allOptions, Alternatives @@ (Rule[#, _]& /@ validBoxOptions)],
			DeleteCases[
				Table[
					currentOpts = Intersection[Options[i][[All, 1]], optNames];
					Symbol[SymbolName[i] <> "Options"] -> Cases[allOptions, Alternatives @@ (Rule[#, _]& /@ currentOpts), {1}],
					{i, boxes}],
				_ -> {}, 
				{1}]]	
	]	
	
filterOptionNames[scope_, CSSBlockData_] :=
	Module[{optionNames},
		optionNames = Union @ Flatten[Keys /@ CSSBlockData[[All, "Interpretation"]]];
		optionNames =
			Select[
				optionNames, 
				Switch[scope, 
					_?validBoxesQ,    !MemberQ[optionsToAvoidAtBoxLevel, #]&,
					{__?validBoxesQ}, !MemberQ[optionsToAvoidAtBoxLevel, #]&,
					Cell,              MemberQ[cellLevelOptions, #]&, 
					Notebook,          MemberQ[notebookLevelOptions, #]&,
					Box,              !MemberQ[optionsToAvoidAtBoxLevel, #]&,
					All | None,        True&]]
	]


(* ::Subsection::Closed:: *)
(*Assemble possibly conditioned options*)


(* These assembly functions are used within CSSCascade. *)

(* 
	Media queries add conditions to the normal CSS cascade. There may be multiple valid conditions. 
	Option values and their conditions are combined into a held Which expression. *)
assembleWithConditions[CSSBlockData_?validCSSBlockFullQ, option_, Hold[subKeys___], modifyValueFunction_Function] :=
	Module[{h = Hold[], j = 1, value, cond},
		While[j <= Length[CSSBlockData],
			value = CSSBlockData[[j]]["Interpretation", option, subKeys];
			value = modifyValueFunction[value, CSSBlockData[[j]]];
			cond = CSSBlockData[[j]]["Condition"];
			If[!MissingQ[value],
				If[cond === None, Break[]]; (* break if found fallthrough case *)
				With[{i = value, c = cond}, h = Replace[h, Hold[x___] :> Hold[x, c, i]]]
			];
			j++
		];
		(* set the fallthrough condition if reached end of list *)
		If[j > Length[CSSBlockData], value = Automatic];
		With[{i = value}, h = Replace[h, Hold[x___] :> Hold[x, True, i]]];
		(* remove any internal holds, add the head Which *)
		h = h //. Hold[a___, Hold[x___], b___] :> Hold[a, x, b];
		h = Replace[h, Hold[x___] :> Hold[Which[x]]];
		h = Replace[h, Hold[x___] :> x, {1}]; (* not sure if this is needed.... *)
		(* reduce to just the held default if only the default exists *)
		Replace[h, Hold[Which[True, x_]] :> Hold[x]]
	]

ClearAll[assemble];
Options[assemble] = {"PropertyIsCaseSensitive" -> False};

(* Assembling general CSS properties into FE options. This is the most general case. *)
assemble[prop_?StringQ, scope_, CSSBlockData_, opts:OptionsPattern[]] :=
	Module[{validBlockData, optionNames},
		(* only look at declarations that match 'prop' *)
		validBlockData = Select[CSSBlockData, StringMatchQ[#Property, prop, IgnoreCase -> Not[TrueQ @ OptionValue["PropertyIsCaseSensitive"]]]&];
		(* filter FE options in the remaining declaration interpretations to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		assembleByFEOption[#, validBlockData]& /@ optionNames
	]

(* 
	The FE options that take option values of the form {{left, right}, {bottom, top}} need to be assembled
	from each side's individual CSS cascade. The 'prop' input is assumed normalized to lowercase. 
	If media conditions (that contain Which) are detected, then the entire expression is wrapped in Dynamic. *)
assembleLRBT[prop_?StringQ, LRBTList_] := 
	Replace[LRBTList, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]


(* Assembling background properties into FE options:
	CSS background properties are often just using the shorthand 'background'. *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "background", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp, color, image, position, repeat},
		(* check whether input is a valid background long-form properties *)
		If[!StringMatchQ[prop, "background" | "background-attachment" | "background-color" | "background-image" | "background-position" | "background-repeat"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized background property.", "Prop" -> inputProp|>]];
			
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop' *)
		validBlockData =
			Which[
				StringMatchQ[prop, "background"], Select[CSSBlockData, StringStartsQ[#Property, "background"]&],
				True,                             Select[CSSBlockData, StringMatchQ[#Property, prop]&]
			];
		validBlockData = DeleteCases[validBlockData, _?(MissingQ[#Interpretation] || FailureQ[#Interpretation]&)];
		
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		
		If[StringMatchQ[prop, "background"], (* only case where multiple options are combined *)
			position = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "background-position", IgnoreCase -> True]&], System`BackgroundAppearanceOptions, Hold[], #&];
			repeat   = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "background-repeat",   IgnoreCase -> True]&], System`BackgroundAppearanceOptions, Hold[], #&];
			color    = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "background-color",    IgnoreCase -> True]&], Background, Hold[], #&];
			image    = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "background-image",    IgnoreCase -> True]&], System`BackgroundAppearance, Hold[], #&];
			{
				If[MatchQ[color, Hold[Automatic]], Nothing, Background -> If[FreeQ[color, Which], First @ color, Dynamic @@ color]],
				If[MatchQ[image, Hold[Automatic]], Nothing, System`BackgroundAppearance -> If[FreeQ[image, Which], First @ image, Dynamic @@ image]],
				Which[
					MatchQ[repeat, Hold[Automatic]] && MatchQ[position, Hold[Automatic]], 
						Nothing,
					MatchQ[repeat, Hold[Automatic]], 
						System`BackgroundAppearanceOptions -> If[FreeQ[position, Which], First @ position, Dynamic @@ position],
					MatchQ[position, Hold[Automatic]], 
						System`BackgroundAppearanceOptions -> If[FreeQ[repeat, Which], First @ repeat, Dynamic @@ repeat],
					(* both position and repeat become the same option. Merging them (if both specified) is too ambiguous so don't try *)
					True,
						Nothing
				]}
			,
			assembleByFEOption[#, Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&]]& /@ optionNames
		]
	]
	
	
(* Assembling border properties into FE options:
	CSS border properties are often shorthands for groups of more specific properties e.g. border and border-top also include border-top-style. 
	If performing the CSS cascade on a particular side, or the whole border, then these shorthand propreties need to be included as well 
	since they include the side of interest. *)

frameStyleOptionQ[input_] := MemberQ[{CellFrameStyle, FrameStyle}, input]
frameStyleOptionQ[___] := False

modifyValueToDirective = (* value modification function for assembleWithConditions of directives *)
	Function[{v, dummy},
		Module[{value = v},
			If[AssociationQ[value],
				If[KeyExistsQ[value, "Style"] && value["Style"] === None && Length[value] > 1, value = KeyDropFrom[value, "Style"]];
				Directive @@ Values @ value
				,
				value
			]
		]
	]
	
combineBorderDirectives[heldDirectives_] :=
	Module[{temp},
		temp = Replace[Thread[heldDirectives, Hold], Hold[{x___}] :> Hold[Directive[x]]];
		temp = DeleteCases[temp, Automatic, {2}];
		Which[
			MatchQ[temp, Hold[Directive[]]],               Hold[Automatic],
			MatchQ[temp, Hold[Directive[___, None, ___]]], Hold[None], 
			True,                                          temp
		]
	]

assemble[
	Condition[
		inputProp_?StringQ,
		And[
			StringStartsQ[StringTrim @ inputProp, "border", IgnoreCase -> True],
			!StringMatchQ[StringTrim @ inputProp, "border-spacing" | "border-collapse", IgnoreCase -> True]]], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, temp, color, style, width, index, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp},
		(* check whether input is a valid border long-form properties *)
		If[!StringMatchQ[prop, 
				"border" | "border-top" | "border-bottom" | "border-right" | "border-left" |
				"border-color" | "border-style" | "border-width" |
				"border-top-style"    | "border-top-width"    | "border-top-color" |
				"border-bottom-style" | "border-bottom-width" | "border-bottom-color" |
				"border-left-style"   | "border-left-width"   | "border-left-color" |
				"border-right-style"  | "border-right-width"  | "border-right-color"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized border property.", "Prop" -> inputProp|>]];
		
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'border' shorthand; include matched side shorthand; include any color/style/width shorthand *)
		validBlockData =
			Which[
				StringMatchQ[prop, "border"],        Select[CSSBlockData,  StringStartsQ[#Property, "border"]&],
				StringMatchQ[prop, "border-style"],  Select[CSSBlockData, (StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "style"])&],
				StringMatchQ[prop, "border-width"],  Select[CSSBlockData, (StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "width"])&],
				StringMatchQ[prop, "border-color"],  Select[CSSBlockData, (StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "color"])&],
				StringMatchQ[prop, "border-left"],   Select[CSSBlockData,  StringStartsQ[#Property, "border-left"]&],
				StringMatchQ[prop, "border-right"],  Select[CSSBlockData,  StringStartsQ[#Property, "border-right"]&],
				StringMatchQ[prop, "border-top"],    Select[CSSBlockData,  StringStartsQ[#Property, "border-top"]&],
				StringMatchQ[prop, "border-bottom"], Select[CSSBlockData,  StringStartsQ[#Property, "border-bottom"]&],
				True,                                Select[CSSBlockData,  StringMatchQ[#Property, prop]&]
			];
		
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
				
		temp =
		Function[{y},
			y -> 
				Which[
					validBlockData === {},
						Automatic,
					StringMatchQ[prop, "border"],        
						color = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-color", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						style = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-style", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						width = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-width", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						assembleLRBT[prop, combineBorderDirectives /@ Transpose[{color, style, width}]],
					StringMatchQ[prop, "border-style"],
						style = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-style", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						assembleLRBT[prop, style],
					StringMatchQ[prop, "border-width"],
						width = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-width", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						assembleLRBT[prop, width],
					StringMatchQ[prop, "border-color"],
						color = Function[{x}, assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "border-" <> x <> "-color", IgnoreCase -> True]&], y, Hold[], #&]] /@ {"left", "right", "bottom", "top"};
						assembleLRBT[prop, color],
					StringMatchQ[prop, "border-left" | "border-right" | "border-bottom" | "border-top" ],
						color = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop <> "-color", IgnoreCase -> True]&], y, Hold[], #&];
						style = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop <> "-style", IgnoreCase -> True]&], y, Hold[], #&];
						width = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop <> "-width", IgnoreCase -> True]&], y, Hold[], #&];
						index = Which[StringContainsQ[prop, "left"], 1, StringContainsQ[prop, "right"], 2, StringContainsQ[prop, "bottom"], 3, StringContainsQ[prop, "top"], 4]; 
						temp = {Hold[Inherited], Hold[Inherited], Hold[Inherited], Hold[Inherited]};
						temp[[index]] = combineBorderDirectives[{color, style, width}];
						assembleLRBT[prop, temp],
					True, 
						index = Which[StringContainsQ[prop, "left"], 1, StringContainsQ[prop, "right"], 2, StringContainsQ[prop, "bottom"], 3, StringContainsQ[prop, "top"], 4]; 
						temp = {Hold[Inherited], Hold[Inherited], Hold[Inherited], Hold[Inherited]};
						temp[[index]] = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&], y, Hold[], #&];
						assembleLRBT[prop, temp]
				]
		] /@ optionNames;
		
		(* remove Directive wrapper within any CellFrame instances *)
		temp /. HoldPattern[CellFrame -> x_] :> (CellFrame -> (x /. Directive[y___] :> y))
	]
	
	
(* Assembling font properties into FE options:
	CSS font properties are often just using the shorthand 'font'. *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "font", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp, family, size, style, variant, weight},
		(* check whether input is a valid font long-form properties *)
		If[!StringMatchQ[prop, "font" | "font-family" | "font-size" | "font-style" | "font-variant" | "font-weight"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized font property.", "Prop" -> inputProp|>]];
		
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop' *)
		validBlockData =
			Which[
				StringMatchQ[prop, "font"], Select[CSSBlockData, StringStartsQ[#Property, "font"]&],
				True,                       Select[CSSBlockData, StringMatchQ[#Property, prop]&]
			];
		validBlockData = DeleteCases[validBlockData, _?(MissingQ[#Interpretation] || FailureQ[#Interpretation]&)];
		
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		
		If[StringMatchQ[prop, "font"], (* only case where multiple options are combined *)
			If[optionNames === {}, Return @ {}];
			family  = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "font-family",  IgnoreCase -> True]&], FontFamily,     Hold[], #&];
			size    = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "font-size",    IgnoreCase -> True]&], FontSize,       Hold[], #&];
			style   = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "font-style",   IgnoreCase -> True]&], FontSlant,      Hold[], #&];
			variant = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "font-variant", IgnoreCase -> True]&], FontVariations, Hold["CapsType"], #&];
			weight  = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "font-weight",  IgnoreCase -> True]&], FontWeight,     Hold[], #&];
			Join[
				MapThread[
					If[MatchQ[#1, Hold[Automatic]], Nothing, #2 -> If[FreeQ[#1, Which], First @ #1, Dynamic @@ #1]]&, 
					{{family, size, style, weight}, {FontFamily, FontSize, FontSlant, FontWeight}}],
				If[variant === Hold[Automatic], 
					{}
					,
					{FontVariations -> <|"CapsType" -> If[FreeQ[variant, Which], First @ variant, Dynamic @@ variant]|>}
				]
			]
			,
			Function[{y},
				Which[
					StringMatchQ[prop, "font-variant"],
						With[{val = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&], y, Hold[], #&]},
							y -> <|"CapsType" -> If[FreeQ[val, Which], First @ val, Dynamic @@ val]|>
						],
					True,
						With[{val = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&], y, Hold[], #&]},
							y -> If[FreeQ[val, Which], First @ val, Dynamic @@ val]
						]
				]
			] /@ optionNames
		]
	]
	
	
(* Assembling list-style properties into FE options *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "list", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp, image, type},
		(* check whether input is a valid list-style long-form properties *)
		If[!StringMatchQ[prop, "list-style" | "list-style-image" | "list-style-position" | "list-style-type"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized ist-style property.", "Prop" -> inputProp|>]];
		
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop' *)
		validBlockData =
			Which[
				StringMatchQ[prop, "list-style"], Select[CSSBlockData, StringStartsQ[#Property, "list-style"]&],
				True,                             Select[CSSBlockData, StringMatchQ[#Property, prop]&]
			];
		validBlockData = DeleteCases[validBlockData, _?(MissingQ[#Interpretation] || FailureQ[#Interpretation]&)];
		
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		
		If[StringMatchQ[prop, "list-style"], (* only case where multiple options are combined; position is ignored because it can only be 'outside' in WD *)
			image = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "list-style-image", IgnoreCase -> True]&], CellDingbat, Hold[], #&];
			type  = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, "list-style-type",  IgnoreCase -> True]&], CellDingbat, Hold[], #&];
			{
				Which[
					(* both type and image become the same FE option. If image doesn't exist, fallback to type if type exists *)
					MatchQ[image, Hold[Automatic | None]] && MatchQ[type, Hold[Automatic | None]], 
						Nothing,
					!MatchQ[image, Hold[Automatic | None]], 
						CellDingbat -> If[FreeQ[image, Which], First @ image, Dynamic @@ image],
					MatchQ[image, Hold[Automatic | None]] && !MatchQ[type, Hold[Automatic | None]], 
						CellDingbat -> If[FreeQ[type, Which], First @ type, Dynamic @@ type],
					True,
						Nothing
				]}
			,
			assembleByFEOption[#, Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&]]& /@ optionNames
		]
	]
	

(* Assembling margin properties into FE options:
	CSS margin properties are often shorthands for groups of more specific properties e.g. margin also include margin-top. 
	If performing the CSS cascade on a particular side, or the whole border, then these shorthand propreties need to be included as well 
	since they include the side of interest. 
	The @page rule can also contain the 'margin' proprty. It translates to a different FE option and must be handled separately. *)
marginOptionQ[input_] := MemberQ[{CellFrameMargins, FrameMargins, ImageMargins, CellMargins}, input]
marginOptionQ[___] := False

assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "margin" | "padding", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, temp, index, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp, mainProp},
		(* check whether input is a valid margin/padding long-form properties *)
		mainProp = If[StringStartsQ[prop, "margin"], "margin", "padding"];
		If[!StringMatchQ[prop, Alternatives[#, # <> "-top", # <> "-bottom", # <> "-right", # <> "-left"]& @ mainProp], 
			Return @ Failure["BadProp", <|"MessageTemplate" -> "Unrecognized `main` property.", "MessageParameters" -> <|"main" -> mainProp|>, "Prop" -> prop|>]];
		
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop' *)
		validBlockData =
			Which[
				StringMatchQ[prop, mainProp], Select[CSSBlockData, StringStartsQ[#Property, mainProp]&],
				True,                         Select[CSSBlockData, StringMatchQ[#Property, prop]&]
			];
		validBlockData = DeleteCases[validBlockData, _?(MissingQ[#Interpretation] || FailureQ[#Interpretation]&)];
						
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		
		(* run the FE option assembly function *)
		Function[{y},
			y -> 
				Which[
					StringMatchQ[prop, mainProp],        
						temp = {
							assembleWithConditions[Select[validBlockData, StringEndsQ[#Property, "left"]&],   y, Hold[], #&],
							assembleWithConditions[Select[validBlockData, StringEndsQ[#Property, "right"]&],  y, Hold[], #&],
							assembleWithConditions[Select[validBlockData, StringEndsQ[#Property, "bottom"]&], y, Hold[], #&],
							assembleWithConditions[Select[validBlockData, StringEndsQ[#Property, "top"]&],    y, Hold[], #&]};
						assembleLRBT[prop, temp],
					True, 
						index = Which[StringContainsQ[prop, "left"], 1, StringContainsQ[prop, "right"], 2, StringContainsQ[prop, "bottom"], 3, StringContainsQ[prop, "top"], 4]; 
						temp = {Hold[Inherited], Hold[Inherited], Hold[Inherited], Hold[Inherited]};
						temp[[index]] = assembleWithConditions[Select[validBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&], y, Hold[], #&];
						assembleLRBT[prop, temp]
				]
		] /@ optionNames		
	]
	
	
(* Assembling height/width properties into FE options:
	CSS height/width properties can be overridden by min/max versions of height/width properties. 
	If performing the CSS cascade then these min/max propreties need to be included as well. *)
sizeOptionQ[input_] := MemberQ[{ImageSize}, input]
sizeOptionQ[___] := False		

assemble[
	inputProp_?StringQ /; StringEndsQ[StringTrim @ inputProp, "height" | "width", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_, 
	opts:OptionsPattern[]
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ CSSNormalizeEscapes @ ToLowerCase @ inputProp, mainProp},
		(* check whether input is a valid margin/padding long-form properties *)
		mainProp = If[StringEndsQ[prop, "height"], "height", "width"];
		If[!StringMatchQ[prop, Alternatives[#, "min-" <> #, "max-" <> #]& @ mainProp], 
			Return @ Failure["BadProp", <|"MessageTemplate" -> "Unrecognized `main` property.", "MessageParameters" -> <|"main" -> mainProp|>, "Prop" -> prop|>]];
		
		If[TrueQ @ OptionValue["PropertyIsCaseSensitive"], Message[CSSCascade::case, inputProp]];
		
		(* only look at declarations that match 'prop'; always include all height/width properties *)
		validBlockData = Select[CSSBlockData, StringEndsQ[#Property, mainProp]&];
		
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		
		(* run the FE option assembly function *)
		
		Function[{x},
			x ->
				Which[
					MatchQ[x, _?sizeOptionQ], (* ImageSize *)
						temp = {
							assembleWithConditions[validBlockData, x, Hold["Width",  "Min"], #&],
							assembleWithConditions[validBlockData, x, Hold["Width",  "Max"], #&],
							assembleWithConditions[validBlockData, x, Hold["Height", "Min"], #&],
							assembleWithConditions[validBlockData, x, Hold["Height", "Max"], #&]};
						(* convert to simplified forms if possible, adding Dynamic if needed *)
						Which[
							temp[[1]] === temp[[2]] && temp[[3]] === temp[[4]],
								Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {wn, hn}, Dynamic[{wn, hn}]]],
							temp[[1]] === temp[[2]],
								Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {wn, {hn, hx}}, Dynamic[{wn, {hn, hx}}]]],
							temp[[3]] === temp[[4]],
								Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {{wn, wx}, hn}, Dynamic[{{wn, wx}, hn}]]],
							True,
								Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {{wn, wx}, {hn, hx}}, Dynamic[{{wn, wx}, {hn, hx}}]]]
						]
					,
					True, 
						Automatic
				]
		] /@ optionNames				
	]


(* CellFrameColor is only used in the outline property, which is not supported *)
(* (*TODO*) assembleByFEOption[opt:CellFrameColor, CSSBlockData_?validCSSBlockFullQ] := opt -> assembleLRBTDirectives[CSSBlockData, opt] *)


(* general case 
	It looks at all the interpretations in a block of declarations (assumed sorted by importance, specificity, and already filtered for validity) 
	and combines them taking into account any @media conditions. *)
assembleByFEOption[FEopt_, CSSBlockData_?validCSSBlockFullQ] := 
	With[{val = assembleWithConditions[CSSBlockData, FEopt, Hold[], #&]},
		FEopt -> If[FreeQ[val, Which], First @ val, Dynamic @@ val]
	]

(* Assembling @page declarations into FE options:
	All CSS @page properties eventually end up as FE PrintingOptions. *)
assembleByFEOption[opt:PrintingOptions, CSSBlockData_?validCSSBlockFullQ] := 
	Module[{allSubOptions},
		allSubOptions = Union @ Flatten[Keys /@ DeleteMissing[#[opt] & /@ CSSBlockData[[All, "Interpretation"]]]];
		opt -> (assembleSubOption[CSSBlockData, opt, #]& /@ allSubOptions)
	]

assembleSubOption[CSSBlockData_?validCSSBlockFullQ, option:PrintingOptions, subOption:"PrintingMargins"] :=
	Module[{temp},
		temp = assembleWithConditions[CSSBlockData, option, Hold[subOption, #], #&]& /@ {"Left", "Right", "Bottom", "Top"};
		subOption -> Replace[temp, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]
	]
	
assembleSubOption[CSSBlockData_?validCSSBlockFullQ, option:PrintingOptions, subOption_] :=
	Module[{temp},
		temp = assembleWithConditions[CSSBlockData, option, Hold[subOption], #&];
		subOption -> Replace[temp, p:Hold[x___] :> If[FreeQ[p, Which], x, Dynamic[x]]]
	]


(* ::Subsection::Closed:: *)
(* Reduce property values during compute-value time *)


(* Take a CSS declaration and reduce any compute-value-time functions 
	Care must be taken for var() and rem unit substitution as there could be CSS custom properties and/or root font-size 
	declarations that have the same property name but with different conditions. 
	Each must be substituted independently with all conditions merged. This can lead to multiple versions of 
	compute-time-resolved css property values. 
	At the end, use the 'assemble' function to correctly merge each declaration together to a FE option. *)
reduceAtComputeValueTime[dec_?AssociationQ, customPropertiesWithCondition_?ListQ, remFontSizesWithCondition_?ListQ] :=
	Module[{customProps, customPropSets, fontsizes, i, j},
		(* determine which custom properties are needed for resolving this declaration's property value *)
		customProps = 
			Map[
				CSSTools`CSSCustomProperties1`Private`getCustomPropertyNameFromVarFunction,
				Extract[dec, Position[dec, Condition[t_?CSSTokenQ, TokenTypeIs["function", t] && TokenStringIs["var", t]]]]];
		
		If[TrueQ @ $Debug, Echo[dec, "CVT: declaration"]];
		If[TrueQ @ $Debug, Echo[customProps, "CVT: customProps"]];
				
		(* get every complete set of custom props; there are n-factorial combinations *)
		If[Length[customProps] > 0, 
			customPropSets = Flatten[Outer[List, Sequence @@ Function[{x}, Select[customPropertiesWithCondition, #Property === x &]] /@ customProps], Length[customProps] - 1];
			If[customPropSets === {}, customPropSets = {{}}];
			,
			customPropSets = {{}};
		];
		
		If[TrueQ @ $Debug, Echo[customPropSets, "CVT: customPropSets"]];
		
		(* set up rem font sizes *)
		If[Length[remFontSizesWithCondition] == 0, fontsizes = {None}, fontsizes = remFontSizesWithCondition];
		
		If[TrueQ @ $Debug, Echo[fontsizes, "CVT: fontsizes"]];
				
		DeleteCases[Flatten @ Table[doSingleCaseAtComputeValueTime[dec, customPropertiesWithCondition, i, j], {i, customPropSets}, {j, fontsizes}], _?FailureQ]
	]
	
doSingleCaseAtComputeValueTime[dec_?AssociationQ, customPropertiesWithCondition_, customPropSet_, remFontSizeWithCondition_] :=
	Module[{keep, remove, try, newCondition1, newCondition2, fontsize, customPropSubset, tokens, pos, l},
		(* choose a set of props. Keep those in place and remove duplicate props elsewhere in the customPropertiesWithCondition list. *)
		keep = Flatten @ Position[customPropertiesWithCondition, Alternatives @@ customPropSet];
		remove = Complement[Position[customPropertiesWithCondition, Alternatives @@ customPropSet[[All, "Property"]]][[All, 1]], keep];
		customPropSubset = Delete[customPropertiesWithCondition, List /@ remove];
		newCondition1 = Sort @ Union @ Flatten[convertConditionToList /@ customPropertiesWithCondition[[keep, "Condition"]]];
		
		(* get the font-size value and its condition *)
		If[TrueQ @ $Debug, Echo[remFontSizeWithCondition, "CVT: rem fontsize with condition"]];
		If[remFontSizeWithCondition === None, 
			fontsize = None; newCondition2 = {None};
			, 
			fontsize = remFontSizeWithCondition["Value"]; newCondition2 = convertConditionToList[remFontSizeWithCondition["Condition"]];
		];
		
		If[TrueQ[$Debug], Echo[{fontsize, customPropSubset}, "CVT single case: fontsize + prop set"]];
		
		(* reduce declaration to a simplified expression *)
		try = reduceAtComputeValueTimeDoReplacements[dec, (#Property -> #Value)& /@ customPropSubset, fontsize];
		If[TrueQ[$Debug], Echo[try, "CVT single case: after all replacements"]];
		If[FailureQ[try["Interpretation"]], Echo[try, "Failed to reduce declaration."]; Return @ try];
		
		(* clean up declaration: combine conditions *)
		try["Condition"] = combineConditions[try["Condition"], {newCondition1, newCondition2}];
		If[ListQ[try[["Condition"]]], 
			try[["Condition"]] = 
				Replace[
					Thread[DeleteCases[try[["Condition"]], None], Hold], 
					{
						Hold[{x___}] :> If[Length[{x}] > 1, Hold[And[x]], Hold[x]], 
						{} -> None}]];
		
		(* clean up declaration: reduce property value to string and re-interpret *)
		try["Value"] = StringTrim @ CSSUntokenize[try[["Interpretation", "CSSResolveValueAtComputeTime", "Tokens"]]];
		tokens = try[["Interpretation", "CSSResolveValueAtComputeTime", "Tokens"]];
		pos = 1; l = Length[tokens]; TrimWhitespaceTokens[pos, l, tokens];
		
		If[TrueQ[$Debug], Echo[{pos, l, tokens}, "CVT single case: pos, l, tokens"]];
		
		try["Interpretation"] = consumeProperty[try["Property"], tokens[[pos ;; l]]];
		try
	]

combineConditions[cOld_, cAdd_] := DeleteDuplicates @ Sort @ Flatten @ {cOld, cAdd}
convertConditionToList[condition_] := Which[MatchQ[#, Hold[And[__]]], Apply[List, Thread[#, And]], True, {#}]&[condition]
convertScaledFontSizeToCurrentValue[input_] := 
	If[!FreeQ[input["Interpretation", FontSize], Scaled],
		Replace[
			Dynamic[Evaluate[input["Interpretation", FontSize]]] /. Scaled[x_] :> x*CurrentValue[$FrontEnd, FontSize], 
			Dynamic[y___] :> y, 
			Infinity]
		,
		input["Interpretation", FontSize]
	]

(* if any future compute-time substitutions are to be added, then this is where those new rules would go *)
reduceAtComputeValueTimeDoReplacements[dec_?AssociationQ, customPropertyDefinitions_?ListQ, remFontSize_] :=
	Module[{d = dec, funcPositions, token, try},
		(* resolve instances from the inside-out; the Position function naturally orders from deepest to shallowest *)
		funcPositions = 
			Position[
				d, 
				Condition[
					t_?CSSTokenQ,
					Or[
						TokenTypeIs["function", t] && TokenStringIs["attr" | "calc" | "var", t],
						TokenTypeIs["dimension", t] && TokenUnitIs["rem", t] && !KeyExistsQ[First[t], "Interpretation"]]]];
		
		If[TrueQ[$Debug], Echo[funcPositions, "CVT: do replacement; funcPositions"]];
		
		Do[
			token = Extract[d, funcPosIndex];
			If[TrueQ[$Debug], Echo[token, "CVT: do replacement; token"]];
			try = Catch @ 
				Which[
					TokenTypeIs["function", token],
						Which[
							TokenStringIs["var",  token], CSSTools`CSSCustomProperties1`Private`replaceVarFunctionWithTokens[{token}, customPropertyDefinitions],
							TokenStringIs["calc", token], CSSTools`CSSValuesAndUnits3`Private`replaceCalcFunctionsWithTokens[{token}, d["Property"]],
							TokenStringIs["attr", token], CSSTools`CSSValuesAndUnits3`Private`replaceAttrFunctionsWithTokens[{token}, d["Element"], d["Interpretation", "CSSResolveValueAtComputeTime", "Namespaces"]],
							True,                         Failure["BadParse", <|"MessageTemplate" -> "Could not find expected function token."|>]
						],
					TokenTypeIs["dimension", token] && TokenUnitIs["rem", token] && !KeyExistsQ[First[token], "Interpretation"],
						{Replace[token, 
							CSSToken[<|"Type" -> "dimension", "String" -> s_, "Value" -> v_, "ValueType" -> t_, "Unit" -> u_|>] :>
								CSSToken[<|"Type" -> "dimension", "String" -> s, "Value" -> v, "ValueType" -> t, "Unit" -> u,
									If[MatchQ[remFontSize, None], 
										Nothing
										,
										"Interpretation" -> If[MatchQ[remFontSize, _Dynamic], Replace[remFontSize, Dynamic[x___] :> Dynamic[v*(x)]], v*remFontSize]
									]|>]]},
					True, 
						Throw @ Failure["UnexpectedState", <|"MessageTemplate" -> "Expected compute-time parameter."|>]
				];
			If[FailureQ[try], 
				d["Interpretation"] = try; Break[]
				,
				d = ReplacePart[d, funcPosIndex -> Unevaluated[Sequence @@ try]] //. HoldPattern[{a___, Sequence[b__], c___}] :> {a, b, c}
			]
			,
			{funcPosIndex, funcPositions}];
			
		If[TrueQ[$Debug], Echo[d, "CVT: do replacement; done"]];
		
		d
	]
	

(* ::Section:: *)
(*Main Functions*)


(* ::Subsection::Closed:: *)
(*CSSCascade*)


(* From https://developer.mozilla.org/en-US/docs/Web/CSS/Cascade
The cascading algorithm determines how to find the value to apply for each property for each document element.

1. It first filters all the delarations from the different sources to keep only the delarations that apply to a given element. 
That means delarations whose selector matches the given element and which are part of an appropriate media at-rule.

2. Then it sorts these delarations according to their importance, that is, whether or not they are followed by !important, 
and by their origin. The cascade is in ascending order, which means that !important values from a user-defined style sheet 
have precedence over normal values originated from a user-agent style sheet:
	Origin		Importance
A	user agent	normal
B	user		normal
C	author		normal
D	animations	
E	author		!important
F	user		!important
G	user agent	!important
H	transitions	

3. In case of equality, the specificity of a value is considered to choose one or the other.

How then do we resolve the cascade in WL code? 
There is also no CSS user agent stylesheet because the WL frontend does not have a native CSS stylesheet. 
Also, because of the imported CSS data, it behaves as if all rules come from a single style sheet (origin).
Thus rule (1) only has one SS source but multiple selectors within the SS can be simultaneously active.
This is tricky with @media conditions since these conditions are not immediately resolved by the FE.
Rule (2), due to only one origin, is only a bisected sorting of declarations by importance.
Rule (3) often applies; if two rules have the same importance then the one with the higher specificity wins.
If specificity is also equal, then the last rule wins.

In practice, we 
1. gather all relevant declarations, including all from @media rulesets
2. delete any declarations that failed to parse or are missing
3. since we have only one origin, sort remaining declarations by importance then by specificity (unless turned off via an option)
4. merge across related properties into FE options

Step (4) is complicated 
SIDED PROPERTIES LEFT/RIGHT/BOTTOM/TOP:
Some properties are given per side of the box, whereas the FE has only one option e.g. FrameStyle.
For example, 'margin-left' can override 'margin' settings so long 'margin-left' comes after 'margin'.
We thus perform the cascade on each side, then join them together into the FE option value.
SIZE PROPERTIES MIN/MAX:
Some properties are given as min/max values, whereas the FE has only one option e.g. ImageSize.
For example, 'min-width' can override 'width' settings so long as 'min-width' comes after 'width'.
We thus perform the cascade on each min/max limit, then join them together into the FE option value.
FONT PROPERTIES:
The CSS properties text-decoration, text-transform, and font-variant are separate props.
The FE has similar features, but all are given as suboptions in the FontVariations option.
Resolving this cascade is perhaps not exactly conformant with the CSS spec because 3 props must merge to one option value.
*)

(* 
	Some CSS Modules introduce the concept where property values "resolve at compute time" aka computed-value time. 
	reduceAtComputeValueTime calls consumeProperty after all property value reductions.
	
	CSS Custom Properties: 
		* introduces var() function

	CSS Values and Units:
		* introduces calc() and attr() functions 
		* introduces rem dimension i.e. "root ems"; these can't resolve in CSSCascade and should instead scale with CurentValue[$FrontEnd, FontSize]
*)
		
(* CSSCascade:
	1. Select the entries in the CSS data based on the provided selectors
	2. order the selectors based on specificity and importance (if those options are on)
	3. merge resulting list of interpreted options 
Arg1: scope
	CSS lacks scope in that all/most properties can apply to all boxes.
	WD has options that apply to specific levels e.g. Cell/Notebook/Box/All. 
	This argument can also take a list of boxes e.g. ActionMenuBox or a list of box generators e.g. ActionMenu.
Arg2:  
*)	
ClearAll[CSSCascade];
Options[CSSCascade] = {"IgnoreSpecificity" -> False, "IgnoreImportance" -> False, "PropertyIsCaseSensitive" -> False};

CSSCascade::case = "A case-sensitive property matches the shorthand property `1`. Some case-sensitive values may be overwritten during shorthand expansion.";

(* normalize Dataset input *)
CSSCascade[props_, scope_, CSSData_Dataset, selectorList_, opts:OptionsPattern[]] := 
	CSSCascade[props, scope, Normal @ CSSData, selectorList, opts]

CSSCascade[{}, scope_, CSSData_, selectorList_, opts:OptionsPattern[]] := {}

(* main function *)
CSSCascade[
	inputProps:_?StringQ | {__?StringQ} | All, 
	scope:(Cell | Notebook | Box | All | None | _?validBoxesQ | {__?validBoxesQ}), 
	CSSData_?validCSSDataQ | {}, 
	inputSelectorList:_?(Function[CSSSelectorQ[#] || StringQ[#]]) | {__?(Function[CSSSelectorQ[#] || StringQ[#]])} | All, 
	opts:OptionsPattern[]
] :=
	Module[{usedProps, declarations, resolvedOptions, CSSDataSubset, element, selectorList, customPropertyDefinitions, temp, itemsToResolve},
		(* determine all properties that the user wishes to collapse to FE options *)
		usedProps = 
			StringTrim @ If[Not[TrueQ @ OptionValue["PropertyIsCaseSensitive"]], CSSNormalizeEscapes[ToLowerCase[#]], Identity[#]]& /@ 
				Which[
					MatchQ[inputProps, All],     Union @ Flatten @ CSSData[[All, "Block", All, "Property"]], 
					MatchQ[inputProps, _?ListQ], Union @ Flatten @ inputProps,
					True,                        Union @ Flatten @ {inputProps}
				];
		
		(* filter the data by the given list of selectors; ordering is maintained *)
		(* upgrade any string selectors to CSSSelector objects *)
		(* match against the tokenized selector sequence, which should be unique *)
		selectorList = 
			Which[
				MatchQ[inputSelectorList, All],     CSSData[[All, "Selector"]],
				MatchQ[inputSelectorList, _?ListQ], inputSelectorList,
				True,                               {inputSelectorList}
			];
		selectorList = Select[selectorList, ((StringQ[#] && !StringStartsQ[#, "@"]) || MatchQ[#, _CSSSelector])&];
		selectorList = Replace[selectorList, s_?StringQ :> CSSSelector[s], {1}];
		If[AnyTrue[selectorList, _?FailureQ], Return @ FirstCase[selectorList, _?FailureQ]];
		CSSDataSubset = 
 			Select[
 				CSSData, 
  				MatchQ[#Selector["Sequence"], Alternatives @@ Through[selectorList["Sequence"]]] &];
  		If[CSSDataSubset === {}, Return @ {}];
  		
  		(* if a CSSTarget exists, add the target element to each declaration in each rule in order for attr() values to correctly resolve wherever they appear *)
  		(* due to ambiguity we only take the first target if found; if none are found then the attr() fallback value should be used automatically if it exists *)
		Do[
			element = If[KeyExistsQ[CSSDataSubset[[j]], "Targets"], First[CSSDataSubset[[j, "Targets"]], None], None];
			CSSDataSubset[[j, "Block"]] = <|#, "Element" -> element|>& /@ CSSDataSubset[[j, "Block"]];
			,
			{j, Length[CSSDataSubset]}
		];
		
		If[TrueQ @ $Debug, Echo[CSSDataSubset, "CSSCascade data subset before compute-value-time"]];

		(* custom property definitions must fully resolve first *) 
		(* This function also reduces the rulesets to just a list of cascade-order declarations *)
		{declarations, customPropertyDefinitions} = CSSTools`CSSCustomProperties1`Private`resolveCSSCustomPropertyDefinition[CSSDataSubset, scope, opts];
		
		(* tokenize CSSResolveValueAtComputeTime instances and add tokens as a new key *)
		Do[
			temp = declarations[[i]];
			If[AssociationQ[temp["Interpretation"]] && KeyExistsQ[temp["Interpretation"], "CSSResolveValueAtComputeTime"], 
				AssociateTo[temp["Interpretation", "CSSResolveValueAtComputeTime"], "Tokens" -> CSSTokenize[temp["Value"]]]];
			declarations[[i]] = temp
			,
			{i, Length[declarations]}
		];
		
		If[TrueQ @ $Debug, Echo[customPropertyDefinitions, "CSSCascade custom props"]];
		If[TrueQ @ $Debug, Echo[declarations, "CSSCascade declarations before compute-value-time"]];
		
		(* resolve the compute-time declarations *)	
		(* note that reduceAtComputeValueTime calls consumeProperty to produce a fully resolved "Interpretation" *)
		itemsToResolve = Flatten @ Position[(AssociationQ[#] && KeyExistsQ[#, "CSSResolveValueAtComputeTime"])& /@ declarations[[All, "Interpretation"]], True];
		(* 
			'rem' cannot resolve within CSSCascade because it does not have access to the :root properties. 
			Regardlesss, set the RemIsResolved flag to True in order to avoid the consumeProperty DownValue that pre-empts rem-containing property values *)
		Block[{CSSTools`CSSValuesAndUnits3`Private`RemIsResolved = True},
			Do[declarations[[j]] = reduceAtComputeValueTime[declarations[[j]], customPropertyDefinitions, {}], {j, itemsToResolve}];
		];
			
		If[TrueQ @ $Debug, Echo[declarations, "CSSCascade declarations after compute-value-time"]];
		
		(* clean up anything added to declarations during compute-time processing *)
		declarations = KeyDrop["Element"] /@ Flatten[declarations];
		
		(* assemble declarations into FE options *)
		resolvedOptions = Flatten[assemble[#, scope, declarations, "PropertyIsCaseSensitive" -> OptionValue["PropertyIsCaseSensitive"]]& /@ usedProps];
		If[TrueQ @ $Debug, Echo[resolvedOptions, "resolved options"]];
		Flatten @ 
			Which[
				MatchQ[scope, Box | All],
					movePseudoOptionsIntoBoxOptions[resolvedOptions, validBoxes],
				MatchQ[scope, _?validBoxesQ | {__?validBoxesQ}],
					movePseudoOptionsIntoBoxOptions[resolvedOptions, If[ListQ[scope], scope, {scope}] /. Thread[validBoxGenerators -> validBoxes]],
				True,
					resolvedOptions
			]		
	]

CSSCascade[props_, scope_, ___] /; !MatchQ[scope, (Cell | Notebook | Box | All | None | _?validBoxesQ | {__?validBoxesQ})] := 
	Failure["BadScope", <|"Message" -> "Second argument expected as All, Notebook, Cell, Box or box generator."|>]
CSSCascade[props_, scope_, CSSData_ /; !validCSSDataQ[CSSData], ___] := 
	Failure["BadCSSData", <|"Message" -> "CSS data appears invalid."|>]

shorthandProperties = {
	"background",
	"border", "border-top", "border-bottom", "border-left", "border-right", "border-style", "border-color", "border-width",
	"font", "list-style", "margin", "outline", "padding"};
	

(* main cascade steps *)
Options[cssCascadeDeclarations] = Options[CSSCascade];
cssCascadeDeclarations[
	scope:(Cell | Notebook | Box | All | None | _?validBoxesQ | {__?validBoxesQ}), 
	CSSData_ | {}, 
	inputSelectorList:_?(Function[CSSSelectorQ[#] || StringQ[#]]) | {__?(Function[CSSSelectorQ[#] || StringQ[#]])} | All, 
	opts:OptionsPattern[]
] :=
	Module[{selectorList, atRules, sel, dataSubset, specificities, declarations},
		(* check for empty data *)
		If[CSSData === {}, Return @ {}];
		
		selectorList = 
			Which[
				MatchQ[inputSelectorList, All],     CSSData[[All, "Selector"]],
				MatchQ[inputSelectorList, _?ListQ], inputSelectorList,
				True,                               {inputSelectorList}
			];
		
		(* start by filtering the data by the given list of selectors; ordering is maintained *)
		(* exceptions: @page rules *)
		(* upgrade any string selectors to CSSSelector objects *)
		(* match against the tokenized selector sequence, which should be unique *)
		sel = Select[selectorList, ((StringQ[#] && !StringStartsQ[#, "@"]) || MatchQ[#, _CSSSelector])&];
		sel = Replace[sel, s_?StringQ :> CSSSelector[s], {1}];
		If[AnyTrue[sel, _?FailureQ], Return @ FirstCase[sel, _?FailureQ]];
		dataSubset = 
 			Select[
 				CSSData, 
  				MatchQ[#Selector["Sequence"], Alternatives @@ Through[sel["Sequence"]]] &];
  		
  		(* gather all declarations, ordered by specificity unless that process is switched off *)
		If[TrueQ @ OptionValue["IgnoreSpecificity"],
			(* if ignoring specificity, then leave the user-supplied selector list alone *)
			declarations = Flatten @ dataSubset[[All, "Block"]]
			,
			(* otherwise sort based on specificity but maintain order of duplicates; this is what should happen based on the CSS specification *)
			specificities = Through[dataSubset[[All, "Selector"]]["Specificity"]];
			declarations = Flatten @ dataSubset[[Ordering[specificities]]][[All, "Block"]];
		];
		
		(* expand property shorthands; maintain importance and any conditions *)
  		declarations = 
  			Replace[
  				declarations, 
  				kvp:KeyValuePattern[{
  					"Property" -> s_?StringQ /; StringMatchQ[s, Alternatives @@ shorthandProperties, IgnoreCase -> True], 
  					"Interpretation" -> int_}
  				] :> Sequence @@ (<|#, KeyDrop[kvp, {"Property", "Value", "Interpretation"}]|>& /@ int), {1}];
  		
		(* add @page declarations; they should only apply at Notebook scope *)
		(* FIXME: in paged module 3 the pages have their own specificity. Page specificity has not been implemented *)
  		atRules = Select[selectorList, StringQ[#] && StringStartsQ[#, "@"]&];
  		If[MemberQ[atRules, s_ /; StringStartsQ[s, "@page"]],
  			dataSubset = Select[CSSData, MatchQ[#Selector, s_?StringQ /; StringStartsQ[s, "@page"]]&];
  			declarations = Join[Flatten @ dataSubset[[All, "Block"]], declarations]
  		];
  		
  		(* Remove properties that failed to parse or are unsupported. *)
  		(*declarations = Select[declarations, Not[FailureQ[#Interpretation] || MissingQ[#Interpretation]]&];*)
  		
  		(* Move !important CSS properties to the end but maintain specificty ordering amongst regular and important properties. *)
  		declarations = 
  			Flatten @ 
				If[Not[TrueQ @ OptionValue["IgnoreImportance"]],
					Join[
						Select[declarations, #Important == False&], 
						Select[declarations, #Important == True&]]
					,
					declarations
				];
				
		(* 
			At this point the declarations should be ordered in increasing importance and specificity, with the last entry of any ties coming later.
			Said differently, for a given property search from the end of the list. The first entry found is the most important, specific, or the tie breaker. *)
		(* 
			Reverse the order so the to-be-used declaration is now first. 
			We can't delete duplicates because some declarations may not apply due to a Condition. 
			Said differently, if a Condition is not met, then that declaration is removed from the cascade entirely. *)
		Reverse @ declarations
	]


(* ::Subsection::Closed:: *)
(*CSSTargets, ExtractCSSFromXML*)


(* ::Subsubsection::Closed:: *)
(*Patterns in XMLElement where inline CSS can appear *)


linkElementPattern[] :=
	XMLElement[
		x_String | {_, x_String} /; StringMatchQ[x, "link", IgnoreCase -> True], 
		Alternatives[
			{
				___, 
				(attr1_String | {_, attr1_String} /; StringMatchQ[attr1, "rel", IgnoreCase -> True]) -> 
					(attrVal_String /; StringMatchQ[attrVal, "stylesheet", IgnoreCase -> True]), 
				___, 
				(attr2_String | {_, attr2_String} /; StringMatchQ[attr2, "href", IgnoreCase -> True]) -> loc_, 
				___},
			{
				___, 
				(attr2_String | {_, attr2_String} /; StringMatchQ[attr2, "href", IgnoreCase -> True]) -> loc_, 
				___, 
				(attr1_String | {_, attr1_String} /; StringMatchQ[attr1, "rel", IgnoreCase -> True]) -> 
					(attrVal_String /; StringMatchQ[attrVal, "stylesheet", IgnoreCase -> True]), 
				___}],
		___
	] :> loc

styleElementPattern[] :=
	XMLElement[
		x_String | {_, x_String} /; StringMatchQ[x, "style", IgnoreCase -> True], 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "type", IgnoreCase -> True]) -> 
				(attrVal_String /; StringMatchQ[attrVal, "text/css", IgnoreCase -> True]), 
			___}, 
		{css_String}
	] :> css
		
styleAttributePattern[] :=
	XMLElement[
		_, 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "style", IgnoreCase -> True]) -> css_, 
			___}, 
		___
	] :> css


(* ::Subsubsection::Closed:: *)
(*CSSTargets extension to CSS data*)


(* CSSTargets:
	Generally applies a selector to an XML document and returns the positions where the selector targets.
	It has two different scopes:
               CSSSelector  ---->  returns extractable positions, similar to Position syntax
[defined here] CSSDataset   ---->  returns same dataset, but with added Targets column of extractable positions *)
(* normalize Dataset input *)
CSSTargets[doc:XMLObject["Document"][___], CSSData_Dataset, wrapInDataset_:True] := 
	CSSTargets[doc, Normal @ CSSData, wrapInDataset]

(* main function *)
CSSTargets[doc:XMLObject["Document"][___], CSSData_?validCSSDataQ, wrapInDataset_:True] :=
	If[TrueQ @ wrapInDataset, Dataset, Identity][
		(* Rebuild the CSS data with the targets included. *)
		MapThread[
			<|
				"Selector" -> #1["Selector"], 
				"Targets"  -> #2, 
				"Block"    -> #1["Block"]|>&,
			{CSSData, CSSTargets[doc, #]& /@ CSSData[[All, "Selector"]]}] (* defined in CSSSelectors3 *)
	]
		
CSSTargets[_, CSSData_?validCSSDataQ, ___]      := Failure["BadDocument", <|"Message" -> "Invalid XML document."|>]
CSSTargets[doc:XMLObject["Document"][___], ___] := Failure["BadData", <|"Message" -> "Invalid CSS."|>]


(* ::Subsubsection::Closed:: *)
(*ExtractCSSFromXML*)


(* ExtractCSSFromXML:
	*)
Options[ExtractCSSFromXML] = {"RootDirectory" -> Automatic};
ExtractCSSFromXML::nodir = "Directory `1` does not exist.";

ExtractCSSFromXML[doc:XMLObject["Document"][___], opts:OptionsPattern[]] :=
	Module[
		{
			currentDir, externalSSPositions, externalSSContent, internalSSPositions, internalSSContent, 
			directStylePositions, directStyleContent, all},
			
		currentDir = Directory[];
		Which[
			OptionValue["RootDirectory"] === Automatic, SetDirectory[Directory[]],
			DirectoryQ[OptionValue["RootDirectory"]],   SetDirectory[OptionValue["RootDirectory"]],
			True,                                       
				Message[ExtractCSSFromXML::nodir, OptionValue["RootDirectory"]]; 
				SetDirectory[Directory[]]
		];
		
		(* process externally linked style sheets via <link> elements *)
		externalSSPositions = Position[doc, First @ linkElementPattern[]];
		externalSSContent = ExternalCSS /@ Cases[doc, linkElementPattern[], Infinity];
		(* filter out files that weren't found *)
		With[{bools =  # =!= $Failed& /@ externalSSContent},
			externalSSPositions = Pick[externalSSPositions, bools];
			externalSSContent = Pick[externalSSContent, bools];];
		externalSSContent = CSSTargets[doc, #, False]& /@ externalSSContent;
				
		(* process internal style sheets given by <style> elements *)
		internalSSPositions = Position[doc, First @ styleElementPattern[]];
		internalSSContent = InternalCSS /@ Cases[doc, styleElementPattern[], Infinity];
		internalSSContent = CSSTargets[doc, #, False]& /@ internalSSContent;
		
		(* process internal styles given by 'style' attributes *)
		directStylePositions = Position[doc, First @ styleAttributePattern[]];
		directStyleContent = Cases[doc, styleAttributePattern[], Infinity];
		directStyleContent = 
			MapThread[
				<|
					"Selector" -> CSSSelector[<|"String" -> None, "Sequence" -> {}, "Specificity" -> {1, 0, 0, 0}|>], 
					"Targets"  -> 
						(* TODO: this is so bad. These Block'ed variables should not be necessary. What should be done is
						CSSSelectors re-worked to pass the document as a variable instead of lazy Block'ing. 
						Moreover the namespace functions should be pulled into their own module namely CSS Namespaces Module Level 3 *)
						Block[{CSSTools`CSSSelectors3`Private`$Document = doc, CSSTools`CSSSelectors3`Private`$DocumentNamespaces}, 
							CSSTools`CSSSelectors3`Private`$DocumentNamespaces = getDocumentNamespaces[CSSTools`CSSSelectors3`Private`$Document];
							{convertXMLPositionToCSSTarget[#1]}], 
					"Block"    -> consumeDeclarationBlock[CSSTokenize @ #2, {}]|>&, 
				{directStylePositions, directStyleContent}];
		
		(* combine all CSS sources based on position in XMLObject *)
		all =
			Flatten @ 
				Part[
					Join[externalSSContent, internalSSContent, directStyleContent],
					Ordering @ Join[externalSSPositions, internalSSPositions, directStylePositions]];
		
		(* reset Directory[] and return extracted CSS *)
		SetDirectory[currentDir];
		Dataset @ all		
	]


(* ::Subsection::Closed:: *)
(*CSSInheritance*)


(* CSSInheritance
	Based on the position in the XMLObject, 
	1. look up all ancestors's positions and start from the most ancient ancestor
	2. get all declarations that apply to that ancestor using the CSS cascade algorithm
	3. resolve compute-time property values
	4. append inheritable properties from the previous generation to the current (ignore 'inherit' keyword)
	5. repeat (2-4) for each generation
	
	In the final generation:
	1. resolve any 'inherit' keywords 
	2. assemble declarations into FE options *)

ClearAll[CSSInheritance];
Options[CSSInheritance] = {"PropertyIsCaseSensitive" -> False};

(* normalize Dataset object *)
CSSInheritance[target_?CSSTargetQ, scope_, CSSData_Dataset, opts:OptionsPattern[]] := CSSInheritance[target, scope, Normal @ CSSData, opts]

(* main function *)
CSSInheritance[target_?CSSTargetQ, scope_, CSSData_?validCSSDataQ, opts:OptionsPattern[]] :=
	Module[
		{
			lineage, CSSDataSubset, i, inheritableProps, declarations, case = OptionValue["PropertyIsCaseSensitive"], 
			element, itemsToResolve, generations, resolvedOptions, props, temp, customPropertyDefinitions, pos,	rootFontSizes = {}},
		(* get the position of all ancestors *)
		lineage = Append[parents[target["Position"]], target["Position"]];
		generations = Table[{<|"Property" -> "dummy", "Value" -> "dummy", "Interpretation" -> Missing["Not supported."], "Important" -> False, "Condition" -> None|>}, Length[lineage] + 1];
		
		(* Perform inheritance at each remaining ancestor:
			1. perform CSS cascade to get computed value of all inheritable properties
			2. for any properties that do not have values, get them from the previous ancestor if possible 
			3. repeat at each generation until a final set of properties is obtained *)
		Do[
			(* get all CSS data entries that target the input position *)
			CSSDataSubset = Pick[CSSData, MemberQ[#, lineage[[i]]]& /@ (Through[#["Position"]]& /@ CSSData[[All, "Targets"]])];
			
			(* add the target element to each declaration in each rule in order for attr() values to correctly resolve wherever they appear *)
			Do[
				element = First[Select[CSSDataSubset[[j, "Targets"]], MatchQ[#["Position"], lineage[[i]]]&], None];
				CSSDataSubset[[j, "Block"]] = <|#, "Element" -> element|>& /@ CSSDataSubset[[j, "Block"]];
				,
				{j, Length[CSSDataSubset]}
			];
			
			(* custom property definitions must fully resolve first 
				This function also reduces the rulesets to just a list of cascade-order declarations *)
			{declarations, customPropertyDefinitions} = CSSTools`CSSCustomProperties1`Private`resolveCSSCustomPropertyDefinition[CSSDataSubset, scope, opts];
			
			(* tokenize CSSResolveValueAtComputeTime instances and add tokens as a new key *)
			Do[
				temp = declarations[[i]];
				If[AssociationQ[temp["Interpretation"]] && KeyExistsQ[temp["Interpretation"], "CSSResolveValueAtComputeTime"], 
					AssociateTo[temp["Interpretation", "CSSResolveValueAtComputeTime"], "Tokens" -> CSSTokenize[temp["Value"]]]];
				declarations[[i]] = temp
				,
				{i, Length[declarations]}];
			
			If[TrueQ @ $Debug, Echo["generation " <> ToString[i]]];
			If[TrueQ @ $Debug, Echo[customPropertyDefinitions, "CSSInheritance new custom props from this generation"]];
			If[TrueQ @ $Debug, Echo[declarations, "CSSInheritance declarations before compute-value-time"]];
			
			(* add any inherited custom props *)
			customPropertyDefinitions = Join[customPropertyDefinitions, Select[generations[[i]], StringStartsQ[#Property, "--"]&]];
		
			(* compute root font-size 'rem' *)
			If[i == 1,
				(* get indices of font-size declaration *)
				pos = Flatten @ Position[declarations, KeyValuePattern[{"Property" -> s_?StringQ}] /; StringMatchQ[CSSNormalizeEscapes @ s, "font-size", IgnoreCase -> !case], {1}];
				(* only resolve at-compute-time instances *)
				itemsToResolve = Pick[pos, (AssociationQ[declarations[[#, "Interpretation"]]] && KeyExistsQ[declarations[[#, "Interpretation"]], "CSSResolveValueAtComputeTime"])& /@ pos];
				Do[
					(* for each at-compute-time instance, convert relative sizes to those based on CurrentValue[$FrontEnd, FontSize] *)
					temp = reduceAtComputeValueTime[declarations[[i]], customPropertyDefinitions, {}];
					Do[temp[[j, "Interpretation", Key[FontSize]]] = convertScaledFontSizeToCurrentValue[temp[[j]]], {j, Length[temp]}];
					declarations[[i]] = temp
					, 
					{i, itemsToResolve}];
				(* convert relative sizes for each non-at-compute-time instances *)
				Do[declarations[[i, "Interpretation", Key[FontSize]]] = convertScaledFontSizeToCurrentValue @ declarations[[i]], {i, Complement[pos, itemsToResolve]}];
				(* create root font-size set and clean up declarations *)
				rootFontSizes =	<|"Value" -> #["Interpretation", FontSize], "Condition" -> #Condition|>& /@ Flatten[declarations[[pos]]];
				declarations = Flatten[declarations];
			];
						
			(* resolve any compute-time declarations of this generation *)	
			itemsToResolve = Flatten @ Position[(AssociationQ[#] && KeyExistsQ[#, "CSSResolveValueAtComputeTime"])& /@ declarations[[All, "Interpretation"]], True];
			(* set the RemIsResolved flag to True in order to avoid the consumeProperty DownValue that pre-empts rem-containing property values *)
			Block[{CSSTools`CSSValuesAndUnits3`Private`RemIsResolved = True},
				Do[declarations[[j]] = reduceAtComputeValueTime[declarations[[j]], customPropertyDefinitions, rootFontSizes], {j, itemsToResolve}];
			];
			
			(* clean up anything added to declarations during compute-time processing *)
			declarations = KeyDrop["Element"] /@ Flatten[declarations];
						
			(* append the current generation's declarations with the previous generations's inheritable ones *)
			inheritableProps = Union @ Flatten @ generations[[i]][[All, "Property"]];
			inheritableProps = Pick[inheritableProps, (Or @@ Through[inheritedPropertyRules[#]])& /@ inheritableProps];
			
			If[TrueQ @ $Debug, Echo[inheritableProps, "props inherited from previous generation"]];
			
			generations[[i+1]] = Join[declarations, Select[generations[[i]], MatchQ[#Property, Alternatives @@ inheritableProps]&]];
			,
			{i, Length[lineage]}
		];
		
		If[TrueQ @ $Debug, Echo[generations, "all generations"]];
		
		(* resolve any 'inherit' keywords of the last generation; multiple conditioned declarations can be pulled from a previous generation *)
		itemsToResolve = Flatten @ Position[generations[[-1]], KeyValuePattern[{"Property" -> _, "Value" -> s_ /; StringQ[s] && StringMatchQ[s, "inherit", IgnoreCase -> True]}]];
		Do[
			temp = resolveInheritKeyword[generations, generations[[-1, i, "Property"]], case];
			generations[[-1, i]] = temp
			, 
			{i, itemsToResolve}];
		generations[[-1]] = Flatten[generations[[-1]]];
		
		(* gather property names; combine shorthand properties *)
		props = Union @ generations[[-1, All, "Property"]];
		If[!case, props = Union @ ToLowerCase @ props];
		props = Union @ StringReplace[props, {StartOfString ~~ p:(Alternatives @@ SortBy[StringLength] @ shorthandProperties) ~~ __ :> p}];
		
		(* assemble declarations into FE options *)
		resolvedOptions = Flatten[assemble[#, scope, generations[[-1]], "PropertyIsCaseSensitive" -> case]& /@ props];
		Flatten @ 
			Which[
				MatchQ[scope, Box | All],
					movePseudoOptionsIntoBoxOptions[resolvedOptions, validBoxes],
				MatchQ[scope, _?validBoxesQ | {__?validBoxesQ}],
					movePseudoOptionsIntoBoxOptions[resolvedOptions, If[ListQ[scope], scope, {scope}] /. Thread[validBoxGenerators -> validBoxes]],
				True,
					resolvedOptions
			]
	]
	
CSSInheritance[position:{___?IntegerQ}, _] := 
	Failure["BadData", <|"Message" -> "Invalid CSS data. CSS data must include specificity and target."|>]


parents[x:{__Integer}] := Most @ Reverse @ NestWhileList[Drop[#, -2]&, x, Length[#] > 2&]

inheritedProperties[] := Pick[Keys @ #, Values @ #]& @ CSSPropertyData[[All, "Inherited"]];
inheritedPropertyRules = {MemberQ[inheritedProperties[], #]&};

resolveInheritKeyword[generations_?ListQ, prop_?StringQ, propIsCaseSensitive_] :=
	Module[{l = Length[generations], value, i=-1},
		value = 
			FirstCase[
				generations[[i]], 
				KeyValuePattern[{"Property" -> s_ /; StringQ[s] && StringMatchQ[s, prop, IgnoreCase -> !propIsCaseSensitive], "Value" -> v_}] :> v,
				None];
		While[i > -l && StringQ[value] && StringMatchQ[value, "inherit", IgnoreCase -> True],
			i--;
			value = 
				FirstCase[
					generations[[i]], 
					KeyValuePattern[{"Property" -> s_ /; StringQ[s] && StringMatchQ[s, prop, IgnoreCase -> !propIsCaseSensitive], "Value" -> v_}] :> v, 
					None];
		];
		If[value === None, 
			(* get initial value of property if inherit chain hits dead end *)
			CSSPropertyData[[ToLowerCase @ prop, "InterpretedGlobalValues", "initial"]]
			,
			(* else get this generation's interpreted property value, which may be multiple values if there are numerous conditions *)
			Cases[
				generations[[i]], 
				KeyValuePattern[{"Property" -> s_ /; StringQ[s] && StringMatchQ[s, prop, IgnoreCase -> !propIsCaseSensitive]}], 
				{1}]
		]
	]


(* ::Subsection::Closed:: *)
(*Import*)


(* slightly faster than using ImportString *)
importText[path_String, encoding_:"UTF8ISOLatin1"] := 
	Module[{str, strm, bytes},
		strm = OpenRead[path];
		If[FailureQ[strm], Return[$Failed]];
		str = Read[strm, Record, RecordSeparators -> {}];
		If[str === $Failed, Quiet @ Close[strm]; Return @ $Failed];
		If[str === EndOfFile, Quiet @ Close[strm]; Return @ {{}}];
		Close[strm];
		bytes = ToCharacterCode @ str;
		Quiet @ 
			If[encoding === "UTF8ISOLatin1", 
				Check[FromCharacterCode[bytes, "UTF8"], FromCharacterCode[bytes, "ISOLatin1"]]
				, 
				FromCharacterCode[bytes, encoding]
			]
	]
	
ExternalCSS[filepath_String] := 
	If[FailureQ[FindFile[filepath]],
		Message[Import::nffil, filepath, "CSS extraction"]; $Failed
		,
		With[{i = importText[filepath]}, If[FailureQ[i], $Failed, consumeStyleSheet @ CSSTokenize @ i]]
	]
		
InternalCSS[data_String] := consumeStyleSheet @ CSSTokenize @ data

RawCSS[filepath_String, opts___] := 
	Module[{raw},
		Block[{$RawImport = True}, raw = ExternalCSS[filepath]];
		If[TrueQ @ validCSSDataRawQ[raw] || MatchQ[raw, {}], raw, Failure["BadCSSFile", <||>]]
	]

InterpretedCSS[filepath_String, opts___] := 
	Module[{raw},
		raw = ExternalCSS[filepath];
		If[TrueQ @ validCSSDataBareQ[raw] || MatchQ[raw, {}], raw, Failure["BadCSSFile", <||>]]
	]


ProcessToStylesheet[filepath_String, opts___] :=
	Module[{raw, uniqueSelectors, allProcessed},
		raw = ExternalCSS[filepath];
		If[!validCSSDataBareQ[raw], Return @ Failure["BadCSSFile", <||>]];
		
		(* get all selectors preserving order, but favor the last entry of any duplicates *)
		uniqueSelectors = Reverse @ DeleteDuplicates[Reverse @ raw[[All, "Selector"]]];
		
		allProcessed = CSSCascade[All, All, raw, #]& /@ uniqueSelectors;
		(*TODO: convert options like FrameMargins to actual styles ala FrameBoxOptions -> {FrameMargins -> _}*)
		"Stylesheet" -> 
			NotebookPut @ 
				Notebook[
					MapThread[
						Cell[StyleData[#1], Sequence @@ #2]&, 
						{StringTrim @ Through[uniqueSelectors["String"]], allProcessed}], 
					StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
	]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
