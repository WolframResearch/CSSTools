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

(* CSSTools`
	---> defines wrappers like CSSHeightMax *)
(* Selectors3` 
	---> defines CSSSelector function, consumeCSSSelector *)
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
	<<token>>["String"]     canonical string i.e. lower case and escape sequences are translated e.g. "\30 Red" --> "0red"
	<<token>>["RawString"]  original unaltered string
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
			If[TrueQ @ $Debug, Echo[pos, "position after @import check"]];
		];
		imports = Join @@ imports;
		
		(* check for @namespace rules *)
		(* These must appear after the @charset and @import rules and before rule sets *)
		While[TokenTypeIs["at-keyword"] && TokenStringIs["namespace"], 
			AppendTo[namespaces, consumeAtNamespaceKeyword[pos, l, tokens]]
		];
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
		rulesets = ConstantArray[0, lRulesets]; (* container for processed rulesets *)
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
			prefix = tokens[[pos]]["RawString"]; (* case-sensitive *)
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
		If[TokenTypeIsNot["delim", tokens[[pos]]] || TokenStringIsNot[";", tokens[[pos]]], 
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
		TokenStringIs["page", tokens[[pos]]], consumeAtPageRule[pos, l, tokens],
			
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
		(* a media query list is true of any component is true, and false only if all are false, so combine into Or *)
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
				Switch[ToLowerCase @ tokens[[pos]]["String"],
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


consumeAtPageRule[pos_, l_, tokens_] := 
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
					Switch[ToLowerCase @ tokens[[pos]]["String"],
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
		block = consumeAtPageBlock[tokens[[pos]]["Children"], pageSelectors];
		block[[All, "Condition"]] = Hold[CurrentValue[InputNotebook[], ScreenStyleEnvironment] === "Printout"];
		<|
			"Selector" -> "@page",
			"Block"    -> block|>	
	]


(* The @page {...} block contains only margin rules; CSS 2.1 does not allow specifying page size *)
consumeAtPageBlock[tokens:{___?CSSTokenQ}, scope_] :=
	Module[{pos = 1, l = Length[tokens], dec, decStart, decEnd, declarations = {}},
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]]; 
		
		While[pos <= l,
			If[TokenTypeIs["ident", tokens[[pos]]] && TokenStringIs["margin" | "margin-top" | "margin-bottom" | "margin-left" | "margin-right", tokens[[pos]]],
				decStart = decEnd = pos; AdvancePosToNextSemicolon[decEnd, l, tokens];
				dec = consumeDeclaration[tokens[[decStart ;; decEnd]]];
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
		ruleset = 
			<|
				"Selector" -> consumeCSSSelector[tokens[[selectorStartPos ;; pos - 1]], namespaces], 
				(* The block token is already encapsulated CSSToken[<|"Type" -> {}, "Children" -> {CSSTokens...}|>] *)
				"Block" -> consumeDeclarationBlock @ If[Length[tokens[[pos]]["Children"]] > 1, tokens[[pos]]["Children"], {}]|>; 
		(* return the formatted ruleset, but first make sure to skip the block *)
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		ruleset
	]


consumeDeclarationBlock[{}] := {} 

consumeDeclarationBlock[blockTokens:{__?CSSTokenQ}] :=
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
			dec = consumeDeclaration[blockTokens[[decStart ;; blockPos]]];
			If[!FailureQ[dec], validDeclarations[[i++]] = dec];
			(* skip over semi-colon *)
			AdvancePosAndSkipWhitespace[blockPos, blockLength, blockTokens]
		];					
		(* remove possible excess declarations *)
		DeleteCases[validDeclarations, 0, {1}]
	]


(* a declaration is "prop:val" or "prop:val !important" with optional semicolon if it is the last declaration *)
consumeDeclaration[decTokens:{__?CSSTokenQ}] :=
	Module[{decPos = 1, decLength = Length[decTokens], propertyPosition, valuePosition, important = False, declaration},
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
		If[TokenTypeIs["ident", decTokens[[decPos]]] && TokenStringIs["important", decTokens[[decPos]]], 
			RetreatPosAndSkipWhitespace[decPos, decLength, decTokens];
			If[TokenTypeIs["delim", decTokens[[decPos]]] && TokenStringIs["!", decTokens[[decPos]]], 
				important = True; RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]
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
			AssociateTo[declaration, "Interpretation" -> consumeProperty[declaration["Property"], declaration["Interpretation"]]]
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
validCSSBlockQ[data:{__Association}]     := AllTrue[Keys /@ data, MatchQ[expectedBlockKeys]]
validCSSBlockFullQ[{}]                   := True
validCSSBlockFullQ[data:{__Association}] := AllTrue[Keys /@ data, MatchQ[expectedBlockKeysFull]]

validCSSDataRawQ[data:{__Association}]  := And[AllTrue[Keys /@ data, MatchQ[expectedMainKeys]],     validCSSBlockQ[Flatten @ data[[All, "Block"]]]]
validCSSDataBareQ[data:{__Association}] := And[AllTrue[Keys /@ data, MatchQ[expectedMainKeys]],	    validCSSBlockFullQ[Flatten @ data[[All, "Block"]]]]
validCSSDataFullQ[data:{__Association}] := And[AllTrue[Keys /@ data, MatchQ[expectedMainKeysFull]],	validCSSBlockFullQ[Flatten @ data[[All, "Block"]]]]

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
(* Assembling general CSS properties into FE options. This is the most general case. *)
assemble[prop_?StringQ, scope_, CSSBlockData_] :=
	Module[{validBlockData, optionNames},
		(* only look at declarations that match 'prop' *)
		validBlockData = Select[CSSBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&];
		(* filter FE options in the remaining declaration interpretations to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		assembleByFEOption[#, validBlockData]& /@ optionNames
	]

(* 
	The FE options that take option values of the form {{left, right}, {bottom, top}} need to be assembled
	from each side's individual CSS cascade. If the input is specific to one side, the other sides are 
	filled in with Automatic. The 'prop' input is assumed normalized to lowercase. 
	If media conditions (that contain Which) are detected, then the entire expression is wrapped in Dynamic. *)
assembleLRBT[prop_?StringQ, LRBTList_] :=
	Which[
		StringContainsQ[prop, "top"], 
			Replace[
				LRBTList, 
				p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
					If[FreeQ[p, Which], {{Automatic, Automatic}, {Automatic, t}}, Dynamic[{{Automatic, Automatic}, {Automatic, t}}]]],
		StringContainsQ[prop, "bottom"], 
			Replace[
				LRBTList, 
				p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
					If[FreeQ[p, Which], {{Automatic, Automatic}, {b, Automatic}}, Dynamic[{{Automatic, Automatic}, {b, Automatic}}]]],
		StringContainsQ[prop, "left"], 
			Replace[
				LRBTList, 
				p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
					If[FreeQ[p, Which], {{l, Automatic}, {Automatic, Automatic}}, Dynamic[{{l, Automatic}, {Automatic, Automatic}}]]],
		StringContainsQ[prop, "right"], 
			Replace[
				LRBTList, 
				p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
					If[FreeQ[p, Which], {{Automatic, r}, {Automatic, Automatic}}, Dynamic[{{Automatic, r}, {Automatic, Automatic}}]]],
		True,
			Replace[LRBTList, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]
	]


(* Assembling background properties into FE options:
	CSS background properties are often just using the shorthand 'background'. *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "background", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp},
		(* check whether input is a valid background long-form properties *)
		If[!MatchQ[prop, "background" | "background-attachment" | "background-color" | "background-image" | "background-position" | "background-repeat"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized background property.", "Prop" -> prop|>]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'background' shorthand *)
		validBlockData = 
			Select[
				CSSBlockData, 
				If[StringMatchQ[prop, "background"],
					StringStartsQ[#Property, "background"] (* If only asking for 'background', include every background long-form property. *)
					,
					Or[
						StringMatchQ[#Property, prop],        (* always include literal property *)
						StringMatchQ[#Property, "background"] (* always include 'background' shorthand *)
					]
				]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		temp = assembleByFEOption[#, validBlockData]& /@ optionNames;
		Switch[prop,
			"background-color",    Select[temp, MatchQ[#, Background -> _]&],
			"background-image",    Select[temp, MatchQ[#, System`BackgroundAppearance -> _]&],
			"background-position", Select[temp, MatchQ[#, System`BackgroundAppearanceOptions -> _]&],
			"background-repeat",   Select[temp, MatchQ[#, System`BackgroundAppearanceOptions -> _]&],
			_,                     temp
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
			If[!MissingQ[value],
				If[KeyExistsQ[value, "Style"] && value["Style"] === None && Length[value] > 1, value = KeyDropFrom[value, "Style"]];
				Directive @@ Values @ value
				,
				value
			]
		]
	]

assemble[
	Condition[
		inputProp_?StringQ,
		And[
			StringStartsQ[StringTrim @ inputProp, "border", IgnoreCase -> True],
			!StringMatchQ[StringTrim @ inputProp, "border-spacing" | "border-collapse", IgnoreCase -> True]]], 
	scope_, 
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp},
		(* check whether input is a valid border long-form properties *)
		If[!MatchQ[prop, 
				"border" | "border-top" | "border-bottom" | "border-right" | "border-left" |
				"border-color" | "border-style" | "border-width" |
				"border-top-style"    | "border-top-width"    | "border-top-color" |
				"border-bottom-style" | "border-bottom-width" | "border-bottom-color" |
				"border-left-style"   | "border-left-width"   | "border-left-color" |
				"border-right-style"  | "border-right-width"  | "border-right-color"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized border property.", "Prop" -> prop|>]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'border' shorthand; include matched side shorthand; include any color/style/width shorthand *)
		validBlockData = 
			Select[
				CSSBlockData, 
				Which[
					StringMatchQ[prop, "border"],
						StringStartsQ[#Property, "border"] (* If only asking for 'border', include every border property. *)
					,
					StringMatchQ[prop, "border-style"],
						StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "style"] (* If only asking for 'border-style', include every border style property. *)
					,
					StringMatchQ[prop, "border-width"],
						StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "width"] (* If only asking for 'border-width', include every border width property. *)
					,
					StringMatchQ[prop, "border-color"],
						StringStartsQ[#Property, "border"] && StringEndsQ[#Property, "color"] (* If only asking for 'border-color', include every border color property. *)
					,
					True,
						Or[
							StringMatchQ[#Property, prop],     (* always include literal property *)
							StringMatchQ[#Property, "border"], (* always include 'border' shorthand *)
							If[StringStartsQ[prop, "border-top"],    StringMatchQ[#Property, "border-top"],    False], (* possibly include matched side shorthands *)
							If[StringStartsQ[prop, "border-left"],   StringMatchQ[#Property, "border-left"],   False], 
							If[StringStartsQ[prop, "border-right"],  StringMatchQ[#Property, "border-right"],  False],
							If[StringStartsQ[prop, "border-bottom"], StringMatchQ[#Property, "border-bottom"], False], 
							If[StringEndsQ[prop, "color"],           StringMatchQ[#Property, "border-color"],  False], (* possibly include color/style/width shorthands *)
							If[StringEndsQ[prop, "style"],           StringMatchQ[#Property, "border-style"],  False],
							If[StringEndsQ[prop, "width"],           StringMatchQ[#Property, "border-width"],  False]
						]
				]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		
		Function[{x},
			x ->
				Which[
					MatchQ[x, _?frameStyleOptionQ], (* FrameStyle, CellFrameStyle *)
						temp = assembleWithConditions[validBlockData, x, Hold[#], modifyValueToDirective]& /@ {"Left", "Right", "Bottom", "Top"};
						assembleLRBT[prop, temp]
					,
					MatchQ[x, CellFrame],
						temp = assembleWithConditions[validBlockData, x, Hold[#, "Width"], #&]& /@ {"Left", "Right", "Bottom", "Top"};
						assembleLRBT[prop, temp]
					,
					True, 
						temp = {Hold[Automatic], Hold[Automatic], Hold[Automatic], Hold[Automatic]};
						assembleLRBT[prop, temp]
				]
		] /@ optionNames				
	]
	
	
(* Assembling font properties into FE options:
	CSS font properties are often just using the shorthand 'font'. *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "font", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp},
		(* check whether input is a valid font long-form properties *)
		If[!MatchQ[prop, "font" | "font-family" | "font-size" | "font-style" | "font-variant" | "font-weight"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized font property.", "Prop" -> prop|>]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'font' shorthand *)
		validBlockData = 
			Select[
				CSSBlockData, 
				If[StringMatchQ[prop, "font"],
					StringStartsQ[#Property, "font"] (* If only asking for 'font', include every font long-form property. *)
					,
					Or[
						StringMatchQ[#Property, prop],  (* always include literal property *)
						StringMatchQ[#Property, "font"] (* always include 'font' shorthand *)
					]
				]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		temp = assembleByFEOption[#, validBlockData]& /@ optionNames;
		Switch[prop,
			"font-family",  Select[temp, MatchQ[#, FontFamily -> _]&],
			"font-size",    Select[temp, MatchQ[#, FontSize -> _]&],
			"font-style",   Select[temp, MatchQ[#, FontSlant -> _]&],
			"font-variant", Select[temp, MatchQ[#, FontVariations -> _]&],
			"font-weight",  Select[temp, MatchQ[#, FontWeight -> _]&],
			_,              temp
		]
	]
	
	
(* Assembling list-style properties into FE options *)
assemble[
	inputProp_?StringQ /; StringStartsQ[StringTrim @ inputProp, "list", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp},
		(* check whether input is a valid list-style long-form properties *)
		If[!MatchQ[prop, "list-style" | "list-style-image" | "list-style-position" | "list-style-type"], 
			Return @ Failure["BadProp", <|"Message" -> "Unrecognized ist-style property.", "Prop" -> prop|>]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'list-style' shorthand *)
		validBlockData = 
			Select[
				CSSBlockData, 
				If[StringMatchQ[prop, "list-style"],
					StringStartsQ[#Property, "list-style"] (* If only asking for 'list-style', include every list-style long-form property. *)
					,
					Or[
						StringMatchQ[#Property, prop],        (* always include literal property *)
						StringMatchQ[#Property, "list-style"] (* always include 'list-style' shorthand *)
					]
				]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		temp = assembleByFEOption[#, validBlockData]& /@ optionNames;
		(* The only FE option that 'list-style' translates to is CellDingbat, so no filtering is needed here. *)
		temp
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
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp, mainProp},
		(* check whether input is a valid margin/padding long-form properties *)
		mainProp = If[StringStartsQ[prop, "margin"], "margin", "padding"];
		If[!MatchQ[prop, Alternatives[#, # <> "-top", # <> "-bottom", # <> "-right", # <> "-left"]& @ mainProp], 
			Return @ Failure["BadProp", <|"MessageTemplate" -> "Unrecognized `main` property.", "MessageParameters" -> <|"main" -> mainProp|>, "Prop" -> prop|>]];
		
		(* only look at declarations that match 'prop', including relevant shorthand properties *)
		(* always include 'margin'/'padding' shorthand *)
		validBlockData = 
			Select[
				CSSBlockData, 
				If[StringMatchQ[prop, mainProp],
					StringStartsQ[#Property, mainProp] (* If only asking for 'margin'/'padding', include every margin/padding property. *)
					,
					Or[
						StringMatchQ[#Property, prop],    (* always include literal property *)
						StringMatchQ[#Property, mainProp] (* always include 'margin'/'padding' shorthand *)
					]
				]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		
		Function[{x},
			x ->
				Which[
					MatchQ[x, _?marginOptionQ], (* CellFrameMargins, FrameMargins, ImageMargins, CellMargins *)
						temp = assembleWithConditions[validBlockData, x, Hold[#], #&]& /@ {"Left", "Right", "Bottom", "Top"};
						assembleLRBT[prop, temp]
					,
					MatchQ[x, PrintingOptions], 
						PrintingOptions /. assembleByFEOption[PrintingOptions, validBlockData]
					,
					True, 
						temp = {Hold[Automatic], Hold[Automatic], Hold[Automatic], Hold[Automatic]};
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
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = StringTrim @ ToLowerCase @ inputProp, mainProp},
		(* check whether input is a valid margin/padding long-form properties *)
		mainProp = If[StringEndsQ[prop, "height"], "height", "width"];
		If[!MatchQ[prop, Alternatives[#, "min-" <> #, "max-" <> #]& @ mainProp], 
			Return @ Failure["BadProp", <|"MessageTemplate" -> "Unrecognized `main` property.", "MessageParameters" -> <|"main" -> mainProp|>, "Prop" -> prop|>]];
		
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
	and takes only those that match the given FE option. The rest are deleted. 
	Assuming the sorting has put the most important option at the top, only the first element is taken. 
	'Normal' is used to convert suboption associations into lists e.g. FontVariations -> <|"CapsType" -> ...|> *)
assembleByFEOption[FEopt_, CSSBlockData_?validCSSBlockFullQ] := FEopt -> First[Normal @ DeleteMissing[#[FEopt]& /@ CSSBlockData[[All, "Interpretation"]]], Automatic]


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
Because of the imported CSS data, it behaves is if all rules come from a single style sheet (origin).
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


(* CSSCascade:
	1. Select the entries in the CSS data based on the provided selectors
	2. order the selectors based on specificity and importance (if those options are on)
	3. merge resulting list of interpreted options 
Arg1: scope
	CSS lacks scope in that all most properties can apply to all boxes.
	WD has options that apply to specific levels e.g. Cell/Notebook/Box/All. 
	This argument can also take a list of boxes e.g. ActionMenuBox or a list of box generators e.g. ActionMenu.
Arg2:  
*)	
ClearAll[CSSCascade];
Options[CSSCascade] = {"IgnoreSpecificity" -> False, "IgnoreImportance" -> False};

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
	Module[{props, selectorList, atRules, sel, dataSubset, specificities, declarations, resolvedOptions},
		(* check for empty data *)
		If[CSSData === {}, Return @ {}];
		
		(* expand any inputs *)
		props = 
			StringTrim @ ToLowerCase @ 
				Which[
					MatchQ[inputProps, All],     Union @ Flatten @ CSSData[[All, "Block", All, "Property"]], 
					MatchQ[inputProps, _?ListQ], Union @ Flatten @ inputProps,
					True,                        Union @ Flatten @ {inputProps}
				];
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
		sel = Replace[selectorList, s_?StringQ :> CSSSelector[s], {1}];
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
		
		(* add @page declarations; they should only apply at Notebook scope *)
		(* FIXME: in paged module 3 the pages have their own specificity. Page specificity has not been implemented *)
  		atRules = Select[selectorList, StringQ[#] && StringStartsQ[#, "@"]&];
  		If[MemberQ[atRules, s_ /; StringStartsQ[s, "@page"]],
  			dataSubset = Select[CSSData, MatchQ[#Selector, s_?StringQ /; StringStartsQ[s, "@page"]]&];
  			declarations = Join[Flatten @ dataSubset[[All, "Block"]], declarations]
  		];
  		
  		(* Remove props that failed to parse or are unsupported. *)
  		declarations = Select[declarations, Not[FailureQ[#Interpretation] || MissingQ[#Interpretation]]&];
  		
		(* 
			Following CSS cascade spec, move !important CSS properties to the end 
			since they should override all other properties, but maintain their ordering. 
			Then reverse the order so most important declaration is now first (so we can later scan through the list from the start). *)
		declarations = 
			Reverse @ Flatten @ 
				If[Not[TrueQ @ OptionValue["IgnoreImportance"]],
					Join[
						Select[declarations, #Important == False&], 
						Select[declarations, #Important == True&]]
					,
					declarations
				];
		
		(* assemble declarations into FE options *)
		resolvedOptions = Flatten[assemble[#, scope, declarations]& /@ props];
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
			{CSSData, CSSTargets[doc, CSSData[[All, "Selector"]]]}] (* defined in CSSSelectors3 *)
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
					"Targets"  -> {#1}, 
					"Block"    -> consumeDeclarationBlock @ CSSTokenize @ #2|>&, 
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
	1. look up all ancestors' positions
	2. starting from the most ancient ancestor, calculate the styles of each ancestor, including inherited properties
	3. with all inheritance resolved, recalculate the style at the XMLObject position *)

(* normalize Dataset position input *)
CSSInheritance[position_Dataset, scope_, CSSData_] := CSSInheritance[Normal @ position, scope, CSSData]
CSSInheritance[position_, scope_, CSSData_Dataset] := CSSInheritance[position, scope, Normal @ CSSData]

(* main function *)
CSSInheritance[position:{___?IntegerQ}, scope_, CSSData_?validCSSDataFullQ] :=
	Module[{lineage, CSSDataSubset, i, inheritableProps, previous, current},
		(* get the position of all ancestors *)
		lineage = parents[position];
		
		(* Perform inheritance at each ancestor:
			1. perform CSS cascade to get computed value of all inheritable properties
			2. for any properties that do not have values, get them from the previous ancestor if possible 
			3. repeat at each generation until a final set of properties is obtained *)
		current = <|"Selector" -> CSSSelector["dummy"], "Targets" -> {}, "Block" -> {}|>;
		Do[
			(* get recent ancestor's inheritable declarations *)
			previous = current;
			
			(* get all CSS data entries that target the input position *)
			CSSDataSubset = Pick[CSSData, MemberQ[#, i]& /@ CSSData[[All, "Targets"]]];
			
			(* order the subset by specificity and prepend the subset with the recent ancestor's result *)
			CSSDataSubset = CSSDataSubset[[Ordering[Through[CSSDataSubset[[All, "Selector"]]["Specificity"]]]]];
			CSSDataSubset = Prepend[CSSDataSubset, previous];
			
			(* select from this CSS data subset those declarations that have inheritable properties *)
			inheritableProps = Union @ Flatten @ CSSDataSubset[[All, "Block", All, "Property"]];
			inheritableProps = Pick[inheritableProps, MemberQ[inheritedProperties[], #] & /@ inheritableProps];
			
			(* Build the current generation's dummy ruleset:
				In "Interpretation", perform the cascade but ignore specificity since we already ordered by it earlier.
				Convert the list of options back into the expected associations. 
				This is easy since none of the current options have non-standard suboptions like "Left"/"Right"/"Bottom"/"Top" or "Min"/"Max". *)
			current = 
				<|
					"Selector" -> CSSSelector["dummy"], 
					"Targets"  -> {}, 
					"Block" -> 
						Flatten[{
							<|
								"Property" -> #, "Value" -> None, "Important" -> False, 
								"Interpretation" -> CSSCascade[#, scope, CSSDataSubset, All, "IgnoreSpecificity" -> True] //. HoldPattern[{x__Rule}] :> With[{v = <|x|>}, v /; True], 
								"Condition" -> None
							|>& /@ inheritableProps}]|>;
			,
			{i, lineage}];
			
		(* perform cascade on the final generation *)
		CSSDataSubset = Pick[CSSData, MemberQ[#, position]& /@ CSSData[[All, "Targets"]]];
		CSSDataSubset = CSSDataSubset[[Ordering[Through[CSSDataSubset[[All, "Selector"]]["Specificity"]]]]];
		CSSDataSubset = Prepend[CSSDataSubset, current];
		CSSCascade[All, scope, CSSDataSubset, All, "IgnoreSpecificity" -> True]
	]
	
CSSInheritance[position:{___?IntegerQ}, _] := 
	Failure["BadData", <|"Message" -> "Invalid CSS data. CSS data must include specificity and target."|>]


parents[x:{__Integer}] := Most @ Reverse @ NestWhileList[Drop[#, -2]&, x, Length[#] > 2&]

inheritedProperties[] := Pick[Keys @ #, Values @ #]& @ CSSPropertyData[[All, "Inherited"]];


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
		Message[Import::nffil, "CSS extraction"]; $Failed
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
		
		allProcessed = CSSCascade[All, All, raw, uniqueSelectors];
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
