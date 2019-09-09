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
getSideFromLRBTDirective;
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
					dec = dec /. head:((Left | Right | Bottom | Top)[_]) :> scope[head];
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
		If[!KeyExistsQ["Interpretation"] || FreeQ[declaration["Interpretation"], ImageMargins], Return @ declaration];
		value = Flatten[{"PrintingMargins" /. PrintingOptions /. declaration["Interpretation"]}];
		(* CSS 2.1 does not allow ex or em lengths *)
		If[!FreeQ[value, FontSize | "FontXHeight"], Return @ Failure["BadLength", <|"Message" -> "Page margins cannot us 'em' or 'ex' units."|>]];
		value = 
			Replace[
				value, 
				{
					(h:Left | Right)[Scaled[x_]] :> h @ Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 1}]],
					(h:Top | Bottom)[Scaled[x_]] :> h @ Dynamic[x*CurrentValue[EvaluationNotebook[], {PrintingOptions, "PaperSize", 2}]]},
				{1}
			];
		<|
			declaration, 
			"Interpretation" -> 
				PrintingOptions -> {
					Which[
						MatchQ[scope, Left | Right], "InnerOuterMargins" -> {FirstCase[value, x:Left[_] :> x, Nothing], FirstCase[value, x:Right[_] :> x, Nothing]},
						MatchQ[scope, First],        Missing["Not supported."],
						True,                        "PrintingMargins" -> value
					]}|>
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


validCSSBlockQ[data:{__Association}]     := AllTrue[Keys /@ data, MatchQ[expectedBlockKeys]]
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


(* 
	Media queries add conditions to the normal CSS cascade. There may be multiple valid conditions. 
	Option values and their conditions are combined into a held Which expression. *)
assembleGeneralCase[CSSBlockData_?validCSSBlockFullQ, option_, Hold[subKeys___], modifyValueFunction_Function] :=
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
	
assemble[prop_?StringQ, scope_, CSSBlockData_] :=
	Module[{validBlockData, optionNames},
		(* only look at declarations that match 'prop' *)
		validBlockData = Select[CSSBlockData, StringMatchQ[#Property, prop, IgnoreCase -> True]&];
		(* filter FE options in the remaining declaration interpretations to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		assembleByFEOption[#, validBlockData]& /@ optionNames
	]
	
assemble[
	inputProp_?StringQ /; StringMatchQ[StringTrim @ inputProp, "border" | "border-top" | "border-bottom" | "border-right" | "border-left", IgnoreCase -> True], 
	scope_, 
	CSSBlockData_
] :=
	Module[{validBlockData, optionNames, temp, prop = ToLowerCase @ inputProp},
		(* only look at declarations that match 'prop' *)
		validBlockData = Select[CSSBlockData, StringContainsQ[#Property, prop | "border"]&];
		(* filter FE options to the set scope *)
		optionNames = filterOptionNames[scope, validBlockData];
		(* run the FE option assembly function *)
		
		Function[{x},
			Which[
				MatchQ[x, _?frameStyleOptionQ],
					temp = assembleGeneralCase[validBlockData, x, Hold[#], modifyValueToDirective]& /@ {"Left", "Right", "Bottom", "Top"};
				,
				MatchQ[x, CellFrame],
					temp = assembleGeneralCase[validBlockData, x, Hold[#, "Width"], #&]& /@ {"Left", "Right", "Bottom", "Top"}
				,
				True, 
					temp = {Hold[Automatic], Hold[Automatic], Hold[Automatic], Hold[Automatic]}
			]
		] /@ optionNames;
		Which[
			StringContainsQ[prop, "top"], 
				Replace[
					temp, 
					p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
						If[FreeQ[p, Which], {{Automatic, Automatic}, {Automatic, t}}, Dynamic[{{Automatic, Automatic}, {Automatic, t}}]]],
			StringContainsQ[prop, "bottom"], 
				Replace[
					temp, 
					p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
						If[FreeQ[p, Which], {{Automatic, Automatic}, {b, Automatic}}, Dynamic[{{Automatic, Automatic}, {b, Automatic}}]]],
			StringContainsQ[prop, "left"], 
				Replace[
					temp, 
					p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
						If[FreeQ[p, Which], {{l, Automatic}, {Automatic, Automatic}}, Dynamic[{{l, Automatic}, {Automatic, Automatic}}]]],
			StringContainsQ[prop, "right"], 
				Replace[
					temp, 
					p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> 
						If[FreeQ[p, Which], {{Automatic, r}, {Automatic, Automatic}}, Dynamic[{{Automatic, r}, {Automatic, Automatic}}]]],
			True, True(*START HERE*)
		]		
	]

frameStyleOptionQ[input_] := MemberQ[{CellFrameStyle, FrameStyle}, input]
frameStyleOptionQ[___] := False

modifyValueToDirective = 
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

assembleLRBTDirectives[CSSBlockData_?validCSSBlockFullQ, option_?frameStyleOptionQ] :=
	Module[{temp},
		temp = assembleGeneralCase[CSSBlockData, option, Hold[#], modifyValueToDirective]& /@ {"Left", "Right", "Bottom", "Top"};
		Replace[temp, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]
	]


marginOptionQ[input_] := MemberQ[{CellFrameMargins, FrameMargins, ImageMargins, CellMargins, CellFrame}, input]
marginOptionQ[___] := False

assembleLRBTPadding[CSSBlockData_?validCSSBlockFullQ, option_?marginOptionQ] :=
	Module[{temp},
		temp = assembleGeneralCase[CSSBlockData, option, Hold[#, If[option === CellFrame, "Width", Unevaluated[Sequence[]]]], #&]& /@ {"Left", "Right", "Bottom", "Top"};
		Replace[temp, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]
	]
	

sizeOptionQ[input_] := MemberQ[{ImageSize}, input]
sizeOptionQ[___] := False		

assembleLRBTSizing[CSSBlockData_?validCSSBlockFullQ, option_?sizeOptionQ] :=
	Module[{temp},
		temp = {
			assembleGeneralCase[CSSBlockData, option, Hold["Width",  "Min"], #&],
			assembleGeneralCase[CSSBlockData, option, Hold["Width",  "Max"], #&],
			assembleGeneralCase[CSSBlockData, option, Hold["Height", "Min"], #&],
			assembleGeneralCase[CSSBlockData, option, Hold["Height", "Max"], #&]};
		(* convert to simplified forms if possible, adding Dynamic if needed *)
		Which[
			temp[[1]] === temp[[2]] && temp[[3]] === temp[[4]],
				Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {wn, hn}, Dynamic[{wn, hn}]]],
			True,
				Replace[temp, p:{Hold[wn_], Hold[wx_], Hold[hn_], Hold[hx_]} :> If[FreeQ[p, Which], {{wn, wx}, {hn, hx}}, Dynamic[{{wn, wx}, {hn, hx}}]]]
		]
	]
	

modifyValueIfTextDecoration = Function[{v, data}, If[MissingQ[v] && data["Property"] === "text-decoration", Automatic, v]];

assembleFontVariations[CSSBlockData_?validCSSBlockFullQ, option:FontVariations] :=
	Module[{suboptions},
		suboptions = DeleteMissing[#[option] & /@ CSSBlockData[[All, "Interpretation"]], Infinity];
		suboptions = Union @ Flatten[Keys /@ suboptions];
		(# -> Replace[assembleGeneralCase[CSSBlockData, option, Hold[#], modifyValueIfTextDecoration], {Hold[x__] :> Dynamic[x]}])& /@ suboptions
	]

ClearAll[assembleByFEOption];
assembleByFEOption[opt_?frameStyleOptionQ,  CSSBlockData_?validCSSBlockFullQ] := opt -> assembleLRBTDirectives[CSSBlockData, opt]
assembleByFEOption[opt_?marginOptionQ, CSSBlockData_?validCSSBlockFullQ] := opt -> assembleLRBTPadding[CSSBlockData, opt]
assembleByFEOption[opt_?sizeOptionQ,   CSSBlockData_?validCSSBlockFullQ] := opt -> assembleLRBTSizing[CSSBlockData, opt]
assembleByFEOption[opt:FontVariations, CSSBlockData_?validCSSBlockFullQ] := opt -> assembleFontVariations[CSSBlockData, opt]

(* CellFrameColor is only used in the outline property, which is not supported *)
(* (*TODO*) assembleByFEOption[opt:CellFrameColor, CSSBlockData_?validCSSBlockFullQ] := opt -> assembleLRBTDirectives[CSSBlockData, opt] *)

assembleSubOption[CSSBlockData_?validCSSBlockFullQ, option:PrintingOptions, subOption:"PrintingMargins"] :=
	Module[{temp},
		temp = assembleGeneralCase[CSSBlockData, option, Hold[subOption, #], #&]& /@ {"Left", "Right", "Bottom", "Top"};
		Replace[temp, p:{Hold[l_], Hold[r_], Hold[b_], Hold[t_]} :> If[FreeQ[p, Which], {{l, r}, {b, t}}, Dynamic[{{l, r}, {b, t}}]]]
	]
	
assembleSubOption[CSSBlockData_?validCSSBlockFullQ, option:PrintingOptions, subOption_] :=
	Module[{temp},
		temp = assembleGeneralCase[CSSBlockData, option, Hold[subOption], #&];
		subOption -> Replace[temp, p:Hold[x___] :> If[FreeQ[p, Which], x, Dynamic[x]]]
	]
	
assembleByFEOption[opt:PrintingOptions, CSSBlockData_?validCSSBlockFullQ] := 
	Module[{allSubOptions},
		allSubOptions = Union @ Flatten[Keys /@ DeleteMissing[#[opt] & /@ CSSBlockData[[All, "Interpretation"]]]];
		opt -> (assembleSubOption[CSSBlockData, opt, #]& /@ allSubOptions)
	]

(* fallthrough *)
assembleByFEOption[opt_, CSSBlockData_?validCSSBlockFullQ] := opt -> First[DeleteMissing[#[opt]& /@ CSSBlockData[[All, "Interpretation"]]], Automatic]


(* ::Section:: *)
(*Main Functions*)


(* ::Subsection::Closed:: *)
(*ResolveCSSCascade*)


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


(* ResolveCSSCascade:
	1. Select the entries in the CSS data based on the provided selectors
	2. order the selectors based on specificity and importance (if those options are on)
	3. merge resulting list of interpreted options 
Arg1: scope
	CSS lacks scope in that all most properties can apply to all boxes.
	WD has options that apply to specific levels e.g. Cell/Notebook/Box/All. 
	This argument can also take a list of boxes e.g. ActionMenuBox or a list of box generators e.g. ActionMenu.
Arg2:  
*)	
ClearAll[ResolveCSSCascade];
Options[ResolveCSSCascade] = {"IgnoreSpecificity" -> False, "IgnoreImportance" -> False};

(* normalize Dataset input *)
ResolveCSSCascade[props_, scope_, CSSData_Dataset, selectorList_, opts:OptionsPattern[]] := 
	ResolveCSSCascade[props, scope, Normal @ CSSData, selectorList, opts]

ResolveCSSCascade[{}, scope_, CSSData_, selectorList_, opts:OptionsPattern[]] := {}

(* main function *)
ResolveCSSCascade[
	inputProps:_?StringQ | {__?StringQ} | All, 
	scope:(Cell | Notebook | Box | All | None | _?validBoxesQ | {__?validBoxesQ}), 
	CSSData_?validCSSDataQ, 
	inputSelectorList:_?(Function[CSSSelectorQ[#] || StringQ[#]]) | {__?(Function[CSSSelectorQ[#] || StringQ[#]])} | All, 
	opts:OptionsPattern[]
] :=
	Module[{props, selectorList, atRules, sel, dataSubset, specificities, declarations, optionNames, resolvedOptions},
		(* expand any inputs *)
		props = 
			Which[
				inputProps === All,          Union @ Flatten @ CSSData[[All, "Block", All, "Property"]], 
				MatchQ[inputProps, _?ListQ], inputProps,
				True,                        {inputProps}
			];
		selectorList = 
			Which[
				MatchQ[inputSelectorList, All],     CSSData[[All, "Selector"]],
				MatchQ[inputSelectorList, _?ListQ], inputSelectorList,
				True,                               {inputSelectorList}
			];
		
		(* start by filtering the data by the given list of selectors; ordering is maintained *)
		(* match against the tokenized selector sequence, which should be unique *)
		(* upgrade any string selectors to CSSSelector objects *)
		(* exceptions: @page rules *)
		sel = Select[selectorList, (StringQ[#] && !StringStartsQ[#, WhitespaceCharacter... ~~ "@"] || MatchQ[#, _CSSSelector])&];
		sel = Replace[selectorList, s_?StringQ :> CSSSelector[s], {1}];
		If[AnyTrue[sel, _?FailureQ], Return @ FirstCase[sel, _?FailureQ]];
		dataSubset = 
 			Select[
 				CSSData, 
  				MatchQ[#Selector["Sequence"], Alternatives @@ Through[sel["Sequence"]]] &];
  		
		If[TrueQ @ OptionValue["IgnoreSpecificity"],
			(* if ignoring specificity, then leave the user-supplied selector list alone *)
			declarations = Flatten @ dataSubset[[All, "Block"]]
			,
			(* otherwise sort based on specificity but maintain order of duplicates; this is what should happen based on the CSS specification *)
			specificities = Through[dataSubset[[All, "Selector"]]["Specificity"]];
			declarations = Flatten @ dataSubset[[Ordering[specificities]]][[All, "Block"]];
		];
		
		(* add @page at rules, should only apply at Notebook scope *)
		(* FIXME: in paged module 3 the pages have their own specificity. Page specificity has not been implemented *)
  		atRules = Select[selectorList, StringQ[#] && StringStartsQ[#, WhitespaceCharacter... ~~ "@"]&];
  		If[MemberQ[atRules, s_ /; StringStartsQ[s, WhitespaceCharacter... ~~ "@page", IgnoreCase -> True]],
  			dataSubset = Select[CSSData, MatchQ[#Selector, s_?StringQ /; StringStartsQ[s, WhitespaceCharacter... ~~ "@page", IgnoreCase -> True]]&];
  			declarations = Join[Flatten @ dataSubset[[All, "Block"]], declarations]
  		];
  		
  		(* Remove props that failed to parse or are unsupported. *)
  		declarations = Select[declarations, Not[FailureQ[#Interpretation] || MissingQ[#Interpretation]]&];
  	
  		(* filter properties involved in this cascade, expanding any short-hand properties *)
  		(*declarations = Pick[declarations, StringMatchQ[#Property, Alternatives @@ props, IgnoreCase -> True] & /@ declarations];*)
  		  		
		(* 
			Following CSS cascade spec, move !important CSS properties to the end 
			since they should override all other properties, but maintain their ordering. *)
		declarations = 
			Flatten @ 
				If[Not[TrueQ @ OptionValue["IgnoreImportance"]],
					Join[
						Select[declarations, #Important == False&], 
						Select[declarations, #Important == True&]]
				];
		
		(* Reverse the order so most important declaration is now first (so we can later scan through the list from the start). *)		
		declarations = Reverse @ declarations;
						
		(* assemble declarations into FE options *)
		resolvedOptions = assemble[#, scope, declarations]& /@ props;
		Flatten @ Which[
			MatchQ[scope, Box | All],
				movePseudoOptionsIntoBoxOptions[resolvedOptions, validBoxes],
			MatchQ[scope, _?validBoxesQ | {__?validBoxesQ}],
				movePseudoOptionsIntoBoxOptions[resolvedOptions, If[ListQ[scope], scope, {scope}] /. Thread[validBoxGenerators -> validBoxes]],
			True,
				resolvedOptions
		]		
	]

ResolveCSSCascade[props_, scope_, ___] /; !MatchQ[scope, (Cell | Notebook | Box | All | None | _?validBoxesQ | {__?validBoxesQ})] := 
	Failure["BadScope", <|"Message" -> "Second argument expected as All, Notebook, Cell, Box or box generator."|>]
ResolveCSSCascade[props_, scope_, CSSData_ /; !validCSSDataQ[CSSData], ___] := 
	Failure["BadCSSData", <|"Message" -> "CSS data appears invalid."|>]
(*ResolveCSSCascade[props_, scope_, _, _, ___] := 
	Failure["BadCSSData", <|"Message" -> "Fourth argument should be a list of CSSSelectors or strings."|>]*)


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
					"Block"    ->  consumeDeclarationBlock @ CSSTokenize @ #2|>&, 
				{directStylePositions, directStyleContent}];
		
		(* combine all CSS sources based on position in XMLObject *)
		all =
			Flatten @ 
				Part[
					Join[externalSSContent, internalSSContent, directStyleContent],
					Ordering @ Join[externalSSPositions, internalSSPositions, directStylePositions]];
		SetDirectory[currentDir];
		Dataset @ all		
	]


(* ::Subsection::Closed:: *)
(*ResolveCSSInheritance*)


(* ResolveCSSInheritance
	Based on the position in the XMLObject, 
	1. look up all ancestors' positions
	2. starting from the most ancient ancestor, calculate the styles of each ancestor, including inherited properties
	3. with all inheritance resolved, recalculate the style at the XMLObject position *)

(* normalize Dataset position input *)
ResolveCSSInheritance[position_Dataset, CSSData_] := ResolveCSSInheritance[Normal @ position, CSSData]

(* normalize CSS Dataset input *)
ResolveCSSInheritance[position_, CSSData_Dataset] := ResolveCSSInheritance[position, Normal @ CSSData]

ResolveCSSInheritance[position:{___?IntegerQ}, CSSData_?validCSSDataFullQ] :=
	Module[{lineage, data = CSSData, a, temp, temp2, i, inheritableProps},
		(* order data by specificity *)
		data = data[[Ordering[Through[data[[All, "Selector"]]["Specificity"]]]]];
		
		(* *)
		lineage = Append[parents[position], position];
		a = <|Map[# -> <|"All" -> None, "Inherited" -> None|>&, lineage]|>;
		Do[
			(* get all CSS data entries that target the input position *)
			temp = Pick[data, MemberQ[#, i]& /@ data[[All, "Targets"]]];
			
			(* select from CSS data subset those that have inheritable properties *)
			inheritableProps = Union @ Flatten @ temp[[All, "Block", All, "Property"]];
			inheritableProps = Pick[inheritableProps, MemberQ[inheritedProperties[], #] & /@ inheritableProps];
			temp = Pick[temp, MemberQ[#[["Block", All, "Property"]], Alternatives @@ inheritableProps] & /@ temp];
			
			(* resolve the CSS cascade for the inheritable properties *)
			temp = Flatten @ temp[[All, "Block", All, {"Important", "Property", "Interpretation"}]];
			
			(* prepend all inherited properties from ancestors, removing possible duplicated inheritance *)
			temp2 = Join @@ Values @ a[[Key /@ parents[i], "Inherited"]];
			a[[Key[i], "All"]] = With[{values = Join[temp2, temp]}, Reverse @ DeleteDuplicates @ Reverse @ values];
			
			(* pass on any inheritable properties, but reset their importance so they don't overwrite later important props *)
			a[[Key[i], "Inherited"]] = Select[a[[Key[i], "All"]], MemberQ[inheritedProperties[], #Property]&];
			With[{values = a[[Key[i], "Inherited", All, "Important"]]}, 
				a[[Key[i], "Inherited", All, "Important"]] = ConstantArray[False, Length[values]]
			];,
			{i, lineage}];
			
		(* return computed properties, putting important properties last *)
		Join[
			Select[a[[Key @ position, "All"]], #Important == False&][[All, "Interpretation"]], 
			Select[a[[Key @ position, "All"]], #Important == True& ][[All, "Interpretation"]]]
	]
	
ResolveCSSInheritance[position:{___?IntegerQ}, _] := 
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
		
		allProcessed = ResolveCSSCascade[All, raw, uniqueSelectors];
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
