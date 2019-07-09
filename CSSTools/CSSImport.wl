(* ::Package:: *)

(* ::Title:: *)
(*CSS 2.1 Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20190321*)
(*Version: 1*)


(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["CSSImport`", {"GeneralUtilities`", "Selectors3`"}];
Needs["CSSTools`CSSTokenizer`"];           (* keep tokenizer utilities hidden from $ContextPath *)
Needs["CSSTools`CSSPropertyInterpreter`"]; (* keep property interpreters hidden from $ContextPath *)

(* Selectors3` needed for Selector function *)

SetUsage[ResolveCSSCascade, "\
ResolveCSSCascade[type$, CSSData$, {selectors$, $$}] combines options that were interpreted from the CSS importer. \
CSS styles are merged following the CSS cascade and the resulting options are filtered by type$."];

SetUsage[ExtractCSSFromXML, "\
ExtractCSSFromXML[XMLObject$] imports the CSS declarations within XMLObject$."];

SetUsage[ApplyCSSToXML, "\
ApplyCSSToXML[XMLObject$, CSSData$] applies the CSSData$ to the symbolic XML, \
returning the CSSData$ with additional position and specificity information."];

SetUsage[ResolveCSSInterpretations, "\
ResolveCSSInterpretations[type$, CSSInterpretations$] combines options that were interpreted from the CSS importer. \
Any Left/Right/Bottom/Top and Min/Max values are merged."];

SetUsage[ResolveCSSInheritance, "\
ResolveCSSInheritance[target$, CSSData$] calculates the properties of the element at target$ including any inherited CSS properties."];

System`CellFrameStyle; (* needed in System` context *)
System`Box;

Begin["`Private`"];


(* ::Section::Closed:: *)
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


(* ::Section::Closed:: *)
(*Consume Token Sequences*)


(* 
	It is assumed that the string input has already been tokenized into CSS tokens.
	The main token consumers have the HoldFirst attribute. 
	This allows the position variable to be tracked continuously through each token consumer.
	Some token consumers also advance the position.
	
	The tokenizer and token accessor functions are defined in CSSTools`CSSTokenizer:
		CSSTokenType:      returns the type of token e.g. "ident", "number", "function"...
		CSSTokenString:    returns the string of the token which may have been normalized (removed escapes, possibly lowercase)
		CSSTokenValue:     returns the value of numeric tokens
		CSSTokenValueType: returns the numeric type of numeric tokens e.g. "number" or "integer"
		CSSDimensionUnit:  returns the units of dimension tokens e.g. "em", "px", "cm"...
*)


(* ::Subsection::Closed:: *)
(*Consume Style Sheet*)


(* Block is used such that the private variables pos, l, and tokens are known by any token consumer. *)
consumeStyleSheet[tokens:{__?CSSTokenQ}] :=
	Module[{pos = 1, l = Length[tokens], imports = {}, i = 1, lRulesets, rulesets},
		If[TrueQ @ $Debug, Echo[l, "Token Length"]];
		
		(* skip any leading whitespace (there shouldn't be any if @charset exists) *)
		If[TokenTypeIs[" ", pos, tokens], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		If[TrueQ @ $Debug, Echo[pos, "position"]];
		
		(* check for @charset rule *)
		If[TokenTypeIs["at-keyword", pos, tokens] && TokenStringIs["charset", pos, tokens], consumeAtCharsetKeyword[pos, l, tokens]];
		If[TrueQ @ $Debug, Echo[pos, "position after @charset check"]];
		
		(* check for @import rules *)
		While[TokenTypeIs["at-keyword", pos, tokens] && TokenStringIs["import", pos, tokens], 
			AppendTo[imports, consumeAtImportKeyword[pos, l, tokens]];
			If[TrueQ @ $Debug, Echo[pos, "position after @import check"]];
		];
		imports = Join @@ imports;
				
		lRulesets = Count[tokens, {"{}", ___}, {1}]; (* upper bound of possible rulesets *)
		rulesets = ConstantArray[0, lRulesets]; (* container for processed rulesets *)
		While[pos < l,
			If[TrueQ @ $Debug, Echo[pos, "position before rule"]];
			Which[
				(* any at-rule *)
				TokenTypeIs["at-keyword", pos, tokens], (*TODO*)consumeAtRule[pos, l, tokens],
				
				(* bad ruleset: missing a selector *)
				TokenTypeIs["{}", pos, tokens], AdvancePosAndSkipWhitespace[pos, l, tokens], 
				
				(* anything else treated as a ruleset *)
				True, rulesets[[i]] = consumeRuleset[pos, l, tokens]; i++;
			];
		];
		Join[imports, DeleteCases[rulesets, 0, {1}]]
	]


(* ::Subsection::Closed:: *)
(*Consume Style Sheet Preambles (charset, import)*)


SetAttributes[{consumeAtCharsetKeyword, consumeAtImportKeyword}, HoldFirst];


(* The character set is assumed UTF-8 and any charset is ignored. *)
consumeAtCharsetKeyword[pos_, l_, tokens_] :=
	Module[{},
		If[TokenTypeIsNot["at-keyword", pos, tokens] || TokenStringIsNot["charset", pos, tokens],
			Echo[Row[{"Expected @charset keyword. Had instead ", tokens[[pos]]}], "@charset error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; 
			Return @ Null;
		];
		pos++;
		If[MatchQ[tokens[[pos ;; pos + 2]], {" ", {"string", _}, ";"}],
			pos = pos + 3
			,
			(* invalid @charset *)
			AdvancePosToNextSemicolon[pos, l, tokens];
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
	]; 


consumeAtImportKeyword[pos_, l_, tokens_] :=  
	Module[{path, mediums, mediaStart, data},
		If[TokenTypeIsNot["at-keyword", pos, tokens] || TokenStringIsNot["import", pos, tokens],
			Echo[Row[{"Expected @import keyword. Had instead ", tokens[[pos]]}], "@import error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; Return @ {};
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		(* next token must be URL or string path to file *)
		If[TokenTypeIsNot["url" | "string", pos, tokens],
			Echo["Expected URL not found.", "@import error"];
			AdvancePosToNextSemicolon[pos, l, tokens]; AdvancePosAndSkipWhitespace[pos, l, tokens]; Return @ {};
		];
		path = CSSTokenString @ tokens[[pos]];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; 	
		If[TrueQ @ $Debug, Echo[pos, "position before @import media check"]];
		
		(* anything else is a comma-delimited set of media queries *)
		(*TODO: implement proper media queries *)
		mediums = {};
		While[TokenTypeIsNot[";", pos, tokens],
			mediaStart = pos;
			AdvancePosToNextSemicolonOrComma[pos, l, tokens];
			If[TrueQ @ $Debug, Echo[pos, "here"]];
			If[pos == l, Echo["Media query has no closing. Reached EOF.", "@import error"]; Return @ {}];
			AppendTo[mediums, CSSUntokenize @ tokens[[mediaStart ;; pos - 1]]];
			If[TokenTypeIs[";", pos, tokens],
				(* break out of media loop*)
				Break[] 
				, 
				(* skip comma only *)
				AdvancePosAndSkipWhitespace[pos, l, tokens] 
			]
		];
		AdvancePosAndSkipWhitespace[pos, l, tokens]; (* skip semicolon *)
				
		(* import without interpretation *)
		data = 
			With[{loc = FindFile[path]},
				If[FailureQ[loc], 
					Import[Echo[FileNameJoin[{Directory[], path}], "@import"], "Text"]
					,
					Import[Echo[loc, "@import"], "Text"]
				]
			];
		If[FailureQ[data],
			Return @ {}
			, 
			data = consumeStyleSheet @ CSSTokenize @ data;
			If[mediums =!= {}, data[[All, "Condition"]] = ConstantArray[mediums, Length[data]]];
			Return @ data
		]
	]	


(* ::Subsection::Closed:: *)
(*Consume Style Sheet Body (@rule, ruleset)*)


SetAttributes[{consumeAtRule, consumeRuleset}, HoldFirst];

consumeAtRule[pos_, l_, tokens_] :=
	Which[
		(* @import is not allowed after the top of the stylesheet, so skip them *)
		TokenStringIs["import", pos, tokens], 
			AdvancePosToNextSemicolon[pos, l, tokens]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens], 
			
		(* @page *)
		TokenStringIs["page", pos, tokens], consumeAtPageRule[pos, l, tokens];,
			
		(* @media *)
		TokenStringIs["media", pos, tokens], 
			AdvancePosToNextSemicolonOrBlock[pos, l, tokens]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens], 
			
		(* unrecognized @rule *)
		True, 
			AdvancePosToNextSemicolonOrBlock[pos, l, tokens]; 
			AdvancePosAndSkipWhitespace[pos, l, tokens]
	] 


consumeRuleset[pos_, l_, tokens_] :=
	Module[{selectorStartPos = pos, ruleset},
		AdvancePosToNextBlock[pos, l, tokens];
		ruleset = 
			<|
				"Selector" -> StringTrim @ CSSUntokenize @ tokens[[selectorStartPos ;; pos - 1]], 
				"Condition" -> None,
				(* The block token is already encapsulated {{}, CSSTokens...} *)
				"Block" -> consumeDeclarationBlock @ If[Length[tokens[[pos]]] > 1, tokens[[pos, 2 ;; ]], {}]|>; 
		(* return the formatted ruleset, but first make sure to skip the block *)
		AdvancePosAndSkipWhitespace[pos, l, tokens];
		ruleset
	]

consumeDeclarationBlock[{}] := {} 

consumeDeclarationBlock[blockTokens:{__?CSSTokenQ}] :=
	Module[{blockPos = 1, blockLength = Length[blockTokens], lDeclarations, i = 1, decStart, dec, validDeclarations},
		(* skip any initial whitespace *)
		If[TokenTypeIs[" ", blockPos, blockTokens], AdvancePosAndSkipWhitespace[blockPos, blockLength, blockTokens]]; 
		
		(*
			Each declaration is of the form 'property:value;'. The last declaration may leave off the semicolon.
			Like we did with parsing blocks, we count the number of colons as the upper limit of the number of declarations.
		*)
		lDeclarations = Count[blockTokens, ":"];
		validDeclarations = ConstantArray[0, lDeclarations];
		While[blockPos < blockLength && i <= lDeclarations,
			decStart = blockPos; AdvancePosToNextSemicolon[blockPos, blockLength, blockTokens];
			dec = consumeDeclaration[blockTokens[[decStart ;; blockPos]]];
			If[TrueQ @ $RawImport, 
				KeyDropFrom[dec, "Interpretation"]
				,
				AssociateTo[dec, "Interpretation" -> CSSTools`CSSPropertyInterpreter`Private`consumeProperty[dec["Property"], dec["Interpretation"]]]
			];
			If[!FailureQ[dec], validDeclarations[[i++]] = dec];
			(* skip over semi-colon *)
			AdvancePosAndSkipWhitespace[blockPos, blockLength, blockTokens]
		];					
		(* remove possible excess declarations *)
		DeleteCases[validDeclarations, 0, {1}]
	]
	
(* a declaration is "prop:val" or "prop:val !important" with optional semicolon if it is the last declaration *)
consumeDeclaration[decTokens:{__?CSSTokenQ}] :=
	Module[{decPos = 1, decLength = Length[decTokens], propertyPosition, valuePosition, important = False},
		(* check for bad property *)
		If[TokenTypeIsNot["ident", decPos, decTokens], Return @ $Failed];
		propertyPosition = decPos; AdvancePosAndSkipWhitespace[decPos, decLength, decTokens];
		
		(* check for EOF or missing colon *)
		If[decPos >= decLength || TokenTypeIsNot[":", decPos, decTokens], Return @ $Failed];
		AdvancePosAndSkipWhitespace[decPos, decLength, decTokens]; 
		valuePosition = decPos;
		
		(* remove trailing whitespace and possible trailing semi-colon*)
		decPos = decLength;
		If[TokenTypeIs[";", decPos, decTokens], 
			RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]
			,
			While[decPos > 1 && TokenTypeIs[" ", decPos, decTokens], decPos--];
			If[TokenTypeIs[";", decPos, decTokens], RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]];
		];
		
		(* check for !important token sequence *)
		If[TokenTypeIs["ident", decPos, decTokens] && TokenStringIs["important", decPos, decTokens], 
			RetreatPosAndSkipWhitespace[decPos, decLength, decTokens];
			If[TokenTypeIs["!", decPos, decTokens], important = True; RetreatPosAndSkipWhitespace[decPos, decLength, decTokens]]
		];
		
		With[
			{
				prop = CSSNormalizeEscapes @ ToLowerCase @ CSSTokenString @ decTokens[[propertyPosition]],
				(*check for empty property*)
				valueTokens = If[decPos < valuePosition, {}, decTokens[[valuePosition ;; decPos]]]
			},
			<|
				"Important" -> important,
				"Property" -> prop, 
				"Value" -> CSSUntokenize @ valueTokens,
				"Interpretation" -> valueTokens				|>
		]		
	]




(* ::Subsection::Closed:: *)
(*Merge Properties*)


expectedMainKeys     = {"Selector", "Condition", "Block"};
expectedMainKeysFull = {"Selector", "Specificity", "Targets", "Condition", "Block"};
expectedBlockKeys    = {"Important", "Property", "Value", "Interpretation"};
expectedBlockKeysRaw = {"Important", "Property", "Value"};

validCSSDataRawQ[data:{__Association}] := 
	And[
		AllTrue[Keys /@ data, MatchQ[expectedMainKeys]],
		AllTrue[Keys /@ Flatten @ data[[All, "Block"]], MatchQ[expectedBlockKeysRaw]]]
validCSSDataBareQ[data:{__Association}] := 
	And[
		AllTrue[Keys /@ data, MatchQ[expectedMainKeys]],
		AllTrue[Keys /@ Flatten @ data[[All, "Block"]], MatchQ[expectedBlockKeys]]]
validCSSDataFullQ[data:{__Association}] := 
	And[
		AllTrue[Keys /@ data, MatchQ[expectedMainKeysFull]],
		AllTrue[Keys /@ Flatten @ data[[All, "Block"]], MatchQ[expectedBlockKeys]]]
validCSSDataQ[data:{__Association}] := validCSSDataBareQ[data] || validCSSDataFullQ[data]
validCSSDataQ[___] := False

(* these include all inheritable options that make sense to pass on in a Notebook environment *)
notebookLevelOptions = 
	{
		Background, BackgroundAppearance, BackgroundAppearanceOptions, 
		FontColor, FontFamily, FontSize, FontSlant, FontTracking, FontVariations, FontWeight, 
		LineIndent, LineSpacing, ParagraphIndent, ShowContents, TextAlignment};
		
(* these include all options (some not inheritable in the CSS sense) that make sense to set at the Cell level *)
cellLevelOptions = 
	{
		Background, 
		CellBaseline, CellDingbat, CellMargins, 
		CellFrame, CellFrameColor, CellFrameLabelMargins, CellFrameLabels, CellFrameMargins, CellFrameStyle, 
		CellLabel, CellLabelMargins, CellLabelPositioning, CellLabelStyle, 
		CounterIncrements, CounterAssignments,
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
		SetterBox, Slider2DBox, SliderBox, TabViewBox, TogglerBox, TooltipBox};	
validExpressions =
	{
		ActionMenu, Animator, Button, Checkbox, ColorSetter, 
		Dynamic, DynamicWrapper, Frame, Graphics3D, Graphics, 
		Grid, InputField, Inset, Item, Locator, 
		LocatorPane, Opener, Overlay, Pane, Panel, 
		PaneSelector, PopupMenu, ProgressIndicator, RadioButton,
		Setter, Slider2D, Slider, TabView, Toggler, Tooltip};	
validBoxOptions =
	{
		Alignment, Appearance, Background, Frame, FrameMargins, FrameStyle, 
		FontTracking, ImageMargins, ImageSize, ImageSizeAction, Spacings, Scrollbars};
validBoxesQ = MemberQ[Join[validBoxes, validExpressions], #]&;

removeBoxOptions[allOptions_, boxes:{__?validBoxesQ}] :=
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


(* ResolveCSSInterpretations:
	1. Remove Missing and Failure interpretations.
	2. Filter the options based on Notebook/Cell/Box levels.
	3. Merge together Left/Right/Bottom/Top and Width/Height options.  *)
ResolveCSSInterpretations[type:(Cell|Notebook|Box|All), interpretationList_Dataset] :=
	ResolveCSSInterpretations[type, Normal @ interpretationList]
	
ResolveCSSInterpretations[type:(Cell|Notebook|Box|All), interpretationList_] := 
	Module[{valid, initialSet},
		valid = DeleteCases[Flatten @ interpretationList, _?FailureQ | _Missing, {1}];
		valid = Select[valid, 
			Switch[type, 
				Cell,      MemberQ[cellLevelOptions, #[[1]]]&, 
				Notebook,  MemberQ[notebookLevelOptions, #[[1]]]&,
				Box,      !MemberQ[optionsToAvoidAtBoxLevel, #[[1]]]&,
				All,       True&]];
		(* assemble options *)
		initialSet = assemble[#, valid]& /@ Union[First /@ valid];
		If[type === Box || type === All,
			removeBoxOptions[initialSet, validBoxes]
			,
			initialSet]
	]

ResolveCSSInterpretations[box:_?validBoxesQ, interpretationList_Dataset] := ResolveCSSInterpretations[{box}, Normal @ interpretationList]
ResolveCSSInterpretations[box:_?validBoxesQ, interpretationList_] := ResolveCSSInterpretations[{box}, interpretationList]	
ResolveCSSInterpretations[boxes:{__?validBoxesQ}, interpretationList_] := 
	Module[{valid, initialSet},
		valid = DeleteCases[Flatten @ interpretationList, _?FailureQ | _Missing, {1}];
		valid = Select[valid, !MemberQ[optionsToAvoidAtBoxLevel, #[[1]]]&];
		(* assemble options *)
		initialSet = assemble[#, valid]& /@ Union[First /@ valid];
		removeBoxOptions[initialSet, boxes /. Thread[validExpressions -> validBoxes]]				
	]


assembleLRBTDirectives[x_List] := 
	Module[{xLocal = Flatten @ x, r = {{Automatic, Automatic}, {Automatic, Automatic}}},
		With[{l = getSideFromLRBTDirective[Left,   xLocal]}, If[l =!= {}, r[[1, 1]] = setDirective @ l]]; 
		With[{l = getSideFromLRBTDirective[Right,  xLocal]}, If[l =!= {}, r[[1, 2]] = setDirective @ l]]; 
		With[{l = getSideFromLRBTDirective[Bottom, xLocal]}, If[l =!= {}, r[[2, 1]] = setDirective @ l]]; 
		With[{l = getSideFromLRBTDirective[Top,    xLocal]}, If[l =!= {}, r[[2, 2]] = setDirective @ l]]; 
		r
	]
	
getSideFromLRBTDirective[side:Left | Right | Bottom | Top, list_] := Reverse @ DeleteDuplicatesBy[Reverse[Join @@ Cases[list, side[___], {1}]], Head]

(* Directive does not always like Dynamic inside of it, so move it outside if it exists. *)
setDirective[(side:Left | Right | Bottom | Top)[        a___, (CSSBorderColor | CSSBorderStyle | CSSBorderWidth)[Dynamic[prop_]], b___] ] := setDirective[side[Dynamic[a, prop, b]]] 
setDirective[(side:Left | Right | Bottom | Top)[Dynamic[a___, (CSSBorderColor | CSSBorderStyle | CSSBorderWidth)[Dynamic[prop_]], b___]]] := setDirective[side[Dynamic[a, prop, b]]]
setDirective[(side:Left | Right | Bottom | Top)[Dynamic[a___, (CSSBorderColor | CSSBorderStyle | CSSBorderWidth)[        prop_ ], b___]]] := setDirective[side[Dynamic[a, prop, b]]]
setDirective[(side:Left | Right | Bottom | Top)[        a___, (CSSBorderColor | CSSBorderStyle | CSSBorderWidth)[        prop_ ], b___] ] := setDirective[side[        a, prop, b] ]

setDirective[(side:Left | Right | Bottom | Top)[Dynamic[a___]]] := Dynamic[Directive[a]]
setDirective[(side:Left | Right | Bottom | Top)[        a___] ] := Directive[a]
	
assembleLRBT[x_List] := 
	Module[{r = {{Automatic, Automatic}, {Automatic, Automatic}}},
		Map[
			With[{value = First[#]}, 
				Switch[Head[#], 
					Bottom | CSSHeightMin, r[[2, 1]] = value,
					Top    | CSSHeightMax, r[[2, 2]] = value,
					Left   | CSSWidthMin,  r[[1, 1]] = value,
					Right  | CSSWidthMax,  r[[1, 2]] = value]
			]&,
			Flatten[x]];
		r]

Clear[assemble]
assemble[opt:(FrameStyle | CellFrameStyle), rules_List] := 
	opt -> assembleLRBTDirectives @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]

assemble[opt:(FrameMargins | ImageMargins | CellMargins | CellFrameMargins), rules_List] := 
	opt -> assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]

assemble[opt:ImageSize, rules_List] := 
	opt -> Replace[assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], {x_, x_} :> x, {1}]
	 
assemble[opt:CellFrame, rules_List] := 
	opt -> Replace[assembleLRBT @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], CSSBorderWidth[x_] :> x, {2}]

assemble[opt:FontVariations, rules_List] := 
	opt -> DeleteDuplicatesBy[Flatten @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}], First]

(* not used much *)
assemble[opt:CellFrameColor, rules_List] := opt -> Last @ Cases[rules, HoldPattern[opt -> x_] :> x, {1}]

(* fallthrough *)
assemble[opt_, rules_List] := Last @ Cases[rules, HoldPattern[opt -> _], {1}]


(* ::Section::Closed:: *)
(*Main Functions*)


(* ::Subsection::Closed:: *)
(*ResolveCSSCascade*)


(* ResolveCSSCascade:
	1. Select the entries in the CSS data based on the provided selectors
	2. order the selectors based on specificity and importance (if those options are on)
	3. merge resulting list of interpreted options *)
Options[ResolveCSSCascade] = {"IgnoreSpecificity" -> False, "IgnoreImportance" -> False};

ResolveCSSCascade[box:_?validBoxesQ, CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] := 
	ResolveCSSCascade[{box}, CSSData, selectorList, opts]	

ResolveCSSCascade[boxes:{__?validBoxesQ}, CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] :=
	ResolveCSSCascade[boxes, CSSData, selectorList, opts]
	
ResolveCSSCascade[type:(Cell|Notebook|Box|All), CSSData_Dataset, selectorList:{__String}, opts:OptionsPattern[]] :=
	ResolveCSSCascade[type, Normal @ CSSData, selectorList, opts]

ResolveCSSCascade[type:(Cell|Notebook|Box|All), CSSData:{__Association} /; validCSSDataQ[CSSData], selectorList:{__String}, opts:OptionsPattern[]] :=
	Module[{interpretationList, specificities},
		(* start by filtering the data by the given list of selectors; ordering is maintained *)
		interpretationList = Select[CSSData, MatchQ[#Selector, Alternatives @@ selectorList]&];
		
		If[TrueQ @ OptionValue["IgnoreSpecificity"],
			(* if ignoring specificity, then leave the user-supplied selector list alone *)
			interpretationList = Flatten @ interpretationList[[All, "Block"]]
			,
			(* otherwise sort based on specificity but maintain order of duplicates; this is what should happen based on the CSS specification *)
			specificities = Selector["", #][["Specificity"]]& /@ interpretationList[[All, "Selector"]];
			interpretationList = Flatten @ interpretationList[[Ordering[specificities]]][[All, "Block"]];
		];
		
		(* Following CSS cascade spec:
			Move !important CSS properties to the end since they should override all other properties, but maintain their ordering.
		*)
		interpretationList = 
			Flatten @ 
				If[TrueQ @ OptionValue["IgnoreImportance"],
					interpretationList[[All, "Interpretation"]]
					,
					Join[Select[interpretationList, #Important == False&], Select[interpretationList, #Important == True&]][[All, "Interpretation"]]
				];
				
		(* now that the styles are all sorted, merge them *)
		ResolveCSSInterpretations[type, interpretationList]
	]

ResolveCSSCascade[___] := Failure["BadCSSData", <||>]


(* ::Subsection::Closed:: *)
(*Read styles from XMLObject*)


linkElementPattern :=
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

styleElementPattern :=
	XMLElement[
		x_String | {_, x_String} /; StringMatchQ[x, "style", IgnoreCase -> True], 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "type", IgnoreCase -> True]) -> 
				(attrVal_String /; StringMatchQ[attrVal, "text/css", IgnoreCase -> True]), 
			___}, 
		{css_String}
	] :> css
		
styleAttributePattern :=
	XMLElement[
		_, 
		{
			___, 
			(attr_String | {_, attr_String} /; StringMatchQ[attr, "style", IgnoreCase -> True]) -> css_, 
			___}, 
		___
	] :> css


ApplyCSSToXML[doc:XMLObject["Document"][___], CSSData_Dataset, wrapInDataset_:True] := ApplyCSSToXML[doc, Normal @ CSSData, wrapInDataset]
ApplyCSSToXML[doc:XMLObject["Document"][___], CSSData_?validCSSDataQ, wrapInDataset_:True] :=
	If[TrueQ @ wrapInDataset, Dataset, Identity][
		With[{t = Selector[doc, #Selector]}, 
			<|"Selector" -> #Selector, "Specificity" -> t[["Specificity"]], "Targets" -> t[["Elements"]], "Condition" -> #Condition, "Block" -> #Block|>
		]& /@ CSSData]
		
ApplyCSSToXML[_, CSSData_?validCSSDataQ, ___] := Failure["BadDoc", <|"Message" -> "Invalid XML document."|>]
ApplyCSSToXML[doc:XMLObject["Document"][___], ___] := Failure["BadData", <|"Message" -> "Invalid CSS data."|>]


Options[ExtractCSSFromXML] = {"RootDirectory" -> Automatic};

ExtractCSSFromXML[doc:XMLObject["Document"][___], opts:OptionsPattern[]] :=
	Module[
		{
			currentDir, externalSSPositions, externalSSContent, internalSSPositions, internalSSContent, 
			directStylePositions, directStyleContent, all},
			
		currentDir = Directory[];
		SetDirectory[If[OptionValue["RootDirectory"] === Automatic, Directory[], OptionValue["RootDirectory"]]]; 
		
		(* process externally linked style sheets via <link> elements *)
		externalSSPositions = Position[doc, First @ linkElementPattern];
		externalSSContent = ExternalCSS /@ Cases[doc, linkElementPattern, Infinity];
		(* filter out files that weren't found *)
		With[{bools =  # =!= $Failed& /@ externalSSContent},
			externalSSPositions = Pick[externalSSPositions, bools];
			externalSSContent = Pick[externalSSContent, bools];];
		externalSSContent = ApplyCSSToXML[doc, #, False]& /@ externalSSContent;
				
		(* process internal style sheets given by <style> elements *)
		internalSSPositions = Position[doc, First @ styleElementPattern];
		internalSSContent = InternalCSS /@ Cases[doc, styleElementPattern, Infinity];
		internalSSContent = ApplyCSSToXML[doc, #, False]& /@ internalSSContent;
		
		(* process internal styles given by 'style' attributes *)
		directStylePositions = Position[doc, First @ styleAttributePattern];
		directStyleContent = Cases[doc, styleAttributePattern, Infinity];
		directStyleContent = 
			MapThread[
				<|
					"Selector" -> None, 
					"Specificity" -> {1, 0, 0, 0}, 
					"Targets" -> {#1}, 
					"Condition" -> None, 
					"Block" ->  consumeDeclarationBlock @ CSSTokenize @ #2|>&, 
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


parents[x:{__Integer}] := Most @ Reverse @ NestWhileList[Drop[#, -2]&, x, Length[#] > 2&]

inheritedProperties = Pick[Keys@ #, Values @ #]& @ CSSPropertyData[[All, "Inherited"]];

ResolveCSSInheritance[position_Dataset, CSSData_] := ResolveCSSInheritance[Normal @ position, CSSData]
ResolveCSSInheritance[position_, CSSData_Dataset] := ResolveCSSInheritance[position, Normal @ CSSData]

(* ResolveCSSInheritance
	Based on the position in the XMLObject, 
	1. look up all ancestors' positions
	2. starting from the most ancient ancestor, calculate the styles of each ancestor, including inherited properties
	3. with all inheritance resolved, recalculate the style at the XMLObject position *)
ResolveCSSInheritance[position:{___?IntegerQ}, CSSData_?validCSSDataFullQ] :=
	Module[{lineage, data = CSSData, a, temp, temp2, i},
		(* order data by specificity *)
		data = data[[Ordering[data[[All, "Specificity"]]]]];
		
		(* *)
		lineage = Append[parents[position], position];
		a = <|Map[# -> <|"All" -> None, "Inherited" -> None|>&, lineage]|>;
		Do[
			(* get all CSS data entries that target the input position *)
			temp = Pick[data, MemberQ[#, i]& /@ data[[All, "Targets"]]];
			temp = Flatten @ temp[[All, "Block", All, {"Important", "Property", "Interpretation"}]];
			
			(* prepend all inherited properties from ancestors, removing possible duplicated inheritance *)
			temp2 = Join @@ Values @ a[[Key /@ parents[i], "Inherited"]];
			a[[Key[i], "All"]] = With[{values = Join[temp2, temp]}, Reverse @ DeleteDuplicates @ Reverse @ values];
			
			(* pass on any inheritable properties, but reset their importance so they don't overwrite later important props *)
			a[[Key[i], "Inherited"]] = Select[a[[Key[i], "All"]], MemberQ[inheritedProperties, #Property]&];
			With[{values = a[[Key[i], "Inherited", All, "Important"]]}, 
				a[[Key[i], "Inherited", All, "Important"]] = ConstantArray[False, Length[values]]
			];,
			{i, lineage}];
			
		(* return computed properties, putting important properties last *)
		Join[
			Select[a[[Key @ position, "All"]], #Important == False&][[All, "Interpretation"]], 
			Select[a[[Key @ position, "All"]], #Important == True& ][[All, "Interpretation"]]]
	]
	
ResolveCSSInheritance[position:{___?IntegerQ}, _] := Failure["BadData", <|"Message" -> "Invalid CSS data. CSS data must include specificity and target."|>]


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
		
		allProcessed = ResolveCSSCascade[All, raw, {#}]& /@ uniqueSelectors;
		(*TODO: convert options like FrameMargins to actual styles ala FrameBoxOptions -> {FrameMargins -> _}*)
		"Stylesheet" -> 
			NotebookPut @ 
				Notebook[
					MapThread[Cell[StyleData[#1], Sequence @@ #2]&, {uniqueSelectors, allProcessed}], 
					StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
	]

ImportExport`RegisterImport[
	"CSS",
	{
		"Elements" :> (("Elements" -> {"RawData", "Interpreted", "Stylesheet"})&),
		(* Interpreted is the same as default *)
		"Interpreted" :> (("Interpreted" -> With[{d = CSSImport`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&), 
		"RawData" :> (("RawData" -> With[{d = CSSImport`Private`RawCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&),
		"Stylesheet" :> CSSImport`Private`ProcessToStylesheet,
		((With[{d = CSSImport`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&)},
	{},
	"AvailableElements" -> {"Elements", "RawData", "Interpreted", "Stylesheet"}]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
