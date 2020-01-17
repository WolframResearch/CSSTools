(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["CSSTools`CSSSelectors3`", {"CSSTools`"}];

consumeCSSSelector;

Needs["CSSTools`CSSTokenizer`"];  

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Namespace and Language*)


(*
	CSS namespaces are declared in at-keywords e.g. "@namespace prefix:URI" at the start of the CSS document.
	We assume the @namespace CSS namespaces are processed and passed as an option to this package in the form of a list of associations, one per namespace
		{<|"Prefix" -> string, "Namespace" -> string, "Default" -> True|False|>, ...}
	Namespace prefixes:
	  * They are case sensitive
	  * The CSS and document's namespace prefixes do not need to match
	  * Prefixes are resolved into the full URIs during parsing
	  * The URIs must match between CSS and the document
	If a CSS namespace prefix in a selector does not match one of the CSS namespace declarations, then the selector is invalid.
	If a CSS namespace URI does not match one of the document's namespace URIs, then the selector is valid but does not target any element in the document.
*)

(*
	Documents are assumed to have head XMLObject or XMLElement.
	The inclusion of namespace depends on how the string was parsed to symbolic XML. 
	We will try our best to guess the namespace, but we should ask the user to re-import ambiguous documents with option '"IncludeNamespaces" -> True'.
	If the document was imported with '"IncludeNamespaces" -> False' then we're mostly out of luck. At best we can determine the root default namespace.
*)
getDocumentNamespaces[document_] := 
	Module[{default, namespaces, root, rootPos},
		(* 
			If '"IncludeNamespaces" -> True' or 'Automatic', then non-default namespaces can be gathered from elements where they're defined.
			The defining element has the namespace in both the attribute and the type.
			If '"IncludeNamespaces" -> False', then there's not enough information to unambiguously get the namespace attribute.
			Some patterns may find duplicate namespaces, but a final Union elimnates duplicates.
		*)
		namespaces = 
			Join[
				(*True*) 
				Transpose @ 
					{
						Cases[document, XMLElement[{ns_String, _String}, {___, {_String, _String} -> ns_String, ___}, _] :> ns, Infinity],
						Position[document, XMLElement[{x_String, _String}, {___, {_String, _String} -> y_String, ___}, _] /; StringMatchQ[x, y]]},
				(*Automatic*)
				Transpose @ 
					{
						Cases[document, XMLElement[_, {___, {"http://www.w3.org/2000/xmlns/", "xmlns"} -> ns_String, ___}, _] :> ns, Infinity],
						Position[document, XMLElement[_, {___, {"http://www.w3.org/2000/xmlns/", "xmlns"} -> _String, ___}, _]]},
				(*False*)
				Transpose @ 
					{
						Cases[document, XMLElement[_String, {___, "xmlns" -> ns_String, ___}, _] :> ns, Infinity],
						Position[document, XMLElement[_String, {___, "xmlns" -> _String, ___}, _]]}];
		namespaces = Map[<|"Namespace" -> #[[1]], "Element" -> #[[2]]|>&, namespaces];
		
		(* 
			A default namespace is assumed to be defined in the root element which can be overlooked by the above Cases.
			At most only one of the 'Cases' below should return a non-empty list since each pattern is unique.
		*)
		root = Extract[document, rootPos = First[Sort @ Position[document, XMLElement[__]], {}](*$DocumentRootPosition*)];
		default = 
			Join[
				(*True*)     Cases[root, XMLElement[{ns_String, _String}, {___, {_String, _String} -> ns_String, ___}, _] :> ns, {0}],
				(*Automatic*)Cases[root, XMLElement[_String, {___, {x_String /; !StringMatchQ[x, ""], "xmlns"} -> ns_String, ___}, _] :> ns, {0}],
				(*False*)    Cases[root, XMLElement[_String, {___, HoldPattern["xmlns" -> ns_String], ___}, _] :> ns, {0}]];
		If[default =!= {}, default = {<|"Namespace" -> First[default], "Element" -> rootPos(*$DocumentRootPosition*)|>}];
		
		Union @ Join[default, namespaces]
	]
	
getNamespaceOfDocumentElement[XMLElement[{ns_String, type_String}, __]] := ns

getNamespaceOfDocumentElement[XMLElement[__]] := {}

getNamespaceOfDocumentElement[elemPos:{_Integer..}] := 
	Module[{allowedMatches},
		(* first check whether namespace is directly defined *)
		With[{ns = getNamespaceOfDocumentElement[Extract[$Document, elemPos]]}, If[StringQ[ns], Return[ns]]];
		
		(* otherwise, check ancestors for namespace *)
		allowedMatches = Pick[$DocumentNamespaces, isDescendant[elemPos, #]& /@ $DocumentNamespaces[[All, "Element"]]];
		allowedMatches = First[TakeLargestBy[allowedMatches, Length[#["Element"]]&, UpTo[1]], {}];
		If[allowedMatches === {}, "", allowedMatches["Namespace"]]
	]


isDescendant[pos1:{_Integer..}, pos2:{_Integer..}] := Length[pos1] > Length[pos2] && pos1[[;;Length[pos2]]] === pos2


(*
	XML uses 'xml:lang' to declare langage, HTML uses 'lang'
	A document's language is inherited and usually defined in the root element.
	Other elements can override the language attribute by defining a new language value.
*)
getDocumentLanguages[document_] := 
	Module[{positions, languages},
		positions = Position[document, HoldPattern[({_String, l_String} | l_String) -> _String] /; StringMatchQ[l, "lang", IgnoreCase -> $IgnoreCase["AttributeName"]]];
		languages = Transpose @ {Extract[document, Append[#, 2]& /@ positions], positions[[All, ;;-3]]};
		Map[<|"Language" -> #[[1]], "Element" -> #[[2]]|>&, languages]
	]
	
getLanguageOfDocumentElement[elemPos:{_Integer..}] := 
	Module[{allowedMatches},
		(* check ancestors for language *)
		allowedMatches = Pick[$DocumentLanguages, isDescendant[elemPos, #]& /@ $DocumentLanguages[[All, "Element"]]];
		allowedMatches = First[TakeLargestBy[allowedMatches, Length[#["Element"]]&, UpTo[1]], {}];
		If[allowedMatches === {}, "", allowedMatches["Language"]]
	]


(* ::Section::Closed:: *)
(*Consume CSS tokens*)


(* ::Subsection::Closed:: *)
(*Error Highlighting*)


untokenizeWithoutComments[tokens_] := StringReplace[CSSUntokenize @ tokens, "/**/" -> ""]

showError[startPos_, stopPos_, tokens_] :=
	If[Length[tokens] < 3,
		Style[
			untokenizeWithoutComments @ tokens,
			Background -> Yellow]
		,
		Row[{
			untokenizeWithoutComments @ tokens[[;; startPos - 1]],
			Style[
				untokenizeWithoutComments @ tokens[[startPos ;; stopPos]],
				Background -> Yellow],
			untokenizeWithoutComments @ tokens[[stopPos + 1 ;; ]]
		}]
	]
	
modifyError[fail_Failure, pos_, tokens_] :=
	Failure[fail[[1]], <|
		"Message"  -> fail[[2, "Message"]], 
		"Position" -> showError[pos, pos, tokens]|>]


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
		If[posCheck <= l,
			Or[
				TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["*", tokens[[posCheck]]], 
				TokenTypeIs["ident", tokens[[posCheck]]]]
			,
			False
		]
	]
	

isNoNamespace[pos_, l_, tokens_] :=
	Module[{posCheck = pos},
		If[TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["|", tokens[[posCheck]]],
			posCheck++
			,
			Return @ False
		];
		If[posCheck <= l,
			Or[
				TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs["*", tokens[[posCheck]]], 
				TokenTypeIs["ident", tokens[[posCheck]]]]
			,
			False
		]
	]
	
	
SetAttributes[{consumeNamespace, consumeDefaultNamespace}, HoldFirst]
consumeNamespace[pos_, l_, tokens_, namespaces_] := 
	Module[{ns},
		(* get case-sensitive namespace from declared namespaces *)
		With[{s = tokens[[pos]]["RawString"]}, 
			ns = FirstCase[namespaces, _?(Function[#Prefix === s])];
			(* a selector with an undeclared namespace prefix is invalid *)
			If[MissingQ[ns], 
				Throw @ 
					Failure["BadNamespace", <|
						"Message"  -> "Not a declared namespace.",
						"Position" -> showError[pos, pos, tokens]|>]
				,
			ns = ns["Namespace"]];							
		];
		pos++; 
		pos++; (* skip "|" delim token *)
		ns
	]
	
consumeDefaultNamespace[pos_, l_, tokens_, namespaces_] :=
	Module[{ns},
		ns = FirstCase[namespaces, _?(Function[#Default === True])];
		If[MissingQ[ns], ns, ns["Namespace"]] (* could be Missing if not declared, which is an ambiguous state *)
	]


(* ::Subsection::Closed:: *)
(*Type or Universal*)


SetAttributes[{consumeTypeOrUniversalSelector},	HoldFirst]
consumeTypeOrUniversalSelector[pos_, l_, tokens_, namespace_, Hold[specificity_]] :=
	Module[{value},
		value = 
			Which[
				TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["*", tokens[[pos]]], 
					"Universal" -> <|"Namespace" -> namespace|>,
				TokenTypeIs["ident", tokens[[pos]]], 
					specificity["c"]++;
					"Type" -> <|
						"Namespace" -> namespace,
						"Name"      -> tokens[[pos]]["RawString"]|>, (* doc language determines case, so leave this alone *)
				True,
					Throw @  
						Failure["BadSimpleSelector", <|
							"Message"  -> "Expected Type or Universal simple selector.",
							"Position" -> showError[pos, pos, tokens]|>]
			];
		pos++;
		value		
	]
	
	
(* ::Subsection::Closed:: *)
(*Combinator*)


SetAttributes[{consumeCSSCombinator}, HoldFirst]
consumeCSSCombinator[pos_, l_, tokens_] :=
	Module[{value},
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		If[TokenTypeIs["delim", tokens[[pos]]],
			value = 
				Switch[tokens[[pos]]["String"],
					"+", "Combinator" -> <|"Type" -> "NextSibling"|>,
					"~", "Combinator" -> <|"Type" -> "SubsequentSibling"|>,
					">", "Combinator" -> <|"Type" -> "Child"|>,
					_,   
						Throw @
							Failure["BadCombinator", <|
								"Message" -> "Unrecognized combinator.", 
								"Position" -> showError[pos, pos, tokens]|>]
				];
			AdvancePosAndSkipWhitespace[pos, l, tokens]
			,
			value = "Combinator" -> <|"Type" -> "Descendant"|>
		];
		value
	]
	
	
(* ::Subsection::Closed:: *)
(*Attribute*)


(* For any errors, to get the error position correct we need to modify it outside of this function. *)
consumeAttributeSelector[tokens:{}, namespaces_] := 
	Throw @ Failure["BadAttribute", <|"Message"  -> "Empty attribute selector."|>]

consumeAttributeSelector[tokens:{__?CSSTokenQ}, namespaces_] :=
	Module[{pos, l, ns, attrib, match, value},
		(* remove any trailing whitespace by adjusting token count *)
		pos = Length[tokens];
		If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]];
		l = pos; pos = 1;
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		(* consume initial ident with possible namespace *)
		If[pos <= l, 
			ns = 
				Which[
					isNoNamespace[pos, l, tokens], pos++; None,
					isNamespace[pos, l, tokens],   consumeNamespace[pos, l, tokens, namespaces], 
					True,                          None (* default namespaces do not apply to attribute selectors *)
				];
			,
			Throw @ Failure["BadAttribute", <|"Message"  -> "Empty attribute selector."|>]
		];
		If[pos <= l,
			attrib = 
				If[TokenTypeIs["ident", tokens[[pos]]], 
					tokens[[pos]]["RawString"] (* case depends on document language, so keep this as the raw string *)
					,
					Throw @ 
						Failure["BadAttribute", <|
							"Message"  -> "Attribute selector should start with an ident token."|>]
				];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
		];
		If[pos > l,
			(* if no more tokens exist, then exit as is*)
			"Attribute" -> <|
				"Namespace" -> ns,
				"Name"      -> attrib,
				"Match"     -> "All",
				"Value"     -> Missing[]|>
			, (*ELSE*)
			(* consume match operator *)
			If[pos <= l,
				match = 
					Which[
						TokenTypeIs["delim", tokens[[pos]]] && TokenStringIs["=", tokens[[pos]]], "Exact",
						TokenTypeIs["dash-match",      tokens[[pos]]], "Dash",
						TokenTypeIs["prefix-match",    tokens[[pos]]], "Prefix",
						TokenTypeIs["include-match",   tokens[[pos]]], "Include",
						TokenTypeIs["suffix-match",    tokens[[pos]]], "Suffix",
						TokenTypeIs["substring-match", tokens[[pos]]], "Substring",
						True, 
							Throw @ 
								Failure["BadAttribute", <|
									"Message" -> "Attribute selector has unknown match operator."|>]
					];
				,
				Throw @ Failure["BadAttribute", <|"Message" -> "Attribute selector missing match operator."|>]
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* consume attribute value *)
			If[pos <= l,
				value =
					Switch[tokens[[pos]]["Type"], (* case depends on document language, so keep this as the raw string *)
						"ident",  tokens[[pos]]["RawString"],
						"string", tokens[[pos]]["String"],
						_,        
							Throw @ 
								Failure["BadAttribute", <|
									"Message" -> "Attribute value should be a string or ident token.",
									"String"  -> CSSUntokenize @ tokens|>]	
					];
				,
				Throw @ Failure["BadAttribute", <|"Message" -> "Missing attribute value."|>]
			];
			AdvancePosAndSkipWhitespace[pos, l, tokens];
			(* check for non-whitespace at the end *)
			If[pos <= l, 
				Throw @ Failure["BadAttribute", <|"Message"  -> "Attribute selector has too many tokens."|>]
				,
				"Attribute" -> <|
					"Namespace" -> ns,
					"Name"      -> attrib,
					"Match"     -> match,
					"Value"     -> value|>
			]
		]		
	]
	

(* ::Subsection::Closed:: *)
(*Class*)


isClassSelector[pos_, l_, tokens_] :=
	Module[{posCheck = pos},
		If[TokenTypeIs["delim", tokens[[posCheck]]] && TokenStringIs[".", tokens[[posCheck]]],
			posCheck++
			,
			Return @ False
		];
		posCheck <= l && TokenTypeIs["ident", tokens[[posCheck]]]
	]


(* 
	The class selector e.g. ".value" is a shortcut for [class~=value]. 
	The shorcut syntax makes it impossible to assign a namespace. 
	Following the XML recommendation for attribute selectors, the shortcut notation implies no namespace. 
	
	In XML, the class attribute can be defined to have an attribute name other than "class".
	As this would depend on a definition within the document, it is outside the scope of this token consumer.
	We assume the attriute name is "class" but this could be changed during document parsing. *)
SetAttributes[{consumeClassSelector}, HoldFirst]
consumeClassSelector[pos_, l_, tokens_] :=
	Module[{value},
		pos++;
		value = 
			"Class" -> <|
				"Namespace" -> None,
				"Name"      -> "class",
				"Match"     -> "Include",
				"Value"     -> tokens[[pos]]["RawString"]|>; (* case-sensitivity outside the scope of CSS *)
		pos++;
		value
	]


(* ::Subsection::Closed:: *)
(*ID*)


(* 
	The ID selector e.g. "#value" is an approximate shortcut for [name=value] with the additional restriction
	that only one instance of this id value may appear in the document, regardless of the element type that carries it.
	Documents may be non-conformant; in this case the first element found is used.
	
	HTML assumes the attribute name "id".
	In XML, the ID selector can be defined to have an attribute name other than "id".
	As this would depend on a definition within the document, it is outside the scope of this token consumer.
	We assume the attriute name is "id" but this could be changed during document parsing.
	If the attribute name for the ID selector is suspected to be unknown, CSS authors are encouraged to use 
	the [name=value] syntax. 
	
	Unlike the class selector (which implies no namespace for the attribute name),
	if an element has multiple ID attributes, all of them must be treated as IDs for that element for the purposes 
	of the ID selector. Such a situation could be reached using mixtures of xml:id, DOM, XML DTDs, 
	and namespace-specific knowledge. 
	I believe this implies that the ID selector attribute name belongs in all namespaces, even no namespace.
	*)
SetAttributes[{consumeIDSelector}, HoldFirst]
consumeIDSelector[pos_, l_, tokens_] :=
	Module[{value},
		If[tokens[[pos]]["Flag"] =!= "id", 
			Throw @ 
				Failure["BadID", <|
					"Message"  -> "Not a valid ID selector.",
					"Position" -> showError[pos, pos, tokens]|>]];
		value = 
			"ID" -> <|
				"Namespace" -> All,
				"Name"      -> "id",
				"Match"     -> "Exact",
				"Value"     -> tokens[[pos]]["RawString"]|>; (* case-sensitivity outside the scope of CSS *)
		pos++;
		value
	]
	
	
(* ::Subsection::Closed:: *)
(*Pseudo Element*)


isPseudoElement[pos_, l_, tokens_] :=
	Module[{posCheck = pos},
		If[TokenTypeIs["colon", tokens[[posCheck]]], posCheck++, Return @ False];
		If[posCheck <= l,
			If[TokenTypeIs["colon", tokens[[posCheck]]], 
				posCheck++; 
				posCheck <= l && TokenTypeIs["ident", tokens[[posCheck]]]
				,
				And[
					TokenTypeIs["ident", tokens[[posCheck]]],
					TokenStringIs["first-line"|"first-letter"|"before"|"after", tokens[[posCheck]]]]
			]
			,
			False
		]
	]
	
	
SetAttributes[{consumePseudoElementSelector}, HoldFirst]
consumePseudoElementSelector[pos_, l_, tokens_] :=
	Module[{value},
		If[TokenTypeIs["colon", tokens[[pos]]], pos++];
		If[TokenTypeIs["colon", tokens[[pos]]], pos++];
		value = "PseudoElement" -> <|"Name" -> CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"]|>;
		pos++;
		value
	]
	
	
(* ::Subsection::Closed:: *)
(*Pseudo Class*)


SetAttributes[{consumePseudoClassSelector}, HoldFirst]
consumePseudoClassSelector[pos_, l_, tokens_, namespaces_, Hold[specificity_]] :=
	Module[{value},
		If[TokenTypeIs["colon", tokens[[pos]]], pos++];
		value =
			Switch[tokens[[pos]]["Type"],
				"ident",    
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						Alternatives[
							"link", "visited", "hover", "active", "focus", "target",
							"enabled", "disabled", "checked", "indeterminate", "root",
							"first-child", "last-child", "first-of-type", "last-of-type",
							"only-child", "only-of-type", "empty"
						],
							specificity["b"]++;
							"PseudoClass" -> <|"Name" -> CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"]|>,
						_, 
							Throw @ Failure["BadPseudoClass", <|"Message"  -> "Unrecognized pseudo class selector."|>]
					]
				,
				"function", 
					Switch[CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"],
						"lang", 
							specificity["b"]++;
							"PseudoClass" -> <|
								"Name"  -> "lang",
								"Value" -> CSSUntokenize @ tokens[[pos]]["Children"]|>,
						Alternatives[
							"nth-child", "nth-last-child", 
							"nth-of-type", "nth-last-of-type"
						],
							specificity["b"]++;
							"PseudoClass" -> <|
								"Name" -> CSSNormalizeEscapes @ ToLowerCase @ tokens[[pos]]["String"], 
								"ANB"  -> consumeANB[tokens[[pos]]["Children"]]|>
						,
						"not", consumeNegationPseudoClass[tokens[[pos]]["Children"], namespaces, Hold[specificity]]
						,
						_, Throw @ Failure["BadPseudoClass", <|"Message"  -> "Unrecognized pseudo class selector."|>]
					]
				,
				_, Throw @ Failure["BadPseudoClass", <|"Message"  -> "Pseudo class selector should be a function or ident token."|>]
			];
		pos++;
		value
	]
	
consumeANB[tokens:{__?CSSTokenQ}] := parseANB @ untokenizeWithoutComments @ tokens

(* The An+B argument is its own micro-grammar, but we can reuse some of the regexs from the tokenizer. *)
parseANB[s_String] :=
	Module[{x = StringTrim @ s},
		Which[
			StringMatchQ[x, RegularExpression[RE["O"] ~~ RE["D"] ~~ RE["D"]]],            {2, 1},
			StringMatchQ[x, RegularExpression[RE["E"] ~~ RE["V"] ~~ RE["E"] ~~ RE["N"]]], {2, 0}, 
			StringMatchQ[x, RegularExpression[RE["integer"]]],                            {0, Interpreter["Integer"][x]}, 
			StringMatchQ[x, RegularExpression["[+\\-]?([0-9]+)?" ~~ RE["N"] ~~ "(" ~~ RE["ws*"] ~~ "[+\\-]" ~~ RE["ws*"] ~~ "([0-9]+))?"]], 
				First @ StringCases[x, 
					(a:RegularExpression["[+\\-]?([0-9]+)?"]) ~~ RegularExpression[RE["N"]] ~~ rest___ :> 
					{
						Switch[a, "+"|"", 1, "-", -1, _, Interpreter["Integer"][a]], 
						With[{str = StringReplace[rest, Whitespace -> ""]}, If[str == "", 0, Interpreter["Integer"][str]]]}], 
			True, Throw @ Failure["BadANB", <|"Message" -> "Unrecognized ANB pseudo class argument.", "Input" -> s|>]
		]		
	]

(* The 'not' itself does not contribute to the specificity, but the body of not(...) does. *)
consumeNegationPseudoClass[{}, namespaces_, Hold[specificity_]] := 
	Throw @ 
		Failure["BadNegation", <|
			"Message" -> "Negation pseudo class selector argument should be a simple selector."|>]

consumeNegationPseudoClass[tokens:{__?CSSTokenQ}, namespaces_, Hold[specificity_]] :=
	Module[{localSelectorSequence, selectors},
		localSelectorSequence = consumeCSSSelector[tokens, namespaces];
		If[FailureQ[localSelectorSequence], 
			Throw @ 
				Failure["BadNegation", <|
					"Message" -> "Negation pseudo class selector has unrecognized argument.",
					"String"  -> CSSUntokenize @ tokens|>]];
		
		selectors = localSelectorSequence["Sequence"]; (* list of SimpleSelectorSequence and Combinator instances *)
		If[selectors === {}, 
			Throw @ 
				Failure["BadNegation", <|
					"Message" -> "Negation pseudo class selector has no arguments.",
					"String"  -> CSSUntokenize @ tokens|>]];
		If[Length[selectors] > 1, 
			Throw @ 
				Failure["BadNegation", <|
					"Message" -> "Negation pseudo class selector argument should be a simple selector.",
					"String"  -> CSSUntokenize @ tokens|>]];
		If[MatchQ[selectors, {"SimpleSelectorSequence" -> {_ -> _?AssociationQ}}],
			specificity += localSelectorSequence["Specificity"];
			"PseudoClass" -> <|
				"Name"     -> "not",
				"Children" -> selectors[[1, -1]]|>
			,
			"BadNegation" (* shouldn't see this *)
		]
	]
		

(* ::Subsection::Closed:: *)
(*Single Selector*)


consumeCSSSelector[tokens:{__?CSSTokenQ}, namespaces_] := 
	Module[{selectors},
		selectors = 
			DeleteCases[
				SplitBy[tokens, MatchQ[CSSToken[KeyValuePattern["Type" -> "comma"]]]], 
				{CSSToken[KeyValuePattern["Type" -> "comma"]]}];
		selectors = consumeSingleSelector[#, namespaces]& /@ selectors;
		FirstCase[selectors, _?FailureQ, If[Length[selectors] == 1, First[selectors], selectors]]
	]



(* Notes: 
	Each simple selector sequence must start with Type or Universal. 
	The Universal simple selector is often implied and can be omitted.
	Only the last simple selector sequence can have a pseudo element. *)
consumeSingleSelector[{}, namespaces_] := Failure["BadSelector", <|"Message" -> "Selector is empty."|>]
	
consumeSingleSelector[tokens:{__?CSSTokenQ}, namespaces_] :=
	Catch @
	Module[{pos, l, ns, value, specificity = <|"a" -> 0, "b" -> 0, "c" -> 0|>, objects = {}, sss = {}, inSimpleSelector = False},
		(* trim any trailing whitespace (reduce the token count) *)
		pos = Length[tokens];
		If[TokenTypeIs["whitespace", tokens[[pos]]], RetreatPosAndSkipWhitespace[pos, l, tokens]];
		l = pos; pos = 1;
		
		(* skip any initial whitespace *)
		If[TokenTypeIs["whitespace", tokens[[pos]]], AdvancePosAndSkipWhitespace[pos, l, tokens]];
		
		
		While[pos <= l,
			Switch[tokens[[pos]]["Type"],
				"ident",
					(* check whether ident is a namespace *)
					ns = 
						If[isNamespace[pos, l, tokens], 
							consumeNamespace[pos, l, tokens, namespaces]
							,
							consumeDefaultNamespace[pos, l, tokens, namespaces]
						];
					If[!inSimpleSelector, inSimpleSelector = True];
					AppendTo[sss, consumeTypeOrUniversalSelector[pos, l, tokens, ns, Hold[specificity]]],
				"delim",
					Switch[tokens[[pos]]["String"],
						"*", 
							(* check whether universal selector is the namespace *)
							ns = 
								If[isNamespace[pos, l, tokens], 
									pos++; pos++; All
									,
									consumeDefaultNamespace[pos, l, tokens, namespaces]
								];
							If[!inSimpleSelector, inSimpleSelector = True];
							AppendTo[sss, consumeTypeOrUniversalSelector[pos, l, tokens, ns, Hold[specificity]]],
						"|", 
							(* check whether the namespace declaration is followed by a Type or Universal selector *)
							ns = 
								If[isNoNamespace[pos, l, tokens], 
									pos++; None
									,
									Throw @ 
										Failure["BadSelector", <|
											"Message"  -> "A Type or Universal selector is expected after the \"|\" token.",
											"Position" -> showError[pos, pos, tokens]|>]
								];
							If[!inSimpleSelector, inSimpleSelector = True];
							AppendTo[sss, consumeTypeOrUniversalSelector[pos, l, tokens, ns, Hold[specificity]]],
						"+" | "~" | ">", 
							If[inSimpleSelector, 
								inSimpleSelector = False;
								AppendTo[objects, "SimpleSelectorSequence" -> sss]; sss = {}; 
								, 
								If[pos == 1,
									Throw @ 
										Failure["BadSelector", <|
											"Message"  -> "A selector cannot start with a combinator.",
											"Position" -> showError[pos, pos, tokens]|>]
									,
									Throw @ 
										Failure["BadSelector", <|
											"Message"  -> "A combinator cannot follow another combinator.",
											"Position" -> showError[pos-1, pos, tokens]|>]
								]
							];
							AppendTo[objects, consumeCSSCombinator[pos, l, tokens]],
						".",
							If[isClassSelector[pos, l, tokens],
								specificity["b"]++;
								If[!inSimpleSelector, inSimpleSelector = True];
								AppendTo[sss, consumeClassSelector[pos, l, tokens]]
								,
								Throw @ 
									Failure["BadSelector", <|
										"Message"  -> "Unrecognized delimiter.",
										"Position" -> showError[pos, pos, tokens]|>]
							],
						_, 
							Throw @ 
								Failure["BadSelector", <|
									"Message"  -> "Unrecognized delimiter.",
									"Position" -> showError[pos, pos, tokens]|>]
					],
				"whitespace", (* This shouldn't be hit in a valid selector. *)
					If[inSimpleSelector, 
						inSimpleSelector = False;
						AppendTo[objects, "SimpleSelectorSequence" -> sss]; sss = {}; 
						, 
						Throw @ 
							Failure["BadSelector", <|
								"Message"  -> "A combinator cannot follow another combinator.",
								"Position" -> showError[pos-1, pos, tokens]|>]
					];
					AppendTo[objects, consumeCSSCombinator[pos, l, tokens]],
				"[]", 
					specificity["b"]++;
					If[!inSimpleSelector, inSimpleSelector = True];
					value = Catch @ consumeAttributeSelector[tokens[[pos]]["Children"], namespaces];
					If[FailureQ[value], Throw @ modifyError[value, pos, tokens], AppendTo[sss, value]];
					pos++,
				"hash", 
					specificity["a"]++;
					If[!inSimpleSelector, inSimpleSelector = True];
					AppendTo[sss, consumeIDSelector[pos, l, tokens]],
				"colon",
					If[isPseudoElement[pos, l, tokens],
						specificity["c"]++;
						value = consumePseudoElementSelector[pos, l, tokens];
						If[pos <= l, 
							Throw @ 
								Failure["BadSelector", <|
									"Message"  -> "Pseudo element must be after the last simple selector sequence.",
									"Position" -> showError[pos-3, pos-1, tokens]|>]
							,
							AppendTo[sss, value];
						];
						,
						value = Catch @ consumePseudoClassSelector[pos, l, tokens, namespaces, Hold[specificity]];
						If[FailureQ[value], 
							Throw @ modifyError[value, pos, tokens]
							, 
							AppendTo[sss, value]
						];
					];
					If[!inSimpleSelector, inSimpleSelector = True],
				_, 
					Throw @ 
						Failure["BadSelector", <|
							"Message"  -> "Unrecognized simple selector or combinator.",
							"Position" -> showError[pos, pos, tokens]|>]				
			]
		];
		If[!inSimpleSelector, 
			Failure["BadSelector", <|
				"Message"  -> "The selector must end with a simple selector.",
				"Position" -> showError[l, l, tokens]|>]
			,
			AppendTo[objects, "SimpleSelectorSequence" -> sss];
			CSSSelector[<|
				"String" -> StringReplace[CSSUntokenize @ tokens, "/**/" -> ""], 
				"Sequence" -> objects, 
				"Specificity" -> Prepend[Values @ specificity, 0]|>]
		]
	]


(* ::Section::Closed:: *)
(*Process simple selectors*)


$Debug = False;


(* ::Subsection::Closed:: *)
(*Universal selector*)


(* 
	A missing namespace is an ambiguous case. 
	HTML documents automatically assign the HTML or XHTML namespace, as well as "unknown" but common namespaces e.g. SVG. 
	If there is no namespace declared in the CSS, then should the selectors apply in all namespaces or no namespace? 
	Each User Agent (e.g. Mathematica) can make its own choice. We choose to apply the selector to all namespaces. *)
selectUniversal[content:{{_Integer..}...}, namespace_] :=
	If[MatchQ[namespace, All | _Missing], 
		content
		,
		Pick[content, Echo @ StringMatchQ[getNamespaceOfDocumentElement /@ content, namespace]]
	]


(* ::Subsection::Closed:: *)
(*Type selector*)


(* An XMLElement expression is of either form 
		XMLElement[typeName, {attributeName -> attributeValue, ... }, children]
		XMLElement[{namespace, typeName}, {{namespace, attributeName} -> attributeValue, ... }, children]
	A valid type name check must account for a possible namespace. *)
validTypeQ[typeName_String] := 
	With[{type = Extract[$Document, Append[#, 1]]},
		Or[
			StringQ[type] && StringMatchQ[type, typeName, IgnoreCase -> $IgnoreCase["Type"]],
			ListQ[type] && MatchQ[type, {_String, t_String /; StringMatchQ[t, typeName, IgnoreCase -> $IgnoreCase["Type"]]}]
		]
	]&
	
	
selectType[content:{{_Integer..}...}, namespace_, name_] :=
	If[MatchQ[namespace, All | _Missing], 
		Pick[content, validTypeQ[name] /@ content]
		,
		Pick[
			content, 
			Thread[
				And[
					StringMatchQ[getNamespaceOfDocumentElement /@ content, namespace], (* case must match *)
					validTypeQ[name] /@ content]]]
	]


(* ::Subsection::Closed:: *)
(*Attribute selector*)


(* ::Subsubsection::Closed:: *)
(*Notes*)


(* NAMESPACE
	Attributes are NEVER in a namespace unless they have a (previously declared) namespace prefix.
	Even if a default namespace is declared, the default namespace does not apply to attributes!
	I suppose the idea is that attribute names are pretty unique between XML vocabularies so should rarely collide...
	
	A small example CSS:
		@namespace foo "http://www.example.com";
		[foo|att=val] { color: blue }  ----> matches 'att' only in namespace 'foo' and value 'val'
		[*|att] { color: yellow }      ----> matches 'att' regardless of namespace, even no namespace
		[|att] { color: green }        ----> matches 'att' without a namespace
		[att] { color: green }         ----> matches 'att' without a namespace
		
	In a document (like XML), attributes can be namespaced separately from the element type's namespace, but this is rare:
		<prefix1:p prefix2:style='red'><\p>
		
	If the attribute namespace is not specified, it belongs to no namespace. In Clark notation where curly brackets indicates namespace:
		<prefix:p style='red'><\p> is effectively <{URI}p {}style='red'><\p>
	
	In Wolfram Language, "IncludeNamespaces" conforms to the XML standard leaving un-prefixed attributes without a namespace:
		TRUE:          <p style='red'><\p>  --->  XMLElement[{"URI", "p"}, {{"", "style"} -> "red"}, {}]
		AUTO or FALSE: <p style='red'><\p>  --->  XMLElement["p", {"style" -> "red"}, {}]
		
	In Wolfram Language, if a valid prefix is included on the attribute:
		TRUE or AUTO:  <prefix1:p prefix2:style='red'><\p>  --->  XMLElement[{"URI1", "p"}, {{"URI2", "style"} -> "red"}, {}]
		FALSE:         <prefix1:p prefix2:style='red'><\p>  --->  XMLElement["p", {"style" -> "red"}, {}]
		
	Generally, the user should import with '"IncludeNamespaces" -> True' to avoid ambiguity.
*)

(* CASE SENSITIVITY
	Attribute name case sensitivity depends on the document type. 
	We use a global flag for easiest portability.
*)


(* ::Subsubsection::Closed:: *)
(*name, operator, and value*)


operatorValueCondition[value_, operator_, attributeValue_] :=
	If[MissingQ[attributeValue],
		True
		,
		Switch[operator,
			"All",              True,
			"="  | "Exact",     StringMatchQ[   value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
			"*=" | "Substring", StringContainsQ[value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
			"^=" | "Prefix",    StringStartsQ[  value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
			"$=" | "Suffix",    StringEndsQ    [value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
			"~=" | "Include",   AnyTrue[StringSplit[value, Whitespace], StringMatchQ[attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]]], 
			"|=" | "Dash",      StringMatchQ[First[StringSplit[value, "-"]], attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
			_, If[TrueQ @ $Debug, "Operator fallthrough."]; False
		]
	]


validAttributeQ[None, {attributeName_String, operator_, attributeValue_}] :=
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[TrueQ @ $Debug, Print["AttAndVal: ", #, " ", attributeList, " ", {attributeName, operator, attributeValue}]];
		If[TrueQ @ $Debug, Echo, Identity][
			MatchQ[attributeList,
				{___,
					((name_String | {"", name_String}) /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) ->
						(value_String /; operatorValueCondition[value, operator, attributeValue]),
				___}
			]
		]
	]&
	
(* Used for ID attribute. *)
validAttributeQ[All, {attributeName_String, operator_, attributeValue_}] :=
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[TrueQ @ $Debug, Print["AttAndVal: ", #, " ", attributeList, " ", {attributeName, operator, attributeValue}]];
		If[TrueQ @ $Debug, Echo, Identity][
			MatchQ[attributeList,
				{___,
					((name_String | {_, name_String}) /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) ->
						(value_String /; operatorValueCondition[value, operator, attributeValue]),
				___}
			]
		]
	]&


validAttributeQ[attributeNamespace_String, {attributeName_String, operator_, attributeValue_}] :=
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[TrueQ @ $Debug, Print["AttAndNameAndVal: ", #, " ", attributeList]];
		Which[
			(* case of '"IncludeNamespaces" -> True' and check for a match against the given attrib namespace, which could be empty e.g. ""*)
			MatchQ[attributeList, 
				{___, {ns_String /; StringMatchQ[ns, attributeNamespace], n_String /; StringMatchQ[n, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]} -> 
					(v_String /; operatorValueCondition[v, operator, attributeValue]), ___}
			],
				If[TrueQ @ $Debug, Print["True."]];
				True,
			
			(* case of '"IncludeNamespaces" -> Automatic' or 'False'; assume namespace is empty and only match against case  *)
			MatchQ[attributeList, 
				{___, (n_String /; StringMatchQ[n, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) -> 
					(v_String /; operatorValueCondition[v, operator, attributeValue]), ___}
			],
				If[TrueQ @ $Debug, Print["Automatic/False."]];
				True,
			
			(* fallthrough case: return False *)
			True, If[TrueQ @ $Debug, Print["Fallthrough."]]; False
		]
	]&


(* ::Subsubsection::Closed:: *)
(*selectAttribute*)


(* operator and value may not be present; selection steps are almost the same *)
selectAttribute[content:{{_Integer..}...}, namespace_, attributeName_, operator_, attributeValue_] := 
	Pick[content, validAttributeQ[namespace, {attributeName, operator, attributeValue}] /@ content]


(* ::Subsection::Closed:: *)
(*Class selector*)


(*
	This selector is syntactic sugar. "div.value" has the same meaning as "div[class~=value]". 
	Because "." is effectively expanded to "class", the case-sensitivity is ambiguous. Using all lowercase is most conforming (and at least conforms to HTML).
	However, specifying a possible namespace is not available to this notation.
	In HTML, the lack of namespace is not an issue since all types and attributes belong in the HTML namespace.
	In XML docs, this can be a problem. For now, the implementation puts the 'class' attribute in the empty namespace.
	
	FIXME:
		CSS authors should probably avoid this shorthand notation, or we should provide a way to expand the notation given user-supplied information.
		The case-sensitivity issue is also bad since we assume lowercase.
*)
selectClass[content:{{_Integer..}...}, value_String] := selectAttribute[content, None, "class", "Include", value]


(* ::Subsection::Closed:: *)
(*ID selector*)


(*
	Using lowercase 'id' in HTML is required.
	XML documents can use any attribute name as the 'id' attribute; the DTD contains the info that indicates the attribute to be used as 'id'.
	To complicate matters, a different 'id' attribute name can exist in different namespaces in the XML document.
	However, the selector is typically just "#id".
	For now we assume that the 'id' is given as "id" in all lowercase.
*)
(*FIXME: 
	CSS authors that wish to target XML should probably avoid this shorthand notation and instead use e.g. [name=p371].
	Since the id attribute can exist in multiple namespaces, to be most conforming it is assumed the shorthand notation has namespace All.
*)
selectID[content:{{_Integer..}...}, value_String] := selectAttribute[content, All, $ID, "Exact", value]


(* ::Subsection::Closed:: *)
(*Negation selector*)


(*
	The surrounding 'not' and parentheses are assumed to be parsed away.
	Only one simple selector is allowed within 'not(...)'.
	The 'not' pseudoclass head does not contribute to specificity, but its argument does.
*)
(*TODO: get negation selection working*)
selectNegation[content:{{_Integer..}...}, argument:{Rule[_?StringQ, _?AssociationQ]...}] :=
	Module[{newSelection = Catch @ processSimpleSelectorSequence[argument]},
		If[AnyTrue[newSelection, _?FailureQ], 
			Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Invalid negation argument.", "Token" -> argument|>]
		];
		Complement[content, newSelection]
	]


(* ::Subsection::Closed:: *)
(*Pseudo class selector*)


(* ::Subsubsection::Closed:: *)
(*Notes*)


(* CSS Selectors 2.1
	active -> mouse down
	first-child -> first sibling in a group of siblings
	focus -> element with received focus (e.g. InputField's field)
	hover -> mouse over
	lang() -> targets element with lang attribute and language value
	link -> matches unvisited links <a>, <area>, or <link>
	visited -> matches link already visited
*)

(* CSS Selectors 3
	checked -> RadioButton or Checkbox condition is True, or selection list selectors (e.g. PopupMenu)
	disabled -> any Enabled -> False control condition
	empty -> element with no children elements or text (or whitespace)
	enabled -> any Enabled -> True (or Automatic)
	first-of-type -> first sibling of element type (not sure if it works with universal selector)
	indeterminate -> RadioButton or Checkbox condition is not one of the allowed choices
	last-child -> last sibling in a group of siblings
	last-of-type -> last sibling of element type (not sure if it works with universal selector)
	nth-child()
	nth-last-child()
	nth-last-of-type()
	nth-of-type()
	not() -> takes single selector as argument; cannot contain other negation or pseudo-element
	only-child -> element with no siblings
	only-of-type -> element with no siblings of matching type
	root
	target
*)

(*
	Pseudo classes do not reference particular attribute names and thus are completely case-insensitive.
*)


(* ::Subsubsection::Closed:: *)
(*Dynamic classes*)


selectPseudoClass[content:{{_Integer..}...}, "active"] := content (* dynamic if mouse down *)
selectPseudoClass[content:{{_Integer..}...}, "focus"] := content (* dynamic if selector is within *)
selectPseudoClass[content:{{_Integer..}...}, "hover"] := content (* dynamic if mouse over *)

selectPseudoClass[content:{{_Integer..}...}, "visited"] := (* dynamic if link has been clicked *)
	Pick[content, 
		MatchQ[#, 
			(type_String | {_String, type_String}) /; StringMatchQ[type, "a", IgnoreCase -> $IgnoreCase["Type"]]
		]& /@ Extract[$Document, Append[#, 1]& /@ content]]

selectPseudoClass[content:{{_Integer..}...}, "link"] := (* dynamic if link has NOT been clicked *)
	Pick[content, 
		Thread[
			And[
				MatchQ[#, 
					(type_String | {_String, type_String}) /; StringMatchQ[type, "a" | "area" | "link", IgnoreCase -> $IgnoreCase["Type"]]
				]& /@ Extract[$Document, Append[#, 1]& /@ content]
				,
				MatchQ[#,
					{___, ((name_String | {_String, name_String}) /; StringMatchQ[name, "href", IgnoreCase -> $IgnoreCase["AttributeName"]]) -> _, ___}
				]& /@ Extract[$Document, Append[#, 2]& /@ content]
			]]]


(* ::Subsubsection::Closed:: *)
(*Target class*)


(* 
	this is inherently wrong; 
	we pick out any element that has an ID attribute because it is able to be targeted, not because it is the current target
*)
selectPseudoClass[content:{{_Integer..}...}, "target"] := (* dynamic only if current URI targets the ID *)
	Pick[content, 
		MatchQ[#,
			{___, ((name_String | {_String, name_String}) /; StringMatchQ[name, $ID, IgnoreCase -> $IgnoreCase["AttributeName"]]) -> _, ___}
		]& /@ Extract[$Document, Append[#, 2]& /@ content]
	]


(* ::Subsubsection::Closed:: *)
(*Language class*)


selectPseudoClass[content:{{_Integer..}...}, {"lang", type_String}] := 
	Module[{languages},
		(* 
			Language is inherited and is typically either specified at the document root or within the tag itself. 
			Because it can be specified in any ancestor, we must go down to the root and look for any 'lang' attributes.
			If the 'type' language identifier is missing, then the selector is invalid.
			The type can also match if it begins the elements's language followed by a dash, e.g. :lang(zh) matches both "zh-gan" and "zh".
		*)
		If[type == "", Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Missing language type in :lang() pseudo class.", "Token" -> type|>]];
		languages = getLanguageOfDocumentElement /@ content;
		Pick[content, Or[StringMatchQ[#, type, IgnoreCase -> True], StringStartsQ[#, type ~~ "-", IgnoreCase -> True]]& /@ languages]
	]


(* ::Subsubsection::Closed:: *)
(*UI element state classes (TODO?)*)


(* 
	Dynamic states like these are tough to target when compared to static symbolic XML.
	UI elements are document-specific. We can at least search for these attributes...?
	Moreover, in HTML these are 'boolean' attributes that don't require a value, but in XML they do. If the attribute is missing, then the value is assumed 'False'.
	In XHTML the att-value pair is supposed to be e.g. enabled="enabled". 
	
	TODO: 
		If the document class is (X)HTML, then search for the appropriate UI elements. 
		If XML, then search for attributes, or implement an option that tells us what UI elements to expect.
*)
selectPseudoClass[content:{{_Integer..}...}, "enabled"] := content 
selectPseudoClass[content:{{_Integer..}...}, "disabled"] := content 
selectPseudoClass[content:{{_Integer..}...}, "checked"] := content 
selectPseudoClass[content:{{_Integer..}...}, "indeterminate"] := content 


(* ::Subsubsection::Closed:: *)
(*Structural classes*)


siblingPositions[child:{_Integer..}] := 
	With[{childListPosition = child[[;;-2]]}, Append[childListPosition, #]& /@ Range @ Length @ Extract[$Document, childListPosition]]


selectPseudoClass[content:{{_Integer..}...}, "root"] := Pick[content, MatchQ[#, $DocumentRootPosition]& /@ content]


selectPseudoClass[content:{{_Integer..}...}, "empty"] := Pick[content, MatchQ[#, {}]& /@ Extract[$Document, Append[#, 3]& /@ content]]


selectPseudoClass[content:{{_Integer..}...}, "first-child"] := selectPseudoClass[content, {"nth-child", {0, 1}}]
selectPseudoClass[content:{{_Integer..}...}, "last-child"] :=  selectPseudoClass[content, {"nth-last-child", {0, 1}}]

(* 
	The 'only-child' pseudo class ignores element type in the list of siblings.
	As spec'ed, only XMLElement children are considered; string literal children are removed.
*)
selectPseudoClass[content:{{_Integer..}...}, "only-child"] := Pick[content, Length[DeleteCases[Extract[$Document, Most @ #], _String]] == 1& /@ content]
	
(* 
	The 'nth-child' and 'nth-last-child' pseudo classes ignores XML element type; string literals are also ignored in position counting 
*)	
selectPseudoClass[content:{{_Integer..}...}, {type:"nth-child"|"nth-last-child", {a_Integer, b_Integer}}] := 
	Module[{allValidSiblingPositions, positionsAmongSiblings},
		allValidSiblingPositions = With[{s = siblingPositions[#]}, Pick[s, MatchQ[Extract[$Document, #], _XMLElement]& /@ s]]& /@ content;
	
		(* counting from the back is the same as reversing the sibling order and counting from the front *)
		If[type == "nth-last-child", allValidSiblingPositions = Reverse /@ allValidSiblingPositions];
		positionsAmongSiblings = FirstPosition[allValidSiblingPositions, #][[-1]]& /@ content;
		If[a==0, 
			Pick[content, Thread[positionsAmongSiblings == b]]
			,
			Pick[content, (IntegerQ[#] && NonNegative[#])& /@ ((positionsAmongSiblings - b)/a)]
		]
	]


selectPseudoClass[content:{{_Integer..}...}, "first-of-type"] := selectPseudoClass[content, {"nth-of-type", {0, 1}}]
selectPseudoClass[content:{{_Integer..}...}, "last-of-type"] :=  selectPseudoClass[content, {"nth-last-of-type", {0, 1}}]

(* 
	The 'only-of-type' pseudo class ignores all XML element types that do not match the current selection's type.
	String literals are also ignored in position counting.
	Care must be taken since element types can belong to particular namespaces.
*)	
selectPseudoClass[content:{{_Integer..}...}, "only-of-type"] := 
	Module[{allSiblingPositions, elementTypes, allValidSiblingPositions},
		elementTypes = Extract[$Document, Append[#, 1]]& /@ content;
		allSiblingPositions = siblingPositions /@ content;
		(* only select the siblings that match the element type *)
		allValidSiblingPositions = 
			MapThread[
				Pick[#1, 
					Function[x, 
						If[ListQ[#2], 
							MatchQ[x, XMLElement[{_String, type_String /; StringMatchQ[type, #2, IgnoreCase -> $IgnoreCase["Type"]]}, __]]
							,
							MatchQ[x, XMLElement[type_String /; StringMatchQ[type, #2, IgnoreCase -> $IgnoreCase["Type"]], __]]
						]
					] /@ Extract[$Document, #1]
				]&, 
				{allSiblingPositions, elementTypes}];
		Pick[content, Length[#] == 1& /@ allValidSiblingPositions]
	]

(* 
	The 'nth-of-type' and 'nth-last-of-type' pseudo classes ignores all XML element types that do not match the current selection type.
	String literals are also ignored in position counting.
*)		
selectPseudoClass[content:{{_Integer..}...}, {className:"nth-of-type"|"nth-last-of-type", {a_Integer, b_Integer}}] := 
	Module[{allSiblingPositions, allValidSiblingPositions, elementTypes, positionsAmongSiblings},
		elementTypes = Extract[$Document, Append[#, 1]]& /@ content;
		allSiblingPositions = siblingPositions /@ content;
		(* only select the siblings that match the element type *)
		allValidSiblingPositions = 
			MapThread[
				Pick[#1, 
					Function[x, 
						If[ListQ[#2], 
							MatchQ[x, XMLElement[{_String, type_String /; StringMatchQ[type, #2, IgnoreCase -> $IgnoreCase["Type"]]}, __]]
							,
							MatchQ[x, XMLElement[type_String /; StringMatchQ[type, #2, IgnoreCase -> $IgnoreCase["Type"]], __]]
						]
					] /@ Extract[$Document, #1]]&, 
				{allSiblingPositions, elementTypes}];
		
		(* counting from the back is the same as reversing the sibling order and counting from the front *)
		If[className == "nth-last-of-type", allValidSiblingPositions = Reverse /@ allValidSiblingPositions];
		positionsAmongSiblings = MapThread[FirstPosition[#1, #2]&, {allValidSiblingPositions, content}][[All, 1]];
		If[a==0, 
			Pick[content, Thread[positionsAmongSiblings == b]]
			,
			Pick[content, (IntegerQ[#] && NonNegative[#])& /@ ((positionsAmongSiblings - b)/a)]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Fallthrough failure case*)


selectPseudoClass[content:{{_Integer..}...}, arg1_] := 
	Throw @ 
		Failure["UnexpectedParse", <|
			"MessageTemplate" -> "Unrecognized pseudo class.", 
			"Class" -> First[arg1, arg1]|>]


(* ::Subsection::Closed:: *)
(*Pseudo-element (TODO)*)


(*
	Not sure how to implement these. 
		::before and ::after don't exist in the document (they refer to generated content, not static XML)
		::first-line and ::first-letter are not accessible with Extract+Part
*)
selectPseudoElement[content:{{_Integer..}...}, arg1_] := content


(* ::Section::Closed:: *)
(*Process full selector*)


(* ::Subsection::Closed:: *)
(*Process simple selector sequence*)
processSimpleSelectorSequence[sss:{Rule[_?StringQ, _?AssociationQ]..}] :=
	Module[{selection(*, token, value, pseudo*)},
		(* $Elements is a list of positions of all XMLElement expressions *)
		selection = $Elements; 
		
		(* process simple selectors, updating selection at each step *)
		Map[
			Switch[#[[1]],
				"Universal",     selection = selectUniversal[selection, #[[2]]["Namespace"]],
				"Type",          selection = selectType[selection, #[[2]]["Namespace"], #[[2]]["Name"]],
				"Attribute",     selection = selectAttribute[selection, #[[2]]["Namespace"], #[[2]]["Name"], #[[2]]["Match"], #[[2]]["Value"]],
				"Class",         selection = selectClass[selection, #[[2]]["Value"]],
				"ID",            selection = selectID[selection, #[[2]]["Value"]],
				"PseudoElement", selection = selectPseudoElement[selection, #[[2]]["Name"]],
				"PseudoClass", 
					Which[
						#[[2]]["Name"] === "not",    selection = selectNegation[selection, #[[2]]["Children"]],
						KeyExistsQ[#[[2]], "ANB"],   selection = selectPseudoClass[selection, {#[[2]]["Name"], #[[2]]["ANB"]}],
						KeyExistsQ[#[[2]], "Value"], selection = selectPseudoClass[selection, {#[[2]]["Name"], #[[2]]["Value"]}],
						True,                        selection = selectPseudoClass[selection, #[[2]]["Name"]]
					],
				_, 
					Echo @ (* shouldn't hit this if selectors were properly processed *) 
						Failure["BadSimpleSelector", <|
							"MessageTemplate" -> "Malformed tokenized simple selector sequence.", 
							"Token" -> #[[1]], 
							"AllTokens" -> sss|>]		
			]&,
			sss
		];
		If[selection === {}, {}, Sort @ selection]
	]


(* ::Subsection::Closed:: *)
(*Process combinator*)


(* Direct descendant: check that position depth is exactly greater by 2, but matches all ancestor positions indices *)
processCombinator[positions1_, ">" | "Child", positions2_] := 
	Union @ Flatten[Select[positions2, Function[x, Length[#] + 2 == Length[x] && MatchQ[#, x[[;;Length[#]]]]] ]& /@ positions1, 1]

(* Any descendent: check that position depth is greater, but matches all ancestor position indices *)	
processCombinator[positions1_, "" | ">>" | "Descendant", positions2_] := 
	Union @ Flatten[Select[positions2, Function[x, Length[#] < Length[x] && MatchQ[#, x[[;;Length[#]]]]] ]& /@ positions1, 1]
	
(* Immediate siblings; '#' is a slot for the older sibling, 'x' is a formal slot for the younger sibling *)
processCombinator[positions1_, "+" | "NextSibling", positions2_] := 
	Union @ 
		Flatten[
			Select[positions2, 
				Function[x, 
					And[
						Length[#] == Length[x], (* siblings must match in all but the last position index *)
						MatchQ[Most[#], Most[x]],
						Last[#] < Last[x], (* last position index of older sibling must be larger than younger sibling's last position index *)
						Or[(* if not an immediate sibling, check that only strings exist in between the siblings *)
							Last[#] + 1 == Last[x], 
							AllTrue[Extract[$Document, Most[#]][[Last[#]+1;;Last[x]-1]], StringQ]]]] 
			]& /@ positions1,
			1]
			
(* Eventual siblings; '#' is a slot for the older sibling, 'x' is a formal slot for the younger sibling *)
processCombinator[positions1_, "~" | "SubsequentSibling", positions2_] := 
	Union @ 
		Flatten[
			Select[positions2,
				Function[x, 
					And[
						Length[#] == Length[x], (* siblings must match in all but the last position index *)
						MatchQ[Most[#], Most[x]],
						Last[#] < Last[x]]] (* last position index of older sibling must be larger than younger sibling's last position index *)
			]& /@ positions1,
			1]
			
processCombinator[positions1_, combinator_String, positions2_] :=
	Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Unrecognized combinator.", "Combinator" -> combinator|>]


(* ::Subsection::Closed:: *)
(*Process selector*)


(* This assumes variables such as $Document and $Element have been defined. *)
processSelector[selector_?CSSSelectorQ] :=
	Module[{processed, pos, l},
		processed = selector["Sequence"];
		processed = 
			Replace[
				processed, 
				{
					Rule["SimpleSelectorSequence", x_] :> processSimpleSelectorSequence @ x,
					Rule["Combinator", x_] :> x["Type"]}, 
				{1}];
		
		(* Simply return if there are no combinators. *)
		l = Length[processed];
		If[l == 1, Return[First @ processed]];
		
		(* Otherwise combine selector results based on combinator type. Overwrite the right-most set of elements each time. *)
		pos = 2;
		While[pos < l,
			processed[[pos + 1]] = processCombinator[processed[[pos - 1]], processed[[pos]], processed[[pos + 1]]];
			pos += 2;
		];
		Sort @ Last @ processed
	]


(* ::Subsection::Closed:: *)
(*Process full selector (comma-separated selectors)*)


processFullSelector[fullSelector:{__?CSSSelectorQ}] :=
	Module[{selectors, attempt},
		attempt = Catch[processSelector /@ fullSelector];
		If[FailureQ[attempt], 
			attempt
			,
			If[Length[selectors] == 1, First @ attempt, attempt]
		]
	]


(* ::Section::Closed:: *)
(*Main Functions*)


(* ::Subsection::Closed:: *)
(*CSSSelector*)


(* ::Subsubsection::Closed:: *)
(*Token access*)


CSSSelector[a_?AssociationQ][key_] := a[key] 


(* ::Subsubsection::Closed:: *)
(*Constructor via a string*)


(* Recall that namespaces are stored as <|"Prefix" -> prefix, "Namespace" -> namespace, "Default" -> default|>. *)
CSSSelector[s_?StringQ, opts:OptionsPattern[{"Namespaces" -> {}}]] := 
	Module[{constructedNamespaces},
		constructedNamespaces =
			Map[
				Which[
					MatchQ[#, Rule[_?StringQ, _?StringQ]], 
						<|"Prefix" -> #[[1]], "Namespace" -> #[[2]], "Default" -> False|>
					,
					MatchQ[#, Rule[_?StringQ, Rule[_?StringQ, _?StringQ]]], 
						<|
							"Prefix"    -> #[[1]], 
							"Namespace" -> #[[2, 1]], 
							"Default"   -> StringMatchQ[#[[2, 2]], "Default", IgnoreCase -> True]|>
					,
					And[
						MatchQ[#, _?AssociationQ],
						KeyExistsQ[#, "Prefix"],    StringQ[#["Prefix"]],
						KeyExistsQ[#, "Namespace"], StringQ[#["Namespace"]]
					], 
						<|
							"Prefix"    -> #["Prefix"], 
							"Namespace" -> #["Namespace"], 
							If[KeyExistsQ[#, "Default"],
								"Default" -> TrueQ @ #["Default"]
								,
								"Default" -> False]|>
					,
					True, 
						Nothing
				]&,
				If[MatchQ[OptionValue["Namespaces"], {___}], OptionValue["Namespaces"], {}]];
		consumeCSSSelector[CSSTokenize @ s, constructedNamespaces]
	]


(* ::Subsubsection::Closed:: *)
(*MakeBoxes*)


(* 
	Dataset can't handle this box structure and displays it as an ellipses. 
	According to Chris Carlson, Dataset will display this in full in the next release.
	For now, we greatly simplify the boxes just so we have something to see. *)
(*CSSSelector /: MakeBoxes[s:CSSSelector[a_?AssociationQ], StandardForm] :=
	ToBoxes @ s*)
			
CSSSelector /: MakeBoxes[s:CSSSelector[a_?AssociationQ], StandardForm] :=
	ToBoxes[
		Interpretation[
			Style[
				Framed[
					Row[{
						Style["#CSS ", RGBColor[0, 0.5, 1], Selectable -> False], 
						Style[If[StringQ[a["String"]], StringTrim @ a["String"], a["String"]], Selectable -> True]}],
					RoundingRadius -> 4, 
					Background -> RGBColor[0.92, 0.98, 1],
					ImageMargins -> 2,
					FrameMargins -> {{5, 5}, {2, 2}},
					FrameStyle -> Directive[RGBColor[0, 0.5, 1], AbsoluteThickness[1]],
					BaseStyle -> {FontFamily -> Dynamic[CurrentValue[{StyleHints, "CodeFont"}]], FontWeight -> Bold, FontColor -> GrayLevel[0.2]}],
				Editable -> False,
				ShowStringCharacters -> False], 
			s]]
			

(* ::Subsection::Closed:: *)
(*CSSSelectorQ*)


CSSSelector /: CSSSelectorQ[CSSSelector[a_?AssociationQ]] := 
	And[
		Length[a] === 3,
		KeyExistsQ[a, "String"],
		KeyExistsQ[a, "Sequence"],
		KeyExistsQ[a, "Specificity"]]
CSSSelectorQ[___] := False


(* ::Subsection::Closed:: *)
(*CSSTargets*)


(* CSSTargets:
	Applies a selector to an XML document and returns the positions where the selector targets.
	Similar to Position syntax, it takes an XML document as the first argument and scope as the second.
	It has two different scopes:
		CSSSelector  ---->  returns extractable positions, similar to Position syntax
		CSSDataset   ---->  returns same dataset, but with added Targets column of extractable positions *)
Options[CSSTargets] = {
	"ID" -> "id",
	"CaseSensitive" -> {"Type" -> False, "AttributeName" -> False, "AttributeValue" -> False}};


(* It returns the expression positions in the XML document after applying one or more CSS selectors. *)
CSSTargets[doc:XMLObject["Document"][___], sel_?(Function[CSSSelectorQ[#] || StringQ[#]]), opts:OptionsPattern[]] :=
	CSSTargets[doc, {sel}, opts]

CSSTargets[doc:XMLObject["Document"][___], sel:{__?(Function[CSSSelectorQ[#] || StringQ[#]])}, opts:OptionsPattern[]] :=
	Block[
		{
			$Document, $Elements, 
			$DocumentRootPosition, $DocumentNamespaces, $DocumentLanguages, 
			$ID, $IgnoreCase, temp, type, name, value, sel2
		},
		
		(* upgrade any strings to CSS selector objects and then process *)
		sel2 = Replace[sel, s_?StringQ :> CSSSelector[s], {1}];
		If[AnyTrue[sel2, _?FailureQ], Return @ FirstCase[sel2, _?FailureQ]];
		
		$Document = doc;
		
		$Elements = Sort @ Position[$Document, XMLElement[__]];
		$DocumentRootPosition = First[$Elements, {}];
		$DocumentNamespaces   = getDocumentNamespaces[$Document];
		$DocumentLanguages    = getDocumentLanguages[$Document];
		
		$ID = OptionValue["ID"];
		
		temp = OptionValue["CaseSensitive"];
		{type, name, value} =
			Which[
				temp === True,  {True, True, True},
				temp === False, {False, False, False},
				True, {"Type", "AttributeName", "AttributeValue"} /. OptionValue["CaseSensitive"] /. _String :> False
			];
		$IgnoreCase = <|"Type" -> !type, "AttributeName" -> !name, "AttributeValue" -> !value|>;
		
		processFullSelector[sel2]
	]

CSSTargets[_, sel:{__?CSSSelectorQ}, ___]       := Failure["BadDocument", <|"Message" -> "Invalid XML document."|>]


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
