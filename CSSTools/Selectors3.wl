(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Header*)


BeginPackage["Selectors3`", {"GeneralUtilities`"}];

SetUsage[Selector, "\
Selector[selector$, XMLObject$] returns the CSS selector$ specificity and positions of XMLElement expressions in XMLObject$. 
Position[XMLObject$, Selector[selector$]] returns only the positions of XMLElement expressions."];

Begin["`Private`"];


(* ::Section::Closed:: *)
(*Namespace and Language*)


(*
	CSS namespaces are declared in at-keywords e.g. "@namespace prefix:URI" at the start of the CSS document.
	We assume the CSS namespaces are processed and passed as an option to this package in the form of a list {namespacePrefix -> URI, ...}.
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
	Module[{default, namespaces, root},
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
		root = Extract[document, $DocumentRootPosition];
		default = 
			Join[
				(*True*)     Cases[root, XMLElement[{ns_String, _String}, {___, {_String, _String} -> ns_String, ___}, _] :> ns, {0}],
				(*Automatic*)Cases[root, XMLElement[_String, {___, {x_String /; !StringMatchQ[x, ""], "xmlns"} -> ns_String, ___}, _] :> ns, {0}],
				(*False*)    Cases[root, XMLElement[_String, {___, HoldPattern["xmlns" -> ns_String], ___}, _] :> ns, {0}]];
		If[default =!= {}, default = {<|"Namespace" -> First[default], "Element" -> $DocumentRootPosition|>}];
		
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
(*Selectors Level 3 Grammar*)


(* ::Subsection::Closed:: *)
(*Regular Expressions (alphabetical)*)


RE["ident"]    = "([\\-]?" ~~ RE["nmstart"] ~~ "(" ~~ RE["nmchar"] ~~ ")*)";
RE["escape"]   = "(" ~~ RE["unicode"] ~~ "|\\\\[^\n\r\f0-9a-fA-F])"; 
RE["invalid"]  = "(" ~~ RE["invalid1"] ~~ "|" ~~ RE["invalid2"] ~~ ")";
RE["invalid1"] = "(\"([^\n\r\f(\\\\\")]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\\?)";
RE["invalid2"] = "('([^\n\r\f(\\\\')]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")*\\\\?)";
RE["name"]     = "(" ~~ RE["nmchar"] ~~ "+)";
RE["nl"]       = "(\n|\r\n|\r|\f)";
RE["nmchar"]   = "([_a-zA-Z0-9-]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["nmstart"]  = "([_a-zA-Z]|" ~~ RE["nonascii"] ~~ "|" ~~ RE["escape"] ~~ ")";
RE["num"]      = "([0-9]*\\.[0-9]+|[0-9]+)"; (* \\. literal dot for RE; pattern match against reals before integers *)
RE["nonascii"] = "([^[:ascii:]])";
RE["string"]   = "(" ~~ RE["string1"] ~~ "|" ~~ RE["string2"] ~~ ")";
RE["string1"]  = "(\"([^\n\r\f(\\\\\")]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*\")"; (* [] contains escaped double quote *)
RE["string2"]  = "('([^\n\r\f(\\\\')]|\\\\" ~~ RE["nl"] ~~ "|" ~~ RE["escape"] ~~ ")*')"; (* [] contains escaped single quote *)
RE["unicode"]  = "(\\\\[0-9a-fA-F]{1,6}(\r\n|[ \n\r\t\f])?)"; (* \\\\ literal backslash for RE *)
RE["w"]        = "([ \t\r\n\f]*)";


RE["D"] = "(d|D|\\\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?|\\\\d|\\\\D)";
RE["E"] = "(e|E|\\\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?|\\\\e|\\\\E)";
RE["N"] = "(n|N|\\\\0{0,4}(4e|6e|4E|6E)(\r\n|[ \t\r\n\f])?|\\\\n|\\\\N)";
RE["O"] = "(o|O|\\\\0{0,4}(4f|6f|4F|6F)(\r\n|[ \t\r\n\f])?|\\\\o|\\\\O)";
RE["T"] = "(t|T|(\\\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?)|\\\\t|\\\\T)";
RE["V"] = "(v|V|(\\\\0{0,4}(56|76)(\r\n|[ \t\r\n\f])?)|\\\\v|\\\\V)";


(* ::Subsection::Closed:: *)
(*Tokens (alphabetical)*)


T["ATKEYWORD"]   = "(@" ~~ RE["ident"] ~~ ")";

T["CDC"]         = "(-->)";
T["CDO"]         = "(<!--)";
T["COMMA"]       = "(" ~~ RE["w"] ~~ "," ~~ ")";
T["COMMENT"]     = "(/\\*[^*]*\\*+([^/*][^*]*\\*+)*/)";

T["DASHMATCH"]   = "(\\|=)";
T["DIMENSION"]   = "(" ~~ RE["num"] ~~ RE["ident"] ~~ ")";

T["FUNCTION"]    = "(" ~~ RE["ident"] ~~ "\\()";

T["GREATER"]     = "(" ~~ RE["w"] ~~ ">" ~~ ")";

T["HASH"]        = "(#" ~~ RE["name"] ~~ ")";

T["IDENT"]       = RE["ident"];
T["INCLUDES"]    = "(~=)";
T["INVALID"]     = RE["invalid"];

T["NOT"]         = "(:" ~~ RE["N"] ~~ RE["O"] ~~ RE["T"] ~~ "\\()";
T["NUMBER"]      = RE["num"];

T["PERCENTAGE"]  = "(" ~~ RE["num"] ~~ "%)";
T["PLUS"]        = "(" ~~ RE["w"] ~~ "\\+" ~~ ")";
T["PREFIXMATCH"] = "(\\^=)";

T["S"]           = "([ \t\r\n\f]+)";
T["STRING"]      = RE["string"];
T["SUBSTRINGMATCH"] = "(\\*=)";
T["SUFFIXMATCH"] = "(\\$=)";

T["TILDE"]       = "(" ~~ RE["w"] ~~ "~" ~~ ")";


(* ::Subsection::Closed:: *)
(*Productions (alphabetical)*)


P["attrib"] = 
	StringExpression["(", 
		"\\[", T["S"] ~~ "*", P["namespace_prefix"] ~~ "?", T["IDENT"], T["S"] ~~ "*",
			"(",
				"(" ~~ T["PREFIXMATCH"] ~~ "|" ~~ T["SUFFIXMATCH"] ~~ "|" ~~ T["SUBSTRINGMATCH"] ~~ "|" ~~ "=" ~~ "|" ~~ T["INCLUDES"] ~~ "|" ~~ T["DASHMATCH"] ~~ ")",
				T["S"] ~~ "*", 
				"(" ~~ T["IDENT"] ~~ "|" ~~ T["STRING"] ~~ ")", 
				T["S"] ~~ "*",
			")?", 
		"\\]",
	")"];
	
P["class"] = "(\\." ~~ T["IDENT"] ~~ ")";
P["combinator"] = 
	StringExpression["(",
		"(" ~~ T["PLUS"] ~~ T["S"] ~~ "*" ~~ ")" ~~ "|",
		"(" ~~ T["GREATER"] ~~ T["S"] ~~ "*" ~~ ")" ~~ "|",
		"(" ~~ T["TILDE"] ~~ T["S"] ~~ "*" ~~ ")" ~~ "|",
		T["S"] ~~ "+",
	")"];
	
P["element_name"] = T["IDENT"];
P["expression"] = 
	StringExpression["(",
		"(",
			"(" ~~ T["PLUS"] ~~ "|" ~~ "-" ~~ "|" ~~ T["DIMENSION"] ~~ "|" ~~ T["NUMBER"] ~~ "|" ~~ T["STRING"] ~~ "|" ~~ T["IDENT"] ~~ ")",
			T["S"] ~~ "*",
		")+",
	")"];
	
P["functional_pseudo"] = "(" ~~ T["FUNCTION"] ~~ T["S"] ~~ "*" ~~ P["expression"] ~~ "\\))";

P["namespace_prefix"] = "((" ~~ T["IDENT"] ~~ "|\\*)?\\|)";
P["negation"] = "(" ~~ T["NOT"] ~~ T["S"] ~~ "*" ~~ P["negation_arg"] ~~ T["S"] ~~ "*\\)" ~~ ")";
P["negation_arg"] = "(" ~~ P["attrib"] ~~ "|" ~~ T["HASH"] ~~ "|" ~~ P["class"] ~~ "|" ~~ P["pseudo"] ~~ "|" ~~ P["universal"] ~~ "|" ~~ P["type_selector"] ~~ ")";

P["pseudo"] = "(::?" ~~ "(" ~~ P["functional_pseudo"] ~~ "|" ~~ T["IDENT"] ~~ "))";

P["selector"] = "(" ~~ P["simple_selector_sequence"] ~~ "(" ~~ P["combinator"] ~~ P["simple_selector_sequence"] ~~ ")*" ~~ ")";
P["selectors_group"] = "(" ~~ P["selector"] ~~ "(" ~~ T["COMMA"] ~~ T["S"] ~~ "*" ~~ P["selector"] ~~ ")*" ~~ ")";
P["simple_selector_sequence"] = 
	StringExpression["(",
		"(",
			"(" ~~ P["type_selector"] ~~ "|" ~~ P["universal"] ~~ ")",
			"(" ~~ T["HASH"] ~~ "|" ~~ P["class"] ~~ "|" ~~ P["attrib"] ~~ "|" ~~ P["negation"] ~~ "|" ~~ P["pseudo"] ~~ ")*",
		")",
		"|",
		"(" ~~ T["HASH"] ~~ "|" ~~ P["class"] ~~ "|" ~~ P["attrib"] ~~ "|" ~~ P["negation"] ~~ "|" ~~ P["pseudo"] ~~ ")+",
	")"];
	
P["type_selector"] = "(" ~~ P["namespace_prefix"] ~~ "?" ~~ P["element_name"] ~~ ")";

P["universal"] = "(" ~~ P["namespace_prefix"] ~~ "?\\*)";


(* ::Section::Closed:: *)
(*Parse strings*)


(* ::Subsection::Closed:: *)
(*Identify full selectors and combinators*)


label["full_selector", x_String] := 
	Which[
		StringMatchQ[x, RegularExpression @ P["simple_selector_sequence"]], "simple_selector_sequence",
		StringMatchQ[x, RegularExpression @ P["combinator"]], "combinator",
		True, "error"]


parseSelector[selector_String] := 
	DeleteCases[
		StringSplit[selector, 
			s:(RegularExpression @ P["simple_selector_sequence"] | RegularExpression @ P["combinator"] | __) :> {label["full_selector", s], StringTrim @ s}], 
		""]


(* ::Subsection::Closed:: *)
(*Identify simple selectors*)


label["simple_selector", x_String] :=
	Which[
		StringMatchQ[x, RegularExpression @ P["attrib"]],        "attrib",
		StringMatchQ[x, RegularExpression @ T["HASH"]],          "hash",
		StringMatchQ[x, RegularExpression @ P["class"]],         "class",
		StringMatchQ[x, RegularExpression @ P["negation"]],      "negation",
		StringMatchQ[x, RegularExpression @ P["pseudo"]],        "pseudo",
		StringMatchQ[x, RegularExpression @ P["type_selector"]], "type",
		StringMatchQ[x, RegularExpression @ P["universal"]],     "universal",
		True, "error"]		
		
parseSimpleSelectorSequence[selectorSequence_String] := 
	DeleteCases[
		StringSplit[selectorSequence, (* StringSplit requires that productions and tokens be organized in decreasing complexity *)
			s:( 
				RegularExpression @ P["attrib"]    | RegularExpression @ T["HASH"]   | RegularExpression @ P["class"] | 
				RegularExpression @ P["negation"]  | RegularExpression @ P["pseudo"] | RegularExpression @ P["type_selector"] |
				RegularExpression @ P["universal"] | __ (* fall through case *)
			) :> {label["simple_selector", s], s}], 
		""]


(* ::Subsection::Closed:: *)
(*Attribute selector ---> [prefix|title=value]*)


(* parse the attribute selector string and return {attributeName, operator, attributeValue} or {attributeName} *)
parseAttrib[s_String] :=
	Which[
		(* full attribute pattern with or without attribute namespace prefix *)
		StringMatchQ[s, 
			RegularExpression[
				"\\[" ~~ T["S"] ~~ "*" ~~ P["namespace_prefix"] ~~ "?" ~~ T["IDENT"] ~~ T["S"] ~~ "*" ~~
				"((" ~~ T["PREFIXMATCH"] ~~ "|" ~~ T["SUFFIXMATCH"] ~~ "|" ~~ T["SUBSTRINGMATCH"] ~~ "|" ~~ "=" ~~ "|" ~~ T["INCLUDES"] ~~ "|" ~~ T["DASHMATCH"] ~~ ")" ~~
				T["S"] ~~ "*" ~~ "(" ~~ T["IDENT"] ~~ "|" ~~ T["STRING"] ~~ ")" ~~ T["S"] ~~ "*)\\]"]
		],
			StringCases[s, 
				StringExpression[
					RegularExpression["\\[" ~~ T["S"] ~~ "*"],
					attributeName:RegularExpression[P["namespace_prefix"] ~~ "?" ~~ T["IDENT"]],
					RegularExpression[T["S"] ~~ "*"],
					operator:RegularExpression[T["PREFIXMATCH"] ~~ "|" ~~ T["SUFFIXMATCH"] ~~ "|" ~~ T["SUBSTRINGMATCH"] ~~ "|" ~~ "=" ~~ "|" ~~ T["INCLUDES"] ~~ "|" ~~ T["DASHMATCH"]],
					RegularExpression[T["S"] ~~ "*"],
					attributeValue:RegularExpression[T["IDENT"] ~~ "|" ~~ T["STRING"]],
					RegularExpression[T["S"] ~~ "*"]
				] :> {attributeName, operator, attributeValue}][[1]]
		,
			
		(* minimal attribute pattern with or without attribute namespace prefix *)
		StringMatchQ[s, RegularExpression["\\[" ~~ T["S"] ~~ "*" ~~ P["namespace_prefix"] ~~ "?" ~~ T["IDENT"] ~~ T["S"] ~~ "*" ~~ "\\]"]],
			StringCases[s, 
				StringExpression[
					RegularExpression["\\[" ~~ T["S"] ~~ "*"],
					attributeName:RegularExpression[P["namespace_prefix"] ~~ "?" ~~ T["IDENT"]], 
					RegularExpression[T["S"] ~~ "*\\]"]
				] :> attributeName]			
		,
		
		True, Throw @ 
			Failure["UnexpectedParse", <|
				"MessageTemplate" -> "Unrecognized attribute selector.", 
				"Token" -> s|>]
	]


(* ::Subsection::Closed:: *)
(*Class selector ---> .class*)


parseClass[s_String] := StringTake[s, {2, -1}]


(* ::Subsection::Closed:: *)
(*ID selector ---> #tag*)


parseID[s_String] := StringTake[s, {2, -1}]


(* ::Subsection::Closed:: *)
(*Negation pseudo class ---> .not(...)*)


parseNegation[s_String] := StringTrim @ StringTake[s, {6, -2}]


(* ::Subsection::Closed:: *)
(*Pseudo ---> :class or ::element*)


parsePseudo[s_String] := 
	Module[{pseudoClassesNoArg, pseudoClassesANBArg, pseudoElements},
		pseudoClassesNoArg = 
			Alternatives[
				"hover", "active", "focus", "link", "visited", 
				"target", "enabled", "disabled", "checked", "indeterminate", "root",
				"first-child", "last-child", "first-of-type", "last-of-type",
				"only-child", "only-of-type", "empty"];
		pseudoClassesANBArg = Alternatives["nth-child", "nth-last-child", "nth-of-type", "nth-last-of-type"];
		pseudoElements = Alternatives["first-line", "first-letter", "before", "after"];
		
		Which[
			StringStartsQ[s, ":" ~~  pseudoClassesNoArg,  IgnoreCase -> True], {"pseudoclass",   StringTake[s, {2, -1}]},
			StringStartsQ[s, ":" ~~  pseudoClassesANBArg, IgnoreCase -> True], 
				{"pseudoclass",   First @ StringCases[s, ":" ~~ name:pseudoClassesANBArg ~~ "(" ~~ arg:__ ~~")" :> {name, parseANB @ arg}]},
			StringStartsQ[s, ":" ~~  pseudoElements,      IgnoreCase -> True], {"pseudoelement", StringTake[s, {2, -1}]},
			StringStartsQ[s, "::" ~~ pseudoElements,      IgnoreCase -> True], {"pseudoelement", StringTake[s, {3, -1}]},
			True, Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Unrecognized pseudo class or element.", "Token" -> s|>]
		]
	]

parseANB[s_String] :=
	Module[{x = StringTrim @ s},
		Which[
			StringMatchQ[x, RegularExpression[RE["O"] ~~ RE["D"] ~~ RE["D"]]],            {2, 1},
			StringMatchQ[x, RegularExpression[RE["E"] ~~ RE["V"] ~~ RE["E"] ~~ RE["N"]]], {2, 0}, 
			StringMatchQ[x, RegularExpression["[+\\-]?"] ~~ RegularExpression["[0-9]+"]], {0, Interpreter["Integer"][x]}, 
			StringMatchQ[x, RegularExpression["[+\\-]?([0-9]+)?" ~~ RE["N"] ~~ "(" ~~ RE["w"] ~~ "[+\\-]" ~~ RE["w"] ~~ "([0-9]+))?"]], 
				First @ StringCases[x, 
					(a:RegularExpression["[+\\-]?([0-9]+)?"]) ~~ RegularExpression[RE["N"]] ~~ rest___ :> 
					{
						Switch[a, "+"|"", 1, "-", -1, _, Interpreter["Integer"][a]], 
						With[{str = StringReplace[rest, Whitespace -> ""]}, If[str == "", 0, Interpreter["Integer"][str]]]}], 
			True, Throw[Failure["SelectorParse", <|"MessageTemplate" -> "Bad ANB pseudo class argument.", "AllTokens" -> s|>]]
		]		
	]


(* ::Section::Closed:: *)
(*Process simple selectors*)


$Debug = False;


(* ::Subsection::Closed:: *)
(*Universal selector*)


selectUniversal[content:{{_Integer..}...}, universal_String] := 
	Module[{namespace, default = "" /. $CSSNamespaces},
		namespace = 
			Which[
				(* match everything in all namespaces *)
				StringMatchQ[universal, "\\*|\\*"], None,
				
				(* match everything in a specific namespace *)			
				StringMatchQ[universal, __ ~~ "|\\*"], StringTake[universal, {1, -3}] /. $CSSNamespaces,
				
				(* match everything without a namespace *)
				StringMatchQ[universal, "|\\*"], "",
				
				(* match everything if no default namespace, or everything in the default namespace *)
				StringMatchQ[universal, "\\*"], If[default == "", None, default],
				
				True, Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Unrecognized universal selector.", "Token" -> universal|>]
			];
			
		If[namespace === None, 
			content
			,
			Pick[content, StringMatchQ[getNamespaceOfDocumentElement /@ content, namespace]]
		]
	]


(* ::Subsection::Closed:: *)
(*Type selector*)


validTypeQ[typeName_String] := 
	With[{type = Extract[$Document, Append[#, 1]]},
		Or[
			StringQ[type] && StringMatchQ[type, typeName, IgnoreCase -> $IgnoreCase["Type"]],
			ListQ[type] && MatchQ[type, {_String, t_String /; StringMatchQ[t, typeName, IgnoreCase -> $IgnoreCase["Type"]]}]
		]
	]&


selectType[content:{{_Integer..}...}, type_String] := 
	Module[{namespace, name, default = "" /. $CSSNamespaces},
		{namespace, name} = 
			Which[
				(* match element in all namespaces, including those without a namespace *)
				StringMatchQ[type, "\\*|" ~~ __], {None, StringTake[type, {3, -1}]},
				
				(* match element in a specific namespace *)
				StringMatchQ[type, __ ~~ "|" ~~ __], First @ StringCases[type, ns__ ~~ "|" ~~ t__ :> {ns /. $CSSNamespaces, t}],
					
				(* match element without a namespace *)
				StringMatchQ[type, "|" ~~ __], {"", StringTake[type, {2, -1}]},
					
				(* match element if no default namespace, or element in the default namespace *)
				True, {If[default == "", None, default], type}
			];
		If[namespace === None, 
			Pick[content, validTypeQ[name] /@ content]
			,
			Pick[content, Thread[And[StringMatchQ[getNamespaceOfDocumentElement /@ content, namespace], validTypeQ[name] /@ content]]]
		]			
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
(*name only*)


(* check against any namespace *)
validAttributeQ[None, {attributeName_String}] := 
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[$Debug, Print["AttOnly: ", #, " ", attributeList]];
		If[$Debug, Echo, Identity][
			MatchQ[attributeList, {___, ((name_String | {_String, name_String}) /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) -> _, ___}]
		]
	]&


(* check against specific namespace; empty string implies no namespace *)
validAttributeQ[attributeNamespace_String, {attributeName_String}] := 
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[$Debug, Print["AttAndName: ", #, " ", attributeList]];
		Which[
			(* case of '"IncludeNamespaces" -> True' and check for a match against the given attrib namespace, which could be empty e.g. "" *)
			MatchQ[attributeList, {___, {ns_String /; StringMatchQ[ns, attributeNamespace], name_String /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]} -> _, ___}],
				If[$Debug, Print["True and use attrib."]];
				True,
			
			(* case of '"IncludeNamespaces" -> Automatic' or 'False'; assume namespace is empty and only match against case *)
			MatchQ[attributeList, {___, (name_String /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) -> _, ___}],
				If[$Debug, Print["Automatic/False."]];
				True,
			
			(* fallthrough case: return False *)
			If[$Debug, Print["Fallthrough."]];
			True, False
		]
	]&


(* ::Subsubsection::Closed:: *)
(*name, operator, and value*)


operatorValueCondition[value_, operator_, attributeValue_] :=
	Switch[operator,
		"=", StringMatchQ[value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
		"*=", StringContainsQ[value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
		"^=", StringStartsQ[value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
		"$=", StringEndsQ[value, attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
		"~=", AnyTrue[StringSplit[value, Whitespace], StringMatchQ[attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]]], 
		"|=", StringMatchQ[First[StringSplit[value, "-"]], attributeValue, IgnoreCase -> $IgnoreCase["AttributeValue"]], 
		_, If[$Debug, "Operator fallthrough."]; False
	]


validAttributeQ[None, {attributeName_String, operator_String, attributeValue_String}] :=
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[$Debug, Print["AttAndVal: ", #, " ", attributeList, " ", {attributeName, operator, attributeValue}]];
		If[$Debug, Echo, Identity][
			MatchQ[attributeList,
				{___,
					((name_String | {_String, name_String}) /; StringMatchQ[name, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) ->
						(value_String /; operatorValueCondition[value, operator, attributeValue]),
				___}
			]
		]
	]&


validAttributeQ[attributeNamespace_String, {attributeName_String, operator_String, attributeValue_String}] :=
	With[{attributeList = Extract[$Document, Append[#, 2]]},
		If[$Debug, Print["AttAndNameAndVal: ", #, " ", attributeList]];
		Which[
			(* case of '"IncludeNamespaces" -> True' and check for a match against the given attrib namespace, which could be empty e.g. ""*)
			MatchQ[attributeList, 
				{___, {ns_String /; StringMatchQ[ns, attributeNamespace], n_String /; StringMatchQ[n, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]} -> 
					(v_String /; operatorValueCondition[v, operator, attributeValue]), ___}
			],
				If[$Debug, Print["True."]];
				True,
			
			(* case of '"IncludeNamespaces" -> Automatic' or 'False'; assume namespace is empty and only match against case  *)
			MatchQ[attributeList, 
				{___, (n_String /; StringMatchQ[n, attributeName, IgnoreCase -> $IgnoreCase["AttributeName"]]) -> 
					(v_String /; operatorValueCondition[v, operator, attributeValue]), ___}
			],
				If[$Debug, Print["Automatic/False."]];
				True,
			
			(* fallthrough case: return False *)
			True, If[$Debug, Print["Fallthrough."]]; False
		]
	]&


(* ::Subsubsection::Closed:: *)
(*selectAttribute*)


(* operator and value may not be present; selection steps are almost the same *)
selectAttribute[content:{{_Integer..}...}, fullAttribute:{attributeName_String, operatorAndValue___String}] := 
	Module[{namespace, name},
		If[Length[fullAttribute] == 2 || Length[fullAttribute] > 3, 
			Return[Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Wrong number of attribute components encountered.", "Token" -> StringJoin @ fullAttribute|>]]
		];
		
		{namespace, name} = 
			Which[
				(* match attribute in all namespaces, including those without a namespace *)
				StringMatchQ[attributeName, "\\*|" ~~ __], {None, StringTake[attributeName, {3, -1}]},
				
				(* match attribute in a specific namespace *)
				StringMatchQ[attributeName, __ ~~ "|" ~~ __], First @ StringCases[attributeName, ns__ ~~ "|" ~~ n__ :> {ns /. $CSSNamespaces, n}],
				
				(* match attribute without a namespace; the following two cases return the same values but must be parsed separately *)
				StringMatchQ[attributeName, "|" ~~ __], {"", StringTake[attributeName, {2, -1}]},
				True, {"",  attributeName}
			];
		Pick[content, validAttributeQ[namespace, {name, operatorAndValue}] /@ content]
	]


(* ::Subsection::Closed:: *)
(*Class selector*)


(*
	This selector is syntactic sugar. "div.value" has the same meaning as "div[class~=value]". 
	Because "." is effectively expanded to "class", the case-sensitivity is ambiguous. Using all lowercase is most conforming (and at least conforms to HTML).
	However, specifying a possible namespace is not available to this notation.
	In HTML, the lack of namespace is not an issue since all types and attributes belong in the HTML namespace.
	In XML docs, this can be a problem. For now, the implementation puts the 'class' attribute in the empty namespace.
	
	FIXME:
		XML authors should probably avoid this shorthand notation, or we should provide a way to expand the notation given user-supplied information.
		The case-sensitivity issue is also bad since we assume lowercase.
*)
selectClass[content:{{_Integer..}...}, value_String] := selectAttribute[content, {"class", "~=", value}]


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
	XML CSS authors should probably avoid this shorthand notation and instead use e.g. [name=p371].
	We could allow the user to specify multiple namespace+ID, e.g. an option-value could be '"ID" -> {(URI1 or prefix1) -> "id", (URI2 or prefix2) -> "name"}'
	selectID[content:{{_Integer..}...}, value_String] := selectAttribute[content, {("URI" /. $CSSnamespaces) <> "|" <> $ID, "=", value}]
*)
selectID[content:{{_Integer..}...}, value_String] := selectAttribute[content, {$ID, "=", value}]


(* ::Subsection::Closed:: *)
(*Negation selector*)


(*
	The surrounding 'not' and parentheses are assumed to be parsed away.
	Technically only one simple selector is allowed within 'not(...)', but we use processSelector with additional checks.
	The 'not' pseudoclass head does not contribute to specificity, but its argument does.
*)
Attributes[selectNegation] = {HoldFirst};
selectNegation[specificity_, content:{{_Integer..}...}, argument_String] :=
	Module[{parsed = processSimpleSelectorSequence[argument], newSelection},
		If[AnyTrue[parsed, MatchQ[{"error" | "negation", _String}]], 
			Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Invalid negation argument.", "Token" -> argument|>]
		];
		newSelection = processSelector[argument];
		specificity += newSelection["Specificity"];
		Complement[content, newSelection["Elements"]]
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
	As spec'ed, only XML elements are considered; string literals are removed.
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


selectPseudoClass[content:{{_Integer..}...}, arg1_] := Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Unrecognized pseudo class.", "Class" -> arg1|>]


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


processSimpleSelectorSequence[tokenizedSSS:{{_String, _String}...}] :=
	Module[{pos = 1, l = Length[tokenizedSSS], selection, specificity = {0, 0, 0, 0}, token, value, pseudo},
		(* $Elements is a list of positions of all XMLElement expressions *)
		selection = $Elements; 
		
		(* consume simple selectors until EOF or the next token is not part of a simple selector *)
		While[pos <= l,
			If[$Debug, Echo, Identity][{token, value} = tokenizedSSS[[pos]]];
			Switch[token,
				"universal", selection = selectUniversal[selection, value],				
				"type",   specificity[[4]]++; selection = selectType[selection, value],
				"attrib", specificity[[3]]++; selection = selectAttribute[selection, parseAttrib @ value],
				"class",  specificity[[3]]++; selection = selectClass[selection, parseClass @ value],
				"hash",   specificity[[2]]++; selection = selectID[selection, parseID @ value],
				"negation", selection = selectNegation[specificity, selection, parseNegation @ value],
				"pseudo", 
					Switch[pseudo = parsePseudo @ value,
						{"pseudoclass", _},   specificity[[3]]++; selection = selectPseudoClass[selection, Last @ pseudo],
						{"pseudoelement", _}, specificity[[4]]++; selection = selectPseudoElement[selection, Last @ pseudo]
					],
				"error", Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Unrecognized token in simple selector sequence.", "Token" -> value, "AllTokens" -> StringJoin @ tokenizedSSS[[All, 2]]|>],
				(* this fall through should never occur *)
				_, Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Malformed tokenized simple selector sequence.", "Token" -> tokenizedSSS[[pos]], "AllTokens" -> tokenizedSSS|>]
			];
			pos++
		];
		<|"Specificity" -> specificity, "Elements" -> If[selection === {}, {}, Sort @ selection]|>
	]


(* ::Subsection::Closed:: *)
(*Process combinator*)


(* Direct descendant: check that position depth is exactly greater by 2, but matches all ancestor positions indices *)
processCombinator[positions1_, ">", positions2_] := 
	Union @ Flatten[Select[positions2, Function[x, Length[#] + 2 == Length[x] && MatchQ[#, x[[;;Length[#]]]]] ]& /@ positions1, 1]

(* Any descendent: check that position depth is greater, but matches all ancestor position indices *)	
processCombinator[positions1_, "" | ">>", positions2_] := 
	Union @ Flatten[Select[positions2, Function[x, Length[#] < Length[x] && MatchQ[#, x[[;;Length[#]]]]] ]& /@ positions1, 1]
	
(* Immediate siblings; '#' is a slot for the older sibling, 'x' is a formal slot for the younger sibling *)
processCombinator[positions1_, "+", positions2_] := 
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
processCombinator[positions1_, "~", positions2_] := 
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


selectorOrderedCheck[tokenizedSelector:{{_String, _String}...}] :=
	Module[{pos = 2, l = Length[tokenizedSelector]},
		If[!TrueQ[tokenizedSelector[[1, 1]] == "simple_selector_sequence"], 
			Throw @ 
				Failure["UnexpectedParse", <|
					"MessageTemplate" -> "Selector does not start with simple selector sequence.", 
					"Token" -> tokenizedSelector[[1, 1]],
					"AllTokens" -> StringJoin @ tokenizedSelector[[All, 2]]|>]
		];
		If[!TrueQ[tokenizedSelector[[-1, 1]] == "simple_selector_sequence"], 
			Throw @ 
				Failure["UnexpectedParse", <|
					"MessageTemplate" -> "Selector does not end with simple selector sequence.", 
					"Token" -> tokenizedSelector[[1, 1]],
					"AllTokens" -> StringJoin @ tokenizedSelector[[All, 2]]|>]
		];
		If[l == 1, Return[]];
					
		While[pos <= l,
			If[!TrueQ[tokenizedSelector[[pos, 1]] == "combinator"], 
				Throw @ 
					Failure["UnexpectedParse", <|
						"MessageTemplate" -> "Expected combinator.", 
						"Token" -> tokenizedSelector[[pos, 2]], 
						"AllTokens" -> StringJoin @ tokenizedSelector[[All, 2]]|>]
			];
			pos++;
			If[!TrueQ[tokenizedSelector[[pos, 1]] == "simple_selector_sequence"], 
				Throw @ 
					Failure["UnexpectedParse", <|
						"MessageTemplate" -> "Expected simple selector.", 
						"Token" -> tokenizedSelector[[pos, 2]], 
						"AllTokens" -> StringJoin @ tokenizedSelector[[All, 2]]|>]
			];
			pos++
		]
	]


processSelector[selector_String] :=
	Module[{processed, attempt, pos, l},
		processed = parseSelector @ selector;
		l = Length[processed];
		
		(* check that nothing failed or had an empty simple selector sequence *)
		attempt = Cases[processed, {___, {"error", _String}, ___} | {}];
		If[attempt =!= {} || l == 0,
			Throw @ Failure["UnexpectedParse", <|"MessageTemplate" -> "Malformed selector sequence.", "Token" -> attempt[[All, 2]], "AllTokens" -> selector|>]
		];
		
		(* check that selectors start and end with a simple selector sequence, and if they have combinators, they alternate SSS, C, SSS, C, etc. *)
		selectorOrderedCheck @ processed;
		processed = processed /. {"simple_selector_sequence", x_} :> processSimpleSelectorSequence @ parseSimpleSelectorSequence @ x;
		
		(* Simply return if there are no combinators. *)
		If[l == 1, Return[First @ processed]];
		
		(* Otherwise combine selector results based on combinator type. Overwrite the right-most set of elements each time. *)
		pos = 2;
		While[pos < l,
			processed[[pos+1, "Elements"]] = processCombinator[processed[[pos-1, "Elements"]], processed[[pos, 2]], processed[[pos+1, "Elements"]]];
			pos += 2;
		];
		processed[[-1, "Specificity"]] = Total @ processed[[1;;-1;;2, "Specificity"]];
		processed[[-1, "Elements"]] = Sort @ processed[[-1, "Elements"]];
		Last[processed]
	]


(* ::Subsection::Closed:: *)
(*Process full selector (comma-separated selectors)*)


processFullSelector[fullSelector_String] :=
	Module[{selectors, attempt},
		selectors = StringTrim @ StringSplit[fullSelector, ","];
		attempt = Catch[processSelector /@ selectors];
		If[FailureQ[attempt], 
			attempt
			,
			If[Length[selectors] == 1, First @ attempt, attempt]
		]
	]


(* ::Section::Closed:: *)
(*Main Function*)


initializeGlobals[doc_, namespaces_List, id_String, {type_, name_, value_}] := (
	If[StringQ[doc], 
		$Document = Quiet @ ImportString[doc, "XML"];
		If[$Document === $Failed,
			$Elements = $DocumentRootPosition = $CSSNamespaces = $DocumentNamespaces = $DocumentLanguages = {};
			$ID = "id";
			$IgnoreCase = <|"Type" -> type, "AttributeName" -> name, "AttributeValue" -> value|>;
			Return[]
		]
		,
		$Document = doc
	];
	
	$Elements = Sort @ Position[$Document, XMLElement[__]];
	$DocumentRootPosition = First[$Elements, {}];

	(* CSS name spaces most likely passed in as an option *)
	$CSSNamespaces = namespaces;
	$DocumentNamespaces = getDocumentNamespaces[$Document];
	$DocumentLanguages = getDocumentLanguages[$Document];
	
	(* ID can be given as option; assumed to be "id" *)
	$ID = id;
	
	$IgnoreCase = <|"Type" -> type, "AttributeName" -> name, "AttributeValue" -> value|>;
	
	(* class is also assumed lowercase, but give user option to specify case *)
	(*$CLASSattrib = "class";*)
)


Options[Selector] = {
	"Namespaces" -> {}, 
	"ID" -> "id",
	"CaseSensitive" -> {"Type" -> False, "AttributeName" -> False, "AttributeValue" -> False}};

(*Selector[fullSelector_String] := Selector[fullSelector, ""]*)

Selector[document_, fullSelector_String, OptionsPattern[]] := 
	Module[{namespaces, id, type, name, value, temp},
		namespaces = OptionValue["Namespaces"];
		id = OptionValue["ID"];
		temp = OptionValue["CaseSensitive"];
		{type, name, value} =
			Which[
				temp === True,  {True, True, True},
				temp === False, {False, False, False},
				True, {"Type", "AttributeName", "AttributeValue"} /. OptionValue["CaseSensitive"] /. _String :> False
			];
				
		initializeGlobals[document, namespaces, id, {!type, !name, !value}];
		processFullSelector @ fullSelector
	]
	
Selector /: Position[document:(obj_ /; Head[obj] === XMLObject["Document"] || Head[obj] === XMLElement), Selector[fullSelector_String, opts:OptionsPattern[]]] := 
	Module[{temp = Selector[document, fullSelector, opts]},
		If[ListQ[temp], temp[[All, "Elements"]], temp["Elements"]]
	]

Selector /: Position[_, Selector[fullSelector_String]] := {}


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
