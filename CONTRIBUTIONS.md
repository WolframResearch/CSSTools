
# Contributing to CSSTools

CSSTools consists of packages that are designed to follow CSS module organization. The core packages include:

| File                        | Description |
| --------------------------- | ----------- |
| CSSTokenizer.wl             | converts strings into CSS tokens; contains utilities for processing CSS tokens |
| CSSPropretyInterpreter.wl   | contains all CSS Level 2 Revision 1 properties and how they are translated to Wolfram Desktop options |
| CSSStyleSheetInterpreter.wl | parsing of a CSS stylesheet and assembly of interpreted Wolfram Desktop options |
| CSSSelectors3.wl            | defines CSSSelector following CSS Selectors Level 3 |

Additional packages are named after the corresponding CSS module. They are intended to modify existing functionality within the core packages.

The tokenizer is not loaded when CSSTools loads but can be accessed via ``Needs[CSSTools`CSSTokenizer`]``.

The other three files are always loaded first in order to define the core functionality. Additional packages are loaded after and modify or add to the core functions.


## Example of Modifying an Existing Parser

The color definitions from CSS Level 2 Revision 1 are limited in comparison with Colors Module Level 4. We extend its definitions by creating a new package: "CSSColors4.wl". Its functionality is extended by following these steps:

1. The parser function `parseSingleColor` defined in CSSPropretyInterpreter.wl is made public by moving its name before the ``Begin["`Private`"]`` section.  

    _tart of CSSPropretyInterpreter.wl_  
        
        (* Wolfram Language Package *)
        
        BeginPackage["CSSTools`CSSPropertyInterpreter`", {"CSSTools`"}]
        
        Needs["CSSTools`CSSTokenizer`"]; (* keep tokenizer utilities hidden *)
        
        (* functions to expose to outside packages via Needs *)
        ...
        (*STEP 1*)parseSingleColor;
        CSSPropertyData;
        ...
        
        Begin["`Private`"] (* Begin Private Context *)
        ...
        
2. Create a new package CSSColors4.wl where in this new package
	A. keep the context within CSSTools
	B. load the tokenizer as we will be processing CSS tokens
	C. load the property interpreter to modify `parseSingleColor`: 
    
    _Start of CSSColors4.wl_  

        (* Wolfram Language Package *)
        
        BeginPackage["CSSTools`CSSColors4`", {(*STEP 2A*)"CSSTools`"}]; 
        
        (*STEP 2B*) Needs["CSSTools`CSSTokenizer`"];
        (*STEP 2C*) Needs["CSSTools`CSSPropertyInterpreter`"];
        
        Begin["`Private`"]; (* Begin Private Context *)
        ...

3. Create new private functions that extend the functionality.

    _Somewhere within CSSPropretyInterpreter.wl after ``Begin["`Private`"]``_  
        
        ...
        parseSingleColorKeyWord[prop_String, keyword_String] := ...
        ...
        parseSingleColorHex[prop_String, hexString_String] := ...
        ...
        parseSingleColorFunction[prop_String, token_?CSSTokenQ] := ...
        ...

4. Modify as necessary existing functions such as the `CSSPropertyData` (also publicly exposed):

    _Somewhere within CSSPropretyInterpreter.wl after ``Begin["`Private`"]``_  
        
        ...
        If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
        AssociateTo[CSSPropertyData, { (* AssociateTo is used in case the package order changes *)
	        "color" -> <|
		        "Inherited" -> True,
		        "CSSInitialValue" -> "black", (* CSS 2.1 did not define this *)
		        "InterpretedGlobalValues" -> <|
 			        "inherit" -> FontColor -> Inherited,
			        "initial" -> FontColor -> Black|>|>}]
        ...

5. Add a new definition for `parseSingleColor`.

    _Somewhere within CSSPropretyInterpreter.wl after ``Begin["`Private`"]``_  

        ...
        (* new interpreters *)
        (* parse all color types *)
        parseSingleColor[prop_String, token_?CSSTokenQ] := parseSingleColor[prop, token] = 
	        Switch[token["Type"],
		        "ident",    parseSingleColorKeyWord[prop, token["String"]],
		        "hash",     parseSingleColorHex[prop, token["String"]],
		        "function", parseSingleColorFunction[prop, token],
        		_,          unrecognizedValueFailure @ prop]
        ...

6. Add the package to CSSTools.m after the loading of the main packages: ``Get["CSSTools`CSSColors4`"];``

    _Somewhere within CSSTools.m after defining package-scope functions_  

        ...
        (* ==== load base packages ==== *)
        Get["CSSTools`CSSSelectors3`"]            (* defines CSSSelector object *)
        Get["CSSTools`CSSPropertyInterpreter`"]   (* needs to be loaded first to define basic CSS 2.1 properties *)
        Get["CSSTools`CSSStyleSheetInterpreter`"] (* defines basic stylesheet token consumers and CSS cascade *)
        
        (* ==== load upgrade packages ==== *)
        (*STEP 6*)Get["CSSTools`CSSColors4`"];    (* redefines parseSingleColor     (first defined in CSSPropertyInterpreter) *)
        Get["CSSTools`CSSPagedMedia3`"]           (* redefines @page token consumer (first defined in CSSStyleSheetInterpreter) *)
        Get["CSSTools`CSSMediaQueries4`"]         (* redefines consumeMediaQuery    (first defined in CSSStyleSheetInterpreter) *)
        ...

## Example of Adding a New Property

The [CSS Paged Media Module Level 3](https://www.w3.org/TR/css-page-3/) adds new page-specific properties 'size', 'marks', 'bleed' and 'page'. Though these printing properties are not all fully supported in the Wolfram Desktop, any new properties should still be developed to validate the input CSS. The following steps show how to add the 'bleed' property to the existing set of parsed CSS properties.

1. The parser function `consumeProperty` should already be made public in the CSSPropretyInterpreter.wl package.

    _Start of CSSPropretyInterpreter.wl_
        
        (* Wolfram Language Package *)
        
        BeginPackage["CSSTools`CSSPropertyInterpreter`", {"CSSTools`"}]
        (* Exported symbols added here with SymbolName::usage *)  
        
        Needs["CSSTools`CSSTokenizer`"]; (* keep tokenizer utilities hidden *)
        
        (* functions to expose to outside packages via Needs *)
        CSSPropertyData;
        (*STEP 1*)consumeProperty;
        ...
        
        Begin["`Private`"] (* Begin Private Context *) 
        ...
        
2. In the sub-package (here CSSPagedMedia3.wl) create the new property consuming variant:
    
    _Somewhere within CSSPagedMedia3.wl_  
        
        consumeProperty[prop:"bleed", tokens:{__?CSSTokenQ}] := 
        	Module[{pos = 1, l = Length[tokens], value},
        		If[l > 1, Return @ tooManyTokensFailure @ tokens];
        		value = 
        			Switch[tokens[[pos]]["Type"],
        				"ident", 
        					Switch[ToLowerCase @ tokens[[pos]]["String"],
        						"auto", Missing["Not supported."], (* Computes to 6pt if marks has crop and to zero otherwise. *)
        						_,      unrecognizedKeyWordFailure @ prop
        					],
        				"dimension", parseLength @ tokens[[pos]]
        			];
        		If[FailureQ[value], value, Missing["Not supported."]]
        	]
        ...

3. Add to the CSSPropertyData to define the initial and inherited CSS values.

    _Somewhere within CSSPagedMedia3.wl_  

        If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
        AssociateTo[CSSPropertyData, 
        	{
        		"bleed" -> <|
        			"Inherited" -> False,
        			"CSSInitialValue" -> "auto",
        			"InterpretedGlobalValues" -> <|
        				"inherit" -> <||>, (* no equivalent FE option *)
        				"initial" -> Missing["Not supported."]|>|>}];
        ...

The method of parsing of new properties is largely up to the contributer, but we encourage certain features.

A. follow the existing specification 
B. return a descriptive `Failure` object if parsing fails for any reason
C. return `Missing["Not supported."]` if the Wolfram Desktop front end can not currently use this feature


## Details of the tokenizer

To assist parsing of tokens functions like `CSSTokenQ`, `TokenTypeIs` and `AdvancePosAndSkipWhitespace` exist in the CSSTokenizer.wl package. These utility functions were not used in this simpler example. 

In the above example the functions `tooManyTokensFailure` and `parseLength` were used and come from the CSSPropretyInterpreter.wl package (publicly exposed).
