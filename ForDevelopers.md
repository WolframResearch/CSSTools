# Contributing to CSSTools

## Table of Contents

[Overview](#overview)

[Details of the tokenizer](#details-of-the-tokenizer)

[Example of Modifying an Existing Parser](#example-of-modifying-an-existing-parser)

[Example of Adding a New Property](#example-of-adding-a-new-property)


## Overview

CSSTools consists of packages that are designed to follow CSS module organization. The core packages include:

| File                        | Description |
| --------------------------- | ----------- |
| CSSTokenizer.wl             | converts strings into CSS tokens; contains utilities for processing CSS tokens |
| CSSPropretyInterpreter.wl   | contains all CSS Level 2 Revision 1 properties and how they are translated to Wolfram Desktop options |
| CSSStyleSheetInterpreter.wl | parsing of a CSS stylesheet and assembly of interpreted Wolfram Desktop options |
| CSSSelectors3.wl            | defines CSSSelector following CSS Selectors Level 3 |

Additional packages are named after the corresponding CSS module. They are intended to modify existing functionality within the core packages.

The other three files are always loaded first in order to define the core functionality. Additional packages are loaded after and modify or add to the core functions.


## <a name="details-of-the-tokenizer"></a>Details of the tokenizer

The tokenizer follows CSS Syntax Module Level 3. It also allows "ident" tokens to start with "--" for possible future use of [CSS Custom Properties for Cascading Variables Module Level 1](https://www.w3.org/TR/css-variables-1/). The tokenizer also does a small amount of parsing. In particular, brackets like `[]`, `{}` and `()` are matched into block tokens. These block tokens have a key "Children" whose value is a flat list of CSS tokens that are within the scope of the block. 

The tokenizer is not loaded when CSSTools loads but can be accessed via 
```
Needs["CSSTools`CSSTokenizer`"]
```
There are two functions that provide the tokenizing and the serialization: `CSSTokenize` and `CSSUntokenize`. `CSSTokenize` operates on a string and creates a flat list of tokens. Individual tokens have the head `CSSToken` and contain a single argument: an association that indicates their structure. For example:
```
In[] := tokens = CSSTokenize["h1    {color:\\red}"]
Out[] = {
  CSSToken[<|"Type" -> "ident", "String" -> "h1", "RawString" -> "h1"|>], 
  CSSToken[<|"Type" -> "whitespace", "String" -> " "|>], 
  CSSToken[<|"Type" -> "{}", "Children" -> {
    CSSToken[<|"Type" -> "ident", "String" -> "color", "RawString" -> "color"|>], 
    CSSToken[<|"Type" -> "colon", "String" -> ":"|>], 
    CSSToken[<|"Type" -> "ident", "String" -> "red", "RawString" -> "\\red"|>]}|>]}
```
The `"RawString"` key is used to store the unmodified original string, while the `"String"` key contains a "normalized" version of the string e.g. any escaped characters converted.

Use `CSSUntokenize` to serialize the tokens back into a string:
```
In[] := CSSUntokenize[tokens]
Out[] = "h1 {color:\\red}" 
```
Following the CSS syntax module specification, a round-trip of `CSSUntokenize[CSSTokenize[...]` is not guaranteed to return the same initial string. The reason is that some characters like whitespace can be simplified without loss of information. However, `CSSTokenize[CSSUntokenize[CSSTokenize[...]]` must be the same as the original set of tokens.
```
In[] := tokens === CSSTokenize[CSSUntokenize[tokens]]
Out[] = True
```
To assist parsing of tokens functions like `CSSTokenQ`, `TokenTypeIs` and `AdvancePosAndSkipWhitespace` exist in the CSSTokenizer.wl package. These utility functions were not used in this simpler example. 


## <a name="example-of-modifying-an-existing-parser"></a>Example of Modifying an Existing Parser

The color definitions from CSS Level 2 Revision 1 are limited in comparison with Colors Module Level 4. We extend its definitions by creating a new package: "CSSColors4.wl". Its functionality is extended by following these steps:

1. The parser function `parseSingleColor` defined in CSSPropretyInterpreter.wl is made public by moving its name before the ``Begin["`Private`"]`` section.  

    _Start of CSSPropretyInterpreter.wl_  
        
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


## <a name="example-of-adding-a-new-property"></a>Example of Adding a New Property

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
        
        ...
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
        
    In the above code the functions `tooManyTokensFailure` and `parseLength` were used and come from the CSSPropretyInterpreter.wl package (publicly exposed).
  
3. Add to the CSSPropertyData to define the initial and inherited CSS values.

    _Somewhere within CSSPagedMedia3.wl_  

        ...
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

1. follow the existing specification 

2. return a descriptive `Failure` object if parsing fails for any reason

3. return `Missing["Not supported."]` if the Wolfram Desktop front end can not currently use this feature


