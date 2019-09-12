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

The color definitions from CSS Level 2 Revision 1 are limited in comparison with Colors Module Level 4. We extend its definitions by creating a new package:
 "CSSColors4.wl". Its functionality is extended by following these steps:

1. The parser function `parseSingleColor` defined in CSSPropretyInterpreter.wl is made public by moving its name before the ``Begin["`Private`"]`` section.
2. Create a new package CSSColors4.wl
3. In the new package keep the context within CSSTools: ``BeginPackage["CSSTools`CSSColors4`", {"CSSTools`"}];``
4. Always load the tokenizer as we will be processing CSS tokens: ``Needs["CSSTools`CSSTokenizer`"];``
5. Load the property interpreter to modify `parseSingleColor`: ``Needs["CSSTools`CSSPropertyInterpreter`"]``
6. Create new private functions that extend the functionality.
7. Modify as necessary existing functions such as the `CSSPropertyData` (also publicly exposed):
 ```
 If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, { (* AssociateTo is used in case the package order changes *)
	"color" -> <|
		"Inherited" -> True,
		"CSSInitialValue" -> "black", (* CSS 2.1 did not define this *)
		"InterpretedGlobalValues" -> <|
			"inherit" -> FontColor -> Inherited,
			"initial" -> FontColor -> Black|>|>}]
 ```
8. Add a new definition for `parseSingleColor`.
9. Add the package to CSSTools.m after the loading of the main packages: ``Get["CSSTools`CSSColors4`"];``


## Example of Adding a New Property

The [CSS Paged Media Module Level 3](https://www.w3.org/TR/css-page-3/) adds new page-specific properties 'size', 'marks', 'bleed' and 'page'. Though these printing properties are not all fully supported in the Wolfram Desktop, any new properties should still be developed to validate the input CSS. The following steps show how to add the 'bleed' property to the existing set of parsed CSS properties.

1. The parser function `consumeProperty` should already be made public in the  CSSPropretyInterpreter.wl package.
2. In the sub-package (here CSSPagedMedia3.wl) create the new property consuming variant:
```
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
``` 
3. Add to the CSSPropertyData to define the initial and inherit CSS values.
```
If[!AssociationQ[CSSPropertyData], CSSPropertyData = <||>];
AssociateTo[CSSPropertyData, 
	{
		"bleed" -> <|
			"Inherited" -> False,
			"CSSInitialValue" -> "auto",
			"InterpretedGlobalValues" -> <|
				"inherit" -> <||>, (* no equivalent FE option *)
				"initial" -> Missing["Not supported."]|>|>}];
```

The method of parsing of new properties is largely up to the contributer, but we encourage certain features.
A. follow the existing specification 
B. return a descriptive `Failure` object if parsing fails for any reason
C. return `Missing["Not supported."]` if the Wolfram Desktop front end can not currently use this feature

To assist parsing of tokens functions like `CSSTokenQ`, `TokenTypeIs` and `AdvancePosAndSkipWhitespace` exist in the CSSTokenizer.wl package. These utility functions were not used in this simpler example. 

In the above example the functions `tooManyTokensFailure` and `parseLength` were used and come from the CSSPropretyInterpreter.wl package (publicly exposed).
