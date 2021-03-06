(* Wolfram Language Package *)

(* ::Title:: *)
(*CSS Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20200716*)
(*Version: 1.3.1*)


BeginPackage["CSSTools`"]
(* Exported symbols added here with SymbolName::usage *) 

Needs["GeneralUtilities`"]; (* for SetUsage *)

Unprotect["CSSTools`*"]

(* ==== CSSStyleSheetInterpreter` ==== *)
SetUsage[CSSCascade, "\
CSSCascade[CSSProperty$, type$, CSSData$, CSSSelectorFilter$] combines options that were interpreted from the CSS importer. \
CSS declarations are merged following the CSS cascade and the resulting options are filtered by type$."];

SetUsage[ExtractCSSFromXML, "\
ExtractCSSFromXML[XMLObject$] imports the CSS declarations within XMLObject$."];

SetUsage[CSSSubjects, "\
CSSSubjects[XMLObject$, CSSData$] applies the CSSData$ to the symbolic XML, returning the CSSData$ with the subjects of the selectors."];

SetUsage[CSSInheritance, "\
CSSInheritance[CSSSubject$, type$, CSSData$] calculates the properties of the CSSSubject$ including any inherited CSS properties."];

(* ==== Selectors3` ==== *)
SetUsage[CSSSelector, "\
CSSSelector[string$] parses string$ as a CSS selector. If valid it returns a CSSSelector object."];

SetUsage[CSSSelectorQ, "\
CSSSelectorQ[CSSSelector$] returns True if CSSSelector$ is a valid CSSSelector object."];

SetUsage[CSSSubject, "\
CSSSubject[string$] parses string$ as the XML object that is the subject of a CSS selector. If valid it returns a CSSSubject object."];

SetUsage[CSSSubjectQ, "\
CSSSubjectQ[CSSSubject$] returns True if CSSSubject$ is a valid CSSSubject object."];

(* === CSSImages3` === *)
SetUsage[CSSLinearGradientImage, "\
CSSLinearGradientImage[{{pos$1, color$1}, {pos$2, color$2}, $$}] returns an image with values linearly changing from top to bottom following the CSS specification for color stops.
CSSLinearGradientImage[{{pos$1, color$1}, {pos$2, color$2}, $$}, direction$] returns an image where the gradient points along direction$.
CSSLinearGradientImage[$$, size$] returns a linear gradient image of the specified size$."];

SetUsage[CSSRadialGradientImage, "\
CSSRadialGradientImage[{{pos$1, color$1}, {pos$2, color$2}, $$}] returns an image with values linearly changing from the center to corners following the CSS specification for color stops.
CSSRadialGradientImage[{{pos$1, color$1}, {pos$2, color$2}, $$}, startPosition$] returns an image where the gradient center is located at startPosition$.
CSSRadialGradientImage[$$, size$] returns a radial gradient image of the specified size$."];

(* ==== required System` functions ==== *)
System`CellFrameStyle; 
System`Box;

(* ==== load base packages ==== *)
Get["CSSTools`CSSSelectors3`"]            (* defines CSSSelector object *)
Get["CSSTools`CSSPropertyInterpreter`"]   (* needs to be loaded first to define basic CSS 2.1 properties *)
Get["CSSTools`CSSStyleSheetInterpreter`"] (* defines basic stylesheet token consumers and CSS cascade *)

(* ==== load upgrade packages ==== *)
Get["CSSTools`CSSColors4`"];              (* redefines parseSingleColor     (first defined in CSSPropertyInterpreter) *)
Get["CSSTools`CSSPagedMedia3`"]           (* redefines @page token consumer (first defined in CSSStyleSheetInterpreter) *)
Get["CSSTools`CSSMediaQueries4`"]         (* redefines consumeMediaQuery    (first defined in CSSStyleSheetInterpreter) *)
Get["CSSTools`SVG11`"]                    (* adds consumeProperty definitions for SVG 1.1 *)
Get["CSSTools`CSSValuesAndUnits3`"]       (* prepends consumeProperty definitions for values that contain attr() and calc() functions *)
Get["CSSTools`CSSCustomProperties1`"]     (* prepends consumeProperty definitions for custom properties i.e. --* idents, and var() function *)
Get["CSSTools`CSSImages3`"]               (* defines gradient images *)


Begin["`Private`"]

ImportExport`RegisterImport[
	"CSS",
	{
		"Elements" :> (("Elements" -> {"RawData", "Interpreted", "Stylesheet"})&),
		(* Interpreted is the same as default *)
		"Interpreted" :> (("Interpreted" -> With[{d = CSSTools`CSSStyleSheetInterpreter`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&), 
		"RawData" :> (("RawData" -> With[{d = CSSTools`CSSStyleSheetInterpreter`Private`RawCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&),
		"Stylesheet" :> (("Stylesheet" -> CSSTools`CSSStyleSheetInterpreter`Private`ProcessToStylesheet[#])&),
		((With[{d = CSSTools`CSSStyleSheetInterpreter`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&)},
	{},
	"AvailableElements" -> {"Elements", "RawData", "Interpreted", "Stylesheet"}]

End[]


SetAttributes[
	{
		CSSCascade, ExtractCSSFromXML, CSSSubjects, CSSInheritance, 
		CSSSelector, CSSSelectorQ, CSSSubject, CSSSubjectQ,
		CSSLinearGradientImage, CSSRadialGradientImage}, 
	{Protected, ReadProtected}];


EndPackage[]

