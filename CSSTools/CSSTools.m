(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 5, 2019 *)

(* ::Title:: *)
(*CSS 2.1 Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20190321*)
(*Version: 1*)


BeginPackage["CSSTools`", { "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *) 

(* ==== CSSPropertyInterpreter` ==== *)
SetUsage[CSSHeightMin, "\
CSSHeightMin[value$] indicates value$ is to be interpreted as a minimum height taken from a CSS property."];
SetUsage[CSSHeightMax, "\
CSSHeightMax[value$] indicates value$ is to be interpreted as a maximum height taken from a CSS property."];
SetUsage[CSSWidthMin, "\
CSSWidthMin[value$] indicates value$ is to be interpreted as a minimum width taken from a CSS property."];
SetUsage[CSSWidthMax, "\
CSSWidthMax[value$] indicates value$ is to be interpreted as a maximum width taken from a CSS property."];
SetUsage[CSSBorderColor, "\
CSSBorderColor[value$] indicates value$ is an interpreted CSS color."];
SetUsage[CSSBorderStyle, "\
CSSBorderStyle[value$] indicates value$ is an interpreted CSS border style."];
SetUsage[CSSBorderWidth, "\
CSSBorderWidth[value$] indicates value$ is an interpreted CSS border width."];

(* ==== CSSStyleSheetInterpreter` ==== *)
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

(* ==== Selectors3` ==== *)
SetUsage[Selector, "\
Selector[selector$, XMLObject$] returns the CSS selector$ specificity and positions of XMLElement expressions in XMLObject$. 
Position[XMLObject$, Selector[selector$]] returns only the positions of XMLElement expressions."];

(* ==== required System` functions ==== *)
System`CellFrameStyle; 
System`Box;

Get["CSSTools`CSSPropertyInterpreter`"]   (* needs to be loaded first to define basic CSS 2.1 properties *)
Get["CSSTools`CSSStyleSheetInterpreter`"] (* defines basic stylesheet token consumers and CSS cascade *)
Get["CSSTools`CSSColors4`"];              (* redefines parseSingleColor (first defined in CSSPropertyInterpreter) *)
Get["CSSTools`CSSPagedMedia3`"]           (* redefines @page token consumer (first defined in CSSStyleSheetInterpreter) *)

ImportExport`RegisterImport[
	"CSS",
	{
		"Elements" :> (("Elements" -> {"RawData", "Interpreted", "Stylesheet"})&),
		(* Interpreted is the same as default *)
		"Interpreted" :> (("Interpreted" -> With[{d = CSSTools`CSSStyleSheetInterpreter`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&), 
		"RawData" :> (("RawData" -> With[{d = CSSTools`CSSStyleSheetInterpreter`Private`RawCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&),
		"Stylesheet" :> ProcessToStylesheet,
		((With[{d = CSSTools`CSSStyleSheetInterpreter`Private`InterpretedCSS[#]}, If[FailureQ[d], d, Dataset @ d]])&)},
	{},
	"AvailableElements" -> {"Elements", "RawData", "Interpreted", "Stylesheet"}]

EndPackage[]

