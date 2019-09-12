(* Wolfram Language Package *)

(* ::Title:: *)
(*CSS Visual Style Importer*)


(* ::Text:: *)
(*Author: Kevin Daily*)
(*Date: 20190911*)
(*Version: 1*)


BeginPackage["CSSTools`", { "GeneralUtilities`"}]
(* Exported symbols added here with SymbolName::usage *) 

(* ==== CSSStyleSheetInterpreter` ==== *)
SetUsage[CSSCascade, "\
CSSCascade[CSSProperty$, type$, CSSData$, CSSSelectorFilter] combines options that were interpreted from the CSS importer. \
CSS declarations are merged following the CSS cascade and the resulting options are filtered by type$."];
SetUsage[ExtractCSSFromXML, "\
ExtractCSSFromXML[XMLObject$] imports the CSS declarations within XMLObject$."];
SetUsage[CSSTargets, "\
CSSTargets[XMLObject$, CSSData$] applies the CSSData$ to the symbolic XML, \
returning the CSSData$ with additional position and specificity information."];
SetUsage[CSSInheritance, "\
CSSInheritance[target$, type$, CSSData$] calculates the properties of the element at target$ including any inherited CSS properties."];

(* ==== Selectors3` ==== *)
SetUsage[CSSSelector, "\
CSSSelector[string$] parses string$ as a CSS selector. If valid it returns a CSSSelector object."];
SetUsage[CSSSelectorQ, "\
CSSSelectorQ[CSSSelector$] returns True if CSSSelector$ is a valid CSSSelector object."];

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

EndPackage[]

