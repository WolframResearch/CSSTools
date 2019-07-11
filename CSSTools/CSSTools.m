(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 5, 2019 *)

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

(* ==== CSSImport` ==== *)
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

(* ==== required System` functions ==== *)
System`CellFrameStyle; 
System`Box;

Needs["CSSTools`CSSPropertyInterpreter`"] (* needs to be loaded first to define basic CSS 2.1 properties *)
Echo[Keys @ CSSPropertyData]

Needs["CSSTools`CSSColors4`"]; (* redefines parseSingleColor first defined in CSSPropertyInterpreter *)
Echo[Keys @ CSSPropertyData]

Needs["CSSTools`CSSImport`"]

EndPackage[]

