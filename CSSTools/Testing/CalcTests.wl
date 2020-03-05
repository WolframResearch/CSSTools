(* Wolfram Language Package *)


(* ::Section:: *)
(*CSS calc() Expression Tests*)


Get["CSSTools`CSSTokenizer`"] (* defines CSSTokenize/CSSUntokenize *)


(* Basic addition/subtraction *)
Test[
	And[
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(1px - 2px)"]]], "-1px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px - 2px  )"]]], "-1px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px   -   2px  )"]]], "-1px"]
	],
	True]
	

Test[
	And[
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(1px + 2px)"]]], "3px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px + 2px  )"]]], "3px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px   +   2px  )"]]], "3px"]
	],
	True]


Test[
	And[
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2px - 1px)"]]], "1px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  2px - 1px  )"]]], "1px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  2px   -   1px  )"]]], "1px"]
	],
	True]
	
	
(* Basic multiplication/division *)
Test[
	And[
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px*2)"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 5px*2 )"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2*5px)"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2 * 5px)"]]], "10px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2.2 * 5px)"]]], "11.0px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc((2)*(((5px))))"]]], "10px"] 
	],
	True]
	
	
Test[
	And[
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(10px/2)"]]], "5px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 10px/2 )"]]], "5px"],
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px/2)"]]], "2.5px"], 
		StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 5px/2 )"]]], "2.5px"] 
	],
	True]
	
	
(* keep similar types together *)
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(100%/3 - 2*1em - 2*1px)"]]], "calc(33.3333% - 2em - 2px)"], True]

(* nesting is allowed *)
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(calc(100%)"]]], "100%"], True]
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc((2))"]]], "2"], True]
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2/(3))"]]], "0.666667"], True]

(* complicated example with parenthetical grouping *)
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px + 2*calc(100%/3 - 21px/2)/3)"]]], "calc(22.2222% - 2.0px)"], True]

(* corner case of required grouping with subtraction *)
Test[StringMatchQ[CSSUntokenize @ Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(calc(2% - 2px) - calc(4% - 5px))"]]], "calc(-2% + 3px)"], True]

(* each term should have an operator in between (checked after multiplication resolves) *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(3px+10%)"]]], _Failure]

(* ==== correct failure detections ==== *)
(* space between number and unit *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(100%/3 - 2*1 em - 2*1px)"]]], _Failure]

(* missing multiplication sign (multiplication is not implied) *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(calc(3) calc(calc(100%)))"]]], _Failure]
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(((3px)(2))*(2))"]]], _Failure]

(* missing spaces around + operator *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(3px+10%)"]]], _Failure]

(* px (length) and hz (frequency) are units of different types *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(3px + 2hz)"]]], _Failure]

(* missing terms *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(3px + ( + ))"]]], _Failure]
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(( + ))"]]], _Failure]



