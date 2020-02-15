(* Wolfram Language Package *)


(* ::Section:: *)
(*CSS calc() Expression Tests*)


Get["CSSTools`CSSTokenizer`"] (* defines CSSTokenize/CSSUntokenize *)


(* Basic addition/subtraction *)
Test[
	And[
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(1px - 2px)"]]], "-1px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px - 2px  )"]]], "-1px"],
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px   -   2px  )"]]], "-1px"]
	],
	True]
	

Test[
	And[
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(1px + 2px)"]]], "3px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px + 2px  )"]]], "3px"],
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  1px   +   2px  )"]]], "3px"]
	],
	True]


Test[
	And[
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2px - 1px)"]]], "1px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  2px - 1px  )"]]], "1px"],
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(  2px   -   1px  )"]]], "1px"]
	],
	True]
	
	
(* Basic multiplication/division *)
Test[
	And[
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px*2)"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 5px*2 )"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2*5px)"]]], "10px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(2 * 5px)"]]], "10px"] 
	],
	True]
	
	
Test[
	And[
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(10px/2)"]]], "5px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 10px/2 )"]]], "5px"],
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px/2)"]]], "2.5px"], 
		StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc( 5px/2 )"]]], "2.5px"] 
	],
	True]
	
	
(* keep similar types together *)
Test[StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(100%/3 - 2*1em - 2*1px)"]]], "calc(33.3333% - 2em - 2px)"], True]

(* nesting is allowed *)
Test[StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(calc(100%)"]]], "100%"], True]

(* complicated example with parenthetical grouping *)
Test[StringMatchQ[CSSUntokenize @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(5px + 2*calc(100%/3 - 21px/2)/3)"]]], "calc(22.2222% - 2.0px)"], True]

(* each term should have an operator in between (checked after multiplication resolves) *)
TestMatch[Catch @ CSSTools`CSSValuesAndUnits3`Private`calcReduce[First[CSSTokenize["calc(3px+10%)"]]], _Failure]

