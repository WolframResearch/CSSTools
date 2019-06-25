(* Wolfram Language Package *)

BeginPackage["CSS21Parser`"];
Needs["CSSTools`CSSTokenizer`"];

Begin["`Private`"]; (* Begin Private Context *) 

(* we assume that the @page rule has already been tokenized *)


SetAttributes[{consumeAtPageRule}, HoldFirst];

consumeAtPageRule[pos_, tokens_] := 
	Module[{},
		Null
	]


End[]; (* End Private Context *)
EndPackage[];
