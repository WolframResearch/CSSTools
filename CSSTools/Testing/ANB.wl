(* ::Package:: *)

(* ::Section:: *)
(*ANB Sub-Grammar Tests*)


(* ::Subsection::Closed:: *)
(*Generate Tests*)


(* ::Input:: *)
(*(* Original test data. Null indicates expected failure. *)tests={{"",Null},{"  \n",Null},{"odd",{2,1}},{"even",{2,0}},{"\[ODoubleDot]dd",Null},{"\[EAcute]ven",Null},{" /**/\t OdD /**/\n",{2,1}},{" /**/\t EveN /**/\n",{2,0}},{"3",{0,3}},{"+2 ",{0,2}},{" -14 ",{0,-14}},{"+ 2 ",Null},{"- 14 ",Null},{"3.1",Null},{"3N",{3,0}},{"+2N ",{2,0}},{" -14n ",{-14,0}},{"+ 2N ",Null},{"- 14N ",Null},{"3.1N",Null},{"3 n",Null},{"  N",{1,0}},{" +n",{1,0}},{" -n",{-1,0}},{"+ n",Null},{"- n",Null},{"3N+1",{3,1}},{"+2n+1 ",{2,1}},{" -14n+1 ",{-14,1}},{"+ 2N+1 ",Null},{"- 14n+1 ",Null},{"3.1n+1",Null},{"3 n+1",Null},{"  n+1",{1,1}},{" +N+1",{1,1}},{" -n+1",{-1,1}},{"+ N+1",Null},{"- N+1",Null},{"3n-1",{3,-1}},{"+2N-1 ",{2,-1}},{" -14n-1 ",{-14,-1}},{"+ 2N-1 ",Null},{"- 14N-1 ",Null},{"3.1n-1",Null},{"3 n-1",Null},{"3n-1foo",Null},{"  n-1",{1,-1}},{" +n-1",{1,-1}},{" -n-1",{-1,-1}},{"+ n-1",Null},{"- n-1",Null},{" +n-1foo",Null},{" -n-1foo",Null},{"3N +1",{3,1}},{"+2N +1 ",{2,1}},{" -14n +1 ",{-14,1}},{"+ 2N +1 ",Null},{"- 14n +1 ",Null},{"3.1N +1",Null},{"3 n +1",Null},{"3n foo",Null},{"3n + foo",Null},{"  n +1",{1,1}},{" +N +1",{1,1}},{" -n +1",{-1,1}},{"+ n +1",Null},{"- N +1",Null},{"3N -1",{3,-1}},{"+2n -1 ",{2,-1}},{" -14n -1 ",{-14,-1}},{"+ 2n -1 ",Null},{"- 14N -1 ",Null},{"3.1N -1",Null},{"3 N -1",Null},{"  N -1",{1,-1}},{" +N -1",{1,-1}},{" -n -1",{-1,-1}},{"+ n -1",Null},{"- n -1",Null},{"3n+ 1",{3,1}},{"+2n+ 1 ",{2,1}},{" -14n+ 1 ",{-14,1}},{"+ 2n+ 1 ",Null},{"- 14N+ 1 ",Null},{"3.1n+ 1",Null},{"3 N+ 1",Null},{"  N+ 1",{1,1}},{" +N+ 1",{1,1}},{" -N+ 1",{-1,1}},{"+ n+ 1",Null},{"- N+ 1",Null},{"3n- 1",{3,-1}},{"+2N- 1 ",{2,-1}},{" -14N- 1 ",{-14,-1}},{"+ 2N- 1 ",Null},{"- 14n- 1 ",Null},{"3.1n- 1",Null},{"3 n- 1",Null},{"  N- 1",{1,-1}},{" +N- 1",{1,-1}},{" -n- 1",{-1,-1}},{"+ n- 1",Null},{"- N- 1",Null},{"3N + 1",{3,1}},{"+2N + 1 ",{2,1}},{" -14n + 1 ",{-14,1}},{"+ 2n + 1 ",Null},{"- 14N + 1 ",Null},{"3.1n + 1",Null},{"3 N + 1",Null},{"  n + 1",{1,1}},{" +n + 1",{1,1}},{" -N + 1",{-1,1}},{"+ N + 1",Null},{"- N + 1",Null},{"3N - 1",{3,-1}},{"+2n - 1 ",{2,-1}},{" -14n - 1 ",{-14,-1}},{"+ 2N - 1 ",Null},{"- 14N - 1 ",Null},{"3.1N - 1",Null},{"3 n - 1",Null},{"  N - 1",{1,-1}},{" +n - 1",{1,-1}},{" -n - 1",{-1,-1}},{"+ N - 1",Null},{"- N - 1",Null}};*)


(* ::Input:: *)
(*MapThread[*)
(*	(CellPrint[*)
(*		ExpressionCell[*)
(*			If[Head[#2] === Failure, Defer[TestMatch[#1, _Failure]], Defer[Test[#1, #2]]],*)
(*			"Code",*)
(*			CellAutoOverwrite -> False,*)
(*			GeneratedCell -> False]*)
(*])&, *)
(*{*)
(*	Table[With[{t = tests[[i, 1]]}, Defer[CSSTools`CSSSelectors3`Private`parseANB[t]]], {i, Length[tests]}], *)
(*	CSSTools`CSSSelectors3`Private`parseANB /@ tests[[All, 1]]}];*)


(* ::Subsection:: *)
(*Full tests*)


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N - 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ N - 1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n - 1"],{-1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +n - 1"],{1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  N - 1"],{1,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n - 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1N - 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N - 1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N - 1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n - 1 "],{-14,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2n - 1 "],{2,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N - 1"],{3,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N + 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ N + 1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -N + 1"],{-1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +n + 1"],{1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  n + 1"],{1,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 N + 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1n + 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N + 1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2n + 1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n + 1 "],{-14,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2N + 1 "],{2,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N + 1"],{3,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N- 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n- 1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n- 1"],{-1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +N- 1"],{1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  N- 1"],{1,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n- 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1n- 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14n- 1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N- 1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14N- 1 "],{-14,-1}] 


Test[CSSTools`CSSSelectors3`Private`parseANB["+2N- 1 "],{2,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3n- 1"],{3,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N+ 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n+ 1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -N+ 1"],{-1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +N+ 1"],{1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  N+ 1"],{1,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 N+ 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1n+ 1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N+ 1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2n+ 1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n+ 1 "],{-14,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2n+ 1 "],{2,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3n+ 1"],{3,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- n -1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n -1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n -1"],{-1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +N -1"],{1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  N -1"],{1,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 N -1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1N -1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N -1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2n -1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n -1 "],{-14,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2n -1 "],{2,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N -1"],{3,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N +1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n +1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n +1"],{-1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +N +1"],{1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  n +1"],{1,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3n + foo"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3n foo"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n +1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1N +1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14n +1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N +1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n +1 "],{-14,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2N +1 "],{2,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N +1"],{3,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB[" -n-1foo"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB[" +n-1foo"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- n-1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n-1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n-1"],{-1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +n-1"],{1,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  n-1"],{1,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3n-1foo"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n-1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1n-1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N-1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N-1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n-1 "],{-14,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2N-1 "],{2,-1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3n-1"],{3,-1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- N+1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ N+1"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n+1"],{-1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +N+1"],{1,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  n+1"],{1,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n+1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1n+1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14n+1 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N+1 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n+1 "],{-14,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2n+1 "],{2,1}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N+1"],{3,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- n"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ n"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -n"],{-1,0}]


Test[CSSTools`CSSSelectors3`Private`parseANB[" +n"],{1,0}]


Test[CSSTools`CSSSelectors3`Private`parseANB["  N"],{1,0}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3 n"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1N"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14N "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2N "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14n "],{-14,0}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2N "],{2,0}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3N"],{3,0}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["3.1"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["- 14 "],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["+ 2 "],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB[" -14 "],{0,-14}]


Test[CSSTools`CSSSelectors3`Private`parseANB["+2 "],{0,2}]


Test[CSSTools`CSSSelectors3`Private`parseANB["3"],{0,3}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB[" /**/\t EveN /**/\n"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB[" /**/\t OdD /**/\n"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["\[EAcute]ven"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["\[ODoubleDot]dd"],_Failure]


Test[CSSTools`CSSSelectors3`Private`parseANB["even"],{2,0}]


Test[CSSTools`CSSSelectors3`Private`parseANB["odd"],{2,1}]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB["  \n"],_Failure]


TestMatch[CSSTools`CSSSelectors3`Private`parseANB[""],_Failure]
