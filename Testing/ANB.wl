(* ::Package:: *)

(* ::Section:: *)
(*ANB Sub-Grammar Tests*)


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
