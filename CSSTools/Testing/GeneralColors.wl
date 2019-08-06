(* ::Package:: *)

(* ::Section:: *)
(*General Color Tests*)


(* ::Subsection::Closed:: *)
(*Generate Tests*)


(* ::Input:: *)
(*(* Original test data. Null results are expected failures. *)*)
(*generalColorTests={{"",Null},{" /* hey */\n",Null},{"4",Null},{"top",Null},{"/**/transparent",None},{"transparent",None},{" transparent\n",None},{"TransParent",None},{"currentColor","currentColor"},{"CURRENTcolor","currentColor"},{"current-Color",Null},{"black",{0,0,0}},{"white",{1,1,1}},{"fuchsia",{1,0,1}},{"cyan",{0,1,1}},{"CyAn",{0,1,1}},{"#",Null},{"#f",Null},{"#ff",Null},{"#fff",{1,1,1}},{"#ffg",Null},{"#ffff",{1,1,1,1}},{"#fffg",Null},{"#fffff",Null},{"#ffffff",{1,1,1}},{"#fffffg",Null},{"#fffffff",Null},{"#ffffffff",{1,1,1,1}},{"#fffffffg",Null},{"#fffffffff",Null},{"#FFCc99",{1,0.8`,0.6`}},{"#369",{0.2`,0.4`,0.6`}},{"rgb(00, 51, 102)",{0,0.2`,0.4`}},{"r\\gb(00, 51, 102)",{0,0.2`,0.4`}},{"r\\67 b(00, 51, 102)",{0,0.2`,0.4`}},{"RGB(153, 204, 255)",{0.6`,0.8`,1}},{"rgB(0, 0, 0)",{0,0,0}},{"rgB(0, 51, 255)",{0,0.2`,1}},{"rgb(0,51,255)",{0,0.2`,1}},{"rgb(0\t,  51 ,255)",{0,0.2`,1}},{"rgb(/* R */0, /* G */51, /* B */255)",{0,0.2`,1}},{"rgb(-51, 306, 0)",{-0.2`,1.2`,0}},{"rgb(42%, 3%, 50%)",{0.42`,0.03`,0.5`}},{"RGB(100%, 100%, 100%)",{1,1,1}},{"rgB(0%, 0%, 0%)",{0,0,0}},{"rgB(10%, 20%, 30%)",{0.1`,0.2`,0.3`}},{"rgb(10%,20%,30%)",{0.1`,0.2`,0.3`}},{"rgb(10%\t,  20% ,30%)",{0.1`,0.2`,0.3`}},{"rgb(/* R */ 10%, /* G */ 20%, /* B */ 30%)",{0.1`,0.2`,0.3`}},{"rgb(-12%, 110%, 1400%)",{-0.12`,1.1`,14}},{"rgb(10%, 50%, 0)",Null},{"rgb(255, 50%, 0%)",Null},{"rgb(0, 0 0)",Null},{"rgb(0, 0, 0deg)",Null},{"rgb(0, 0, light)",Null},{"rgb()",Null},{"rgb(0)",Null},{"rgb(0, 0)",Null},{"rgb(0, 0, 0, 0)",Null},{"rgb(0%)",Null},{"rgb(0%, 0%)",Null},{"rgb(0%, 0%, 0%, 0%)",Null},{"rgb(0%, 0%, 0%, 0)",Null},{"rgba(0, 0, 0, 0)",{0,0,0,0}},{"rgba(204, 0, 102, 0.3)",{0.8`,0,0.4`,0.3`}},{"RGBA(255, 255, 255, 0)",{1,1,1,0}},{"rgBA(0, 51, 255, 1)",{0,0.2`,1,1}},{"rgba(0, 51, 255, 1.1)",{0,0.2`,1,1}},{"rgba(0, 51, 255, 37)",{0,0.2`,1,1}},{"rgba(0, 51, 255, 0.42)",{0,0.2`,1,0.42`}},{"rgba(0, 51, 255, 0)",{0,0.2`,1,0}},{"rgba(0, 51, 255, -0.1)",{0,0.2`,1,0}},{"rgba(0, 51, 255, -139)",{0,0.2`,1,0}},{"rgba(42%, 3%, 50%, 0.3)",{0.42`,0.03`,0.5`,0.3`}},{"RGBA(100%, 100%, 100%, 0)",{1,1,1,0}},{"rgBA(0%, 20%, 100%, 1)",{0,0.2`,1,1}},{"rgba(0%, 20%, 100%, 1.1)",{0,0.2`,1,1}},{"rgba(0%, 20%, 100%, 37)",{0,0.2`,1,1}},{"rgba(0%, 20%, 100%, 0.42)",{0,0.2`,1,0.42`}},{"rgba(0%, 20%, 100%, 0)",{0,0.2`,1,0}},{"rgba(0%, 20%, 100%, -0.1)",{0,0.2`,1,0}},{"rgba(0%, 20%, 100%, -139)",{0,0.2`,1,0}},{"rgba(255, 255, 255, 0%)",Null},{"rgba(10%, 50%, 0, 1)",Null},{"rgba(255, 50%, 0%, 1)",Null},{"rgba(0, 0, 0 0)",Null},{"rgba(0, 0, 0, 0deg)",Null},{"rgba(0, 0, 0, light)",Null},{"rgba()",Null},{"rgba(0)",Null},{"rgba(0, 0, 0)",Null},{"rgba(0, 0, 0, 0, 0)",Null},{"rgba(0%)",Null},{"rgba(0%, 0%)",Null},{"rgba(0%, 0%, 0%)",Null},{"rgba(0%, 0%, 0%, 0%)",Null},{"rgba(0%, 0%, 0%, 0%, 0%)",Null},{"HSL(0, 0%, 0%)",{0,0,0}},{"hsL(0, 100%, 50%)",{1,0,0}},{"hsl(60, 100%, 37.5%)",{0.75`,0.75`,0}},{"hsl(780, 100%, 37.5%)",{0.75`,0.75`,0}},{"hsl(-300, 100%, 37.5%)",{0.75`,0.75`,0}},{"hsl(300, 50%, 50%)",{0.75`,0.25`,0.75`}},{"hsl(10, 50%, 0)",Null},{"hsl(50%, 50%, 0%)",Null},{"hsl(0, 0% 0%)",Null},{"hsl(30deg, 100%, 100%)",Null},{"hsl(0, 0%, light)",Null},{"hsl()",Null},{"hsl(0)",Null},{"hsl(0, 0%)",Null},{"hsl(0, 0%, 0%, 0%)",Null},{"HSLA(-300, 100%, 37.5%, 1)",{0.75`,0.75`,0,1}},{"hsLA(-300, 100%, 37.5%, 12)",{0.75`,0.75`,0,1}},{"hsla(-300, 100%, 37.5%, 0.2)",{0.75`,0.75`,0,0.2`}},{"hsla(-300, 100%, 37.5%, 0)",{0.75`,0.75`,0,0}},{"hsla(-300, 100%, 37.5%, -3)",{0.75`,0.75`,0,0}},{"hsla(10, 50%, 0, 1)",Null},{"hsla(50%, 50%, 0%, 1)",Null},{"hsla(0, 0% 0%, 1)",Null},{"hsla(30deg, 100%, 100%, 1)",Null},{"hsla(0, 0%, light, 1)",Null},{"hsla()",Null},{"hsla(0)",Null},{"hsla(0, 0%)",Null},{"hsla(0, 0%, 0%, 50%)",Null},{"hsla(0, 0%, 0%, 1, 0%)",Null},{"cmyk(0, 0, 0, 0)",Null}};*)


(* ::Input:: *)
(*MapThread[*)
(*	(CellPrint[*)
(*		ExpressionCell[*)
(*			If[Head[#2] === Failure, Defer[TestMatch[#1, _Failure]], Defer[NTest[#1, #2]]],*)
(*			"Code",*)
(*			CellAutoOverwrite -> False,*)
(*			GeneratedCell -> False]*)
(*])&, *)
(*{*)
(*	Table[*)
(*		With[{t = generalColorTests[[i, 1]]}, *)
(*			Defer[*)
(*				With[{val =CSSTools`CSSPropertyInterpreter`parseSingleColor["color", First[CSSTools`CSSTokenizer`CSSTokenize @ t, {}]]},*)
(*					If[FailureQ[val] || val === None  || Head[val] === Dynamic, val, List @@ ColorConvert[val, "RGB"]]]]], *)
(*		{i, Length[generalColorTests]}], *)
(*	Table[*)
(*		With[{t = generalColorTests[[i, 1]]}, *)
(*			With[{val = CSSTools`CSSPropertyInterpreter`parseSingleColor["color", First[CSSTools`CSSTokenizer`CSSTokenize @ t, {}]]},*)
(*				If[FailureQ[val] || val === None  || Head[val] === Dynamic, val, List @@ ColorConvert[val, "RGB"]]]],*)
(*		{i, Length[generalColorTests]}]}]*)


(* ::Subsection::Closed:: *)
(*Tests*)


Get["CSSTools`CSSTools`"];
Get["CSSTools`CSSTokenizer`"];


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["cmyk(0, 0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0, 0%, 0%, 1, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0, 0%, 0%, 50%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla()"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0, 0%, light, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(30deg, 100%, 100%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(0, 0% 0%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(50%, 50%, 0%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(10, 50%, 0, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(-300, 100%, 37.5%, -3)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(-300, 100%, 37.5%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsla(-300, 100%, 37.5%, 0.2)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`,0.2`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsLA(-300, 100%, 37.5%, 12)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["HSLA(-300, 100%, 37.5%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(0, 0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(0, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl()"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(0, 0%, light)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(30deg, 100%, 100%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(0, 0% 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(50%, 50%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(10, 50%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(300, 50%, 50%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.25`,0.75`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(-300, 100%, 37.5%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(780, 100%, 37.5%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.7499999999999993`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsl(60, 100%, 37.5%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.75`,0.75`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["hsL(0, 100%, 50%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["HSL(0, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 0%, 0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba()"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0, light)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0, 0deg)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(255, 50%, 0%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(10%, 50%, 0, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(255, 255, 255, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, -139)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, -0.1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, 0.42)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.42`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, 37)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0%, 20%, 100%, 1.1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgBA(0%, 20%, 100%, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["RGBA(100%, 100%, 100%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(42%, 3%, 50%, 0.3)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.42`,0.03`,0.5`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, -139)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, -0.1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, 0.42)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,0.42`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, 37)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 51, 255, 1.1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgBA(0, 51, 255, 1)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["RGBA(255, 255, 255, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(204, 0, 102, 0.3)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.8`,0.`,0.4`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgba(0, 0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0%, 0%, 0%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0%, 0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0, 0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb()"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0, 0, light)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0, 0, 0deg)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0, 0 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(255, 50%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(10%, 50%, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(-12%, 110%, 1400%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(/* R */ 10%, /* G */ 20%, /* B */ 30%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.1`,0.2`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(10%\t,  20% ,30%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.1`,0.2`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(10%,20%,30%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.1`,0.2`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgB(10%, 20%, 30%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.1`,0.2`,0.3`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgB(0%, 0%, 0%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["RGB(100%, 100%, 100%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(42%, 3%, 50%)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.42`,0.03`,0.5`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(-51, 306, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,1.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(/* R */0, /* G */51, /* B */255)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0\t,  51 ,255)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(0,51,255)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgB(0, 51, 255)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgB(0, 0, 0)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["RGB(153, 204, 255)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.6`,0.8`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["r\\67 b(00, 51, 102)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,0.4`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["r\\gb(00, 51, 102)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,0.4`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["rgb(00, 51, 102)"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.2`,0.4`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#369"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.2`,0.4`,0.6`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#FFCc99"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,0.8`,0.6`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffffffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffffffg"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#ffffffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffffg"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#ffffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fffg"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#ffff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#ffg"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#fff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#ff"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#f"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["#"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["CyAn"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["cyan"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["fuchsia"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,0.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["white"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{1.`,1.`,1.`}]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["black"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],{0.`,0.`,0.`}]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["current-Color"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["CURRENTcolor"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]], Dynamic[CurrentValue[FontColor],___]]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["currentColor"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]], Dynamic[CurrentValue[FontColor],___]]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["TransParent"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],None]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize[" transparent\n"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["transparent"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],None]


NTest[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["/**/transparent"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],None]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["top"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize["4"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize[" /* hey */\n"],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]


TestMatch[With[{val=CSSTools`CSSPropertyInterpreter`parseSingleColor["color",First[CSSTokenize[""],{}]]},If[FailureQ[val]||val===None||Head[val]===Dynamic,val,List@@ColorConvert[val,"RGB"]]],_Failure]
