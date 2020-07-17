(* ::Package:: *)

(* ::Section:: *)
(*Regex Tests*)


Get["CSSTools`CSSTokenizer`"] (* defines RE *)


Test[And@@(StringMatchQ[#, RegularExpression[RE["hex digit"]]]& /@ ToString /@ Join[Range[0,9], CharacterRange["a","f"], CharacterRange["A","F"]]), True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["A"]]& /@ Flatten@{"a","A",Outer[StringJoin, {"\\"},{"","0","00","000","0000"},{"41","61"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["C"]]& /@ Flatten@{"c","C",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"43","63"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["D"]]& /@ Flatten@{"d","D",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"44","64"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["E"]]& /@ Flatten@{"e","E",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"45","65"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["G"]]& /@ Flatten@{"g","G","\\g","\\G",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"47","67"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["H"]]& /@ Flatten@{"h","H","\\h","\\H",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"48","68"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["I"]]& /@ Flatten@{"i","I","\\i","\\I",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"49","69"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["K"]]& /@ Flatten@{"k","K","\\k","\\K",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"4b","6b"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["L"]]& /@ Flatten@{"l","L","\\l","\\L",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"4c","6c"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["M"]]& /@ Flatten@{"m","M","\\m","\\M",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"4d","6d"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["N"]]& /@ Flatten@{"n","N","\\n","\\N",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"4e","6e"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["O"]]& /@ Flatten@{"o","O","\\o","\\O",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"4f","6f"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["P"]]& /@ Flatten@{"p","P","\\p","\\P",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"50","70"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["R"]]& /@ Flatten@{"r","R","\\r","\\R",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"52","72"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["S"]]& /@ Flatten@{"s","S","\\s","\\S",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"53","73"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["T"]]& /@ Flatten@{"t","T","\\t","\\T",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"44","74"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["U"]]& /@ Flatten@{"u","U","\\u","\\U",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"55","75"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["X"]]& /@ Flatten@{"x","X","\\x","\\X",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"58","78"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]


Test[Apply[
	And,
	StringMatchQ[#, RegularExpression @ RE["Z"]]& /@ Flatten@{"z","Z","\\z","\\Z",Outer[StringJoin, {"\\"},{"0","00","000","0000"},{"5a","7a"},{"","\r\n"," ","\t","\r","\n","\f"}]}], True]
