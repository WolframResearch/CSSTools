(* ::Package:: *)

(* Wolfram Language package *)

Get["MUnit`"];
SetDirectory[NotebookDirectory[]];


Get["CSSTools`CSSTools`"];
Get["CSSTools`CSSTokenizer`"];


res = TestReport["CalcTests.wl"];
res["TestsFailed"]


res = TestReport["ANB.wl"];
res["TestsFailed"]


res = TestReport["GeneralColors.wl"];
res["TestsFailed"]


res = TestReport["HSLColors.wl"];
res["TestsFailed"]


res = TestReport["NamedColors.wl"];
res["TestsFailed"]


res = TestReport["RegularExpressions.wl"];
res["TestsFailed"]


res = TestReport["Tokenizer.wl"];
res["TestsFailed"]
