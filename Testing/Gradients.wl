(* ::Package:: *)

(* ::Section:: *)
(*Named Colors Tests*)

(* basic top-to-bottom linear gradient tests *)
res = {
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(yellow, blue)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(to bottom, yellow, blue)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(180deg, yellow, blue)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(to top, blue, yellow)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(to bottom, yellow 0%, blue 100%)"]]};

TestMatch[AnyTrue[res, _?FailureQ], False]

TestRequirement[!AnyTrue[res, _?FailureQ]];

(* all expressions in 'res' should produce the same gradient within tolerance *)
res = Through @ res[{200, 100}];
res = Table[Union[Clip[Round[Flatten[ImageData[#]], 0.01]]]& @ ImageDifference[res[[i]], res[[j]]], {i, 1, Length[res]}, {j, i + 1, Length[res]}];

TestMatch[Equal[Clip[Union[Flatten[res, {0.}]]]], True]

EndRequirement[];



(* basic angled gradient tests *)
res = {
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(135deg, yellow, blue)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(-45deg, blue, yellow)"]]}

TestMatch[AnyTrue[res, _?FailureQ], False]

TestRequirement[!AnyTrue[res, _?FailureQ]];

(* all expressions in 'res' should produce the same gradient within tolerance *)
res = Through @ res[{200, 100}];
res = Table[Union[Clip[Round[Flatten[ImageData[#]], 0.01]]]& @ ImageDifference[res[[i]], res[[j]]], {i, 1, Length[res]}, {j, i + 1, Length[res]}];

TestMatch[Equal[Clip[Union[Flatten[res, {0.}]]]], True]

EndRequirement[];



(* basic angled gradient tests *)
res = {
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(135deg, yellow, blue)"]],
	Catch @ CSSTools`CSSImages3`Private`parseLinearGradientFunction["test", First @ CSSTokenize["linear-gradient(-45deg, blue, yellow)"]]}

TestMatch[AnyTrue[res, _?FailureQ], False]

TestRequirement[!AnyTrue[res, _?FailureQ]];

(* all expressions in 'res' should produce the same gradient within tolerance *)
res = Through @ res[{200, 100}];
res = Table[Union[Clip[Round[Flatten[ImageData[#]], 0.01]]]& @ ImageDifference[res[[i]], res[[j]]], {i, 1, Length[res]}, {j, i + 1, Length[res]}];

TestMatch[Equal[Clip[Union[Flatten[res, {0.}]]]], True]

EndRequirement[];


