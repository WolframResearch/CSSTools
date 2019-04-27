(* Wolfram Language Package *)

BeginPackage["CSS21Parser`"];
Begin["`Private`"]; (* Begin Private Context *) 

(* we assume that the colors have already been tokenized *)


(* keyword *)
parseSingleColorKeyWord[prop_String, keyword_String] := 
	Switch[normalizeKeyWord @ keyword,
		"initial",        initialValues @ prop,
		"inherit",        Inherited,
		"currentcolor",   Dynamic @ CurrentValue[FontColor], 
		"transparent",    None, (* this 'ident' is interpreted as GrayLevel[0, 0] by Interpreter["Color"] *)
		"aliceblue",      RGBColor[Rational[16, 17], Rational[248, 255], 1], 
		"antiquewhite",   RGBColor[Rational[50, 51], Rational[47, 51], Rational[43, 51]], 
		"aqua",           RGBColor[0, 1, 1], 
		"aquamarine",     RGBColor[Rational[127, 255], 1, Rational[212, 255]], 
		"azure",          RGBColor[Rational[16, 17], 1, 1], 
		"beige",          RGBColor[Rational[49, 51], Rational[49, 51], Rational[44, 51]], 
		"bisque",         RGBColor[1, Rational[76, 85], Rational[196, 255]], 
		"black",          RGBColor[0, 0, 0], 
		"blanchedalmond", RGBColor[1, Rational[47, 51], Rational[41, 51]], 
		"blue",           RGBColor[0, 0, 1], 
		"blueviolet",     RGBColor[Rational[46, 85], Rational[43, 255], Rational[226, 255]], 
		"brown",          RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]], 
		"brown",          RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]],
		"burlywood",      RGBColor[Rational[74, 85], Rational[184, 255], Rational[9, 17]],
		"cadetblue",      RGBColor[Rational[19, 51], Rational[158, 255], Rational[32, 51]],
		"chartreuse",     RGBColor[Rational[127, 255], 1, 0],
		"chocolate",      RGBColor[Rational[14, 17], Rational[7, 17], Rational[2, 17]],
		"coral",          RGBColor[1, Rational[127, 255], Rational[16, 51]],
		"cornflowerblue", RGBColor[Rational[20, 51], Rational[149, 255], Rational[79, 85]],
		"cornsilk",       RGBColor[1, Rational[248, 255], Rational[44, 51]],
		"crimson",        RGBColor[Rational[44, 51], Rational[4, 51], Rational[4, 17]],
		"cyan",           RGBColor[0, 1, 1],
		"darkblue",       RGBColor[0, 0, Rational[139, 255]],
		"darkcyan",       RGBColor[0, Rational[139, 255], Rational[139, 255]],
		"darkgoldenrod",  RGBColor[Rational[184, 255], Rational[134, 255], Rational[11, 255]],
		"darkgray",       RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]],
		"darkgreen",      RGBColor[0, Rational[20, 51], 0],
		"darkgrey",       RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]],
		"darkkhaki",      RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]],
		"darkmagenta",    RGBColor[Rational[139, 255], 0, Rational[139, 255]],
		"darkolivegreen", RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]],
		"darkorange",     RGBColor[1, Rational[28, 51], 0],
		"darkorchid",     RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]],
		"darkred",        RGBColor[Rational[139, 255], 0, 0],
		"darksalmon",     RGBColor[Rational[233, 255], Rational[10, 17], Rational[122, 255]],
		"darkseagreen",   RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]],
		"darkslateblue",  RGBColor[Rational[24, 85], Rational[61, 255], Rational[139, 255]],
		"darkslategray",  RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]],
		"darkslategrey",  RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]],
		"darkturquoise",  RGBColor[0, Rational[206, 255], Rational[209, 255]],
		"darkviolet",     RGBColor[Rational[148, 255], 0, Rational[211, 255]],
		"deeppink",       RGBColor[1, Rational[4, 51], Rational[49, 85]],
		"deepskyblue",    RGBColor[0, Rational[191, 255], 1],
		"dimgray",        RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]],
		"dimgrey",        RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]],
		"dodgerblue",     RGBColor[Rational[2, 17], Rational[48, 85], 1],
		"firebrick",      RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]],
		"floralwhite",    RGBColor[1, Rational[50, 51], Rational[16, 17]],
		"forestgreen",    RGBColor[Rational[2, 15], Rational[139, 255], Rational[2, 15]],
		"fuchsia",        RGBColor[1, 0, 1],
		"gainsboro",      RGBColor[Rational[44, 51], Rational[44, 51], Rational[44, 51]],
		"ghostwhite",     RGBColor[Rational[248, 255], Rational[248, 255], 1],
		"gold",           RGBColor[1, Rational[43, 51], 0],
		"goldenrod",      RGBColor[Rational[218, 255], Rational[11, 17], Rational[32, 255]],
		"gray",           RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]],
		"green",          RGBColor[0, Rational[128, 255], 0],
		"greenyellow",    RGBColor[Rational[173, 255], 1, Rational[47, 255]],
		"grey",           RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]],
		"honeydew",       RGBColor[Rational[16, 17], 1, Rational[16, 17]],
		"hotpink",        RGBColor[1, Rational[7, 17], Rational[12, 17]],
		"indianred",      RGBColor[Rational[41, 51], Rational[92, 255], Rational[92, 255]],
		"indigo",         RGBColor[Rational[5, 17], 0, Rational[26, 51]],
		"ivory",          RGBColor[1, 1, Rational[16, 17]],
		"khaki",          RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]],
		"lavender",       RGBColor[Rational[46, 51], Rational[46, 51], Rational[50, 51]],
		"lavenderblush",  RGBColor[1, Rational[16, 17], Rational[49, 51]],
		"lawngreen",      RGBColor[Rational[124, 255], Rational[84, 85], 0],
		"lemonchiffon",   RGBColor[1, Rational[50, 51], Rational[41, 51]],
		"lightblue",      RGBColor[Rational[173, 255], Rational[72, 85], Rational[46, 51]],
		"lightcoral",     RGBColor[Rational[16, 17], Rational[128, 255], Rational[128, 255]],
		"lightcyan",      RGBColor[Rational[224, 255], 1, 1],
		"lightgoldenrodyellow", RGBColor[Rational[50, 51], Rational[50, 51], Rational[14, 17]],
		"lightgray",      RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]],
		"lightgreen",     RGBColor[Rational[48, 85], Rational[14, 15], Rational[48, 85]],
		"lightgrey",      RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]],
		"lightpink",      RGBColor[1, Rational[182, 255], Rational[193, 255]],
		"lightsalmon",    RGBColor[1, Rational[32, 51], Rational[122, 255]],
		"lightseagreen",  RGBColor[Rational[32, 255], Rational[178, 255], Rational[2, 3]],
		"lightskyblue",   RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]],
		"lightslategray", RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]],
		"lightslategrey", RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]],
		"lightsteelblue", RGBColor[Rational[176, 255], Rational[196, 255], Rational[74, 85]],
		"lightyellow",    RGBColor[1, 1, Rational[224, 255]],
		"lime",           RGBColor[0, 1, 0],
		"limegreen",      RGBColor[Rational[10, 51], Rational[41, 51], Rational[10, 51]],
		"linen",          RGBColor[Rational[50, 51], Rational[16, 17], Rational[46, 51]],
		"magenta",        RGBColor[1, 0, 1],
		"maroon",         RGBColor[Rational[128, 255], 0, 0],
		"mediumaquamarine", RGBColor[Rational[2, 5], Rational[41, 51], Rational[2, 3]],
		"mediumblue",     RGBColor[0, 0, Rational[41, 51]],
		"mediumorchid",   RGBColor[Rational[62, 85], Rational[1, 3], Rational[211, 255]],
		"mediumpurple",   RGBColor[Rational[49, 85], Rational[112, 255], Rational[73, 85]],
		"mediumseagreen", RGBColor[Rational[4, 17], Rational[179, 255], Rational[113, 255]],
		"mediumslateblue", RGBColor[Rational[41, 85], Rational[104, 255], Rational[14, 15]],
		"mediumspringgreen", RGBColor[0, Rational[50, 51], Rational[154, 255]],
		"mediumturquoise", RGBColor[Rational[24, 85], Rational[209, 255], Rational[4, 5]],
		"mediumvioletred", RGBColor[Rational[199, 255], Rational[7, 85], Rational[133, 255]],
		"midnightblue",   RGBColor[Rational[5, 51], Rational[5, 51], Rational[112, 255]],
		"mintcream",      RGBColor[Rational[49, 51], 1, Rational[50, 51]],
		"mistyrose",      RGBColor[1, Rational[76, 85], Rational[15, 17]],
		"moccasin",       RGBColor[1, Rational[76, 85], Rational[181, 255]],
		"navajowhite",    RGBColor[1, Rational[74, 85], Rational[173, 255]],
		"navy",           RGBColor[0, 0, Rational[128, 255]],
		"oldlace",        RGBColor[Rational[253, 255], Rational[49, 51], Rational[46, 51]],
		"olive",          RGBColor[Rational[128, 255], Rational[128, 255], 0],
		"olivedrab",      RGBColor[Rational[107, 255], Rational[142, 255], Rational[7, 51]],
		"orange",         RGBColor[1, Rational[11, 17], 0],
		"orangered",      RGBColor[1, Rational[23, 85], 0],
		"orchid",         RGBColor[Rational[218, 255], Rational[112, 255], Rational[214, 255]],
		"palegoldenrod",  RGBColor[Rational[14, 15], Rational[232, 255], Rational[2, 3]],
		"palegreen",      RGBColor[Rational[152, 255], Rational[251, 255], Rational[152, 255]],
		"paleturquoise",  RGBColor[Rational[35, 51], Rational[14, 15], Rational[14, 15]],
		"palevioletred",  RGBColor[Rational[73, 85], Rational[112, 255], Rational[49, 85]],
		"papayawhip",     RGBColor[1, Rational[239, 255], Rational[71, 85]],
		"peachpuff",      RGBColor[1, Rational[218, 255], Rational[37, 51]],
		"peru",           RGBColor[Rational[41, 51], Rational[133, 255], Rational[21, 85]],
		"pink",           RGBColor[1, Rational[64, 85], Rational[203, 255]],
		"plum",           RGBColor[Rational[13, 15], Rational[32, 51], Rational[13, 15]],
		"powderblue",     RGBColor[Rational[176, 255], Rational[224, 255], Rational[46, 51]],
		"purple",         RGBColor[Rational[128, 255], 0, Rational[128, 255]],
		"rebeccapurple",  RGBColor[Rational[2, 5], Rational[1, 5], Rational[3, 5]],
		"red",            RGBColor[1, 0, 0],
		"rosybrown",      RGBColor[Rational[188, 255], Rational[143, 255], Rational[143, 255]],
		"royalblue",      RGBColor[Rational[13, 51], Rational[7, 17], Rational[15, 17]],
		"saddlebrown",    RGBColor[Rational[139, 255], Rational[23, 85], Rational[19, 255]],
		"salmon",         RGBColor[Rational[50, 51], Rational[128, 255], Rational[38, 85]],
		"sandybrown",     RGBColor[Rational[244, 255], Rational[164, 255], Rational[32, 85]],
		"seagreen",       RGBColor[Rational[46, 255], Rational[139, 255], Rational[29, 85]],
		"seashell",       RGBColor[1, Rational[49, 51], Rational[14, 15]],
		"sienna",         RGBColor[Rational[32, 51], Rational[82, 255], Rational[3, 17]],
		"silver",         RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]],
		"skyblue",        RGBColor[Rational[9, 17], Rational[206, 255], Rational[47, 51]],
		"slateblue",      RGBColor[Rational[106, 255], Rational[6, 17], Rational[41, 51]],
		"slategray",      RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]],
		"slategrey",      RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]],
		"snow",           RGBColor[1, Rational[50, 51], Rational[50, 51]],
		"springgreen",    RGBColor[0, 1, Rational[127, 255]],
		"steelblue",      RGBColor[Rational[14, 51], Rational[26, 51], Rational[12, 17]],
		"tan",            RGBColor[Rational[14, 17], Rational[12, 17], Rational[28, 51]],
		"teal",           RGBColor[0, Rational[128, 255], Rational[128, 255]],
		"thistle",        RGBColor[Rational[72, 85], Rational[191, 255], Rational[72, 85]],
		"tomato",         RGBColor[1, Rational[33, 85], Rational[71, 255]],
		"turquoise",      RGBColor[Rational[64, 255], Rational[224, 255], Rational[208, 255]],
		"violet",         RGBColor[Rational[14, 15], Rational[26, 51], Rational[14, 15]],
		"wheat",          RGBColor[Rational[49, 51], Rational[74, 85], Rational[179, 255]],
		"white",          RGBColor[1, 1, 1],
		"whitesmoke",     RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]],
		"yellow",         RGBColor[1, 1, 0],
		"yellowgreen",    RGBColor[Rational[154, 255], Rational[41, 51], Rational[10, 51]],
		_,                unrecognizedValueFailure @ prop
	]


(* Hex *)
$1XC = Repeated[RegularExpression[RE["h"]], {1}];
$2XC = Repeated[RegularExpression[RE["h"]], {2}];
fromhexdigits[s_] := FromDigits[s, 16]
(* 3 digits *) hexPattern3 := StartOfString ~~ "#" ~~ r:$1XC ~~ g:$1XC ~~ b:$1XC           ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b   } / 15);
(* 4 digits *) hexPattern4 := StartOfString ~~ "#" ~~ r:$1XC ~~ g:$1XC ~~ b:$1XC ~~ a:$1XC ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b, a} / 15);
(* 6 digits *) hexPattern6 := StartOfString ~~ "#" ~~ r:$2XC ~~ g:$2XC ~~ b:$2XC           ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b   } / 255);
(* 8 digits *) hexPattern8 := StartOfString ~~ "#" ~~ r:$2XC ~~ g:$2XC ~~ b:$2XC ~~ a:$2XC ~~ EndOfString :> RGBColor @@ (fromhexdigits /@ {r, g, b, a} / 255);

parseSingleColorHex[prop_String, hexString_String] :=
	Which[
		StringMatchQ[hexString, First @ hexPattern3], First[StringCases[hexString, hexPattern3], unrecognizedValueFailure @ prop],
		StringMatchQ[hexString, First @ hexPattern4], First[StringCases[hexString, hexPattern4], unrecognizedValueFailure @ prop],
		StringMatchQ[hexString, First @ hexPattern6], First[StringCases[hexString, hexPattern6], unrecognizedValueFailure @ prop],
		StringMatchQ[hexString, First @ hexPattern8], First[StringCases[hexString, hexPattern8], unrecognizedValueFailure @ prop],
		True, Failure["UnexpectedParse", <|"Message" -> "Unrecognized hex color " <> hexString <> "."|>]]


(* RGB *)
rgbaPattern := 
	{
		{v1:"number"|"percentage", _String, r_, _String}, d:Repeated[",", {0, 1}], 
		{v1:"number"|"percentage", _String, g_, _String}, d:Repeated[",", {0, 1}], 
		{v1:"number"|"percentage", _String, b_, _String}, d:Repeated[",", {0, 1}], 
		{v2:"number"|"percentage", _String, a_, _String}
	} :> Apply[RGBColor, Append[{r, g, b}/If[v1 == "number", 255, 100.], If[v2 == "number", Clip[a, {0, 1}], a/100.]]]
rgbPattern := 
	{
		{v1:"number"|"percentage", _String, r_, _String}, d:Repeated[",", {0, 1}], 
		{v1:"number"|"percentage", _String, g_, _String}, d:Repeated[",", {0, 1}], 
		{v1:"number"|"percentage", _String, b_, _String}
	} :> Apply[RGBColor, {r, g, b}/If[v1 == "number", 255, 100.]]


(* HSL *)
HSLtoHSB[h_, s_, l_, a___] := With[{b = (2 * l + s * (1 - Abs[2 * l - 1])) / 2}, If[b == 0, Hue[h, 0, 0, a], Hue[h, 2 * (b - l) / b, b, a]]]

hslaPattern := 
	{
		Alternatives[
			hAll:{type:"dimension", _String, h_, _String, "deg"|"grad"|"rad"|"turn"},
			hAll:{type:"number",    _String, h_, _String}], 
		d:Repeated[",", {0, 1}], {"percentage", _String, s_, _String}, 
		d:Repeated[",", {0, 1}], {"percentage", _String, l_, _String}, 
		d:Repeated[",", {0, 1}], {"number",     _String, a_, _String}
	} :> HSLtoHSB[If[type == "number", h, parseAngle[hAll]]/360, s/100, l/100, Clip[a, {0, 1}]]
hslPattern := 
	{
		Alternatives[
			hAll:{type:"dimension", _String, h_, _String, "deg"|"grad"|"rad"|"turn"},
			hAll:{type:"number",    _String, h_, _String}], 
		d:Repeated[",", {0, 1}], {"percentage", _String, s_, _String}, 
		d:Repeated[",", {0, 1}], {"percentage", _String, l_, _String}
	} :> HSLtoHSB[If[type == "number", h, parseAngle[hAll]]/360, s/100, l/100]


parseSingleColorFunction[prop_String, tokens:{__?validTokenQ}] :=
	Module[{relevantTokens},
		(* relevantTokens drops the function head and closing paren and removes all whitespace tokens *)
		relevantTokens = DeleteCases[tokens, {"whitespace", _}, {1}][[2 ;; -2]];
		Which[
			(* the "a" of rgb function heads is optional *)
			StringMatchQ[tokenString @ tokens[[1]], RegularExpression[RE["R"] ~~ RE["G"] ~~ RE["B"] ~~ "(" ~~ RE["A"] ~~ "?)" ~~ "\\("]],
				Which[
					MatchQ[relevantTokens, First @ rgbaPattern], Replace[relevantTokens, rgbaPattern], 
					MatchQ[relevantTokens, First @ rgbPattern],  Replace[relevantTokens, rgbPattern], 
					True, unrecognizedValueFailure @ prop 
				],
			(* the "a" of hsl function heads is optional *)
			StringMatchQ[tokenString @ tokens[[1]], RegularExpression[RE["H"] ~~ RE["S"] ~~ RE["L"] ~~ "(" ~~ RE["A"] ~~ "?)" ~~ "\\("]],
				Which[
					MatchQ[relevantTokens, First @ hslaPattern], Replace[relevantTokens, hslaPattern], 
					MatchQ[relevantTokens, First @ hslPattern],  Replace[relevantTokens, hslPattern], 
					True, unrecognizedValueFailure @ prop 
				],
			True, Failure["UnexpectedParse", <|"Message" -> "Unrecognized color function" <> tokens[[1, 2]] <> "."|>]
		]
	]


(* parse all color types *)
parseSingleColor[prop_String, tokens:{__?validTokenQ}] := parseSingleColor[prop, tokens] = 
	Which[
		Length[tokens] == 1 && MatchQ[tokenType @ tokens[[1]], "ident"],    parseSingleColorKeyWord[prop, tokenString @ tokens[[1]]],
		Length[tokens] == 1 && MatchQ[tokenType @ tokens[[1]], "hexcolor"], parseSingleColorHex[prop, tokenString @ tokens[[1]]],
		Length[tokens] > 1  && MatchQ[tokenType @ tokens[[1]], "function"], parseSingleColorFunction[prop, tokens],
		True, unrecognizedValueFailure @ prop
	]



End[]; (* End Private Context *)
EndPackage[];
