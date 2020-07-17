(* ::Package:: *)

(* ::Section:: *)
(*Named Colors Tests*)


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellowgren"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellowgre\\65 n"]],RGBColor[Rational[154, 255], Rational[41, 51], Rational[10, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellowgreEn"]],RGBColor[Rational[154, 255], Rational[41, 51], Rational[10, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellowgreen"]],RGBColor[Rational[154, 255], Rational[41, 51], Rational[10, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ellow"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\79 ellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\yellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Yellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whitesmo\:212ae"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["witesmoke"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["w\\68 itesmoke"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["w\\hitesmoke"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["wHitesmoke"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whitesmoke"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[49, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hite"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\77 hite"]],RGBColor[1, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\white"]],RGBColor[1, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["White"]],RGBColor[1, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["white"]],RGBColor[1, 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whea"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whea\\74 "]],RGBColor[Rational[49, 51], Rational[74, 85], Rational[179, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whea\\t"]],RGBColor[Rational[49, 51], Rational[74, 85], Rational[179, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["wheaT"]],RGBColor[Rational[49, 51], Rational[74, 85], Rational[179, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["wheat"]],RGBColor[Rational[49, 51], Rational[74, 85], Rational[179, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["vilet"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["vi\\6F let"]],RGBColor[Rational[14, 15], Rational[26, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["vi\\olet"]],RGBColor[Rational[14, 15], Rational[26, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["viOlet"]],RGBColor[Rational[14, 15], Rational[26, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["violet"]],RGBColor[Rational[14, 15], Rational[26, 51], Rational[14, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["turqoise"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["turq\\75 oise"]],RGBColor[Rational[64, 255], Rational[224, 255], Rational[208, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["turq\\uoise"]],RGBColor[Rational[64, 255], Rational[224, 255], Rational[208, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["turqUoise"]],RGBColor[Rational[64, 255], Rational[224, 255], Rational[208, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["turquoise"]],RGBColor[Rational[64, 255], Rational[224, 255], Rational[208, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["omato"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\74 omato"]],RGBColor[1, Rational[33, 85], Rational[71, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\tomato"]],RGBColor[1, Rational[33, 85], Rational[71, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Tomato"]],RGBColor[1, Rational[33, 85], Rational[71, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tomato"]],RGBColor[1, Rational[33, 85], Rational[71, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tistle"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["t\\68 istle"]],RGBColor[Rational[72, 85], Rational[191, 255], Rational[72, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["t\\histle"]],RGBColor[Rational[72, 85], Rational[191, 255], Rational[72, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tHistle"]],RGBColor[Rational[72, 85], Rational[191, 255], Rational[72, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["thistle"]],RGBColor[Rational[72, 85], Rational[191, 255], Rational[72, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tel"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["te\\61 l"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["teAl"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["teal"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["an"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\74 an"]],RGBColor[Rational[14, 17], Rational[12, 17], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\tan"]],RGBColor[Rational[14, 17], Rational[12, 17], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Tan"]],RGBColor[Rational[14, 17], Rational[12, 17], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tan"]],RGBColor[Rational[14, 17], Rational[12, 17], Rational[28, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["steelblu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["steelblu\\65 "]],RGBColor[Rational[14, 51], Rational[26, 51], Rational[12, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["steelbluE"]],RGBColor[Rational[14, 51], Rational[26, 51], Rational[12, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["steelblue"]],RGBColor[Rational[14, 51], Rational[26, 51], Rational[12, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["springgren"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["springgr\\65 en"]],RGBColor[0, 1, Rational[127, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["springgrEen"]],RGBColor[0, 1, Rational[127, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["springgreen"]],RGBColor[0, 1, Rational[127, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["snw"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sn\\6F w"]],RGBColor[1, Rational[50, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sn\\ow"]],RGBColor[1, Rational[50, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["snOw"]],RGBColor[1, Rational[50, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["snow"]],RGBColor[1, Rational[50, 51], Rational[50, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slaterey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slate\\67 rey"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slate\\grey"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slateGrey"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slategrey"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slatgray"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slat\\65 gray"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slatEgray"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slategray"]],RGBColor[Rational[112, 255], Rational[128, 255], Rational[48, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slaeblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sla\\74 eblue"]],RGBColor[Rational[106, 255], Rational[6, 17], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sla\\teblue"]],RGBColor[Rational[106, 255], Rational[6, 17], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slaTeblue"]],RGBColor[Rational[106, 255], Rational[6, 17], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slateblue"]],RGBColor[Rational[106, 255], Rational[6, 17], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["s\:212ayblue"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[47, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["skyblu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["skyblu\\65 "]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[47, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["skybluE"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[47, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["skyblue"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[47, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["slver"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["s\\69 lver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["s\\ilver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sIlver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["silver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ienna"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\73 ienna"]],RGBColor[Rational[32, 51], Rational[82, 255], Rational[3, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\sienna"]],RGBColor[Rational[32, 51], Rational[82, 255], Rational[3, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Sienna"]],RGBColor[Rational[32, 51], Rational[82, 255], Rational[3, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sienna"]],RGBColor[Rational[32, 51], Rational[82, 255], Rational[3, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seashel"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seashel\\6C "]],RGBColor[1, Rational[49, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seashel\\l"]],RGBColor[1, Rational[49, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seashelL"]],RGBColor[1, Rational[49, 51], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seashell"]],RGBColor[1, Rational[49, 51], Rational[14, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seagren"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seagre\\65 n"]],RGBColor[Rational[46, 255], Rational[139, 255], Rational[29, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seagreEn"]],RGBColor[Rational[46, 255], Rational[139, 255], Rational[29, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["seagreen"]],RGBColor[Rational[46, 255], Rational[139, 255], Rational[29, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sndybrown"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["s\\61 ndybrown"]],RGBColor[Rational[244, 255], Rational[164, 255], Rational[32, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sAndybrown"]],RGBColor[Rational[244, 255], Rational[164, 255], Rational[32, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sandybrown"]],RGBColor[Rational[244, 255], Rational[164, 255], Rational[32, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["samon"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sa\\6C mon"]],RGBColor[Rational[50, 51], Rational[128, 255], Rational[38, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["sa\\lmon"]],RGBColor[Rational[50, 51], Rational[128, 255], Rational[38, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saLmon"]],RGBColor[Rational[50, 51], Rational[128, 255], Rational[38, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["salmon"]],RGBColor[Rational[50, 51], Rational[128, 255], Rational[38, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saddlebown"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saddleb\\72 own"]],RGBColor[Rational[139, 255], Rational[23, 85], Rational[19, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saddleb\\rown"]],RGBColor[Rational[139, 255], Rational[23, 85], Rational[19, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saddlebRown"]],RGBColor[Rational[139, 255], Rational[23, 85], Rational[19, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["saddlebrown"]],RGBColor[Rational[139, 255], Rational[23, 85], Rational[19, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["roylblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["roy\\61 lblue"]],RGBColor[Rational[13, 51], Rational[7, 17], Rational[15, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["royAlblue"]],RGBColor[Rational[13, 51], Rational[7, 17], Rational[15, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["royalblue"]],RGBColor[Rational[13, 51], Rational[7, 17], Rational[15, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["roybrown"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ro\\73 ybrown"]],RGBColor[Rational[188, 255], Rational[143, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ro\\sybrown"]],RGBColor[Rational[188, 255], Rational[143, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["roSybrown"]],RGBColor[Rational[188, 255], Rational[143, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["rosybrown"]],RGBColor[Rational[188, 255], Rational[143, 255], Rational[143, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["rd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["r\\65 d"]],RGBColor[1, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["rEd"]],RGBColor[1, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["red"]],RGBColor[1, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["purle"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pur\\70 le"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pur\\ple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["purPle"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["purple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["powdrblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["powd\\65 rblue"]],RGBColor[Rational[176, 255], Rational[224, 255], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["powdErblue"]],RGBColor[Rational[176, 255], Rational[224, 255], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["powderblue"]],RGBColor[Rational[176, 255], Rational[224, 255], Rational[46, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pum"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["p\\6C um"]],RGBColor[Rational[13, 15], Rational[32, 51], Rational[13, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["p\\lum"]],RGBColor[Rational[13, 15], Rational[32, 51], Rational[13, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pLum"]],RGBColor[Rational[13, 15], Rational[32, 51], Rational[13, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["plum"]],RGBColor[Rational[13, 15], Rational[32, 51], Rational[13, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pin\:212a"]],RGBColor[1, Rational[64, 85], Rational[203, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ink"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\70 ink"]],RGBColor[1, Rational[64, 85], Rational[203, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\pink"]],RGBColor[1, Rational[64, 85], Rational[203, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Pink"]],RGBColor[1, Rational[64, 85], Rational[203, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pink"]],RGBColor[1, Rational[64, 85], Rational[203, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["per"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["per\\75 "]],RGBColor[Rational[41, 51], Rational[133, 255], Rational[21, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["per\\u"]],RGBColor[Rational[41, 51], Rational[133, 255], Rational[21, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["perU"]],RGBColor[Rational[41, 51], Rational[133, 255], Rational[21, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peru"]],RGBColor[Rational[41, 51], Rational[133, 255], Rational[21, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peacpuff"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peac\\68 puff"]],RGBColor[1, Rational[218, 255], Rational[37, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peac\\hpuff"]],RGBColor[1, Rational[218, 255], Rational[37, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peacHpuff"]],RGBColor[1, Rational[218, 255], Rational[37, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["peachpuff"]],RGBColor[1, Rational[218, 255], Rational[37, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["papayawhi"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["papayawhi\\70 "]],RGBColor[1, Rational[239, 255], Rational[71, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["papayawhi\\p"]],RGBColor[1, Rational[239, 255], Rational[71, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["papayawhiP"]],RGBColor[1, Rational[239, 255], Rational[71, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["papayawhip"]],RGBColor[1, Rational[239, 255], Rational[71, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palevioletrd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palevioletr\\65 d"]],RGBColor[Rational[73, 85], Rational[112, 255], Rational[49, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palevioletrEd"]],RGBColor[Rational[73, 85], Rational[112, 255], Rational[49, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palevioletred"]],RGBColor[Rational[73, 85], Rational[112, 255], Rational[49, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["paleturquose"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["paleturquo\\69 se"]],RGBColor[Rational[35, 51], Rational[14, 15], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["paleturquo\\ise"]],RGBColor[Rational[35, 51], Rational[14, 15], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["paleturquoIse"]],RGBColor[Rational[35, 51], Rational[14, 15], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["paleturquoise"]],RGBColor[Rational[35, 51], Rational[14, 15], Rational[14, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["alegreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\70 alegreen"]],RGBColor[Rational[152, 255], Rational[251, 255], Rational[152, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\palegreen"]],RGBColor[Rational[152, 255], Rational[251, 255], Rational[152, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Palegreen"]],RGBColor[Rational[152, 255], Rational[251, 255], Rational[152, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palegreen"]],RGBColor[Rational[152, 255], Rational[251, 255], Rational[152, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palegoldnrod"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palegold\\65 nrod"]],RGBColor[Rational[14, 15], Rational[232, 255], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palegoldEnrod"]],RGBColor[Rational[14, 15], Rational[232, 255], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["palegoldenrod"]],RGBColor[Rational[14, 15], Rational[232, 255], Rational[2, 3]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orchd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orch\\69 d"]],RGBColor[Rational[218, 255], Rational[112, 255], Rational[214, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orch\\id"]],RGBColor[Rational[218, 255], Rational[112, 255], Rational[214, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orchId"]],RGBColor[Rational[218, 255], Rational[112, 255], Rational[214, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orchid"]],RGBColor[Rational[218, 255], Rational[112, 255], Rational[214, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orangeed"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orange\\72 ed"]],RGBColor[1, Rational[23, 85], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orange\\red"]],RGBColor[1, Rational[23, 85], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orangeRed"]],RGBColor[1, Rational[23, 85], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orangered"]],RGBColor[1, Rational[23, 85], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ornge"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["or\\61 nge"]],RGBColor[1, Rational[11, 17], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orAnge"]],RGBColor[1, Rational[11, 17], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["orange"]],RGBColor[1, Rational[11, 17], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["olivdrab"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["oliv\\65 drab"]],RGBColor[Rational[107, 255], Rational[142, 255], Rational[7, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["olivEdrab"]],RGBColor[Rational[107, 255], Rational[142, 255], Rational[7, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["olivedrab"]],RGBColor[Rational[107, 255], Rational[142, 255], Rational[7, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["live"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\6F live"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\olive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Olive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["olive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ldlace"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\6F ldlace"]],RGBColor[Rational[253, 255], Rational[49, 51], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\oldlace"]],RGBColor[Rational[253, 255], Rational[49, 51], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Oldlace"]],RGBColor[Rational[253, 255], Rational[49, 51], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["oldlace"]],RGBColor[Rational[253, 255], Rational[49, 51], Rational[46, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["nay"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["na\\76 y"]],RGBColor[0, 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["na\\vy"]],RGBColor[0, 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["naVy"]],RGBColor[0, 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navy"]],RGBColor[0, 0, Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navajowite"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navajow\\68 ite"]],RGBColor[1, Rational[74, 85], Rational[173, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navajow\\hite"]],RGBColor[1, Rational[74, 85], Rational[173, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navajowHite"]],RGBColor[1, Rational[74, 85], Rational[173, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navajowhite"]],RGBColor[1, Rational[74, 85], Rational[173, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["moccsin"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mocc\\61 sin"]],RGBColor[1, Rational[76, 85], Rational[181, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["moccAsin"]],RGBColor[1, Rational[76, 85], Rational[181, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["moccasin"]],RGBColor[1, Rational[76, 85], Rational[181, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mistyroe"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mistyro\\73 e"]],RGBColor[1, Rational[76, 85], Rational[15, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mistyro\\se"]],RGBColor[1, Rational[76, 85], Rational[15, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mistyroSe"]],RGBColor[1, Rational[76, 85], Rational[15, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mistyrose"]],RGBColor[1, Rational[76, 85], Rational[15, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mintcram"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mintcr\\65 am"]],RGBColor[Rational[49, 51], 1, Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mintcrEam"]],RGBColor[Rational[49, 51], 1, Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mintcream"]],RGBColor[Rational[49, 51], 1, Rational[50, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["midnihtblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["midni\\67 htblue"]],RGBColor[Rational[5, 51], Rational[5, 51], Rational[112, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["midni\\ghtblue"]],RGBColor[Rational[5, 51], Rational[5, 51], Rational[112, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["midniGhtblue"]],RGBColor[Rational[5, 51], Rational[5, 51], Rational[112, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["midnightblue"]],RGBColor[Rational[5, 51], Rational[5, 51], Rational[112, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumvoletred"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumv\\69 oletred"]],RGBColor[Rational[199, 255], Rational[7, 85], Rational[133, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumv\\ioletred"]],RGBColor[Rational[199, 255], Rational[7, 85], Rational[133, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumvIoletred"]],RGBColor[Rational[199, 255], Rational[7, 85], Rational[133, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumvioletred"]],RGBColor[Rational[199, 255], Rational[7, 85], Rational[133, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumurquoise"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["medium\\74 urquoise"]],RGBColor[Rational[24, 85], Rational[209, 255], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["medium\\turquoise"]],RGBColor[Rational[24, 85], Rational[209, 255], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumTurquoise"]],RGBColor[Rational[24, 85], Rational[209, 255], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumturquoise"]],RGBColor[Rational[24, 85], Rational[209, 255], Rational[4, 5]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumspinggreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumsp\\72 inggreen"]],RGBColor[0, Rational[50, 51], Rational[154, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumsp\\ringgreen"]],RGBColor[0, Rational[50, 51], Rational[154, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumspRinggreen"]],RGBColor[0, Rational[50, 51], Rational[154, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumspringgreen"]],RGBColor[0, Rational[50, 51], Rational[154, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["medimslateblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["medi\\75 mslateblue"]],RGBColor[Rational[41, 85], Rational[104, 255], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["medi\\umslateblue"]],RGBColor[Rational[41, 85], Rational[104, 255], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediUmslateblue"]],RGBColor[Rational[41, 85], Rational[104, 255], Rational[14, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumslateblue"]],RGBColor[Rational[41, 85], Rational[104, 255], Rational[14, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumseageen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumseag\\72 een"]],RGBColor[Rational[4, 17], Rational[179, 255], Rational[113, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumseag\\reen"]],RGBColor[Rational[4, 17], Rational[179, 255], Rational[113, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumseagReen"]],RGBColor[Rational[4, 17], Rational[179, 255], Rational[113, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumseagreen"]],RGBColor[Rational[4, 17], Rational[179, 255], Rational[113, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumpurpl"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumpurpl\\65 "]],RGBColor[Rational[49, 85], Rational[112, 255], Rational[73, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumpurplE"]],RGBColor[Rational[49, 85], Rational[112, 255], Rational[73, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumpurple"]],RGBColor[Rational[49, 85], Rational[112, 255], Rational[73, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumorchd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumorch\\69 d"]],RGBColor[Rational[62, 85], Rational[1, 3], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumorch\\id"]],RGBColor[Rational[62, 85], Rational[1, 3], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumorchId"]],RGBColor[Rational[62, 85], Rational[1, 3], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumorchid"]],RGBColor[Rational[62, 85], Rational[1, 3], Rational[211, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediublue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediu\\6D blue"]],RGBColor[0, 0, Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediu\\mblue"]],RGBColor[0, 0, Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediuMblue"]],RGBColor[0, 0, Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumblue"]],RGBColor[0, 0, Rational[41, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumaqamarine"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumaq\\75 amarine"]],RGBColor[Rational[2, 5], Rational[41, 51], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumaq\\uamarine"]],RGBColor[Rational[2, 5], Rational[41, 51], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumaqUamarine"]],RGBColor[Rational[2, 5], Rational[41, 51], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mediumaquamarine"]],RGBColor[Rational[2, 5], Rational[41, 51], Rational[2, 3]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mroon"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["m\\61 roon"]],RGBColor[Rational[128, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mAroon"]],RGBColor[Rational[128, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["maroon"]],RGBColor[Rational[128, 255], 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mageta"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mage\\6E ta"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mage\\nta"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["mageNta"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["magenta"]],RGBColor[1, 0, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lnen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\69 nen"]],RGBColor[Rational[50, 51], Rational[16, 17], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\inen"]],RGBColor[Rational[50, 51], Rational[16, 17], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lInen"]],RGBColor[Rational[50, 51], Rational[16, 17], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["linen"]],RGBColor[Rational[50, 51], Rational[16, 17], Rational[46, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lmegreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\69 megreen"]],RGBColor[Rational[10, 51], Rational[41, 51], Rational[10, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\imegreen"]],RGBColor[Rational[10, 51], Rational[41, 51], Rational[10, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lImegreen"]],RGBColor[Rational[10, 51], Rational[41, 51], Rational[10, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["limegreen"]],RGBColor[Rational[10, 51], Rational[41, 51], Rational[10, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lim"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lim\\65 "]],RGBColor[0, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["limE"]],RGBColor[0, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lime"]],RGBColor[0, 1, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyello"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyello\\77 "]],RGBColor[1, 1, Rational[224, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyello\\w"]],RGBColor[1, 1, Rational[224, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyelloW"]],RGBColor[1, 1, Rational[224, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyellow"]],RGBColor[1, 1, Rational[224, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightsteelblu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightsteelblu\\65 "]],RGBColor[Rational[176, 255], Rational[196, 255], Rational[74, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightsteelbluE"]],RGBColor[Rational[176, 255], Rational[196, 255], Rational[74, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightsteelblue"]],RGBColor[Rational[176, 255], Rational[196, 255], Rational[74, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategry"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategr\\65 y"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategrEy"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategrey"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategay"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslateg\\72 ay"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslateg\\ray"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategRay"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightslategray"]],RGBColor[Rational[7, 15], Rational[8, 15], Rational[3, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lights\:212ayblue"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightskyble"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightskybl\\75 e"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightskybl\\ue"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightskyblUe"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightskyblue"]],RGBColor[Rational[9, 17], Rational[206, 255], Rational[50, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lihtseagreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["li\\67 htseagreen"]],RGBColor[Rational[32, 255], Rational[178, 255], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["li\\ghtseagreen"]],RGBColor[Rational[32, 255], Rational[178, 255], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["liGhtseagreen"]],RGBColor[Rational[32, 255], Rational[178, 255], Rational[2, 3]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightseagreen"]],RGBColor[Rational[32, 255], Rational[178, 255], Rational[2, 3]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lighsalmon"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ligh\\74 salmon"]],RGBColor[1, Rational[32, 51], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ligh\\tsalmon"]],RGBColor[1, Rational[32, 51], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lighTsalmon"]],RGBColor[1, Rational[32, 51], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightsalmon"]],RGBColor[1, Rational[32, 51], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightpin\:212a"]],RGBColor[1, Rational[182, 255], Rational[193, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lghtpink"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\69 ghtpink"]],RGBColor[1, Rational[182, 255], Rational[193, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\ightpink"]],RGBColor[1, Rational[182, 255], Rational[193, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lIghtpink"]],RGBColor[1, Rational[182, 255], Rational[193, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightpink"]],RGBColor[1, Rational[182, 255], Rational[193, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ightgrey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\6C ightgrey"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\lightgrey"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Lightgrey"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgrey"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgree"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgree\\6E "]],RGBColor[Rational[48, 85], Rational[14, 15], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgree\\n"]],RGBColor[Rational[48, 85], Rational[14, 15], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgreeN"]],RGBColor[Rational[48, 85], Rational[14, 15], Rational[48, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgreen"]],RGBColor[Rational[48, 85], Rational[14, 15], Rational[48, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgry"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgr\\61 y"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgrAy"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgray"]],RGBColor[Rational[211, 255], Rational[211, 255], Rational[211, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgodenrodyellow"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgo\\6C denrodyellow"]],RGBColor[Rational[50, 51], Rational[50, 51], Rational[14, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgo\\ldenrodyellow"]],RGBColor[Rational[50, 51], Rational[50, 51], Rational[14, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgoLdenrodyellow"]],RGBColor[Rational[50, 51], Rational[50, 51], Rational[14, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightgoldenrodyellow"]],RGBColor[Rational[50, 51], Rational[50, 51], Rational[14, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightyan"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["light\\63 yan"]],RGBColor[Rational[224, 255], 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightCyan"]],RGBColor[Rational[224, 255], 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightcyan"]],RGBColor[Rational[224, 255], 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightoral"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["light\\63 oral"]],RGBColor[Rational[16, 17], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightCoral"]],RGBColor[Rational[16, 17], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightcoral"]],RGBColor[Rational[16, 17], Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ligtblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lig\\68 tblue"]],RGBColor[Rational[173, 255], Rational[72, 85], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lig\\htblue"]],RGBColor[Rational[173, 255], Rational[72, 85], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ligHtblue"]],RGBColor[Rational[173, 255], Rational[72, 85], Rational[46, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lightblue"]],RGBColor[Rational[173, 255], Rational[72, 85], Rational[46, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lemonchiffo"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lemonchiffo\\6E "]],RGBColor[1, Rational[50, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lemonchiffo\\n"]],RGBColor[1, Rational[50, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lemonchiffoN"]],RGBColor[1, Rational[50, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lemonchiffon"]],RGBColor[1, Rational[50, 51], Rational[41, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lwngreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["l\\61 wngreen"]],RGBColor[Rational[124, 255], Rational[84, 85], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lAwngreen"]],RGBColor[Rational[124, 255], Rational[84, 85], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lawngreen"]],RGBColor[Rational[124, 255], Rational[84, 85], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lavnderblush"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lav\\65 nderblush"]],RGBColor[1, Rational[16, 17], Rational[49, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lavEnderblush"]],RGBColor[1, Rational[16, 17], Rational[49, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lavenderblush"]],RGBColor[1, Rational[16, 17], Rational[49, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["avender"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\6C avender"]],RGBColor[Rational[46, 51], Rational[46, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\lavender"]],RGBColor[Rational[46, 51], Rational[46, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Lavender"]],RGBColor[Rational[46, 51], Rational[46, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lavender"]],RGBColor[Rational[46, 51], Rational[46, 51], Rational[50, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\:212aha\:212ai"]],RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["khak"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["khak\\69 "]],RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["khak\\i"]],RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["khakI"]],RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["khaki"]],RGBColor[Rational[16, 17], Rational[46, 51], Rational[28, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ivoy"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ivo\\72 y"]],RGBColor[1, 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ivo\\ry"]],RGBColor[1, 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ivoRy"]],RGBColor[1, 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ivory"]],RGBColor[1, 1, Rational[16, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indig"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indig\\6F "]],RGBColor[Rational[5, 17], 0, Rational[26, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indig\\o"]],RGBColor[Rational[5, 17], 0, Rational[26, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indigO"]],RGBColor[Rational[5, 17], 0, Rational[26, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indigo"]],RGBColor[Rational[5, 17], 0, Rational[26, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indinred"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indi\\61 nred"]],RGBColor[Rational[41, 51], Rational[92, 255], Rational[92, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indiAnred"]],RGBColor[Rational[41, 51], Rational[92, 255], Rational[92, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["indianred"]],RGBColor[Rational[41, 51], Rational[92, 255], Rational[92, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpin\:212a"]],RGBColor[1, Rational[7, 17], Rational[12, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpik"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpi\\6E k"]],RGBColor[1, Rational[7, 17], Rational[12, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpi\\nk"]],RGBColor[1, Rational[7, 17], Rational[12, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpiNk"]],RGBColor[1, Rational[7, 17], Rational[12, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hotpink"]],RGBColor[1, Rational[7, 17], Rational[12, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hoeydew"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ho\\6E eydew"]],RGBColor[Rational[16, 17], 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ho\\neydew"]],RGBColor[Rational[16, 17], 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["hoNeydew"]],RGBColor[Rational[16, 17], 1, Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["honeydew"]],RGBColor[Rational[16, 17], 1, Rational[16, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["g\\72 ey"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["g\\rey"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gRey"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["grey"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["greenyllow"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["greeny\\65 llow"]],RGBColor[Rational[173, 255], 1, Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["greenyEllow"]],RGBColor[Rational[173, 255], 1, Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["greenyellow"]],RGBColor[Rational[173, 255], 1, Rational[47, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["geen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["g\\72 een"]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["g\\reen"]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gReen"]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["green"]],RGBColor[0, Rational[128, 255], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gry"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gr\\61 y"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["grAy"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gray"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["goldenod"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["golden\\72 od"]],RGBColor[Rational[218, 255], Rational[11, 17], Rational[32, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["golden\\rod"]],RGBColor[Rational[218, 255], Rational[11, 17], Rational[32, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["goldenRod"]],RGBColor[Rational[218, 255], Rational[11, 17], Rational[32, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["goldenrod"]],RGBColor[Rational[218, 255], Rational[11, 17], Rational[32, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["old"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\67 old"]],RGBColor[1, Rational[43, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\gold"]],RGBColor[1, Rational[43, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Gold"]],RGBColor[1, Rational[43, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gold"]],RGBColor[1, Rational[43, 51], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ghostwhte"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ghostwh\\69 te"]],RGBColor[Rational[248, 255], Rational[248, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ghostwh\\ite"]],RGBColor[Rational[248, 255], Rational[248, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ghostwhIte"]],RGBColor[Rational[248, 255], Rational[248, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ghostwhite"]],RGBColor[Rational[248, 255], Rational[248, 255], 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gaisboro"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gai\\6E sboro"]],RGBColor[Rational[44, 51], Rational[44, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gai\\nsboro"]],RGBColor[Rational[44, 51], Rational[44, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gaiNsboro"]],RGBColor[Rational[44, 51], Rational[44, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gainsboro"]],RGBColor[Rational[44, 51], Rational[44, 51], Rational[44, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fuhsia"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fu\\63 hsia"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fuChsia"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fuchsia"]],RGBColor[1, 0, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["forestgren"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["forestgre\\65 n"]],RGBColor[Rational[2, 15], Rational[139, 255], Rational[2, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["forestgreEn"]],RGBColor[Rational[2, 15], Rational[139, 255], Rational[2, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["forestgreen"]],RGBColor[Rational[2, 15], Rational[139, 255], Rational[2, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["floralwhte"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["floralwh\\69 te"]],RGBColor[1, Rational[50, 51], Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["floralwh\\ite"]],RGBColor[1, Rational[50, 51], Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["floralwhIte"]],RGBColor[1, Rational[50, 51], Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["floralwhite"]],RGBColor[1, Rational[50, 51], Rational[16, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebric\:212a"]],RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebric"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebric\\6B "]],RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebric\\k"]],RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebricK"]],RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["firebrick"]],RGBColor[Rational[178, 255], Rational[2, 15], Rational[2, 15]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ddgerblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\6F dgerblue"]],RGBColor[Rational[2, 17], Rational[48, 85], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\odgerblue"]],RGBColor[Rational[2, 17], Rational[48, 85], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dOdgerblue"]],RGBColor[Rational[2, 17], Rational[48, 85], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dodgerblue"]],RGBColor[Rational[2, 17], Rational[48, 85], 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimgey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimg\\72 ey"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimg\\rey"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimgRey"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimgrey"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimray"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dim\\67 ray"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dim\\gray"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimGray"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dimgray"]],RGBColor[Rational[7, 17], Rational[7, 17], Rational[7, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deeps\:212ayblue"]],RGBColor[0, Rational[191, 255], 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deeskyblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dee\\70 skyblue"]],RGBColor[0, Rational[191, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dee\\pskyblue"]],RGBColor[0, Rational[191, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deePskyblue"]],RGBColor[0, Rational[191, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deepskyblue"]],RGBColor[0, Rational[191, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deeppin\:212a"]],RGBColor[1, Rational[4, 51], Rational[49, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deppink"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\65 eppink"]],RGBColor[1, Rational[4, 51], Rational[49, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dEeppink"]],RGBColor[1, Rational[4, 51], Rational[49, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["deeppink"]],RGBColor[1, Rational[4, 51], Rational[49, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aviolet"]],RGBColor[Rational[148, 255], 0, Rational[211, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkvilet"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkvi\\6F let"]],RGBColor[Rational[148, 255], 0, Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkvi\\olet"]],RGBColor[Rational[148, 255], 0, Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkviOlet"]],RGBColor[Rational[148, 255], 0, Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkviolet"]],RGBColor[Rational[148, 255], 0, Rational[211, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aturquoise"]],RGBColor[0, Rational[206, 255], Rational[209, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darturquoise"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\6B turquoise"]],RGBColor[0, Rational[206, 255], Rational[209, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\kturquoise"]],RGBColor[0, Rational[206, 255], Rational[209, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darKturquoise"]],RGBColor[0, Rational[206, 255], Rational[209, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkturquoise"]],RGBColor[0, Rational[206, 255], Rational[209, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aslategrey"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dakslategrey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["da\\72 kslategrey"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["da\\rkslategrey"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["daRkslategrey"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkslategrey"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aslategray"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["drkslategray"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\61 rkslategray"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dArkslategray"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkslategray"]],RGBColor[Rational[47, 255], Rational[79, 255], Rational[79, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aslateblue"]],RGBColor[Rational[24, 85], Rational[61, 255], Rational[139, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["arkslateblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\64 arkslateblue"]],RGBColor[Rational[24, 85], Rational[61, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Darkslateblue"]],RGBColor[Rational[24, 85], Rational[61, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkslateblue"]],RGBColor[Rational[24, 85], Rational[61, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aseagreen"]],RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darseagreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\6B seagreen"]],RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\kseagreen"]],RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darKseagreen"]],RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkseagreen"]],RGBColor[Rational[143, 255], Rational[188, 255], Rational[143, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212asalmon"]],RGBColor[Rational[233, 255], Rational[10, 17], Rational[122, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["arksalmon"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\64 arksalmon"]],RGBColor[Rational[233, 255], Rational[10, 17], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Darksalmon"]],RGBColor[Rational[233, 255], Rational[10, 17], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darksalmon"]],RGBColor[Rational[233, 255], Rational[10, 17], Rational[122, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212ared"]],RGBColor[Rational[139, 255], 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["arkred"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\64 arkred"]],RGBColor[Rational[139, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Darkred"]],RGBColor[Rational[139, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkred"]],RGBColor[Rational[139, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aorchid"]],RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorchd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorch\\69 d"]],RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorch\\id"]],RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorchId"]],RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorchid"]],RGBColor[Rational[3, 5], Rational[10, 51], Rational[4, 5]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aorange"]],RGBColor[1, Rational[28, 51], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorage"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkora\\6E ge"]],RGBColor[1, Rational[28, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkora\\nge"]],RGBColor[1, Rational[28, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkoraNge"]],RGBColor[1, Rational[28, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkorange"]],RGBColor[1, Rational[28, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212aolivegreen"]],RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darklivegreen"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dark\\6F livegreen"]],RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dark\\olivegreen"]],RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkOlivegreen"]],RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkolivegreen"]],RGBColor[Rational[1, 3], Rational[107, 255], Rational[47, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212amagenta"]],RGBColor[Rational[139, 255], 0, Rational[139, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["drkmagenta"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\61 rkmagenta"]],RGBColor[Rational[139, 255], 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dArkmagenta"]],RGBColor[Rational[139, 255], 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkmagenta"]],RGBColor[Rational[139, 255], 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212a\:212aha\:212ai"]],RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkkhak"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkkhak\\69 "]],RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkkhak\\i"]],RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkkhakI"]],RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkkhaki"]],RGBColor[Rational[63, 85], Rational[61, 85], Rational[107, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212agrey"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dargrey"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\6B grey"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\\kgrey"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darKgrey"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgrey"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212agreen"]],RGBColor[0, Rational[20, 51], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgren"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgr\\65 en"]],RGBColor[0, Rational[20, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgrEen"]],RGBColor[0, Rational[20, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgreen"]],RGBColor[0, Rational[20, 51], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212agray"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["drkgray"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\61 rkgray"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dArkgray"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgray"]],RGBColor[Rational[169, 255], Rational[169, 255], Rational[169, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212agoldenrod"]],RGBColor[Rational[184, 255], Rational[134, 255], Rational[11, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["drkgoldenrod"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["d\\61 rkgoldenrod"]],RGBColor[Rational[184, 255], Rational[134, 255], Rational[11, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dArkgoldenrod"]],RGBColor[Rational[184, 255], Rational[134, 255], Rational[11, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkgoldenrod"]],RGBColor[Rational[184, 255], Rational[134, 255], Rational[11, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212acyan"]],RGBColor[0, Rational[139, 255], Rational[139, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkcya"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkcya\\6E "]],RGBColor[0, Rational[139, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkcya\\n"]],RGBColor[0, Rational[139, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkcyaN"]],RGBColor[0, Rational[139, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkcyan"]],RGBColor[0, Rational[139, 255], Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["dar\:212ablue"]],RGBColor[0, 0, Rational[139, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkble"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkbl\\75 e"]],RGBColor[0, 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkbl\\ue"]],RGBColor[0, 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkblUe"]],RGBColor[0, 0, Rational[139, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["darkblue"]],RGBColor[0, 0, Rational[139, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["can"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\79 an"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\yan"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cYan"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cyan"]],RGBColor[0, 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cimson"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\72 imson"]],RGBColor[Rational[44, 51], Rational[4, 51], Rational[4, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\rimson"]],RGBColor[Rational[44, 51], Rational[4, 51], Rational[4, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cRimson"]],RGBColor[Rational[44, 51], Rational[4, 51], Rational[4, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["crimson"]],RGBColor[Rational[44, 51], Rational[4, 51], Rational[4, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornsil\:212a"]],RGBColor[1, Rational[248, 255], Rational[44, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["corsilk"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cor\\6E silk"]],RGBColor[1, Rational[248, 255], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cor\\nsilk"]],RGBColor[1, Rational[248, 255], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["corNsilk"]],RGBColor[1, Rational[248, 255], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornsilk"]],RGBColor[1, Rational[248, 255], Rational[44, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornflwerblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornfl\\6F werblue"]],RGBColor[Rational[20, 51], Rational[149, 255], Rational[79, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornfl\\owerblue"]],RGBColor[Rational[20, 51], Rational[149, 255], Rational[79, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornflOwerblue"]],RGBColor[Rational[20, 51], Rational[149, 255], Rational[79, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cornflowerblue"]],RGBColor[Rational[20, 51], Rational[149, 255], Rational[79, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["corl"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cor\\61 l"]],RGBColor[1, Rational[127, 255], Rational[16, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["corAl"]],RGBColor[1, Rational[127, 255], Rational[16, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["coral"]],RGBColor[1, Rational[127, 255], Rational[16, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["chocoate"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["choco\\6C ate"]],RGBColor[Rational[14, 17], Rational[7, 17], Rational[2, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["choco\\late"]],RGBColor[Rational[14, 17], Rational[7, 17], Rational[2, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["chocoLate"]],RGBColor[Rational[14, 17], Rational[7, 17], Rational[2, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["chocolate"]],RGBColor[Rational[14, 17], Rational[7, 17], Rational[2, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cartreuse"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\68 artreuse"]],RGBColor[Rational[127, 255], 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["c\\hartreuse"]],RGBColor[Rational[127, 255], 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cHartreuse"]],RGBColor[Rational[127, 255], 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["chartreuse"]],RGBColor[Rational[127, 255], 1, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cadtblue"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cad\\65 tblue"]],RGBColor[Rational[19, 51], Rational[158, 255], Rational[32, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cadEtblue"]],RGBColor[Rational[19, 51], Rational[158, 255], Rational[32, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["cadetblue"]],RGBColor[Rational[19, 51], Rational[158, 255], Rational[32, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bulywood"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bu\\72 lywood"]],RGBColor[Rational[74, 85], Rational[184, 255], Rational[9, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bu\\rlywood"]],RGBColor[Rational[74, 85], Rational[184, 255], Rational[9, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["buRlywood"]],RGBColor[Rational[74, 85], Rational[184, 255], Rational[9, 17]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["burlywood"]],RGBColor[Rational[74, 85], Rational[184, 255], Rational[9, 17]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bron"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bro\\77 n"]],RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bro\\wn"]],RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["broWn"]],RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["brown"]],RGBColor[Rational[11, 17], Rational[14, 85], Rational[14, 85]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bluevioet"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bluevio\\6C et"]],RGBColor[Rational[46, 85], Rational[43, 255], Rational[226, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bluevio\\let"]],RGBColor[Rational[46, 85], Rational[43, 255], Rational[226, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bluevioLet"]],RGBColor[Rational[46, 85], Rational[43, 255], Rational[226, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blueviolet"]],RGBColor[Rational[46, 85], Rational[43, 255], Rational[226, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ble"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bl\\75 e"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bl\\ue"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blUe"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blue"]],RGBColor[0, 0, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blanchedalmnd"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blanchedalm\\6F nd"]],RGBColor[1, Rational[47, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blanchedalm\\ond"]],RGBColor[1, Rational[47, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blanchedalmOnd"]],RGBColor[1, Rational[47, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blanchedalmond"]],RGBColor[1, Rational[47, 51], Rational[41, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blac\:212a"]],RGBColor[0, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blac"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blac\\6B "]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blac\\k"]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blacK"]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["black"]],RGBColor[0, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bisqu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bisqu\\65 "]],RGBColor[1, Rational[76, 85], Rational[196, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bisquE"]],RGBColor[1, Rational[76, 85], Rational[196, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bisque"]],RGBColor[1, Rational[76, 85], Rational[196, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bege"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["be\\69 ge"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["be\\ige"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["beIge"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[44, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["beige"]],RGBColor[Rational[49, 51], Rational[49, 51], Rational[44, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aure"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["a\\7A ure"]],RGBColor[Rational[16, 17], 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["a\\zure"]],RGBColor[Rational[16, 17], 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aZure"]],RGBColor[Rational[16, 17], 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["azure"]],RGBColor[Rational[16, 17], 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["quamarine"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\61 quamarine"]],RGBColor[Rational[127, 255], 1, Rational[212, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Aquamarine"]],RGBColor[Rational[127, 255], 1, Rational[212, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aquamarine"]],RGBColor[Rational[127, 255], 1, Rational[212, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aqu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aqu\\61 "]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aquA"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aqua"]],RGBColor[0, 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["antiquwhite"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["antiqu\\65 white"]],RGBColor[Rational[50, 51], Rational[47, 51], Rational[43, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["antiquEwhite"]],RGBColor[Rational[50, 51], Rational[47, 51], Rational[43, 51]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["antiquewhite"]],RGBColor[Rational[50, 51], Rational[47, 51], Rational[43, 51]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aliceblu"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aliceblu\\65 "]],RGBColor[Rational[16, 17], Rational[248, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["alicebluE"]],RGBColor[Rational[16, 17], Rational[248, 255], 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aliceblue"]],RGBColor[Rational[16, 17], Rational[248, 255], 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["qua"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\61 qua"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Aqua"]],RGBColor[0, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["aqua"]],RGBColor[0, 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tea"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tea\\6C "]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["tea\\l"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["teaL"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["teal"]],RGBColor[0, Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ble"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bl\\75 e"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bl\\ue"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blUe"]],RGBColor[0, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blue"]],RGBColor[0, 0, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["nvy"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["n\\61 vy"]],RGBColor[0, 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["nAvy"]],RGBColor[0, 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["navy"]],RGBColor[0, 0, Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ellow"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\79 ellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\yellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Yellow"]],RGBColor[1, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["yellow"]],RGBColor[1, 1, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["oive"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["o\\6C ive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["o\\live"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["oLive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["olive"]],RGBColor[Rational[128, 255], Rational[128, 255], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lie"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["li\\6D e"]],RGBColor[0, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["li\\me"]],RGBColor[0, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["liMe"]],RGBColor[0, 1, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["lime"]],RGBColor[0, 1, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gree"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gree\\6E "]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gree\\n"]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["greeN"]],RGBColor[0, Rational[128, 255], 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["green"]],RGBColor[0, Rational[128, 255], 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fchsia"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["f\\75 chsia"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["f\\uchsia"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fUchsia"]],RGBColor[1, 0, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["fuchsia"]],RGBColor[1, 0, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["prple"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["p\\75 rple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["p\\urple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["pUrple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["purple"]],RGBColor[Rational[128, 255], 0, Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ed"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\72 ed"]],RGBColor[1, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\red"]],RGBColor[1, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Red"]],RGBColor[1, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["red"]],RGBColor[1, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["maroo"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["maroo\\6E "]],RGBColor[Rational[128, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["maroo\\n"]],RGBColor[Rational[128, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["marooN"]],RGBColor[Rational[128, 255], 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["maroon"]],RGBColor[Rational[128, 255], 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whit"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whit\\65 "]],RGBColor[1, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["whitE"]],RGBColor[1, 1, 1]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["white"]],RGBColor[1, 1, 1]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gra"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gra\\79 "]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gra\\y"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["graY"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["gray"]],RGBColor[Rational[128, 255], Rational[128, 255], Rational[128, 255]]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["siver"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["si\\6C ver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["si\\lver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["siLver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["silver"]],RGBColor[Rational[64, 85], Rational[64, 85], Rational[64, 85]]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["blac\:212a"]],RGBColor[0, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["back"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["b\\6C ack"]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["b\\lack"]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["bLack"]],RGBColor[0, 0, 0]]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["black"]],RGBColor[0, 0, 0]]


TestMatch[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["ransparent"]],_Failure]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\74 ransparent"]],None]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["\\transparent"]],None]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["Transparent"]],None]


Test[CSSTools`CSSColors4`Private`parseSingleColorKeyWord["color",CSSNormalizeEscapes["transparent"]],None]
