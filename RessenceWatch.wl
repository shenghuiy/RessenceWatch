(* ::Package:: *)

(* Ressence Type 1 style watch face drawn with graphics primitives; numerals are
   Ostrich Sans glyph outlines.

   ROCS (Ressence Orbital Convex System) model
   -------------------------------------------
   * The whole main disc is the planet carrier: it turns clockwise once per hour.
     The long "minute hand" is simply a marker printed on that disc.
   * Each sub-dial is a "planet" mounted on the carrier and orbits
     with it, while its own hand spins on its own axis.
   * The hour, seconds and 7-day rings are counter-geared so they never spin:
     they orbit but stay upright, and only the hand inside turns.
   * The outer minute scale (10 ... 60) is fixed to the case.

   Usage:
     ressenceFace[DateObject[...]]        static face
     ressenceFace[{h, m, s, weekday}]     weekday 1 = Monday ... 7 = Sunday
     ressenceWatch[]                      live, animated watch (needs front end)
*)

BeginPackage["RessenceWatch`"];

ressenceFace::usage = "ressenceFace[date] draws a Ressence Type 1 style face for date (DateObject or {h, m, s, weekday}).";
ressenceWatch::usage = "ressenceWatch[] gives a live, dynamically updating ROCS watch with a time-warp control.";

Begin["`Private`"];

(* ---------- palette ---------- *)
ink    = GrayLevel[0.17];
disc   = GrayLevel[0.925];
planet = GrayLevel[0.915];
accent = RGBColor[0.96, 0.55, 0.12];
rim    = GrayLevel[0.72];

(* ---------- carrier geometry (disc frame, face radius = 1) ----------
   Positions are taken from the reference photo, where the minute marker sits
   at alpha0 ~ 63 degrees; the satellites are upright in that configuration. *)
alpha0 = 63 Degree;
hourPos = {-0.33, -0.165};  hourR = 0.43;
secPos  = { 0.015, 0.545};  secR  = 0.25;
dayPos  = { 0.455, -0.335}; dayR  = 0.235;

clockwise[v_, a_] := RotationMatrix[-a] . v;

faceHalf = 1.13;   (* half-width of the plot range *)

(* unit vector for a clock angle a (clockwise from 12) *)
clockDir[a_] := {Sin[a], Cos[a]};

(* Slightly tapered hand: wide rounded base, narrower rounded tip, running from
   radius r0 to r1 along unit vector u. *)
taperedHand[u_, r0_, r1_, w0_, w1_] := With[{n = {-u[[2]], u[[1]]}},
  FilledCurve[{
    Line[{r0 u - w0 n, r1 u - w1 n}], BezierCurve[{r1 u - w1 n + 1.3 w1 u, r1 u + w1 n + 1.3 w1 u, r1 u + w1 n}],
    Line[{r0 u + w0 n}], BezierCurve[{r0 u + w0 n - 1.3 w0 u, r0 u - w0 n - 1.3 w0 u, r0 u - w0 n}]}]];

(* Sizes scale with the picture, so the face looks the same at any ImageSize. *)
thin  = Thickness[0.0016];
lineHalf[t_] := t faceHalf;   (* half the width of a Thickness[t] line, in plot units *)

(* ---------- numerals ----------
   Numerals are drawn as filled glyph outlines (FilledCurve with Bezier
   segments) instead of Text: the font rasterizer pixel-snaps small or rotated
   text, which looks jagged next to the vector hands.

   Digits 0-9 of Ostrich Sans Medium by Tyler Finck (Copyright (c) 2011 by
   Tyler Finck), licensed under the SIL Open Font License 1.1
   (https://openfontlicense.org). Taken from the TrueType glyf table, in font
   units (2048 per em, cap height 1380), with the quadratic splines converted
   to cubic Bezier curves. Each entry is {advance width, outline}. *)
ostrichDigits = <|
  "0" -> {655, FilledCurve[{{Line[{{18, 371}}], Line[{{18, 1012}}], BezierCurve[{{18, 1129}, {39, 1219}, {80, 1280}}], BezierCurve[{{122, 1347}, {187, 1380}, {274, 1380}}], BezierCurve[{{360, 1380}, {424, 1347}, {467, 1280}}], BezierCurve[{{509, 1216}, {530, 1127}, {530, 1012}}], Line[{{530, 371}}], BezierCurve[{{530, 256}, {510, 167}, {469, 102}}], BezierCurve[{{424, 34}, {359, 0}, {274, 0}}], BezierCurve[{{188, 0}, {123, 34}, {80, 102}}], BezierCurve[{{39, 164}, {18, 254}, {18, 371}}]}, {Line[{{82, 371}}], BezierCurve[{{82, 166}, {146, 64}, {274, 63}}], BezierCurve[{{401, 63}, {464, 166}, {465, 371}}], Line[{{465, 1012}}], BezierCurve[{{465, 1215}, {401, 1317}, {274, 1317}}], BezierCurve[{{145, 1317}, {81, 1215}, {82, 1012}}], Line[{{82, 371}}]}}]},
  "1" -> {397, FilledCurve[{{Line[{{16, 1108}}], Line[{{225, 1380}}], Line[{{289, 1380}}], Line[{{289, 0}}], Line[{{223, 0}}], Line[{{223, 1270}}], Line[{{66, 1067}}], Line[{{16, 1108}}]}}]},
  "2" -> {655, FilledCurve[{{Line[{{25, 1010}}], Line[{{25, 1042}}], BezierCurve[{{25, 1150}, {46, 1233}, {88, 1292}}], BezierCurve[{{131, 1351}, {195, 1380}, {281, 1380}}], BezierCurve[{{363, 1380}, {428, 1347}, {475, 1282}}], BezierCurve[{{516, 1222}, {536, 1145}, {537, 1051}}], BezierCurve[{{537, 865}, {459, 702}, {303, 563}}], BezierCurve[{{230, 499}, {177, 427}, {144, 348}}], BezierCurve[{{111, 269}, {93, 174}, {90, 63}}], Line[{{281, 63}}], Line[{{504, 63}}], Line[{{537, 63}}], Line[{{537, 0}}], Line[{{504, 0}}], Line[{{281, 0}}], Line[{{25, 0}}], Line[{{25, 33}}], BezierCurve[{{25, 94}, {29, 150}, {37, 200}}], BezierCurve[{{45, 251}, {57, 298}, {74, 342}}], BezierCurve[{{109, 442}, {171, 532}, {260, 612}}], BezierCurve[{{401, 741}, {471, 887}, {471, 1051}}], BezierCurve[{{471, 1133}, {455, 1198}, {422, 1246}}], BezierCurve[{{389, 1293}, {342, 1317}, {281, 1317}}], BezierCurve[{{217, 1317}, {170, 1296}, {139, 1253}}], BezierCurve[{{106, 1208}, {90, 1137}, {90, 1042}}], Line[{{90, 1010}}], Line[{{25, 1010}}]}}]},
  "3" -> {620, FilledCurve[{{Line[{{20, 365}}], Line[{{20, 397}}], Line[{{86, 397}}], Line[{{86, 365}}], BezierCurve[{{86, 260}, {103, 183}, {137, 135}}], BezierCurve[{{168, 87}, {215, 63}, {276, 63}}], BezierCurve[{{335, 63}, {381, 90}, {416, 143}}], BezierCurve[{{434, 169}, {447, 200}, {455, 236}}], BezierCurve[{{463, 273}, {467, 313}, {467, 356}}], BezierCurve[{{467, 417}, {458, 466}, {438, 504}}], BezierCurve[{{420, 540}, {400, 569}, {379, 588}}], BezierCurve[{{352, 614}, {324, 632}, {296, 642}}], BezierCurve[{{268, 652}, {244, 657}, {223, 657}}], Line[{{164, 657}}], Line[{{164, 721}}], Line[{{223, 721}}], BezierCurve[{{248, 721}, {274, 727}, {303, 740}}], BezierCurve[{{332, 752}, {358, 770}, {382, 794}}], BezierCurve[{{406, 819}, {426, 850}, {442, 888}}], BezierCurve[{{459, 925}, {467, 970}, {467, 1022}}], BezierCurve[{{467, 1066}, {463, 1106}, {454, 1142}}], BezierCurve[{{446, 1178}, {433, 1210}, {416, 1237}}], BezierCurve[{{383, 1290}, {337, 1317}, {276, 1317}}], BezierCurve[{{149, 1317}, {86, 1216}, {86, 1014}}], Line[{{86, 981}}], Line[{{20, 981}}], Line[{{20, 1014}}], BezierCurve[{{20, 1130}, {41, 1219}, {82, 1280}}], BezierCurve[{{127, 1347}, {192, 1380}, {276, 1380}}], BezierCurve[{{361, 1380}, {426, 1343}, {471, 1270}}], BezierCurve[{{512, 1209}, {532, 1126}, {532, 1022}}], BezierCurve[{{532, 951}, {520, 893}, {496, 848}}], BezierCurve[{{473, 803}, {449, 768}, {424, 745}}], BezierCurve[{{399, 720}, {373, 701}, {346, 688}}], BezierCurve[{{376, 673}, {402, 654}, {424, 633}}], BezierCurve[{{449, 610}, {473, 576}, {496, 532}}], BezierCurve[{{520, 487}, {532, 429}, {532, 356}}], BezierCurve[{{532, 247}, {509, 160}, {462, 96}}], BezierCurve[{{416, 32}, {354, 0}, {276, 0}}], BezierCurve[{{191, 0}, {127, 33}, {82, 98}}], BezierCurve[{{41, 160}, {20, 249}, {20, 365}}]}}]},
  "4" -> {663, FilledCurve[{{Line[{{20, 422}}], Line[{{20, 483}}], Line[{{506, 1380}}], Line[{{567, 1380}}], Line[{{567, 0}}], Line[{{502, 0}}], Line[{{502, 422}}], Line[{{20, 422}}]}, {Line[{{96, 485}}], Line[{{502, 485}}], Line[{{502, 1235}}], Line[{{96, 485}}]}}]},
  "5" -> {634, FilledCurve[{{Line[{{18, 360}}], Line[{{18, 383}}], Line[{{84, 383}}], Line[{{84, 360}}], BezierCurve[{{84, 256}, {101, 180}, {135, 133}}], BezierCurve[{{165, 86}, {211, 63}, {274, 63}}], BezierCurve[{{334, 63}, {381, 91}, {414, 147}}], BezierCurve[{{432, 174}, {445, 206}, {453, 242}}], BezierCurve[{{461, 279}, {465, 318}, {465, 360}}], Line[{{465, 506}}], BezierCurve[{{465, 607}, {449, 682}, {416, 729}}], BezierCurve[{{386, 773}, {339, 795}, {274, 795}}], BezierCurve[{{214, 795}, {168, 769}, {135, 717}}], BezierCurve[{{101, 664}, {84, 593}, {84, 506}}], Line[{{84, 498}}], Line[{{84, 494}}], Line[{{18, 494}}], Line[{{18, 498}}], Line[{{18, 506}}], Line[{{18, 1380}}], Line[{{84, 1380}}], Line[{{530, 1380}}], Line[{{530, 1317}}], Line[{{84, 1317}}], Line[{{84, 756}}], BezierCurve[{{131, 825}, {194, 860}, {274, 860}}], BezierCurve[{{357, 860}, {422, 829}, {469, 766}}], BezierCurve[{{510, 706}, {530, 619}, {530, 506}}], Line[{{530, 360}}], BezierCurve[{{530, 312}, {525, 267}, {514, 224}}], BezierCurve[{{503, 181}, {488, 144}, {469, 113}}], BezierCurve[{{422, 38}, {357, 0}, {274, 0}}], BezierCurve[{{189, 0}, {125, 32}, {82, 96}}], BezierCurve[{{39, 159}, {18, 247}, {18, 360}}]}}]},
  "6" -> {638, FilledCurve[{{Line[{{18, 324}}], Line[{{18, 518}}], Line[{{18, 520}}], Line[{{18, 928}}], BezierCurve[{{18, 1026}, {27, 1105}, {45, 1165}}], BezierCurve[{{63, 1225}, {85, 1271}, {111, 1302}}], BezierCurve[{{137, 1334}, {165, 1355}, {195, 1365}}], BezierCurve[{{225, 1375}, {251, 1380}, {274, 1380}}], BezierCurve[{{359, 1380}, {423, 1350}, {467, 1290}}], BezierCurve[{{509, 1235}, {530, 1160}, {530, 1065}}], Line[{{530, 1034}}], Line[{{465, 1034}}], Line[{{465, 1065}}], BezierCurve[{{465, 1146}, {449, 1208}, {416, 1252}}], BezierCurve[{{383, 1295}, {336, 1317}, {274, 1317}}], BezierCurve[{{233, 1317}, {200, 1304}, {175, 1277}}], BezierCurve[{{150, 1250}, {130, 1217}, {116, 1178}}], BezierCurve[{{103, 1138}, {94, 1096}, {90, 1050}}], BezierCurve[{{86, 1006}, {84, 965}, {84, 928}}], Line[{{84, 774}}], BezierCurve[{{131, 843}, {194, 878}, {274, 879}}], BezierCurve[{{361, 879}, {426, 847}, {469, 784}}], BezierCurve[{{510, 723}, {530, 634}, {530, 518}}], Line[{{530, 369}}], BezierCurve[{{530, 320}, {525, 273}, {514, 230}}], BezierCurve[{{503, 186}, {488, 148}, {469, 115}}], BezierCurve[{{424, 38}, {359, 0}, {274, 0}}], BezierCurve[{{193, 0}, {130, 28}, {86, 85}}], BezierCurve[{{40, 142}, {18, 221}, {18, 324}}]}, {Line[{{84, 324}}], BezierCurve[{{84, 299}, {86, 272}, {90, 242}}], BezierCurve[{{94, 212}, {103, 184}, {116, 157}}], BezierCurve[{{130, 130}, {150, 108}, {175, 90}}], BezierCurve[{{200, 72}, {233, 63}, {274, 63}}], BezierCurve[{{334, 63}, {381, 92}, {414, 150}}], BezierCurve[{{432, 177}, {445, 210}, {453, 247}}], BezierCurve[{{461, 284}, {465, 325}, {465, 369}}], Line[{{465, 518}}], BezierCurve[{{465, 622}, {449, 699}, {416, 748}}], BezierCurve[{{385, 793}, {337, 816}, {274, 815}}], BezierCurve[{{214, 815}, {168, 788}, {135, 733}}], BezierCurve[{{101, 680}, {84, 609}, {84, 520}}], Line[{{84, 324}}]}}]},
  "7" -> {770, FilledCurve[{{Line[{{12, 1317}}], Line[{{12, 1380}}], Line[{{694, 1380}}], Line[{{694, 1319}}], Line[{{143, 0}}], Line[{{74, 0}}], Line[{{625, 1317}}], Line[{{12, 1317}}]}}]},
  "8" -> {679, FilledCurve[{{Line[{{80, 92}}], BezierCurve[{{36, 152}, {14, 242}, {14, 360}}], BezierCurve[{{14, 480}, {36, 569}, {80, 629}}], BezierCurve[{{100, 656}, {123, 677}, {150, 690}}], BezierCurve[{{123, 705}, {99, 726}, {80, 752}}], BezierCurve[{{36, 812}, {14, 901}, {14, 1020}}], BezierCurve[{{14, 1139}, {36, 1228}, {80, 1288}}], BezierCurve[{{125, 1349}, {196, 1380}, {293, 1380}}], BezierCurve[{{388, 1380}, {459, 1349}, {504, 1288}}], BezierCurve[{{548, 1231}, {570, 1141}, {570, 1020}}], BezierCurve[{{569, 899}, {547, 809}, {504, 752}}], BezierCurve[{{485, 726}, {461, 705}, {434, 690}}], BezierCurve[{{461, 677}, {485, 656}, {504, 629}}], BezierCurve[{{548, 572}, {570, 482}, {570, 360}}], BezierCurve[{{569, 239}, {547, 149}, {504, 92}}], BezierCurve[{{459, 31}, {388, 0}, {293, 0}}], BezierCurve[{{196, 0}, {125, 31}, {80, 92}}]}, {Line[{{131, 592}}], BezierCurve[{{97, 544}, {80, 467}, {80, 360}}], BezierCurve[{{80, 254}, {97, 178}, {131, 131}}], BezierCurve[{{165, 86}, {219, 63}, {292, 63}}], BezierCurve[{{366, 63}, {420, 86}, {453, 131}}], BezierCurve[{{487, 178}, {504, 254}, {504, 360}}], BezierCurve[{{504, 467}, {487, 544}, {453, 592}}], BezierCurve[{{420, 637}, {367, 660}, {293, 660}}], BezierCurve[{{219, 659}, {165, 637}, {131, 592}}]}, {Line[{{131, 1249}}], BezierCurve[{{97, 1203}, {80, 1127}, {80, 1020}}], BezierCurve[{{80, 913}, {97, 837}, {131, 791}}], BezierCurve[{{165, 746}, {219, 723}, {292, 723}}], BezierCurve[{{366, 723}, {420, 746}, {453, 791}}], BezierCurve[{{487, 838}, {504, 914}, {504, 1020}}], BezierCurve[{{504, 1127}, {487, 1203}, {453, 1249}}], BezierCurve[{{420, 1294}, {367, 1317}, {293, 1317}}], BezierCurve[{{219, 1317}, {165, 1294}, {131, 1249}}]}}]},
  "9" -> {630, FilledCurve[{{Line[{{16, 334}}], Line[{{16, 367}}], Line[{{82, 367}}], Line[{{82, 334}}], BezierCurve[{{82, 245}, {98, 178}, {131, 131}}], BezierCurve[{{164, 86}, {211, 63}, {272, 63}}], BezierCurve[{{311, 63}, {344, 75}, {368, 99}}], BezierCurve[{{394, 123}, {413, 154}, {428, 192}}], BezierCurve[{{442, 229}, {451, 271}, {456, 316}}], BezierCurve[{{461, 362}, {463, 407}, {463, 451}}], Line[{{463, 606}}], BezierCurve[{{441, 572}, {414, 546}, {382, 528}}], BezierCurve[{{350, 509}, {313, 500}, {272, 500}}], BezierCurve[{{187, 500}, {123, 532}, {80, 596}}], BezierCurve[{{38, 656}, {17, 745}, {16, 864}}], Line[{{16, 1016}}], BezierCurve[{{16, 1127}, {39, 1215}, {86, 1281}}], BezierCurve[{{133, 1347}, {195, 1380}, {272, 1380}}], BezierCurve[{{357, 1380}, {421, 1351}, {465, 1292}}], BezierCurve[{{507, 1237}, {528, 1160}, {528, 1061}}], Line[{{528, 864}}], Line[{{528, 862}}], Line[{{528, 451}}], BezierCurve[{{528, 310}, {508, 202}, {469, 125}}], BezierCurve[{{425, 42}, {359, 0}, {272, 0}}], BezierCurve[{{187, 0}, {123, 31}, {80, 94}}], BezierCurve[{{37, 153}, {16, 233}, {16, 334}}]}, {Line[{{82, 864}}], BezierCurve[{{82, 757}, {99, 680}, {133, 633}}], BezierCurve[{{163, 588}, {209, 565}, {272, 565}}], BezierCurve[{{332, 565}, {379, 592}, {412, 647}}], BezierCurve[{{429, 673}, {441, 704}, {450, 742}}], BezierCurve[{{459, 778}, {463, 819}, {463, 862}}], Line[{{463, 1061}}], BezierCurve[{{463, 1146}, {447, 1210}, {414, 1253}}], BezierCurve[{{383, 1295}, {335, 1316}, {272, 1317}}], BezierCurve[{{213, 1317}, {167, 1290}, {133, 1235}}], BezierCurve[{{99, 1179}, {82, 1106}, {82, 1016}}], Line[{{82, 864}}]}}]}|>;

ostrichCap = 1380;

(* str set in Ostrich Sans with digit height h, its ink box centred at the origin *)
numeral[str_, h_] := Module[{ds = Characters[str], xs, g, c},
  xs = Most@FoldList[Plus, 0, ostrichDigits[#][[1]] & /@ ds];
  g = MapThread[ostrichDigits[#1][[2]] /. {x_Integer, y_Integer} :> {x + #2, y} &, {ds, xs}];
  c = Mean /@ CoordinateBounds[Cases[g, {_?NumericQ, _?NumericQ}, Infinity]];
  g /. {x_?NumericQ, y_?NumericQ} :> h ({x, y} - c)/ostrichCap];

(* numeral centred at p with height h, baseline along dir *)
(* numeral centred at p with height h, baseline along dir. bold > 0 thickens
   the strokes by tracing the outline with an ink edge of width bold h; the glyph
   shrinks to keep its overall height h. (Ostrich Sans Medium strokes are about
   0.046 h.) *)
label[str_, p_, h_, dir_ : {1, 0}, bold_ : 0] := With[{m = RotationMatrix[{{1, 0}, dir}]},
  {ink, If[bold > 0, EdgeForm[{ink, JoinForm["Round"], Thickness[bold h/(2 faceHalf)]}], {}],
   numeral[str, (1 - bold) h] /. {x_?NumericQ, y_?NumericQ} :> p + m . {x, y}}];

(* points along the outline of a numeral of height 1 *)
outlinePoints[str_] := outlinePoints[str] = Flatten[
  Cases[numeral[str, 1], FilledCurve[cs_] :> Map[Module[{cur, out = {}},
    Do[Switch[Head[s],
      Line, out = Join[out, s[[1]]],
      BezierCurve, With[{p0 = cur, p1 = s[[1, 1]], p2 = s[[1, 2]], p3 = s[[1, 3]]},
        out = Join[out, Table[(1 - t)^3 p0 + 3 (1 - t)^2 t p1 + 3 (1 - t) t^2 p2 + t^3 p3, {t, 0.1, 1, 0.1}]]]];
      cur = Last[s[[1]]], {s, #}]; out] &, cs], Infinity], 2];

(* Largest upright numeral (height at most ro - ri) for clock angle a that fits
   in the ring ri <= |p| <= ro. Gives {height, centre radius}; the centre is in
   the middle of the radii that fit, so the numeral sits evenly in the ring. *)
ringFit[str_, a_, ri_, ro_] := Module[{pts = outlinePoints[str], u = clockDir[a], rhos, ok, lo, hi, mid},
  rhos = Subdivide[ri, ro, 200];
  ok[h_] := Select[rhos, Function[rho, With[{n = Norm /@ (h pts + ConstantArray[rho u, Length[pts]])},
    Min[n] >= ri && Max[n] <= ro]]];
  If[ok[ro - ri] =!= {}, {ro - ri, Mean[MinMax[ok[ro - ri]]]},
    {lo, hi} = {0.5 (ro - ri), ro - ri};
    Do[mid = (lo + hi)/2; If[ok[mid] === {}, hi = mid, lo = mid], {12}];
    {lo, Mean[MinMax[ok[lo]]]}]];

(* ---------- satellites: drawn upright, centred at the origin, radius r ----------
   Each satellite is translated to its orbit position but never rotated: the
   planet gear cancels the carrier rotation, so only the hand inside turns. *)

(* Hour dial: numerals on an outer ring, hand on the inner disc. 12 is replaced
   by the OpenMoji "backhand index pointing left" emoji (U+1F448). *)

(* OpenMoji 1F448, black line version (https://openmoji.org/library/emoji-1F448/),
   by the OpenMoji project, licensed CC BY-SA 4.0. Converted from its SVG paths
   into primitives: y flipped, centred on the drawing, in SVG units (the
   drawing is 56.2 units wide, drawn with a 2-unit stroke). *)
openmojiPointLeft = {
  BezierCurve[{{11.647, -19.62}, {20.736, -19.62}, {28.103, -12.253}, {28.103, -3.165}}],
  BezierCurve[{{28.103, -3.165}, {28.103, 5.924}, {20.736, 13.291}, {11.647, 13.291}}],
  Line[{{10.774, -19.62}, {-1.226, -19.62}}],
  BezierCurve[{{-1.52, -12.628}, {-3.451, -12.709}, {-4.951, -14.34}, {-4.87, -16.272}}],
  BezierCurve[{{-4.87, -16.272}, {-4.789, -18.203}, {-3.158, -19.703}, {-1.226, -19.622}}],
  BezierCurve[{{-1.753, -5.631}, {-3.684, -5.712}, {-5.184, -7.343}, {-5.103, -9.275}}],
  BezierCurve[{{-5.103, -9.275}, {-5.022, -11.206}, {-3.39, -12.706}, {-1.459, -12.625}}],
  BezierCurve[{{-1.886, -5.635}, {-3.813, -5.79}, {-5.249, -7.478}, {-5.094, -9.405}}],
  BezierCurve[{{-5.094, -9.405}, {-4.939, -11.331}, {-3.252, -12.768}, {-1.325, -12.613}}],
  BezierCurve[{{-2.231, 1.406}, {-4.16, 1.283}, {-5.623, -0.381}, {-5.5, -2.31}}],
  BezierCurve[{{-5.5, -2.31}, {-5.376, -4.239}, {-3.712, -5.703}, {-1.783, -5.579}}],
  BezierCurve[{{-0.007, 18.36}, {-1.398, 19.703}, {-3.613, 19.665}, {-4.956, 18.274}}],
  BezierCurve[{{-4.956, 18.274}, {-6.299, 16.884}, {-6.261, 14.668}, {-4.87, 13.325}}],
  BezierCurve[{{-24.603, 8.406}, {-26.536, 8.406}, {-28.103, 6.839}, {-28.103, 4.906}}],
  BezierCurve[{{-28.103, 4.906}, {-28.103, 2.973}, {-26.536, 1.406}, {-24.603, 1.406}}],
  Line[{{-2.268, 1.406}, {-24.603, 1.406}}],
  Line[{{-24.603, 8.406}, {3.187, 8.406}}],
  Line[{{-2.656, 11.367}, {-4.87, 13.325}}],
  BezierCurve[{{2.312, 16.593}, {4.456, 14.64}, {7.48, 13.219}, {11.834, 13.291}}],
  Line[{{-0.007, 18.36}, {2.373, 16.539}}]};

(* The emoji centred at c, w wide; the stroke keeps OpenMoji's proportions.
   Coordinates are mapped directly because a Scale transform would also thin
   the stroke. *)
handEmoji[c_, w_] := With[{k = w/56.2},
  {ink, Thickness[2 k/(2 faceHalf)], CapForm["Round"], JoinForm["Round"],
   openmojiPointLeft /. {x_?NumericQ, y_?NumericQ} :> c + k {x, y}}];

(* Upright numerals filling the ring between the two circles: each is as large
   as fits, up to the full ring width. Computed once per dial radius. *)
hourNumerals[r_] := hourNumerals[r] =
  With[{ri = 0.70 r + lineHalf[0.0016], ro = r - lineHalf[0.0016]},
    Table[With[{a = 2 Pi k/12}, With[{f = ringFit[ToString[k], a, ri, ro]},
      label[ToString[k], f[[2]] clockDir[a], f[[1]]]]], {k, 1, 11}]];

hourDial[h_, m_, r_] := {
  {planet, Disk[{0, 0}, r]},
  {ink, thin, Circle[{0, 0}, r], Circle[{0, 0}, 0.70 r]},
  hourNumerals[r],
  handEmoji[0.82 r clockDir[0], 0.30 r],
  {ink, taperedHand[clockDir[2 Pi (Mod[h, 12] + m/60)/12], -0.04 r, 0.62 r, 0.032 r, 0.016 r]}};

(* Seconds dial: 12 ticks between two rings, the 12 o'clock tick in orange. *)
secondsDial[s_, r_] := {
  {planet, Disk[{0, 0}, r]},
  {ink, thin, Circle[{0, 0}, r], Circle[{0, 0}, 0.64 r]},
  {CapForm["Round"], Thickness[0.0042],
    Table[{If[k == 0, accent, ink],
      Line[{0.74 r clockDir[2 Pi k/12], 0.90 r clockDir[2 Pi k/12]}]}, {k, 0, 11}]},
  {ink, taperedHand[clockDir[2 Pi s/60], -0.06 r, 0.58 r, 0.06 r, 0.028 r]}};

(* Day-of-week dial: 7 arcs = Monday ... Sunday clockwise from 12, weekend in
   orange; the hand sweeps through the current day's arc over the day. *)
dayDial[wd_, frac_, r_] := With[{gap = 0.11},
  {
   {planet, Disk[{0, 0}, r]},
   {ink, thin, Circle[{0, 0}, r]},
   {CapForm["Butt"], Thickness[0.0062],
     Table[{If[k >= 5, accent, ink],
       (* Circle angles are counter-clockwise from 3 o'clock *)
       Circle[{0, 0}, 0.78 r, {Pi/2 - 2 Pi (k + 1)/7 + gap, Pi/2 - 2 Pi k/7 - gap}]}, {k, 0, 6}]},
   {ink, taperedHand[clockDir[2 Pi (wd - 1 + frac)/7], -0.06 r, 0.60 r, 0.065 r, 0.03 r]}}];

satellite[g_, pos_, rot_] := Translate[g, clockwise[pos, rot]];

(* ---------- fixed outer minute ring ----------
   Ticks run inward from radius tickOut. Each numeral replaces a 10-minute tick
   and spans exactly the minute ticks beside it: from the inner to the outer
   end of their round caps, measured along the numeral's axis (the ticks sit
   one minute = 6 degrees off it). The numerals are emboldened to about the
   weight of the ticks. *)
minuteRing = Module[{tickOut = 0.95, short = 0.055, long = 0.085, tw = 0.0052, ticks, labels, rIn, rOut},
  ticks = Table[
    If[Mod[k, 10] == 0, Nothing,
      With[{a = 2 Pi k/60, l = If[Mod[k, 5] == 0, long, short]},
        Line[{tickOut clockDir[a], (tickOut - l) clockDir[a]}]]],
    {k, 0, 59}];
  rOut = Cos[2 Pi/60] (tickOut + lineHalf[tw]); rIn = Cos[2 Pi/60] (tickOut - short - lineHalf[tw]);
  labels = Table[
    With[{a = 2 Pi k/60},
      label[ToString[If[k == 0, 60, k]], (rIn + rOut)/2 clockDir[a], rOut - rIn, {Cos[a], -Sin[a]}, 0.08]],
    {k, 0, 50, 10}];
  {ink, CapForm["Round"], Thickness[tw], ticks, labels}];

(* black line separating the fixed minute ring from the rotating disc *)
ringBoundary = {Black, AbsoluteThickness[1.4], Circle[{0, 0}, 0.845]};

(* flat case ring *)
caseRing = {rim, Disk[{0, 0}, 1.1]};

(* ---------- the face ---------- *)
ressenceFace[d_DateObject] := ressenceFace[{
  DateValue[d, "Hour"], DateValue[d, "Minute"], DateValue[d, "Second"],
  DateValue[d, "ISOWeekDay"]}];

ressenceFace[{h_, m_, s_, wd_}, opts : OptionsPattern[Graphics]] := Module[
  {alpha, rot, dir},
  alpha = 2 Pi (m + s/60)/60;          (* carrier / minute angle, clockwise from 12 *)
  rot = alpha - alpha0;                 (* carrier rotation relative to photo pose *)
  dir = {Sin[alpha], Cos[alpha]};
  Graphics[{
    caseRing,
    {disc, Disk[{0, 0}, 1.0]},
    minuteRing,
    ringBoundary,
    (* planets riding on the carrier *)
    satellite[hourDial[h, m + s/60, hourR], hourPos, rot],
    satellite[secondsDial[s, secR], secPos, rot],
    satellite[dayDial[wd, (h + m/60)/24, dayR], dayPos, rot],
    (* minute marker printed on the carrier *)
    {ink, taperedHand[dir, 0.18, 0.79, 0.022, 0.010]}
    },
    opts, PlotRange -> faceHalf, ImageSize -> 420,
    Background -> None]
  ];

(* ---------- live watch ---------- *)
ressenceWatch[] := DynamicModule[{warp = 1, t0 = AbsoluteTime[], base = AbsoluteTime[]},
  Panel[Column[{
    Dynamic[
      Refresh[
        Module[{t = base + warp (AbsoluteTime[] - t0)},
          ressenceFace[DateObject[t]]],
        UpdateInterval -> 0.1, TrackedSymbols :> {}]],
    Row[{"Time warp ",
      Slider[Dynamic[warp, (base = base + warp (AbsoluteTime[] - t0); t0 = AbsoluteTime[]; warp = #) &],
        {1, 600}, ImageSize -> 220],
      " ", Dynamic[Round[warp]], "\[Times]"}],
    Button["Reset to now", (base = AbsoluteTime[]; t0 = AbsoluteTime[]; warp = 1)]
  }, Alignment -> Center], Background -> Transparent],
  SaveDefinitions -> True];

End[];
EndPackage[];
