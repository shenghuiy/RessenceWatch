(* ::Package:: *)

(* Ressence Type 1 style watch face drawn with graphics primitives; numerals are
   Varela Round glyph outlines.

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
     RessenceWatchFace[]                       static face for now
     RessenceWatchFace[DateObject[...]]        static face for that date and time
     RessenceWatch[]                      live, animated watch (needs front end)
*)

BeginPackage["RessenceWatch`"];

RessenceWatchFace::usage = "RessenceWatchFace[date] draws a Ressence Type 1 style face for a DateObject. RessenceWatchFace[] uses the current time.";
RessenceWatch::usage = "RessenceWatch[] gives a live, dynamically updating ROCS watch with a time-warp control.";

Begin["`Private`"];

(* ---------- palette ----------
   LightDarkSwitched[light, dark]: a light-mode notebook shows the black-dial
   Type 1 with white markings and green lume; dark mode shows the pale dial. *)
ink    = LightDarkSwitched[GrayLevel[0.95], GrayLevel[0.17]];
disc   = LightDarkSwitched[GrayLevel[0.04], GrayLevel[0.925]];
planet = LightDarkSwitched[GrayLevel[0.07], GrayLevel[0.915]];
accent = LightDarkSwitched[RGBColor[0.45, 0.85, 0.30], RGBColor[0.96, 0.55, 0.12]];
rim    = LightDarkSwitched[GrayLevel[0.12], GrayLevel[0.72]];
line   = LightDarkSwitched[GrayLevel[0.35], Black];                      (* ring boundary *)
marker = LightDarkSwitched[RGBColor[0.45, 0.85, 0.30], GrayLevel[0.17]];  (* minute marker *)

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
thinW = 0.001632;   (* ring boundary lines *)
thin  = Thickness[thinW];
lineHalf[t_] := t faceHalf;   (* half the width of a Thickness[t] line, in plot units *)

(* ---------- numerals ----------
   Numerals are drawn as filled glyph outlines (FilledCurve with Bezier
   segments) instead of Text: the font rasterizer pixel-snaps small or rotated
   text, which looks jagged next to the vector hands.

   They use Varela Round (by Joe Prince, SIL Open Font License 1.1). Its
   outlines are stored below, so the font need not be installed. They were read
   from the glyf table of VarelaRound-Regular.ttf (google/fonts), the quadratic
   contours converted to cubic Beziers and the glyphs set at their advance
   widths, then normalised to ink height 1 with the ink box centred at the
   origin, and are stored in units of 1/10000. Only the strings the dials use
   are included. *)
fontOutlines = <|
  "3" -> {FilledCurve[{{Line[{{-167, -5000}}], BezierCurve[{{-789, -5000}, {-1344, -4942}, {-1831, -4826}}], BezierCurve[{{-2319, -4710}, {-2734, -4582}, {-3078, -4443}}], BezierCurve[{{-3180, -4396}, {-3266, -4329}, {-3336, -4241}}], BezierCurve[{{-3405, -4153}, {-3440, -4044}, {-3440, -3914}}], BezierCurve[{{-3440, -3765}, {-3389, -3630}, {-3287, -3510}}], BezierCurve[{{-3185, -3389}, {-3050, -3329}, {-2883, -3329}}], BezierCurve[{{-2799, -3329}, {-2711, -3347}, {-2618, -3384}}], BezierCurve[{{-2340, -3496}, {-2015, -3591}, {-1643, -3670}}], BezierCurve[{{-1272, -3749}, {-859, -3788}, {-404, -3788}}], BezierCurve[{{5, -3788}, {406, -3744}, {801, -3656}}], BezierCurve[{{1195, -3568}, {1523, -3412}, {1783, -3189}}], BezierCurve[{{2043, -2967}, {2173, -2651}, {2173, -2242}}], BezierCurve[{{2173, -1899}, {2075, -1595}, {1880, -1330}}], BezierCurve[{{1685, -1065}, {1365, -859}, {919, -710}}], BezierCurve[{{474, -562}, {-135, -487}, {-905, -487}}], Line[{{-1128, -487}}], BezierCurve[{{-1286, -487}, {-1423, -429}, {-1539, -313}}], BezierCurve[{{-1655, -197}, {-1713, -60}, {-1713, 97}}], BezierCurve[{{-1713, 255}, {-1655, 392}, {-1539, 508}}], BezierCurve[{{-1423, 624}, {-1286, 682}, {-1128, 682}}], Line[{{-1045, 682}}], BezierCurve[{{-135, 682}, {571, 822}, {1072, 1100}}], BezierCurve[{{1574, 1379}, {1825, 1801}, {1825, 2368}}], BezierCurve[{{1825, 2804}, {1643, 3155}, {1281, 3419}}], BezierCurve[{{919, 3684}, {367, 3816}, {-376, 3816}}], BezierCurve[{{-812, 3816}, {-1209, 3772}, {-1567, 3684}}], BezierCurve[{{-1924, 3596}, {-2247, 3487}, {-2535, 3357}}], BezierCurve[{{-2572, 3347}, {-2609, 3336}, {-2646, 3322}}], BezierCurve[{{-2683, 3308}, {-2730, 3301}, {-2786, 3301}}], BezierCurve[{{-2925, 3301}, {-3045, 3350}, {-3148, 3447}}], BezierCurve[{{-3250, 3545}, {-3301, 3668}, {-3301, 3816}}], BezierCurve[{{-3301, 4030}, {-3208, 4188}, {-3022, 4290}}], BezierCurve[{{-2697, 4466}, {-2303, 4629}, {-1838, 4777}}], BezierCurve[{{-1374, 4926}, {-850, 5000}, {-265, 5000}}], BezierCurve[{{812, 5000}, {1641, 4782}, {2221, 4345}}], BezierCurve[{{2802, 3909}, {3092, 3292}, {3092, 2493}}], BezierCurve[{{3092, 2029}, {3006, 1641}, {2834, 1330}}], BezierCurve[{{2662, 1019}, {2449, 773}, {2194, 592}}], BezierCurve[{{1938, 411}, {1676, 269}, {1407, 167}}], BezierCurve[{{1602, 121}, {1815, 46}, {2047, -56}}], BezierCurve[{{2279, -158}, {2502, -304}, {2716, -494}}], BezierCurve[{{2929, -685}, {3104, -929}, {3238, -1226}}], BezierCurve[{{3373, -1523}, {3440, -1885}, {3440, -2312}}], BezierCurve[{{3440, -2906}, {3280, -3401}, {2960, -3795}}], BezierCurve[{{2639, -4190}, {2208, -4489}, {1664, -4694}}], BezierCurve[{{1121, -4898}, {511, -5000}, {-167, -5000}}]}}]},
  "6" -> {FilledCurve[{{Line[{{188, -5000}}], BezierCurve[{{-499, -5000}, {-1072, -4891}, {-1532, -4673}}], BezierCurve[{{-1992, -4455}, {-2361, -4162}, {-2639, -3795}}], BezierCurve[{{-2918, -3429}, {-3129, -3027}, {-3273, -2591}}], BezierCurve[{{-3417, -2154}, {-3512, -1711}, {-3558, -1260}}], BezierCurve[{{-3605, -810}, {-3628, -395}, {-3628, -14}}], BezierCurve[{{-3628, 534}, {-3561, 1098}, {-3426, 1678}}], BezierCurve[{{-3292, 2259}, {-3069, 2802}, {-2758, 3308}}], BezierCurve[{{-2447, 3814}, {-2031, 4222}, {-1511, 4533}}], BezierCurve[{{-991, 4844}, {-351, 5000}, {411, 5000}}], BezierCurve[{{987, 5000}, {1472, 4937}, {1866, 4812}}], BezierCurve[{{2261, 4687}, {2597, 4550}, {2876, 4401}}], BezierCurve[{{3071, 4308}, {3169, 4141}, {3169, 3900}}], BezierCurve[{{3169, 3751}, {3117, 3619}, {3015, 3503}}], BezierCurve[{{2913, 3387}, {2774, 3329}, {2597, 3329}}], BezierCurve[{{2514, 3329}, {2435, 3343}, {2361, 3370}}], BezierCurve[{{2129, 3482}, {1857, 3577}, {1546, 3656}}], BezierCurve[{{1235, 3735}, {889, 3774}, {508, 3774}}], BezierCurve[{{-86, 3774}, {-571, 3651}, {-947, 3405}}], BezierCurve[{{-1323, 3159}, {-1616, 2846}, {-1825, 2465}}], BezierCurve[{{-2033, 2084}, {-2180, 1692}, {-2263, 1288}}], BezierCurve[{{-2347, 884}, {-2398, 525}, {-2416, 209}}], BezierCurve[{{-2184, 506}, {-1841, 785}, {-1386, 1045}}], BezierCurve[{{-931, 1305}, {-323, 1435}, {439, 1435}}], BezierCurve[{{1070, 1435}, {1625, 1300}, {2103, 1031}}], BezierCurve[{{2581, 761}, {2955, 383}, {3224, -104}}], BezierCurve[{{3494, -592}, {3628, -1170}, {3628, -1838}}], BezierCurve[{{3628, -2423}, {3489, -2955}, {3210, -3433}}], BezierCurve[{{2932, -3911}, {2537, -4292}, {2026, -4575}}], BezierCurve[{{1516, -4858}, {903, -5000}, {188, -5000}}]}, {Line[{{202, -3816}}], BezierCurve[{{648, -3816}, {1031, -3723}, {1351, -3538}}], BezierCurve[{{1671, -3352}, {1920, -3106}, {2096, -2799}}], BezierCurve[{{2273, -2493}, {2361, -2163}, {2361, -1811}}], BezierCurve[{{2361, -1198}, {2189, -696}, {1845, -306}}], BezierCurve[{{1502, 84}, {982, 279}, {286, 279}}], BezierCurve[{{-262, 279}, {-713, 188}, {-1065, 7}}], BezierCurve[{{-1418, -174}, {-1695, -383}, {-1894, -620}}], BezierCurve[{{-2094, -857}, {-2245, -1068}, {-2347, -1253}}], BezierCurve[{{-2282, -1699}, {-2161, -2117}, {-1985, -2507}}], BezierCurve[{{-1808, -2897}, {-1544, -3213}, {-1191, -3454}}], BezierCurve[{{-838, -3695}, {-374, -3816}, {202, -3816}}]}}]},
  "9" -> {FilledCurve[{{Line[{{-411, -5000}}], BezierCurve[{{-987, -5000}, {-1472, -4937}, {-1866, -4812}}], BezierCurve[{{-2261, -4687}, {-2597, -4550}, {-2876, -4401}}], BezierCurve[{{-3071, -4299}, {-3169, -4132}, {-3169, -3900}}], BezierCurve[{{-3169, -3751}, {-3117, -3619}, {-3015, -3503}}], BezierCurve[{{-2913, -3387}, {-2774, -3329}, {-2597, -3329}}], BezierCurve[{{-2505, -3329}, {-2426, -3343}, {-2361, -3370}}], BezierCurve[{{-2119, -3482}, {-1845, -3577}, {-1539, -3656}}], BezierCurve[{{-1233, -3735}, {-889, -3774}, {-508, -3774}}], BezierCurve[{{95, -3774}, {583, -3651}, {954, -3405}}], BezierCurve[{{1325, -3159}, {1616, -2846}, {1825, -2465}}], BezierCurve[{{2033, -2084}, {2180, -1692}, {2263, -1288}}], BezierCurve[{{2347, -884}, {2398, -525}, {2416, -209}}], BezierCurve[{{2184, -497}, {1843, -773}, {1393, -1038}}], BezierCurve[{{942, -1302}, {332, -1435}, {-439, -1435}}], BezierCurve[{{-1070, -1435}, {-1625, -1300}, {-2103, -1031}}], BezierCurve[{{-2581, -761}, {-2955, -381}, {-3224, 111}}], BezierCurve[{{-3494, 604}, {-3628, 1179}, {-3628, 1838}}], BezierCurve[{{-3628, 2423}, {-3489, 2955}, {-3210, 3433}}], BezierCurve[{{-2932, 3911}, {-2535, 4292}, {-2019, 4575}}], BezierCurve[{{-1504, 4858}, {-894, 5000}, {-188, 5000}}], BezierCurve[{{499, 5000}, {1072, 4891}, {1532, 4673}}], BezierCurve[{{1992, 4455}, {2361, 4162}, {2639, 3795}}], BezierCurve[{{2918, 3429}, {3129, 3027}, {3273, 2591}}], BezierCurve[{{3417, 2154}, {3512, 1711}, {3558, 1260}}], BezierCurve[{{3605, 810}, {3628, 395}, {3628, 14}}], BezierCurve[{{3628, -525}, {3561, -1086}, {3426, -1671}}], BezierCurve[{{3292, -2256}, {3069, -2802}, {2758, -3308}}], BezierCurve[{{2447, -3814}, {2033, -4222}, {1518, -4533}}], BezierCurve[{{1003, -4844}, {360, -5000}, {-411, -5000}}]}, {Line[{{-286, -279}}], BezierCurve[{{262, -279}, {713, -188}, {1065, -7}}], BezierCurve[{{1418, 174}, {1697, 385}, {1901, 627}}], BezierCurve[{{2105, 868}, {2254, 1077}, {2347, 1253}}], BezierCurve[{{2282, 1708}, {2161, 2129}, {1985, 2514}}], BezierCurve[{{1808, 2899}, {1546, 3213}, {1198, 3454}}], BezierCurve[{{850, 3695}, {383, 3816}, {-202, 3816}}], BezierCurve[{{-648, 3816}, {-1031, 3723}, {-1351, 3538}}], BezierCurve[{{-1671, 3352}, {-1920, 3108}, {-2096, 2806}}], BezierCurve[{{-2273, 2505}, {-2361, 2173}, {-2361, 1811}}], BezierCurve[{{-2361, 1198}, {-2189, 696}, {-1845, 306}}], BezierCurve[{{-1502, -84}, {-982, -279}, {-286, -279}}]}}]},
  "10" -> {FilledCurve[{{Line[{{-6873, -4861}}], BezierCurve[{{-7040, -4861}, {-7180, -4805}, {-7291, -4694}}], BezierCurve[{{-7403, -4582}, {-7458, -4443}, {-7458, -4276}}], BezierCurve[{{-7458, -4109}, {-7403, -3967}, {-7291, -3851}}], BezierCurve[{{-7180, -3735}, {-7040, -3677}, {-6873, -3677}}], Line[{{-5216, -3677}}], Line[{{-5216, 3384}}], Line[{{-6636, 2688}}], BezierCurve[{{-6683, 2669}, {-6729, 2651}, {-6776, 2632}}], BezierCurve[{{-6822, 2614}, {-6883, 2604}, {-6957, 2604}}], BezierCurve[{{-7133, 2604}, {-7277, 2660}, {-7389, 2772}}], BezierCurve[{{-7500, 2883}, {-7556, 3027}, {-7556, 3203}}], BezierCurve[{{-7556, 3463}, {-7435, 3654}, {-7194, 3774}}], Line[{{-5299, 4763}}], BezierCurve[{{-5169, 4828}, {-5039, 4861}, {-4909, 4861}}], Line[{{-4617, 4861}}], BezierCurve[{{-4431, 4861}, {-4273, 4796}, {-4143, 4666}}], BezierCurve[{{-4013, 4536}, {-3948, 4378}, {-3948, 4192}}], Line[{{-3948, -3677}}], Line[{{-2570, -3677}}], BezierCurve[{{-2403, -3677}, {-2261, -3735}, {-2145, -3851}}], BezierCurve[{{-2029, -3967}, {-1971, -4109}, {-1971, -4276}}], BezierCurve[{{-1971, -4443}, {-2029, -4582}, {-2145, -4694}}], BezierCurve[{{-2261, -4805}, {-2403, -4861}, {-2570, -4861}}], Line[{{-6873, -4861}}]}, {Line[{{3781, -5000}}], BezierCurve[{{2946, -5000}, {2247, -4791}, {1685, -4373}}], BezierCurve[{{1123, -3955}, {703, -3370}, {425, -2618}}], BezierCurve[{{146, -1866}, {7, -994}, {7, 0}}], BezierCurve[{{7, 994}, {146, 1866}, {425, 2618}}], BezierCurve[{{703, 3370}, {1123, 3955}, {1685, 4373}}], BezierCurve[{{2247, 4791}, {2946, 5000}, {3781, 5000}}], BezierCurve[{{4617, 5000}, {5313, 4791}, {5870, 4373}}], BezierCurve[{{6428, 3955}, {6848, 3370}, {7131, 2618}}], BezierCurve[{{7414, 1866}, {7556, 994}, {7556, 0}}], BezierCurve[{{7556, -994}, {7414, -1866}, {7131, -2618}}], BezierCurve[{{6848, -3370}, {6428, -3955}, {5870, -4373}}], BezierCurve[{{5313, -4791}, {4617, -5000}, {3781, -5000}}]}, {Line[{{3781, -3830}}], BezierCurve[{{4608, -3830}, {5232, -3487}, {5655, -2799}}], BezierCurve[{{6077, -2112}, {6288, -1179}, {6288, 0}}], BezierCurve[{{6288, 1170}, {6077, 2098}, {5655, 2786}}], BezierCurve[{{5232, 3473}, {4608, 3816}, {3781, 3816}}], BezierCurve[{{2964, 3816}, {2342, 3473}, {1915, 2786}}], BezierCurve[{{1488, 2098}, {1274, 1170}, {1274, 0}}], BezierCurve[{{1274, -1179}, {1488, -2112}, {1915, -2799}}], BezierCurve[{{2342, -3487}, {2964, -3830}, {3781, -3830}}]}}]},
  "20" -> {FilledCurve[{{Line[{{-7256, -4861}}], BezierCurve[{{-7470, -4861}, {-7646, -4793}, {-7786, -4659}}], BezierCurve[{{-7925, -4524}, {-7994, -4350}, {-7994, -4136}}], BezierCurve[{{-7994, -3644}, {-7860, -3201}, {-7591, -2806}}], BezierCurve[{{-7321, -2412}, {-6975, -2050}, {-6553, -1720}}], BezierCurve[{{-6130, -1390}, {-5682, -1077}, {-5209, -780}}], BezierCurve[{{-4735, -483}, {-4287, -188}, {-3865, 104}}], BezierCurve[{{-3442, 397}, {-3097, 703}, {-2827, 1024}}], BezierCurve[{{-2558, 1344}, {-2423, 1695}, {-2423, 2075}}], BezierCurve[{{-2423, 2567}, {-2588, 2978}, {-2918, 3308}}], BezierCurve[{{-3247, 3637}, {-3816, 3802}, {-4624, 3802}}], BezierCurve[{{-5097, 3802}, {-5534, 3753}, {-5933, 3656}}], BezierCurve[{{-6332, 3558}, {-6690, 3440}, {-7006, 3301}}], BezierCurve[{{-7015, 3292}, {-7047, 3278}, {-7103, 3259}}], BezierCurve[{{-7159, 3240}, {-7214, 3231}, {-7270, 3231}}], BezierCurve[{{-7419, 3231}, {-7544, 3285}, {-7646, 3391}}], BezierCurve[{{-7748, 3498}, {-7799, 3626}, {-7799, 3774}}], BezierCurve[{{-7799, 4006}, {-7697, 4178}, {-7493, 4290}}], BezierCurve[{{-7103, 4485}, {-6650, 4652}, {-6135, 4791}}], BezierCurve[{{-5620, 4930}, {-5042, 5000}, {-4401, 5000}}], BezierCurve[{{-3807, 5000}, {-3268, 4882}, {-2786, 4645}}], BezierCurve[{{-2303, 4408}, {-1917, 4078}, {-1630, 3656}}], BezierCurve[{{-1342, 3234}, {-1198, 2739}, {-1198, 2173}}], BezierCurve[{{-1198, 1699}, {-1314, 1260}, {-1546, 857}}], BezierCurve[{{-1778, 453}, {-2082, 81}, {-2458, -258}}], BezierCurve[{{-2834, -597}, {-3240, -917}, {-3677, -1219}}], BezierCurve[{{-4113, -1520}, {-4540, -1806}, {-4958, -2075}}], BezierCurve[{{-5376, -2344}, {-5743, -2611}, {-6058, -2876}}], BezierCurve[{{-6374, -3141}, {-6597, -3408}, {-6727, -3677}}], Line[{{-1880, -3677}}], BezierCurve[{{-1713, -3677}, {-1571, -3735}, {-1455, -3851}}], BezierCurve[{{-1339, -3967}, {-1281, -4109}, {-1281, -4276}}], BezierCurve[{{-1281, -4443}, {-1339, -4582}, {-1455, -4694}}], BezierCurve[{{-1571, -4805}, {-1713, -4861}, {-1880, -4861}}], Line[{{-7256, -4861}}]}, {Line[{{4220, -5000}}], BezierCurve[{{3384, -5000}, {2686, -4791}, {2124, -4373}}], BezierCurve[{{1562, -3955}, {1142, -3370}, {864, -2618}}], BezierCurve[{{585, -1866}, {446, -994}, {446, 0}}], BezierCurve[{{446, 994}, {585, 1866}, {864, 2618}}], BezierCurve[{{1142, 3370}, {1562, 3955}, {2124, 4373}}], BezierCurve[{{2686, 4791}, {3384, 5000}, {4220, 5000}}], BezierCurve[{{5056, 5000}, {5752, 4791}, {6309, 4373}}], BezierCurve[{{6866, 3955}, {7286, 3370}, {7570, 2618}}], BezierCurve[{{7853, 1866}, {7994, 994}, {7994, 0}}], BezierCurve[{{7994, -994}, {7853, -1866}, {7570, -2618}}], BezierCurve[{{7286, -3370}, {6866, -3955}, {6309, -4373}}], BezierCurve[{{5752, -4791}, {5056, -5000}, {4220, -5000}}]}, {Line[{{4220, -3830}}], BezierCurve[{{5046, -3830}, {5671, -3487}, {6093, -2799}}], BezierCurve[{{6516, -2112}, {6727, -1179}, {6727, 0}}], BezierCurve[{{6727, 1170}, {6516, 2098}, {6093, 2786}}], BezierCurve[{{5671, 3473}, {5046, 3816}, {4220, 3816}}], BezierCurve[{{3403, 3816}, {2781, 3473}, {2354, 2786}}], BezierCurve[{{1927, 2098}, {1713, 1170}, {1713, 0}}], BezierCurve[{{1713, -1179}, {1927, -2112}, {2354, -2799}}], BezierCurve[{{2781, -3487}, {3403, -3830}, {4220, -3830}}]}}]},
  "30" -> {FilledCurve[{{Line[{{-4763, -5000}}], BezierCurve[{{-5385, -5000}, {-5940, -4942}, {-6428, -4826}}], BezierCurve[{{-6915, -4710}, {-7331, -4582}, {-7674, -4443}}], BezierCurve[{{-7776, -4396}, {-7862, -4329}, {-7932, -4241}}], BezierCurve[{{-8001, -4153}, {-8036, -4044}, {-8036, -3914}}], BezierCurve[{{-8036, -3765}, {-7985, -3630}, {-7883, -3510}}], BezierCurve[{{-7781, -3389}, {-7646, -3329}, {-7479, -3329}}], BezierCurve[{{-7396, -3329}, {-7307, -3347}, {-7214, -3384}}], BezierCurve[{{-6936, -3496}, {-6611, -3591}, {-6240, -3670}}], BezierCurve[{{-5868, -3749}, {-5455, -3788}, {-5000, -3788}}], BezierCurve[{{-4591, -3788}, {-4190, -3744}, {-3795, -3656}}], BezierCurve[{{-3401, -3568}, {-3073, -3412}, {-2813, -3189}}], BezierCurve[{{-2553, -2967}, {-2423, -2651}, {-2423, -2242}}], BezierCurve[{{-2423, -1899}, {-2521, -1595}, {-2716, -1330}}], BezierCurve[{{-2911, -1065}, {-3231, -859}, {-3677, -710}}], BezierCurve[{{-4123, -562}, {-4731, -487}, {-5501, -487}}], Line[{{-5724, -487}}], BezierCurve[{{-5882, -487}, {-6019, -429}, {-6135, -313}}], BezierCurve[{{-6251, -197}, {-6309, -60}, {-6309, 97}}], BezierCurve[{{-6309, 255}, {-6251, 392}, {-6135, 508}}], BezierCurve[{{-6019, 624}, {-5882, 682}, {-5724, 682}}], Line[{{-5641, 682}}], BezierCurve[{{-4731, 682}, {-4025, 822}, {-3524, 1100}}], BezierCurve[{{-3022, 1379}, {-2772, 1801}, {-2772, 2368}}], BezierCurve[{{-2772, 2804}, {-2953, 3155}, {-3315, 3419}}], BezierCurve[{{-3677, 3684}, {-4229, 3816}, {-4972, 3816}}], BezierCurve[{{-5409, 3816}, {-5805, 3772}, {-6163, 3684}}], BezierCurve[{{-6520, 3596}, {-6843, 3487}, {-7131, 3357}}], BezierCurve[{{-7168, 3347}, {-7205, 3336}, {-7242, 3322}}], BezierCurve[{{-7279, 3308}, {-7326, 3301}, {-7382, 3301}}], BezierCurve[{{-7521, 3301}, {-7642, 3350}, {-7744, 3447}}], BezierCurve[{{-7846, 3545}, {-7897, 3668}, {-7897, 3816}}], BezierCurve[{{-7897, 4030}, {-7804, 4188}, {-7618, 4290}}], BezierCurve[{{-7293, 4466}, {-6899, 4629}, {-6435, 4777}}], BezierCurve[{{-5970, 4926}, {-5446, 5000}, {-4861, 5000}}], BezierCurve[{{-3784, 5000}, {-2955, 4782}, {-2375, 4345}}], BezierCurve[{{-1794, 3909}, {-1504, 3292}, {-1504, 2493}}], BezierCurve[{{-1504, 2029}, {-1590, 1641}, {-1762, 1330}}], BezierCurve[{{-1934, 1019}, {-2147, 773}, {-2403, 592}}], BezierCurve[{{-2658, 411}, {-2920, 269}, {-3189, 167}}], BezierCurve[{{-2994, 121}, {-2781, 46}, {-2549, -56}}], BezierCurve[{{-2317, -158}, {-2094, -304}, {-1880, -494}}], BezierCurve[{{-1667, -685}, {-1493, -929}, {-1358, -1226}}], BezierCurve[{{-1223, -1523}, {-1156, -1885}, {-1156, -2312}}], BezierCurve[{{-1156, -2906}, {-1316, -3401}, {-1636, -3795}}], BezierCurve[{{-1957, -4190}, {-2389, -4489}, {-2932, -4694}}], BezierCurve[{{-3475, -4898}, {-4085, -5000}, {-4763, -5000}}]}, {Line[{{4262, -5000}}], BezierCurve[{{3426, -5000}, {2727, -4791}, {2166, -4373}}], BezierCurve[{{1604, -3955}, {1184, -3370}, {905, -2618}}], BezierCurve[{{627, -1866}, {487, -994}, {487, 0}}], BezierCurve[{{487, 994}, {627, 1866}, {905, 2618}}], BezierCurve[{{1184, 3370}, {1604, 3955}, {2166, 4373}}], BezierCurve[{{2727, 4791}, {3426, 5000}, {4262, 5000}}], BezierCurve[{{5097, 5000}, {5794, 4791}, {6351, 4373}}], BezierCurve[{{6908, 3955}, {7328, 3370}, {7611, 2618}}], BezierCurve[{{7895, 1866}, {8036, 994}, {8036, 0}}], BezierCurve[{{8036, -994}, {7895, -1866}, {7611, -2618}}], BezierCurve[{{7328, -3370}, {6908, -3955}, {6351, -4373}}], BezierCurve[{{5794, -4791}, {5097, -5000}, {4262, -5000}}]}, {Line[{{4262, -3830}}], BezierCurve[{{5088, -3830}, {5713, -3487}, {6135, -2799}}], BezierCurve[{{6558, -2112}, {6769, -1179}, {6769, 0}}], BezierCurve[{{6769, 1170}, {6558, 2098}, {6135, 2786}}], BezierCurve[{{5713, 3473}, {5088, 3816}, {4262, 3816}}], BezierCurve[{{3445, 3816}, {2823, 3473}, {2396, 2786}}], BezierCurve[{{1968, 2098}, {1755, 1170}, {1755, 0}}], BezierCurve[{{1755, -1179}, {1968, -2112}, {2396, -2799}}], BezierCurve[{{2823, -3487}, {3445, -3830}, {4262, -3830}}]}}]},
  "40" -> {FilledCurve[{{Line[{{-2591, -4930}}], BezierCurve[{{-2767, -4930}, {-2916, -4870}, {-3036, -4749}}], BezierCurve[{{-3157, -4629}, {-3217, -4480}, {-3217, -4304}}], Line[{{-3217, -2409}}], Line[{{-7465, -2409}}], BezierCurve[{{-7688, -2409}, {-7878, -2333}, {-8036, -2180}}], BezierCurve[{{-8194, -2026}, {-8273, -1838}, {-8273, -1616}}], Line[{{-8273, -1490}}], BezierCurve[{{-8273, -1295}, {-8217, -1128}, {-8106, -989}}], Line[{{-3733, 4596}}], BezierCurve[{{-3658, 4698}, {-3558, 4779}, {-3433, 4840}}], BezierCurve[{{-3308, 4900}, {-3175, 4930}, {-3036, 4930}}], Line[{{-2758, 4930}}], BezierCurve[{{-2516, 4930}, {-2321, 4851}, {-2173, 4694}}], BezierCurve[{{-2024, 4536}, {-1950, 4345}, {-1950, 4123}}], Line[{{-1950, -1281}}], Line[{{-891, -1281}}], BezierCurve[{{-734, -1281}, {-599, -1337}, {-487, -1448}}], BezierCurve[{{-376, -1560}, {-320, -1695}, {-320, -1852}}], BezierCurve[{{-320, -2010}, {-376, -2143}, {-487, -2249}}], BezierCurve[{{-599, -2356}, {-734, -2409}, {-891, -2409}}], Line[{{-1950, -2409}}], Line[{{-1950, -4304}}], BezierCurve[{{-1950, -4480}, {-2013, -4629}, {-2138, -4749}}], BezierCurve[{{-2263, -4870}, {-2414, -4930}, {-2591, -4930}}]}, {Line[{{-6922, -1281}}], Line[{{-3217, -1281}}], Line[{{-3217, 3496}}], Line[{{-6922, -1281}}]}, {Line[{{4499, -5000}}], BezierCurve[{{3663, -5000}, {2964, -4791}, {2403, -4373}}], BezierCurve[{{1841, -3955}, {1421, -3370}, {1142, -2618}}], BezierCurve[{{864, -1866}, {724, -994}, {724, 0}}], BezierCurve[{{724, 994}, {864, 1866}, {1142, 2618}}], BezierCurve[{{1421, 3370}, {1841, 3955}, {2403, 4373}}], BezierCurve[{{2964, 4791}, {3663, 5000}, {4499, 5000}}], BezierCurve[{{5334, 5000}, {6031, 4791}, {6588, 4373}}], BezierCurve[{{7145, 3955}, {7565, 3370}, {7848, 2618}}], BezierCurve[{{8131, 1866}, {8273, 994}, {8273, 0}}], BezierCurve[{{8273, -994}, {8131, -1866}, {7848, -2618}}], BezierCurve[{{7565, -3370}, {7145, -3955}, {6588, -4373}}], BezierCurve[{{6031, -4791}, {5334, -5000}, {4499, -5000}}]}, {Line[{{4499, -3830}}], BezierCurve[{{5325, -3830}, {5949, -3487}, {6372, -2799}}], BezierCurve[{{6794, -2112}, {7006, -1179}, {7006, 0}}], BezierCurve[{{7006, 1170}, {6794, 2098}, {6372, 2786}}], BezierCurve[{{5949, 3473}, {5325, 3816}, {4499, 3816}}], BezierCurve[{{3682, 3816}, {3059, 3473}, {2632, 2786}}], BezierCurve[{{2205, 2098}, {1992, 1170}, {1992, 0}}], BezierCurve[{{1992, -1179}, {2205, -2112}, {2632, -2799}}], BezierCurve[{{3059, -3487}, {3682, -3830}, {4499, -3830}}]}}]},
  "50" -> {FilledCurve[{{Line[{{-4756, -5000}}], BezierCurve[{{-5406, -5000}, {-5975, -4921}, {-6462, -4763}}], BezierCurve[{{-6950, -4605}, {-7370, -4415}, {-7723, -4192}}], BezierCurve[{{-7788, -4146}, {-7851, -4088}, {-7911, -4018}}], BezierCurve[{{-7971, -3948}, {-8001, -3839}, {-8001, -3691}}], BezierCurve[{{-8001, -3533}, {-7950, -3394}, {-7848, -3273}}], BezierCurve[{{-7746, -3152}, {-7611, -3092}, {-7444, -3092}}], BezierCurve[{{-7342, -3092}, {-7245, -3115}, {-7152, -3162}}], BezierCurve[{{-6855, -3319}, {-6518, -3456}, {-6142, -3572}}], BezierCurve[{{-5766, -3688}, {-5360, -3747}, {-4923, -3747}}], BezierCurve[{{-4376, -3747}, {-3897, -3686}, {-3489, -3565}}], BezierCurve[{{-3080, -3445}, {-2765, -3243}, {-2542, -2960}}], BezierCurve[{{-2319, -2676}, {-2208, -2284}, {-2208, -1783}}], BezierCurve[{{-2208, -1207}, {-2400, -752}, {-2786, -418}}], BezierCurve[{{-3171, -84}, {-3698, 84}, {-4366, 84}}], BezierCurve[{{-4840, 84}, {-5258, 19}, {-5620, -111}}], BezierCurve[{{-5982, -241}, {-6279, -376}, {-6511, -515}}], BezierCurve[{{-6576, -552}, {-6646, -590}, {-6720, -627}}], BezierCurve[{{-6794, -664}, {-6887, -682}, {-6999, -682}}], BezierCurve[{{-7259, -682}, {-7477, -610}, {-7653, -467}}], BezierCurve[{{-7830, -323}, {-7918, -125}, {-7918, 125}}], BezierCurve[{{-7918, 237}, {-7909, 348}, {-7890, 460}}], Line[{{-7263, 4164}}], BezierCurve[{{-7235, 4378}, {-7154, 4547}, {-7019, 4673}}], BezierCurve[{{-6885, 4798}, {-6720, 4861}, {-6525, 4861}}], Line[{{-2054, 4861}}], BezierCurve[{{-1887, 4861}, {-1746, 4803}, {-1630, 4687}}], BezierCurve[{{-1513, 4571}, {-1455, 4429}, {-1455, 4262}}], BezierCurve[{{-1455, 4095}, {-1513, 3955}, {-1630, 3844}}], BezierCurve[{{-1746, 3733}, {-1887, 3677}, {-2054, 3677}}], Line[{{-6093, 3677}}], Line[{{-6539, 724}}], BezierCurve[{{-6251, 882}, {-5915, 1017}, {-5529, 1128}}], BezierCurve[{{-5144, 1240}, {-4696, 1295}, {-4185, 1295}}], BezierCurve[{{-3480, 1295}, {-2888, 1158}, {-2409, 884}}], BezierCurve[{{-1931, 610}, {-1567, 237}, {-1316, -237}}], BezierCurve[{{-1065, -710}, {-940, -1249}, {-940, -1852}}], BezierCurve[{{-940, -2447}, {-1075, -2981}, {-1344, -3454}}], BezierCurve[{{-1613, -3928}, {-2029, -4304}, {-2591, -4582}}], BezierCurve[{{-3152, -4861}, {-3874, -5000}, {-4756, -5000}}]}, {Line[{{4227, -5000}}], BezierCurve[{{3391, -5000}, {2693, -4791}, {2131, -4373}}], BezierCurve[{{1569, -3955}, {1149, -3370}, {870, -2618}}], BezierCurve[{{592, -1866}, {453, -994}, {453, 0}}], BezierCurve[{{453, 994}, {592, 1866}, {870, 2618}}], BezierCurve[{{1149, 3370}, {1569, 3955}, {2131, 4373}}], BezierCurve[{{2693, 4791}, {3391, 5000}, {4227, 5000}}], BezierCurve[{{5063, 5000}, {5759, 4791}, {6316, 4373}}], BezierCurve[{{6873, 3955}, {7293, 3370}, {7577, 2618}}], BezierCurve[{{7860, 1866}, {8001, 994}, {8001, 0}}], BezierCurve[{{8001, -994}, {7860, -1866}, {7577, -2618}}], BezierCurve[{{7293, -3370}, {6873, -3955}, {6316, -4373}}], BezierCurve[{{5759, -4791}, {5063, -5000}, {4227, -5000}}]}, {Line[{{4227, -3830}}], BezierCurve[{{5053, -3830}, {5678, -3487}, {6100, -2799}}], BezierCurve[{{6523, -2112}, {6734, -1179}, {6734, 0}}], BezierCurve[{{6734, 1170}, {6523, 2098}, {6100, 2786}}], BezierCurve[{{5678, 3473}, {5053, 3816}, {4227, 3816}}], BezierCurve[{{3410, 3816}, {2788, 3473}, {2361, 2786}}], BezierCurve[{{1934, 2098}, {1720, 1170}, {1720, 0}}], BezierCurve[{{1720, -1179}, {1934, -2112}, {2361, -2799}}], BezierCurve[{{2788, -3487}, {3410, -3830}, {4227, -3830}}]}}]},
  "60" -> {FilledCurve[{{Line[{{-4220, -5000}}], BezierCurve[{{-4907, -5000}, {-5481, -4891}, {-5940, -4673}}], BezierCurve[{{-6400, -4455}, {-6769, -4162}, {-7047, -3795}}], BezierCurve[{{-7326, -3429}, {-7537, -3027}, {-7681, -2591}}], BezierCurve[{{-7825, -2154}, {-7920, -1711}, {-7967, -1260}}], BezierCurve[{{-8013, -810}, {-8036, -395}, {-8036, -14}}], BezierCurve[{{-8036, 534}, {-7969, 1098}, {-7834, 1678}}], BezierCurve[{{-7700, 2259}, {-7477, 2802}, {-7166, 3308}}], BezierCurve[{{-6855, 3814}, {-6439, 4222}, {-5919, 4533}}], BezierCurve[{{-5399, 4844}, {-4759, 5000}, {-3997, 5000}}], BezierCurve[{{-3422, 5000}, {-2936, 4937}, {-2542, 4812}}], BezierCurve[{{-2147, 4687}, {-1811, 4550}, {-1532, 4401}}], BezierCurve[{{-1337, 4308}, {-1240, 4141}, {-1240, 3900}}], BezierCurve[{{-1240, 3751}, {-1291, 3619}, {-1393, 3503}}], BezierCurve[{{-1495, 3387}, {-1634, 3329}, {-1811, 3329}}], BezierCurve[{{-1894, 3329}, {-1973, 3343}, {-2047, 3370}}], BezierCurve[{{-2279, 3482}, {-2551, 3577}, {-2862, 3656}}], BezierCurve[{{-3173, 3735}, {-3519, 3774}, {-3900, 3774}}], BezierCurve[{{-4494, 3774}, {-4979, 3651}, {-5355, 3405}}], BezierCurve[{{-5731, 3159}, {-6024, 2846}, {-6233, 2465}}], BezierCurve[{{-6442, 2084}, {-6588, 1692}, {-6671, 1288}}], BezierCurve[{{-6755, 884}, {-6806, 525}, {-6825, 209}}], BezierCurve[{{-6592, 506}, {-6249, 785}, {-5794, 1045}}], BezierCurve[{{-5339, 1305}, {-4731, 1435}, {-3969, 1435}}], BezierCurve[{{-3338, 1435}, {-2783, 1300}, {-2305, 1031}}], BezierCurve[{{-1827, 761}, {-1453, 383}, {-1184, -104}}], BezierCurve[{{-915, -592}, {-780, -1170}, {-780, -1838}}], BezierCurve[{{-780, -2423}, {-919, -2955}, {-1198, -3433}}], BezierCurve[{{-1476, -3911}, {-1871, -4292}, {-2382, -4575}}], BezierCurve[{{-2892, -4858}, {-3505, -5000}, {-4220, -5000}}]}, {Line[{{-4206, -3816}}], BezierCurve[{{-3760, -3816}, {-3377, -3723}, {-3057, -3538}}], BezierCurve[{{-2737, -3352}, {-2488, -3106}, {-2312, -2799}}], BezierCurve[{{-2136, -2493}, {-2047, -2163}, {-2047, -1811}}], BezierCurve[{{-2047, -1198}, {-2219, -696}, {-2563, -306}}], BezierCurve[{{-2906, 84}, {-3426, 279}, {-4123, 279}}], BezierCurve[{{-4670, 279}, {-5121, 188}, {-5474, 7}}], BezierCurve[{{-5826, -174}, {-6103, -383}, {-6302, -620}}], BezierCurve[{{-6502, -857}, {-6653, -1068}, {-6755, -1253}}], BezierCurve[{{-6690, -1699}, {-6569, -2117}, {-6393, -2507}}], BezierCurve[{{-6216, -2897}, {-5952, -3213}, {-5599, -3454}}], BezierCurve[{{-5246, -3695}, {-4782, -3816}, {-4206, -3816}}]}, {Line[{{4262, -5000}}], BezierCurve[{{3426, -5000}, {2727, -4791}, {2166, -4373}}], BezierCurve[{{1604, -3955}, {1184, -3370}, {905, -2618}}], BezierCurve[{{627, -1866}, {487, -994}, {487, 0}}], BezierCurve[{{487, 994}, {627, 1866}, {905, 2618}}], BezierCurve[{{1184, 3370}, {1604, 3955}, {2166, 4373}}], BezierCurve[{{2727, 4791}, {3426, 5000}, {4262, 5000}}], BezierCurve[{{5097, 5000}, {5794, 4791}, {6351, 4373}}], BezierCurve[{{6908, 3955}, {7328, 3370}, {7611, 2618}}], BezierCurve[{{7895, 1866}, {8036, 994}, {8036, 0}}], BezierCurve[{{8036, -994}, {7895, -1866}, {7611, -2618}}], BezierCurve[{{7328, -3370}, {6908, -3955}, {6351, -4373}}], BezierCurve[{{5794, -4791}, {5097, -5000}, {4262, -5000}}]}, {Line[{{4262, -3830}}], BezierCurve[{{5088, -3830}, {5713, -3487}, {6135, -2799}}], BezierCurve[{{6558, -2112}, {6769, -1179}, {6769, 0}}], BezierCurve[{{6769, 1170}, {6558, 2098}, {6135, 2786}}], BezierCurve[{{5713, 3473}, {5088, 3816}, {4262, 3816}}], BezierCurve[{{3445, 3816}, {2823, 3473}, {2396, 2786}}], BezierCurve[{{1968, 2098}, {1755, 1170}, {1755, 0}}], BezierCurve[{{1755, -1179}, {1968, -2112}, {2396, -2799}}], BezierCurve[{{2823, -3487}, {3445, -3830}, {4262, -3830}}]}}]}
|>;

(* outlines of str with ink height 1, ink box centred at the origin *)
fontOutline[str_] := fontOutline[str] = fontOutlines[str] /. {x_Integer, y_Integer} :> {x, y}/10.^4;

(* str with digit height h, its ink box centred at the origin *)
numeral[str_, h_] := fontOutline[str] /. {x_?NumericQ, y_?NumericQ} :> h {x, y};

(* numeral centred at p with height h, baseline along dir *)
label[str_, p_, h_, dir_ : {1, 0}] := With[{m = RotationMatrix[{{1, 0}, dir}]},
  {ink, numeral[str, h] /. {x_?NumericQ, y_?NumericQ} :> p + m . {x, y}}];

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
  {ink, Thickness[1.05 * 2 k/(2 faceHalf)], CapForm["Round"], JoinForm["Round"],
   openmojiPointLeft /. {x_?NumericQ, y_?NumericQ} :> c + k {x, y}}];

(* Hour ring: upright numerals at 3, 6 and 9, each as large as fits between the
   two circles (up to the full ring width); short radial bars at the other
   hours. The 6 is drawn at 85% of its fitted size. Computed once per dial
   radius. *)
hourNumerals[r_] := hourNumerals[r] =
  With[{ri = 0.70 r + lineHalf[thinW], ro = r - lineHalf[thinW], bar = 0.4 (0.30 r)},
    Table[With[{a = 2 Pi k/12},
      If[MemberQ[{3, 6, 9}, k],
        With[{f = ringFit[ToString[k], a, ri, ro]},
          label[ToString[k], f[[2]] clockDir[a], If[k == 6, 0.85, 1] f[[1]]]],
        {ink, CapForm["Round"], Thickness[0.005408],
          Line[{(0.85 r - bar/2) clockDir[a], (0.85 r + bar/2) clockDir[a]}]}]], {k, 1, 11}]];

hourDial[h_, m_, r_] := {
  {planet, Disk[{0, 0}, r]},
  {ink, thin, Circle[{0, 0}, r], Circle[{0, 0}, 0.70 r]},
  hourNumerals[r],
  handEmoji[0.85 r clockDir[0], 0.30 r],
  {ink, taperedHand[clockDir[2 Pi (Mod[h, 12] + m/60)/12], -0.04 r, 0.62 r, 0.032 r, 0.016 r]}};

(* Seconds dial: 12 ticks between two rings, the 12 o'clock tick in orange. *)
secondsDial[s_, r_] := {
  {planet, Disk[{0, 0}, r]},
  {ink, thin, Circle[{0, 0}, r], Circle[{0, 0}, 0.64 r]},
  {CapForm["Round"], Thickness[0.0042],
    Table[{If[k == 0, accent, ink],
      Line[{0.74 r clockDir[2 Pi k/12], 0.90 r clockDir[2 Pi k/12]}]}, {k, 0, 11}]},
  {ink, taperedHand[clockDir[2 Pi s/60], -0.06 r, 0.58 r, 0.06 r, 0.028 r]}};

(* Day-of-week dial: two hollow tubes around the ring, Monday-Friday in ink and
   Saturday-Sunday in orange, clockwise from 12 (each day is 1/7 of the turn,
   marked off by dividers);
   the hand sweeps through the current day's seventh over the day. *)

(* outline of a tube along radius rr from clock angle c1 to c2 (clockwise from
   12), width w, with round ends. Circle angles are counter-clockwise from
   3 o'clock. *)
hollowTube[rr_, w_, c1_, c2_] := With[{t1 = Pi/2 - c2, t2 = Pi/2 - c1},
  {Circle[{0, 0}, rr + w/2, {t1, t2}], Circle[{0, 0}, rr - w/2, {t1, t2}],
   Circle[rr {Cos[t1], Sin[t1]}, w/2, {t1 - Pi, t1}],
   Circle[rr {Cos[t2], Sin[t2]}, w/2, {t2, t2 + Pi}]}];

(* the same tube as a filled polygon *)
solidTube[rr_, w_, c1_, c2_] := With[{t1 = Pi/2 - c2, t2 = Pi/2 - c1},
  Polygon[Join[
    Table[(rr + w/2) {Cos[t], Sin[t]}, {t, t1, t2, (t2 - t1)/60}],
    Table[rr {Cos[t2], Sin[t2]} + w/2 {Cos[t], Sin[t]}, {t, t2, t2 + Pi, Pi/24}],
    Table[(rr - w/2) {Cos[t], Sin[t]}, {t, t2, t1, (t1 - t2)/60}],
    Table[rr {Cos[t1], Sin[t1]} + w/2 {Cos[t], Sin[t]}, {t, t1 + Pi, t1 + 2 Pi, Pi/24}]]]];

(* one short tube per day: Monday-Friday solid, Saturday-Sunday hollow *)
dayDial[wd_, frac_, r_] := With[{gap = 0.13, rr = 0.78 r, w = 0.11 r},
  {
   {planet, Disk[{0, 0}, r]},
   {ink, thin, Circle[{0, 0}, r]},
   Table[With[{c1 = 2 Pi k/7 + gap, c2 = 2 Pi (k + 1)/7 - gap},
     If[k < 5, {ink, solidTube[rr, w, c1, c2]},
       {accent, Thickness[0.0024], hollowTube[rr, w, c1, c2]}]], {k, 0, 6}],
   {ink, taperedHand[clockDir[2 Pi (wd - 1 + frac)/7], -0.06 r, 0.60 r, 0.065 r, 0.03 r]}}];

satellite[g_, pos_, rot_] := Translate[g, clockwise[pos, rot]];

(* ---------- fixed outer minute ring ----------
   Ticks run inward from radius tickOut. Each numeral replaces a 10-minute tick
   and spans exactly the minute ticks beside it: from the inner to the outer
   end of their round caps, measured along the numeral's axis (the ticks sit
   one minute = 6 degrees off it). *)
minuteRing = Module[{tickOut = 0.95, short = 0.055, long = 0.085, tw = 0.0052, ticks, labels, rIn, rOut},
  ticks = Table[
    If[Mod[k, 10] == 0, Nothing,
      With[{a = 2 Pi k/60, l = If[Mod[k, 5] == 0, long, short]},
        Line[{tickOut clockDir[a], (tickOut - l) clockDir[a]}]]],
    {k, 0, 59}];
  rOut = Cos[2 Pi/60] (tickOut + lineHalf[tw]); rIn = Cos[2 Pi/60] (tickOut - short - lineHalf[tw]);
  labels = Table[
    With[{a = 2 Pi k/60},
      label[ToString[If[k == 0, 60, k]], (rIn + rOut)/2 clockDir[a], rOut - rIn, {Cos[a], -Sin[a]}]],
    {k, 0, 50, 10}];
  {ink, CapForm["Round"], Thickness[tw], ticks, labels}];

(* black line separating the fixed minute ring from the rotating disc *)
ringBoundary = {line, AbsoluteThickness[1.428], Circle[{0, 0}, 0.845]};

(* flat case ring *)
caseRing = {rim, Disk[{0, 0}, 1.097]};

(* ---------- the face ---------- *)
RessenceWatchFace[opts : OptionsPattern[Graphics]] := RessenceWatchFace[Now, opts];

RessenceWatchFace[d_DateObject, opts : OptionsPattern[Graphics]] := faceAt[{
  DateValue[d, "Hour"], DateValue[d, "Minute"], DateValue[d, "Second"],
  DateValue[d, "ISOWeekDay"]}, opts];

faceAt[{h_, m_, s_, wd_}, opts : OptionsPattern[Graphics]] := Module[
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
    {marker, taperedHand[dir, 0.18, 0.79, 0.022, 0.010]}
    },
    opts, PlotRange -> faceHalf, ImageSize -> 420,
    Background -> None]
  ];

(* ---------- live watch ---------- *)
RessenceWatch[] := DynamicModule[{warp = 1, t0 = AbsoluteTime[], base = AbsoluteTime[]},
  Panel[Column[{
    Row[{"Time warp ",
      SetterBar[Dynamic[warp, (base = base + warp (AbsoluteTime[] - t0); t0 = AbsoluteTime[]; warp = #) &],
        {1 -> "1\[Times]", 100 -> "100\[Times]", 250 -> "250\[Times]"}],
      "  ", Button["Reset to now", (base = AbsoluteTime[]; t0 = AbsoluteTime[]; warp = 1)]}],
    Dynamic[
      Refresh[
        Module[{t = base + warp (AbsoluteTime[] - t0)},
          RessenceWatchFace[DateObject[t]]],
        UpdateInterval -> 0.1, TrackedSymbols :> {}]]
  }, Alignment -> Center], Background -> Transparent],
  SaveDefinitions -> True];

End[];
EndPackage[];
