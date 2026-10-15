(* ::Package:: *)

(* Ressence Type 1 style watch face drawn with graphics primitives; numerals are
   Nunito Bold glyph outlines.

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

   They use Nunito Bold (by Vernon Adams and others, SIL Open Font License
   1.1). Its outlines are stored below, so the font need not be installed. They
   were read from the glyf table of the weight-700 TTF served by Google Fonts,
   with the quadratic contours converted to cubic Beziers and the glyphs set at
   their advance widths. That TTF is an instance of a variable font and keeps
   overlapping strokes, which FilledCurve (even-odd fill) would show as holes,
   so each contour was split at its self-crossings into simple loops, one
   FilledCurve per loop (with any counters inside it). The outlines are
   normalised to ink height 1 with the ink box centred at the origin and stored
   in units of 1/10000. Only the strings the dials use are included. *)
fontOutlines = <|
  "3" -> {FilledCurve[{{Line[{{-63, -5000}}], BezierCurve[{{-560, -5000}, {-1075, -4936}, {-1608, -4807}}], BezierCurve[{{-2141, -4678}, {-2606, -4485}, {-3001, -4228}}], BezierCurve[{{-3185, -4108}, {-3309, -3970}, {-3374, -3814}}], BezierCurve[{{-3438, -3657}, {-3454, -3508}, {-3422, -3366}}], BezierCurve[{{-3390, -3223}, {-3321, -3101}, {-3215, -3000}}], BezierCurve[{{-3109, -2899}, {-2976, -2839}, {-2815, -2821}}], BezierCurve[{{-2654, -2802}, {-2477, -2844}, {-2284, -2945}}], BezierCurve[{{-1907, -3147}, {-1544, -3292}, {-1194, -3379}}], BezierCurve[{{-845, -3467}, {-486, -3510}, {-118, -3510}}], BezierCurve[{{286, -3510}, {626, -3455}, {902, -3345}}], BezierCurve[{{1178, -3234}, {1387, -3069}, {1530, -2848}}], BezierCurve[{{1672, -2628}, {1744, -2361}, {1744, -2048}}], BezierCurve[{{1744, -1589}, {1590, -1246}, {1282, -1021}}], BezierCurve[{{974, -795}, {525, -683}, {-63, -683}}], Line[{{-946, -683}}], BezierCurve[{{-1203, -683}, {-1399, -618}, {-1532, -490}}], BezierCurve[{{-1666, -361}, {-1732, -182}, {-1732, 48}}], BezierCurve[{{-1732, 278}, {-1666, 457}, {-1532, 586}}], BezierCurve[{{-1399, 715}, {-1203, 779}, {-946, 779}}], Line[{{-229, 779}}], BezierCurve[{{121, 779}, {420, 837}, {668, 952}}], BezierCurve[{{916, 1067}, {1109, 1230}, {1247, 1441}}], BezierCurve[{{1385, 1653}, {1454, 1910}, {1454, 2214}}], BezierCurve[{{1454, 2637}, {1314, 2959}, {1033, 3179}}], BezierCurve[{{753, 3400}, {346, 3510}, {-187, 3510}}], BezierCurve[{{-528, 3510}, {-847, 3471}, {-1146, 3393}}], BezierCurve[{{-1445, 3315}, {-1774, 3170}, {-2132, 2959}}], BezierCurve[{{-2298, 2857}, {-2456, 2816}, {-2608, 2834}}], BezierCurve[{{-2760, 2853}, {-2886, 2913}, {-2987, 3014}}], BezierCurve[{{-3089, 3115}, {-3157, 3237}, {-3194, 3379}}], BezierCurve[{{-3231, 3522}, {-3222, 3669}, {-3167, 3821}}], BezierCurve[{{-3111, 3972}, {-2997, 4103}, {-2822, 4214}}], BezierCurve[{{-2426, 4471}, {-1990, 4667}, {-1511, 4800}}], BezierCurve[{{-1033, 4933}, {-541, 5000}, {-36, 5000}}], BezierCurve[{{636, 5000}, {1210, 4897}, {1689, 4690}}], BezierCurve[{{2167, 4483}, {2532, 4189}, {2785, 3807}}], BezierCurve[{{3038, 3425}, {3164, 2963}, {3164, 2421}}], BezierCurve[{{3164, 2034}, {3091, 1683}, {2944, 1366}}], BezierCurve[{{2797, 1048}, {2590, 779}, {2323, 559}}], BezierCurve[{{2134, 402}, {1917, 278}, {1672, 186}}], BezierCurve[{{2185, 50}, {2595, -201}, {2902, -566}}], BezierCurve[{{3270, -1002}, {3454, -1547}, {3454, -2200}}], BezierCurve[{{3454, -2770}, {3311, -3267}, {3026, -3690}}], BezierCurve[{{2741, -4113}, {2337, -4437}, {1813, -4662}}], BezierCurve[{{1289, -4887}, {663, -5000}, {-63, -5000}}]}}], FilledCurve[{{Line[{{1672, 186}}], BezierCurve[{{1572, 148}, {1467, 116}, {1357, 90}}], Line[{{1357, 255}}], BezierCurve[{{1467, 237}, {1572, 214}, {1672, 186}}]}}]},
  "6" -> {FilledCurve[{{Line[{{283, -5000}}], BezierCurve[{{-517, -5000}, {-1200, -4809}, {-1766, -4428}}], BezierCurve[{{-2331, -4046}, {-2761, -3497}, {-3055, -2779}}], BezierCurve[{{-3349, -2062}, {-3497, -1193}, {-3497, -172}}], BezierCurve[{{-3497, 655}, {-3402, 1389}, {-3214, 2028}}], BezierCurve[{{-3025, 2667}, {-2752, 3207}, {-2393, 3648}}], BezierCurve[{{-2034, 4090}, {-1595, 4425}, {-1076, 4655}}], BezierCurve[{{-556, 4885}, {30, 5000}, {683, 5000}}], BezierCurve[{{1087, 5000}, {1499, 4938}, {1917, 4814}}], BezierCurve[{{2336, 4690}, {2729, 4503}, {3097, 4255}}], BezierCurve[{{3244, 4163}, {3340, 4044}, {3386, 3897}}], BezierCurve[{{3432, 3749}, {3439, 3605}, {3407, 3462}}], BezierCurve[{{3375, 3320}, {3310, 3195}, {3214, 3090}}], BezierCurve[{{3117, 2984}, {2995, 2920}, {2848, 2897}}], BezierCurve[{{2701, 2874}, {2540, 2913}, {2366, 3014}}], BezierCurve[{{2053, 3198}, {1759, 3326}, {1483, 3400}}], BezierCurve[{{1207, 3474}, {931, 3510}, {655, 3510}}], BezierCurve[{{122, 3510}, {-329, 3386}, {-697, 3138}}], BezierCurve[{{-1064, 2890}, {-1343, 2517}, {-1531, 2021}}], BezierCurve[{{-1720, 1524}, {-1814, 903}, {-1814, 159}}], Line[{{-1814, -237}}], BezierCurve[{{-1736, -30}, {-1637, 159}, {-1517, 331}}], BezierCurve[{{-1283, 667}, {-982, 924}, {-614, 1103}}], BezierCurve[{{-246, 1283}, {163, 1372}, {614, 1372}}], BezierCurve[{{1175, 1372}, {1671, 1239}, {2103, 972}}], BezierCurve[{{2536, 706}, {2876, 338}, {3124, -131}}], BezierCurve[{{3372, -600}, {3497, -1138}, {3497, -1745}}], BezierCurve[{{3497, -2370}, {3361, -2929}, {3090, -3421}}], BezierCurve[{{2818, -3913}, {2441, -4299}, {1959, -4579}}], BezierCurve[{{1476, -4860}, {917, -5000}, {283, -5000}}]}, {Line[{{186, -3593}}], BezierCurve[{{517, -3593}, {809, -3520}, {1062, -3372}}], BezierCurve[{{1315, -3225}, {1510, -3016}, {1648, -2745}}], BezierCurve[{{1786, -2474}, {1855, -2163}, {1855, -1814}}], BezierCurve[{{1855, -1455}, {1786, -1143}, {1648, -876}}], BezierCurve[{{1510, -609}, {1315, -400}, {1062, -248}}], BezierCurve[{{809, -97}, {517, -21}, {186, -21}}], BezierCurve[{{-145, -21}, {-434, -97}, {-683, -248}}], BezierCurve[{{-931, -400}, {-1126, -609}, {-1269, -876}}], BezierCurve[{{-1411, -1143}, {-1483, -1455}, {-1483, -1814}}], BezierCurve[{{-1483, -2163}, {-1411, -2474}, {-1269, -2745}}], BezierCurve[{{-1126, -3016}, {-931, -3225}, {-683, -3372}}], BezierCurve[{{-434, -3520}, {-145, -3593}, {186, -3593}}]}}], FilledCurve[{{Line[{{-1814, -237}}], Line[{{-1814, -876}}], Line[{{-1979, -876}}], BezierCurve[{{-1943, -647}, {-1888, -434}, {-1814, -237}}]}}]},
  "9" -> {FilledCurve[{{Line[{{-683, -5000}}], BezierCurve[{{-1078, -5000}, {-1487, -4938}, {-1910, -4814}}], BezierCurve[{{-2333, -4690}, {-2729, -4503}, {-3097, -4255}}], BezierCurve[{{-3244, -4163}, {-3340, -4044}, {-3386, -3897}}], BezierCurve[{{-3432, -3749}, {-3439, -3605}, {-3407, -3462}}], BezierCurve[{{-3375, -3320}, {-3310, -3195}, {-3214, -3090}}], BezierCurve[{{-3117, -2984}, {-2995, -2920}, {-2848, -2897}}], BezierCurve[{{-2701, -2874}, {-2540, -2913}, {-2366, -3014}}], BezierCurve[{{-2053, -3198}, {-1759, -3326}, {-1483, -3400}}], BezierCurve[{{-1207, -3474}, {-931, -3510}, {-655, -3510}}], BezierCurve[{{-122, -3510}, {329, -3386}, {697, -3138}}], BezierCurve[{{1064, -2890}, {1343, -2515}, {1531, -2014}}], BezierCurve[{{1720, -1513}, {1814, -894}, {1814, -159}}], Line[{{1814, 223}}], BezierCurve[{{1739, 26}, {1642, -156}, {1524, -324}}], BezierCurve[{{1285, -664}, {982, -924}, {614, -1103}}], BezierCurve[{{246, -1283}, {-163, -1372}, {-614, -1372}}], BezierCurve[{{-1166, -1372}, {-1660, -1239}, {-2097, -972}}], BezierCurve[{{-2533, -706}, {-2876, -336}, {-3124, 138}}], BezierCurve[{{-3372, 611}, {-3497, 1147}, {-3497, 1745}}], BezierCurve[{{-3497, 2379}, {-3361, 2940}, {-3090, 3428}}], BezierCurve[{{-2818, 3915}, {-2439, 4299}, {-1952, 4579}}], BezierCurve[{{-1464, 4860}, {-908, 5000}, {-283, 5000}}], BezierCurve[{{526, 5000}, {1211, 4811}, {1772, 4434}}], BezierCurve[{{2333, 4057}, {2761, 3508}, {3055, 2786}}], BezierCurve[{{3349, 2064}, {3497, 1193}, {3497, 172}}], BezierCurve[{{3497, -655}, {3402, -1389}, {3214, -2028}}], BezierCurve[{{3025, -2667}, {2752, -3207}, {2393, -3648}}], BezierCurve[{{2034, -4090}, {1595, -4425}, {1076, -4655}}], BezierCurve[{{556, -4885}, {-30, -5000}, {-683, -5000}}]}, {Line[{{-172, 34}}], BezierCurve[{{159, 34}, {451, 110}, {703, 262}}], BezierCurve[{{956, 414}, {1152, 625}, {1290, 897}}], BezierCurve[{{1428, 1168}, {1497, 1478}, {1497, 1828}}], BezierCurve[{{1497, 2186}, {1428, 2499}, {1290, 2766}}], BezierCurve[{{1152, 3032}, {956, 3239}, {703, 3386}}], BezierCurve[{{451, 3533}, {159, 3607}, {-172, 3607}}], BezierCurve[{{-503, 3607}, {-793, 3533}, {-1041, 3386}}], BezierCurve[{{-1290, 3239}, {-1485, 3032}, {-1628, 2766}}], BezierCurve[{{-1770, 2499}, {-1841, 2186}, {-1841, 1828}}], BezierCurve[{{-1841, 1478}, {-1770, 1168}, {-1628, 897}}], BezierCurve[{{-1485, 625}, {-1290, 414}, {-1041, 262}}], BezierCurve[{{-793, 110}, {-503, 34}, {-172, 34}}]}}], FilledCurve[{{Line[{{1814, 223}}], Line[{{1814, 876}}], Line[{{1979, 876}}], BezierCurve[{{1947, 643}, {1892, 425}, {1814, 223}}]}}]},
  "10" -> {FilledCurve[{{Line[{{-6524, -4862}}], BezierCurve[{{-6763, -4862}, {-6949, -4795}, {-7083, -4662}}], BezierCurve[{{-7216, -4529}, {-7283, -4347}, {-7283, -4117}}], BezierCurve[{{-7283, -3878}, {-7216, -3699}, {-7083, -3579}}], BezierCurve[{{-6949, -3460}, {-6763, -3400}, {-6524, -3400}}], Line[{{-4979, -3400}}], Line[{{-4979, 2855}}], Line[{{-6152, 2145}}], BezierCurve[{{-6326, 2044}, {-6490, 2005}, {-6641, 2028}}], BezierCurve[{{-6793, 2051}, {-6922, 2115}, {-7028, 2221}}], BezierCurve[{{-7133, 2326}, {-7205, 2453}, {-7241, 2600}}], BezierCurve[{{-7278, 2747}, {-7267, 2897}, {-7207, 3048}}], BezierCurve[{{-7147, 3200}, {-7030, 3326}, {-6855, 3428}}], Line[{{-4952, 4572}}], BezierCurve[{{-4768, 4683}, {-4584, 4777}, {-4400, 4855}}], BezierCurve[{{-4216, 4933}, {-4046, 4972}, {-3890, 4972}}], BezierCurve[{{-3697, 4972}, {-3536, 4920}, {-3407, 4814}}], BezierCurve[{{-3278, 4708}, {-3214, 4536}, {-3214, 4297}}], Line[{{-3214, -3400}}], Line[{{-1807, -3400}}], BezierCurve[{{-1568, -3400}, {-1382, -3460}, {-1248, -3579}}], BezierCurve[{{-1115, -3699}, {-1048, -3878}, {-1048, -4117}}], BezierCurve[{{-1048, -4356}, {-1115, -4540}, {-1248, -4669}}], BezierCurve[{{-1382, -4798}, {-1568, -4862}, {-1807, -4862}}], Line[{{-6524, -4862}}]}}], FilledCurve[{{Line[{{-4979, 2855}}], Line[{{-4979, 3372}}], Line[{{-4124, 3372}}], Line[{{-4979, 2855}}]}}], FilledCurve[{{Line[{{3738, -5000}}], BezierCurve[{{2589, -5000}, {1710, -4566}, {1103, -3697}}], BezierCurve[{{497, -2828}, {193, -1589}, {193, 21}}], BezierCurve[{{193, 1106}, {329, 2018}, {600, 2759}}], BezierCurve[{{871, 3499}, {1271, 4057}, {1800, 4434}}], BezierCurve[{{2329, 4811}, {2975, 5000}, {3738, 5000}}], BezierCurve[{{4897, 5000}, {5777, 4577}, {6379, 3731}}], BezierCurve[{{6982, 2885}, {7283, 1653}, {7283, 34}}], BezierCurve[{{7283, -1041}, {7147, -1954}, {6876, -2703}}], BezierCurve[{{6605, -3453}, {6207, -4023}, {5683, -4414}}], BezierCurve[{{5159, -4805}, {4510, -5000}, {3738, -5000}}]}, {Line[{{3738, -3552}}], BezierCurve[{{4363, -3552}, {4825, -3262}, {5124, -2683}}], BezierCurve[{{5423, -2103}, {5572, -1198}, {5572, 34}}], BezierCurve[{{5572, 1267}, {5423, 2161}, {5124, 2717}}], BezierCurve[{{4825, 3274}, {4363, 3552}, {3738, 3552}}], BezierCurve[{{3122, 3552}, {2662, 3274}, {2359, 2717}}], BezierCurve[{{2055, 2161}, {1903, 1267}, {1903, 34}}], BezierCurve[{{1903, -1198}, {2055, -2103}, {2359, -2683}}], BezierCurve[{{2662, -3262}, {3122, -3552}, {3738, -3552}}]}}]},
  "20" -> {FilledCurve[{{Line[{{-6526, -4862}}], BezierCurve[{{-6821, -4862}, {-7034, -4795}, {-7168, -4662}}], BezierCurve[{{-7301, -4529}, {-7368, -4333}, {-7368, -4076}}], BezierCurve[{{-7368, -3920}, {-7326, -3768}, {-7244, -3621}}], BezierCurve[{{-7161, -3474}, {-7041, -3317}, {-6885, -3152}}], Line[{{-4113, -200}}], BezierCurve[{{-3690, 241}, {-3386, 644}, {-3202, 1007}}], BezierCurve[{{-3018, 1370}, {-2926, 1736}, {-2926, 2103}}], BezierCurve[{{-2926, 2563}, {-3071, 2913}, {-3361, 3152}}], BezierCurve[{{-3651, 3391}, {-4071, 3510}, {-4623, 3510}}], BezierCurve[{{-4936, 3510}, {-5248, 3467}, {-5561, 3379}}], BezierCurve[{{-5874, 3292}, {-6191, 3143}, {-6513, 2931}}], BezierCurve[{{-6678, 2821}, {-6832, 2775}, {-6975, 2793}}], BezierCurve[{{-7117, 2811}, {-7241, 2871}, {-7347, 2972}}], BezierCurve[{{-7453, 3074}, {-7524, 3198}, {-7561, 3345}}], BezierCurve[{{-7598, 3492}, {-7591, 3641}, {-7540, 3793}}], BezierCurve[{{-7490, 3945}, {-7382, 4076}, {-7216, 4186}}], BezierCurve[{{-6821, 4453}, {-6377, 4655}, {-5885, 4793}}], BezierCurve[{{-5393, 4931}, {-4899, 5000}, {-4402, 5000}}], BezierCurve[{{-3703, 5000}, {-3115, 4894}, {-2637, 4683}}], BezierCurve[{{-2159, 4471}, {-1795, 4163}, {-1547, 3759}}], BezierCurve[{{-1299, 3354}, {-1175, 2862}, {-1175, 2283}}], BezierCurve[{{-1175, 1897}, {-1237, 1520}, {-1361, 1152}}], BezierCurve[{{-1485, 784}, {-1674, 411}, {-1926, 34}}], BezierCurve[{{-2179, -343}, {-2508, -743}, {-2913, -1166}}], Line[{{-5056, -3400}}], Line[{{-1451, -3400}}], BezierCurve[{{-1202, -3400}, {-1014, -3460}, {-885, -3579}}], BezierCurve[{{-756, -3699}, {-692, -3878}, {-692, -4117}}], BezierCurve[{{-692, -4356}, {-756, -4540}, {-885, -4669}}], BezierCurve[{{-1014, -4798}, {-1202, -4862}, {-1451, -4862}}], Line[{{-6526, -4862}}]}}], FilledCurve[{{Line[{{-5056, -3400}}], Line[{{-5506, -3869}}], Line[{{-5506, -3400}}], Line[{{-5056, -3400}}]}}], FilledCurve[{{Line[{{4053, -5000}}], BezierCurve[{{2903, -5000}, {2025, -4566}, {1418, -3697}}], BezierCurve[{{811, -2828}, {508, -1589}, {508, 21}}], BezierCurve[{{508, 1106}, {644, 2018}, {915, 2759}}], BezierCurve[{{1186, 3499}, {1586, 4057}, {2115, 4434}}], BezierCurve[{{2644, 4811}, {3290, 5000}, {4053, 5000}}], BezierCurve[{{5211, 5000}, {6092, 4577}, {6694, 3731}}], BezierCurve[{{7297, 2885}, {7598, 1653}, {7598, 34}}], BezierCurve[{{7598, -1041}, {7462, -1954}, {7191, -2703}}], BezierCurve[{{6920, -3453}, {6522, -4023}, {5998, -4414}}], BezierCurve[{{5474, -4805}, {4825, -5000}, {4053, -5000}}]}, {Line[{{4053, -3552}}], BezierCurve[{{4678, -3552}, {5140, -3262}, {5439, -2683}}], BezierCurve[{{5738, -2103}, {5887, -1198}, {5887, 34}}], BezierCurve[{{5887, 1267}, {5738, 2161}, {5439, 2717}}], BezierCurve[{{5140, 3274}, {4678, 3552}, {4053, 3552}}], BezierCurve[{{3437, 3552}, {2977, 3274}, {2674, 2717}}], BezierCurve[{{2370, 2161}, {2218, 1267}, {2218, 34}}], BezierCurve[{{2218, -1198}, {2370, -2103}, {2674, -2683}}], BezierCurve[{{2977, -3262}, {3437, -3552}, {4053, -3552}}]}}]},
  "30" -> {FilledCurve[{{Line[{{-4311, -5000}}], BezierCurve[{{-4808, -5000}, {-5323, -4936}, {-5856, -4807}}], BezierCurve[{{-6390, -4678}, {-6854, -4485}, {-7249, -4228}}], BezierCurve[{{-7433, -4108}, {-7557, -3970}, {-7622, -3814}}], BezierCurve[{{-7686, -3657}, {-7702, -3508}, {-7670, -3366}}], BezierCurve[{{-7638, -3223}, {-7569, -3101}, {-7463, -3000}}], BezierCurve[{{-7357, -2899}, {-7224, -2839}, {-7063, -2821}}], BezierCurve[{{-6902, -2802}, {-6725, -2844}, {-6532, -2945}}], BezierCurve[{{-6155, -3147}, {-5792, -3292}, {-5443, -3379}}], BezierCurve[{{-5093, -3467}, {-4734, -3510}, {-4367, -3510}}], BezierCurve[{{-3962, -3510}, {-3622, -3455}, {-3346, -3345}}], BezierCurve[{{-3070, -3234}, {-2861, -3069}, {-2718, -2848}}], BezierCurve[{{-2576, -2628}, {-2505, -2361}, {-2505, -2048}}], BezierCurve[{{-2505, -1589}, {-2659, -1246}, {-2967, -1021}}], BezierCurve[{{-3275, -795}, {-3723, -683}, {-4311, -683}}], Line[{{-5194, -683}}], BezierCurve[{{-5452, -683}, {-5647, -618}, {-5780, -490}}], BezierCurve[{{-5914, -361}, {-5980, -182}, {-5980, 48}}], BezierCurve[{{-5980, 278}, {-5914, 457}, {-5780, 586}}], BezierCurve[{{-5647, 715}, {-5452, 779}, {-5194, 779}}], Line[{{-4477, 779}}], BezierCurve[{{-4128, 779}, {-3829, 837}, {-3580, 952}}], BezierCurve[{{-3332, 1067}, {-3139, 1230}, {-3001, 1441}}], BezierCurve[{{-2863, 1653}, {-2794, 1910}, {-2794, 2214}}], BezierCurve[{{-2794, 2637}, {-2934, 2959}, {-3215, 3179}}], BezierCurve[{{-3495, 3400}, {-3902, 3510}, {-4436, 3510}}], BezierCurve[{{-4776, 3510}, {-5095, 3471}, {-5394, 3393}}], BezierCurve[{{-5693, 3315}, {-6022, 3170}, {-6380, 2959}}], BezierCurve[{{-6546, 2857}, {-6705, 2816}, {-6856, 2834}}], BezierCurve[{{-7008, 2853}, {-7134, 2913}, {-7236, 3014}}], BezierCurve[{{-7337, 3115}, {-7406, 3237}, {-7443, 3379}}], BezierCurve[{{-7479, 3522}, {-7470, 3669}, {-7415, 3821}}], BezierCurve[{{-7360, 3972}, {-7245, 4103}, {-7070, 4214}}], BezierCurve[{{-6675, 4471}, {-6238, 4667}, {-5760, 4800}}], BezierCurve[{{-5282, 4933}, {-4790, 5000}, {-4284, 5000}}], BezierCurve[{{-3613, 5000}, {-3038, 4897}, {-2560, 4690}}], BezierCurve[{{-2082, 4483}, {-1716, 4189}, {-1463, 3807}}], BezierCurve[{{-1210, 3425}, {-1084, 2963}, {-1084, 2421}}], BezierCurve[{{-1084, 2034}, {-1157, 1683}, {-1305, 1366}}], BezierCurve[{{-1452, 1048}, {-1659, 779}, {-1925, 559}}], BezierCurve[{{-2115, 402}, {-2332, 278}, {-2576, 186}}], BezierCurve[{{-2063, 50}, {-1653, -201}, {-1346, -566}}], BezierCurve[{{-978, -1002}, {-794, -1547}, {-794, -2200}}], BezierCurve[{{-794, -2770}, {-937, -3267}, {-1222, -3690}}], BezierCurve[{{-1507, -4113}, {-1911, -4437}, {-2436, -4662}}], BezierCurve[{{-2960, -4887}, {-3585, -5000}, {-4311, -5000}}]}}], FilledCurve[{{Line[{{-2576, 186}}], BezierCurve[{{-2677, 148}, {-2781, 116}, {-2891, 90}}], Line[{{-2891, 255}}], BezierCurve[{{-2782, 237}, {-2677, 214}, {-2576, 186}}]}}], FilledCurve[{{Line[{{4157, -5000}}], BezierCurve[{{3008, -5000}, {2130, -4566}, {1523, -3697}}], BezierCurve[{{916, -2828}, {613, -1589}, {613, 21}}], BezierCurve[{{613, 1106}, {748, 2018}, {1020, 2759}}], BezierCurve[{{1291, 3499}, {1691, 4057}, {2220, 4434}}], BezierCurve[{{2748, 4811}, {3394, 5000}, {4157, 5000}}], BezierCurve[{{5316, 5000}, {6197, 4577}, {6799, 3731}}], BezierCurve[{{7401, 2885}, {7702, 1653}, {7702, 34}}], BezierCurve[{{7702, -1041}, {7567, -1954}, {7295, -2703}}], BezierCurve[{{7024, -3453}, {6626, -4023}, {6102, -4414}}], BezierCurve[{{5578, -4805}, {4930, -5000}, {4157, -5000}}]}, {Line[{{4157, -3552}}], BezierCurve[{{4783, -3552}, {5245, -3262}, {5544, -2683}}], BezierCurve[{{5843, -2103}, {5992, -1198}, {5992, 34}}], BezierCurve[{{5992, 1267}, {5843, 2161}, {5544, 2717}}], BezierCurve[{{5245, 3274}, {4783, 3552}, {4157, 3552}}], BezierCurve[{{3541, 3552}, {3082, 3274}, {2778, 2717}}], BezierCurve[{{2475, 2161}, {2323, 1267}, {2323, 34}}], BezierCurve[{{2323, -1198}, {2475, -2103}, {2778, -2683}}], BezierCurve[{{3082, -3262}, {3541, -3552}, {4157, -3552}}]}}]},
  "40" -> {FilledCurve[{{Line[{{-2517, -4972}}], BezierCurve[{{-2793, -4972}, {-3009, -4897}, {-3166, -4745}}], BezierCurve[{{-3322, -4593}, {-3400, -4375}, {-3400, -4090}}], Line[{{-3400, -3014}}], Line[{{-6807, -3014}}], BezierCurve[{{-7092, -3014}, {-7320, -2954}, {-7490, -2834}}], BezierCurve[{{-7660, -2715}, {-7745, -2526}, {-7745, -2269}}], BezierCurve[{{-7745, -2122}, {-7706, -1961}, {-7628, -1786}}], BezierCurve[{{-7549, -1611}, {-7409, -1382}, {-7207, -1097}}], Line[{{-3428, 4379}}], BezierCurve[{{-3299, 4572}, {-3156, 4720}, {-3000, 4821}}], BezierCurve[{{-2844, 4922}, {-2664, 4972}, {-2462, 4972}}], BezierCurve[{{-2223, 4972}, {-2025, 4901}, {-1869, 4759}}], BezierCurve[{{-1713, 4616}, {-1634, 4393}, {-1634, 4090}}], Line[{{-1634, -1552}}], Line[{{-959, -1552}}], BezierCurve[{{-692, -1552}, {-492, -1614}, {-359, -1738}}], BezierCurve[{{-225, -1862}, {-159, -2044}, {-159, -2283}}], BezierCurve[{{-159, -2522}, {-225, -2703}, {-359, -2828}}], BezierCurve[{{-492, -2952}, {-692, -3014}, {-959, -3014}}], Line[{{-1634, -3014}}], Line[{{-1634, -4090}}], BezierCurve[{{-1634, -4375}, {-1713, -4593}, {-1869, -4745}}], BezierCurve[{{-2025, -4897}, {-2241, -4972}, {-2517, -4972}}]}, {Line[{{-3400, -1552}}], Line[{{-3400, 2165}}], Line[{{-5935, -1552}}], Line[{{-3400, -1552}}]}}], FilledCurve[{{Line[{{-5935, -1552}}], Line[{{-6255, -2021}}], Line[{{-6255, -1552}}], Line[{{-5935, -1552}}]}}], FilledCurve[{{Line[{{-3400, 2165}}], Line[{{-3400, 2752}}], Line[{{-3000, 2752}}], Line[{{-3400, 2165}}]}}], FilledCurve[{{Line[{{4200, -5000}}], BezierCurve[{{3051, -5000}, {2172, -4566}, {1566, -3697}}], BezierCurve[{{959, -2828}, {655, -1589}, {655, 21}}], BezierCurve[{{655, 1106}, {791, 2018}, {1062, 2759}}], BezierCurve[{{1333, 3499}, {1733, 4057}, {2262, 4434}}], BezierCurve[{{2791, 4811}, {3437, 5000}, {4200, 5000}}], BezierCurve[{{5359, 5000}, {6239, 4577}, {6841, 3731}}], BezierCurve[{{7444, 2885}, {7745, 1653}, {7745, 34}}], BezierCurve[{{7745, -1041}, {7609, -1954}, {7338, -2703}}], BezierCurve[{{7067, -3453}, {6669, -4023}, {6145, -4414}}], BezierCurve[{{5621, -4805}, {4972, -5000}, {4200, -5000}}]}, {Line[{{4200, -3552}}], BezierCurve[{{4825, -3552}, {5287, -3262}, {5586, -2683}}], BezierCurve[{{5885, -2103}, {6034, -1198}, {6034, 34}}], BezierCurve[{{6034, 1267}, {5885, 2161}, {5586, 2717}}], BezierCurve[{{5287, 3274}, {4825, 3552}, {4200, 3552}}], BezierCurve[{{3584, 3552}, {3124, 3274}, {2821, 2717}}], BezierCurve[{{2517, 2161}, {2366, 1267}, {2366, 34}}], BezierCurve[{{2366, -1198}, {2517, -2103}, {2821, -2683}}], BezierCurve[{{3124, -3262}, {3584, -3552}, {4200, -3552}}]}}]},
  "50" -> {FilledCurve[{{Line[{{-4215, -5000}}], BezierCurve[{{-4702, -5000}, {-5199, -4936}, {-5705, -4807}}], BezierCurve[{{-6210, -4678}, {-6675, -4480}, {-7098, -4214}}], BezierCurve[{{-7272, -4103}, {-7390, -3972}, {-7449, -3821}}], BezierCurve[{{-7509, -3669}, {-7523, -3517}, {-7491, -3366}}], BezierCurve[{{-7459, -3214}, {-7390, -3085}, {-7284, -2979}}], BezierCurve[{{-7178, -2874}, {-7049, -2811}, {-6898, -2793}}], BezierCurve[{{-6746, -2775}, {-6578, -2816}, {-6394, -2917}}], BezierCurve[{{-6054, -3110}, {-5709, -3257}, {-5360, -3359}}], BezierCurve[{{-5010, -3460}, {-4638, -3510}, {-4243, -3510}}], BezierCurve[{{-3856, -3510}, {-3525, -3441}, {-3249, -3303}}], BezierCurve[{{-2974, -3166}, {-2762, -2972}, {-2615, -2724}}], BezierCurve[{{-2468, -2476}, {-2394, -2186}, {-2394, -1855}}], BezierCurve[{{-2394, -1340}, {-2551, -924}, {-2863, -607}}], BezierCurve[{{-3176, -290}, {-3603, -131}, {-4146, -131}}], BezierCurve[{{-4440, -131}, {-4725, -179}, {-5001, -276}}], BezierCurve[{{-5277, -372}, {-5548, -540}, {-5815, -779}}], BezierCurve[{{-5898, -853}, {-6003, -924}, {-6132, -993}}], BezierCurve[{{-6261, -1062}, {-6399, -1097}, {-6546, -1097}}], BezierCurve[{{-6776, -1097}, {-6951, -1037}, {-7070, -917}}], BezierCurve[{{-7190, -798}, {-7249, -632}, {-7249, -421}}], Line[{{-7249, 4090}}], BezierCurve[{{-7249, 4338}, {-7183, 4529}, {-7049, 4662}}], BezierCurve[{{-6916, 4795}, {-6725, 4862}, {-6477, 4862}}], Line[{{-1939, 4862}}], BezierCurve[{{-1691, 4862}, {-1500, 4800}, {-1367, 4676}}], BezierCurve[{{-1233, 4552}, {-1167, 4370}, {-1167, 4131}}], BezierCurve[{{-1167, 3901}, {-1233, 3722}, {-1367, 3593}}], BezierCurve[{{-1500, 3464}, {-1691, 3400}, {-1939, 3400}}], Line[{{-5525, 3400}}], Line[{{-5525, 766}}], BezierCurve[{{-5377, 878}, {-5211, 975}, {-5029, 1055}}], BezierCurve[{{-4633, 1230}, {-4201, 1317}, {-3732, 1317}}], BezierCurve[{{-3116, 1317}, {-2580, 1186}, {-2125, 924}}], BezierCurve[{{-1670, 662}, {-1316, 299}, {-1063, -166}}], BezierCurve[{{-810, -630}, {-684, -1170}, {-684, -1786}}], BezierCurve[{{-684, -2421}, {-826, -2979}, {-1111, -3462}}], BezierCurve[{{-1397, -3945}, {-1803, -4322}, {-2332, -4593}}], BezierCurve[{{-2861, -4864}, {-3489, -5000}, {-4215, -5000}}]}}], FilledCurve[{{Line[{{-5525, 766}}], Line[{{-5525, 310}}], Line[{{-5980, 310}}], BezierCurve[{{-5852, 484}, {-5700, 636}, {-5525, 766}}]}}], FilledCurve[{{Line[{{3978, -5000}}], BezierCurve[{{2829, -5000}, {1951, -4566}, {1344, -3697}}], BezierCurve[{{737, -2828}, {433, -1589}, {433, 21}}], BezierCurve[{{433, 1106}, {569, 2018}, {840, 2759}}], BezierCurve[{{1111, 3499}, {1511, 4057}, {2040, 4434}}], BezierCurve[{{2569, 4811}, {3215, 5000}, {3978, 5000}}], BezierCurve[{{5137, 5000}, {6017, 4577}, {6620, 3731}}], BezierCurve[{{7222, 2885}, {7523, 1653}, {7523, 34}}], BezierCurve[{{7523, -1041}, {7387, -1954}, {7116, -2703}}], BezierCurve[{{6845, -3453}, {6447, -4023}, {5923, -4414}}], BezierCurve[{{5399, -4805}, {4751, -5000}, {3978, -5000}}]}, {Line[{{3978, -3552}}], BezierCurve[{{4603, -3552}, {5066, -3262}, {5364, -2683}}], BezierCurve[{{5663, -2103}, {5813, -1198}, {5813, 34}}], BezierCurve[{{5813, 1267}, {5663, 2161}, {5364, 2717}}], BezierCurve[{{5066, 3274}, {4603, 3552}, {3978, 3552}}], BezierCurve[{{3362, 3552}, {2902, 3274}, {2599, 2717}}], BezierCurve[{{2295, 2161}, {2144, 1267}, {2144, 34}}], BezierCurve[{{2144, -1198}, {2295, -2103}, {2599, -2683}}], BezierCurve[{{2902, -3262}, {3362, -3552}, {3978, -3552}}]}}]},
  "60" -> {FilledCurve[{{Line[{{-3841, -5000}}], BezierCurve[{{-4641, -5000}, {-5324, -4809}, {-5890, -4428}}], BezierCurve[{{-6455, -4046}, {-6885, -3497}, {-7179, -2779}}], BezierCurve[{{-7474, -2062}, {-7621, -1193}, {-7621, -172}}], BezierCurve[{{-7621, 655}, {-7526, 1389}, {-7338, 2028}}], BezierCurve[{{-7149, 2667}, {-6876, 3207}, {-6517, 3648}}], BezierCurve[{{-6159, 4090}, {-5720, 4425}, {-5200, 4655}}], BezierCurve[{{-4680, 4885}, {-4094, 5000}, {-3441, 5000}}], BezierCurve[{{-3037, 5000}, {-2625, 4938}, {-2207, 4814}}], BezierCurve[{{-1789, 4690}, {-1395, 4503}, {-1028, 4255}}], BezierCurve[{{-880, 4163}, {-784, 4044}, {-738, 3897}}], BezierCurve[{{-692, 3749}, {-685, 3605}, {-717, 3462}}], BezierCurve[{{-749, 3320}, {-814, 3195}, {-910, 3090}}], BezierCurve[{{-1007, 2984}, {-1129, 2920}, {-1276, 2897}}], BezierCurve[{{-1423, 2874}, {-1584, 2913}, {-1759, 3014}}], BezierCurve[{{-2071, 3198}, {-2366, 3326}, {-2641, 3400}}], BezierCurve[{{-2917, 3474}, {-3193, 3510}, {-3469, 3510}}], BezierCurve[{{-4002, 3510}, {-4453, 3386}, {-4821, 3138}}], BezierCurve[{{-5189, 2890}, {-5467, 2517}, {-5655, 2021}}], BezierCurve[{{-5844, 1524}, {-5938, 903}, {-5938, 159}}], Line[{{-5938, -237}}], BezierCurve[{{-5860, -30}, {-5761, 159}, {-5641, 331}}], BezierCurve[{{-5407, 667}, {-5106, 924}, {-4738, 1103}}], BezierCurve[{{-4370, 1283}, {-3961, 1372}, {-3510, 1372}}], BezierCurve[{{-2949, 1372}, {-2453, 1239}, {-2021, 972}}], BezierCurve[{{-1589, 706}, {-1248, 338}, {-1000, -131}}], BezierCurve[{{-752, -600}, {-628, -1138}, {-628, -1745}}], BezierCurve[{{-628, -2370}, {-763, -2929}, {-1034, -3421}}], BezierCurve[{{-1306, -3913}, {-1683, -4299}, {-2166, -4579}}], BezierCurve[{{-2648, -4860}, {-3207, -5000}, {-3841, -5000}}]}, {Line[{{-3938, -3593}}], BezierCurve[{{-3607, -3593}, {-3315, -3520}, {-3062, -3372}}], BezierCurve[{{-2809, -3225}, {-2614, -3016}, {-2476, -2745}}], BezierCurve[{{-2338, -2474}, {-2269, -2163}, {-2269, -1814}}], BezierCurve[{{-2269, -1455}, {-2338, -1143}, {-2476, -876}}], BezierCurve[{{-2614, -609}, {-2809, -400}, {-3062, -248}}], BezierCurve[{{-3315, -97}, {-3607, -21}, {-3938, -21}}], BezierCurve[{{-4269, -21}, {-4559, -97}, {-4807, -248}}], BezierCurve[{{-5055, -400}, {-5251, -609}, {-5393, -876}}], BezierCurve[{{-5536, -1143}, {-5607, -1455}, {-5607, -1814}}], BezierCurve[{{-5607, -2163}, {-5536, -2474}, {-5393, -2745}}], BezierCurve[{{-5251, -3016}, {-5055, -3225}, {-4807, -3372}}], BezierCurve[{{-4559, -3520}, {-4269, -3593}, {-3938, -3593}}]}}], FilledCurve[{{Line[{{-5938, -237}}], Line[{{-5938, -876}}], Line[{{-6103, -876}}], BezierCurve[{{-6068, -647}, {-6012, -434}, {-5938, -237}}]}}], FilledCurve[{{Line[{{4076, -5000}}], BezierCurve[{{2926, -5000}, {2048, -4566}, {1441, -3697}}], BezierCurve[{{834, -2828}, {531, -1589}, {531, 21}}], BezierCurve[{{531, 1106}, {667, 2018}, {938, 2759}}], BezierCurve[{{1209, 3499}, {1609, 4057}, {2138, 4434}}], BezierCurve[{{2667, 4811}, {3313, 5000}, {4076, 5000}}], BezierCurve[{{5234, 5000}, {6115, 4577}, {6717, 3731}}], BezierCurve[{{7320, 2885}, {7621, 1653}, {7621, 34}}], BezierCurve[{{7621, -1041}, {7485, -1954}, {7214, -2703}}], BezierCurve[{{6943, -3453}, {6545, -4023}, {6021, -4414}}], BezierCurve[{{5497, -4805}, {4848, -5000}, {4076, -5000}}]}, {Line[{{4076, -3552}}], BezierCurve[{{4701, -3552}, {5163, -3262}, {5462, -2683}}], BezierCurve[{{5761, -2103}, {5910, -1198}, {5910, 34}}], BezierCurve[{{5910, 1267}, {5761, 2161}, {5462, 2717}}], BezierCurve[{{5163, 3274}, {4701, 3552}, {4076, 3552}}], BezierCurve[{{3460, 3552}, {3000, 3274}, {2697, 2717}}], BezierCurve[{{2393, 2161}, {2241, 1267}, {2241, 34}}], BezierCurve[{{2241, -1198}, {2393, -2103}, {2697, -2683}}], BezierCurve[{{3000, -3262}, {3460, -3552}, {4076, -3552}}]}}]}
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
