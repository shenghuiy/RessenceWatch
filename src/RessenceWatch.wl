(* ::Package:: *)

(* Ressence Type 1 style watch face drawn with graphics primitives; numerals are
   Arial Rounded MT Bold glyph outlines.

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

   They use Arial Rounded MT Bold. Its outlines are stored below, so the font
   need not be installed. They were made by exporting each string to PDF at
   FontSize 100 and importing it with "TextOutlines" -> True, then normalised
   to ink height 1 with the ink box centred at the origin, and are stored in
   units of 1/10000. Only the strings the dials use are included. *)
fontOutlines = <|
  "3" -> {FilledCurve[{{Line[{{-340, 1075}}], BezierCurve[{{67, 1075}, {421, 1195}, {718, 1435}}], BezierCurve[{{1011, 1676}, {1162, 2019}, {1162, 2470}}], BezierCurve[{{1162, 2810}, {1045, 3107}, {808, 3354}}], BezierCurve[{{574, 3601}, {254, 3725}, {-147, 3725}}], BezierCurve[{{-417, 3725}, {-641, 3688}, {-818, 3611}}], BezierCurve[{{-991, 3538}, {-1132, 3438}, {-1235, 3311}}], BezierCurve[{{-1335, 3188}, {-1432, 3027}, {-1525, 2830}}], BezierCurve[{{-1615, 2637}, {-1699, 2450}, {-1776, 2276}}], BezierCurve[{{-1819, 2183}, {-1899, 2109}, {-2016, 2056}}], BezierCurve[{{-2130, 2003}, {-2263, 1976}, {-2417, 1976}}], BezierCurve[{{-2593, 1976}, {-2757, 2046}, {-2907, 2193}}], BezierCurve[{{-3054, 2336}, {-3131, 2530}, {-3131, 2770}}], BezierCurve[{{-3131, 3001}, {-3061, 3244}, {-2921, 3501}}], BezierCurve[{{-2780, 3755}, {-2577, 3999}, {-2306, 4232}}], BezierCurve[{{-2039, 4463}, {-1702, 4650}, {-1302, 4790}}], BezierCurve[{{-901, 4930}, {-454, 5000}, {40, 5000}}], BezierCurve[{{471, 5000}, {864, 4943}, {1222, 4823}}], BezierCurve[{{1575, 4706}, {1886, 4536}, {2150, 4312}}], BezierCurve[{{2410, 4092}, {2610, 3832}, {2744, 3538}}], BezierCurve[{{2877, 3244}, {2944, 2931}, {2944, 2590}}], BezierCurve[{{2944, 2146}, {2847, 1766}, {2654, 1445}}], BezierCurve[{{2460, 1128}, {2183, 818}, {1822, 514}}], BezierCurve[{{2170, 327}, {2460, 113}, {2700, -127}}], BezierCurve[{{2937, -367}, {3117, -631}, {3238, -925}}], BezierCurve[{{3358, -1215}, {3418, -1529}, {3418, -1869}}], BezierCurve[{{3418, -2273}, {3338, -2664}, {3174, -3044}}], BezierCurve[{{3014, -3421}, {2774, -3758}, {2457, -4055}}], BezierCurve[{{2143, -4349}, {1766, -4583}, {1332, -4750}}], BezierCurve[{{898, -4917}, {421, -5000}, {-107, -5000}}], BezierCurve[{{-641, -5000}, {-1118, -4907}, {-1542, -4713}}], BezierCurve[{{-1963, -4523}, {-2313, -4282}, {-2587, -3995}}], BezierCurve[{{-2860, -3708}, {-3067, -3411}, {-3208, -3104}}], BezierCurve[{{-3348, -2797}, {-3418, -2543}, {-3418, -2343}}], BezierCurve[{{-3418, -2086}, {-3334, -1879}, {-3168, -1719}}], BezierCurve[{{-3001, -1562}, {-2794, -1482}, {-2543, -1482}}], BezierCurve[{{-2420, -1482}, {-2300, -1519}, {-2183, -1592}}], BezierCurve[{{-2069, -1666}, {-1993, -1752}, {-1956, -1856}}], BezierCurve[{{-1726, -2473}, {-1479, -2934}, {-1212, -3234}}], BezierCurve[{{-948, -3535}, {-574, -3685}, {-93, -3685}}], BezierCurve[{{180, -3685}, {447, -3618}, {704, -3481}}], BezierCurve[{{958, -3348}, {1172, -3144}, {1338, -2877}}], BezierCurve[{{1505, -2610}, {1589, -2303}, {1589, -1949}}], BezierCurve[{{1589, -1429}, {1449, -1021}, {1162, -724}}], BezierCurve[{{878, -431}, {481, -280}, {-27, -280}}], BezierCurve[{{-113, -280}, {-254, -287}, {-441, -307}}], BezierCurve[{{-628, -324}, {-748, -334}, {-801, -334}}], BezierCurve[{{-1045, -334}, {-1235, -274}, {-1368, -150}}], BezierCurve[{{-1502, -30}, {-1569, 140}, {-1569, 360}}], BezierCurve[{{-1569, 574}, {-1489, 744}, {-1328, 878}}], BezierCurve[{{-1168, 1008}, {-931, 1075}, {-614, 1075}}], Line[{{-340, 1075}}]}}]},
  "6" -> {FilledCurve[{{Line[{{-1643, 453}}], BezierCurve[{{-1343, 813}, {-1013, 1080}, {-657, 1253}}], BezierCurve[{{-303, 1427}, {97, 1513}, {537, 1513}}], BezierCurve[{{957, 1513}, {1347, 1437}, {1707, 1277}}], BezierCurve[{{2063, 1120}, {2377, 897}, {2647, 603}}], BezierCurve[{{2913, 313}, {3123, -30}, {3270, -427}}], BezierCurve[{{3417, -820}, {3490, -1233}, {3490, -1667}}], BezierCurve[{{3490, -2293}, {3347, -2860}, {3060, -3373}}], BezierCurve[{{2773, -3883}, {2373, -4283}, {1860, -4570}}], BezierCurve[{{1347, -4857}, {767, -5000}, {117, -5000}}], BezierCurve[{{-630, -5000}, {-1273, -4820}, {-1817, -4453}}], BezierCurve[{{-2357, -4090}, {-2773, -3550}, {-3060, -2837}}], BezierCurve[{{-3347, -2123}, {-3490, -1253}, {-3490, -220}}], BezierCurve[{{-3490, 620}, {-3403, 1370}, {-3230, 2033}}], BezierCurve[{{-3057, 2693}, {-2810, 3243}, {-2483, 3680}}], BezierCurve[{{-2160, 4113}, {-1767, 4443}, {-1303, 4667}}], BezierCurve[{{-843, 4887}, {-317, 5000}, {277, 5000}}], BezierCurve[{{843, 5000}, {1347, 4890}, {1780, 4667}}], BezierCurve[{{2213, 4447}, {2540, 4170}, {2760, 3840}}], BezierCurve[{{2980, 3513}, {3090, 3197}, {3090, 2893}}], BezierCurve[{{3090, 2713}, {3023, 2547}, {2883, 2397}}], BezierCurve[{{2747, 2250}, {2570, 2173}, {2357, 2173}}], BezierCurve[{{2170, 2173}, {1997, 2230}, {1837, 2350}}], BezierCurve[{{1677, 2467}, {1560, 2630}, {1483, 2840}}], BezierCurve[{{1383, 3107}, {1220, 3320}, {997, 3483}}], BezierCurve[{{777, 3643}, {517, 3727}, {223, 3727}}], BezierCurve[{{-10, 3727}, {-247, 3660}, {-480, 3523}}], BezierCurve[{{-713, 3390}, {-910, 3207}, {-1070, 2973}}], BezierCurve[{{-1393, 2503}, {-1583, 1663}, {-1643, 453}}]}, {Line[{{157, -3687}}], BezierCurve[{{603, -3687}, {977, -3503}, {1270, -3137}}], BezierCurve[{{1563, -2770}, {1710, -2290}, {1710, -1693}}], BezierCurve[{{1710, -1290}, {1643, -943}, {1503, -650}}], BezierCurve[{{1367, -360}, {1177, -137}, {937, 17}}], BezierCurve[{{697, 170}, {433, 247}, {143, 247}}], BezierCurve[{{-130, 247}, {-393, 173}, {-647, 27}}], BezierCurve[{{-897, -120}, {-1100, -330}, {-1253, -610}}], BezierCurve[{{-1407, -887}, {-1483, -1213}, {-1483, -1587}}], BezierCurve[{{-1483, -2187}, {-1330, -2687}, {-1017, -3087}}], BezierCurve[{{-707, -3487}, {-317, -3687}, {157, -3687}}]}}]},
  "9" -> {FilledCurve[{{Line[{{1636, -461}}], BezierCurve[{{1342, -808}, {1015, -1068}, {651, -1245}}], BezierCurve[{{290, -1419}, {-113, -1509}, {-554, -1509}}], BezierCurve[{{-971, -1509}, {-1362, -1429}, {-1726, -1265}}], BezierCurve[{{-2086, -1105}, {-2400, -878}, {-2667, -584}}], BezierCurve[{{-2931, -294}, {-3134, 47}, {-3278, 437}}], BezierCurve[{{-3418, 824}, {-3491, 1235}, {-3491, 1662}}], BezierCurve[{{-3491, 2119}, {-3411, 2553}, {-3251, 2967}}], BezierCurve[{{-3091, 3378}, {-2864, 3735}, {-2563, 4039}}], BezierCurve[{{-2266, 4339}, {-1913, 4576}, {-1499, 4746}}], BezierCurve[{{-1088, 4913}, {-634, 5000}, {-134, 5000}}], BezierCurve[{{441, 5000}, {951, 4897}, {1402, 4686}}], BezierCurve[{{1849, 4479}, {2230, 4176}, {2537, 3775}}], BezierCurve[{{2844, 3378}, {3077, 2877}, {3244, 2270}}], BezierCurve[{{3408, 1666}, {3491, 981}, {3491, 214}}], BezierCurve[{{3491, -641}, {3405, -1392}, {3231, -2046}}], BezierCurve[{{3057, -2697}, {2807, -3244}, {2473, -3688}}], BezierCurve[{{2143, -4129}, {1746, -4459}, {1282, -4676}}], BezierCurve[{{821, -4890}, {294, -5000}, {-294, -5000}}], BezierCurve[{{-871, -5000}, {-1375, -4890}, {-1802, -4670}}], BezierCurve[{{-2230, -4449}, {-2553, -4176}, {-2777, -3848}}], BezierCurve[{{-2997, -3521}, {-3111, -3208}, {-3111, -2904}}], BezierCurve[{{-3111, -2717}, {-3044, -2550}, {-2904, -2403}}], BezierCurve[{{-2767, -2256}, {-2590, -2183}, {-2377, -2183}}], BezierCurve[{{-2183, -2183}, {-2006, -2236}, {-1849, -2346}}], BezierCurve[{{-1696, -2453}, {-1575, -2617}, {-1495, -2837}}], BezierCurve[{{-1399, -3104}, {-1235, -3318}, {-1008, -3481}}], BezierCurve[{{-781, -3642}, {-527, -3725}, {-240, -3725}}], BezierCurve[{{13, -3725}, {254, -3662}, {481, -3531}}], BezierCurve[{{708, -3405}, {901, -3221}, {1061, -2984}}], BezierCurve[{{1382, -2510}, {1572, -1669}, {1636, -461}}]}, {Line[{{1482, 1582}}], BezierCurve[{{1482, 1883}, {1442, 2163}, {1358, 2420}}], BezierCurve[{{1278, 2674}, {1162, 2897}, {1011, 3084}}], BezierCurve[{{864, 3271}, {684, 3418}, {477, 3525}}], BezierCurve[{{270, 3632}, {47, 3685}, {-200, 3685}}], BezierCurve[{{-621, 3685}, {-981, 3505}, {-1275, 3141}}], BezierCurve[{{-1569, 2780}, {-1716, 2300}, {-1716, 1702}}], BezierCurve[{{-1716, 1111}, {-1569, 641}, {-1275, 287}}], BezierCurve[{{-981, -63}, {-611, -240}, {-160, -240}}], BezierCurve[{{113, -240}, {377, -170}, {631, -23}}], BezierCurve[{{881, 120}, {1085, 330}, {1245, 607}}], BezierCurve[{{1402, 881}, {1482, 1208}, {1482, 1582}}]}}]},
  "10" -> {FilledCurve[{{Line[{{-4359, -3921}, {-4359, 2137}}], BezierCurve[{{-5484, 1272}, {-6243, 839}, {-6636, 839}}], BezierCurve[{{-6823, 839}, {-6986, 912}, {-7132, 1062}}], BezierCurve[{{-7275, 1208}, {-7349, 1381}, {-7349, 1578}}], BezierCurve[{{-7349, 1804}, {-7279, 1971}, {-7136, 2077}}], BezierCurve[{{-6996, 2184}, {-6743, 2320}, {-6383, 2490}}], BezierCurve[{{-5847, 2743}, {-5418, 3009}, {-5095, 3289}}], BezierCurve[{{-4775, 3569}, {-4489, 3881}, {-4239, 4228}}], BezierCurve[{{-3993, 4574}, {-3830, 4787}, {-3753, 4867}}], BezierCurve[{{-3680, 4947}, {-3537, 4987}, {-3327, 4987}}], BezierCurve[{{-3094, 4987}, {-2905, 4897}, {-2761, 4714}}], BezierCurve[{{-2622, 4534}, {-2548, 4281}, {-2548, 3961}}], Line[{{-2548, -3662}}], BezierCurve[{{-2548, -4554}, {-2851, -5000}, {-3460, -5000}}], BezierCurve[{{-3730, -5000}, {-3946, -4910}, {-4113, -4727}}], BezierCurve[{{-4276, -4547}, {-4359, -4278}, {-4359, -3921}}]}}], FilledCurve[{{Line[{{7349, -47}}], BezierCurve[{{7349, -772}, {7305, -1398}, {7215, -1924}}], BezierCurve[{{7129, -2447}, {6962, -2923}, {6723, -3349}}], BezierCurve[{{6423, -3872}, {6030, -4274}, {5551, -4561}}], BezierCurve[{{5072, -4844}, {4539, -4987}, {3953, -4987}}], BezierCurve[{{3281, -4987}, {2675, -4797}, {2139, -4414}}], BezierCurve[{{1606, -4035}, {1203, -3505}, {930, -2830}}], BezierCurve[{{794, -2463}, {694, -2054}, {628, -1608}}], BezierCurve[{{564, -1162}, {531, -679}, {531, -160}}], BezierCurve[{{531, 509}, {564, 1112}, {638, 1651}}], BezierCurve[{{707, 2187}, {817, 2650}, {970, 3036}}], BezierCurve[{{1230, 3675}, {1613, 4161}, {2112, 4497}}], BezierCurve[{{2612, 4830}, {3207, 5000}, {3900, 5000}}], BezierCurve[{{4356, 5000}, {4772, 4927}, {5145, 4777}}], BezierCurve[{{5518, 4630}, {5844, 4411}, {6127, 4125}}], BezierCurve[{{6406, 3838}, {6646, 3485}, {6843, 3063}}], BezierCurve[{{7179, 2330}, {7349, 1295}, {7349, -47}}]}, {Line[{{5524, 87}}], BezierCurve[{{5524, 899}, {5478, 1561}, {5378, 2081}}], BezierCurve[{{5281, 2597}, {5118, 2986}, {4885, 3252}}], BezierCurve[{{4655, 3515}, {4336, 3648}, {3926, 3648}}], BezierCurve[{{3341, 3648}, {2934, 3356}, {2705, 2763}}], BezierCurve[{{2478, 2174}, {2362, 1265}, {2362, 33}}], BezierCurve[{{2362, -799}, {2408, -1485}, {2508, -2017}}], BezierCurve[{{2605, -2550}, {2768, -2953}, {3001, -3226}}], BezierCurve[{{3231, -3499}, {3544, -3635}, {3940, -3635}}], BezierCurve[{{4346, -3635}, {4665, -3495}, {4899, -3209}}], BezierCurve[{{5128, -2926}, {5291, -2520}, {5385, -1991}}], BezierCurve[{{5478, -1465}, {5524, -772}, {5524, 87}}]}}]},
  "20" -> {FilledCurve[{{Line[{{-5118, -3273}, {-1638, -3273}}], BezierCurve[{{-1292, -3273}, {-1028, -3343}, {-845, -3487}}], BezierCurve[{{-665, -3627}, {-572, -3820}, {-572, -4067}}], BezierCurve[{{-572, -4283}, {-642, -4467}, {-788, -4620}}], BezierCurve[{{-932, -4770}, {-1152, -4847}, {-1445, -4847}}], Line[{{-6352, -4847}}], BezierCurve[{{-6685, -4847}, {-6945, -4757}, {-7132, -4570}}], BezierCurve[{{-7318, -4387}, {-7412, -4170}, {-7412, -3920}}], BezierCurve[{{-7412, -3760}, {-7352, -3550}, {-7232, -3283}}], BezierCurve[{{-7112, -3020}, {-6982, -2813}, {-6838, -2660}}], BezierCurve[{{-6248, -2047}, {-5715, -1523}, {-5238, -1083}}], BezierCurve[{{-4765, -647}, {-4425, -360}, {-4218, -220}}], BezierCurve[{{-3855, 37}, {-3552, 297}, {-3308, 557}}], BezierCurve[{{-3068, 817}, {-2882, 1083}, {-2755, 1357}}], BezierCurve[{{-2628, 1630}, {-2565, 1897}, {-2565, 2160}}], BezierCurve[{{-2565, 2443}, {-2632, 2697}, {-2768, 2923}}], BezierCurve[{{-2902, 3147}, {-3088, 3320}, {-3322, 3447}}], BezierCurve[{{-3555, 3570}, {-3808, 3633}, {-4085, 3633}}], BezierCurve[{{-4665, 3633}, {-5125, 3380}, {-5458, 2867}}], BezierCurve[{{-5502, 2800}, {-5575, 2620}, {-5682, 2320}}], BezierCurve[{{-5785, 2023}, {-5902, 1793}, {-6035, 1633}}], BezierCurve[{{-6165, 1473}, {-6358, 1393}, {-6612, 1393}}], BezierCurve[{{-6832, 1393}, {-7018, 1467}, {-7165, 1613}}], BezierCurve[{{-7312, 1760}, {-7385, 1960}, {-7385, 2213}}], BezierCurve[{{-7385, 2520}, {-7318, 2840}, {-7178, 3173}}], BezierCurve[{{-7042, 3507}, {-6835, 3807}, {-6562, 4080}}], BezierCurve[{{-6288, 4350}, {-5942, 4570}, {-5522, 4737}}], BezierCurve[{{-5102, 4903}, {-4612, 4987}, {-4045, 4987}}], BezierCurve[{{-3365, 4987}, {-2785, 4880}, {-2305, 4667}}], BezierCurve[{{-1995, 4527}, {-1722, 4330}, {-1485, 4080}}], BezierCurve[{{-1252, 3833}, {-1068, 3543}, {-935, 3217}}], BezierCurve[{{-805, 2890}, {-738, 2553}, {-738, 2200}}], BezierCurve[{{-738, 1650}, {-875, 1150}, {-1148, 697}}], BezierCurve[{{-1422, 247}, {-1698, -107}, {-1985, -363}}], BezierCurve[{{-2268, -617}, {-2745, -1020}, {-3415, -1567}}], BezierCurve[{{-4082, -2113}, {-4542, -2537}, {-4792, -2840}}], BezierCurve[{{-4898, -2960}, {-5005, -3103}, {-5118, -3273}}]}}], FilledCurve[{{Line[{{7412, -53}}], BezierCurve[{{7412, -780}, {7368, -1407}, {7278, -1933}}], BezierCurve[{{7192, -2457}, {7025, -2933}, {6785, -3360}}], BezierCurve[{{6485, -3883}, {6092, -4287}, {5612, -4573}}], BezierCurve[{{5132, -4857}, {4598, -5000}, {4012, -5000}}], BezierCurve[{{3338, -5000}, {2732, -4810}, {2195, -4427}}], BezierCurve[{{1662, -4047}, {1258, -3517}, {985, -2840}}], BezierCurve[{{848, -2473}, {748, -2063}, {682, -1617}}], BezierCurve[{{618, -1170}, {585, -687}, {585, -167}}], BezierCurve[{{585, 503}, {618, 1107}, {692, 1647}}], BezierCurve[{{762, 2183}, {872, 2647}, {1025, 3033}}], BezierCurve[{{1285, 3673}, {1668, 4160}, {2168, 4497}}], BezierCurve[{{2668, 4830}, {3265, 5000}, {3958, 5000}}], BezierCurve[{{4415, 5000}, {4832, 4927}, {5205, 4777}}], BezierCurve[{{5578, 4630}, {5905, 4410}, {6188, 4123}}], BezierCurve[{{6468, 3837}, {6708, 3483}, {6905, 3060}}], BezierCurve[{{7242, 2327}, {7412, 1290}, {7412, -53}}]}, {Line[{{5585, 80}}], BezierCurve[{{5585, 893}, {5538, 1557}, {5438, 2077}}], BezierCurve[{{5342, 2593}, {5178, 2983}, {4945, 3250}}], BezierCurve[{{4715, 3513}, {4395, 3647}, {3985, 3647}}], BezierCurve[{{3398, 3647}, {2992, 3353}, {2762, 2760}}], BezierCurve[{{2535, 2170}, {2418, 1260}, {2418, 27}}], BezierCurve[{{2418, -807}, {2465, -1493}, {2565, -2027}}], BezierCurve[{{2662, -2560}, {2825, -2963}, {3058, -3237}}], BezierCurve[{{3288, -3510}, {3602, -3647}, {3998, -3647}}], BezierCurve[{{4405, -3647}, {4725, -3507}, {4958, -3220}}], BezierCurve[{{5188, -2937}, {5352, -2530}, {5445, -2000}}], BezierCurve[{{5538, -1473}, {5585, -780}, {5585, 80}}]}}]},
  "30" -> {FilledCurve[{{Line[{{-4376, 1059}}], BezierCurve[{{-3970, 1059}, {-3617, 1178}, {-3321, 1418}}], BezierCurve[{{-3028, 1658}, {-2878, 2001}, {-2878, 2450}}], BezierCurve[{{-2878, 2790}, {-2994, 3086}, {-3231, 3332}}], BezierCurve[{{-3464, 3579}, {-3783, 3702}, {-4183, 3702}}], BezierCurve[{{-4452, 3702}, {-4675, 3665}, {-4852, 3589}}], BezierCurve[{{-5025, 3515}, {-5165, 3415}, {-5268, 3289}}], BezierCurve[{{-5368, 3166}, {-5464, 3006}, {-5558, 2810}}], BezierCurve[{{-5648, 2617}, {-5731, 2430}, {-5807, 2257}}], BezierCurve[{{-5851, 2164}, {-5930, 2091}, {-6047, 2037}}], BezierCurve[{{-6160, 1984}, {-6293, 1957}, {-6446, 1957}}], BezierCurve[{{-6623, 1957}, {-6786, 2027}, {-6936, 2174}}], BezierCurve[{{-7082, 2317}, {-7159, 2510}, {-7159, 2750}}], BezierCurve[{{-7159, 2979}, {-7089, 3222}, {-6949, 3479}}], BezierCurve[{{-6809, 3732}, {-6606, 3975}, {-6337, 4208}}], BezierCurve[{{-6070, 4437}, {-5734, 4624}, {-5335, 4764}}], BezierCurve[{{-4935, 4903}, {-4489, 4973}, {-3996, 4973}}], BezierCurve[{{-3567, 4973}, {-3174, 4917}, {-2818, 4797}}], BezierCurve[{{-2465, 4680}, {-2155, 4511}, {-1893, 4288}}], BezierCurve[{{-1633, 4068}, {-1433, 3808}, {-1300, 3515}}], BezierCurve[{{-1167, 3222}, {-1100, 2909}, {-1100, 2570}}], BezierCurve[{{-1100, 2127}, {-1197, 1748}, {-1390, 1428}}], BezierCurve[{{-1583, 1112}, {-1859, 802}, {-2219, 499}}], BezierCurve[{{-1873, 313}, {-1583, 100}, {-1343, -140}}], BezierCurve[{{-1107, -379}, {-927, -642}, {-807, -935}}], BezierCurve[{{-687, -1225}, {-628, -1538}, {-628, -1877}}], BezierCurve[{{-628, -2280}, {-707, -2670}, {-871, -3049}}], BezierCurve[{{-1030, -3425}, {-1270, -3762}, {-1586, -4058}}], BezierCurve[{{-1899, -4351}, {-2275, -4584}, {-2708, -4750}}], BezierCurve[{{-3141, -4917}, {-3617, -5000}, {-4143, -5000}}], BezierCurve[{{-4675, -5000}, {-5152, -4907}, {-5574, -4714}}], BezierCurve[{{-5994, -4524}, {-6343, -4284}, {-6616, -3998}}], BezierCurve[{{-6889, -3712}, {-7096, -3415}, {-7235, -3109}}], BezierCurve[{{-7375, -2803}, {-7445, -2550}, {-7445, -2350}}], BezierCurve[{{-7445, -2094}, {-7362, -1887}, {-7195, -1728}}], BezierCurve[{{-7029, -1571}, {-6823, -1491}, {-6573, -1491}}], BezierCurve[{{-6450, -1491}, {-6330, -1528}, {-6213, -1601}}], BezierCurve[{{-6100, -1674}, {-6024, -1761}, {-5987, -1864}}], BezierCurve[{{-5757, -2480}, {-5511, -2939}, {-5245, -3239}}], BezierCurve[{{-4982, -3539}, {-4609, -3688}, {-4130, -3688}}], BezierCurve[{{-3857, -3688}, {-3590, -3622}, {-3334, -3485}}], BezierCurve[{{-3081, -3352}, {-2868, -3149}, {-2701, -2883}}], BezierCurve[{{-2535, -2617}, {-2452, -2310}, {-2452, -1957}}], BezierCurve[{{-2452, -1438}, {-2592, -1032}, {-2878, -736}}], BezierCurve[{{-3161, -443}, {-3557, -293}, {-4063, -293}}], BezierCurve[{{-4150, -293}, {-4289, -300}, {-4476, -320}}], BezierCurve[{{-4662, -336}, {-4782, -346}, {-4835, -346}}], BezierCurve[{{-5078, -346}, {-5268, -286}, {-5401, -163}}], BezierCurve[{{-5534, -43}, {-5601, 126}, {-5601, 346}}], BezierCurve[{{-5601, 559}, {-5521, 729}, {-5361, 862}}], BezierCurve[{{-5201, 992}, {-4965, 1059}, {-4649, 1059}}], Line[{{-4376, 1059}}]}}], FilledCurve[{{Line[{{7445, -47}}], BezierCurve[{{7445, -772}, {7402, -1398}, {7312, -1924}}], BezierCurve[{{7225, -2447}, {7059, -2923}, {6819, -3349}}], BezierCurve[{{6520, -3872}, {6127, -4274}, {5648, -4561}}], BezierCurve[{{5168, -4844}, {4636, -4987}, {4050, -4987}}], BezierCurve[{{3377, -4987}, {2771, -4797}, {2235, -4414}}], BezierCurve[{{1703, -4035}, {1300, -3505}, {1027, -2830}}], BezierCurve[{{891, -2463}, {791, -2054}, {724, -1608}}], BezierCurve[{{661, -1162}, {628, -679}, {628, -160}}], BezierCurve[{{628, 509}, {661, 1112}, {734, 1651}}], BezierCurve[{{804, 2187}, {914, 2650}, {1067, 3036}}], BezierCurve[{{1327, 3675}, {1709, 4161}, {2209, 4497}}], BezierCurve[{{2708, 4830}, {3304, 5000}, {3996, 5000}}], BezierCurve[{{4452, 5000}, {4869, 4927}, {5241, 4777}}], BezierCurve[{{5614, 4630}, {5940, 4411}, {6223, 4125}}], BezierCurve[{{6503, 3838}, {6743, 3485}, {6939, 3063}}], BezierCurve[{{7275, 2330}, {7445, 1295}, {7445, -47}}]}, {Line[{{5621, 87}}], BezierCurve[{{5621, 899}, {5574, 1561}, {5474, 2081}}], BezierCurve[{{5378, 2597}, {5215, 2986}, {4982, 3252}}], BezierCurve[{{4752, 3515}, {4432, 3648}, {4023, 3648}}], BezierCurve[{{3437, 3648}, {3031, 3356}, {2801, 2763}}], BezierCurve[{{2575, 2174}, {2458, 1265}, {2458, 33}}], BezierCurve[{{2458, -799}, {2505, -1485}, {2605, -2017}}], BezierCurve[{{2701, -2550}, {2865, -2953}, {3098, -3226}}], BezierCurve[{{3327, -3499}, {3640, -3635}, {4036, -3635}}], BezierCurve[{{4442, -3635}, {4762, -3495}, {4995, -3209}}], BezierCurve[{{5225, -2926}, {5388, -2520}, {5481, -1991}}], BezierCurve[{{5574, -1465}, {5621, -772}, {5621, 87}}]}}]},
  "40" -> {FilledCurve[{{Line[{{-3071, -3993}, {-3071, -2762}}], Line[{{-6396, -2762}}], BezierCurve[{{-6813, -2762}, {-7128, -2669}, {-7339, -2477}}], BezierCurve[{{-7548, -2288}, {-7654, -2030}, {-7654, -1702}}], BezierCurve[{{-7654, -1619}, {-7641, -1533}, {-7608, -1447}}], BezierCurve[{{-7578, -1361}, {-7531, -1272}, {-7469, -1175}}], BezierCurve[{{-7409, -1083}, {-7343, -990}, {-7277, -901}}], BezierCurve[{{-7210, -815}, {-7131, -705}, {-7031, -576}}], Line[{{-3522, 4119}}], BezierCurve[{{-3293, 4427}, {-3104, 4652}, {-2955, 4791}}], BezierCurve[{{-2810, 4930}, {-2631, 5000}, {-2422, 5000}}], BezierCurve[{{-1753, 5000}, {-1416, 4616}, {-1416, 3848}}], Line[{{-1416, -1278}}], Line[{{-1131, -1278}}], BezierCurve[{{-793, -1278}, {-518, -1325}, {-313, -1417}}], BezierCurve[{{-108, -1510}, {-5, -1712}, {-5, -2026}}], BezierCurve[{{-5, -2281}, {-88, -2467}, {-257, -2586}}], BezierCurve[{{-422, -2702}, {-674, -2762}, {-1012, -2762}}], Line[{{-1416, -2762}}], Line[{{-1416, -3993}}], BezierCurve[{{-1416, -4331}, {-1488, -4586}, {-1641, -4752}}], BezierCurve[{{-1790, -4917}, {-1992, -5000}, {-2243, -5000}}], BezierCurve[{{-2488, -5000}, {-2690, -4917}, {-2843, -4745}}], BezierCurve[{{-2995, -4576}, {-3071, -4325}, {-3071, -3993}}]}, {Line[{{-5919, -1278}, {-3071, -1278}}], Line[{{-3071, 2570}}], Line[{{-5919, -1278}}]}}], FilledCurve[{{Line[{{7654, -73}}], BezierCurve[{{7654, -795}, {7611, -1417}, {7522, -1940}}], BezierCurve[{{7435, -2460}, {7270, -2934}, {7031, -3358}}], BezierCurve[{{6733, -3877}, {6343, -4278}, {5866, -4563}}], BezierCurve[{{5389, -4844}, {4859, -4987}, {4277, -4987}}], BezierCurve[{{3608, -4987}, {3005, -4798}, {2472, -4417}}], BezierCurve[{{1942, -4040}, {1541, -3513}, {1270, -2841}}], BezierCurve[{{1134, -2477}, {1035, -2070}, {969, -1626}}], BezierCurve[{{906, -1182}, {873, -702}, {873, -185}}], BezierCurve[{{873, 480}, {906, 1079}, {979, 1616}}], BezierCurve[{{1048, 2149}, {1157, 2609}, {1310, 2993}}], BezierCurve[{{1568, 3629}, {1949, 4113}, {2445, 4447}}], BezierCurve[{{2942, 4778}, {3535, 4947}, {4224, 4947}}], BezierCurve[{{4677, 4947}, {5091, 4874}, {5462, 4725}}], BezierCurve[{{5833, 4579}, {6157, 4361}, {6439, 4076}}], BezierCurve[{{6717, 3791}, {6955, 3440}, {7151, 3020}}], BezierCurve[{{7485, 2291}, {7654, 1262}, {7654, -73}}]}, {Line[{{5839, 60}}], BezierCurve[{{5839, 868}, {5793, 1526}, {5694, 2043}}], BezierCurve[{{5598, 2556}, {5435, 2944}, {5204, 3209}}], BezierCurve[{{4975, 3470}, {4657, 3603}, {4250, 3603}}], BezierCurve[{{3667, 3603}, {3263, 3311}, {3035, 2722}}], BezierCurve[{{2810, 2136}, {2694, 1232}, {2694, 7}}], BezierCurve[{{2694, -821}, {2740, -1503}, {2839, -2033}}], BezierCurve[{{2935, -2563}, {3098, -2964}, {3330, -3235}}], BezierCurve[{{3558, -3507}, {3869, -3642}, {4263, -3642}}], BezierCurve[{{4667, -3642}, {4985, -3503}, {5217, -3219}}], BezierCurve[{{5445, -2937}, {5608, -2533}, {5700, -2007}}], BezierCurve[{{5793, -1483}, {5839, -795}, {5839, 60}}]}}]},
  "50" -> {FilledCurve[{{Line[{{-1829, 3282}, {-4992, 3282}}], Line[{{-5371, 1125}}], BezierCurve[{{-4742, 1461}, {-4159, 1631}, {-3627, 1631}}], BezierCurve[{{-3201, 1631}, {-2805, 1551}, {-2432, 1391}}], BezierCurve[{{-2062, 1232}, {-1736, 1012}, {-1456, 729}}], BezierCurve[{{-1177, 449}, {-957, 113}, {-797, -273}}], BezierCurve[{{-638, -659}, {-558, -1072}, {-558, -1518}}], BezierCurve[{{-558, -2160}, {-701, -2746}, {-990, -3276}}], BezierCurve[{{-1277, -3802}, {-1693, -4221}, {-2239, -4534}}], BezierCurve[{{-2781, -4844}, {-3407, -5000}, {-4120, -5000}}], BezierCurve[{{-4912, -5000}, {-5558, -4854}, {-6054, -4561}}], BezierCurve[{{-6546, -4268}, {-6899, -3931}, {-7106, -3552}}], BezierCurve[{{-7312, -3172}, {-7415, -2843}, {-7415, -2557}}], BezierCurve[{{-7415, -2383}, {-7345, -2220}, {-7199, -2064}}], BezierCurve[{{-7056, -1911}, {-6876, -1831}, {-6656, -1831}}], BezierCurve[{{-6283, -1831}, {-6007, -2044}, {-5824, -2477}}], BezierCurve[{{-5648, -2870}, {-5408, -3179}, {-5105, -3399}}], BezierCurve[{{-4805, -3619}, {-4466, -3728}, {-4093, -3728}}], BezierCurve[{{-3753, -3728}, {-3454, -3642}, {-3194, -3462}}], BezierCurve[{{-2938, -3286}, {-2738, -3043}, {-2598, -2730}}], BezierCurve[{{-2458, -2420}, {-2389, -2067}, {-2389, -1671}}], BezierCurve[{{-2389, -1245}, {-2462, -882}, {-2615, -579}}], BezierCurve[{{-2765, -280}, {-2968, -53}, {-3221, 97}}], BezierCurve[{{-3474, 243}, {-3753, 320}, {-4066, 320}}], BezierCurve[{{-4446, 320}, {-4719, 273}, {-4885, 180}}], BezierCurve[{{-5048, 87}, {-5285, -77}, {-5594, -310}}], BezierCurve[{{-5901, -543}, {-6157, -659}, {-6357, -659}}], BezierCurve[{{-6573, -659}, {-6766, -583}, {-6936, -423}}], BezierCurve[{{-7102, -266}, {-7189, -87}, {-7189, 113}}], BezierCurve[{{-7189, 133}, {-7165, 286}, {-7116, 566}}], Line[{{-6556, 3755}}], BezierCurve[{{-6496, 4134}, {-6380, 4414}, {-6210, 4587}}], BezierCurve[{{-6044, 4760}, {-5767, 4847}, {-5385, 4847}}], Line[{{-1869, 4847}}], BezierCurve[{{-1177, 4847}, {-831, 4584}, {-831, 4055}}], BezierCurve[{{-831, 3825}, {-914, 3638}, {-1084, 3495}}], BezierCurve[{{-1250, 3356}, {-1500, 3282}, {-1829, 3282}}]}}], FilledCurve[{{Line[{{7415, -47}}], BezierCurve[{{7415, -772}, {7372, -1398}, {7282, -1924}}], BezierCurve[{{7195, -2447}, {7029, -2923}, {6789, -3349}}], BezierCurve[{{6490, -3872}, {6097, -4274}, {5618, -4561}}], BezierCurve[{{5138, -4844}, {4606, -4987}, {4020, -4987}}], BezierCurve[{{3347, -4987}, {2741, -4797}, {2205, -4414}}], BezierCurve[{{1673, -4035}, {1270, -3505}, {997, -2830}}], BezierCurve[{{861, -2463}, {761, -2054}, {694, -1608}}], BezierCurve[{{631, -1162}, {598, -679}, {598, -160}}], BezierCurve[{{598, 509}, {631, 1112}, {704, 1651}}], BezierCurve[{{774, 2187}, {884, 2650}, {1037, 3036}}], BezierCurve[{{1297, 3675}, {1679, 4161}, {2179, 4497}}], BezierCurve[{{2678, 4830}, {3274, 5000}, {3966, 5000}}], BezierCurve[{{4422, 5000}, {4839, 4927}, {5211, 4777}}], BezierCurve[{{5584, 4630}, {5910, 4411}, {6193, 4125}}], BezierCurve[{{6473, 3838}, {6713, 3485}, {6909, 3063}}], BezierCurve[{{7245, 2330}, {7415, 1295}, {7415, -47}}]}, {Line[{{5591, 87}}], BezierCurve[{{5591, 899}, {5544, 1561}, {5444, 2081}}], BezierCurve[{{5348, 2597}, {5185, 2986}, {4952, 3252}}], BezierCurve[{{4722, 3515}, {4403, 3648}, {3993, 3648}}], BezierCurve[{{3407, 3648}, {3001, 3356}, {2771, 2763}}], BezierCurve[{{2545, 2174}, {2428, 1265}, {2428, 33}}], BezierCurve[{{2428, -799}, {2475, -1485}, {2575, -2017}}], BezierCurve[{{2671, -2550}, {2835, -2953}, {3068, -3226}}], BezierCurve[{{3297, -3499}, {3610, -3635}, {4006, -3635}}], BezierCurve[{{4412, -3635}, {4732, -3495}, {4965, -3209}}], BezierCurve[{{5195, -2926}, {5358, -2520}, {5451, -1991}}], BezierCurve[{{5544, -1465}, {5591, -772}, {5591, 87}}]}}]},
  "60" -> {FilledCurve[{{Line[{{-5621, 446}}], BezierCurve[{{-5321, 806}, {-4992, 1072}, {-4636, 1245}}], BezierCurve[{{-4283, 1418}, {-3883, 1505}, {-3444, 1505}}], BezierCurve[{{-3024, 1505}, {-2635, 1428}, {-2275, 1268}}], BezierCurve[{{-1919, 1112}, {-1606, 889}, {-1337, 596}}], BezierCurve[{{-1070, 306}, {-861, -37}, {-714, -433}}], BezierCurve[{{-568, -826}, {-494, -1238}, {-494, -1671}}], BezierCurve[{{-494, -2297}, {-638, -2863}, {-924, -3375}}], BezierCurve[{{-1210, -3885}, {-1610, -4284}, {-2122, -4571}}], BezierCurve[{{-2635, -4857}, {-3214, -5000}, {-3863, -5000}}], BezierCurve[{{-4609, -5000}, {-5251, -4820}, {-5794, -4454}}], BezierCurve[{{-6333, -4091}, {-6749, -3552}, {-7036, -2840}}], BezierCurve[{{-7322, -2127}, {-7465, -1258}, {-7465, -226}}], BezierCurve[{{-7465, 613}, {-7379, 1362}, {-7205, 2024}}], BezierCurve[{{-7032, 2683}, {-6786, 3232}, {-6460, 3668}}], BezierCurve[{{-6137, 4101}, {-5744, 4431}, {-5281, 4654}}], BezierCurve[{{-4822, 4874}, {-4296, 4987}, {-3703, 4987}}], BezierCurve[{{-3138, 4987}, {-2635, 4877}, {-2202, 4654}}], BezierCurve[{{-1769, 4434}, {-1443, 4158}, {-1223, 3828}}], BezierCurve[{{-1004, 3502}, {-894, 3186}, {-894, 2883}}], BezierCurve[{{-894, 2703}, {-960, 2537}, {-1100, 2387}}], BezierCurve[{{-1237, 2240}, {-1413, 2164}, {-1626, 2164}}], BezierCurve[{{-1813, 2164}, {-1986, 2220}, {-2146, 2340}}], BezierCurve[{{-2305, 2457}, {-2422, 2620}, {-2498, 2830}}], BezierCurve[{{-2598, 3096}, {-2761, 3309}, {-2984, 3472}}], BezierCurve[{{-3204, 3632}, {-3464, 3715}, {-3757, 3715}}], BezierCurve[{{-3990, 3715}, {-4226, 3648}, {-4459, 3512}}], BezierCurve[{{-4692, 3379}, {-4889, 3196}, {-5048, 2963}}], BezierCurve[{{-5371, 2493}, {-5561, 1654}, {-5621, 446}}]}, {Line[{{-3823, -3688}}], BezierCurve[{{-3377, -3688}, {-3004, -3505}, {-2711, -3139}}], BezierCurve[{{-2418, -2773}, {-2272, -2294}, {-2272, -1698}}], BezierCurve[{{-2272, -1295}, {-2339, -949}, {-2478, -656}}], BezierCurve[{{-2615, -366}, {-2805, -143}, {-3044, 10}}], BezierCurve[{{-3284, 163}, {-3547, 240}, {-3837, 240}}], BezierCurve[{{-4110, 240}, {-4373, 166}, {-4626, 20}}], BezierCurve[{{-4875, -126}, {-5078, -336}, {-5231, -616}}], BezierCurve[{{-5385, -892}, {-5461, -1218}, {-5461, -1591}}], BezierCurve[{{-5461, -2190}, {-5308, -2690}, {-4995, -3089}}], BezierCurve[{{-4685, -3489}, {-4296, -3688}, {-3823, -3688}}]}}], FilledCurve[{{Line[{{7465, -47}}], BezierCurve[{{7465, -772}, {7422, -1398}, {7332, -1924}}], BezierCurve[{{7245, -2447}, {7079, -2923}, {6839, -3349}}], BezierCurve[{{6540, -3872}, {6147, -4274}, {5667, -4561}}], BezierCurve[{{5188, -4844}, {4655, -4987}, {4070, -4987}}], BezierCurve[{{3397, -4987}, {2791, -4797}, {2255, -4414}}], BezierCurve[{{1723, -4035}, {1320, -3505}, {1047, -2830}}], BezierCurve[{{910, -2463}, {811, -2054}, {744, -1608}}], BezierCurve[{{681, -1162}, {648, -679}, {648, -160}}], BezierCurve[{{648, 509}, {681, 1112}, {754, 1651}}], BezierCurve[{{824, 2187}, {934, 2650}, {1087, 3036}}], BezierCurve[{{1347, 3675}, {1729, 4161}, {2229, 4497}}], BezierCurve[{{2728, 4830}, {3324, 5000}, {4016, 5000}}], BezierCurve[{{4472, 5000}, {4889, 4927}, {5261, 4777}}], BezierCurve[{{5634, 4630}, {5960, 4411}, {6243, 4125}}], BezierCurve[{{6523, 3838}, {6763, 3485}, {6959, 3063}}], BezierCurve[{{7295, 2330}, {7465, 1295}, {7465, -47}}]}, {Line[{{5641, 87}}], BezierCurve[{{5641, 899}, {5594, 1561}, {5494, 2081}}], BezierCurve[{{5398, 2597}, {5235, 2986}, {5002, 3252}}], BezierCurve[{{4772, 3515}, {4452, 3648}, {4043, 3648}}], BezierCurve[{{3457, 3648}, {3051, 3356}, {2821, 2763}}], BezierCurve[{{2595, 2174}, {2478, 1265}, {2478, 33}}], BezierCurve[{{2478, -799}, {2525, -1485}, {2625, -2017}}], BezierCurve[{{2721, -2550}, {2885, -2953}, {3118, -3226}}], BezierCurve[{{3347, -3499}, {3660, -3635}, {4056, -3635}}], BezierCurve[{{4462, -3635}, {4782, -3495}, {5015, -3209}}], BezierCurve[{{5245, -2926}, {5408, -2520}, {5501, -1991}}], BezierCurve[{{5594, -1465}, {5641, -772}, {5641, 87}}]}}]}
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
