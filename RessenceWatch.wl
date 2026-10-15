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
thinW = 0.001632;   (* ring boundary lines *)
thin  = Thickness[thinW];
lineHalf[t_] := t faceHalf;   (* half the width of a Thickness[t] line, in plot units *)

(* ---------- numerals ----------
   Numerals are drawn as filled glyph outlines (FilledCurve with Bezier
   segments) instead of Text: the font rasterizer pixel-snaps small or rotated
   text, which looks jagged next to the vector hands.

   They use Arial Rounded MT Bold. It is a commercial font, so its outlines are
   not embedded: they are read from the installed font once (PDF export with
   outlined text) and cached. A missing font is silently substituted by the
   front end. *)
numeralFont = "Arial Rounded MT Bold";

(* FilledCurve[segment specs, points] (as imported) -> explicit Line and
   BezierCurve components, which outlinePoints walks *)
explicitCurve[FilledCurve[specs_, pts_]] := FilledCurve@MapThread[
  Function[{sp, ps}, Module[{i = 1},
    (* a contour's first segment includes its start point: give that to a Line *)
    Flatten@Map[Function[s, With[{seg = ps[[i ;; i + s[[2]] - 1]], first = i == 1}, i += s[[2]];
        Which[s[[1]] == 0, Line[seg], first, {Line[{First[seg]}], BezierCurve[Rest[seg]]}, True, BezierCurve[seg]]]],
      sp]]], {specs, pts}];
explicitCurve[fc_FilledCurve] := fc;

(* outlines of str with ink height 1, ink box centred at the origin *)
fontOutline[str_] := fontOutline[str] = Module[{fc, g, b},
  fc = Cases[First@ImportString[
      ExportString[Style[str, FontFamily -> numeralFont, FontSize -> 100], "PDF"],
      {"PDF", "PageGraphics"}, "TextOutlines" -> True], _FilledCurve, Infinity];
  g = explicitCurve /@ Rest[fc];   (* the first curve is the page box *)
  b = CoordinateBounds[Cases[g, {_?NumericQ, _?NumericQ}, Infinity]];
  g /. {x_?NumericQ, y_?NumericQ} :> ({x, y} - Mean /@ b)/(b[[2, 2]] - b[[2, 1]])];

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

(* Upright numerals filling the ring between the two circles: each is as large
   as fits, up to the full ring width. Computed once per dial radius. *)
hourNumerals[r_] := hourNumerals[r] =
  With[{ri = 0.70 r + lineHalf[thinW], ro = r - lineHalf[thinW]},
    Table[With[{a = 2 Pi k/12}, With[{f = ringFit[ToString[k], a, ri, ro]},
      label[ToString[k], f[[2]] clockDir[a], f[[1]]]]], {k, 1, 11}]];

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

dayDial[wd_, frac_, r_] := With[{gap = 0.2, rr = 0.78 r, w = 0.11 r},
  {
   {planet, Disk[{0, 0}, r]},
   {ink, thin, Circle[{0, 0}, r]},
   {Thickness[0.0024],
     {ink, hollowTube[rr, w, gap, 2 Pi 5/7 - gap]},
     {accent, hollowTube[rr, w, 2 Pi 5/7 + gap, 2 Pi - gap]},
     (* dividers across the tubes between days *)
     Table[{If[k > 5, accent, ink], Line[{(rr - w/2) clockDir[2 Pi k/7], (rr + w/2) clockDir[2 Pi k/7]}]},
       {k, {1, 2, 3, 4, 6}}]},
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
ringBoundary = {Black, AbsoluteThickness[1.428], Circle[{0, 0}, 0.845]};

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
