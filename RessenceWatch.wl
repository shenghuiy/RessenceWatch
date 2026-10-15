(* ::Package:: *)
(* Ressence Type 1 style watch face built from ClockGauge satellites.

   ROCS (Ressence Orbital Convex System) model
   -------------------------------------------
   * The whole main disc is the planet carrier: it turns clockwise once per hour.
     The long "minute hand" is simply a marker printed on that disc.
   * Each sub-dial is a ClockGauge "planet" mounted on the carrier and orbits
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
planet = GrayLevel[0.905];
steel  = {GrayLevel[0.93], GrayLevel[0.62], GrayLevel[0.97], GrayLevel[0.55]};

(* ---------- carrier geometry (disc frame, face radius = 1) ----------
   Positions are taken from the reference photo, where the minute marker sits
   at alpha0 ~ 63 degrees; the satellites are upright in that configuration. *)
alpha0 = 63 Degree;
hourPos = {-0.33, -0.165};  hourR = 0.43;
secPos  = { 0.015, 0.545};  secR  = 0.25;
dayPos  = { 0.455, -0.335}; dayR  = 0.235;

clockwise[v_, a_] := RotationMatrix[-a] . v;

(* Slightly tapered hand: wide rounded base at the pivot, narrower rounded tip.
   ClockGauge lays a graphic marker along +x with its pivot at the origin and
   scales it to the gauge, so the shape is drawn from x = -w to 1. *)
taperedHand[w_] := Graphics[{ink, FilledCurve[{
  Line[{{0, -w}, {0.95, -0.45 w}}], BezierCurve[{{1.0, 0}, {0.95, 0.45 w}}],
  Line[{{0, w}}], BezierCurve[{{-w, 0}, {0, -w}}]}]}];

(* the same taper as plotting primitives, from radius r0 to r1 along unit vector u *)
taperedHand[u_, r0_, r1_, w0_, w1_] := With[{n = {-u[[2]], u[[1]]}},
  FilledCurve[{
    Line[{r0 u - w0 n, r1 u - w1 n}], BezierCurve[{r1 u - w1 n + 1.3 w1 u, r1 u + w1 n + 1.3 w1 u, r1 u + w1 n}],
    Line[{r0 u + w0 n}], BezierCurve[{r0 u + w0 n - 1.3 w0 u, r0 u - w0 n - 1.3 w0 u, r0 u - w0 n}]}]];

(* ---------- ClockGauge satellites (drawn in their own upright frame) ---------- *)
gaugeBase = Sequence[
  GaugeFrameStyle -> None, ImagePadding -> 0, PlotRangePadding -> 0,
  GaugeStyle -> ink, Background -> None];

hourGauge[h_, m_, s_] := ClockGauge[{h, m, s},
  GaugeMarkers -> {taperedHand[0.07], None, None},
  GaugeFaceStyle -> planet, ScaleDivisions -> {12, 1}, TicksStyle -> None,
  LabelStyle -> Directive[ink, 13, FontFamily -> "Arial", FontWeight -> Plain],
  gaugeBase];

secondsGauge[s_] := ClockGauge[{0, 0, s},
  GaugeMarkers -> {None, None, taperedHand[0.06]},
  GaugeFaceStyle -> planet, ScaleDivisions -> {12, 1},
  TicksStyle -> Directive[ink, AbsoluteThickness[2]], LabelStyle -> None,
  gaugeBase];

(* day-of-week: 7 sectors spread round a 12h dial, the hour hand points to the day *)
dayGauge[wd_, frac_] := ClockGauge[{12 (wd - 1 + frac)/7, 0, 0},
  GaugeMarkers -> {taperedHand[0.07], None, None},
  GaugeFaceStyle -> planet, TicksStyle -> None, LabelStyle -> None,
  ScaleDivisions -> {7, 1},
  ScaleRanges -> Table[{12 k/7 + 0.18, 12 (k + 1)/7 - 0.18}, {k, 0, 6}],
  ScaleRangeStyle -> ink,
  gaugeBase];

(* Upright satellite: its centre orbits with the carrier, but the planet gear
   cancels the carrier rotation so the ring never spins (numerals stay upright).
   Only the hand inside turns. *)
uprightSatellite[g_, pos_, r_, rot_] := Inset[g, clockwise[pos, rot], Center, 2 r, {1, 0}];

(* ---------- fixed outer minute ring ---------- *)
minuteRing = Module[{ticks, labels},
  ticks = Table[
    If[Mod[k, 10] == 0, Nothing,
      With[{a = 2 Pi k/60, l = If[Mod[k, 5] == 0, 0.085, 0.055]},
        Line[{0.95 {Sin[a], Cos[a]}, (0.95 - l) {Sin[a], Cos[a]}}]]],
    {k, 0, 59}];
  labels = Table[
    With[{a = 2 Pi k/60},
      Text[Style[ToString[If[k == 0, 60, k]], ink, 15, FontFamily -> "Arial"],
        0.905 {Sin[a], Cos[a]}, Center, {Cos[-a], Sin[-a]}]],
    {k, 0, 50, 10}];
  {ink, CapForm["Round"], AbsoluteThickness[2.2], ticks, labels}];

(* black line separating the fixed minute ring from the rotating disc *)
ringBoundary = {Black, AbsoluteThickness[1.4], Circle[{0, 0}, 0.845]};

(* convex sapphire dome: soft vignette towards the rim *)
domeShade = Table[{Opacity[0.015], Black, Annulus[{0, 0}, {r, 1.0}]}, {r, 0.90, 0.98, 0.02}];

caseRing = {
  {GrayLevel[0.2], Disk[{0, 0}, 1.115]},
  Table[{Blend[steel, i/40], Annulus[{0, 0}, {1.0 + 0.11 i/41, 1.0 + 0.11 (i + 1)/41}]}, {i, 0, 40}]};

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
    domeShade,
    minuteRing,
    ringBoundary,
    (* planets riding on the carrier *)
    uprightSatellite[hourGauge[h, m, s], hourPos, hourR, rot],
    uprightSatellite[secondsGauge[s], secPos, secR, rot],
    uprightSatellite[dayGauge[wd, (h + m/60)/24], dayPos, dayR, rot],
    (* minute marker printed on the carrier *)
    {ink, taperedHand[dir, 0.18, 0.79, 0.022, 0.010]}
    },
    opts, PlotRange -> 1.13, ImageSize -> 420,
    Background -> White]
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
  }, Alignment -> Center], Background -> White],
  SaveDefinitions -> True];

End[];
EndPackage[];
