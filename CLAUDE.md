# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Ressence Type 1–style watch face in Wolfram Language. Everything lives in one package, `src/RessenceWatch.wl` (context `RessenceWatch``, public symbols `ressenceFace` and `ressenceWatch`). `src/RessenceWatch.nb` loads it from `NotebookDirectory[]` and runs the examples. `img/` holds the rendered images that the README shows. There is no build, lint or test setup.

## Working with the package

- Evaluate through the Wolfram MCP evaluator. Its kernel keeps definitions between calls, so clear before reloading, otherwise stale definitions survive:
  ```wl
  ClearAll["RessenceWatch`*", "RessenceWatch`Private`*"];
  Get["<repo>/src/RessenceWatch.wl"];
  ```
  If a session-context symbol such as `Sessions`…`ressenceFace` shadows the package symbol, call `RessenceWatch`ressenceFace` explicitly.
- After a visual change, regenerate both images with the same settings so they stay consistent:
  ```wl
  rf = RessenceWatch`ressenceFace;
  Export["img/ressence_face.png",
    ImageResize[Rasterize[rf[DateObject[{2026, 10, 14, 10, 10, 30}]], ImageResolution -> 72*2000/420, Background -> None, LightDark -> "Dark"], 2000]];
  (* same with LightDark -> "Light" for img/ressence_face_black.png *)
  Export["img/ressence_rocs.gif",
    Table[Rasterize[rf[DateObject[{2026, 10, 14, 0, 0, t}]], ImageSize -> 340, Background -> White, LightDark -> "Dark"],
      {t, 36000, 43100, 100}],
    "DisplayDurations" -> 0.08, "AnimationRepetitions" -> Infinity];
  ```
  Render the PNG with `ImageResolution`, not a large `ImageSize`. The ring boundary uses `AbsoluteThickness`, which would come out as a hairline.
- Keep the README's "How it works" bullets in step with the dial designs.
- The repo uses LF line endings (`core.autocrlf=input`).

## Architecture

- **ROCS model.** The main disc is a planet carrier that turns once per hour; the minute marker is printed on it. The sub-dials (hour, seconds, day) are "satellites" placed by `satellite[g, pos, rot]`. It only translates them along their orbit and never rotates them, so their rings stay upright and only the inner hands turn. Satellite positions (`hourPos`, `secPos`, `dayPos`) are measured from a reference photo taken with the carrier at `alpha0 = 63°`. The outer minute scale and the case ring are fixed.
- **Coordinates.** Face radius is 1 and the plot range is `±faceHalf` (1.13). Clock angles are measured clockwise from 12 (`clockDir[a] = {Sin a, Cos a}`). Wolfram `Circle` arc angles are counter-clockwise from 3 o'clock, so code that uses them converts with `Pi/2 - a`.
- **Line widths** are `Thickness[...]`, a fraction of the plot width (2·faceHalf), so the face looks the same at any size. `lineHalf[t]` converts a thickness into a half-width in plot units, for geometry that has to line up with stroke edges.
- **Numerals are filled outlines, not `Text`.** The front end pixel-snaps text, which looks jagged next to the vector hands. `fontOutline` exports styled Arial Rounded MT Bold text to PDF and imports it back with `"TextOutlines" -> True`, then caches the curves, normalised to ink height 1. Arial Rounded is commercial and isn't embedded, so it must be installed on the machine. `explicitCurve` converts the imported `FilledCurve` specs into `Line`/`BezierCurve` pieces; a contour can start with a 4-point Bézier, which carries its start point. `ringFit` finds by search the largest upright numeral that fits between two radii; the hour dial's 3/6/9 use it.
- **Palette.** Every colour is a `LightDarkSwitched[light, dark]` in the palette block: light-mode notebooks get the black-dial Type 1 (white markings, green lume), dark mode the pale dial. `Rasterize` picks one with `LightDark -> "Light" | "Dark"`; draw with the palette symbols, never literal colours.
- **Hour-dial emoji.** The 👈 at 12 comes from OpenMoji (CC BY-SA 4.0), converted from SVG paths (`openmojiPointLeft`). Keep the credit in the README.
