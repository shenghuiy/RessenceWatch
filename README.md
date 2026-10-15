# RessenWatch

A light-dial Ressence Type 1–style watch face in Wolfram Language, drawn entirely with vector graphics primitives (numerals included, as glyph outlines), with sub-dials that orbit like planetary gears. Hands are slightly tapered, and a thin black line separates the fixed minute ring from the rotating disc.

![ROCS animation](ressence_rocs.gif)

## How it works

Ressence watches use the ROCS (Ressence Orbital Convex System): there are no central hands, and the whole dial is the display.

- **Carrier disc:** the main disc turns clockwise once per hour. The long marker on it reads the minutes against the outer 10…60 scale, which is fixed to the case.
- **Satellites:** the hour, seconds and day-of-week dials are mounted on the disc. They orbit with it, but they're counter-geared so the rings never spin: numerals and ticks stay upright, and only the hand inside each one turns.
- **Hour dial:** upright numerals at 3, 6 and 9, each as large as fits between the ring's two circles, short bars at the other hours, with the OpenMoji 👈 emoji at 12; the hand turns on the inner disc.
- **Seconds dial:** 12 ticks between two rings, with the 12 o'clock tick in orange.
- **Day dial:** two hollow tubes, Monday–Friday and (in orange) Saturday–Sunday, clockwise from 12 o'clock, each day taking a seventh of the turn and marked off by small dividers. The hand moves through the current day's arc over the day.

## Usage

```wl
Get["RessenceWatch.wl"]

ressenceFace[{10, 10.5, 47, 3}]   (* {hour, minute, second, ISO weekday} *)
ressenceFace[Now]                 (* a DateObject also works *)
ressenceWatch[]                   (* live watch with a time-warp slider; needs a notebook front end *)
```

Or open `RessenceWatch.nb`, which loads the package and runs the examples.

## Files

| File | Contents |
| --- | --- |
| `RessenceWatch.wl` | The package: `ressenceFace` and `ressenceWatch` |
| `RessenceWatch.nb` | Notebook with the live watch, a still frame and an animation |
| `ressence_rocs.gif` | One hour compressed into a loop |
| `ressence_face.png` | Still frame |

## Credits

The hand at 12 on the hour dial is [OpenMoji 1F448 "backhand index pointing left"](https://openmoji.org/library/emoji-1F448/) by the [OpenMoji](https://openmoji.org/) project, licensed under [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). Its SVG paths were converted into Wolfram primitives (`openmojiPointLeft` in `RessenceWatch.wl`); that adapted drawing is shared under the same licence.

The numerals use Arial Rounded MT Bold, a commercial Monotype font that is not included here. Its outlines are read from the installed font at run time; without it, the front end substitutes another font.

Inspired by the Ressence Type 1. This is an unofficial fan project, not affiliated with Ressence.
