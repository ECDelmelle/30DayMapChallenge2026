# 30daymapping2026

My maps for the [#30DayMapChallenge](https://30daymapchallenge.com) 2026, made in R.

Same layout as [last year](https://github.com/ECDelmelle/30daymapping2025):

- `R/theme_ed.R` – shared palette (light grey + magenta), Inter font, `theme_ed()`, `ed_caption()`, `ed_save()`
- `Scripts/dayNN_<theme>_<topic>.R` – one script per day; run from the project root
- `Data/` – cached downloads (git-ignored if large)
- `Maps/` – exported PNGs

| Day | Theme  | Map |
|-----|--------|-----|
| 1   | Points | Philadelphia 311 requests mentioning rats, mice or rodents |

Packages: tidyverse, sf, httr2, tigris, scales, showtext, sysfonts.
