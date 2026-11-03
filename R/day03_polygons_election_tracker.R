# Day 3 — Polygons ----------------------------------------------------------
# Election Night Tracker (Nov 3, 2026): a printable coloring sheet.
# Dashed states = close Senate races to color in as they're called.
# Plus a Senate seat counter and a Pennsylvania House inset.
#
# "Close" = rated Toss-up, Tilt or Lean by at least one of Cook Political
# Report, Sabato's Crystal Ball or Inside Elections in their late-2026
# ratings. No predictions: nothing is pre-colored.
#
# Output: Maps/day03_polygons_election_tracker.png — 8.5 x 11 in at 300 dpi,
# so it prints as a letter page (print at 100% / "actual size") and posts fine.

library(tidyverse)
library(sf)
library(tigris)
library(patchwork)
source("R/theme_ed.R")
options(tigris_use_cache = TRUE)

# ---- Kid-friendly font: Fredoka (falls back to Inter) -----------------------
kid_font <- local({
  have <- function() "Fredoka" %in% c(systemfonts::system_fonts()$family,
                                      systemfonts::registry_fonts()$family)
  if (!have()) try(systemfonts::require_font("Fredoka"), silent = TRUE)
  if (have()) "Fredoka" else ed_font
})

ink   <- "#1F1F1F"   # outlines + text
paper <- "#FFFFFF"
dim   <- "#EDEDED"   # states not in play (light, still printable)
dimln <- "#A8A8A8"

# ---- The races ------------------------------------------------------------
close_senate <- tribble(
  ~abbr, ~state,
  "AK", "Alaska",
  "GA", "Georgia",
  "IA", "Iowa",
  "ME", "Maine",
  "MI", "Michigan",
  "NE", "Nebraska",
  "NH", "New Hampshire",
  "NC", "North Carolina",
  "OH", "Ohio",
  "TX", "Texas"
) |> mutate(num = row_number())

pa_house <- tribble(
  ~cd,  ~label,  ~area,
  "01", "PA-1",  "Bucks County",
  "07", "PA-7",  "Lehigh Valley",
  "08", "PA-8",  "Scranton area",
  "10", "PA-10", "Harrisburg / York"
)

# ---- Geometry (tigris by default; can be overridden for testing) ----------
if (!exists("get_states")) get_states <- function() {
  states(cb = TRUE, resolution = "20m", year = 2024) |>
    filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) |>
    shift_geometry()                       # Alaska + Hawaii tucked under
}
if (!exists("get_pa_cd")) get_pa_cd <- function() {
  cd <- congressional_districts(state = "PA", cb = TRUE, year = 2024)
  fp <- grep("^CD1\\d\\dFP$", names(cd), value = TRUE)[1]   # e.g. CD119FP
  cd |>
    mutate(cd = str_pad(as.integer(.data[[fp]]), 2, pad = "0")) |>
    st_transform(6565)                     # PA South (m), just for shape
}

us <- get_states() |>
  left_join(close_senate, by = c("STUSPS" = "abbr")) |>
  mutate(in_play = !is.na(num))

pa <- get_pa_cd() |>
  left_join(pa_house, by = "cd") |>
  mutate(in_play = !is.na(label))

# Badge positions: inside each state, nudged out to sea where states are small
badges <- us |>
  filter(in_play) |>
  mutate(geometry = st_point_on_surface(geometry))
bxy <- st_coordinates(badges)
nudge <- tribble(~abbr, ~dx, ~dy,
                 "NH",  300000, -170000)   # metres; New Hampshire is tiny
badges <- badges |>
  st_drop_geometry() |>
  mutate(x = bxy[, 1], y = bxy[, 2]) |>
  left_join(nudge, by = c("STUSPS" = "abbr")) |>
  mutate(bx = x + coalesce(dx, 0), by = y + coalesce(dy, 0))

# ---- Shared bits ------------------------------------------------------------
theme_kid <- function() {
  theme_void(base_family = kid_font) +
    theme(plot.background  = element_rect(fill = paper, colour = NA),
          panel.background = element_rect(fill = paper, colour = NA),
          plot.title = element_text(size = 13, colour = ink, face = "bold",
                                    margin = margin(b = 4)),
          plot.subtitle = element_text(size = 9, colour = "#555555",
                                       lineheight = 1.15, margin = margin(b = 6)))
}

# Dashed-state recipe: paint over shared borders in white, then dash on top
dashed_states <- function(data, lw = 0.75) list(
  geom_sf(data = data, fill = paper, colour = paper, linewidth = lw * 1.8),
  geom_sf(data = data, fill = NA, colour = ink, linewidth = lw, linetype = "22")
)

badge <- function(x, y, lab, size = 6.2, fill = ed$pop) list(
  annotate("point", x = x, y = y, shape = 21, size = size,
           fill = fill, colour = paper, stroke = 0.9),
  annotate("text", x = x, y = y, label = lab, family = kid_font,
           fontface = "bold", colour = paper, size = size * 0.45)
)

# ---- A. US map ---------------------------------------------------------------
p_us <- ggplot() +
  geom_sf(data = filter(us, !in_play), fill = dim, colour = dimln, linewidth = 0.25) +
  dashed_states(filter(us, in_play)) +
  geom_segment(data = filter(badges, !is.na(dx)),
               aes(x = x, y = y, xend = bx, yend = by),
               colour = ed$pop, linewidth = 0.4) +
  geom_point(data = badges, aes(bx, by), shape = 21, size = 6.2,
             fill = ed$pop, colour = paper, stroke = 0.9) +
  geom_text(data = badges, aes(bx, by, label = num), family = kid_font,
            fontface = "bold", colour = paper, size = 2.9) +
  coord_sf(datum = NA, expand = FALSE,
           xlim = st_bbox(us)[c("xmin", "xmax")] + c(-50000, 420000),
           ylim = st_bbox(us)[c("ymin", "ymax")] + c(-50000, 60000)) +
  theme_kid()

# ---- B. Pennsylvania inset ----------------------------------------------------
pa_lab <- pa |>
  filter(in_play) |>
  mutate(geometry = st_point_on_surface(geometry))
pa_xy  <- st_coordinates(pa_lab)
pa_lab <- pa_lab |> st_drop_geometry() |> mutate(x = pa_xy[, 1], y = pa_xy[, 2])

home <- st_sfc(st_point(c(-75.1652, 39.9526)), crs = 4326) |>
  st_transform(st_crs(pa)) |> st_coordinates()

p_pa <- ggplot() +
  geom_sf(data = filter(pa, !in_play), fill = dim, colour = dimln, linewidth = 0.25) +
  dashed_states(filter(pa, in_play), lw = 0.6) +
  geom_label(data = pa_lab, aes(x, y, label = label), family = kid_font,
             fontface = "bold", size = 2.5, colour = ed$pop, fill = paper,
             label.size = 0, label.padding = unit(0.12, "lines")) +
  annotate("point", x = home[1], y = home[2], shape = 23, size = 2.6,
           fill = ed$alt, colour = paper, stroke = 0.6) +
  annotate("text", x = home[1] + 15000, y = home[2] - 12000, label = "home",
           family = kid_font, fontface = "bold", size = 2.5, colour = ed$alt_dk,
           hjust = 0) +
  coord_sf(datum = NA, expand = FALSE, clip = "off") +
  labs(title = "Close races near us",
       subtitle = "Pennsylvania seats in the U.S. House") +
  theme_kid()

# ---- C. Checklist -------------------------------------------------------------
rows <- bind_rows(
  tibble(kind = "head", txt = "Senate"),
  close_senate |> transmute(kind = "senate", key = as.character(num), txt = state),
  tibble(kind = "head", txt = "PA House"),
  pa_house |> transmute(kind = "house", key = label, txt = area)
) |> mutate(y = -row_number())

r_sen   <- filter(rows, kind == "senate")
r_house <- filter(rows, kind == "house")
r_item  <- filter(rows, kind != "head")

p_list <- ggplot(rows) +
  # section headings
  geom_text(data = filter(rows, kind == "head"), aes(0, y, label = txt),
            family = kid_font, fontface = "bold", size = 3.6, colour = ink,
            hjust = 0) +
  # Senate number badges
  geom_point(data = r_sen, aes(0.25, y), shape = 21, size = 4.6,
             fill = ed$pop, colour = paper, stroke = 0.6) +
  geom_text(data = r_sen, aes(0.25, y, label = key), family = kid_font,
            fontface = "bold", colour = paper, size = 2.2) +
  geom_text(data = r_house, aes(0.25, y, label = key), family = kid_font,
            fontface = "bold", colour = ed$pop, size = 2.6) +
  geom_text(data = r_item, aes(0.62, y, label = txt), family = kid_font,
            size = 3, colour = ink, hjust = 0) +
  # "called?" box + winner line
  geom_tile(data = r_item, aes(2.75, y), width = 0.22, height = 0.55,
            fill = paper, colour = ink, linewidth = 0.35) +
  annotate("segment", x = 3.05, xend = 4.3, y = r_item$y - 0.25,
           yend = r_item$y - 0.25, colour = "#9A9A9A", linewidth = 0.3) +
  annotate("text", x = c(2.75, 3.67), y = 0.15, label = c("Called?", "Winner"),
           family = kid_font, size = 2.6, colour = "#555555") +
  scale_x_continuous(limits = c(-0.05, 4.35), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min(rows$y) - 0.6, 0.6), expand = c(0, 0)) +
  labs(title = "Race checklist") +
  theme_kid()

# ---- D. Senate seat counter ---------------------------------------------------
seats <- expand_grid(row = 1:5, col = 1:20) |>
  mutate(n = (row - 1) * 20 + col)

p_seats <- ggplot(seats, aes(col, -row)) +
  geom_tile(fill = paper, colour = ink, linewidth = 0.3, width = 0.86, height = 0.86) +
  geom_tile(data = filter(seats, n == 51), fill = NA, colour = ed$pop,
            linewidth = 1, width = 0.86, height = 0.86) +
  geom_text(data = filter(seats, n == 51), label = "51", family = kid_font,
            fontface = "bold", size = 2.4, colour = ed$pop) +
  coord_equal(clip = "off") +
  labs(title = "Count the Senate: 100 seats",
       subtitle = "Color one square per seat. The first party to 51 controls the Senate.\nAt 50-50, the Vice President breaks ties.") +
  theme_kid()

# ---- E. Pick-your-colors key ----------------------------------------------------
key <- tibble(x = c(0, 1.35, 2.7), lab = c("Democrat", "Republican", "Independent / other"))

p_key <- ggplot(key) +
  geom_point(aes(x, 0), shape = 22, size = 7, fill = paper, colour = ink,
             stroke = 0.6) +
  geom_text(aes(x + 0.2, 0, label = lab), family = kid_font, size = 3.1,
            colour = ink, hjust = 0) +
  annotate("text", x = -0.2, y = 0, label = "Pick your colors:", family = kid_font,
           fontface = "bold", size = 3.3, colour = ink, hjust = 1) +
  scale_x_continuous(limits = c(-1.75, 4.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.6, 0.6)) +
  theme_kid()

# ---- Assemble ------------------------------------------------------------------
# letters follow plot order: A key, B US, C PA, D checklist, E seats
design <- "
AAAA
BBBB
BBBB
BBBB
CCDD
EEDD
"
page <- p_key + p_us + p_pa + p_list + p_seats +
  plot_layout(design = design, heights = c(0.22, 1, 1, 1, 1.15, 0.75)) +
  plot_annotation(
    title    = "Election Night Tracker",
    subtitle = "Tuesday, November 3, 2026  |  Color in each dashed state when its Senate race is called!",
    caption  = paste0(
      "Close = rated toss-up or lean by at least one of Cook Political Report, ",
      "Sabato's Crystal Ball or Inside Elections (late Oct. 2026). Nothing is predicted here.\n",
      "#30DayMapChallenge 2026  |  Day 3: Polygons  |  Elizabeth Delmelle  |  ",
      "Boundaries: US Census Bureau"),
    theme = theme_kid() + theme(
      plot.title    = element_text(family = kid_font, face = "bold", size = 26,
                                   colour = ink, margin = margin(b = 2)),
      plot.subtitle = element_text(family = kid_font, size = 11, colour = ed$pop,
                                   margin = margin(b = 4)),
      plot.caption  = element_text(family = kid_font, size = 6.5, colour = "#777777",
                                   hjust = 0, lineheight = 1.25, margin = margin(t = 8)),
      plot.caption.position = "plot",
      plot.margin = margin(28, 30, 18, 30))
  )

ggsave("Maps/day03_polygons_election_tracker.png", page, width = 8.5, height = 11,
       dpi = 300, bg = paper, device = ragg::agg_png)
message("Saved Maps/day03_polygons_election_tracker.png")