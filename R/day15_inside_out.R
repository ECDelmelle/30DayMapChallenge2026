# day15_inside_out.R ---------------------------------------------------------
# #30DayMapChallenge 2026 · Day 15: Inside Out
# Philadelphia with its colours turned inside out:
#   roads   -> blue, drawn like a river network (width by road class)
#   rivers  -> asphalt grey
#   parks   -> magenta (green's opposite; red-on-green fails for colour-blind readers)
#   income  -> green ramp run backwards: DARK green = poorest, LIGHT = richest

source("R/theme_ed.R")
source("R/base_phl.R")

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)   # needs a Census API key: census_api_key("...", install = TRUE)
library(osmdata)
library(ggnewscale)   # second fill scale so parks get their own legend key
library(scales)

b <- phl_base()

# ---- Inside-out palette ----------------------------------------------------
io <- list(
  river   = "#8C8C8C",  # rivers become pavement
  road_hi = "#1D5C9E",  # expressways: the main channels
  road_md = "#3577BD",  # arterials: tributaries
  road_lo = "#86AEDA",  # local streets: capillaries
  park    = "rosybrown1"     # parks: magenta
)

# Reversed green ramp built from the theme's green
green_rev <- c("#0A3323", ed$alt_dk, ed$alt, "#8FD3AE", "#E2F4E9")

# ---- Roads (TIGER; class as a stand-in for volume) -------------------------
# Main channels = expressways. Tributaries = a few named arterials only,
# so the network reads like a drainage basin rather than a street grid.
trib_pattern <- regex(
  "Roosevelt Blvd|Roosevelt Expy|US Hwy 1\\b|Broad St|State Rte 611|Lincoln Dr",
  ignore_case = TRUE)

roads <- roads("PA", "Philadelphia", year = 2024) |>
  st_transform(crs_phl) |>
  mutate(cls = case_when(
    MTFCC == "S1100" ~ "Expressway",
    MTFCC %in% c("S1200", "S1400") &
      str_detect(FULLNAME, trib_pattern) ~ "Major road",
    TRUE ~ NA_character_)) |>
  filter(!is.na(cls)) |>
  mutate(cls = factor(cls, levels = c("Expressway", "Major road"))) |>
  arrange(desc(cls))                        # tributaries first, main stems on top

# Quick check of what got picked up as tributaries (tweak trib_pattern if needed)
roads |> st_drop_geometry() |> filter(cls == "Major road") |> count(FULLNAME, sort = TRUE)

# ---- Median household income (ACS 2020-2024 5-year) -----------------------
inc <- get_acs("tract", variables = "B19013_001", state = "PA",
               county = "Philadelphia", year = 2024, survey = "acs5",
               geometry = TRUE, cb = TRUE) |>
  st_transform(crs_phl)

# ---- Parks (OpenStreetMap, cached) ------------------------------------------
parks_file <- "Data/phl_parks_osm.rds"
if (!file.exists(parks_file)) {
  q <- opq(bbox = st_bbox(st_transform(b$phl, 4326)), timeout = 120) |>
    add_osm_feature("leisure", c("park", "nature_reserve")) |>
    osmdata_sf()
  parks <- bind_rows(q$osm_polygons, q$osm_multipolygons) |>
    select(name) |>
    st_make_valid() |>
    st_transform(crs_phl) |>
    st_intersection(select(b$phl)) |>
    filter(as.numeric(st_area(geometry)) > 1e5)   # drop pocket parks (< ~2.3 ac)
  saveRDS(parks, parks_file)
}
parks <- readRDS(parks_file)

# ---- Map --------------------------------------------------------------------
# One guide object shared by the colour and linewidth scales so they merge
road_guide <- guide_legend(order = 3, theme = theme(
  legend.key.width  = unit(2, "lines"),
  legend.key.height = unit(0.8, "lines")))

p <- ggplot() +
  geom_sf(data = b$nbr, fill = ed$land_nbr, colour = ed$line_nbr, linewidth = 0.25) +
  geom_sf(data = inc, aes(fill = estimate), colour = NA) +
  scale_fill_stepsn(
    colours = green_rev,
    breaks  = c(30e3, 50e3, 75e3, 100e3, 150e3),
    limits  = c(0, 250e3), oob = squish,
    labels  = label_dollar(scale = 1e-3, suffix = "k"),
    na.value = ed$land,
    name = "Median household income",
    guide = guide_coloursteps(order = 1)
  ) +
  new_scale_fill() +
  geom_sf(data = parks, aes(fill = "Park"), colour = NA, key_glyph = "rect") +
  scale_fill_manual(
    values = c(Park = io$park), name = " ",   # blank title keeps it level with the income bar
    guide = guide_legend(order = 2, theme = theme(
      legend.key.width  = unit(1, "lines"),
      legend.key.height = unit(1, "lines")))
  ) +
  geom_sf(data = b$water, fill = io$river, colour = NA) +
  # key_glyph rather than show.legend = "line": the latter forces this layer
  # into every legend, which drew a black line through the Park key
  geom_sf(data = roads, aes(linewidth = cls, colour = cls), lineend = "round",
          key_glyph = "path") +
  geom_sf(data = b$phl, fill = NA, colour = ed$line, linewidth = 0.35) +
  # same name + values on both scales -> one merged "roads" legend
  scale_linewidth_manual(
    values = c("Expressway" = 1.1, "Major road" = 0.5), name = " ",
    guide = road_guide) +
  scale_colour_manual(
    values = c("Expressway" = io$road_hi, "Major road" = io$road_md), name = " ",
    guide = road_guide) +
  base_finish(b) +
  labs(
    title = "Median Household Income in Philadelphia",
    subtitle = "By census tract, 2020-2024",
    caption = ed_caption(15, "Inside Out",
                         "ACS 2020-2024 5-year (B19013); TIGER/Line 2024 roads & water; OpenStreetMap parks")
  ) +
  theme_ed() +
  theme(
    legend.title.position = "top",
    legend.key.width  = unit(3.2, "lines"),
    legend.key.height = unit(0.45, "lines")
  )

p
ed_save(p, 15, "15-delmelle.png")