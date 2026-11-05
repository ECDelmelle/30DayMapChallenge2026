# Day 5 — Sight ---------------------------------------------------------------
# Out of Sight: Philadelphia's buried streams.
# Most of the city's creeks were piped into sewers in the 1800s and early
# 1900s. They're invisible, but they still drain the old valleys — which is
# why floods and collapses turn up in places that look nowhere near water.
#
# Data
#   Historic streams: Philadelphia Water Dept., "Historic Streams" (Charles
#     Ellet's 1842 survey + earlier PWD study), via OpenDataPhilly / ArcGIS
#   Surviving streams: US Census TIGER/Line linear + area water
#   Buried = historic stream lines more than `alive_ft` from any surface water
# Stories: PWD blog (Mill Creek, Wingohocking); WHYY (Indian Creek, Germantown)

library(tidyverse)
library(sf)
library(tigris)
library(ggspatial)
source("R/theme_ed.R")
source("R/base_phl.R")
options(tigris_use_cache = TRUE)

b <- phl_base()

hist_file  <- "Data/pwd_historic_streams.gpkg"
alive_ft   <- 250     # historic line within this of surface water = still visible
show_roads <- TRUE    # faint main roads for orientation

# ---- Dark palette (keeps the magenta from theme_ed) -------------------------
dk <- list(
  bg       = "#07080B",
  land     = "#1C1F27",   # Philadelphia (figure) - clearly lifted off the ground
  land_nbr = "#0B0C10",   # neighbours (ground) - nearly the background
  line     = "#343843",
  water    = "#0D141C",   # rivers darker than the city, so they read as voids
  live     = "#8AA3B6",   # streams still on the surface: visible but quiet
  glow     = "#8FE3FF",   # buried streams: icy glow
  core     = "#E9FCFF",   # bright core of the glow
  road     = "#2A2D36",
  ink      = "#F4F4F6",
  muted    = "#C9CBD2",
  faint    = "#70747E",
  pop      = ed$pop,      # named creeks in the stories
  pop_lt   = "#FF8CC6",   # story headings (same hue as its creek)
  pop_core = "#FFD3EA"
)

# ---- ArcGIS FeatureServer -> sf, paging past the record limit ---------------
arcgis_sf <- function(layer_url, page = 1000) {
  out <- list(); off <- 0
  repeat {
    q <- paste0(layer_url, "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson",
                "&resultOffset=", off, "&resultRecordCount=", page)
    x <- read_sf(q, quiet = TRUE)
    if (nrow(x) == 0) break
    out[[length(out) + 1]] <- x
    if (nrow(x) < page) break
    off <- off + page
  }
  bind_rows(out)
}

if (!file.exists(hist_file)) {
  hist_raw <- arcgis_sf(paste0("https://services.arcgis.com/fLeGjb7u4uXqeF9q/",
                               "arcgis/rest/services/HistoricStreams_Arc/FeatureServer/0"))
  dir.create("Data", showWarnings = FALSE)
  write_sf(hist_raw, hist_file)
}
hist <- read_sf(hist_file) |>
  st_transform(crs_phl) |>
  st_zm() |>
  st_intersection(st_geometry(b$phl))
glimpse(st_drop_geometry(hist))   # <- check which column holds creek names

# ---- What's still on the surface? ------------------------------------------
live_lines <- linear_water("PA", "Philadelphia", year = 2024) |> st_transform(crs_phl)
live_zone  <- c(st_geometry(st_buffer(live_lines, alive_ft)),
                st_geometry(st_buffer(b$water, alive_ft))) |>
  st_union() |> st_make_valid()

buried <- hist |>
  st_difference(live_zone) |>
  st_collection_extract("LINESTRING")
alive  <- hist |>
  st_intersection(live_zone) |>
  st_collection_extract("LINESTRING")

mi <- \(x) sum(as.numeric(st_length(x))) / 5280
message(sprintf("Historic: %.0f mi | buried: %.0f mi | still visible: %.0f mi",
                mi(hist), mi(buried), mi(alive)))

# ---- The stories ----------------------------------------------------------------
# lon/lat are approximate; each point snaps to the nearest historic stream line.
# No leaders: text sits right against its (magenta) creek. dx/dy in feet from
# the snapped point to the text's anchor; hjust 1 = text ends at the anchor.
# (Text bottom sits at ty; heading on top, body underneath.)
stories <- tribble(
  ~key,            ~lon,      ~lat,    ~dx,    ~dy,  ~hjust,
  ~head, ~body,
  "wingohock",  -75.1705,  40.0462,  -1500,  20500,  0,
  "WINGOHOCKING CREEK",
  "Germantown streets flood on high ground,\nfar from any creek you can see.\nThey sit right on top of this one.",
  # West-side stories: narrow, left-aligned blocks in the open ground of
  # Montgomery / Delaware Co., so nothing runs off the left edge
  "mill",       -75.2165,  39.9705, -25000,  -8000,  0,
  "MILL CREEK",
  "In 1961 the sewer under\nFunston Street caved in.\nFour houses fell with it;\nthree people died.",
  "dock",       -75.1435,  39.9460,   4500,   2500,  0,
  "DOCK CREEK",
  "The first to go under, in the 1760s.\nDock Street still bends\nthe way the creek did.",
  "indian",     -75.2500,  39.9795, -16500,  16500,  0,
  "INDIAN CREEK",
  "Brought back into sight:\nthe Water Dept.'s first\n\"daylighting\" opened it to\nthe air, in Morris Park."
)

st_pts <- st_as_sf(stories, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_phl)
hist_u <- st_union(hist)
# One connector line per point; its last vertex is the snapped spot
snap <- st_nearest_points(st_geometry(st_pts), hist_u) |>
  st_coordinates() |>
  as_tibble() |>
  slice_tail(n = 1, by = L1)
stories <- stories |>
  mutate(sx = snap$X, sy = snap$Y,
         tx = sx + dx, ty = sy + dy)

# Highlight the named creeks in magenta (similarity: creek + its text share a
# colour). Uses a name column if the data has one; otherwise lights up buried
# segments near each story point.
name_col <- names(hist)[map_lgl(st_drop_geometry(hist), \(x)
                                is.character(x) && any(str_detect(x, regex("wingohock|cohocksink|mill", TRUE)),
                                                       na.rm = TRUE))][1]
if (!is.na(name_col)) {
  message("Creek names from column: ", name_col)
  hl <- buried |>
    filter(str_detect(.data[[name_col]],
                      regex(paste(stories$key, collapse = "|"), TRUE)))
} else {
  message("No name column found; highlighting buried lines near each story.")
  near_zone <- st_union(st_buffer(st_as_sf(stories, coords = c("sx", "sy"),
                                           crs = crs_phl), 4500))
  hl <- st_intersection(buried, near_zone)
}

# ---- Glow: stacked strokes, wide + faint underneath, thin + bright on top ----
glow <- function(data, col, core, widths = c(3.8, 2.3, 1.2, 0.5),
                 alphas = c(0.07, 0.15, 0.38, 1)) {
  c(map2(widths, alphas, \(w, a)
         geom_sf(data = data, colour = col, linewidth = w, alpha = a,
                 lineend = "round")),
    list(geom_sf(data = data, colour = core, linewidth = 0.18,
                 lineend = "round")))
}

# Text with a halo in the city's colour, so it reads over the stream network
halo_text <- function(...) shadowtext::geom_shadowtext(
  ..., family = ed_font, bg.colour = dk$land, bg.r = 0.18)

roads <- if (show_roads) {
  primary_secondary_roads("PA", year = 2024) |>
    st_transform(crs_phl) |>
    st_intersection(st_geometry(b$phl))
} else NULL

# ---- Labels -----------------------------------------------------------------------
# The legend lives in the subtitle: coloured words = line colours
subtitle <- sprintf(paste0(
  "**Philly's buried streams.** The city once had 283 miles of creeks. All but 118<br>",
  "now run <span style='color:%s'>**buried in sewers**</span> beneath the streets; ",
  "only a few still <span style='color:%s'>**flow on the surface**</span>.<br>",
  "You can't see them, but in a hard rain the water still finds its old valleys."),
  dk$glow, dk$live)

p <- ggplot() +
  # ground -> figure -> water
  geom_sf(data = b$nbr, fill = dk$land_nbr, colour = dk$line, linewidth = 0.2) +
  geom_sf(data = b$phl, fill = dk$land, colour = NA) +
  {if (show_roads) geom_sf(data = roads, colour = dk$road, linewidth = 0.25)} +
  geom_sf(data = b$water, fill = dk$water, colour = NA) +
  geom_sf(data = b$phl, fill = NA, colour = dk$line, linewidth = 0.3) +
  # streams you can still see: quiet, but clearly a different line
  geom_sf(data = alive, colour = dk$live, linewidth = 0.45) +
  # streams you can't: glowing
  glow(buried, dk$glow, dk$core) +
  glow(hl, dk$pop, dk$pop_core, alphas = c(0.10, 0.20, 0.45, 1)) +
  # story text: no leaders; proximity + matching colour tie it to its creek
  halo_text(data = stories, aes(tx, ty + 500, label = head, hjust = hjust),
            vjust = 0, fontface = "bold", size = 3.1, colour = dk$pop_lt) +
  halo_text(data = stories, aes(tx, ty - 300, label = body, hjust = hjust),
            vjust = 1, size = 2.7, colour = dk$muted, lineheight = 1.05) +
  ed_scalebar("br") +
  # clip stays on: with it off, the neighbouring counties spill past the frame
  coord_sf(crs = crs_phl, xlim = b$bb[c("xmin", "xmax")],
           ylim = b$bb[c("ymin", "ymax")], expand = FALSE, datum = NA) +
  labs(
    title    = "Out of Sight",
    subtitle = subtitle,
    caption  = ed_caption(5, "Sight", paste0(
      "Philadelphia Water Dept. Historic Streams (Ellet 1842 + PWD), OpenDataPhilly; ",
      "US Census TIGER/Line\nStories: PWD; WHYY; Adam Levine, waterhistoryphl.org"))
  ) +
  theme_ed() +
  theme(
    plot.background  = element_rect(fill = dk$bg, colour = NA),
    panel.background = element_rect(fill = dk$bg, colour = NA),
    panel.border     = element_blank(),
    plot.title       = element_text(face = "bold", size = rel(2.3), colour = dk$ink,
                                    margin = margin(b = 6)),
    plot.subtitle    = ggtext::element_markdown(size = rel(0.95), colour = dk$muted,
                                                lineheight = 1.3,
                                                margin = margin(b = 12)),
    plot.caption     = element_text(size = rel(0.62), colour = dk$faint, hjust = 0,
                                    lineheight = 1.3, margin = margin(t = 12))
  )

$dir.create("Maps", showWarnings = FALSE)
ggsave("Maps/day05_sight_buried_streams.png", p, width = 8, height = 9.5,
       dpi = 300, bg = dk$bg, device = ragg::agg_png)
message("Saved Maps/day05_sight_buried_streams.png")