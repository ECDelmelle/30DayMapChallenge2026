# Day 2 — Lines -------------------------------------------------------------
# Rail lines (active + abandoned) over Philadelphia's 1937 HOLC grades.
#
# Framing (after Hillier 2003, 2005): the HOLC map recorded how appraisers
# already judged neighborhoods — race, housing age, and nearness to rail and
# industry — rather than causing lending discrimination. So the map asks:
# where did appraisers see "risk", and how much of that tracks the rails?
#
# Data
#   HOLC polygons: Mapping Inequality (Univ. of Richmond DSL), CC BY-NC
#   Rail lines:    OpenStreetMap (railway = rail / abandoned / disused / razed)
# Caveat: OSM's record of removed rail is incomplete, and some lines were
# built or removed after 1937 — treat it as "the rail network, past and present".

library(tidyverse)
library(sf)
library(osmdata)
library(ggspatial)
source("R/theme_ed.R")
source("R/base_phl.R")

b <- phl_base()

holc_file <- "Data/holc_philadelphia.gpkg"
rail_file <- "Data/osm_rail_philadelphia.gpkg"
near_ft   <- 1320   # quarter mile

# ---- HOLC grades -------------------------------------------------------------
if (!file.exists(holc_file)) {
  urls <- c(
    "https://dsl.richmond.edu/panorama/redlining/static/mappinginequality.gpkg",
    "https://s3-west.nrp-nautilus.io/public-mappinginequality/mappinginequality.parquet"
  )
  holc_all <- NULL
  for (u in urls) {
    holc_all <- tryCatch({
      message("Trying ", u)
      if (grepl("parquet$", u)) sfarrow::st_read_parquet(u) else read_sf(paste0("/vsicurl/", u))
    }, error = \(e) { message("  failed: ", conditionMessage(e)); NULL })
    if (!is.null(holc_all)) break
  }
  if (is.null(holc_all))
    stop("Couldn't download HOLC data. Download Philadelphia's GeoJSON from ",
         "https://dsl.richmond.edu/panorama/redlining/data and save it as ", holc_file)
  holc <- holc_all |> filter(city == "Philadelphia", state == "PA")
  write_sf(holc, holc_file)
}
holc <- read_sf(holc_file) |>
  st_make_valid() |>
  st_transform(crs_phl) |>
  filter(grade %in% c("A", "B", "C", "D"))
holc |> st_drop_geometry() |> count(grade) |> print()

# ---- Rail lines from OSM ---------------------------------------------------
# Overpass is swamped every November (#30DayMapChallenge!). So: split the
# area into 2x2 tiles, cache each tile, and rotate through mirrors on failure.
# Re-running picks up where it left off.
overpass_mirrors <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.private.coffee/api/interpreter"
)

fetch_tile <- function(bbox_ll, tries = 8) {
  for (i in seq_len(tries)) {
    url <- overpass_mirrors[(i - 1) %% length(overpass_mirrors) + 1]
    osmdata::set_overpass_url(url)
    res <- tryCatch(
      opq(bbox = bbox_ll, timeout = 180) |>
        add_osm_feature("railway",
                        c("rail", "abandoned", "disused", "razed", "dismantled")) |>
        osmdata_sf(),
      error = \(e) { message("  ", basename(dirname(url)), ": ", conditionMessage(e)); NULL })
    if (!is.null(res)) return(res$osm_lines)
    Sys.sleep(min(5 * i, 30))
  }
  stop("All Overpass mirrors failed for this tile. Wait a bit and re-run; ",
       "finished tiles are cached in Data/osm_tiles/.")
}

if (!file.exists(rail_file)) {
  dir.create("Data/osm_tiles", recursive = TRUE, showWarnings = FALSE)
  tiles <- st_make_grid(st_transform(b$view, 4326), n = c(2, 2))
  parts <- vector("list", length(tiles))
  for (k in seq_along(tiles)) {
    tf <- sprintf("Data/osm_tiles/rail_tile_%d.rds", k)
    if (file.exists(tf)) { parts[[k]] <- readRDS(tf); next }
    message("Tile ", k, "/", length(tiles))
    lines <- fetch_tile(st_bbox(tiles[k]))
    keep  <- intersect(c("osm_id", "name", "railway", "usage", "service", "operator"),
                       names(lines))
    lines <- if (is.null(lines) || nrow(lines) == 0) NULL else lines[, keep]
    saveRDS(lines, tf)
    parts[[k]] <- lines
  }
  bind_rows(compact(parts)) |>
    distinct(osm_id, .keep_all = TRUE) |>
    write_sf(rail_file)
}
rail <- read_sf(rail_file)
if (!"service" %in% names(rail)) rail$service <- NA_character_
rail <- rail |>
  st_transform(crs_phl) |>
  filter(!(service %in% c("yard", "siding", "crossover")) | is.na(service)) |>
  mutate(status = if_else(railway == "rail", "Active rail", "Abandoned or removed rail"),
         status = factor(status, c("Active rail", "Abandoned or removed rail"))) |>
  filter(!is.na(railway))
rail |> st_drop_geometry() |> count(status, railway) |> print()

# ---- The numbers: how much of each grade sits within 1/4 mile of rail? ---
rail_zone <- rail |> st_buffer(near_ft) |> st_union()
near <- holc |>
  group_by(grade) |>
  summarise()
near$area <- as.numeric(st_area(near))
near$near <- vapply(seq_len(nrow(near)), \(i)
                    sum(as.numeric(st_area(st_intersection(st_geometry(near)[i], rail_zone)))),
                    numeric(1))
near$pct <- near$near / near$area * 100
near |> st_drop_geometry() |> select(grade, pct) |> print()

pct_d <- round(near$pct[near$grade == "D"])
pct_a <- round(near$pct[near$grade == "A"])

# Industrial flag (if present) — another thing appraisers penalized
if ("industrial" %in% names(holc))
  holc |> st_drop_geometry() |> count(grade, industrial) |> print()

# ---- Map ----------------------------------------------------------------------
holc_d   <- filter(holc, grade == "D")
holc_abc <- filter(holc, grade != "D")

# Display rails: main lines only (drop spurs/yards/sidings — they clutter the
# port). Magenta inside the study area (Philly + the HOLC map's extent),
# quiet grey outside it, so the regional network is context, not story.
rail_main <- filter(rail, is.na(service))
study     <- st_union(c(st_geometry(b$phl), st_geometry(st_union(holc)))) |>
  st_make_valid()
rail_in   <- suppressWarnings(st_intersection(rail_main, study))

subtitle <- sprintf(paste0(
  "In 1937, federal appraisers graded Philly's neighborhoods for mortgage risk.<br>",
  "**%d%%** of the area they called **\"hazardous\"** lay within a quarter mile of a rail line,<br>",
  "vs. %d%% of the top-graded areas. The grades reflected what lenders<br>",
  "were already doing; the maps didn't create redlining."),
  pct_d, pct_a)

p <- ggplot() +
  base_layers(b) +
  # graded areas: A-C as a faint outline (shows the map's extent), D as a wash
  geom_sf(data = holc_abc, fill = NA, colour = ed$line, linewidth = 0.15) +
  geom_sf(data = holc_d, aes(fill = "Graded \"hazardous\" (D), 1937"),
          colour = NA) +
  # rails outside the study area: grey context, no legend
  geom_sf(data = filter(rail_main, status == "Active rail"),
          colour = "#C4C4C4", linewidth = 0.25) +
  # rails inside: ghosts first, active on top
  geom_sf(data = arrange(rail_in, desc(status)),
          aes(colour = status, linetype = status, linewidth = status),
          key_glyph = "path") +
  scale_fill_manual(values = "#D2B1A6", name = NULL) +
  scale_colour_manual(values = c("Active rail" = ed$pop,
                                 "Abandoned or removed rail" = ed$alt), name = NULL) +
  scale_linetype_manual(values = c("Active rail" = "solid",
                                   "Abandoned or removed rail" = "42"), name = NULL) +
  scale_linewidth_manual(values = c("Active rail" = 0.3,
                                    "Abandoned or removed rail" = 0.45), name = NULL) +
  guides(fill = guide_legend(order = 1,
                             override.aes = list(colour = NA)),
         colour = guide_legend(order = 2, override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(order = 2), linewidth = guide_legend(order = 2)) +
  base_finish(b) +
  ed_scalebar("br") +
  labs(
    title    = "Appraisal Risk & Rails in 1937",
    subtitle = subtitle,
    caption  = ed_caption(2, "Lines",
                          "Mapping Inequality (Univ. of Richmond DSL); OpenStreetMap contributors; US Census TIGER/Line\nFraming after Hillier (2003, 2005) on the Philadelphia HOLC map")
  ) +
  theme_ed() +
  theme(panel.border = element_rect(fill = NA, colour = ed$line_nbr, linewidth = 0.3),
        plot.subtitle = ggtext::element_markdown(size = rel(0.95), colour = ed$muted,
                                                 lineheight = 1.3,
                                                 margin = margin(b = 10)),
        legend.position = "top",
        legend.justification = "left",
        legend.margin = margin(0, 0, 6, 0),
        legend.key.width = unit(1.4, "lines"),
        legend.spacing.x = unit(0.3, "lines"))

ed_save(p, 2, "lines_rails_holc", width = 8, height = 9.8)