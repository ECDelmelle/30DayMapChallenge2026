# Day 12 — Power -------------------------------------------------------------
# The Power Broker: Robert Moses's roads and toll crossings in New York City.
#
# Framing (after Caro, The Power Broker, 1974): Moses never won an election.
# His power came from appointed posts (as many as 12 at once) and above all
# from the Triborough Bridge and Tunnel Authority (TBTA), whose tolls gave him
# revenue, bonds and independence that no mayor or governor could take away.
# The map shows the crossings that made the money (dark magenta), the
# expressways and parkways built under him (magenta), and his headquarters
# under the Triborough Bridge on Randall's Island.
#
# Data
#   Roads:     OpenStreetMap (highway = motorway / trunk, plus named crossings)
#   Base:      US Census TIGER/Line cartographic boundaries + area water
# Caveats
#   - "Built under Moses" is a hand list (below) of roads he built as Parks
#     Commissioner, TBTA chair and City Construction Coordinator. Many were
#     joint city/state/federal projects; edit `moses_roads` to taste.
#   - Matching is by today's OSM names (RFK Bridge = Triborough, Jackie
#     Robinson Pkwy = Interborough, Hugh L. Carey Tunnel = Brooklyn-Battery).
#   - Miles halve one-way carriageways so divided highways aren't counted twice.

library(tidyverse)
library(sf)
library(osmdata)
library(tigris)
library(ggspatial)
library(ggtext)
library(ggrepel)
source("R/theme_ed.R")
options(tigris_use_cache = TRUE)

crs_nyc  <- 2263   # NY State Plane Long Island (ft)
roads_file <- "Data/osm_nyc_highways.gpkg"
dir.create("Data", showWarnings = FALSE)

# ---- What counts as Moses ----------------------------------------------------
# Expressways & parkways (OSM name regexes; "." stands in for dashes)
moses_roads <- c(
  "Cross Bronx Expressway", "Major Deegan Expressway", "Bruckner Expressway",
  "Sheridan Expressway", "Brooklyn.Queens Expressway", "Gowanus Expressway",
  "Prospect Expressway", "Long Island Expressway", "Van Wyck Expressway",
  "Clearview Expressway", "Whitestone Expressway", "Throgs Neck Expressway",
  "Staten Island Expressway", "Grand Central Parkway", "Belt Parkway",
  "Shore Parkway", "Southern Parkway", "Cross Island Parkway",
  "Laurelton Parkway", "Jackie Robinson Parkway", "Interborough Parkway",
  "Henry Hudson Parkway", "Harlem River Drive", "FDR Drive",
  "Franklin D. Roosevelt East River Drive", "Hutchinson River Parkway",
  "Bronx River Parkway"
)
moses_regex <- paste(moses_roads, collapse = "|")

# TBTA toll crossings — the revenue base (short label, OSM name regex)
crossings <- tribble(
  ~crossing,           ~pattern,
  "Triborough",        "Robert F. Kennedy Bridge|Triborough",
  "Bronx-Whitestone",  "Whitestone Bridge",
  "Throgs Neck",       "Throgs Neck Bridge",
  "Verrazzano",        "Verraz+ano",
  "Henry Hudson",      "Henry Hudson Bridge",
  "Marine Parkway",    "Marine Parkway",
  "Cross Bay",         "Cross Bay",
  "Brooklyn-Battery",  "Hugh L. Carey Tunnel|Brooklyn.Battery Tunnel",
  "Queens-Midtown",    "Queens.Midtown Tunnel"
)
cross_regex <- paste(crossings$pattern, collapse = "|")

# Parks: his first power base, as Parks Commissioner (1934-1960)
park_key <- tribble(
  ~park,               ~pattern,
  "Flushing Meadows",  "Flushing Meadows",
  "Jacob Riis Park",   "Jacob Riis",
  "Orchard Beach",     "Orchard Beach",
  "Randall's Island",  "Randall.?s Island",
  "Riverside Park",    "Riverside Park"
)
# The 11 WPA pools opened in summer 1936 (today's names where renamed)
pool_key <- tribble(
  ~pool,               ~pattern,
  "Hamilton Fish",     "Hamilton Fish",
  "Thomas Jefferson",  "Thomas Jefferson",
  "Highbridge",        "Highbridge",
  "Jackie Robinson",   "Jackie Robinson|Colonial",
  "Crotona",           "Crotona",
  "Sunset Park",       "Sunset Park",
  "McCarren",          "McCarren",
  "Betsy Head",        "Betsy Head",
  "Sol Goldman",       "Sol Goldman|Red Hook",
  "Astoria",           "Astoria",
  "Lyons",             "Lyons|Tompkinsville"
)
green_regex <- paste(c(park_key$pattern, pool_key$pattern), collapse = "|")

# first label whose pattern matches each name
first_match <- function(x, labels, patterns) map_chr(x, \(n) {
  if (is.na(n)) return(NA_character_)
  hit <- labels[str_detect(n, patterns)]
  if (length(hit)) hit[1] else NA_character_
})

# ---- Base map: NYC, neighbours, water -----------------------------------------
nyc_base <- function(pad = 6000, cache = "Data/base_nyc.rds") {
  if (file.exists(cache)) {
    b <- readRDS(cache)
    if (identical(b$pad, pad)) return(b)
  }
  nyc_geoids <- c("36005", "36047", "36061", "36081", "36085")
  nbr_geoids <- c("36119", "36059", "34017", "34003", "34013", "34039",
                  "34023", "34025")
  cty <- counties(state = c("NY", "NJ"), cb = TRUE, year = 2024) |>
    filter(GEOID %in% c(nyc_geoids, nbr_geoids)) |>
    st_transform(crs_nyc)
  nyc <- filter(cty, GEOID %in% nyc_geoids)
  nbr <- filter(cty, GEOID %in% nbr_geoids)
  
  water <- map2(substr(cty$GEOID, 1, 2), substr(cty$GEOID, 3, 5),
                \(s, c) area_water(s, c, year = 2024)) |>
    bind_rows() |>
    st_transform(crs_nyc)
  
  bb <- st_bbox(nyc)
  bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad
  bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
  view <- st_as_sfc(bb)
  
  b <- list(nyc = nyc, nyc_u = st_make_valid(st_union(nyc)), nbr = nbr,
            water = water, bb = bb, view = view, pad = pad)
  saveRDS(b, cache)
  b
}
b <- nyc_base()

# lon/lat -> projected x/y, for annotations
xy <- function(lon, lat) {
  st_sfc(st_point(c(lon, lat)), crs = 4326) |> st_transform(crs_nyc) |>
    st_coordinates() |> as.numeric()
}

# ---- Roads from OSM (tiled + cached; Overpass is busy every November) -----------
overpass_mirrors <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.private.coffee/api/interpreter"
)
keep_cols <- c("osm_id", "name", "highway", "oneway", "ref", "bridge", "tunnel")

parse_lines <- function(res) {
  x <- res$osm_lines
  if (is.null(x) || nrow(x) == 0) return(NULL)
  for (k in setdiff(keep_cols, names(x))) x[[k]] <- NA_character_
  x[, keep_cols]
}

parse_green <- function(res) {
  grab <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    for (k in setdiff(c("osm_id", "name", "leisure"), names(x))) x[[k]] <- NA_character_
    x <- x[, c("osm_id", "name", "leisure")]
    x$osm_id <- as.character(x$osm_id)
    x
  }
  pts <- grab(res$osm_points)
  if (!is.null(pts)) pts <- filter(pts, !is.na(name), !is.na(leisure))
  out <- bind_rows(compact(list(grab(res$osm_polygons),
                                grab(res$osm_multipolygons), pts)))
  if (nrow(out) == 0) NULL else out
}

fetch_tile <- function(bbox_ll, build, parse = parse_lines, tries = 8) {
  for (i in seq_len(tries)) {
    url <- overpass_mirrors[(i - 1) %% length(overpass_mirrors) + 1]
    osmdata::set_overpass_url(url)
    res <- tryCatch(opq(bbox = bbox_ll, timeout = 180) |> build() |> osmdata_sf(),
                    error = \(e) { message("  ", url, ": ", conditionMessage(e)); NULL })
    if (!is.null(res)) return(parse(res))
    Sys.sleep(min(5 * i, 30))
  }
  stop("All Overpass mirrors failed. Wait and re-run; finished tiles are cached.")
}

queries <- list(
  hwy   = \(q) add_osm_feature(q, "highway", c("motorway", "trunk")),
  cross = \(q) q |> add_osm_feature("highway") |>
    add_osm_feature("name", cross_regex, value_exact = FALSE)
)

if (!file.exists(roads_file)) {
  dir.create("Data/osm_tiles_nyc", recursive = TRUE, showWarnings = FALSE)
  tiles <- st_make_grid(st_transform(b$view, 4326), n = c(2, 2))
  parts <- list()
  for (qn in names(queries)) for (k in seq_along(tiles)) {
    tf <- sprintf("Data/osm_tiles_nyc/%s_%d.rds", qn, k)
    if (!file.exists(tf)) {
      message(qn, " tile ", k, "/", length(tiles))
      saveRDS(fetch_tile(st_bbox(tiles[k]), queries[[qn]]), tf)
    }
    parts[[tf]] <- readRDS(tf)
  }
  bind_rows(compact(parts)) |>
    distinct(osm_id, .keep_all = TRUE) |>
    write_sf(roads_file)
}
roads <- read_sf(roads_file) |> st_transform(crs_nyc)
st_geometry(roads) <- "geometry"   # gpkg calls it "geom"; standardise the name

# ---- Parks & pools from OSM ---------------------------------------------------
green_file  <- "Data/osm_nyc_green.rds"
green_query <- \(q) q |>
  add_osm_feature("leisure", c("park", "swimming_pool", "beach_resort")) |>
  add_osm_feature("name", green_regex, value_exact = FALSE)

if (!file.exists(green_file)) {
  dir.create("Data/osm_tiles_nyc", recursive = TRUE, showWarnings = FALSE)
  tiles <- st_make_grid(st_transform(b$view, 4326), n = c(2, 2))
  parts <- list()
  for (k in seq_along(tiles)) {
    tf <- sprintf("Data/osm_tiles_nyc/green_%d.rds", k)
    if (!file.exists(tf)) {
      message("green tile ", k, "/", length(tiles))
      saveRDS(fetch_tile(st_bbox(tiles[k]), green_query, parse_green), tf)
    }
    parts[[tf]] <- readRDS(tf)
  }
  bind_rows(compact(parts)) |> distinct(osm_id, .keep_all = TRUE) |> saveRDS(green_file)
}
green <- readRDS(green_file) |> st_transform(crs_nyc) |> st_make_valid()
st_geometry(green) <- "geometry"

green_nyc <- suppressWarnings(st_intersection(green, b$nyc_u)) |>
  mutate(
    park = if_else(leisure %in% c("park", "beach_resort"),
                   first_match(name, park_key$park, park_key$pattern), NA_character_),
    pool = if_else(leisure %in% "swimming_pool",
                   first_match(name, pool_key$pool, pool_key$pattern), NA_character_)
  )
parks <- filter(green_nyc, !is.na(park))
parks <- parks[st_dimension(parks) == 2, ]
pools <- green_nyc |>
  filter(!is.na(pool)) |>
  group_by(pool) |>
  summarise() |>
  st_centroid() |>
  suppressWarnings()

parks |> st_drop_geometry() |> count(park, name) |> print()
cat("Pools found:", nrow(pools), "of 11. Missing:",
    paste(setdiff(pool_key$pool, pools$pool), collapse = ", "), "\n")

# ---- Classify -----------------------------------------------------------------
roads <- roads |>
  mutate(
    crossing = map_chr(name, \(n) {
      if (is.na(n)) return(NA_character_)
      hit <- crossings$crossing[str_detect(n, crossings$pattern)]
      if (length(hit)) hit[1] else NA_character_
    }),
    # only the bridge/tunnel spans count as crossings, not same-named streets
    crossing = if_else(!is.na(bridge) | !is.na(tunnel), crossing, NA_character_),
    class = case_when(
      !is.na(crossing)                  ~ "cross",
      str_detect(coalesce(name, ""), moses_regex) ~ "moses",
      highway %in% c("motorway", "trunk") ~ "other",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(class))

roads |> st_drop_geometry() |> count(class) |> print()
roads |> st_drop_geometry() |> filter(class == "cross") |> count(crossing) |> print()
roads |> st_drop_geometry() |> filter(class == "moses") |> count(name, sort = TRUE) |>
  print(n = 50)
# Anything Moses-ish we *didn't* catch? Eyeball the unmatched motorway names:
roads |> st_drop_geometry() |> filter(class == "other", highway == "motorway") |>
  count(name, sort = TRUE) |> print(n = 40)

in_nyc <- suppressWarnings(st_intersection(roads, b$nyc_u))

# ---- The numbers --------------------------------------------------------------
miles <- function(x) {
  half <- if_else(x$oneway %in% c("yes", "-1", "true", "1"), 0.5, 1)
  sum(as.numeric(st_length(x)) * half) / 5280
}
moses_mi  <- miles(filter(in_nyc, class %in% c("moses", "cross")))
moses_mw  <- miles(filter(in_nyc, class %in% c("moses", "cross"), highway == "motorway"))
all_mw    <- miles(filter(in_nyc, highway == "motorway"))
share     <- moses_mw / all_mw * 100
n_cross   <- n_distinct(na.omit(in_nyc$crossing))
cat(sprintf("Moses roads in NYC: %.0f mi | %.0f%% of NYC motorway miles | %d crossings\n",
            moses_mi, share, n_cross))

# ---- Map layers ---------------------------------------------------------------
other_all <- filter(roads, class == "other" | !st_intersects(roads, b$nyc_u, sparse = FALSE)[, 1])
moses_in  <- filter(in_nyc, class == "moses")
cross_in  <- filter(in_nyc, class == "cross")

# Caro's "One Mile": the Cross Bronx through East Tremont
tremont_box <- st_bbox(c(xmin = -73.900, ymin = 40.836, xmax = -73.876, ymax = 40.852),
                       crs = 4326) |> st_as_sfc() |> st_transform(crs_nyc)
one_mile <- moses_in |>
  filter(str_detect(name, "Cross Bronx")) |>
  st_intersection(tremont_box) |>
  suppressWarnings()

# Crossing labels (one per crossing)
cross_lab <- cross_in |>
  group_by(crossing) |>
  summarise() |>
  st_centroid() |>
  suppressWarnings()
cross_lab <- bind_cols(st_drop_geometry(cross_lab), as_tibble(st_coordinates(cross_lab)))

hq   <- xy(-73.9240, 40.7985)          # TBTA admin building, Randall's Island
hq_t <- xy(-74.030, 40.845)            # label sits over New Jersey
om   <- xy(-73.888, 40.844)            # One Mile
om_t <- xy(-73.815, 40.926)            # top-left corner of the note

boro_lab <- tribble(
  ~label,          ~lon,     ~lat,
  "BRONX",         -73.865,  40.883,
  "MANHATTAN",     -73.990,  40.762,
  "BROOKLYN",      -73.945,  40.645,
  "QUEENS",        -73.795,  40.712,
  "STATEN\nISLAND", -74.150, 40.585
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_nyc)

nbr_lab <- tribble(
  ~label,         ~lon,     ~lat,
  "NEW JERSEY",   -74.130,  40.735,
  "NASSAU CO.",   -73.715,  40.690
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_nyc)

lab_other <- "Other highways"
lab_moses <- "Expressways & parkways built under Moses"
lab_cross <- "TBTA toll crossings: his money and his independence"
lab_park  <- "Parks & beaches he built or remade as Parks Commissioner"
lab_pool  <- "The 11 public pools he opened in the summer of 1936"

subtitle <- sprintf(paste0(
  "Robert Moses never won an election. He held as many as 12 appointed posts at once.<br>",
  "As **<span style='color:%s'>Parks Commissioner</span>** he remade beaches and opened 11 public pools in one summer.<br>",
  "Tolls from his **<span style='color:%s'>Triborough Bridge and Tunnel Authority</span>** crossings gave him money no<br>",
  "mayor could touch. He used that power to drive **≈%.0f miles** of expressway and parkway<br>",
  "through the five boroughs: **%.0f%%** of the city's highway miles today."),
  ed$alt_dk, ed$pop_dk, moses_mi, share)

# ---- Map ----------------------------------------------------------------------
p <- ggplot() +
  # ground: water everywhere, then land
  geom_sf(data = b$view, fill = ed$water, colour = NA) +
  geom_sf(data = b$nbr, fill = ed$land_nbr, colour = ed$line_nbr, linewidth = 0.25) +
  geom_sf(data = b$nyc, fill = ed$land, colour = NA) +
  geom_sf(data = b$water, fill = ed$water, colour = NA) +
  geom_sf(data = b$nyc, fill = NA, colour = ed$line, linewidth = 0.3) +
  geom_sf_text(data = boro_lab, aes(label = label), family = ed_font, size = 2.6,
               colour = ed$faint, fontface = "bold", lineheight = 0.9) +
  geom_sf_text(data = nbr_lab, aes(label = label), family = ed_font, size = 2.1,
               colour = ed$faint, fontface = "bold") +
  # parks (under the roads)
  geom_sf(data = parks, aes(fill = lab_park), colour = ed$alt, linewidth = 0.2) +
  # roads
  geom_sf(data = other_all, aes(colour = lab_other, linewidth = lab_other),
          key_glyph = "path") +
  geom_sf(data = one_mile, colour = ed$pop_lt, linewidth = 2.4) +   # halo
  geom_sf(data = moses_in, aes(colour = lab_moses, linewidth = lab_moses),
          key_glyph = "path") +
  geom_sf(data = cross_in, aes(colour = lab_cross, linewidth = lab_cross),
          key_glyph = "path") +
  # pools (on top)
  geom_sf(data = pools, aes(shape = lab_pool), fill = ed$alt, colour = "white",
          size = 2.3, stroke = 0.5) +
  scale_fill_manual(values = setNames(alpha(ed$alt, 0.35), lab_park), name = NULL) +
  scale_shape_manual(values = setNames(21, lab_pool), name = NULL) +
  scale_colour_manual(values = setNames(c("#C4C4C4", ed$pop, ed$pop_dk),
                                        c(lab_other, lab_moses, lab_cross)),
                      breaks = c(lab_cross, lab_moses, lab_other), name = NULL) +
  scale_linewidth_manual(values = setNames(c(0.25, 0.55, 1.3),
                                           c(lab_other, lab_moses, lab_cross)),
                         breaks = c(lab_cross, lab_moses, lab_other), name = NULL) +
  # crossing names
  geom_text_repel(data = cross_lab, aes(x = X, y = Y, label = crossing),
                  family = ed_font, size = 2.3,
                  fontface = "bold", colour = ed$pop_dk, bg.colour = ed$land,
                  bg.r = 0.15, min.segment.length = 0.3, segment.colour = ed$pop_dk,
                  segment.size = 0.25, box.padding = 0.4, seed = 12) +
  # HQ
  annotate("curve", x = hq_t[1] + 800, y = hq_t[2] - 600, xend = hq[1] - 500, yend = hq[2] + 300,
           curvature = -0.25, colour = ed$pop_dk, linewidth = 0.3,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  annotate("point", x = hq[1], y = hq[2], shape = 23, size = 3.4,
           fill = ed$pop_dk, colour = "white", stroke = 0.6) +
  annotate("text", x = hq_t[1], y = hq_t[2], hjust = 1, vjust = 0.5,
           family = ed_font, size = 2.5, colour = ed$ink, lineheight = 1.05,
           label = "His seat of power: TBTA offices\nunder the Triborough toll plaza,\nRandall's Island (1934-1968)") +
  # One Mile
  annotate("curve", x = om_t[1] - 600, y = om_t[2] - 6000, xend = om[1] + 500, yend = om[2] + 600,
           curvature = 0.3, colour = ed$pop_dk, linewidth = 0.3,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  annotate("text", x = om_t[1], y = om_t[2], hjust = 0, vjust = 1,
           family = ed_font, size = 2.5, colour = ed$ink, lineheight = 1.05,
           label = "\"One Mile\": the Cross Bronx\nthrough East Tremont. 1,530\nfamilies lost their homes when\nMoses refused an alternate route.") +
  coord_sf(crs = crs_nyc, xlim = b$bb[c("xmin", "xmax")], ylim = b$bb[c("ymin", "ymax")],
           expand = FALSE, datum = NA) +
  ed_scalebar("br") +
  labs(
    title    = "The Power Broker",
    subtitle = subtitle,
    caption  = ed_caption(12, "Power",
                          "OpenStreetMap contributors; US Census TIGER/Line. Road list after Caro, The Power Broker (1974),\nwho credits Moses with \"627 miles of roads in and around New York City.\" Many were joint state/federal projects.")
  ) +
  guides(colour = guide_legend(order = 1, ncol = 1,
                               override.aes = list(linewidth = c(1.3, 0.8, 0.5))),
         linewidth = guide_legend(order = 1),
         fill  = guide_legend(order = 2),
         shape = guide_legend(order = 3)) +
  theme_ed() +
  theme(panel.border = element_rect(fill = NA, colour = ed$line_nbr, linewidth = 0.3),
        plot.subtitle = element_markdown(size = rel(0.95), colour = ed$muted,
                                         lineheight = 1.3, margin = margin(b = 10)),
        legend.position = "top",
        legend.justification = "left",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.spacing.y = unit(0, "lines"),
        legend.margin = margin(0, 0, 6, 0),
        legend.key.width = unit(1.6, "lines"),
        legend.key.height = unit(0.8, "lines"))

ed_save(p, 12, "power_moses", width = 8, height = 10.8)