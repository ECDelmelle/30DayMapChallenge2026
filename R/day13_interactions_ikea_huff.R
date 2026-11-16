# Day 13 — Interactions -------------------------------------------------------
# Alla vägar leder till IKEA: a Huff model of who shops at which IKEA.
#
# Every county in the lower 48 sends its population to the IKEAs, split by a
# Huff model with distance decay. Desire lines run from each county's
# population-weighted centre to its three most likely stores; line weight is
# expected shoppers (Pop_i x P_ij). A side panel walks through the equation
# with one worked county, the way it would go on a lecture slide.
#
# Model
#   P_ij = A_j d_ij^-beta / sum_k A_k d_ik^-beta
#   All full-size IKEAs are roughly the same size, so A_j cancels and the model
#   is pure competition by distance (a "probabilistic Voronoi").
#   Distance is great-circle miles; beta = 2 is the textbook default.
#
# Data
#   Origins: US Census 2020 county centres of population (population-weighted
#            mean centre + 2020 population), CenPop2020_Mean_CO.txt
#   Stores:  hand list of full-size US IKEA stores (below), coordinates
#            approximate. CHECK against https://www.ikea.com/us/en/stores/
#            before posting: 2026 openings are in a separate block, and the
#            small-format / Plan & Order points are left out on purpose.
#   Base:    US Census cartographic boundary states (tigris)

library(tidyverse)
library(sf)
library(tigris)
library(patchwork)
library(ggtext)
source("R/theme_ed.R")
options(tigris_use_cache = TRUE)

crs_us <- 5070        # CONUS Albers equal area
beta   <- 2           # distance decay
top_n  <- 3           # desire lines drawn per county
example_fips <- "42027"   # worked example in the side panel: Centre County, PA

# ---- Swedish palette (overrides the usual grey + magenta for today) ----------
sv <- list(
  bg      = "#0A2540",   # deep navy page
  land    = "#0F3460",   # land, a step lighter
  line    = "#24517F",   # state lines
  yellow  = "#FECC02",   # flag yellow
  yel_lt  = "#FFE27A",
  blue    = "#006AA7",   # flag blue
  ink     = "#F5F1E6",   # near-white text
  muted   = "#9DB7D3",   # secondary text
  faint   = "#5F82A8"
)

# Jost is a free Futura-alike (IKEA's catalogue face until 2009). Falls back to
# your usual font if systemfonts can't fetch it.
sv_font <- local({
  have <- function() "Jost" %in% systemfonts::system_fonts()$family ||
    "Jost" %in% systemfonts::registry_fonts()$family
  if (!have() && exists("require_font", asNamespace("systemfonts")))
    try(systemfonts::require_font("Jost"), silent = TRUE)
  if (have()) "Jost" else ed_font
})

# ---- Stores -----------------------------------------------------------------
# Full-size stores. Memphis closed May 2026 and is left out.
stores <- tribble(
  ~store,              ~st,  ~lat,     ~lon,
  "Tempe",             "AZ", 33.4345, -111.9331,
  "Burbank",           "CA", 34.1834, -118.3146,
  "Carson",            "CA", 33.8615, -118.2593,
  "Costa Mesa",        "CA", 33.6920, -117.9187,
  "Covina",            "CA", 34.0690, -117.8790,
  "East Palo Alto",    "CA", 37.4610, -122.1390,
  "Emeryville",        "CA", 37.8314, -122.2920,
  "San Diego",         "CA", 32.7800, -117.1210,
  "West Sacramento",   "CA", 38.5870, -121.5420,
  "Centennial",        "CO", 39.5790, -104.8730,
  "New Haven",         "CT", 41.2990, -72.9210,
  "Jacksonville",      "FL", 30.2580, -81.5300,
  "Miami",             "FL", 25.7920, -80.3810,
  "Orlando",           "FL", 28.4290, -81.4310,
  "Sunrise",           "FL", 26.1500, -80.3200,
  "Tampa",             "FL", 27.9580, -82.4340,
  "Atlanta",           "GA", 33.7900, -84.3990,
  "Bolingbrook",       "IL", 41.7190, -88.0420,
  "Schaumburg",        "IL", 42.0600, -88.0400,
  "Fishers",           "IN", 39.9580, -85.9920,
  "Merriam",           "KS", 39.0270, -94.6930,
  "College Park",      "MD", 39.0290, -76.9230,
  "Baltimore",         "MD", 39.3720, -76.4620,
  "Stoughton",         "MA", 42.1390, -71.0710,
  "Canton",            "MI", 42.3240, -83.4840,
  "Bloomington",       "MN", 44.8580, -93.2410,
  "St. Louis",         "MO", 38.6330, -90.2510,
  "Las Vegas",         "NV", 36.0690, -115.2420,
  "Elizabeth",         "NJ", 40.6640, -74.1710,
  "Paramus",           "NJ", 40.9210, -74.0700,
  "Brooklyn",          "NY", 40.6720, -74.0110,
  "Long Island",       "NY", 40.7780, -73.5310,
  "Charlotte",         "NC", 35.3400, -80.7630,
  "Columbus",          "OH", 40.1430, -82.9720,
  "West Chester",      "OH", 39.3420, -84.4290,
  "Portland",          "OR", 45.5700, -122.5610,
  "Conshohocken",      "PA", 40.0830, -75.2930,
  "Philadelphia",      "PA", 39.9190, -75.1420,
  "Pittsburgh",        "PA", 40.4520, -80.1690,
  "Frisco",            "TX", 33.0930, -96.8210,
  "Grand Prairie",     "TX", 32.7900, -97.0320,
  "Houston",           "TX", 29.7880, -95.4600,
  "Live Oak",          "TX", 29.5640, -98.3330,
  "Round Rock",        "TX", 30.5570, -97.6890,
  "Draper",            "UT", 40.5190, -111.8920,
  "Norfolk",           "VA", 36.8600, -76.2080,
  "Woodbridge",        "VA", 38.6390, -77.2900,
  "Renton",            "WA", 47.4420, -122.2200,
  "Oak Creek",         "WI", 42.8820, -87.9180
)

# 2026 openings. Flip to TRUE once you've confirmed which are open and
# full-size; coordinates are city-level placeholders.
include_2026 <- FALSE
stores_2026 <- tribble(
  ~store,              ~st,  ~lat,     ~lon,
  "Huntsville",        "AL", 34.7300, -86.5860,
  "Phoenix",           "AZ", 33.4480, -112.0740,
  "Dallas (Univ. Park)", "TX", 32.8670, -96.7890,
  "Rockwall",          "TX", 32.9000, -96.4600,
  "Webster",           "TX", 29.5380, -95.1180,
  "Chantilly",         "VA", 38.8940, -77.4310,
  "Gurnee",            "IL", 42.3870, -87.9540,
  "Tulsa",             "OK", 36.1540, -95.9930,
  "Fort Collins",      "CO", 40.5850, -105.0840
)
if (include_2026) stores <- bind_rows(stores, stores_2026)

stores_sf <- st_as_sf(stores, coords = c("lon", "lat"), crs = 4326)

# ---- Origins: population-weighted county centres ------------------------------
cenpop_file <- "Data/CenPop2020_Mean_CO.txt"
dir.create("Data", showWarnings = FALSE)
if (!file.exists(cenpop_file))
  download.file(
    "https://www2.census.gov/geo/docs/reference/cenpop2020/county/CenPop2020_Mean_CO.txt",
    cenpop_file, mode = "wb")

lower48_drop <- c("02", "15", "60", "66", "69", "72", "78")

cty <- read_csv(cenpop_file, col_types = cols(.default = "c"),
                locale = locale(encoding = "latin1")) |>
  set_names(c("statefp", "countyfp", "county", "state", "pop", "lat", "lon")) |>
  mutate(fips = paste0(statefp, countyfp),
         across(c(pop, lat, lon), as.numeric)) |>
  filter(!statefp %in% lower48_drop, !is.na(lat), pop > 0) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

states48 <- states(cb = TRUE, resolution = "20m", year = 2024) |>
  filter(!STATEFP %in% lower48_drop) |>
  st_transform(crs_us)

# ---- Huff model ---------------------------------------------------------------
d_mi <- st_distance(cty, stores_sf) |> units::set_units("mi") |> units::drop_units()
d_mi <- pmax(d_mi, 1)                      # no zero distances
w    <- d_mi^(-beta)                       # A_j = 1 for every store
P    <- w / rowSums(w)

huff <- tibble(
  fips  = rep(cty$fips, times = nrow(stores)),
  pop   = rep(cty$pop,  times = nrow(stores)),
  store = rep(stores$store, each = nrow(cty)),
  dist  = as.vector(d_mi),
  p     = as.vector(P)
) |>
  mutate(flow = pop * p)

# Each store's expected catchment (all counties, not just the drawn lines)
catch <- huff |> group_by(store) |> summarise(shoppers = sum(flow)) |>
  arrange(desc(shoppers))
print(catch, n = 10)

# ---- Desire lines ---------------------------------------------------------------
o_xy <- cty |> st_transform(crs_us) |> st_coordinates()
rownames(o_xy) <- cty$fips
s_xy <- stores_sf |> st_transform(crs_us) |> st_coordinates()
rownames(s_xy) <- stores$store

lines <- huff |>
  group_by(fips) |>
  slice_max(p, n = top_n, with_ties = FALSE) |>
  ungroup() |>
  arrange(flow) |>                          # big flows drawn last, on top
  mutate(
    lw    = scales::rescale(sqrt(flow), to = c(0.04, 1.6)),
    alpha = scales::rescale(log10(flow), to = c(0.10, 0.95))
  )
lines$geometry <- st_sfc(
  map2(lines$fips, lines$store,
       \(f, s) st_linestring(rbind(o_xy[f, ], s_xy[s, ]))),
  crs = crs_us)
lines <- st_as_sf(lines)

ex_lines <- filter(lines, fips == example_fips)
ex_pt    <- cty |> filter(fips == example_fips) |> st_transform(crs_us)

stores_pts <- stores_sf |> left_join(catch, by = "store") |> st_transform(crs_us)

# ---- Main map -------------------------------------------------------------------
title_txt <- "ALLA VÄGAR LEDER TILL IKEA"
sub_txt <- paste0(
  "All roads lead to IKEA. Desire lines from every county in the lower 48 to its three ",
  "most likely stores under a Huff model.<br>Line weight is expected shoppers. ",
  "Highlighted lines: <span style='color:", sv$ink, "'>", ex_pt$county, " County, ",
  ex_pt$state, "</span>, worked through on the right."
)

p_map <- ggplot() +
  geom_sf(data = states48, fill = sv$land, colour = sv$line, linewidth = 0.25) +
  # glow, then the lines themselves
  geom_sf(data = lines, aes(linewidth = lw * 4, alpha = alpha * 0.06),
          colour = sv$yellow, lineend = "round") +
  geom_sf(data = lines, aes(linewidth = lw, alpha = alpha),
          colour = sv$yellow, lineend = "round") +
  # worked example
  geom_sf(data = ex_lines, colour = sv$ink, linewidth = 0.7, alpha = 1) +
  geom_sf(data = ex_pt, shape = 21, size = 2.4, fill = sv$ink, colour = sv$bg,
          stroke = 0.5) +
  # stores
  geom_sf(data = stores_pts, shape = 21, size = 2.6, fill = sv$yellow,
          colour = sv$blue, stroke = 0.9) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  coord_sf(crs = crs_us, datum = NA, expand = FALSE) +
  labs(title = title_txt, subtitle = sub_txt) +
  theme_void(base_family = sv_font) +
  theme(
    plot.background  = element_rect(fill = sv$bg, colour = NA),
    panel.background = element_rect(fill = sv$bg, colour = NA),
    plot.title = element_text(face = "bold", size = 30, colour = sv$yellow,
                              margin = margin(b = 6)),
    plot.subtitle = element_markdown(size = 11.5, colour = sv$muted,
                                     lineheight = 1.35, margin = margin(b = 14)),
    plot.title.position = "plot"
  )

# ---- Side panel: the equation ---------------------------------------------------
eq_general <- "P[ij] == frac(A[j] * d[ij]^{-beta}, sum(A[k] * d[ik]^{-beta}, k, ''))"
eq_ikea    <- "P[ij] == frac(d[ij]^{-beta}, sum(d[ik]^{-beta}, k, ''))"
eq_flow    <- "T[ij] == Pop[i] %*% P[ij]"

txt <- function(y, label, size = 3.5, colour = sv$muted, h = 0.18) {
  annotate(ggtext::GeomTextBox, x = 0, y = y, label = label, hjust = 0, vjust = 1,
           halign = 0, width = unit(1, "npc"), box.colour = NA, fill = NA,
           family = sv_font, size = size, colour = colour, lineheight = 1.3,
           box.padding = unit(0, "pt"))
}
eq <- function(y, label, size = 5.2) {
  annotate("text", x = 0.5, y = y, label = label, parse = TRUE,
           family = sv_font, size = size, colour = sv$ink)
}
yl <- function(x) paste0("<span style='color:", sv$yellow, "'>", x, "</span>")

p_eq <- ggplot() +
  txt(1.00, paste0("<b>", yl("THE HUFF MODEL"), "</b>"), size = 5.2) +
  txt(0.93, paste0("The probability that someone in county ", yl("<i>i</i>"),
                   " shops at IKEA ", yl("<i>j</i>"), " depends on its pull ",
                   "relative to every other IKEA:")) +
  eq(0.73, eq_general) +
  txt(0.61, paste0("<i>A</i> is store attractiveness, usually size. ",
                   "But one IKEA is about as big as another, so ",
                   yl("<i>A</i> cancels"), " and only distance is left:")) +
  eq(0.41, eq_ikea) +
  txt(0.29, paste0("With ", yl(paste0("β = ", beta)), ", twice the distance means ",
                   "a quarter of the pull. Each desire line carries the county's ",
                   "expected shoppers:")) +
  eq(0.08, eq_flow, size = 4.6) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.04, 1.02), expand = c(0, 0)) +
  theme_void(base_family = sv_font) +
  theme(plot.background = element_rect(fill = sv$bg, colour = NA))

# ---- Side panel: worked example ---------------------------------------------------
ex <- huff |>
  filter(fips == example_fips) |>
  arrange(desc(p)) |>
  mutate(rank = row_number(),
         store = if_else(rank <= 4, store, "All other IKEAs")) |>
  group_by(store) |>
  summarise(p = sum(p), dist = min(dist), rank = min(rank), .groups = "drop") |>
  arrange(rank) |>
  mutate(label = if_else(store == "All other IKEAs", store,
                         sprintf("%s  ·  %.0f mi", store, dist)),
         label = fct_rev(fct_inorder(label)),
         other = store == "All other IKEAs")

ex_title <- sprintf("<b>%s</b><br><span style='color:%s'>%s County, %s · %s people</span>",
                    yl("WORKED EXAMPLE"), sv$muted, ex_pt$county, ex_pt$state,
                    scales::comma(ex_pt$pop))

p_ex <- ggplot(ex, aes(x = p, y = label)) +
  geom_col(aes(fill = other), width = 0.62) +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), hjust = -0.15,
            family = sv_font, size = 3.6, colour = sv$ink, fontface = "bold") +
  scale_fill_manual(values = c(`FALSE` = sv$yellow, `TRUE` = sv$faint), guide = "none") +
  scale_x_continuous(limits = c(0, max(ex$p) * 1.25), expand = c(0, 0)) +
  labs(title = ex_title, x = NULL, y = NULL,
       caption = sprintf("Expected trips to the top store: %s people × %.0f%% ≈ %s",
                         scales::comma(ex_pt$pop), ex$p[1] * 100,
                         scales::comma(ex_pt$pop * ex$p[1], accuracy = 100))) +
  theme_void(base_family = sv_font) +
  theme(
    plot.background = element_rect(fill = sv$bg, colour = NA),
    plot.title = element_markdown(size = 12, colour = sv$ink, lineheight = 1.3,
                                  margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8.5, colour = sv$muted, hjust = 0,
                                margin = margin(t = 8)),
    plot.caption.position = "plot",
    axis.text.y = element_text(colour = sv$muted, size = 9.5, hjust = 1,
                               margin = margin(r = 4))
  )

# ---- Assemble -------------------------------------------------------------------
side <- (wrap_elements(full = p_eq) +
           theme(plot.background = element_rect(fill = sv$bg, colour = NA),
                 plot.margin = margin(0, 0, 0, 0))) / p_ex + plot_layout(heights = c(1.55, 1))

final <- (p_map | side) +
  plot_layout(widths = c(2.6, 1)) +
  plot_annotation(
    caption = ed_caption(13, "Interactions",
      "US Census 2020 county centres of population; IKEA store locations (hand list, full-size stores); Census TIGER/Line.\nGreat-circle distance, all stores weighted equally. Huff, D. (1963), A probabilistic analysis of shopping center trade areas."),
    theme = theme(
      plot.background = element_rect(fill = sv$bg, colour = NA),
      plot.caption = element_text(family = sv_font, size = 8, colour = sv$faint,
                                  hjust = 0, lineheight = 1.3, margin = margin(t = 10)),
      plot.margin = margin(22, 26, 14, 26)
    )
  )

dir.create("Maps", showWarnings = FALSE)
ggsave("Maps/day13_interactions_ikea_huff.png", final, width = 15, height = 8.4,
       dpi = 300, bg = sv$bg, device = ragg::agg_png)
message("Saved Maps/day13_interactions_ikea_huff.png")
