# Day 1 — Points ------------------------------------------------------------
# Philadelphia 311 requests mentioning rats, mice or rodents.
# Data: OpenDataPhilly 311 Service and Information Requests (Carto SQL API).

library(tidyverse)
library(sf)
library(httr2)
library(tigris)
source("R/theme_ed.R")
options(tigris_use_cache = TRUE)

# ---- Settings --------------------------------------------------------------
start_date  <- "2025-11-01"   # last 12 months; widen for more dots
show_office <- TRUE           # the mouse in Meyerson Hall
raw_file    <- "Data/day01_311_rodents.csv"

# ---- Get data (cached after first run) ------------------------------------
# Word-boundary regex (Postgres \m \M) so "rat" doesn't match "operator".
pattern <- "\\m(rats?|rodents?|mouse|mice|vermin)\\M"

sql <- paste0(
  "SELECT service_request_id, requested_datetime, status, service_name, ",
  "subject, status_notes, address, lat, lon ",
  "FROM public_cases_fc ",
  "WHERE requested_datetime >= '", start_date, "' ",
  "AND (subject ~* '", pattern, "' ",
  "OR service_name ~* '", pattern, "' ",
  "OR status_notes ~* '", pattern, "')"
)

if (!file.exists(raw_file)) {
  dir.create("Data", showWarnings = FALSE)
  resp <- request("https://phl.carto.com/api/v2/sql") |>
    req_url_query(q = sql, format = "csv") |>
    req_perform()
  writeLines(resp_body_string(resp), raw_file)
}
calls <- read_csv(raw_file, show_col_types = FALSE)

# Sanity check: which request types are we catching? Eyeball for false hits.
calls |> count(service_name, sort = TRUE) |> print(n = 30)

# ---- Spatial layers ---------------------------------------------------------
crs_phl <- 2272  # PA State Plane South (ft)

phl <- places(state = "PA", cb = TRUE, year = 2024) |>
  filter(NAME == "Philadelphia") |>
  st_transform(crs_phl)

water <- area_water("PA", "Philadelphia", year = 2024) |>
  st_transform(crs_phl)

pts <- calls |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_phl) |>
  st_filter(phl)

bb <- st_bbox(phl)
pad <- 2500  # feet

# ---- Labels -----------------------------------------------------------------
n_calls <- nrow(pts)
d_range <- range(as.Date(pts$requested_datetime))

subtitle <- sprintf(
  "%s 311 requests mentioning rats, mice or rodents, %s to %s.\nOn Oct. 28 the city named its first Rat Abatement Manager.",
  scales::comma(n_calls),
  format(d_range[1], "%b %Y"),
  format(d_range[2], "%b %Y")
)

office <- st_sfc(st_point(c(-75.1927, 39.9522)), crs = 4326) |>
  st_transform(crs_phl)
office_xy <- st_coordinates(office)

# ---- Map --------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = phl,   fill = ed$land,  colour = ed$line, linewidth = 0.3) +
  geom_sf(data = water, fill = ed$water, colour = NA) +
  geom_sf(data = pts, colour = ed$pop, shape = 16, size = 0.45,
          alpha = 0.4, stroke = 0) +
  {if (show_office) list(
    annotate("point", x = office_xy[1], y = office_xy[2], shape = 21,
             size = 3.2, stroke = 0.7, colour = ed$pop_dk, fill = NA),
    annotate("text", x = office_xy[1] - 9000, y = office_xy[2] - 9000,
             label = "+ one mouse,\nin my office", hjust = 1, size = 2.6,
             family = ed_font, colour = ed$pop_dk, lineheight = 0.95),
    annotate("segment", x = office_xy[1] - 8500, y = office_xy[2] - 8000,
             xend = office_xy[1] - 900, yend = office_xy[2] - 900,
             colour = ed$pop_dk, linewidth = 0.3)
  )} +
  coord_sf(crs = crs_phl,
           xlim = c(bb["xmin"] - pad, bb["xmax"] + pad),
           ylim = c(bb["ymin"] - pad, bb["ymax"] + pad),
           expand = FALSE) +
  labs(
    title    = "Philly's rat czar has work to do",
    subtitle = subtitle,
    caption  = ed_caption(1, "Points", "City of Philadelphia 311 via OpenDataPhilly")
  ) +
  theme_ed()

ed_save(p, 1, "points_rodents", width = 8, height = 9)
