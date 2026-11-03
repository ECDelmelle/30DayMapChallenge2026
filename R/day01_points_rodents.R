# Day 1 — Points ------------------------------------------------------------
# Philadelphia properties cited by L&I for rodents.
# Data: OpenDataPhilly "Licenses and Inspections Code Violations" (Carto table
# `violations`). 311 doesn't record rat complaints — they go to the Health
# Dept's Vector Control line, which isn't published — so code violations for
# rodent harborage / infestation are the best public, point-level record.

library(tidyverse)
library(sf)
library(httr2)
library(tigris)
library(ggspatial)
source("R/theme_ed.R")
options(tigris_use_cache = TRUE)

# ---- Settings --------------------------------------------------------------
start_date  <- "2024-11-01"   # two years; change freely
show_office <- TRUE           # the mouse in Meyerson Hall
raw_file    <- "Data/day01_li_rodent_violations.csv"
refresh     <- FALSE          # TRUE to re-download

# ---- Helper: Carto SQL -> tibble, failing loudly ---------------------------
carto <- function(sql) {
  resp <- request("https://phl.carto.com/api/v2/sql") |>
    req_url_query(q = sql, format = "csv") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  body <- resp_body_string(resp)
  if (resp_status(resp) >= 400 || startsWith(trimws(body), "{"))
    stop("Carto API error:\n", substr(body, 1, 500))
  read_csv(I(body), show_col_types = FALSE)
}

# ---- 1. Find the violation-title column and the rodent codes ---------------
cols <- names(carto("SELECT * FROM violations LIMIT 1"))
title_col <- str_subset(cols, "violationcodetitle|codetitle|violationdescription")[1]
if (is.na(title_col)) stop("No title column found. Columns are:\n",
                           paste(cols, collapse = ", "))
message("Using title column: ", title_col)

rodent_sql <- "(rodent|harborage|\\mrats?\\M|mice|vermin)"
codes <- carto(paste0(
  "SELECT ", title_col, " AS title, COUNT(*) AS n FROM violations ",
  "WHERE violationdate >= '", start_date, "' ",
  "AND ", title_col, " ~* '", rodent_sql, "' ",
  "GROUP BY 1 ORDER BY n DESC"))
print(codes, n = 40)   # <- check these are all rodent-related

# Broader peek, in case the rodent code lives under a generic pest title
carto(paste0(
  "SELECT ", title_col, " AS title, COUNT(*) AS n FROM violations ",
  "WHERE violationdate >= '", start_date, "' ",
  "AND ", title_col, " ~* '(pest|infest|extermin|insect)' ",
  "GROUP BY 1 ORDER BY n DESC")) |> print(n = 40)

if (nrow(codes) == 0)
  stop("No rodent violation titles found. Check the pest/infest table above.")

# ---- 2. Pull the rodent violations as points -------------------------------
if (refresh || !file.exists(raw_file)) {
  calls <- carto(paste0(
    "SELECT *, ST_Y(the_geom) AS lat, ST_X(the_geom) AS lon ",
    "FROM violations WHERE violationdate >= '", start_date, "' ",
    "AND ", title_col, " ~* '", rodent_sql, "'"))
  dir.create("Data", showWarnings = FALSE)
  write_csv(calls, raw_file)
} else {
  calls <- read_csv(raw_file, show_col_types = FALSE)
}

# One dot per property, not per citation (a building can rack up several)
addr_col <- intersect(c("opa_account_num", "address"), names(calls))[1]
calls <- calls |>
  arrange(violationdate) |>
  distinct(.data[[addr_col]], .keep_all = TRUE) |>
  mutate(requested_datetime = violationdate)

message(nrow(calls), " properties cited for rodents since ", start_date)

# ---- Spatial layers ---------------------------------------------------------
crs_phl <- 2272  # PA State Plane South (ft)

# Philly + the six counties that touch it
nbr_geoids <- c(Bucks = "42017", Montgomery = "42091", Delaware = "42045",
                Burlington = "34005", Camden = "34007", Gloucester = "34015")

counties_all <- counties(state = c("PA", "NJ"), cb = TRUE, year = 2024) |>
  filter(GEOID %in% c("42101", nbr_geoids)) |>
  st_transform(crs_phl)

phl <- filter(counties_all, GEOID == "42101")
nbr <- filter(counties_all, GEOID != "42101")

# Water for all seven counties, so the Delaware reads as one river
water <- map2(c("PA","PA","PA","PA","NJ","NJ","NJ"),
              c("Philadelphia","Bucks","Montgomery","Delaware",
                "Burlington","Camden","Gloucester"),
              \(s, c) area_water(s, c, year = 2024)) |>
  bind_rows() |>
  st_transform(crs_phl)

pts <- calls |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_phl) |>
  st_filter(phl)

# ---- Frame: Philly plus a margin so neighbours are visible -----------------
pad <- 9000  # feet
bb  <- st_bbox(phl)
bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad
bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
view <- st_as_sfc(bb)

# County labels at a point inside the *visible* part of each county
nbr_lab <- nbr |>
  st_intersection(view) |>
  mutate(geometry = st_point_on_surface(geometry),
         label = toupper(paste(NAME, "Co.")),
         state = if_else(STATEFP == "42", "PA", "NJ"),
         label = paste0(label, "\n", state))

# River labels at a point on the largest visible piece of each river
river_lab <- water |>
  filter(str_detect(FULLNAME, "^(Delaware|Schuylkill) Riv")) |>
  st_intersection(view) |>
  group_by(name = str_remove(FULLNAME, " Riv.*$")) |>
  summarise() |>
  st_cast("POLYGON") |>
  mutate(a = st_area(geometry)) |>
  slice_max(a, n = 1, by = name) |>
  mutate(geometry = st_point_on_surface(geometry),
         label = paste(name, "River"))

# ---- Labels -----------------------------------------------------------------
n_calls <- nrow(pts)
d_range <- range(as.Date(pts$requested_datetime))

subtitle <- sprintf(
  "Each dot is one of %s properties cited by L&amp;I for rodents, %s to %s.<br>On Oct. 28 the city named its first **Rat Abatement Manager**.",
  scales::comma(n_calls),
  format(d_range[1], "%b %Y"),
  format(d_range[2], "%b %Y")
)

office <- st_sfc(st_point(c(-75.192508, 39.952183)), crs = 4326) |>
  st_transform(crs_phl)
oxy <- st_coordinates(office)

# Dashed ring around the office
ring_r <- 1800  # feet
ring   <- st_buffer(office, ring_r)

# ---- Place the office label out in a neighbouring county --------------------
# Try candidate spots on the ground (outside Philly, not in water) and pick
# the one whose leader line crosses the fewest rodent dots. Override with
# manual coordinates (EPSG:2272 feet) if you prefer: label_xy <- c(x, y)
label_xy <- oxy[1, 1:2] + c(-23000, 14000)   # c(east/west, north/south), in feet

if (is.null(label_xy)) {
  view_land <- view |>
    st_difference(st_union(st_buffer(phl, 2500))) |>
    st_difference(st_make_valid(st_union(water)))
  inner <- st_buffer(view_land, -1500)               # keep off the frame edge
  cand  <- st_make_grid(view_land, n = c(40, 40), what = "centers")
  cand  <- cand[lengths(st_intersects(cand, inner)) > 0]
  d <- as.numeric(st_distance(cand, office))
  cand <- cand[d > 12000 & d < 40000]
  legs <- st_sfc(lapply(seq_along(cand), \(i)
                        st_linestring(rbind(oxy[1, 1:2], st_coordinates(cand[i])))), crs = crs_phl)
  hits <- lengths(st_is_within_distance(legs, pts, dist = 900))
  len  <- as.numeric(st_length(legs))
  best <- which.min(hits + len / 40000)              # fewest crossings, then shortest
  label_xy <- st_coordinates(cand[best])[1, 1:2]
  message("Leader crosses ", hits[best], " dots")
}


# Leader runs from the top of the ring (12 o'clock) to just short of the label
v     <- label_xy - oxy[1, 1:2]; u <- v / sqrt(sum(v^2))
lead0 <- oxy[1, 1:2] + c(0, ring_r + 150)
lead1 <- label_xy + c(5600, 1100)   # just right of "office", mid-height
lab_hjust <- if (u[1] < 0) 1 else 0

# ---- Map: ground -> water -> figure -> data -> labels -----------------------
p <- ggplot() +
  # ground: neighbouring counties recede
  geom_sf(data = nbr, fill = ed$land_nbr, colour = ed$line_nbr, linewidth = 0.25) +
  # figure: Philadelphia is lighter, so the eye lands there
  geom_sf(data = phl, fill = ed$land, colour = NA) +
  geom_sf(data = water, fill = ed$water, colour = NA) +
  geom_sf(data = phl, fill = NA, colour = ed$line, linewidth = 0.35) +
  # data
  geom_sf(data = pts, colour = ed$pop, shape = 16, size = 1.1,
          alpha = 0.55, stroke = 0) +
  # place + water labels: quiet, below the data in visual weight
  geom_sf_text(data = nbr_lab, aes(label = label), family = ed_font,
               size = 2.1, colour = ed$faint, lineheight = 0.9,
               fontface = "bold") +
  # office: small green dot, dashed green ring, leader out to the county
  {if (show_office) list(
    geom_sf(data = ring, fill = NA, colour = ed$alt, linewidth = 0.45,
            linetype = "22"),
    annotate("point", x = oxy[1], y = oxy[2], shape = 16, size = 1.3,
             colour = ed$alt),
    # curvature: 0 = straight; flip the sign to bend the other way
    annotate("curve", x = lead0[1], y = lead0[2],
             xend = lead1[1], yend = lead1[2], curvature = 0.4,
             ncp = 10, colour = ed$alt, linewidth = 0.3),
    annotate("text", x = label_xy[1], y = label_xy[2]+300,
             label = "+ one mouse,\nin my office", hjust = 0.5,
             vjust = 0, size = 3.3, family = ed_font, colour = ed$alt_dk,
             fontface = "bold", lineheight = 0.95)
  )} +
  ed_scalebar("br") +
  coord_sf(crs = crs_phl, xlim = bb[c("xmin", "xmax")],
           ylim = bb[c("ymin", "ymax")], expand = FALSE, datum = NA) +
  labs(
    title    = "Philly's rat czar has work to do",
    subtitle = subtitle,
    caption  = ed_caption(1, "Points", "Philadelphia L&I Code Violations (OpenDataPhilly); US Census TIGER/Line")
  ) +
  theme_ed() +
  theme(panel.border = element_rect(fill = NA, colour = ed$line_nbr, linewidth = 0.3),
        # markdown subtitle so the title can be bold (needs ggtext)
        plot.subtitle = ggtext::element_markdown(size = rel(0.95), colour = ed$muted,
                                                 lineheight = 1.25,
                                                 margin = margin(b = 12)))

ed_save(p, 1, "points_rodents", width = 8, height = 9.5)