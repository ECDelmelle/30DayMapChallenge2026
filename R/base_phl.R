# base_phl.R ---------------------------------------------------------------
# Reusable Philadelphia basemap layers (city, 6 bordering counties, water,
# frame, county labels) in PA State Plane South (ft). Cached after first run.
#   source("R/base_phl.R"); b <- phl_base(); b$phl, b$nbr, b$water, b$bb ...
#   base_layers(b) adds the ground / water / figure layers to a ggplot.

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

crs_phl <- 2272

phl_base <- function(pad = 9000, cache = "Data/base_phl.rds") {
  if (file.exists(cache)) {
    b <- readRDS(cache)
    if (identical(b$pad, pad)) return(b)
  }
  nbr_geoids <- c("42017", "42091", "42045", "34005", "34007", "34015")
  counties_all <- counties(state = c("PA", "NJ"), cb = TRUE, year = 2024) |>
    dplyr::filter(GEOID %in% c("42101", nbr_geoids)) |>
    st_transform(crs_phl)
  phl <- dplyr::filter(counties_all, GEOID == "42101")
  nbr <- dplyr::filter(counties_all, GEOID != "42101")
  
  water <- purrr::map2(
    c("PA", "PA", "PA", "PA", "NJ", "NJ", "NJ"),
    c("Philadelphia", "Bucks", "Montgomery", "Delaware",
      "Burlington", "Camden", "Gloucester"),
    \(s, c) area_water(s, c, year = 2024)) |>
    dplyr::bind_rows() |>
    st_transform(crs_phl)
  
  bb <- st_bbox(phl)
  bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad
  bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
  view <- st_as_sfc(bb)
  
  nbr_lab <- nbr |>
    st_intersection(view) |>
    dplyr::mutate(geometry = st_point_on_surface(geometry),
                  label = paste0(toupper(paste(NAME, "Co.")), "\n",
                                 ifelse(STATEFP == "42", "PA", "NJ")))
  
  b <- list(phl = phl, nbr = nbr, water = water, bb = bb, view = view,
            nbr_lab = nbr_lab, pad = pad)
  dir.create(dirname(cache), showWarnings = FALSE)
  saveRDS(b, cache)
  b
}

# Ground -> figure -> water -> outline. Add data layers after this.
base_layers <- function(b) {
  list(
    geom_sf(data = b$nbr, fill = ed$land_nbr, colour = ed$line_nbr, linewidth = 0.25),
    geom_sf(data = b$phl, fill = ed$land, colour = NA),
    geom_sf(data = b$water, fill = ed$water, colour = NA),
    geom_sf(data = b$phl, fill = NA, colour = ed$line, linewidth = 0.35)
  )
}

# County labels + frame coordinates. Add near the end, after data layers.
base_finish <- function(b) {
  list(
    geom_sf_text(data = b$nbr_lab, aes(label = label), family = ed_font,
                 size = 2.1, colour = ed$faint, lineheight = 0.9,
                 fontface = "bold"),
    coord_sf(crs = crs_phl, xlim = b$bb[c("xmin", "xmax")],
             ylim = b$bb[c("ymin", "ymax")], expand = FALSE, datum = NA)
  )
}