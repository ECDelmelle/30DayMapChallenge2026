# day18_null_blue_line.R -------------------------------------------------------
# #30DayMapChallenge 2026 · Day 18: NULL
# Same train, same homes, nine control groups. Did the LYNX Blue Line Extension
# (opened March 2018) raise sale prices of homes within half a mile of its new
# stations? It depends on who you compare them with. Each panel re-runs the
# chapter's DiD with one of its nine control groups; the nulls are in magenta.
#
# Figure A: county-wide, one pooled estimate per control group, with the control
#           sales drawn so you can see how different each comparison group is.
# Figure B: zoom to the corridor, one estimate per station per control group.
#
# HOW TO RUN: open the TODBookChapter project and run DiD_Analysis_Nilsson.Rmd
# through the chunk that builds `dfs` (the nine datasets). This script uses
# `dfs` and `mecktrt` from that session, refits the chapter's DiD model, and
# draws the maps. The models are the chapter's: log price, house controls,
# year + tract fixed effects, ordinary lm standard errors.

# ---- Settings ------------------------------------------------------------------
theme_path <- "R/theme_ed.R"   # path to your 30DayMapChallenge theme file
# folder holding LYNX_Blue_Line_Stations.shp (the Rmd reads it with here())
if (!exists("data_dir"))
  data_dir <- if (file.exists("data/LYNX_Blue_Line_Stations.shp")) "data" else here::here()
if (!exists("alpha")) alpha <- 0.05             # p >= alpha counts as NULL (the chapter's stars start at 0.10)
min_post   <- 10               # stations with fewer post-2018 treated sales: "too few to say"

if (!exists("dfs") || !exists("mecktrt"))
  stop("Run DiD_Analysis_Nilsson.Rmd through the chunk that builds `dfs` first.")

source(theme_path)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(broom)
library(ggrepel)

spec_order <- c("Rest", "Distance", "Planned",
                "PSMprop", "PSMneig", "PSMboth",
                "CEMprop", "CEMneig", "CEMboth")
spec_labels <- c(
  Rest     = "All homes 1.5+ mi from any station",
  Distance = "Homes 1.5–2 mi from new stations",
  Planned  = "Homes near planned Silver Line",
  PSMprop  = "PSM on house traits",
  PSMneig  = "PSM on neighborhood traits",
  PSMboth  = "PSM on both",
  CEMprop  = "CEM on house traits",
  CEMneig  = "CEM on neighborhood traits",
  CEMboth  = "CEM on both"
)

# ---- Stations and treated areas --------------------------------------------------
all_st <- stations_sf |>
  st_transform(2264) |>
  mutate(NAME = trimws(NAME), ext = StationTyp == "Blue Line Extension Station")
ext_st <- filter(all_st, ext)
old_st <- filter(all_st, !ext)

# As in the chapter: treated = within 1/2 mile of a new station and more than
# 1.5 miles from an old one. Each station gets the part of its buffer that is
# closer to it than to any other new station, so neighbouring circles don't overlap.
no_treat <- st_union(st_buffer(old_st, 7920))
cells <- st_collection_extract(st_voronoi(st_union(ext_st), envelope = st_as_sfc(st_bbox(st_buffer(ext_st, 1e5))))) |>
  st_sf(geometry = _)
cells$NAME <- ext_st$NAME[st_nearest_feature(cells, ext_st)]
station_area <- st_buffer(ext_st, 2640) |>
  select(NAME) |>
  st_intersection(select(cells, cell = NAME)) |>
  filter(NAME == cell) |>
  select(-cell) |>
  st_difference(no_treat) |>
  filter(!st_is_empty(geometry))

footprint <- st_union(station_area)

# ---- Models ----------------------------------------------------------------------
rhs <- "lot_ac + age + age2 + heatedarea + heatedarea2 + bedrooms + fullbaths +
        distcc_mi + distcc_mi2 + year_fe + trt_fe"

prep <- function(df) {
  df <- st_as_sf(df)
  df$year_fe     <- factor(df$year)
  df$trt_fe      <- factor(df$GEOID)
  df$age2        <- df$age^2
  df$distcc_mi2  <- df$distcc_mi^2
  df$heatedarea2 <- df$heatedarea^2
  df$lnprice     <- log(df$price)
  # nearest new station for each treated sale
  df$station <- NA_character_
  tr <- df$treat == 1
  df$station[tr] <- ext_st$NAME[st_nearest_feature(df[tr, ], ext_st)]
  df$tstation <- relevel(factor(ifelse(tr, df$station, "control")), ref = "control")
  df
}

fit_spec <- function(df, spec) {
  message("Fitting ", spec, " (n = ", nrow(df), ")")
  d <- st_drop_geometry(df)

  pooled <- lm(as.formula(paste("lnprice ~ treat * postopen +", rhs)), data = d) |>
    tidy() |>
    filter(term == "treat:postopen") |>
    transmute(spec, estimate, std.error, p.value,
              n_treat = sum(d$treat == 1), n_ctrl = sum(d$treat == 0))

  by_station <- lm(as.formula(paste("lnprice ~ postopen + tstation + tstation:postopen +", rhs)),
                   data = d) |>
    tidy() |>
    filter(grepl("tstation", term), grepl("postopen", term)) |>
    mutate(NAME = sub(":?postopen:?", "", sub("tstation", "", term))) |>
    select(NAME, estimate, std.error, p.value)

  counts <- d |>
    filter(treat == 1) |>
    count(NAME = station, postopen) |>
    pivot_wider(names_from = postopen, values_from = n, names_prefix = "n_", values_fill = 0)
  if (!"n_1" %in% names(counts)) counts$n_1 <- 0

  station_res <- tibble(NAME = station_area$NAME) |>
    left_join(counts, by = "NAME") |>
    left_join(by_station, by = "NAME") |>
    mutate(spec = spec, n_1 = coalesce(n_1, 0L))

  list(pooled = pooled, station = station_res)
}

classify <- function(est, p, enough = TRUE) {
  case_when(!enough | is.na(est) ~ "few",
            p >= alpha            ~ "null",
            est > 0               ~ "up",
            TRUE                  ~ "down")
}

dfs_p <- map(dfs[spec_order], prep)
fits  <- imap(dfs_p, fit_spec)

pooled <- map_dfr(fits, "pooled") |>
  mutate(status = classify(estimate, p.value),
         pct    = 100 * (exp(estimate) - 1),
         spec   = factor(spec, spec_order))

stations_res <- map_dfr(fits, "station") |>
  mutate(status = classify(estimate, p.value, enough = n_1 >= min_post),
         pct    = 100 * (exp(estimate) - 1),
         spec   = factor(spec, spec_order))

print(pooled, width = Inf)
dir.create("Maps", showWarnings = FALSE)
write.csv(pooled, "Maps/day18_pooled_estimates.csv", row.names = FALSE)
write.csv(stations_res, "Maps/day18_station_estimates.csv", row.names = FALSE)

# ---- Labels ------------------------------------------------------------------------
fmt_pct <- function(x) sprintf("%s%.1f%%", ifelse(x >= 0, "+", "−"), abs(x))
fmt_p   <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.2f", p))

pooled <- pooled |>
  mutate(
    strip = sprintf("%s\n%s control sales", spec_labels[as.character(spec)],
                    format(n_ctrl, big.mark = ",", trim = TRUE)),
    big   = ifelse(status == "null", "NULL", fmt_pct(pct)),
    small = ifelse(status == "null",
                   sprintf("%s, %s", fmt_pct(pct), fmt_p(p.value)),
                   fmt_p(p.value))
  )
strip_lab <- setNames(pooled$strip, as.character(pooled$spec))

n_null <- sum(pooled$status == "null")
num_word <- c("No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")[n_null + 1]

fill_vals <- c(null = ed$pop, up = "#5C5C5C", down = "#A6A6A6", few = ed$land)
fill_labs <- c(null = sprintf("NULL (p ≥ %.2f)", alpha),
               up   = "Prices rose (significant)",
               down = "Prices fell (significant)",
               few  = sprintf("Fewer than %d sales after opening", min_post))

# ---- Figure A: the county, one answer per control group -------------------------
county <- st_union(st_make_valid(st_geometry(mecktrt)))
tracts <- st_geometry(mecktrt)

set.seed(18)
ctrl_pts <- imap_dfr(dfs_p, function(df, spec) {
  ctrl <- df[df$treat == 0, ]
  ctrl <- ctrl[sample(nrow(ctrl), min(nrow(ctrl), 15000)), ]
  xy <- st_coordinates(ctrl)
  tibble(spec = factor(spec, spec_order), x = xy[, 1], y = xy[, 2])
})

treated_by_spec <- st_sf(spec = pooled$spec, status = pooled$status,
                         geometry = rep(footprint, nrow(pooled)))

bb <- st_bbox(county)
# top-left corner of the frame, which Mecklenburg's shape leaves empty
lab_xy <- tibble(x = bb["xmin"], y = bb["ymax"],
                 y2 = bb["ymax"] - 0.105 * (bb["ymax"] - bb["ymin"]))
pooled_lab <- bind_cols(pooled, lab_xy)

pA <- ggplot() +
  geom_sf(data = county, fill = ed$land, colour = ed$line, linewidth = 0.3) +
  geom_point(data = ctrl_pts, aes(x, y), colour = "#7A7A7A", size = 0.22, alpha = 0.45,
             stroke = 0) +
  geom_sf(data = st_geometry(old_st), colour = ed$muted, size = 0.25) +
  geom_sf(data = treated_by_spec, aes(fill = status), colour = NA) +
  geom_sf(data = treated_by_spec |> filter(status == "null"),
          fill = NA, colour = ed$pop, linewidth = 1.6, alpha = 0.3) +
  geom_text(data = pooled_lab, aes(x, y, label = big,
                                   colour = status == "null"),
            hjust = 0, vjust = 1, family = ed_font, fontface = "bold", size = 6.5) +
  geom_text(data = pooled_lab, aes(x, y2, label = small, colour = status == "null"),
            hjust = 0, vjust = 1, family = ed_font, size = 2.6) +
  scale_colour_manual(values = c(`TRUE` = ed$pop, `FALSE` = ed$ink), guide = "none") +
  scale_fill_manual(values = fill_vals, labels = fill_labs, drop = TRUE,
                    name = "Homes within ½ mile of a new station") +
  facet_wrap(~spec, ncol = 3, labeller = as_labeller(strip_lab)) +
  coord_sf(crs = 2264, datum = NA) +
  labs(
    title    = sprintf("Null, Null, Not Null", num_word),
    subtitle = paste0(
      "Did the LYNX Blue Line Extension (opened 2018) raise home prices within half a mile of its new\n",
      "stations? Each panel compares the same homes with a different control group (grey dots) in the\n",
      "same difference-in-differences model. Big numbers are the estimated price change after opening;\n",
      "magenta marks the panels where the effect cannot be told apart from zero."),
    caption = ed_caption(18, "NULL",
      "Mecklenburg County residential sales 2004–2023, ACS 2005–09, CATS. DiD on log price with house controls,\nyear and tract fixed effects (Delmelle & Nilsson, TOD book chapter, forthcoming. we think.). Control dots sampled to 15,000 per panel.")
  ) +
  theme_ed() +
  theme(
    strip.text = element_text(family = ed_font, colour = ed$ink, size = 8.5, hjust = 0,
                              lineheight = 1.1, margin = margin(t = 8, b = 3)),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "top", legend.justification = "left",
    legend.margin = margin(b = 6)
  ) +
  guides(fill = guide_legend(title.position = "left", override.aes = list(colour = NA)))

pA
ed_save(pA, 18, "18-delmelle", width = 9.5, height = 12)

# ---- Figure B: the corridor, one answer per station per control group ------------
st_by_spec <- station_area |>
  inner_join(stations_res, by = "NAME") |>
  mutate(status = factor(status, c("null", "up", "down", "few")))

frame <- st_bbox(st_buffer(footprint, 3500))
tracts_c <- st_crop(st_make_valid(st_sf(geometry = tracts)), frame)
line_pts <- st_crop(all_st, frame)

name_lab <- station_area |>
  st_point_on_surface() |>
  mutate(spec = factor("Rest", spec_order),
         x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

panel_lab <- pooled |>
  transmute(spec, lab = ifelse(status == "null", "Pooled: NULL", paste("Pooled:", fmt_pct(pct))),
            pop = status == "null",
            x = frame["xmax"] - 0.02 * (frame["xmax"] - frame["xmin"]),
            y = frame["ymin"] + 0.04 * (frame["ymax"] - frame["ymin"]))

pB <- ggplot() +
  geom_sf(data = tracts_c, fill = ed$land, colour = ed$line_nbr, linewidth = 0.2) +
  geom_sf(data = line_pts, colour = ed$muted, size = 0.5) +
  geom_sf(data = st_by_spec, aes(fill = status), colour = "white", linewidth = 0.3) +
  geom_sf(data = st_by_spec |> filter(status == "few"), fill = NA, colour = ed$faint,
          linewidth = 0.3, linetype = "22") +
  geom_text_repel(data = name_lab, aes(x, y, label = NAME), family = ed_font, size = 2.2,
                  colour = ed$ink, seed = 18, box.padding = 0.5, min.segment.length = 0,
                  segment.colour = ed$faint, segment.size = 0.2, nudge_x = -3000) +
  geom_text(data = panel_lab, aes(x, y, label = lab, colour = pop), hjust = 1, vjust = 0,
            family = ed_font, fontface = "bold", size = 2.8) +
  scale_colour_manual(values = c(`TRUE` = ed$pop, `FALSE` = ed$ink), guide = "none") +
  scale_fill_manual(values = fill_vals, labels = fill_labs, drop = FALSE,
                    name = "Station area result") +
  facet_wrap(~spec, ncol = 3, labeller = as_labeller(spec_labels)) +
  coord_sf(crs = 2264, datum = NA, expand = FALSE,
           xlim = frame[c("xmin", "xmax")], ylim = frame[c("ymin", "ymax")]) +
  labs(
    title    = "Null, Null, Not Null",
    subtitle = paste0(
      "The same model with a separate estimate for each new Blue Line station. Change the control group\n",
      "and stations switch between a significant price rise and a NULL (magenta). Station areas are homes\n",
      "within half a mile, more than 1.5 miles from the original Blue Line; uptown stations drop out."),
    caption = ed_caption(18, "NULL",
      "Mecklenburg County residential sales 2004–2023, ACS 2005–09, CATS. DiD on log price with station × post terms,\nhouse controls, year and tract fixed effects (Delmelle & Nilsson, TOD book chapter).")
  ) +
  theme_ed() +
  theme(
    strip.text = element_text(family = ed_font, colour = ed$ink, size = 8.5, hjust = 0,
                              face = "bold", margin = margin(t = 8, b = 3)),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "top", legend.justification = "left",
    legend.margin = margin(b = 6)
  )

pB
ed_save(pB, 18, "null_blue_line_stations", width = 9.5, height = 11)
