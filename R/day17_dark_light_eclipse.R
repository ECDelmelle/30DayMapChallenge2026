# day17_dark_light_eclipse.R --------------------------------------------------
# #30DayMapChallenge 2026 · Day 17: Dark and Light
# The total solar eclipse of 12 August 2026, Iceland to Iberia, in black and white.
# Black = totality. Everywhere else is shaded by how much sunlight was left at
# maximum eclipse, on a log scale (roughly how your eyes judge brightness), so
# it fades out from the path. Comporta, where I watched it, is the one colour.
#
# No eclipse data to download: the shadow is computed here from NASA's
# Besselian elements (Espenak), which describe the Moon's shadow over time.

source("R/theme_ed.R")

library(dplyr)
library(sf)
library(scales)
library(ggrepel)

# ---- Besselian elements: 2026 Aug 12, t0 = 18:00 TDT --------------------------
# eclipse.gsfc.nasa.gov/SEbeselm/SEbeselm2001/SE2026Aug12Tbeselm.html
bes <- list(
  t0 = 18, dT = 69.2,   # ΔT in seconds (NASA predicted 71.4; ~69 s measured in 2026)
  x  = c(0.475593, 0.5189288, -0.0000773, -0.0000088),
  y  = c(0.771161, -0.2301664, -0.0001245, 0.0000037),
  d  = c(14.79667, -0.012065, -0.000003),
  l1 = c(0.537954, 0.0000940, -0.0000121),
  l2 = c(-0.008142, 0.0000935, -0.0000121),
  mu = c(88.74776, 15.003093),
  tf1 = 0.0046141, tf2 = 0.0045911
)
pv <- function(a, t) sum(a * t^(seq_along(a) - 1))

# Fraction of the Sun's disc covered: circles of radius a (Sun) and b (Moon)
# whose centres are D apart
covered <- function(D, a, b) {
  out  <- numeric(length(D))
  tot  <- D <= b - a                     # Moon covers the whole Sun
  ann  <- D <= a - b                     # Moon wholly inside the Sun (annular)
  part <- !(tot | ann | D >= a + b)
  d <- D[part]; A <- a[part]; B <- b[part]
  lens <- A^2 * acos(pmin(1, (d^2 + A^2 - B^2) / (2 * d * A))) +
          B^2 * acos(pmin(1, (d^2 + B^2 - A^2) / (2 * d * B))) -
          0.5 * sqrt(pmax(0, (-d + A + B) * (d + A - B) * (d - A + B) * (d + A + B)))
  out[part] <- lens / (pi * A^2)
  out[tot]  <- 1
  out[ann]  <- (b[ann] / a[ann])^2
  out
}

# Step through time (hours from t0) and keep each place's deepest eclipse,
# counting only moments when the Sun is above the horizon
eclipse_max <- function(lon, lat, t = seq(-1.2, 1.4, by = 5 / 3600)) {
  rad <- pi / 180
  u  <- atan(0.99664719 * tan(lat * rad))     # geocentric latitude (sea level)
  rs <- 0.99664719 * sin(u)
  rc <- cos(u)
  best <- numeric(length(lon)); t_best <- rep(NA_real_, length(lon))
  total <- logical(length(lon))
  for (tt in t) {
    x <- pv(bes$x, tt); y <- pv(bes$y, tt); d <- pv(bes$d, tt) * rad
    H <- (pv(bes$mu, tt) + lon - 0.00417807 * bes$dT) * rad
    xi   <- rc * sin(H)
    eta  <- rs * cos(d) - rc * cos(H) * sin(d)
    zeta <- rs * sin(d) + rc * cos(H) * cos(d)
    L1 <- pv(bes$l1, tt) - zeta * bes$tf1     # penumbra radius on the observer's plane
    L2 <- pv(bes$l2, tt) - zeta * bes$tf2     # umbra radius (negative = total)
    D  <- sqrt((x - xi)^2 + (y - eta)^2)
    cov <- covered(D, a = (L1 + L2) / 2, b = (L1 - L2) / 2)
    cov[zeta <= 0] <- 0                       # Sun below the horizon
    up <- cov > best
    best[up] <- cov[up]; t_best[up] <- tt
    total <- total | (zeta > 0 & D < -L2)
  }
  tibble(covered = best, total = total,
         ut = bes$t0 + t_best - bes$dT / 3600)  # time of maximum, UT hours
}

# ---- Grid: Iceland to Morocco ----------------------------------------------------
# Wide enough to follow the path from Iceland to the Balearics and let the
# shading fade out toward the edges
crs_ib <- "+proj=laea +lat_0=50 +lon_0=-7 +datum=WGS84 +units=m +no_defs"
frame <- c(xmin = -1.25e6, xmax = 1.45e6, ymin = -1.75e6, ymax = 1.95e6)

cell <- 6000   # metres; ~280k cells, takes 2-3 minutes
grid <- expand.grid(x = seq(frame["xmin"], frame["xmax"], by = cell),
                    y = seq(frame["ymin"], frame["ymax"], by = cell))
ll <- sf_project(crs_ib, "EPSG:4326", as.matrix(grid))
grid <- bind_cols(grid, eclipse_max(ll[, 1], ll[, 2]))

# Sunlight left at maximum, for the fill. Totality sits below the ramp -> black.
floor_light <- 0.002
grid <- grid |>
  mutate(light = if_else(total, floor_light, pmax(1 - covered, floor_light * 1.5)))

# ---- Places ------------------------------------------------------------------
places <- tribble(
  ~name,        ~lon,    ~lat,
  "Reykjav\u00edk", -21.94,   64.15,
  "Dublin",     -6.26,   53.35,
  "London",     -0.13,   51.51,
  "Paris",       2.35,   48.86,
  "Madrid",     -3.70,   40.42,
  "Oviedo",     -5.84,   43.36,
  "Valencia",   -0.38,   39.47,
  "Palma",       2.65,   39.57,
  "Rabat",      -6.84,   34.02
)
comporta <- tibble(name = "Comporta", lon = -8.786, lat = 38.381)

locate <- function(df) {
  xy <- sf_project("EPSG:4326", crs_ib, cbind(df$lon, df$lat))
  bind_cols(df, x = xy[, 1], y = xy[, 2], eclipse_max(df$lon, df$lat))
}
places   <- locate(places) |> mutate(ink = if_else(1 - covered < 0.05 | total, "white", "black"))
comporta <- locate(comporta)
print(select(bind_rows(places, comporta), name, covered, total, ut))

hhmm <- function(h) sprintf("%02d:%02d", floor(h) %% 24, round((h %% 1) * 60))
com_lab <- sprintf("Comporta\n%.1f%% of the Sun covered\n%s WEST",
                   100 * comporta$covered, hhmm(comporta$ut + 1))

# ---- Land outlines -------------------------------------------------------------
if (!exists("get_world")) get_world <- function()
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- get_world() |> st_make_valid() |>
  st_crop(c(xmin = -40, ymin = 25, xmax = 30, ymax = 72)) |>
  st_transform(crs_ib)

# ---- Map -----------------------------------------------------------------------
bw <- list(paper = "#FFFFFF", ink = "#111111", muted = "#6B6B6B", coast = "#8A8A8A")

p <- ggplot() +
  geom_raster(data = grid, aes(x, y, fill = light)) +
  geom_sf(data = world, fill = NA, colour = "white", linewidth = 0.32) +
  geom_point(data = places, aes(x, y, colour = ink), size = 0.9) +
  # dark labels on the light side: white halo
  geom_text_repel(data = filter(places, ink == "black"),
                  aes(x, y, label = name, colour = ink),
                  family = ed_font, size = 2.7, seed = 17,
                  bg.colour = "white", bg.r = 0.12,
                  min.segment.length = Inf, box.padding = 0.25, point.padding = 0.2) +
  # white labels in totality: black halo
  geom_text_repel(data = filter(places, ink == "white"),
                  aes(x, y, label = name, colour = ink),
                  family = ed_font, size = 2.7, seed = 17,
                  bg.colour = "black", bg.r = 0.12,
                  min.segment.length = Inf, box.padding = 0.25, point.padding = 0.2) +
  scale_colour_identity() +
  # the splash of colour
  geom_point(data = comporta, aes(x, y), colour = "gold", size = 3.2) +
  geom_point(data = comporta, aes(x, y), colour = "gold", size = 7, alpha = 0.25) +
  ggfx::with_outer_glow(
    annotate("text", x = comporta$x - 70e3, y = comporta$y - 30e3, label = com_lab,
             hjust = 1, vjust = 1, family = ed_font, size = 3, lineheight = 0.95,
             colour = "goldenrod", fontface = "bold"),
    colour = "white", sigma = 4, expand = 3) +
  scale_fill_gradientn(
    colours = c("#000000", "#262626", "#5E5E5E", "#A8A8A8", "#E6E6E6", "#FFFFFF"),
    values  = rescale(log10(c(floor_light, floor_light * 1.5, 0.02, 0.1, 0.4, 1))),
    trans   = "log10", limits = c(floor_light, 1),
    breaks  = c(floor_light, 0.01, 0.05, 0.25, 1),
    labels  = c("Total", "99%", "95%", "75%", "0%"),
    na.value = "#FFFFFF",
    name    = "Share of the Sun covered at maximum eclipse",
    guide   = guide_colourbar(title.position = "top", barwidth = unit(16, "lines"),
                              barheight = unit(0.5, "lines"), frame.colour = bw$coast,
                              frame.linewidth = 0.2, ticks = FALSE)
  ) +
  coord_sf(crs = crs_ib, expand = FALSE, datum = NA,
           xlim = frame[c("xmin", "xmax")], ylim = frame[c("ymin", "ymax")]) +
  labs(
    title    = "Light and Dark",
    subtitle = "Total solar eclipse of 12 August 2026. Black is the path of totality, from Iceland to the\nBalearics; the fade shows how much sunlight was left everywhere else (log scale, roughly\nas the eye sees it). In the far south-east the Sun set before the eclipse peaked.",
    caption  = ed_caption(17, "Dark and Light",
                          "shadow computed from NASA Besselian elements (F. Espenak, GSFC), sea level, sun above horizon; Natural Earth")
  ) +
  theme_ed() +
  theme(
    plot.background  = element_rect(fill = bw$paper, colour = NA),
    panel.background = element_rect(fill = bw$paper, colour = NA),
    plot.title    = element_text(colour = bw$ink),
    plot.subtitle = element_text(colour = bw$muted),
    plot.caption  = element_text(colour = bw$muted),
    legend.title  = element_text(colour = bw$ink),
    legend.text   = element_text(colour = bw$muted),
    legend.justification = "left"
  )
p
dir.create("Maps", showWarnings = FALSE)
# White page edge to edge (ed_save would pad the fixed-aspect map with grey)
ggsave("Maps/17-delmelle.png", p, width = 8, height = 10.6, dpi = 300,
       bg = bw$paper, device = ragg::agg_png)
