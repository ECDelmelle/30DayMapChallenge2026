# Day 4 — Clusters ----------------------------------------------------------
# Where did Josh Shapiro gain the most between his 2022 and 2026 governor
# races? Change in his margin by Pennsylvania county: arrows show each
# county's shift, shading shows statistically significant clusters (local
# Moran's I / LISA). Two newsroom-inspired looks: style <- "nyt" or "ft".
#
# Data
#   Data/pa2026_gov_county.csv  PA Dept. of State county breakdown (unofficial)
#   Data/pa2022_gov_county.csv  PA Dept. of State county breakdown (certified)
#   Counties: US Census Bureau via tigris
# Montour County reported no mail ballots in the 2026 file, so it is marked
# and left out of the cluster test.
#
# Fonts (free, Google Fonts): Libre Franklin; Playfair Display for the FT look.
# Install them, or put static .ttf files in Data/fonts (see reg() below).

library(tidyverse)
library(sf)
library(spdep)
library(patchwork)
library(ggrepel)
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

if (!exists("style")) style <- "nyt"        # "nyt" or "ft"

# ---- Fonts -----------------------------------------------------------------------
reg <- function(family, plain, bold = plain) {
  if (family %in% systemfonts::system_fonts()$family) return(invisible())
  f <- file.path("Data/fonts", c(plain, bold))
  if (all(file.exists(f))) systemfonts::register_font(family, plain = f[1], bold = f[2])
}
reg("Libre Franklin", "LibreFranklin-400.ttf", "LibreFranklin-800.ttf")
reg("Playfair Display", "Playfair-400.ttf", "Playfair-700.ttf")
f_sans <- "Libre Franklin"

# ---- Look ------------------------------------------------------------------------
looks <- list(
  nyt = list(bg = "#FFFFFF", ink = "#121212", grey = "#727272", rule = "#DCDCDC",
             dem = "#2C6DB4", rep = "#C9372C", title_font = f_sans,
             fills = c("High-High" = "#A8C8E8", "High-Low" = "#DDE9F5",
                       "Low-High" = "#F3EBDA", "Low-Low" = "#DCCBA6",
                       "Not significant" = "#EFEFEF", "Incomplete count" = "#FFFFFF"),
             border = "#FFFFFF"),
  ft  = list(bg = "#FFF1E5", ink = "#33302E", grey = "#66605C", rule = "#CCC1B7",
             dem = "#0F5499", rep = "#990F3D", title_font = "Playfair Display",
             fills = c("High-High" = "#9DBBD9", "High-Low" = "#D4DEE6",
                       "Low-High" = "#F2D9C4", "Low-Low" = "#E3B796",
                       "Not significant" = "#F2DFCE", "Incomplete count" = "#FFF1E5"),
             border = "#FFF1E5")
)
L <- looks[[style]]

cl_labs <- c("High-High"        = "Cluster of big gains",
             "Low-Low"          = "Cluster of small gains",
             "High-Low"         = "Big gain, small-gain neighbors",
             "Low-High"         = "Small gain, big-gain neighbors",
             "Not significant"  = "No significant cluster")

# ---- Results -----------------------------------------------------------------------
read_dos <- function(path, yr) {
  read_csv(path, col_types = cols(.default = "c")) |>
    transmute(county = toupper(`County Name`), party = `Party Name`,
              v = parse_number(Votes)) |>
    group_by(county) |>
    summarise("d{yr}" := sum(v[party == "Democratic"]),
              "r{yr}" := sum(v[party == "Republican"]),
              "t{yr}" := sum(v))
}
c26 <- read_dos("Data/2026PAresults.csv", 26)
c22 <- read_dos("Data/2022PAelection.csv", 22)
d <- inner_join(c22, c26, by = "county") |>
  mutate(m22 = (d22 - r22) / t22 * 100, m26 = (d26 - r26) / t26 * 100,
         shift = m26 - m22)
state_m22 <- with(d, (sum(d22) - sum(r22)) / sum(t22) * 100)
state_m26 <- with(d, (sum(d26) - sum(r26)) / sum(t26) * 100)

# ---- Counties ------------------------------------------------------------------------
if (!exists("get_counties")) get_counties <- function()
  tigris::counties("PA", cb = TRUE, year = 2024) |> mutate(county = toupper(NAME))
pa <- get_counties() |> st_transform(6565) |> left_join(d, by = "county")
stopifnot(nrow(pa) == 67, !anyNA(pa$shift))
pa$incomplete <- pa$county == "MONTOUR"

# ---- LISA --------------------------------------------------------------------------
an <- filter(pa, !incomplete)
lw <- nb2listw(poly2nb(an, queen = TRUE), style = "W")
set.seed(2026)
gm <- moran.mc(an$shift, lw, nsim = 9999)
lm <- localmoran_perm(an$shift, lw, nsim = 9999)
z  <- as.numeric(scale(an$shift)); lag <- lag.listw(lw, z)
p  <- lm[, "Pr(z != E(Ii)) Sim"]
an$cluster <- case_when(p >= 0.05 ~ "Not significant",
                        z > 0 & lag > 0 ~ "High-High", z < 0 & lag < 0 ~ "Low-Low",
                        z > 0 & lag < 0 ~ "High-Low", TRUE ~ "Low-High")
pa <- left_join(pa, st_drop_geometry(an)[, c("county", "cluster")], by = "county") |>
  mutate(cluster = factor(if_else(incomplete, "Incomplete count", cluster),
                          names(L$fills)))
n_up <- sum(an$shift > 0)
hh <- filter(an, cluster == "High-High"); ll <- filter(an, cluster == "Low-Low")

# ---- Shift arrows (NYT-style): length ∝ shift, up-left = more Democratic -----------
k   <- 3200                     # metres of arrow per point of shift
ang <- 40 * pi / 180
ctr <- st_coordinates(st_point_on_surface(st_geometry(pa)))
arrows <- pa |> st_drop_geometry() |>
  mutate(x = ctr[, 1], y = ctr[, 2],
         xend = x - sign(shift) * abs(shift) * k * cos(ang),
         yend = y + abs(shift) * k * sin(ang),
         col = if_else(shift >= 0, L$dem, L$rep)) |>
  filter(!incomplete)

# ---- Frame + callouts -----------------------------------------------------------------
bb <- st_bbox(pa)
xr <- c(bb["xmin"] - 15000, bb["xmax"] + 150000)
yr <- c(bb["ymin"] - 10000, bb["ymax"] + 70000)

pt <- function(cty) { g <- st_point_on_surface(st_geometry(filter(pa, county == cty)))
                      st_coordinates(g)[1, ] }
ne_to <- pt("MONROE"); nw_to <- pt("WARREN"); mo <- pt("MONTOUR")

ne_txt <- sprintf("Poconos and Scranton area\nA significant cluster of big gains:\nmargins grew %.0f to %.0f points",
                  min(hh$shift), max(hh$shift))
nw_txt <- sprintf("Northwest\nA cluster of small gains:\nmargins grew only %.0f to %.0f points",
                  min(ll$shift), max(ll$shift))
ne_xy <- c(bb["xmax"] + 18000, ne_to[2] - 30000)
nw_xy <- c(nw_to[1] + 60000, bb["ymax"] + 55000)

# ---- Map ---------------------------------------------------------------------------
p_map <- ggplot() +
  geom_sf(data = pa, aes(fill = cluster), colour = L$border, linewidth = 0.4) +
  geom_sf(data = filter(pa, incomplete), fill = NA, colour = L$grey,
          linewidth = 0.3, linetype = "22") +
  geom_segment(data = arrows, aes(x, y, xend = xend, yend = yend, colour = col),
               linewidth = 0.55, lineend = "round", linejoin = "mitre",
               arrow = arrow(length = unit(1.3, "mm"), type = "closed", angle = 28)) +
  scale_colour_identity() +
  scale_fill_manual(values = L$fills, guide = "none") +
  # callouts
  annotate("segment", x = ne_xy[1] - 4000, y = ne_xy[2] + 10000,
           xend = ne_to[1] + 18000, yend = ne_to[2], colour = L$ink, linewidth = 0.25) +
  annotate("text", x = ne_xy[1], y = ne_xy[2], label = ne_txt, hjust = 0, vjust = 1,
           family = f_sans, size = 3.1, colour = L$ink, lineheight = 1.05) +
  annotate("segment", x = nw_xy[1] - 3000, y = nw_xy[2] - 22000,
           xend = nw_to[1] + 5000, yend = nw_to[2] + 15000, colour = L$ink, linewidth = 0.25) +
  annotate("text", x = nw_xy[1], y = nw_xy[2], label = nw_txt, hjust = 0, vjust = 1,
           family = f_sans, size = 3.1, colour = L$ink, lineheight = 1.05) +
  annotate("text", x = mo[1] + 16000, y = mo[2] - 9000, hjust = 0, vjust = 1,
           label = "Montour: mail ballots\nnot yet reported", family = f_sans,
           size = 2.2, colour = L$grey, lineheight = 0.95) +
  coord_sf(xlim = xr, ylim = yr, expand = FALSE, datum = NA, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = L$bg, colour = NA))

# ---- Key strip (shading + arrow) ---------------------------------------------------------
keys <- tibble(cluster = names(cl_labs), x = c(0, 1.55, 3.1, 5.05, 7.1))
p_key <- ggplot(keys) +
  geom_tile(aes(x, 0, fill = cluster), width = 0.16, height = 0.55,
            colour = if (style == "nyt") "#C8C8C8" else L$rule, linewidth = 0.2) +
  geom_text(aes(x + 0.13, 0, label = cl_labs[cluster]), hjust = 0, family = f_sans,
            size = 2.85, colour = L$ink) +
  annotate("segment", x = 9.15, xend = 8.85, y = -0.18, yend = 0.18, colour = L$dem,
           linewidth = 0.6, arrow = arrow(length = unit(1.3, "mm"), type = "closed", angle = 28)) +
  annotate("text", x = 9.3, y = 0, hjust = 0, family = f_sans, size = 2.85, colour = L$ink,
           label = "Shift toward Shapiro") +
  annotate("segment", x = 10.95, xend = 11.25, y = -0.18, yend = 0.18, colour = L$rep,
           linewidth = 0.6, arrow = arrow(length = unit(1.3, "mm"), type = "closed", angle = 28)) +
  annotate("text", x = 11.4, y = 0, hjust = 0, family = f_sans, size = 2.85, colour = L$ink,
           label = "Toward Republican") +
  scale_fill_manual(values = L$fills, guide = "none") +
  scale_x_continuous(limits = c(-0.1, 13.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.45, 0.45), expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = L$bg, colour = NA))

# ---- Assemble -----------------------------------------------------------------------------
title <- "Shapiro Gained Almost Everywhere. His Biggest Gains Clustered in the Poconos."
subtitle <- sprintf(paste0(
  "Josh Shapiro widened his margin in %d of %d fully counted counties, from %.1f points ",
  "statewide in 2022 to %.1f points in Tuesday's unofficial count.\n",
  "Arrows show each county's shift; shading marks statistically significant clusters ",
  "of big and small gains (local Moran's I, p < 0.05)."),
  n_up, nrow(an), state_m22, state_m26)
caption <- sprintf(paste0(
  "Note: Margin = (Shapiro − Republican) ÷ all votes for governor. 2026 results are unofficial. ",
  "Global Moran's I = %.2f (p < 0.001); queen contiguity, 9,999 permutations.\n",
  "Source: Pennsylvania Department of State; U.S. Census Bureau  |  ",
  "#30DayMapChallenge Day 4: Clusters  |  Elizabeth Delmelle"), gm$statistic)

page <- p_key / p_map +
  plot_layout(heights = c(0.06, 1)) +
  plot_annotation(
    title = title, subtitle = subtitle, caption = caption,
    theme = theme(
      plot.background = element_rect(fill = L$bg, colour = NA),
      plot.title = element_text(family = L$title_font, face = "bold",
                                size = if (style == "ft") 21 else 19, colour = L$ink,
                                margin = margin(b = 6)),
      plot.subtitle = element_text(family = f_sans, size = 10.5, colour = L$grey,
                                   lineheight = 1.25, margin = margin(b = 10)),
      plot.caption = element_text(family = f_sans, size = 7, colour = L$grey,
                                  hjust = 0, lineheight = 1.3, margin = margin(t = 8)),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = margin(26, 30, 16, 30)))

if (style == "ft")   # FT charts carry a short black bar above the title
  page <- page & theme(plot.background = element_rect(fill = L$bg, colour = NA))

dir.create("Maps", showWarnings = FALSE)
out <- sprintf("Maps/day04_clusters_shapiro_lisa_%s.png", style)
ggsave(out, page, width = 11, height = 8.6, dpi = 300, bg = L$bg, device = ragg::agg_png)
message("Saved ", out)
