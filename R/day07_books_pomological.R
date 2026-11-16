# Day 7 — 10-minute map ----------------------------------------------------
# Books I've finished in 2026 (so far), mapped by where they're set,
# in the style of ggpomological (Garrick Aden-Buie), after the USDA
# Pomological Watercolor plates: paper texture, fruit-wash colours, handwriting.
# Each label: the title in bold, my one-line review in italic underneath.
#
# The palette and paper textures come from ggpomological:
#   remotes::install_github("gadenbuie/ggpomological")
# If it isn't installed, the textures are downloaded from GitHub instead.
# Rendering: ragg at 300 dpi onto a transparent background, then magick lays
# it on the watercolour paper (sharper than paint_pomological()'s screen-res
# device, and it keeps systemfonts fonts working).

library(tidyverse)
library(sf)
library(ggrepel)
library(magick)
sf_use_s2(FALSE)

# ---- Fonts (free, Google Fonts) -----------------------------------------------
get_font <- function(family, fallback) {
  have <- function() family %in% c(systemfonts::system_fonts()$family,
                                   systemfonts::registry_fonts()$family)
  if (!have()) try(systemfonts::require_font(family), silent = TRUE)
  if (have()) family else { message(family, " not found, using ", fallback); fallback }
}
f_hand  <- get_font("Homemade Apple", "serif")   # ggpomological's "fancy" font
f_print <- get_font("EB Garamond", "serif")      # old-style serif with real bold + italic

# ---- Pomological palette (from ggpomological) -------------------------------
pomo <- list(
  red = "#c03728", green = "#919c4c", orange = "#fd8f24", yellow = "#f5c04a",
  pink = "#e68c7c", grey = "#828585", green_lt = "#c3c377", slate = "#4f5157",
  brown = "#6f5438",
  paper = "#fffeea", paper_alt = "#f8eed1", light_line = "#efe1c6",
  medium_line = "#a89985", darker_line = "#6b452b", black = "#3a3e3f",
  dark_blue = "#2b323f"
)

# ---- Books ------------------------------------------------------------------
# Setting = the book's main or opening place (many move around; change freely)
books <- tribble(
  ~title,                              ~author,                ~kind,        ~lat,   ~lon,    ~opinion,
  "Kin",                               "Tayari Jones",         "Novels",     31.0,   -92.3,   "Excellent",
  "Whistler",                          "Ann Patchett",         "Novels",     40.779, -73.963, "Very good",
  "Murder on the Orient Express",      "Agatha Christie",      "Mystery",    45.25,  18.6,    "Fun",
  "Tom Lake",                          "Ann Patchett",         "Novels",     44.76,  -85.62,  "Meh",
  "The Rachel Incident",               "Caroline O'Donoghue",  "Novels",     51.90,  -8.47,   "Good summer read",
  "London Falling",                    "Patrick Radden Keefe", "Nonfiction", 51.49,  -0.13,   "Excellent",
  "A Fever in the Heartland",          "Timothy Egan",         "Nonfiction", 39.77,  -86.16,  "Excellent",
  "Heart the Lover",                   "Lily King",            "Novels",     42.37,  -72.52,  "So Short",
  "The Loneliness of Sonia and Sunny", "Kiran Desai",          "Novels",     25.44,  81.85,   "So Long",
  "Strangers",                         "Belle Burden",         "Nonfiction", 41.39,  -70.64,  "Audiobook, part credit",
  "The Sisters",                       "Jonas Hassen Khemiri", "Novels",     59.33,  18.07,   "Enjoyed",
  "North Woods",                       "Daniel Mason",         "Novels",     42.45,  -73.25,  "Very good",
  "The Correspondent",                 "Virginia Evans",       "Novels",     39.03,  -76.50,  "I recommend it to everyone",
  "The Idiot",                         "Elif Batuman",         "Novels",     47.2,   19.5,    "Hilarious"   # Selin's summer in Hungary
)

# Labels are plotmath so the title can be bold and the review italic:
# atop() stacks lines; long reviews wrap onto a second italic line.
q <- function(x) paste0('"', str_replace_all(x, '"', '\\\\"'), '"')
make_label <- function(title, opinion, width = 30) {
  t <- paste0("bold(", q(title), ")")
  if (opinion == "") return(t)
  r <- str_split(str_wrap(opinion, width), "\n")[[1]]
  if (length(r) > 2) r <- c(r[1], paste(r[-1], collapse = " "))   # max two lines
  r <- paste0("italic(", q(r), ")")
  rev_expr <- if (length(r) == 1) r else paste0("displaystyle(atop(", r[1], ", ", r[2], "))")  # keep full size
  paste0("atop(", t, ", ", rev_expr, ")")
}
books <- books |>
  mutate(kind  = factor(kind, c("Novels", "Mystery", "Nonfiction")),
         label = map2_chr(title, opinion, make_label))

kind_cols <- c(Novels = pomo$red, Mystery = pomo$orange, Nonfiction = pomo$green)

# ---- Geometry -----------------------------------------------------------------
robinson <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

if (!exists("get_world")) get_world <- function()
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

world <- get_world() |> st_make_valid() |> st_transform(robinson)

pts <- books |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(robinson)
xy  <- st_coordinates(pts)
# Whistler (NYC) and Strangers (Martha's Vineyard) can't fit by their dots on the
# crowded East Coast, so they get hand-placed labels out in the open Atlantic
# with curved leader lines. The line is drawn before the other labels, whose paper-coloured halos
# mask it wherever it passes behind them.
# Label anchors are lon/lat of the label's left edge; tweak freely.
leader <- tribble(
  ~title,       ~lab_lon, ~lab_lat,
  "Strangers",  -48,      35,     # Strangers out east, Whistler down south-east:
  "Whistler",   -56,      27      # the two lines diverge and never cross
)
books_xy <- books |>
  mutate(x = xy[, 1], y = xy[, 2]) |>
  left_join(leader, by = "title")
lab_xy <- books_xy |>
  filter(!is.na(lab_lon)) |>
  st_as_sf(coords = c("lab_lon", "lab_lat"), crs = 4326) |>
  st_transform(robinson)
books_lead <- books_xy |>
  filter(!is.na(lab_lon)) |>
  mutate(lx = st_coordinates(lab_xy)[, 1], ly = st_coordinates(lab_xy)[, 2])
books_rest <- filter(books_xy, is.na(lab_lon))

has_book  <- lengths(st_intersects(world, pts)) > 0
world_in  <- world[has_book, ]
world_out <- world[!has_book, ]

xr <- range(xy[, 1]) + c(-2.6e6, 1.6e6)
yr <- range(xy[, 2]) + c(-1.6e6, 1.7e6)

# ---- Map (transparent background; the paper goes on afterwards) ----------------
p <- ggplot() +
  geom_sf(data = world_out, fill = pomo$paper_alt, colour = pomo$medium_line,
          linewidth = 0.15, alpha = 0.7) +
  geom_sf(data = world_in, fill = pomo$green_lt, colour = pomo$medium_line,
          linewidth = 0.2, alpha = 0.35) +
  # watercolour daubs: a big pale wash, a smaller one, then the "seed"
  geom_point(data = books_xy, aes(x, y, colour = kind), size = 7, alpha = 0.18, stroke = 0) +
  geom_point(data = books_xy, aes(x, y, colour = kind), size = 3.6, alpha = 0.35, stroke = 0) +
  geom_point(data = books_xy, aes(x, y, colour = kind), size = 1.4, alpha = 0.95, stroke = 0) +
  # leader lines for the two hand-placed labels
  geom_curve(data = books_lead, aes(x, y, xend = lx - 0.12e6, yend = ly),
             curvature = 0.2, colour = pomo$darker_line, linewidth = 0.3, alpha = 0.7) +
  # everyone else: label by its dot, no lines
  geom_text_repel(
    data = books_rest, aes(x, y, label = label),
    parse = TRUE, family = f_print, size = 3.2, colour = pomo$dark_blue,
    min.segment.length = Inf,
    bg.colour = pomo$paper, bg.r = 0.18,   # paper halo keeps text readable over lines
    box.padding = 0.25, point.padding = 0.35,
    force = 2, force_pull = 2, max.overlaps = Inf, seed = 2026
  ) +
  # the two hand-placed labels, out in open water
  geom_text(data = books_lead, aes(lx, ly, label = label), parse = TRUE,
            family = f_print, size = 3.2, colour = pomo$dark_blue, hjust = 0) +
  scale_colour_manual(values = kind_cols, name = NULL,
                      guide = guide_legend(override.aes = list(size = 4, alpha = 0.8))) +
  coord_sf(crs = robinson, xlim = xr, ylim = yr, expand = FALSE, datum = NA) +
  labs(
    title    = "Books Finished in 2026 (So Far!)",
    subtitle = expression(bold("Title") ~ "over" ~ italic("my succinct review") ~
                            "\u00b7 each book placed more or less where it is set"),
    caption  = paste0("#30DayMapChallenge 2026 · Day 7: 10-minute map (this took way more than 10 minutes. I got carried away on a Saturday morning) · Elizabeth Delmelle\n",
                      "Data: my goodreads app; Natural Earth. Style after ggpomological ",
                      "(Garrick Aden-Buie) and the USDA Pomological Watercolor Collection."),
    tag = "No. 7"
  ) +
  theme_void(base_family = f_print) +
  theme(
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border     = element_rect(fill = NA, colour = pomo$light_line, linewidth = 0.8),
    plot.title    = element_text(family = f_hand, size = 26, colour = pomo$dark_blue,
                                 margin = margin(t = 6, b = 10)),
    plot.subtitle = element_text(family = f_print, size = 12.5,
                                 colour = pomo$darker_line, margin = margin(b = 8)),
    plot.caption  = element_text(family = f_print, size = 8, colour = pomo$medium_line,
                                 hjust = 0, lineheight = 1.2, margin = margin(t = 10)),
    plot.tag      = element_text(family = f_hand, size = 13, colour = pomo$red),
    plot.tag.position = c(0.95, 0.94),
    plot.title.position = "plot", plot.caption.position = "plot",
    legend.position = "bottom", legend.justification = "left",
    legend.direction = "horizontal", legend.margin = margin(4, 0, 0, 0),
    legend.text = element_text(family = f_print, face = "italic", size = 10,
                               colour = pomo$dark_blue),
    legend.key.height = unit(0.9, "lines"), legend.key.width = unit(1.1, "lines"),
    legend.spacing.x = unit(0.2, "lines"),
    plot.margin = margin(26, 34, 18, 34)
  )

# ---- Paint it onto watercolour paper ------------------------------------------
pomo_texture <- function(which) {
  file <- c(background = "pomological_background.png",
            overlay    = "pomological_overlay.jpg")[[which]]
  installed <- system.file("images", file, package = "ggpomological")
  if (nzchar(installed)) return(installed)
  local <- file.path("Data", file)
  if (!file.exists(local)) {
    dir.create("Data", showWarnings = FALSE)
    download.file(paste0("https://raw.githubusercontent.com/gadenbuie/ggpomological/",
                         "master/inst/images/", file), local, mode = "wb")
  }
  local
}

w_in <- 11; h_in <- 6.2; dpi <- 300
fig_file <- tempfile(fileext = ".png")
ggsave(fig_file, p, width = w_in, height = h_in, dpi = dpi, bg = "transparent",
       device = ragg::agg_png)

size <- paste0(w_in * dpi, "x", h_in * dpi, "!")
fig  <- image_read(fig_file)
over <- image_read(pomo_texture("overlay")) |> image_resize(size)
fig  <- image_composite(fig, over, operator = "blend", compose_args = "15")
paper <- image_read(pomo_texture("background")) |> image_resize(size)
out  <- image_composite(paper, fig)

dir.create("Maps", showWarnings = FALSE)
image_write(out, "Maps/07-delmelle", format = "png")
message("Saved Maps/day07_ten_minute_books_pomological.png")