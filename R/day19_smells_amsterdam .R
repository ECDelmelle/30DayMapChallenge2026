# day19_smells_amsterdam.R ----------------------------------------------------
# #30DayMapChallenge 2026 · Day 19: Smells
# What does Amsterdam smell like, according to people who stayed in an Airbnb?
# We search every Inside Airbnb review for a smell word (smell, odour, stink,
# geur, Geruch, odeur, olor, ...) and sort the words around it into five
# smells: bathrooms & drains, damp & mould, cigarette smoke, weed, and
# pleasant. Each panel shows the listings whose reviews mention that smell.
# Spoiler: the city smells far more of bathrooms than of weed.
#
# HOW TO RUN: put reviews.csv.gz and listings.csv.gz (Inside Airbnb,
# Amsterdam) in data/, set theme_path if needed, and source this file.
# Water comes from OpenStreetMap through osmdata and is cached in
# data/ams_water.rds after the first run.

# ---- Settings ------------------------------------------------------------------
if (!exists("theme_path")) theme_path <- "R/theme_ed.R"
day_num       <- 19
day_theme     <- "Smells"
reviews_file  <- "data/reviews.csv.gz"
listings_file <- "data/listings.csv.gz"
water_cache   <- "data/ams_water.rds"
hoods_file    <- "data/neighbourhoods.geojson"   # Inside Airbnb; set to NA to skip
window_chars  <- 40      # characters either side of a smell word that we read

source(theme_path)
library(data.table)
library(dplyr)
library(stringr)
library(sf)

# ---- Reviews -------------------------------------------------------------------
# IDs as text: Airbnb IDs are too long for R integers
rev <- fread(reviews_file, select = c("listing_id", "comments"), encoding = "UTF-8",
             colClasses = list(character = "listing_id"))
n_reviews <- nrow(rev)
rev[, txt := str_to_lower(str_replace_all(comments, "<br\\s*/?>", " "))]
rev[is.na(txt), txt := ""]

# Smell words in English, Dutch, German, French, Spanish and Italian.
smell_re <- paste0(
  "\\b(smell\\w*|odou?r\\w*|stink\\w*|stank|whiff|reek\\w*|",
  "\\bgeur\\w*|(?:rook|wiet|sigaretten|riool|schimmel|muf|brand)geur\\w*|stinkt|ruikt|ruiken|",
  "geruch|gerüche?|riecht|riechen|roch|muffig|",
  "odeur\\w*|pue\\b|puait|puanteur|",
  "olor\\w*|huele|olía|odore|puzza\\w*)"
)

sm <- rev[str_detect(txt, smell_re)]
message(format(nrow(sm), big.mark = ","), " of ", format(n_reviews, big.mark = ","),
        " reviews mention a smell")

# The text within window_chars of each smell word, glued together per review.
loc <- str_locate_all(sm$txt, smell_re)
sm[, near := mapply(function(t, l) {
  a <- pmax(1, l[, "start"] - window_chars)
  b <- pmin(nchar(t), l[, "end"] + window_chars)
  paste(substring(t, a, b), collapse = " ||| ")
}, txt, loc)]

# What the smell is of. A review can count for more than one smell.
cat_re <- list(
  sewer = paste0("sewer|sewage|drain\\w*|toilet\\w*|\\briool\\w*|afvoer|kanalisation|",
                 "abfluss|égout|canalisation|alcantarill\\w*|desag|fogna|scarico|",
                 "bathroom|badkamer|\\bwc\\b"),
  damp  = paste0("must(y|iness)|\\bmou?ld\\w*|mildew|damp\\w*|humid\\w*|moist\\w*|",
                 "schimmel\\w*|vocht\\w*|muffig|feucht\\w*|moisi|humidité|humedad|",
                 "moho|muffa|umid"),
  smoke = paste0("cigarett\\w*|tobacc\\w*|tabac\\w*|smok\\w*|zigarett\\w*|rauch\\w*|",
                 "sigaret\\w*|\\brook\\b|roken|fum(o|ée)|cigarr\\w*"),
  weed  = paste0("\\bweed\\b|cannabis|marijuana|marihuana|skunk|\\bhash\\w*|\\bwiet\\b|",
                 "\\bpot\\b|\\bjoints?\\b|\\bporro|\\bhasch\\w*|\\bgras\\b")
)
# Pleasant smells need the adjective right next to the smell word
# ("smelled so fresh", "lovely scent", "ruikt lekker"), otherwise
# "clean but smelled of mould" would count as pleasant.
adj <- "(?:fresh|clean|nice|lovely|great|good|wonderful|amazing|delicious|pleasant|divine|heavenly|beautiful)"
nice_re <- paste0(
  "\\b(?:smell\\w*|scent\\w*)\\s+(?:so\\s+|really\\s+|very\\s+|super\\s+|always\\s+)?", adj,
  "|\\b", adj, "\\s+(?:smell\\w*|scent\\w*|aroma\\w*)",
  "|smell\\w* (?:of|like) (?:fresh|bread|coffee|flowers|baking|cinnamon|lavender)",
  "|ruikt? (?:lekker|heerlijk|fris)|lekker ruik|riecht (?:gut|frisch)|angenehm\\w* geruch",
  "|sent(?:ait)? bon|bonne odeur|huele (?:bien|rico)|buen olor|profuma"
)

for (k in names(cat_re)) sm[, (k) := str_detect(near, cat_re[[k]])]
neg_re <- paste0(
  "(?:n[\u2019']t|\\bnot|\\bnever)\\s+(?:\\w+\\s+)?(?:smell\\w*|scent\\w*)\\s+(?:so\\s+|really\\s+|very\\s+|super\\s+)?", adj,
  "|\\b(?:no|not a|not very|not so)\\s+(?:\\w+\\s+)?", adj, "\\s+smell")
sm[, nice := str_detect(txt, nice_re) & !str_detect(txt, neg_re)]
sm[, any := TRUE]

cats <- c("any", "sewer", "damp", "smoke", "weed", "nice")
cat_lab <- c(any   = "Any smell at all",
             sewer = "Bathrooms & drains",
             damp  = "Damp & mould",
             smoke = "Cigarette smoke",
             weed  = "Weed",
             nice  = "Pleasant: fresh, clean, baking")

counts <- sm[, lapply(.SD, sum), .SDcols = cats]
print(counts)

# Reviews per listing per smell (long)
by_listing <- melt(sm[, c("listing_id", cats), with = FALSE], id.vars = "listing_id",
                   variable.name = "smell", value.name = "hit")[hit == TRUE,
                   .(n = .N), by = .(listing_id, smell)]

# ---- Listings --------------------------------------------------------------------
lst <- fread(listings_file, select = c("id", "latitude", "longitude",
                                       "neighbourhood_cleansed", "number_of_reviews"),
             colClasses = list(character = "id"))
lst_sf <- st_as_sf(lst, coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(28992)                                    # Dutch RD New, metres
xy <- st_coordinates(lst_sf)
lst$x <- xy[, 1]; lst$y <- xy[, 2]

pts <- merge(by_listing, lst[, .(listing_id = id, x, y)], by = "listing_id")
pts[, smell := factor(smell, cats)]
message(format(nrow(by_listing) - nrow(pts)), " listing-smell rows had no coordinates (delisted)")

# Frame: the dense part of the city (trims the outlying listings in each direction)
trim <- 0.04
pad  <- 300
frame <- c(xmin = quantile(lst$x, trim) - pad, xmax = quantile(lst$x, 1 - trim) + pad,
           ymin = quantile(lst$y, trim) - pad, ymax = quantile(lst$y, 1 - trim) + pad)
frame <- setNames(as.numeric(frame), c("xmin", "xmax", "ymin", "ymax"))
frame_sf <- st_as_sfc(st_bbox(frame, crs = st_crs(28992)))

# ---- Water (OSM) -------------------------------------------------------------------
# Canals, the IJ and the Amstel are what make this read as Amsterdam.
# The main Overpass server often times out (HTTP 504), so try mirrors in turn.
overpass_urls <- c("https://overpass-api.de/api/interpreter",
                   "https://overpass.kumi.systems/api/interpreter",
                   "https://overpass.private.coffee/api/interpreter",
                   "https://maps.mail.ru/osm/tools/overpass/api/interpreter")

get_water <- function() {
  if (file.exists(water_cache)) return(readRDS(water_cache))
  bb <- st_bbox(st_transform(st_buffer(frame_sf, 500), 4326))
  for (u in overpass_urls) {
    message("Asking ", u, " for water...")
    osmdata::set_overpass_url(u)
    w <- tryCatch({
      q <- osmdata::opq(bb, timeout = 180) |>
        osmdata::add_osm_feature("natural", "water") |>
        osmdata::osmdata_sf()
      bind_rows(select(q$osm_polygons, geometry),
                select(q$osm_multipolygons, geometry)) |>
        st_make_valid() |>
        st_transform(28992) |>
        st_intersection(st_buffer(frame_sf, 500)) |>
        st_union()
    }, error = function(e) { message("  failed: ", conditionMessage(e)); NULL })
    if (!is.null(w) && length(w) > 0) {
      dir.create(dirname(water_cache), showWarnings = FALSE)
      saveRDS(w, water_cache)
      message("  got it; cached in ", water_cache)
      return(w)
    }
  }
  message("No water from any Overpass server; drawing the map without it. Re-run later.")
  NULL
}
water <- get_water()

# ---- Neighbourhoods (Inside Airbnb) ------------------------------------------------
hoods <- NULL
if (!is.na(hoods_file) && file.exists(hoods_file)) {
  hoods <- st_read(hoods_file, quiet = TRUE) |>
    st_make_valid() |>
    st_transform(28992)
  # A few names in the first panel only, so readers can find their way
  short <- c("Centrum-West" = "Centrum-West", "Centrum-Oost" = "Centrum-Oost",
             "De Pijp - Rivierenbuurt" = "De Pijp", "De Baarsjes - Oud-West" = "Oud-West",
             "Oud-Oost" = "Oost", "Oud-Noord" = "Noord", "Westerpark" = "Westerpark",
             "Zuid" = "Zuid")
  hood_lab <- hoods |>
    filter(neighbourhood %in% names(short)) |>
    st_point_on_surface() |>
    mutate(label = short[neighbourhood],
           x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],
           smell = factor("any", cats)) |>
    st_drop_geometry()
}

# ---- Panel labels ------------------------------------------------------------------
per_k <- function(n) sprintf("%.1f", 1000 * n / n_reviews)
lab <- data.table(smell = factor(cats, cats),
                  n = unlist(counts[, ..cats]),
                  listings = sapply(cats, function(k) uniqueN(by_listing[smell == k, listing_id])))
lab[, big  := format(n, big.mark = ",", trim = TRUE)]
lab[, small := sprintf("reviews · %s listings · %s per 1,000 reviews",
                       format(listings, big.mark = ",", trim = TRUE), per_k(n))]
lab[, x := frame["xmin"] + 0.02 * (frame["xmax"] - frame["xmin"])]
lab[, y := frame["ymax"] - 0.02 * (frame["ymax"] - frame["ymin"])]
lab[, y2 := y - 0.13 * (frame["ymax"] - frame["ymin"])]

strip_lab <- setNames(cat_lab, cats)
pt_col <- c(any = ed$ink, sewer = ed$pop, damp = ed$pop, smoke = ed$pop,
            weed = ed$pop, nice = ed$alt)
txt_col <- c(any = ed$ink, sewer = ed$pop_dk, damp = ed$pop_dk, smoke = ed$pop_dk,
             weed = ed$pop_dk, nice = ed$alt_dk)

all_listings <- lst[x >= frame["xmin"] & x <= frame["xmax"] & y >= frame["ymin"] & y <= frame["ymax"]]
ratio <- round(lab[smell == "sewer", n] / lab[smell == "weed", n])

# ---- Map -----------------------------------------------------------------------------
p <- ggplot() +
  { if (!is.null(hoods)) geom_sf(data = hoods, fill = ed$land, colour = ed$line_nbr, linewidth = 0.25) } +
  { if (!is.null(water)) geom_sf(data = water, fill = ed$water, colour = NA) } +
  geom_point(data = all_listings, aes(x, y), colour = "#A8A8A8", size = 0.35,
             stroke = 0, alpha = 0.55) +
  geom_point(data = pts[order(n)], aes(x, y, colour = smell, size = n),
             alpha = 0.75, stroke = 0) +
  { if (!is.null(hoods)) geom_label(data = hood_lab, aes(x, y, label = label), colour = ed$ink,
                                    fill = scales::alpha(ed$land, 0.8), label.size = 0,
                                    label.padding = unit(0.08, "lines"),
                                    family = ed_font, fontface = "bold", size = 2.2) } +
  geom_text(data = lab, aes(x, y, label = big, colour = smell), hjust = 0, vjust = 1,
            family = ed_font, fontface = "bold", size = 6.5) +
  geom_text(data = lab, aes(x, y2, label = small), colour = ed$muted, hjust = 0, vjust = 1,
            family = ed_font, size = 2.4) +
  scale_colour_manual(values = pt_col, guide = "none") +
  scale_size_area(max_size = 4.5, breaks = c(1, 3, 6), name = "Reviews mentioning the smell") +
  facet_wrap(~smell, ncol = 2, labeller = as_labeller(strip_lab)) +
  coord_sf(crs = 28992, datum = NA, expand = FALSE,
           xlim = frame[c("xmin", "xmax")], ylim = frame[c("ymin", "ymax")]) +
  labs(
    title    = "Amsterdam Doesn't Smell of Weed",
    subtitle = paste0(
      "Of ", format(n_reviews, big.mark = ","), " Airbnb reviews, ",
      format(nrow(sm), big.mark = ","), " mention a smell. Guests complain\n",
      "about the bathroom ", ratio, " times as often as about weed, and mention mould and cigarette\n",
      "smoke about four times as often. Each dot is a listing whose reviews name that smell; grey\n",
      "dots are all current listings. Reviews read in English, Dutch, German, French, Spanish, Italian."),
    caption = ed_caption(day_num, day_theme,
      "Inside Airbnb, Amsterdam (reviews 2010–2026, listings and neighbourhoods Sept 2026); water © OpenStreetMap\ncontributors. Smells classified from words within 40 characters of a smell word (smell, odour, geur, Geruch, odeur, olor, ...).")
  ) +
  theme_ed() +
  theme(
    strip.text = element_text(family = ed_font, colour = ed$ink, size = 9.5, hjust = 0,
                              face = "bold", margin = margin(t = 8, b = 3)),
    panel.background = element_rect(fill = if (is.null(hoods)) ed$land else ed$land_nbr, colour = NA),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "top", legend.justification = "left",
    legend.margin = margin(b = 6),
    legend.key = element_rect(fill = NA, colour = NA)
  ) +
  guides(size = guide_legend(title.position = "left",
                             override.aes = list(colour = ed$muted, alpha = 1)))

ed_save(p, day_num, "smells_amsterdam", width = 8.6, height = 11.5)

# A few example snippets per smell, for the post text
set.seed(19)
for (k in cats[-1]) {
  cat("\n==", cat_lab[k], "\n")
  ex <- sm[get(k) == TRUE][sample(.N, min(.N, 4))]
  cat(paste0("  - ", str_trunc(str_squish(ex$near), 160)), sep = "\n")
}
