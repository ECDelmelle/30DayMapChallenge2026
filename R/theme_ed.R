# theme_ed.R ---------------------------------------------------------------
# Shared look for #30DayMapChallenge 2026: light grey ground, magenta pop.
# source("R/theme_ed.R") at the top of every day's script.

library(ggplot2)

# ---- Palette ---------------------------------------------------------------
ed <- list(
  bg       = "#E9E9E9",  # page / background
  land     = "#F7F7F7",  # focus land (figure)
  land_nbr = "#E1E1E1",  # surrounding land (ground) - recedes behind focus
  line     = "#BDBDBD",  # focus boundary
  line_nbr = "#CFCFCF",  # neighbouring boundaries - very light
  water    = "#B9D3E6",  # muted blue that sits quietly with grey + magenta
  water_ink= "#6F93B0",  # water labels
  ink      = "#262626",  # titles
  muted    = "#6E6E6E",  # subtitles, captions
  faint    = "#9E9E9E",  # place labels on the ground
  pop      = "#D4157A",  # the magenta
  pop_lt   = "#F2A3C9",  # light magenta (ramps / halos)
  pop_dk   = "#8E0C51",  # dark magenta (annotations)
  alt      = "#1E9C6B",  # green: a second, contrasting highlight
  alt_dk   = "#137A52"   # darker green for text on grey
)

# Sequential grey -> magenta ramp, for days that need a continuous scale
ed_ramp <- grDevices::colorRampPalette(c(ed$land, ed$pop_lt, ed$pop, ed$pop_dk))

# ---- Fonts -----------------------------------------------------------------
# Fonts are rendered with ragg + systemfonts (no showtext: showtext's dpi
# setting breaks ggtext spacing and the RStudio viewer).
# In RStudio set Tools > Global Options > General > Graphics > Backend: AGG
# so the Plots pane matches the saved PNG.
# Inter is used if installed; otherwise systemfonts fetches it from Google
# Fonts; otherwise we fall back to the system sans.
ed_font <- local({
  have <- function() "Inter" %in% systemfonts::system_fonts()$family ||
    "Inter" %in% systemfonts::registry_fonts()$family
  if (!have()) try(systemfonts::require_font("Inter"), silent = TRUE)
  if (have()) "Inter" else { message("Inter not found, using sans."); "sans" }
})

# ---- Theme -----------------------------------------------------------------
theme_ed <- function(base_size = 11, base_family = ed_font) {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = ed$bg, colour = NA),
      panel.background = element_rect(fill = ed$bg, colour = NA),
      plot.title    = element_text(face = "bold", size = rel(1.9), colour = ed$ink,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), colour = ed$muted,
                                   lineheight = 1.25, margin = margin(b = 12)),
      plot.caption  = element_text(size = rel(0.65), colour = ed$muted,
                                   hjust = 0, lineheight = 1.3,
                                   margin = margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(colour = ed$ink, size = rel(0.8)),
      legend.text  = element_text(colour = ed$muted, size = rel(0.75)),
      plot.margin  = margin(20, 22, 14, 22)
    )
}

# ---- Helpers ---------------------------------------------------------------
# Two-line caption so it never runs off the page.
ed_caption <- function(day, theme, source) {
  sprintf("#30DayMapChallenge 2026 · Day %d: %s · Elizabeth Delmelle\nData: %s",
          day, theme, source)
}

# Muted scale bar (needs ggspatial)
ed_scalebar <- function(location = "br") {
  ggspatial::annotation_scale(
    location = location, unit_category = "imperial", style = "ticks",
    line_col = ed$muted, text_col = ed$muted, text_family = ed_font,
    text_cex = 0.6, line_width = 0.4, width_hint = 0.18,
    pad_x = grid::unit(0.3, "cm"), pad_y = grid::unit(0.3, "cm")
  )
}

ed_save <- function(plot, day, name, width = 8, height = 9) {
  dir.create("Maps", showWarnings = FALSE)
  file <- sprintf("Maps/day%02d_%s.png", day, name)
  ggsave(file, plot, width = width, height = height, dpi = 300, bg = ed$bg,
         device = ragg::agg_png)
  message("Saved ", file)
  invisible(file)
}