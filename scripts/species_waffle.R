# species_waffle.R -- "Who lives in the Star Wars galaxy?"
# Pictogram of the Star Wars cast: one dot per character, grouped by species, with
# direct labels, a big callout number for the share of humans, and droids highlighted
# (shown as squares in their own colour).
# Data: dplyr::starwars (built in to tidyverse). Only base R + tidyverse used.
#
# Arguments (all four required, in this order):
#   1. output     path of the PNG to write; must end in .png
#   2. n_species  how many of the most common species to show separately, a whole
#                 number from 1 to 6. Droids are always shown separately as well;
#                 every other species is grouped as "Other".
#   3. n_cols     number of dots per row, a whole number from 5 to 20.
#                 The image size adapts to this.
#   4. palette    "standard" or "colorblind" (Okabe-Ito colours, safe for the
#                 common forms of colour blindness)
#
# Command that produced the committed image (run from the repository root):
#   Rscript scripts/species_waffle.R figures/species_waffle.png 5 10 standard

suppressPackageStartupMessages(library(tidyverse))

# ---- arguments ---------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
usage <- paste0("Usage: Rscript scripts/species_waffle.R ",
                "<output.png> <n_species 1-6> <n_cols 5-20> <standard|colorblind>")

if (length(args) != 4) {
  stop("Expected 4 arguments but got ", length(args), ".\n", usage, call. = FALSE)
}

whole_number <- function(text, label, lo, hi) {
  value <- suppressWarnings(as.numeric(text))
  if (is.na(value) || value != round(value) || value < lo || value > hi) {
    stop("<", label, "> must be a whole number between ", lo, " and ", hi,
         ", got '", text, "'.\n", usage, call. = FALSE)
  }
  as.integer(value)
}

out_file     <- args[1]
n_species    <- whole_number(args[2], "n_species", 1, 6)
n_cols       <- whole_number(args[3], "n_cols", 5, 20)
palette_name <- args[4]
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  stop("<output> must end in .png, got '", out_file, "'.\n", usage, call. = FALSE)
}
if (!palette_name %in% c("standard", "colorblind")) {
  stop("<palette> must be 'standard' or 'colorblind', got '", palette_name, "'.\n",
       usage, call. = FALSE)
}

bg <- "#0b0d17"; fg <- "#f5f5f5"

pal <- switch(palette_name,
              standard   = list(human = "#f2c14e", droid = "#4cc9f0", other = "#8a8fa3",
                                others = c("#f72585", "#80ed99", "#b388eb", "#ff7f50", "#d4e157")),
              colorblind = list(human = "#E69F00", droid = "#56B4E9", other = "#999999",
                                others = c("#009E73", "#F0E442", "#CC79A7", "#D55E00", "#0072B2"))
)

# ---- data --------------------------------------------------------------------
species_known <- starwars |> filter(!is.na(species))

counts_all <- species_known |> count(species, sort = TRUE)
keep <- union(slice_head(counts_all, n = n_species)$species, "Droid")  # droids always shown

group_levels <- c(counts_all$species[counts_all$species %in% keep], "Other")

d <- species_known |>
  mutate(group = if_else(species %in% keep, species, "Other"),
         group = droplevels(factor(group, levels = group_levels)))

# One block of dots per group; each block starts on a new row, with a small gap
blocks <- d |>
  count(group, name = "n_g") |>
  mutate(rows    = ceiling(n_g / n_cols),
         start   = cumsum(dplyr::lag(rows + 0.6, default = 0)),
         label_y = -(start + (rows - 1) / 2),
         label   = sprintf("%s (%d)", as.character(group), n_g))

d <- d |>
  arrange(group, name) |>
  group_by(group) |>
  mutate(k = row_number() - 1) |>
  ungroup() |>
  left_join(select(blocks, group, start), by = "group") |>
  mutate(x = k %% n_cols,
         y = -(start + k %/% n_cols),
         is_droid = group == "Droid")

# colours: Human and Droid fixed, "Other" grey, remaining species from the palette
lv   <- levels(d$group)
cols <- setNames(rep(NA_character_, length(lv)), lv)
cols["Human"] <- pal$human
cols["Droid"] <- pal$droid
if ("Other" %in% lv) cols["Other"] <- pal$other
rest <- is.na(cols)
cols[rest] <- pal$others[seq_len(sum(rest))]

# numbers used in the text (computed from the data)
n_total   <- nrow(d)
human_pct <- 100 * mean(d$species == "Human")
n_droid   <- sum(d$is_droid)
droid_pct <- 100 * n_droid / n_total

# ---- layout ------------------------------------------------------------------
# 1 unit on the grid = 0.42 inch, so dots and text keep the same proportions
# whatever n_cols is. The image size is derived from the grid.
unit_in   <- 0.42
margin_in <- 45 / 72                       # left + right plot margin (20 + 25 pt)
right_ext <- 6.5                           # room on the right for the labels
width_in  <- max(8, unit_in * (n_cols + right_ext) + margin_in)
x_span    <- (width_in - margin_in) / unit_in

y_max    <- 4.0                            # room above the grid for the callout
y_min    <- min(d$y) - 0.8
number_y <- 2.4
height_in <- unit_in * (y_max - y_min) + 2.2   # + title, subtitle and caption

# ---- plot --------------------------------------------------------------------
p <- ggplot(d, aes(x, y)) +
  geom_point(aes(colour = group, shape = is_droid, size = is_droid)) +
  geom_text(data = filter(blocks, group != "Droid"),
            aes(x = n_cols - 0.3, y = label_y, label = label, colour = group),
            hjust = 0, fontface = "bold", size = 4.6) +
  geom_text(data = filter(blocks, group == "Droid"),
            aes(x = n_cols - 0.3, y = label_y, label = label, colour = group),
            hjust = 0, fontface = "bold", size = 5.8) +
  # big number callout above the grid
  annotate("text", x = -0.5, y = number_y, label = sprintf("%.0f%%", human_pct),
           hjust = 0, colour = cols[["Human"]], fontface = "bold", size = 30) +
  annotate("text", x = 5.6, y = number_y,
           label = "of the characters with a\nrecorded species are human",
           hjust = 0, colour = fg, size = 5.2, lineheight = 1.1) +
  scale_colour_manual(values = cols, guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 15), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 8, `TRUE` = 6.8), guide = "none") +
  coord_fixed(xlim = c(-0.5, -0.5 + x_span), ylim = c(y_min, y_max), expand = FALSE) +
  labs(
    title    = "Who lives in the Star Wars galaxy?",
    subtitle = sprintf(
      paste0("Every dot is one character (%d with a recorded species); droids are squares.\n",
             "Droids: %d characters (%.0f%%).\n",
             "The %d most common species (plus droids) are shown separately, all others as Other."),
      n_total, n_droid, droid_pct, n_species),
    caption  = paste0("Source: dplyr::starwars (dplyr ", packageVersion("dplyr"), ").",
                      if (palette_name == "colorblind") " Colours: Okabe-Ito palette." else "")
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    text            = element_text(colour = fg),
    plot.title      = element_text(face = "bold", size = 26, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 12, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption    = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin     = margin(20, 25, 15, 20)
  )

# ---- write -------------------------------------------------------------------
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
ggsave(out_file, p, width = width_in, height = height_in, dpi = 150, bg = bg)
message("Wrote ", out_file)
