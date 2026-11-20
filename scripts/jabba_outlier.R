# jabba_outlier.R -- "One slug skews the whole galaxy"
# Height vs. mass of Star Wars characters, with and without Jabba the Hutt.
# Data: dplyr::starwars (built in to tidyverse). Only base R + tidyverse used.
#
# Arguments (all three required, in this order):
#   1. output     path of the PNG to write; must end in .png
#   2. base_size  base text size in points, a number between 8 and 24
#   3. palette    "standard", or a colour-blind palette: "colorblind" (general),
#                 "deuteranopia", "protanopia" or "tritanopia"
#
# Command that produced the committed image (run from the repository root):
#   Rscript scripts/jabba_outlier.R figures/jabba_outlier.png 14 standard

suppressPackageStartupMessages(library(tidyverse))

# ---- arguments ---------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
usage <- paste0("Usage: Rscript scripts/jabba_outlier.R <output.png> <base_size 8-24> ",
                "<standard|colorblind|deuteranopia|protanopia|tritanopia>")

if (length(args) != 3) {
  stop("Expected 3 arguments but got ", length(args), ".\n", usage, call. = FALSE)
}
out_file     <- args[1]
base_size    <- suppressWarnings(as.numeric(args[2]))
palette_name <- tolower(args[3])
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  stop("<output> must end in .png, got '", out_file, "'.\n", usage, call. = FALSE)
}
if (is.na(base_size) || base_size < 8 || base_size > 24) {
  stop("<base_size> must be a number between 8 and 24, got '", args[2], "'.\n",
       usage, call. = FALSE)
}

# Colour-blind palettes. "colorblind" is Okabe-Ito. The three specific types use the colour
# pair that suits them (checked with a colour-vision simulation): yellow + blue for the
# red-green types (deuteranopia, protanopia), red + teal for tritanopia (blue-yellow).
# human / droid / others[1..5] / other (grey) are used in this order; jabba = highlight colour.
cb_sets <- list(
  colorblind   = list(human = "#E69F00", droid = "#56B4E9", other = "#999999", jabba = "#39ff14",
                      others = c("#009E73", "#F0E442", "#CC79A7", "#D55E00", "#0072B2")),
  deuteranopia = list(human = "#F2C14E", droid = "#4C8DFF", other = "#999999", jabba = "#33FFC2",
                      others = c("#93CEEC", "#93ECBF", "#C67953", "#E467BB", "#53C68C")),
  protanopia   = list(human = "#F2D04E", droid = "#4C8DFF", other = "#999999", jabba = "#4DFFFF",
                      others = c("#DD713C", "#93ECCE", "#93CEEC", "#E467A6", "#D2A679")),
  tritanopia   = list(human = "#FF6B81", droid = "#2EC4B6", other = "#999999", jabba = "#D1FF1A",
                      others = c("#ECCE93", "#BBE467", "#B579D2", "#7988D2", "#EC93CE"))
)

# species = colours for the (up to) four species groups in order, Other last
standard <- list(species = c("#f2c14e", "#4cc9f0", "#f72585", "#9aa0a6"), jabba = "#39ff14")
palettes <- c(
  list(standard = standard),
  lapply(cb_sets, function(x) list(species = c(x$human, x$droid, x$others[1], x$other),
                                   jabba = x$jabba))
)
if (!palette_name %in% names(palettes)) {
  stop("<palette> must be one of: ", paste(names(palettes), collapse = ", "),
       "; got '", args[3], "'.\n", usage, call. = FALSE)
}
pal <- palettes[[palette_name]]

bg <- "#0b0d17"; fg <- "#f5f5f5"; grid_col <- "#2a2f45"

# ---- data --------------------------------------------------------------------
df <- starwars |>
  filter(!is.na(height), !is.na(mass)) |>
  mutate(species = coalesce(species, "Unknown"),
         species = fct_lump_n(species, n = 3, other_level = "Other",
                              ties.method = "first"))
if ("Other" %in% levels(df$species)) {
  df <- df |> mutate(species = fct_relevel(species, "Other", after = Inf))
}

# Jabba gets his own group so he can have his own colour and legend entry
jabba_label <- "Jabba the Hutt"
species_lv  <- levels(df$species)
df <- df |>
  mutate(group = if_else(grepl("^Jabba", name), jabba_label, as.character(species)),
         group = factor(group, levels = c(species_lv, jabba_label)))

cols <- c(
  setNames(pal$species[seq_along(species_lv)], species_lv),
  setNames(pal$jabba, jabba_label)   # highlight colour for Jabba
)

no_jabba <- df |> filter(!grepl("^Jabba", name))

# Two panels: same data, then the same data minus Jabba
panels <- bind_rows(
  df       |> mutate(panel = "Everyone"),
  no_jabba |> mutate(panel = "Without Jabba")
) |>
  mutate(panel = factor(panel, levels = c("Everyone", "Without Jabba")))

labelled <- bind_rows(
  panels |> filter(panel == "Everyone", grepl("^Jabba", name)),
  panels |> filter(panel == "Without Jabba") |> slice_max(mass, n = 2, with_ties = FALSE)
)

# numbers used in the subtitle (computed, so they can never disagree with the plot)
slope_all <- unname(coef(lm(mass ~ height, data = df))[2])
slope_no  <- unname(coef(lm(mass ~ height, data = no_jabba))[2])
heaviest  <- sort(df$mass, decreasing = TRUE)[1:2]

# BMI = mass (kg) / height (m)^2, computed from the same data
bmi_of <- function(name_pattern) {
  row <- df |> filter(grepl(name_pattern, name))
  if (nrow(row) != 1) stop("Expected exactly one match for '", name_pattern, "'.", call. = FALSE)
  row$mass / (row$height / 100)^2
}
bmi_jabba <- bmi_of("^Jabba")
bmi_han   <- bmi_of("^Han Solo$")

# ---- plot --------------------------------------------------------------------
p <- ggplot(panels, aes(height, mass)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = fg, linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(colour = group, size = group == jabba_label), alpha = 0.9) +
  geom_text(data = labelled, aes(label = name), hjust = 1.12,
            colour = fg, size = base_size * 0.3, fontface = "bold") +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  scale_colour_manual(values = cols, limits = names(cols)) +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5), guide = "none") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  labs(
    title    = "One slug skews the whole galaxy",
    subtitle = sprintf(
      paste0("Jabba weighs %.0f kg, %.1fx the next-heaviest character.\n",
             "Each extra cm of height 'adds' %.1f kg with him in the data, %.1f kg without (dashed lines = linear fit).\n",
             "BMI: Jabba %.0f, Han Solo %.1f (healthy range for humans: about 18.5-25)."),
      heaviest[1], heaviest[1] / heaviest[2], slope_all, slope_no, bmi_jabba, bmi_han),
    x = "Height (cm)", y = "Mass (kg)", colour = NULL,
    caption = paste0("Source: dplyr::starwars (dplyr ", packageVersion("dplyr"), "). ",
                     nrow(df), " characters with known height and mass. ",
                     "Species: three most common among those shown, the rest as Other.",
                     if (palette_name != "standard")
                       sprintf(" Colours chosen to stay distinguishable with %s.", palette_name)
                     else "")
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    text             = element_text(colour = fg),
    axis.text        = element_text(colour = fg),
    strip.text       = element_text(colour = fg, face = "bold", size = rel(1.2), hjust = 0),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = rel(2), margin = margin(b = 6)),
    plot.subtitle    = element_text(lineheight = 1.15, margin = margin(b = 14)),
    plot.caption     = element_text(size = rel(0.7), hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin      = margin(20, 25, 15, 20)
  )

# ---- write -------------------------------------------------------------------
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
ggsave(out_file, p, width = 12, height = 7.2, dpi = 150, bg = bg)
message("Wrote ", out_file)
