# species_waffle.R -- "Humans are just N% of Star Wars"
# Waffle chart: one square per character, coloured by species.
# Data: dplyr::starwars (built in to tidyverse). Only base R + tidyverse used.
#
# Arguments (required):
#   1. output     path of the PNG to write; must end in .png
#   2. n_species  how many species to show separately (whole number, 1-10);
#                 all remaining species are grouped as "Other"
#
# Command that produced the committed image (run from the repository root):
#   Rscript scripts/species_waffle.R figures/species_waffle.png 5

suppressPackageStartupMessages(library(tidyverse))

# ---- arguments ---------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: Rscript scripts/species_waffle.R <output.png> <n_species 1-10>"

if (length(args) != 2) {
  stop("Expected 2 arguments but got ", length(args), ".\n", usage, call. = FALSE)
}
out_file <- args[1]
n_num    <- suppressWarnings(as.numeric(args[2]))
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  stop("<output> must end in .png, got '", out_file, "'.\n", usage, call. = FALSE)
}
if (is.na(n_num) || n_num != round(n_num) || n_num < 1 || n_num > 10) {
  stop("<n_species> must be a whole number between 1 and 10, got '", args[2], "'.\n",
       usage, call. = FALSE)
}
n_species <- as.integer(n_num)

bg <- "#0b0d17"; fg <- "#f5f5f5"

# ---- data --------------------------------------------------------------------
d <- starwars |>
  filter(!is.na(species)) |>
  mutate(group = fct_lump_n(species, n = n_species, other_level = "Other",
                            ties.method = "first"),
         group = fct_infreq(group))
if ("Other" %in% levels(d$group)) {
  d <- d |> mutate(group = fct_relevel(group, "Other", after = Inf))
}

d <- d |> arrange(group, name)

# lay the characters out on a grid, 10 squares per row, first row on top
n_cols <- 10
d <- d |>
  mutate(i = row_number() - 1,
         x = i %% n_cols,
         y = -(i %/% n_cols))

counts <- d |> count(group)
labels <- sprintf("%s (%d)", as.character(counts$group), counts$n)

k    <- nlevels(d$group)
cols <- hcl.colors(k, "Dark 3")
cols[1] <- "#f2c14e"                                   # most common group in gold
if ("Other" %in% levels(d$group)) cols[k] <- "#6b7280" # Other in grey

human_pct <- 100 * mean(d$species == "Human")

# ---- plot --------------------------------------------------------------------
p <- ggplot(d, aes(x, y, fill = group)) +
  geom_tile(width = 0.88, height = 0.88) +
  scale_fill_manual(values = cols, limits = levels(d$group), labels = labels) +
  coord_equal() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title    = sprintf("Humans are just %.0f%% of Star Wars", human_pct),
    subtitle = sprintf(
      "Every square is one character (%d with a recorded species).\nTop %d species shown separately, all others grouped as Other.",
      nrow(d), n_species),
    fill    = NULL,
    caption = paste0("Source: dplyr::starwars (dplyr ", packageVersion("dplyr"), ").")
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    text            = element_text(colour = fg),
    legend.position = "bottom",
    legend.text     = element_text(colour = fg, size = 11),
    legend.key.size = unit(0.5, "cm"),
    plot.title      = element_text(face = "bold", size = 26, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 12, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption    = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin     = margin(20, 25, 15, 20)
  )

# ---- write -------------------------------------------------------------------
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
ggsave(out_file, p, width = 8, height = 8, dpi = 150, bg = bg)
message("Wrote ", out_file)
