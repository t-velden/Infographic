# gender_gap.R -- "Where are the women of Star Wars?"
# Share of characters in each film whose gender is "feminine".
# Data: dplyr::starwars (built in to tidyverse). Only base R + tidyverse used.
#
# Arguments (both required, in this order):
#   1. output   path of the PNG to write; must end in .png
#   2. palette  "dark" or "light"
#
# Command that produced the committed image (run from the repository root):
#   Rscript scripts/gender_gap.R figures/gender_gap.png dark

suppressPackageStartupMessages(library(tidyverse))

# ---- arguments ---------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: Rscript scripts/gender_gap.R <output.png> <dark|light>"

if (length(args) != 2) {
  stop("Expected 2 arguments but got ", length(args), ".\n", usage, call. = FALSE)
}
out_file     <- args[1]
palette_name <- args[2]
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  stop("<output> must end in .png, got '", out_file, "'.\n", usage, call. = FALSE)
}
if (!palette_name %in% c("dark", "light")) {
  stop("<palette> must be 'dark' or 'light', got '", palette_name, "'.\n",
       usage, call. = FALSE)
}

pal <- switch(palette_name,
  dark  = list(bg = "#0b0d17", fg = "#f5f5f5", muted = "#2a2f45", accent = "#f2c14e"),
  light = list(bg = "#faf7f0", fg = "#1b1b1b", muted = "#dcd8cc", accent = "#c1121f")
)

# ---- data --------------------------------------------------------------------
film_order <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi",
                "The Phantom Menace", "Attack of the Clones",
                "Revenge of the Sith", "The Force Awakens")

known <- starwars |> filter(!is.na(gender))

# `films` is a list column: one row per character-film pair after unnest()
appearances <- known |>
  select(name, gender, films) |>
  unnest(films)

if (!all(appearances$films %in% film_order)) {
  stop("Found film names not in `film_order`; your dplyr version differs. ",
       "Adjust film_order.", call. = FALSE)
}

by_film <- appearances |>
  mutate(films = factor(films, levels = rev(film_order))) |>  # rev: first film on top
  group_by(films) |>
  summarise(total = n(),
            women = sum(gender == "feminine"),
            share = women / total,
            .groups = "drop")

overall <- mean(known$gender == "feminine")
best    <- by_film |> slice_max(share, n = 1, with_ties = FALSE)

# ---- plot --------------------------------------------------------------------
p <- ggplot(by_film, aes(y = films)) +
  geom_col(aes(x = 1),     fill = pal$muted,  width = 0.6) +
  geom_col(aes(x = share), fill = pal$accent, width = 0.6) +
  geom_text(aes(x = share,
                label = sprintf("%.0f%%  (%d of %d)", 100 * share, women, total)),
            hjust = -0.1, colour = pal$fg, fontface = "bold", size = 4.5) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Where are the women of Star Wars?",
    subtitle = sprintf(
      paste0("Share of characters in each film who are women, among the %d characters\n",
             "with a recorded gender. Whole saga: %.0f%%. Highest: %s (%.0f%%)."),
      nrow(known), 100 * overall, as.character(best$films), 100 * best$share),
    caption  = paste0("Source: dplyr::starwars (dplyr ", packageVersion("dplyr"),
                      "). Gender recorded as 'feminine' or 'masculine'.")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal$bg, colour = NA),
    panel.grid      = element_blank(),
    text            = element_text(colour = pal$fg),
    axis.text.x     = element_blank(),
    axis.text.y     = element_text(colour = pal$fg, size = 13, face = "bold"),
    axis.title      = element_blank(),
    plot.title      = element_text(face = "bold", size = 26, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 12, lineheight = 1.15, margin = margin(b = 16)),
    plot.caption    = element_text(size = 9, hjust = 0, margin = margin(t = 14)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin     = margin(20, 25, 15, 20)
  )

# ---- write -------------------------------------------------------------------
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
ggsave(out_file, p, width = 9, height = 6.5, dpi = 150, bg = pal$bg)
message("Wrote ", out_file)
