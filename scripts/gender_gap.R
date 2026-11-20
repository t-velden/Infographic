# gender_gap.R -- "Where are the women of Star Wars?"
# Share of characters in each film of the original and prequel trilogies whose gender
# is "feminine", with a reference line for both trilogies combined and a comparison of
# the two trilogies. (The sequel trilogy is left out.)
# Data: dplyr::starwars (built in to tidyverse). Only base R + tidyverse used.
#
# Arguments (both required, in this order):
#   1. output   path of the PNG to write; must end in .png
#   2. palette  "dark", "light" or "pastel"
#
# Command that produced the committed image (run from the repository root):
#   Rscript scripts/gender_gap.R figures/gender_gap.png dark

suppressPackageStartupMessages(library(tidyverse))

# ---- arguments ---------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: Rscript scripts/gender_gap.R <output.png> <dark|light|pastel>"

if (length(args) != 2) {
  stop("Expected 2 arguments but got ", length(args), ".\n", usage, call. = FALSE)
}
out_file     <- args[1]
palette_name <- args[2]
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  stop("<output> must end in .png, got '", out_file, "'.\n", usage, call. = FALSE)
}
if (!palette_name %in% c("dark", "light", "pastel")) {
  stop("<palette> must be 'dark', 'light' or 'pastel', got '", palette_name, "'.\n",
       usage, call. = FALSE)
}

trilogy_levels <- c("Original trilogy", "Prequel trilogy")

# bar = bar colour per trilogy, header = colour of the trilogy headers
pal <- switch(palette_name,
              dark   = list(bg = "#0b0d17", fg = "#f5f5f5", muted = "#2a2f45",
                            bar    = setNames(rep("#f2c14e", 2), trilogy_levels),
                            header = setNames(rep("#f2c14e", 2), trilogy_levels)),
              light  = list(bg = "#faf7f0", fg = "#1b1b1b", muted = "#dcd8cc",
                            bar    = setNames(rep("#c1121f", 2), trilogy_levels),
                            header = setNames(rep("#c1121f", 2), trilogy_levels)),
              pastel = list(bg = "#f2eee9", fg = "#4a4458", muted = "#e0dbd4",   # soft warm grey
                            bar    = setNames(c("#f4a9c0", "#9cc3ea"), trilogy_levels),   # light pink, light blue
                            header = setNames(c("#c4557d", "#3f78b8"), trilogy_levels))
)

# ---- data --------------------------------------------------------------------
film_order <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi",
                "The Phantom Menace", "Attack of the Clones", "Revenge of the Sith")

trilogy_of <- c(
  "A New Hope"              = "Original trilogy",
  "The Empire Strikes Back" = "Original trilogy",
  "Return of the Jedi"      = "Original trilogy",
  "The Phantom Menace"      = "Prequel trilogy",
  "Attack of the Clones"    = "Prequel trilogy",
  "Revenge of the Sith"     = "Prequel trilogy"
)

known <- starwars |> filter(!is.na(gender))

# `films` is a list column: one row per character-film pair after unnest().
# Films outside film_order (the sequel trilogy) are dropped here.
appearances <- known |>
  select(name, gender, films) |>
  unnest(films) |>
  filter(films %in% film_order)

if (!all(film_order %in% appearances$films)) {
  stop("Some films in `film_order` were not found; your dplyr version differs. ",
       "Adjust film_order and trilogy_of.", call. = FALSE)
}

# every character who appears in at least one of the six films
cast <- appearances |> distinct(name, gender)

# One row per film. `y` is its vertical position: films are spaced one unit apart,
# with extra room above each trilogy for its header.
by_film <- appearances |>
  group_by(films) |>
  summarise(total = n(),
            women = sum(gender == "feminine"),
            share = women / total,
            .groups = "drop") |>
  mutate(trilogy = factor(unname(trilogy_of[as.character(films)]), levels = trilogy_levels),
         films   = factor(films, levels = film_order)) |>
  arrange(films) |>
  mutate(y = -(row_number() + 1.6 * as.integer(trilogy)))

# One row per trilogy: average of the per-film shares, header text and line extent
group_avg <- by_film |>
  group_by(trilogy) |>
  summarise(avg = mean(share), n_films = n(),
            y_min = min(y), y_max = max(y), .groups = "drop") |>
  mutate(header_y = y_max + 1,
         header = sprintf("%s (average %.0f%%)", as.character(trilogy), 100 * avg))

overall  <- mean(cast$gender == "feminine")
avg_orig <- group_avg$avg[group_avg$trilogy == "Original trilogy"]
avg_pre  <- group_avg$avg[group_avg$trilogy == "Prequel trilogy"]
diff_pp  <- 100 * (avg_pre - avg_orig)

y_bot <- min(by_film$y)

# ---- plot --------------------------------------------------------------------
p <- ggplot(by_film) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = y - 0.35, ymax = y + 0.35), fill = pal$muted) +
  geom_rect(aes(xmin = 0, xmax = share, ymin = y - 0.35, ymax = y + 0.35, fill = trilogy)) +
  # reference line, drawn per trilogy so it never crosses the headers
  geom_segment(data = group_avg,
               aes(x = overall, xend = overall, y = y_min - 0.5, yend = y_max + 0.5),
               colour = pal$fg, linetype = "dashed", linewidth = 0.6) +
  geom_text(aes(x = 0.99, y = y,
                label = sprintf("%.0f%%  (%d of %d)", 100 * share, women, total)),
            hjust = 1, colour = pal$fg, fontface = "bold", size = 4.5) +
  geom_text(data = group_avg, aes(x = 0, y = header_y, label = header, colour = trilogy),
            hjust = 0, fontface = "bold", size = 5) +
  annotate("text", x = overall, y = y_bot - 0.95,
           label = sprintf("Both trilogies: %.0f%%", 100 * overall),
           colour = pal$fg, fontface = "bold", size = 4.2) +
  scale_fill_manual(values = pal$bar, guide = "none") +
  scale_colour_manual(values = pal$header, guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(breaks = by_film$y, labels = as.character(by_film$films),
                     expand = expansion(add = c(1.4, 0.7))) +
  labs(
    title    = "Where are the women of Star Wars?",
    subtitle = sprintf(
      paste0("Share of women per film among characters with a recorded gender (%d in both trilogies).\n",
             "Dashed line: both trilogies combined (%.0f%%).\n",
             "Average per film: original trilogy %.0f%%, prequel trilogy %.0f%% (%+.0f percentage points)."),
      nrow(cast), 100 * overall, 100 * avg_orig, 100 * avg_pre, diff_pp),
    caption  = paste0("Source: dplyr::starwars (dplyr ", packageVersion("dplyr"),
                      "). Gender recorded as 'feminine' or 'masculine'. Sequel trilogy not included. ",
                      "Averages are unweighted means of the per-film shares.")
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
ggsave(out_file, p, width = 9, height = 7.2, dpi = 150, bg = pal$bg)
message("Wrote ", out_file)