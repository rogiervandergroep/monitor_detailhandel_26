library(tidyverse)
library(patchwork)
library(svglite)
library(scales)


source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
)


discreet <- c(
  "#004699",
  "#ec0000",
  "#ff9100",
  "#d48fb9",
  "#ffe600",
  "#6cbd74",
  "#009dec"
)

wild_pal <- c(
  "#004699",
  "#009de6",
  "#53b361",
  "#bed200",
  "#ffe600",
  "#ff9100",
  "#ec0000"
)

discreet9 <- c(
  "#e6e6e6",
  "#004699",
  "#009dec",
  "#6cbd74",
  "#bed200",
  "#ffe600",
  "#fdb0cb",
  "#d48fb9",
  "#ff9100",
  "#ec0000"
)

blauw_pal <- c(
  "#004699",
  "#3858a4",
  "#566bb0",
  "#707ebb",
  "#8992c6",
  "#a1a7d2",
  "#b8bcdd",
  "#d0d2e8",
  "#e7e8f4",
  "#e6e6e6"
)

grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"


theme_map <- function(legend_position = c(0, 0)) {
  theme_bw() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      plot.background = element_blank(),
      legend.justification = c(0, 0),
      legend.position = legend_position,
      plot.caption = element_text(family = font, size = 12),
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(family = font, lineheight = 1.2, size = 12),
      legend.title = element_text(family = font, lineheight = 1.2, size = 12),
      panel.border = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(color = "black", family = font, size = 12)
    )
}


my_plot <- function(x, x_var, y_var, fill_var, color_var, guide_nr = 1) {
  hcl <- farver::decode_colour(color_var, "rgb", "hcl")

  label_col <- ifelse(hcl[, "l"] < 50, "black", "white")

  x |>
    ggplot(aes(
      x = {{ x_var }},
      y = {{ y_var }},
      group = fct_rev({{ fill_var }}),
      fill = fct_rev({{ fill_var }})
    )) +
    geom_col() +
    geom_text(
      aes(
        label = if_else(
          round({{ x_var }} * 100) > 4,
          round({{ x_var }} * 100),
          NA
        ),
        color = {{ fill_var }}
      ),
      size = 4.5,
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +

    labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +

    theme_os(legend_position = "bottom") +
    theme(text = element_text(size = 16)) +

    scale_fill_manual(
      name = NULL,
      values = color_var
    ) +

    scale_color_manual(
      name = NULL,
      values = label_col
    ) +
    scale_x_continuous(labels = scales::label_percent(suffix = "%")) +
    guides(
      fill = guide_legend(
        nrow = guide_nr,
        reverse = T
      ),
      colour = "none"
    )
}


my_plot_een <- function(x, x_var, y_var, treshhold = 4) {
  x |>
    ggplot(aes(
      x = {{ x_var }},
      y = {{ y_var }}
    )) +
    geom_col(fill = "#004699") +
    geom_text(
      aes(
        label = if_else(
          {{ x_var }} > treshhold,
          round({{ x_var }} * 100),
          NA
        )
      ),
      size = 4.5,
      hjust = 1.3,
      color = "#ffffffff",
      position = position_dodge(width = 0.9),
      family = font,
      lineheight = .8
    ) +

    labs(title = NULL, x = NULL, y = NULL) +
    guides(color = 'none') +
    theme_os(legend_position = "bottom") +
    theme(text = element_text(size = 16))
}
