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
  "#FFD4E2",
  "#EC0000",
  "#FFC88E",
  "#d48fb9",
  "#F6F6D4",
  "#BED200",
  "#D6ECD6",
  "#53B361",
  "#949CCC",
  "#004699",
  "#ff9100",
  "#ffe600",
  "#009dec",
  "#a2bad3"
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
  "#e7e8f4"
)


grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

theme_os2 <- function(orientation = "horizontal", legend_position = "right") {
  theme <- theme_bw() +
    theme(
      text = element_text(family = font, size = 12),
      axis.text = element_text(family = font, size = 12),
      plot.caption = element_text(family = font, size = 12),
      axis.title = element_text(family = font, hjust = 1, size = 12),
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(
        family = font,
        lineheight = 1.2,
        size = 12
      ),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      legend.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = legend_position,
      panel.border = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(
        color = "black",
        family = font,
        face = "bold",
        size = 12
      )
    )

  if (orientation %in% c("vertical", "v")) {
    theme <- theme + theme(panel.grid.major.x = element_blank())
  } else if (orientation %in% c("horizontal", "h")) {
    theme <- theme + theme(panel.grid.major.y = element_blank())
  }
}

theme_os3 <- function(legenda_pos) {
  theme_bw() +
    theme(
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(family = font, lineheight = 1.2, size = 14),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      panel.border = element_rect(fill = "transparent", color = NA),
      plot.background = element_blank(),
      legend.position = legenda_pos,
    )
}


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


my_plot <- function(x, y_var, afzet_var) {
  x |>
    ggplot(aes(
      y = {{ y_var }},
      group = fct_rev({{ afzet_var }}),
      x = aandeel_gew_omz * 100,
    )) +

    geom_col(
      aes(fill = {{ afzet_var }})
    ) +

    geom_text(
      aes(
        label = if_else(
          round(aandeel_gew_omz * 100) > 4,
          round(aandeel_gew_omz * 100),
          NA
        ),
        color = {{ afzet_var }}
      ),

      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +

    labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +

    theme_os2(legend_position = "bottom") +

    scale_fill_manual(
      name = NULL,
      values = blauw_pal[c(2, 3, 5, 6, 7, 8)]
    ) +

    scale_color_manual(
      name = NULL,
      values = label_col[c(2, 3, 5, 6, 7, 8)]
    ) +

    guides(
      fill = guide_legend(
        nrow = 1,
        reverse = F
      ),
      colour = "none"
    )
}

# scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
# scale_x_continuous(limits = c(2013, 2025), breaks = c(2014, 2016, 2018, 2020, 2022, 2024))+
# geom_text(aes(
#        label = scales::label_percent(accuracy = 1)(value)
# ),
#   family = font,
#   position = position_stack(vjust = 0.5),
#   lineheight = .8
# ) +
# labs(y = NULL, x = NULL) +
# theme_os(orientation = "horizontal", legend_position = 'bottom') +
# scale_fill_manual(name = NULL, values = blauw_pal[c(1, 2, 4, 6, 8)]) +
# scale_color_manual(name = NULL, values = label_col[c(1, 2, 4, 6, 8)]) +
# scale_x_continuous(labels = scales::label_percent(suffix = "%"))+
