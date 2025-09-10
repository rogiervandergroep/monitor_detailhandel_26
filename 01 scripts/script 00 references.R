library(tidyverse)
library(patchwork)
library(svglite)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

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

