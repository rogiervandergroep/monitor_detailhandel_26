### script herkomst bestedingen centrum ----

library(tidyverse)
library(openxlsx)


# nb: deze dataset is aangemaakt in script 02 kkb tabellen basis
rechte_tellingen_totaal <- read_rds(
  "01 references/kkb_rechte_tellingen_totaal.rds"
)

# herkomst naar stadsdeel
my_herkomst <- function(x, group_vars) {
  x |>
    ungroup() |>
    filter(
      !is.na(woon_gebied_naam),
      gebied == 'stadsdeel'
    ) |>
    select(
      woon_gebied_naam,
      woon_gebied_code,
      aantal,
      aantal_gew,
      aantal_gew_omz,
      all_of(group_vars)
    ) |>
    group_by(
      woon_gebied_naam,
      woon_gebied_code,
      across(all_of(group_vars))
    ) |>
    summarise(
      aantal = sum(aantal),
      aantal_gew = sum(aantal_gew),
      aantal_gew_omz = sum(aantal_gew_omz),
    ) |>
    group_by(across(all_of(group_vars))) |>
    mutate(
      aandeel = aantal / sum(aantal),
      aandeel_gew = aantal_gew / sum(aantal_gew),
      aandeel_gew_omz = aantal_gew_omz / sum(aantal_gew_omz)
    )
}

# toevoegen jaargang
jaren <- c("monitor 2020", "monitor 2022", "monitor 2024", "monitor 2026")

herkomst_winkel_ggw_gebied_mode <- rechte_tellingen_totaal |>
  map(\(x) filter(x, productgroep %in% 'modeartikelen')) |>
  map(\(x) my_herkomst(x, group_vars = c("wink_ggw_code", "wink_ggw_naam"))) |>
  map2_df(jaren, \(x, y) add_column(x, monitor = y))

herkomst_winkel_ggw_gebied_recr <- rechte_tellingen_totaal |>
  map(\(x) filter(x, productgroep %in% recreatief)) |>
  map(\(x) my_herkomst(x, group_vars = c("wink_ggw_code", "wink_ggw_naam"))) |>
  map2_df(jaren, \(x, y) add_column(x, monitor = y))


herkomst_winkel_ggw_gebied_doelg <- rechte_tellingen_totaal |>
  map(\(x) filter(x, productgroep %in% doelgericht)) |>
  map(\(x) my_herkomst(x, group_vars = c("wink_ggw_code", "wink_ggw_naam"))) |>
  map2_df(jaren, \(x, y) add_column(x, monitor = y))


source("02 scr/03 script ggplot functies.R")
source("02 scr/01 script factor levels.R")


my_plot_herk <- function(x, x_var, y_var, fill_var) {
  discreet9 <- c(
    "#004699",
    "#6cbd74",
    "#009dec",
    "#bed200",
    "#fdb0cb",
    "#ffe600",
    "#d48fb9",
    "#ff9100",
    "#ec0000"
  )

  grDevices::windowsFonts(
    "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
  )

  font <- "Amsterdam Sans"

  hcl <- farver::decode_colour(discreet9, "rgb", "hcl")

  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  x |>
    ggplot(aes(
      y = {{ y_var }},
      group = fct_rev({{ fill_var }}),
      x = {{ x_var }} * 100,
    )) +

    geom_col(
      aes(fill = {{ fill_var }})
    ) +

    geom_text(
      aes(
        label = if_else(
          round({{ x_var }} * 100) > 4,
          round({{ x_var }} * 100),
          NA
        ),
        color = {{ fill_var }}
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
      values = discreet9
    ) +

    scale_color_manual(
      name = NULL,
      values = label_col
    ) +

    guides(
      fill = guide_legend(
        nrow = 1,
        reverse = F
      ),
      colour = "none"
    )
}


# fig dagelijks niet dagelijks amsterdam winkelgebieden centrum
herkomst_winkel_ggw_gebied_mode |>
  filter(wink_ggw_code %in% c("GA01", "GA02", "GE05", "GK13")) |>

  mutate(woon_gebied_naam = factor(woon_gebied_naam, levels = lev_sd)) |>
  my_plot_herk(
    x_var = aandeel_gew,
    y_var = fct_rev(monitor),
    fill_var = woon_gebied_naam
  ) +
  facet_wrap(~wink_ggw_naam, nrow = 1)
ggsave(
  "04 reports/04 figuren/fig_bereik_winkelgebieden_mode.svg",
  width = 12,
  height = 5
)


# fig dagelijks niet dagelijks amsterdam winkelgebieden centrum
herkomst_winkel_ggw_gebied_recr |>
  filter(wink_ggw_code %in% c("GA01", "GA02", "GE05", "GK13")) |>
  mutate(woon_gebied_naam = factor(woon_gebied_naam, levels = lev_sd)) |>
  my_plot_herk(
    x_var = aandeel_gew,
    y_var = fct_rev(monitor),
    fill_var = woon_gebied_naam
  ) +
  facet_wrap(~wink_ggw_naam, nrow = 1)
ggsave(
  "04 reports/04 figuren/fig_bereik_winkelgebieden_recr.svg",
  width = 12,
  height = 5
)


# fig dagelijks niet dagelijks amsterdam winkelgebieden centrum
herkomst_winkel_ggw_gebied_doelg |>
  filter(wink_ggw_code %in% c("GA01", "GA02", "GE05", "GK13")) |>
  mutate(woon_gebied_naam = factor(woon_gebied_naam, levels = lev_sd)) |>
  my_plot_herk(
    x_var = aandeel_gew,
    y_var = fct_rev(monitor),
    fill_var = woon_gebied_naam
  ) +
  facet_wrap(~wink_ggw_naam, nrow = 1)
ggsave(
  "04 reports/04 figuren/fig_bereik_winkelgebieden_doelg.svg",
  width = 12,
  height = 5
)
