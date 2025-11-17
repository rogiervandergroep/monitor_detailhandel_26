


my_col_figure <- function(
  x,
  y_var = value,
  group_var,
  positie = "stack",
  facet_var
) {
  x |>
    mutate(
      peildatum = lubridate::as_date(peildatum)
    ) |>

    ggplot(aes(
      x = lubridate::year(peildatum),
      y = {{ y_var }},
      group = fct_rev({{ group_var }}),
      fill = fct_rev({{ group_var }})
    )) +
    geom_col(position = positie) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_os2(orientation = "vertical", legend_position = "bottom") +
    scale_fill_manual(name = NULL, values = wild_pal) +
    scale_y_continuous(
      labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
    ) +
    scale_x_continuous(
      limits = c(2013, 2025),
      breaks = c(2014, 2016, 2018, 2020, 2022, 2024)
    ) +
    guides(fill = guide_legend(reverse = F, nrow = 1)) +
    facet_wrap(vars({{ facet_var }}), nrow = 1)
}


my_line_figure <- function(x, y_val = value, fil_var) {
  x |>

    ggplot(aes(
      x = lubridate::year(peildatum),
      y = {{ y_val }},
      group = {{ fil_var }},
      colour = {{ fil_var }}
    )) +

    geom_line(linewidth = 0.9) +
    labs(y = NULL, x = NULL) +
    theme_os2(orientation = "vertical", legend_position = 'bottom') +
    scale_color_manual(name = NULL, values = discreet) +
    scale_linewidth_manual(name = NULL, values = c(0.8, 1, 2)) +
    guides(colour = guide_legend(ncol = 1, reverse = T), linewidth = 'none') +
    scale_x_continuous(
      limits = c(2014, 2024),
      breaks = c(2014, 2016, 2018, 2020, 2022, 2024)
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
    )
}