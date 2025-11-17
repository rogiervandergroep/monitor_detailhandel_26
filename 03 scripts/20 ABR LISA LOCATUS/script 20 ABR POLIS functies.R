# figuren polis data


# omzetten sd code naar naam
my_mutate_sd <- function(x) {
  x |>
    mutate(
      woondeelregio_sd_naam = case_when(
        woondeelregio_sd == 'A' ~ "woont in Amsterdam",
        woondeelregio_sd == 'B' ~ "woont in Amsterdam",
        woondeelregio_sd == 'E' ~ "woont in Amsterdam",
        woondeelregio_sd == 'F' ~ "woont in Amsterdam",
        woondeelregio_sd == 'K' ~ "woont in Amsterdam",
        woondeelregio_sd == 'M' ~ "woont in Amsterdam",
        woondeelregio_sd == 'N' ~ "woont in Amsterdam",
        woondeelregio_sd == 'T' ~ "woont in Amsterdam",
        woondeelregio_sd == 'S' ~ "woont in Amsterdam",
        woondeelregio_sd == 'woont buiten MRA' ~ "woont buiten MRA",
        TRUE ~ 'woont in overig MRA'
      )
    ) |>
    mutate(
      woondeelregio_sd_naam = factor(
        woondeelregio_sd_naam,
        levels = c(
          "woont in Amsterdam",
          "woont in overig MRA",
          "woont buiten MRA"
        )
      )
    )
}





hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

my_figuur_banen <- function(x, eenheid_var, sector_var, achtergrond_var) {
  x |>
    filter(
      sector %in% sector_var, # selecteer de sector
      jaar == '2023', # selecteer het jaar
      name == eenheid_var,
      achtergrond == achtergrond_var
    ) |> # selecteer het achtergrondkenmerk

    ggplot(aes(
      x = value,
      y = fct_rev(sector_type),
      label = round(value),
      fill = fct_rev(achtergrond_type2)
    )) +
    geom_col() +
    geom_text(
      aes(color = fct_rev(achtergrond_type2)),
      family = font,
      position = position_stack(vjust = 0.5),
      lineheight = .8
    ) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = "horizontal", legend_position = 'bottom') +
    scale_fill_manual(name = NULL, values = blauw_pal[c(1, 8, 6, 4, 2)]) +
    scale_color_manual(name = NULL, values = label_col[c(1, 8, 6, 4, 2)]) +
    facet_wrap(~sector, scales = 'free_y') +
    guides(fill = guide_legend(nrow = 2, reverse = T), colour = "none")
}


my_figuur_banen_sector <- function(
  x,
  achtergrond_var
) {
  x |>
    filter(
      sector != 'overige sectoren',
      jaar == '2023', # selecteer het jaar
      name == "aandeel banen",
      achtergrond %in% achtergrond_var
    ) |> # selecteer het achtergrondkenmerk

    ggplot(aes(
      x = value,
      y = fct_rev(sector),
      label = round(value),
      fill = fct_rev(achtergrond_type2)
    )) +
    geom_col() +
    geom_text(
      aes(color = fct_rev(achtergrond_type2)),
      family = font,
      position = position_stack(vjust = 0.5),
      lineheight = .8
    ) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = "horizontal", legend_position = 'bottom') +
    scale_fill_manual(name = NULL, values = blauw_pal[c(1, 8, 6, 4, 2)]) +
    scale_color_manual(name = NULL, values = label_col[c(1, 8, 6, 4, 2)]) +
    guides(fill = guide_legend(ncol = 1, reverse = T), colour = "none")
}


my_figuur_uren_lijn <- function(x, achtergrondvar) {
  x |>
    filter(
      name == "(uitbetaald) loon per uur",
      achtergrond_type2 != 'onbekend',
      achtergrond %in% c("totaal", achtergrondvar)
    ) |>

    ggplot(aes(
      x = jaar,
      y = value,
      group = fct_rev(achtergrond_type2),
      color = fct_rev(achtergrond_type2)
    )) +

    geom_line(aes(linewidth = achtergrond)) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = "vertical") +
    scale_color_manual(name = NULL, values = discreet) +
    scale_linewidth_manual(name = NULL, values = c(0.8, 1, 2)) +
    guides(colour = guide_legend(nrow = 1, reverse = T), linewidth = 'none') +
    scale_x_continuous(limits = c(2017, 2023), breaks = c(2017, 2020, 2023)) +
    scale_y_continuous(
      labels = scales::label_currency(
        prefix = "€",
        decimal.mark = "."
      )
    ) +
    facet_wrap(~sector)
}

my_woon_werk_bar <- function(x) {
  x |>
    filter(
      jaar == '2023',
      name == 'aandeel'
    ) |>
    ggplot(aes(
      x = value,
      y = fct_rev(sectie),
      label = round(value),
      fill = fct_rev(woondeelregio_sd_naam)
    )) +
    geom_col() +
    geom_text(
      aes(color = fct_rev(woondeelregio_sd_naam)),
      family = font,
      position = position_stack(vjust = 0.5),
      lineheight = .8
    ) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = "horizontal", legend_position = 'bottom') +
    scale_fill_manual(name = NULL, values = blauw_pal[c(6, 4, 2)]) +
    scale_color_manual(name = NULL, values = label_col[c(6, 4, 2)]) +
    guides(fill = guide_legend(nrow = 3, reverse = T), colour = "none")
}
