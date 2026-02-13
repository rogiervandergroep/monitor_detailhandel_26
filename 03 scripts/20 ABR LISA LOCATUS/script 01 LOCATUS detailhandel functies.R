# recreatief: mode, huishoudelijk, sport en spel media en hobby

# doelgericht: elektronica, doe het zelf woning tuinartikelen

my_group_by <- function(x) {
  x |>
    mutate(
      dg_nd = case_when(
        groep == '00-Leegstand' ~ 'leegstand',
        groep == '11-Dagelijks' ~ 'detailhandel dagelijks',
        groep %in% c('59-Leisure', "58-Cultuur & Ontspanning", '59-Horeca') ~
          'horeca en vrije tijd',
        groep == '65-Diensten' ~ 'diensten',
        groep %in%
          c(
            '22-Mode & Luxe',
            '35-Vrije Tijd',
            '37-In/Om Huis',
            '38-Detailh Overig'
          ) ~
          'detailhandel niet-dagelijks'
      )
    ) |>
    mutate(
      rec_doel = case_when(
        groep == '00-Leegstand' ~ 'leegstand',
        groep == '11-Dagelijks' ~ 'detailhandel dagelijks',
        groep %in% c('59-Leisure', "58-Cultuur & Ontspanning", '59-Horeca') ~
          'horeca en vrije tijd',
        groep == '65-Diensten' ~ 'diensten',
        groep %in% c('22-Mode & Luxe', '35-Vrije Tijd') ~ 'recreatief',
        groep %in% c('37-In/Om Huis', '38-Detailh Overig') ~ 'doelgericht'
      )
    )
}


my_summarise <- function(x) {
  x |>
    summarise(
      `aantal vestigingen` = sum(aantal_vestigingen, na.rm = T),
      `winkelvloeroppervlakte (m2)` = sum(winkelvloeroppervlakte, na.rm = T)
    ) |>
    pivot_longer(
      cols = c(`aantal vestigingen`, `winkelvloeroppervlakte (m2)`)
    ) |>
    filter(!is.na(dg_nd))
}

# kolom diagrammen -
my_col_figure <- function(x, aan_per, kleurenschema = blauw_pal) {
  hcl <- farver::decode_colour(kleurenschema, "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  lab_percent = scales::label_percent(
    accuracy = 1,
    big.mark = ".",
    decimal.mark = ",",
    suffix = '%'
  )

  threshold_vest <- x |>
    ungroup() |>
    filter(
      name %in% c('aantal vestigingen', 'vestigingen (%)'),
      type == aan_per
    ) |>
    slice_max(value, n = 1, with_ties = FALSE) |>
    mutate(threshold = 0.15 * value) |>
    pull()

  threshold_opp <- x |>
    ungroup() |>
    filter(
      name %in% c('winkelvloeroppervlakte (m2)', 'winkelvloeroppervlakte (%)'),
      type == aan_per
    ) |>
    slice_max(value, n = 1, with_ties = FALSE) |>
    mutate(threshold = 0.20 * value) |>
    pull()

  lab_aantal = scales::label_comma(
    big.mark = ".",
    decimal.mark = ","
  )

  x |>
    filter(type == aan_per) |>
    mutate(gbd_sdl_naam = factor(gbd_sdl_naam, levels = lev_sd)) |>
    ggplot(aes(
      x = value,
      y = fct_relevel(fct_rev(gbd_sdl_naam), "Amsterdam"),
      group = fct_rev(peildatum),
      label = if (aan_per == 'aandeel') {
        lab_percent(value)
      } else {
        lab_aantal(value)
      },
      fill = peildatum
    )) +
    geom_col(position = "dodge") +
    geom_text(
      aes(
        color = case_when(
          name %in%
            c('aantal vestigingen', 'vestigingen (%)') &
            value > threshold_vest ~ 'inside',
          name %in%
            c('aantal vestigingen', 'vestigingen (%)') &
            value <= threshold_vest ~ 'outside',

          name %in%
            c('winkelvloeroppervlakte (m2)', 'winkelvloeroppervlakte (%)') &
            value > threshold_opp ~ 'inside',
          name %in%
            c('winkelvloeroppervlakte (m2)', 'winkelvloeroppervlakte (%)') &
            value <= threshold_opp ~ 'outside'
        ),
        hjust = case_when(
          name %in%
            c('aantal vestigingen', 'vestigingen (%)') &
            value > threshold_vest ~ 1.3,
          name %in%
            c('aantal vestigingen', 'vestigingen (%)') &
            value <= threshold_vest ~ -0.3,

          name %in%
            c('winkelvloeroppervlakte (m2)', 'winkelvloeroppervlakte (%)') &
            value > threshold_opp ~ 1.3,
          name %in%
            c('winkelvloeroppervlakte (m2)', 'winkelvloeroppervlakte (%)') &
            value <= threshold_opp ~ -0.3
        )
      ),
      family = font,
      position = position_dodge(width = 0.9),
    ) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = "horizontal") +
    scale_fill_manual(name = NULL, values = blauw_pal[c(6, 3, 1)]) +
    scale_color_manual(
      name = NULL,
      values = c(inside = "white", outside = "black"),
    ) +
    scale_x_continuous(
      labels = if (aan_per == 'aandeel') {
        lab_percent
      } else {
        lab_aantal
      }
    ) +
    guides(
      fill = guide_legend(nrow = 1, reverse = F),
      colour = 'none'
    ) +
    facet_wrap(~name, scales = "free_x")
}

### functies voor kaarten
