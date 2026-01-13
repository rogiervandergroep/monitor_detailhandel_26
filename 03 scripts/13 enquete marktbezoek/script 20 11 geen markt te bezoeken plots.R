source("02 scr/03 script ggplot functies.R")

tabel_geen_reden_v14_basis <- read_rds(
  "01 references/tabellen_markt_geenreden.rds"
)

my_figure <- function(x) {
  x |>

    ggplot(aes(
      y = fct_relevel(fct_reorder(v14_cat, aandeel), "anders"),
      x = aandeel
    )) +

    geom_col(fill = blauw_pal[2]) +
    geom_text(
      aes(
        label = if_else(aandeel > 5, as.character(round(aandeel)), "")
      ),
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8,
      color = "white"
    ) +

    labs(title = NULL, x = NULL, y = NULL) +
    theme_os2() +
    scale_fill_manual(name = NULL, values = blauw_pal[2]) +
    guides(fill = guide_legend(nrow = 1, reverse = T)) +
    facet_wrap(~achtergrond_type, nrow = 1)
}

# redenen geen martkbezoek totaal
tabel_geen_reden_v14 <- tabel_geen_reden_v14_basis |>
  mutate(aandeel = aandeel * 100) |>
  filter(
    v14_cat %in%
      c(
        "ga uit gewoonte nooit naar de markt",
        "geen tijd",
        "te duur",
        "producten van te lage kwaliteit",
        "te weinig variatie in producten",
        "te ongezellig, te weinig sfeer"
      )
  )


tabel_geen_reden_v14 |>
  filter(
    monitor == 'monitor 2026',
    achtergrond_type != 'onbekend',
    achtergrond_naam %in% c('totaal', 'leeftijd_klas')
  ) |>
  my_figure()
ggsave(
  "04 reports/04 figuren/fig30_reden_geenmarkt_lft.svg",
  width = 12,
  height = 4
)

# "opleiding_klas",
# "inkomen_klas",
# "huishouden_klas",
# "geslacht",
# "leeftijd_klas
