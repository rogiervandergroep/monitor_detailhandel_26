
# inlezen kolom figuur en lijnfiguur
source("01 scripts/script 00 references.R")
source("01 scripts/script 10 LISA detailhandel functies.R")


#inlezen tabel
data_lisa_det <- readr::read_csv2(
  "00 ruwe data/ruw lisa/tabel_lisa_det_hor_banen.csv"
) |>
  filter(mradeelregio_code == 'AS') |>
  pivot_longer(cols = where(is.numeric))

# figuur banen in de detailhandel
data_lisa_det |>
  filter(
    NG_DG != 'geen detailhandel of horeca',
    name %in% c('aant_werkz_pers_fulltime', 'aant_werkz_pers_parttime')
  ) |>
  mutate(
    name = case_when(
      name == "aant_werkz_pers_fulltime" ~ "banen meer dan 12 uur per week",
      name == "aant_werkz_pers_parttime" ~ "banen minder dan 12 uur per week"
    )
  ) |>
  my_col_figure(
group_var = fct_rev(name),
facet_var = fct_rev(NG_DG))
ggsave("03 tabellen/fig_lisa_det_banen.svg", width = 12, height = 4)
