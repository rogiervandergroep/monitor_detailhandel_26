
library(tidyverse)


data_lisa_det <- readr::read_csv("00 ruwe data/notebook_lisa_detailhandel.csv")|>
  filter(
    mradeelregio_code =='AS',
    NG_DG != 'geen winkel')|>
  pivot_longer(cols = where(is.numeric))


write.xlsx(data_lisa_det, "R 03 output/tabel_lisa_toer.xlsx")


source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")
discreet <- c("#004699","#ec0000", "#ff9100", "#d48fb9", "#ffe600", "#6cbd74", "#009dec" )

library(svglite)


data_lisa_det|>
  filter(name %in% c('aant_werkz_pers', 'aant_vestigingen'))|>
  mutate(name = case_when(
    name == "aant_werkz_pers" ~ "aantal banen",
    name == "aant_vestigingen" ~ "aantal vestigingen"))|>
  my_col_figure(
    group_var = fct_rev(fct_reorder(NG_DG, value)),
    facet_var = fct_rev(name))+
  scale_fill_manual(name= NULL, values  = discreet)
ggsave("03 output figuren/fig_lisa_det.svg", width = 12, height = 6)


data_lisa_det_tour <- bind_rows(
  
  data_lisa_det |>
    group_by(peildatum, name)|>
    summarise(value = sum(value))|>
    add_column(groep = 'detailhandel'),
  
  data_lisa_toer_tot |>
    filter(toerisme == 'toerisme')|>
    rename(groep = 'toerisme')|>
    select(peildatum, groep, name, value)
)

data_lisa_det_tour |>
  filter(name %in% c('aant_werkz_pers', 'aant_vestigingen'))|>
  mutate(name = case_when(
    name == "aant_werkz_pers"  ~ "banen",
    name == "aant_vestigingen" ~ "vestigingen"))|>
  my_line_figure(fil_var = groep)+
  facet_wrap(~ fct_rev(name), ncol = 1, scales = 'free_y')
ggsave("03 output figuren/fig_lisa_det_toer.svg", width = 12, height = 6)




write.xlsx(data_lisa_det, "R 03 output/tabel_lisa_det.xlsx")




