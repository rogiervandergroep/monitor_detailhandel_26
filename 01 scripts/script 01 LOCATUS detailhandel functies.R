



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