

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

discreet <- c("#004699","#ec0000", "#ff9100", "#d48fb9", "#ffe600", "#6cbd74", "#009dec" )

theme_os2 <- function(orientation="horizontal", legend_position = "right"){
  
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  #grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))
  
  font <- "Amsterdam Sans"
  
  
  
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = font, size = 12),
      axis.text = ggplot2::element_text(family = font, size = 12),
      plot.caption = ggplot2::element_text(family = font, size = 12),
      axis.title = ggplot2::element_text(family = font, hjust = 1, size = 12),
      plot.subtitle = ggplot2::element_text(family = font, size = 12),
      legend.text = ggplot2::element_text(family = font, size = 12),
      plot.title = ggplot2::element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.title=element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position=legend_position,
      panel.border = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.text = ggplot2::element_text(color = "black", family = font, face = "bold", size = 12)
    ) 
  
  if (orientation %in% c("vertical", "v")){
    theme <- theme + ggplot2::theme(panel.grid.major.x = element_blank())
  } else if (orientation %in% c("horizontal", "h")){
    theme <- theme + ggplot2::theme(panel.grid.major.y = element_blank())
  }
  
}

theme_os3 <- function (legenda_pos) {
  
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  font <- "Amsterdam Sans"
  
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


wild_pal  <- c("#004699","#009de6", "#53b361", "#bed200", "#ffe600", "#ff9100", "#ec0000")

discreet9 <- c("#e6e6e6","#FFD4E2", "#EC0000","#FFC88E", "#d48fb9", "#F6F6D4", "#BED200", "#D6ECD6", 
               "#53B361","#949CCC", "#004699","#ff9100", "#ffe600", "#009dec", "#a2bad3")

discreet  <- c("#004699","#ec0000", "#ff9100", "#d48fb9", "#ffe600", "#6cbd74", "#009dec" )




my_col_figure  <- function(
    x, y_var = value, group_var, positie = "stack", facet_var ) {
  
  x |>
    mutate(
      peildatum = lubridate::as_date(peildatum)
    )|>
    
    ggplot(aes(
      x = lubridate:: year(peildatum), 
      y = {{ y_var }},
      group = fct_rev( {{ group_var }} ), 
      fill  = fct_rev( {{ group_var }} ))
    )+
    geom_col(position = positie)+
    labs(title = NULL , x = NULL, y = NULL) +
    theme_os2(orientation = "vertical", legend_position = "right")+ 
    scale_fill_manual(name= NULL, values  = discreet9) +
    scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
    scale_x_continuous(limits = c(2013, 2025), breaks = c(2014, 2016, 2018, 2020, 2022, 2024))+
    guides(fill = guide_legend(reverse = F, ncol = 1))+
    facet_wrap(vars( {{ facet_var }}), ncol = 1, scales = 'free_y' )
  
}

my_col_figure_2  <- function(
    x, x_var = value, y_var, group_var, positie = "stack") {
  
  x |>
    
    ggplot(aes(
      y = fct_reorder( {{ y_var }} , groei),
      x =  {{ x_var }},
      group = fct_rev( {{ group_var }} ), 
      fill  = fct_rev( {{ group_var }} ))
    )+
    geom_col(position = positie)+
    labs(title = NULL , x = NULL, y = NULL) +
    theme_os2(orientation="vertical", legend_position = "bottom")+ 
    scale_fill_manual(name= NULL, values  = wild_pal[c(1,2,3,4,5)]) +
    scale_x_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
    
    guides(fill = guide_legend(reverse = T, ncol = 2))
}

my_line_figure <- function(x, y_val = value, fil_var) {
  
  x |>
    
    ggplot(aes(
      x = lubridate:: year(peildatum),
      y = {{ y_val }},
      group  = {{ fil_var }},
      colour = {{ fil_var }})
    )+
    
    geom_line(linewidth = 0.9)+
    labs(y = NULL, x = NULL) +
    theme_os2(orientation = "vertical", legend_position = 'right')+
    scale_color_manual(name= NULL, values = discreet)+
    scale_linewidth_manual(name = NULL, values = c(0.8, 1,2))+
    guides(colour = guide_legend(ncol = 1, reverse = T), linewidth = 'none')+
    scale_x_continuous(limits = c(2014, 2024), breaks = c(2014, 2016, 2018, 2020, 2022, 2024))+
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma(big.mark = ".", decimal.mark = ","))
} 





