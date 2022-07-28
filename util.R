library(DBI)
library(janitor)
library(magrittr)
library(lubridate)
library(tidyverse)
library(stringr)
library(readxl)
library(scales)
library(dbplyr, warn.conflicts = FALSE)

theme_custom <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica",
  appFontColor = "#001F66"
  ,primaryFontColor = "rgb(15,15,15)"
  ,infoFontColor = "rgb(15,15,15)"
  ,successFontColor = "rgb(15,15,15)"
  ,warningFontColor = "rgb(15,15,15)"
  ,dangerFontColor = "rgb(15,15,15)"
  ,bodyBackColor = "rgb(240,240,240)"
  
  ### header
  ,logoBackColor = "#001F66"
  
  ,headerButtonBackColor = "#001F66"
  ,headerButtonIconColor = "rgb(220,220,220)"
  ,headerButtonBackColorHover = "rgb(100,100,100)"
  ,headerButtonIconColorHover = "rgb(60,60,60)"
  
  ,headerBackColor = "#001F66"
  ,headerBoxShadowColor = "#dfdfdf"
  ,headerBoxShadowSize = "3px 5px 5px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(255,255,255)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#dfdfdf"
  
  ,sidebarUserTextColor = "rgb(115,115,115)"
  
  ,sidebarSearchBackColor = "rgb(240,240,240)"
  ,sidebarSearchIconColor = "rgb(100,100,100)"
  ,sidebarSearchBorderColor = "rgb(220,220,220)"
  
  ,sidebarTabTextColor = "#001F66"
  ,sidebarTabTextSize = 11
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(230,230,230)"
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(245,245,245)"
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "rgb(200,200,200)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px"
  ,boxBackColor = "rgb(255, 255, 255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(255,255,255)"
  ,boxPrimaryColor = "rgb(255, 255, 255)"
  ,boxInfoColor = "rgb(180,180,180)"
  ,boxSuccessColor = "rgb(112,173,71)"
  ,boxWarningColor = "rgb(237,125,49)"
  ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 6
  ,tabBoxTabTextColor = "rgb(100,100,100)"
  ,tabBoxTabTextColorSelected = "rgb(45,45,45)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(200,200,200)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(215,215,215)"
  ,buttonTextColor = "rgb(45,45,45)"
  ,buttonBorderColor = "rgb(150,150,150)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(190,190,190)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(150,150,150)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(238,238,238)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



lista_meses <- c("Jan", "Fev", "Marc", "Abr", "Maio", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

dim_mes <- as_tibble_col(1:12, "n") %>% 
  bind_cols(
    as_tibble_col(lista_meses, "mes")
  )

transf_intervalo_mes_em_numero <- function(vetor) {
  
  mes1 <- dim_mes %>%
    filter(
      mes == vetor[1]
    ) %>%
    pull(n)
  
  mes2 <- dim_mes %>%
    filter(
      mes == vetor[2]
    ) %>%
    pull(n)
  
  mes1:mes2
  
}


size_grid <- .4

shape_ponto <-  21
size_ponto <-  6
size_ponto_interactive <- 22
stroke_ponto <-  2.5
alpha_ponto_interactive <- 0.5


paleta_epe <- c(
  "#0C2340",
  "#FFC959"
)

cor_painel <- "#f5f5f5"

remove_rotulo <- labs(
  y = "",
  x = "",
  color = "",
  fill = ""
) 

fill_interactive_selected <- str_glue("fill:{paleta_epe[1]};stroke:black;")
opts_interactive_hover <- str_glue("{fill_interactive_selected};cursor:pointer;")

tema_basico <- theme_minimal()

tema_detalhes <- theme(
  legend.position = "none",
  axis.title = element_text(size = 14),
  axis.text = element_text(
    family = "Helvetica", colour = paleta_epe[1], face = "bold", size = 10
  ),
  panel.grid.major = element_line_interactive(color = "gray", size = size_grid),
  plot.background = element_rect(
    fill = cor_painel,
    color = cor_painel
  ),
  plot.title = element_text(
    color = paleta_epe[1],
    hjust = 0,
    face = "bold",
    size = 22
  )
)




numero_br <- function(numero, scale = 1, acuracia = 1){
  
  number(numero, scale = scale, accuracy = acuracia, big.mark = ".", decimal.mark = "," , trim = TRUE)
  
}





grafico_barras_simples <- function (dados_grafico, eixo_x, eixo_y, tipo_escolhido = tipo_escolhido_local, tooltip = tooltip, data_id = eixo_x) {
 
  grafico <- ggplot(
    dados_grafico,
    aes(
      x = reorder(.data[[eixo_x]], +.data[[eixo_y]]),
      tooltip = tooltip,
      data_id = .data[[data_id]]
    )
  ) +
    geom_bar_interactive(
      aes(y = .data[[eixo_y]]),
      stat = "identity",
      color = "black",
      fill = paleta_epe[1]
    ) +
    scale_y_continuous(
      labels = function(x) number(x, big.mark = ".", scale = 1/10^3)
    ) +
    coord_flip() +
    tema_basico +
    tema_detalhes +
    remove_rotulo +
    ggtitle("Consumo por Setor (EPE-CCEE)")
  
  
  
  if (tipo_escolhido == "Percentual"){
    
    grafico <- grafico +
      scale_y_continuous(
        labels = function(x) percent(x, decimal.mark = ','),
        breaks = seq(0, 1, by = .2)
      )
  }
   return(grafico)
}


grafico_consumo_por_empresa <- function (dados_grafico, eixo_x, eixo_y, tipo_escolhido = tipo_escolhido_local, tooltip = tooltip, data_id = eixo_x) {
  
  grafico <- ggplot(
    dados_grafico, 
    aes(data_id = .data[[data_id]])
  ) +
    geom_line(
      aes(
        x = .data[[eixo_x]],
        y = .data[[eixo_y]]
      ),
      color = paleta_epe[1],
      size = 2.5,
      alpha = .8,
      show.legend = FALSE
    ) +
    geom_point_interactive(
      aes(
        x = .data[[eixo_x]],
        y = .data[[eixo_y]],
        tooltip = tooltip,
        data_id = .data[[data_id]]
      ),
      color = paleta_epe[1],
      shape = shape_ponto,
      fill = paleta_epe[1],
      size = .001,
      stroke = stroke_ponto,
      alpha = 1
    ) +
    remove_rotulo +
    tema_basico +
    tema_detalhes +
    scale_x_continuous(
      breaks = seq(1, max(dados_grafico$n), by = ceiling(max(dados_grafico$n - 1)/8))
    ) +
    ggtitle("Consumo CCEE por Empresa") +
    scale_y_continuous(
      labels = function(x) number(x, big.mark = ".", scale = 1/10^3)
    )
  
  if (tipo_escolhido == "Percentual"){
    
    grafico <- grafico +
      scale_y_continuous(
        labels = function(x) percent(x, decimal.mark = ','),
        breaks = seq(0, 1, by = .2)
      )
  }
  return(grafico)
}

























