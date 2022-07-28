## app.R ##
library(shiny)
library(tidyverse)
library(shinydashboard)
library(dashboardthemes)
library(patchwork)
library(ggiraph)
library(readxl)
library(RColorBrewer)
library(scales)
library(latex2exp)
library(colorspace)
library(reactable)
library(writexl)
library(shinydashboardPlus)
library(shinyjs)   
library(shinyWidgets)
library(janitor)
library(lubridate)
library(DBI)
library(tibble)
library(dbplyr)
library(lubridate)
library(ggrepel)
library(ggnewscale)
library(waiter)
library(scales)
library(magrittr)
library(gghighlight)
library(plotly)
library(waiter)
library(systemfonts)


#### ELEMENTOS INTERFACE ####

source("util.R", encoding = "UTF-8")
source("elementos.R", encoding = "UTF-8")
source("coleta_tratamento.R", encoding = "UTF-8")

shinyOptions(cache = cachem::cache_disk(dir = "cache"))

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              menuItem("Análise do Consumo de Empresas", tabName = "analise_empresas", startExpanded = TRUE,
                                       menuSubItem("Consumo na Rede - CCEE", tabName = "ccee_empresas"),
                                       menuSubItem("Consumo na Rede - EPE", tabName = "epe_empresas")
                              ),
                              menuItem("Comparativo do Consumo EPE-CCEE" , tabName = "epe_ccee_setor")
                            )
)


body <- dashboardBody(
  includeCSS("www/custom.css"),
  withMathJax(),
  useShinyjs(), 
  use_waiter(),
  theme_custom,
  tabItems(
    tabItem(tabName = "ccee_empresas",
      
            modulosUI(namespace = "ccee_empresas", dados_painel =  consumo_ccee, modelo_painel = "empresas")
                  
    ),
    tabItem(tabName = "epe_empresas",

            modulosUI(namespace = "epe_empresas", dados_painel =  consumo_ccee, modelo_painel = "empresas")

    ),
    tabItem(tabName = "epe_ccee_setor",
            
            modulosUI(namespace = "epe_ccee_setor", dados_painel =  comparacao_ccee_epe,  modelo_painel = "epe_ccee")
    
    )
  ),
  tags$head(
    # tags$style(HTML(".main-sidebar { font-size: 10px; }")),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )

)



ui <- dashboardPage(
  title = "Consumidores Livres",
  dashboardHeaderPlus(
    
    title = HTML('<a href="https://www.epe.gov.br/pt"> <img class=logo-epe src=logo_epe_branca.png width="70" > </a> Consum. Livres'),
    titleWidth = 270
  ),
  sidebar,
  body
)


server <- function(input, output, session) {

  modulosServer(namespace = "ccee_empresas", dados_painel =  consumo_ccee, modelo_painel = "empresas")
  # modulosServer(namespace = "epe_empresas", dados_painel = consumo_ccee, modelo_painel = "empresas")
  modulosServer(namespace = "epe_ccee_setor", dados_painel = comparacao_ccee_epe, modelo_painel = "epe_ccee")

}



shinyApp(ui, server)







   