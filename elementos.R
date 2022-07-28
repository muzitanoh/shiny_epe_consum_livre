waiting_screen <- tagList(
  spin_flower()
)



modulosUI <- function(namespace, dados_painel, modelo_painel){

  
  select_ano <- selectInput(
    inputId = NS(namespace, "escolhe_ano"),
    label = "Ano Base:", 
    choices = sort(unique(dados_painel$ano)),
    selected = max(sort(unique(dados_painel$ano))),
    multiple = FALSE
  )
  
  
  radio_tipo <- radioGroupButtons(
    inputId = NS(namespace, "escolhe_tipo"),
    label = "Tipo do Gráfico:",
    choices = c("Absoluto", "Percentual"),
    individual = TRUE,
    checkIcon = list(
      yes = tags$i(class = "fa fa-circle", 
                   style = "color: #0C2340"),
      no = tags$i(class = "fa fa-circle-o", 
                  style = "color: #0C2340"))
  )
  
  select_setor <- selectInput(
    inputId = NS(namespace, "escolhe_setor"),
    label = "Setor:", 
    choices = sort(unique(dados_painel$setor)),
    selected = sort(unique(dados_painel$setor))[1],
    multiple = FALSE
  )
  
  slider_mes <- sliderTextInput(
    inputId = NS(namespace, "escolhe_mes"),
    label = "Mês:",
    choices = lista_meses,
    selected = c("Jan", "Dez")
  )
  

  
  if (modelo_painel == "empresas") {
    
    coluna_filtros <- verticalLayout(select_setor, select_ano, radio_tipo)
    # faixa_filtros <- flowLayout(
    #   tags$div("Filtros", class = "titulo_box"),
    #   select_setor, 
    #   select_ano,
    #   radio_tipo
    # )
  }
  else if (modelo_painel == "epe_ccee") {
  
    coluna_filtros <- verticalLayout(select_ano, slider_mes, radio_tipo)
    # faixa_filtros <- flowLayout(
    #   tags$div("Filtros", class = "titulo_box"),
    #   select_ano, 
    #   slider_mes,
    #   radio_tipo
    # )
  }
  
  
  
  painel_filtros <- wellPanel(
    tags$div("Filtros", class = "titulo_box"),
    coluna_filtros
  )
  

  grafico <- wellPanel(
    girafeOutput(NS(namespace, "grafico"))#, height = "100vh")
  )  
    
  splitLayout(
    style = "overflow:hidden;",
    cellWidths = c("25%", "75%"),
    painel_filtros,
    grafico
  )

  # tagList(
  #   wellPanel(faixa_filtros),
  #   grafico
  # )

  
  
}


modulosServer <- function(namespace, dados_painel, modelo_painel){
  
  
  moduleServer(namespace, function(input, output, session){
    
    
    ano_escolhido <- reactive({
        input$escolhe_ano
    })
    
    tipo_escolhido <- reactive({
      input$escolhe_tipo
    })

    setor_escolhido <- reactive({
      input$escolhe_setor
    })
    
    mes_escolhido <- reactive({
      input$escolhe_mes
    })
    
    
    
    
    dados_tratados <- reactive({
      
      ano_escolhido_local <- ano_escolhido()
      setor_escolhido_local <- setor_escolhido()
      mes_escolhido_local <- mes_escolhido()
      
      
      if (modelo_painel == "empresas") {
        
        dados <- dados_painel %>% 
          filter(
            ano %in% ano_escolhido_local,
            setor %in% setor_escolhido_local
          ) %>% 
          group_by(empresa) %>% 
          summarise(
            consumo = sum(consumo)
          ) %>% 
          ungroup() %>% 
          arrange(
            desc(consumo)
          ) %>% 
          mutate(
            n = row_number(),
            consumo_acumulativo = cumsum(consumo),
            percentual = consumo_acumulativo/max(consumo_acumulativo),
            percentual_ind = consumo/max(consumo_acumulativo),
            tooltip = str_glue(
              "Empresa: {empresa}
              Consumo: {numero_br(consumo, 1/1000)} (Gwh)
              Percentual: {percent(percentual, decimal.mark = ',', accuracy = 0.1)}
              Número de Empresas: {n}"
            )
          )
      }
      
      else if (modelo_painel == "epe_ccee") {
        
        
        mes_n_escolhido_local <- transf_intervalo_mes_em_numero(mes_escolhido_local)
        
        dados <- dados_painel %>% 
          filter(
            mes %in% mes_n_escolhido_local,
            ano %in% ano_escolhido_local
          ) %>% 
          group_by(
            setor
          ) %>%
          summarise(
            consumo_epe = sum(consumo_epe, na.rm = TRUE),
            consumo_ccee = sum(consumo_ccee, na.rm = TRUE)
          ) %>% 
          ungroup() %>% 
          mutate(
            consumo_percentual = consumo_epe/consumo_ccee,
            tooltip = str_glue(
              "Setor: {setor}
              Consumo EPE: {numero_br(consumo_epe, 1/1000)} (Gwh)
              Consumo CCEE: {numero_br(consumo_ccee, 1/1000)} (Gwh)
              EPE/CCEE: {percent(consumo_percentual, decimal.mark = ',', accuracy = 0.1)}"
            )
          )
      }
      
    })
    
    
    

    output$grafico <- renderGirafe({

      tipo_escolhido_local <- tipo_escolhido()
      dados_grafico <- dados_tratados()

      
      if (modelo_painel == "empresas") {
        
        if (tipo_escolhido_local ==  "Absoluto") {
          
          grafico <- grafico_consumo_por_empresa(dados_grafico, "n", "consumo_acumulativo", tipo_escolhido_local)
        } 
        else if (tipo_escolhido_local == "Percentual") {
          
          grafico <- grafico_consumo_por_empresa(dados_grafico, "n", "percentual", tipo_escolhido_local)
        }
        
      }
      
      
      else if (modelo_painel == "epe_ccee"){
        
        if (tipo_escolhido_local ==  "Absoluto") {
          
          grafico <- grafico_barras_simples(dados_grafico, "setor", "consumo_ccee", tipo_escolhido_local) +
            geom_bar_interactive(
              aes(y = consumo_epe),
              stat = "identity",
              color = "black",
              fill = paleta_epe[2]
            )
          
          
        } 
        else if (tipo_escolhido_local == "Percentual") {
          
          grafico <- grafico_barras_simples(dados_grafico, "setor", "consumo_percentual", tipo_escolhido_local)
          
        }
        
      }

      
      girafe(
        code = {print(grafico)},
        width_svg = 8,
        height_svg = 4.5,
        options = list(
          opts_selection(type = "single", css = ""),
          opts_tooltip(css = NULL, opacity = 0.9, delay_mouseover = 200, delay_mouseout = 500),
          opts_hover(css = fill_interactive_selected)
        )
      )

    })
    
    
    # Olhar depois
    # total_filtrado <- reactive({
    # 
    #   dados_filtrado <- dados_filtrado()
    # 
    #   valor <- dados_filtrado %>%
    #     summarise(valor = sum(valor)) %>%
    #     pull() %>%
    #     numero_br()
    # })
    # 
    # output$total <- renderUI(total_filtrado() %>% h2(style = str_glue("margin-top:10px;color:{paleta_anuario[1]};padding-bottom:10px;")  ))
    
    
    
    
  })
  
  
}




