exploratorio_tab = function(){
  
  nav_panel(
    
    title = "Exploratório",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "17%",
        
        uiOutput("seletor_anos_tab_1"),
        
        radioButtons("variavel_exploratorio",
                     "Selecione a variável para ser mostrada: ",
                     c("Chuva" = "chuva", "Vento" = "vento", "Pressão" = "press", "Temperatura" = "temp",
                       "Umidade" = "umidade", "Rajada" = "rajada", "Amplitude" = "amp")),
        
        uiOutput("seletor_estacoes_tab_1"),
        
      ), # sidebar
    
      layout_columns(
        height = "15%",
        value_box(
          title = "Ano",
          value = textOutput("ano_exploratorio_tab_1")
        ), # value_box
        
        value_box(
          title = "Número de Estações",
          value = textOutput("num_estacoes_tab_1")
        ), # value_box
        
        value_box(
          title = "Alguma Coisa",
          value = 5
        ), # value_box
        
        value_box(
          title = "Outra Coisa",
          value = 2
        ), # value_box
        
      ), # layout_columns
      
      layout_columns(
        height = "85%",
        col_widths = c(9, 3),
      
        leafletOutput("mapa_exploratorio"),
        
        plotlyOutput("boxplot_exploratorio")
      
      ) # layout_columns
    
    ) # layout_sidebar
    
  ) # nav_tab
  
}

exploratorio_server = function(input, output, session){
  
  output$seletor_anos_tab_1 = renderUI({
    
    bancos = list.files("dados_shiny")
    
    bancos = gsub("\\.csv$", "", bancos)
    
    selectInput("ano_selecionado_tab_1", "Selecione o Ano em Questão",
                choices = bancos, selected = bancos[length(bancos)])
    
  })
  
  output$seletor_estacoes_tab_1 = renderUI({
    
    pickerInput("estacoes_selecionadas_tab_1", "Selecione as Estações a serem ultilizadas",
                choices = lista_estacoes_tab_1(), selected = lista_estacoes_tab_1(), multiple = T,
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Desselecionar",
                               `select-all-text` = "Selecionar Todos",
                               `none-selected-text` = "Nenhuma"))
    
  })
  
  output$ano_exploratorio_tab_1 = renderText({
    
    input$ano_selecionado_tab_1
    
  })
  
  output$num_estacoes_tab_1 = renderText({
    
    dados_tab_1 = dados_tab_1()
    
    dados_tab_1$codigo |> unique() |> length()
    
  })
  
  dados_tab_1 = reactive({
    
    # Para testes:
    # input = data.frame(ano_selecionado_tab_1 = "2023")
    
    dados_tab_1 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".csv"))
    
    dados_tab_1 = dados_tab_1 |> subset(estacao %in% input$estacoes_selecionadas_tab_1)
    
    coordinates(dados_tab_1) = ~lon + lat
    
    proj4string(dados_tab_1) = CRS("+init=epsg:4674")
    
    return(dados_tab_1)
    
  })
  
  lista_estacoes_tab_1 = reactive({
    
    dados_tab_1 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".csv"))
    return(unique(dados_tab_1$estacao))
    
  })
  
  output$boxplot_exploratorio = renderPlotly({
    
    dados_tab_1 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".csv"))
    
    plot_ly(data = dados_tab_1, type = "box", y =~ dados_tab_1 |> getElement(input$variavel_exploratorio))
    
  })
  
  output$mapa_exploratorio = renderLeaflet({
    
    dados_tab_1 = dados_tab_1()
    
    coords <- coordinates(dados_tab_1)
    df <- as.data.frame(dados_tab_1)
    df$lon <- coords[,1]
    df$lat <- coords[,2]
    
    # Create color palette based on 'chuva'
    pal <- colorNumeric(palette = "Spectral", domain = df |> getElement(input$variavel_exploratorio))
    
    mapa = leaflet(df) |>
      addTiles() |> 
      addCircleMarkers(~lon, ~lat, color =~ pal(df |> getElement(input$variavel_exploratorio))) |>
      setMaxBounds(-34.00, 3.47, -78.14, -34.50)
    
    mapa 
    
  })
  
}