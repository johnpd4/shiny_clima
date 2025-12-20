get_cor = function(var){
  
  cor = switch(var,
               "chuva" = '#1f77b4',
               "vento" = '#ff7f0e',
               "press" = '#2ca02c',
               "temp" = '#d62728',
               "umidade" = '#9467bd',
               "#7f7f7f")
  
  return(cor)
  
}

exploratorio_tab = function(){
  
  nav_panel(
    
    title = "Exploratório",
    
    layout_sidebar(
      fillable = FALSE,
      
      sidebar = sidebar(
        open = "always",
        width = "17%",
        
        uiOutput("seletor_anos_tab_1"),
        
        uiOutput("seletor_dias_tab_1"),
        
        radioButtons("variavel_exploratorio",
                     "Selecione a variável para ser mostrada: ",
                     c("Chuva" = "chuva", "Vento" = "vento", "Pressão" = "press", "Temperatura" = "temp",
                       "Umidade" = "umidade", "Rajada" = "rajada", "Amplitude" = "amp")),
        
        pickerInput("estados_selecionados",
                    "Selecione os estados para serem apresentados no mapa",
                    choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
                                "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS","SC", "SE","SP", "TO"),
                    selected = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
                                 "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS","SC", "SE","SP", "TO"),
                    multiple = T,
                    options = list(`actions-box` = TRUE,
                                   `deselect-all-text` = "Desselecionar",
                                   `select-all-text` = "Selecionar Todos",
                                   `none-selected-text` = "Nenhuma",
                                   size = 10)),
        
        uiOutput("seletor_estacoes_tab_1")
        
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
        col_widths = c(7, 5),
      
        leafletOutput("mapa_exploratorio"),
        
        card(
          
          h1("Clique em uma estação!", style = "text-align: center;"),
        
          plotlyOutput("mapas_series")
        
        )
      
      ), # layout_columns
      
      plotlyOutput("violinos_exploratorio"),
      
      plotlyOutput("medias_temporais_exploratorio"),
      
      card(
        
        layout_columns(
          col_widths = c(2, 10),
          
          card(
          
            radioButtons("variavel_exploratorio_cross",
                         "Selecione uma segunda variável para comarar: ",
                         c("Chuva" = "chuva", "Vento" = "vento", "Pressão" = "press", "Temperatura" = "temp",
                           "Umidade" = "umidade", "Rajada" = "rajada", "Amplitude" = "amp")),
            
            radioButtons("cross_cor_exploratorio",
                         "Como devem ser coloridos os pontos?",
                         c("Não colorir", "Por Região", "Por Estado")),
            
          ), # card
          
          plotlyOutput("scatter_cross_exploratorio")
          
        ) # layout_columns
        
      ), # card
      
    ), # layout_sidebar
    
  ) # nav_tab
  
}

exploratorio_server = function(input, output, session){
  
  output$seletor_anos_tab_1 = renderUI({
    
    bancos = list.files("dados_shiny")
    
    bancos = gsub("\\.parquet$", "", bancos)
    
    selectInput("ano_selecionado_tab_1", "Selecione o Ano em Questão",
                choices = bancos, selected = bancos[length(bancos)])
    
  })
  
  output$seletor_dias_tab_1 = renderUI({
    
    inicio = paste0(input$ano_selecionado_tab_1, "-1-1")
    fim = paste0(input$ano_selecionado_tab_1, "-12-31")
    
    dateInput(inputId = "dia_selecionado_tab_1", label = "Escolha a Data", value = inicio, language = "pt-BR",
              min = inicio, max = fim)
    
  })
  
  output$seletor_estacoes_tab_1 = renderUI({
    
    pickerInput("estacoes_selecionadas_tab_1", "Selecione as Estações a serem ultilizadas",
                choices = lista_estacoes_tab_1(), selected = lista_estacoes_tab_1(), multiple = T,
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Desselecionar",
                               `select-all-text` = "Selecionar Todos",
                               `none-selected-text` = "Nenhuma",
                               size = 10))
    
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
    # input = data.frame(ano_selecionado_tab_1 = "2023", dia_selecionado_tab_1 = "2023-01-01", estados_selecionados = c("RS", "SP"))
    
    dados_tab_1 = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"))
    
    # dados_tab_1 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".csv"))
    
    # Pegar soh o dia selecionado
    dados_tab_1 = dados_tab_1 |> subset(data_dia %in% input$dia_selecionado_tab_1)
    
    # Pegar soh os estados selecionados
    dados_tab_1 = dados_tab_1 |> subset(uf %in% input$estados_selecionados)
    
    # Pegar soh as estacoes selecionadas
    dados_tab_1 = dados_tab_1 |> subset(estacao %in% input$estacoes_selecionadas_tab_1)
    
    coordinates(dados_tab_1) = ~lon + lat
    
    proj4string(dados_tab_1) = CRS("+init=epsg:4674")
    
    return(dados_tab_1)
    
  })
  
  lista_estacoes_tab_1 = reactive({
    
    dados_tab_1 = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"))
    #dados_tab_1 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".csv"))
    
    # Pegar soh os estados selecionados
    dados_tab_1 = dados_tab_1 |> subset(uf %in% input$estados_selecionados)
    
    return(unique(dados_tab_1$estacao))
    
  })
  
  shape = read_state(code_state = "all")
  
  #shape = spTransform(shape, CRS("+init=epsg:4674"))
  
  shapefile = reactive({
    
    # Para testes
    # input = data.frame(estados_selecionados = c("RS", "SP"))
    
    shape_subset = shape |> subset(abbrev_state %in% input$estados_selecionados)
    
    return(shape_subset)
    
  })
  
  output$mapa_exploratorio = renderLeaflet({
    
    dados_tab_1 = dados_tab_1()
    shapefile = shapefile()
    
    coords <- coordinates(dados_tab_1)
    df <- as.data.frame(dados_tab_1)
    df$lon <- coords[,1]
    df$lat <- coords[,2]
    
    pal <- colorNumeric(palette = "Spectral", domain = df |> getElement(input$variavel_exploratorio))
    
    mapa = leaflet(df) |>
            addTiles() |> 
            addPolygons(data = shapefile, color = "black", opacity = 1, weight = 2) |>
            addCircleMarkers(~lon, ~lat, color =~ pal(df |> getElement(input$variavel_exploratorio)),
                             fillOpacity = 0.9, radius = 5, stroke = F,
                             label =~ paste0(str_to_title(df$estacao), " (",
                                             str_to_title(input$variavel_exploratorio), ") : ",
                                             df |> getElement(input$variavel_exploratorio) |> round(digits = 2)),
                             layerId =~ codigo) |>
            addLegend(pal = pal, position = "topright", values = range(df |> getElement(input$variavel_exploratorio)),
                      title = str_to_title(input$variavel_exploratorio)) |>
            setMaxBounds(-34.00, 3.47, -78.14, -34.50)
    
    mapa 
    
  })
  
  output$violinos_exploratorio = renderPlotly({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    fig = plot_ly(data = dados, y =~ chuva, name = "Chuva", type = "violin")
    
    fig1 = plot_ly(data = dados, y =~ vento, name = "Vento", type = "violin")
    
    fig2 = plot_ly(data = dados, y =~ press, name = "Pressão", type = "violin")
    
    fig3 = plot_ly(data = dados, y =~ temp, name = "Temperatura", type = "violin")
    
    fig4 = plot_ly(data = dados, y =~ umidade, name = "Umidade", type = "violin")
    
    subplot(fig, fig1, fig2, fig3, fig4, nrows = 1) |> layout(legend = list(orientation = "h", y = 1.1,
                                                                            x = 0.5, xanchor = "center"))
    
  })
  
  output$medias_temporais_exploratorio = renderPlotly({
    
    # Para testes
    # input = data.frame(ano_selecionado_tab_1 = "2023", variavel_exploratorio = "amp")
    
    dados = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"),
                         col_select = c("data_dia", input$variavel_exploratorio, "estacao"))
    
    # dados = dados |> subset(data_dia %in% input$dia_selecionado_tab_1)
    
    names(dados) = c("data", "var", "estacao")
    
    cor = get_cor(input$variavel_exploratorio)
    
    dados_summ = dados |> group_by(data) |> summarise(data = unique(data),
                                                      var = mean(var, na.rm = T))
    
    fig = plot_ly(data = dados_summ, x =~ data, y =~ var,
                  type = "scatter", mode = "lines+markers",
                  line = list(color = cor), marker = list(color = cor))
    # fig = fig |> add_trace(data = dados, x =~ data, y =~ var,
    #                        type = "scatter", mode = "markers", opacity = 0.5, line = list(width = 0))
    fig = fig |> layout(yaxis = list(title = toTitleCase(input$variavel_exploratorio)))
    fig
    
  })
  
  output$scatter_cross_exploratorio = renderPlotly({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    cor = switch(input$cross_cor_exploratorio,
                 "Não colorir" = "NULL",
                 "Por Região" = "regiao",
                 "Por Estado" = "uf")
    
    fig = plot_ly(data = dados,
                  x = as.formula(paste0("~", input$variavel_exploratorio)),
                  y = as.formula(paste0("~", input$variavel_exploratorio_cross)),
                  text =~ paste0(estacao), color = as.formula(paste0("~", cor)),
                  type = "scatter", mode = "markers")
    fig
    
  })
  
  observeEvent(input$mapa_exploratorio_marker_click, {
    
    req(input$mapa_exploratorio_marker_click)
    
    marker_id = input$mapa_exploratorio_marker_click$id
    
    output$mapas_series = renderPlotly({
      
        dados = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"))
        
        id_estacao = unlist(marker_id)[1]
        
        dados = dados |> subset(codigo == id_estacao)
        
        fig = plot_ly(data = dados, x =~ data_dia, y =~ chuva, name = "Chuva",
                      type = "scatter", mode = "lines+markers")
        
        fig1 = plot_ly(data = dados, x =~ data_dia, y =~ vento, name = "Vento",
                       type = "scatter", mode = "lines+markers")
        
        fig2 = plot_ly(data = dados, x =~ data_dia, y =~ press, name = "Pressão",
                       type = "scatter", mode = "lines+markers")
        
        fig3 = plot_ly(data = dados, x =~ data_dia, y =~ temp, name = "Temperatura",
                       type = "scatter", mode = "lines+markers")
        
        fig4 = plot_ly(data = dados, x =~ data_dia, y =~ umidade, name = "Umidade",
                       type = "scatter", mode = "lines+markers")
        
        subplot(fig, fig1, fig2, fig3, fig4, nrows = 5) |> layout(legend = list(orientation = "h", y = 1.1,
                                                                                x = 0.5, xanchor = "center"))
      
    })
    
  })
  
}