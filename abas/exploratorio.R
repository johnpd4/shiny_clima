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

add_unidade = function(var){
  
  unidade = switch(var,
                   "chuva" = "Chuva mm",
                   "press" = "Pressão mB",
                   "temp" = "Temperatura °C",
                   "amp" = "Amplitude Térm. °C",
                   "umidade" = "Umidade %",
                   "vento" = "Vento m/s",
                   "rajada" = "Maior Rajada m/s")
  
  return(unidade)
  
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
                     "Selecione a variável que será mostrada no mapa:",
                     c("Chuva" = "chuva", "Vento" = "vento", "Pressão" = "press", "Temperatura" = "temp",
                       "Umidade" = "umidade", "Rajada" = "rajada", "Amplitude" = "amp")),
        
        pickerInput("estados_selecionados",
                    "Selecione os estados para serem apresentados no mapa:",
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
          value = textOutput("num_estacoes_tab_1"),
        ), # value_box
        
        value_box(
          title = textOutput("maior_value_box_exploratorio"),
          value = textOutput("maior_valor_exploratorio"),
          p(textOutput("estacao_maior_valor"))
        ), # value_box
        
        value_box(
          title = textOutput("menor_value_box_exploratorio"),
          value = textOutput("menor_valor_exploratorio"),
          p(textOutput("estacao_menor_valor"))
        ), # value_box
        
      ), # layout_columns
      
      layout_columns(
        height = "85%",
        col_widths = c(7, 5),
      
        leafletOutput("mapa_exploratorio"),
        
        card(
          
          h1("Clique em uma estação!", style = "text-align: center;"),
          
          div(
            textOutput("nome_estacao_series"),
            style = "width: 100%; text-align: center;"
          ),
          
          plotlyOutput("mapas_series")
        
        )
      
      ), # layout_columns
      
      card(
        
        h1("Gráficos de Violino"),
        p("Esses gráficos mostram a densidade das cinco principais variáveis no dia selecionado"),
        plotlyOutput("violinos_exploratorio"),
        
      ),
      
      card(
        
        h1("Gráficos de Médias Temporais"),
        p("Esse gráfico mostra a média entre todas as estações selecionadas da variável escolhida em todos os dias do ano"),
        plotlyOutput("medias_temporais_exploratorio"),
        
      ),
      
      card(
        
        h1("Gráfico de Pontos Cross (TODO: nome ruim)"),
        p("Selecione uma segunda variável para comparar a distribuição cruzada entre ela e a variável selecionada na barra lateral.
          Também pode-se colorir os pontos por estado ou região"),
        
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
      
      card(
        
        h1("Gráfico de Agrupamento por Estado ou Região (arrumar tamanho, colocar algo do lado?)"),
        p("Selecione um estado ou região para ver como a variável se distribui nele(a)"),
        
        layout_columns(
          col_widths = c(2, 10),
          
          card(
            
            radioButtons("tipo_agrupamento_shapefile_exploratorio",
                         "Agrupar Regiões ou Estados?",
                         c("Por Região", "Por Estado"))
            
          ), # card
          
          leafletOutput("agrupamento_shapefile_exploratorio")
          
        ) # layout_columns
        
      ), # card
      
    ), # layout_sidebar
    
  ) # nav_tab
  
}

exploratorio_server = function(input, output, session){
  
  output$seletor_anos_tab_1 = renderUI({
    
    bancos = list.files("dados_shiny")
    
    bancos = gsub("\\.parquet$", "", bancos)
    
    selectInput("ano_selecionado_tab_1", "Selecione o ano para ser analisado:",
                choices = bancos, selected = bancos[length(bancos)])
    
  })
  
  output$seletor_dias_tab_1 = renderUI({
    
    inicio = paste0(input$ano_selecionado_tab_1, "-1-1")
    fim = paste0(input$ano_selecionado_tab_1, "-12-31")
    
    dateInput(inputId = "dia_selecionado_tab_1", label = "Escolha a data para ser mostrada no mapa:",
              value = inicio, language = "pt-BR", min = inicio, max = fim)
    
  })
  
  output$seletor_estacoes_tab_1 = renderUI({
    
    pickerInput("estacoes_selecionadas_tab_1", "Selecione as estações a serem mostradas no mapa:",
                choices = lista_estacoes_tab_1(), selected = lista_estacoes_tab_1(), multiple = T,
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Desselecionar",
                               `select-all-text` = "Selecionar Todos",
                               `none-selected-text` = "Nenhuma",
                               size = 10))
    
  })
  
  output$ano_exploratorio_tab_1 = renderText({
    
    return(input$ano_selecionado_tab_1)
    
  })
  
  variavel_exploratorio_com_unidade = reactive({
    
    unidade = switch(input$variavel_exploratorio,
                     "chuva" = "Chuva mm",
                     "press" = "Pressão mB",
                     "temp" = "Temperatura °C",
                     "amp" = "Amplitude Térm. °C",
                     "umidade" = "Umidade %",
                     "vento" = "Vento m/s",
                     "rajada" = "Maior Rajada m/s")
    
    return(unidade)
    
  })
  
  variavel_exploratorio_unidade = reactive({
    
    unidade = switch(input$variavel_exploratorio,
                     "chuva" = "mm",
                     "press" = "mB",
                     "temp" = "°C",
                     "amp" = "°C",
                     "umidade" = "%",
                     "vento" = "m/s",
                     "rajada" = "m/s")
    
    return(unidade)
    
  })
  
  output$maior_value_box_exploratorio = renderText({
    
    paste0("Maior valor da variável ", variavel_exploratorio_com_unidade())
    
  })
  
  output$menor_value_box_exploratorio = renderText({
    
    paste0("Menor valor da variável ", variavel_exploratorio_com_unidade())
    
  })
  
  output$maior_valor_exploratorio = renderText({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    valor = dados |> getElement(input$variavel_exploratorio) |> max() |> getElement(1)
    
    return(paste0(valor, variavel_exploratorio_unidade()))
    
  })
  
  output$menor_valor_exploratorio = renderText({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    return(dados |> getElement(input$variavel_exploratorio) |> min() |> getElement(1))
    
  })
  
  output$estacao_maior_valor = renderText({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    maior_estacao = dados[dados |> getElement(input$variavel_exploratorio) ==
                            dados |> getElement(input$variavel_exploratorio) |> max(),]
    
    paste0("A estação com maior valor foi ", maior_estacao$estacao |> getElement(1), " (", maior_estacao$uf |> getElement(1), ")")
    
  })
  
  output$estacao_menor_valor = renderText({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    menor_estacao = dados[dados |> getElement(input$variavel_exploratorio) ==
                            dados |> getElement(input$variavel_exploratorio) |> min(),]
    
    paste0("A estação com maior valor foi ", menor_estacao$estacao |> getElement(1), " (", menor_estacao$uf |> getElement(1), ")")
    
  })
  
  output$num_estacoes_tab_1 = renderText({
    
    dados_tab_1 = dados_tab_1()
    
    return(dados_tab_1$codigo |> unique() |> length())
    
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
                                             add_unidade(input$variavel_exploratorio), ") : ",
                                             df |> getElement(input$variavel_exploratorio) |> round(digits = 2)),
                             layerId =~ codigo) |>
            addLegend(pal = pal, position = "topright", values = range(df |> getElement(input$variavel_exploratorio)),
                      title = str_to_title(input$variavel_exploratorio)) |>
            setMaxBounds(-34.00, 3.47, -78.14, -34.50)
    
    mapa 
    
  })
  
  output$agrupamento_shapefile_exploratorio = renderLeaflet({
    
    # Para testes
    'input = data.frame(ano_selecionado_tab_1 = "2023", variavel_exploratorio = "chuva",
    tipo_agrupamento_shapefile_exploratorio = "Por Região", dia_selecionado_tab_1 = "2023-01-01")'
  
    if (input$tipo_agrupamento_shapefile_exploratorio == "Por Região"){
      
      shape = read_region()
      agrupamento = "regiao"
      
      
    } else if (input$tipo_agrupamento_shapefile_exploratorio == "Por Estado"){
      
      shape = read_state("all")
      agrupamento = "uf"
      
    }
    
    dados = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"),
                         col_select = c("data_dia", input$variavel_exploratorio, agrupamento))
    
    dados = dados |> subset(data_dia %in% input$dia_selecionado_tab_1)
    
    names(dados) = c("data", "var", "agrupamento")
    
    dados = dados |> group_by(agrupamento) |> summarise(data = unique(data),
                                                        var = mean(var),
                                                        agrupamento = unique(agrupamento))
    
    if (input$tipo_agrupamento_shapefile_exploratorio == "Por Região"){
      
      dados$agrupamento[dados$agrupamento == "N"] = "Norte"
      dados$agrupamento[dados$agrupamento == "NE"] = "Nordeste"
      dados$agrupamento[dados$agrupamento == "SE"] = "Sudeste"
      dados$agrupamento[dados$agrupamento == "S"] = "Sul"
      dados$agrupamento[dados$agrupamento == "CO"] = "Centro Oeste"
      
      shape = left_join(shape, dados, by = c("name_region" = "agrupamento"))
      names(shape) = c("code_region", "nome_regiao", "data", "var", "geom")
      
    } else {
      
      shape = left_join(shape, dados, by = c("abbrev_state" = "agrupamento"))
      names(shape) = c("code_state", "abbrev_state", "nome_regiao", "code_region",
                       "name_region", "data", "var", "geom")
      
    }
    
    pal <- colorNumeric(palette = "Spectral", domain = dados$var)
    
    mapa = leaflet(dados) |>
      addTiles() |> 
      addPolygons(data = shape, color = "#000000", fillColor =~ pal(var),
                  label =~ paste0("Valor da variável ", input$variavel_exploratorio, " em ",
                                  nome_regiao, ": ", var |> round(digits = 2)),
                  opacity = 1, weight = 2, fillOpacity = 1) |>
      addLegend(pal = pal, position = "topright", values = range(dados$var),
                title = str_to_title(input$variavel_exploratorio)) |>
      setMaxBounds(-34.00, 3.47, -78.14, -34.50)
    
    mapa
    
  })
  
  output$violinos_exploratorio = renderPlotly({
    
    dados = dados_tab_1()
    
    dados = dados@data
    
    fig = plot_ly(data = dados, y =~ chuva, name = add_unidade("chuva"), type = "violin", text =~ estacao)
    
    fig1 = plot_ly(data = dados, y =~ vento, name = add_unidade("vento"), type = "violin", text =~ estacao)
    
    fig2 = plot_ly(data = dados, y =~ press, name = add_unidade("press"), type = "violin", text =~ estacao)
    
    fig3 = plot_ly(data = dados, y =~ temp, name = add_unidade("temp"), type = "violin", text =~ estacao)
    
    fig4 = plot_ly(data = dados, y =~ umidade, name = add_unidade("umidade"), type = "violin", text =~ estacao)
    
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
    fig = fig |> layout(yaxis = list(title = add_unidade(input$variavel_exploratorio)))
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
    fig = fig |> layout(xaxis = list(title = add_unidade(input$variavel_exploratorio)),
                        yaxis = list(title = add_unidade(input$variavel_exploratorio_cross)))
    fig
    
  })
  
  observeEvent(input$mapa_exploratorio_marker_click, {
    
    req(input$mapa_exploratorio_marker_click)
    
    marker_id = input$mapa_exploratorio_marker_click$id
    
    output$nome_estacao_series = renderText({
      
      dados = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"))
      
      id_estacao = unlist(marker_id)[1]
      
      dados = dados |> subset(codigo == id_estacao)
      
      return(paste0("Gráficos de temporais do ano inteiro da estação ", dados$estacao[1]))
      
    })
    
    output$mapas_series = renderPlotly({
      
        dados = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_1, ".parquet"))
        
        id_estacao = unlist(marker_id)[1]
        
        dados = dados |> subset(codigo == id_estacao)
        
        fig = plot_ly(data = dados, x =~ data_dia, y =~ chuva, name = add_unidade("chuva"),
                      type = "scatter", mode = "lines+markers")
        
        fig1 = plot_ly(data = dados, x =~ data_dia, y =~ vento, name = add_unidade("vento"),
                       type = "scatter", mode = "lines+markers")
        
        fig2 = plot_ly(data = dados, x =~ data_dia, y =~ press, name = add_unidade("press"),
                       type = "scatter", mode = "lines+markers")
        
        fig3 = plot_ly(data = dados, x =~ data_dia, y =~ temp, name = add_unidade("temp"),
                       type = "scatter", mode = "lines+markers")
        
        fig4 = plot_ly(data = dados, x =~ data_dia, y =~ umidade, name = add_unidade("umidade"),
                       type = "scatter", mode = "lines+markers")
        
        subplot(fig, fig1, fig2, fig3, fig4, nrows = 5) |> layout(legend = list(orientation = "h", y = 1.1,
                                                                                x = 0.5, xanchor = "center"))
      
    })
    
  })
  
}