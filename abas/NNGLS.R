nngls_tab = function(){
  nav_panel(
  
    title = "NNGLS",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "17%",
        
        # Tempo por ano
        uiOutput("seletor_anos_tab_2"),
        
        # Tipo de marcador
        selectInput("marcador_mapa",
                    label = "Selecione o tipo de marcador no Mapa",
                    choices = c("Nenhum" = "nenhum", "Simples" = "simples", "Colorido" = "colorido"),
                    selected = "colorido"),
        # TODO: Estações?
        selectInput(inputId = "angulo",
                    label = "Ângulo Escolhido do Variograma",
                    choices = c(0, 45, 90, 135),
                    selected = 45),
        
        sliderInput(inputId = "psill",
                    label = "Partial Sill do Variograma",
                    value = 100, min = 0, max = 1000),
        
        sliderInput(inputId = "range",
                    label = "Range do Variograma",
                    value = 100, min = 0, max = 1000),
        
        sliderInput(inputId = "nugget",
                    label = "Tamanho do efeito Pepita",
                    value = 0, min = 0, max = 1, step = 0.1),
        
        selectInput(inputId = "modelo_variograma",
                    label = "Tipo de modelo do variograma",
                    choices = c("Exp", "Sph", "Gau", "Mat"),
                    selected = c("Exp", "Sph", "Gau", "Mat"),
                    multiple = T)
        
      ), # sidebar
    
      layout_columns(
        col_widths = c(9, 3),
        
        card(
          
          layout_columns(
            
            card(
              h1("Krigagem Clássica"),
              
              leafletOutput("mapa_krig_r"),
              
              h2("Atualizar o mapa a cima pra krigagem, e colocar em baixo o gráfico de EQM?")
              
            ),
            
            card(
              
              h1("Krigagem NNGLS"),
              
              h2("Gráfico da krigagem NNGLS"),
              
              h2("Gráfico do EQM da NNGLS")
              
            ), # card
            
          ), # layout_columns
          
        ), # layout_columns
        
        card(
          value_box("Vbox1", 39, theme = "purple",
                    h2("EQM da Krig Clássica")
          ),
          
          value_box("Vbox3", 34, theme = "pink",
                    h2("EQM da NNGLS")
          ),
          
          value_box("Vbox2", 23, theme = "teal",
                    h2("Diferença entre os eqms (mudar de cor dependendo de qual for menor)")
          ),
          
        ) # card
        
      ) # layout_columns
      
    ) # layout_sidebar
    
  ) # navpanel
  
}

nngls_server = function(input, output, session){
  
  output$seletor_anos_tab_2 = renderUI({
    
    bancos = list.files("dados_shiny")
    
    bancos = gsub("\\.csv$", "", bancos)
    
    selectInput("ano_selecionado_tab_2", "Selecione o Ano em Questão",
                choices = bancos, selected = bancos[1])
    
  })
  
  dados_tab_2 = reactive({
    
    dados_tab_2 = read.csv(paste0("./dados_shiny/", input$ano_selecionado_tab_2, ".csv"))
    
    coordinates(dados_tab_2) = ~lon + lat
    
    proj4string(dados_tab_2) = CRS("+init=epsg:4674")
    
    return(dados_tab_2)
    
  })
  
  output$mapa_krig_r = renderLeaflet({
    
    dados_tab_2 = dados_tab_2()
    
    coords <- coordinates(dados_tab_2)
    df <- as.data.frame(dados_tab_2)
    df$lon <- coords[,1]
    df$lat <- coords[,2]
    
    # Create color palette based on 'chuva'
    pal <- colorNumeric(palette = "BuGn", domain = df$chuva)
    
    mapa = leaflet(df) |>
      addTiles() |> 
      addCircleMarkers(~lon, ~lat, color =~ pal(chuva)) |>
      setMaxBounds(-34.00, 3.47, -78.14, -34.50)
    
    mapa 
    
  })
  
}