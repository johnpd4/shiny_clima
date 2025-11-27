nngls_tab = function(){
  nav_panel(
  
    title = "NNGLS",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "17%",
        
        # Tempo por ano
        uiOutput("seletor_anos_tab_2"),
        
        uiOutput("seletor_dias_tab_2"),
        
        # Tipo de marcador
        selectInput("marcador_mapa",
                    label = "Selecione o tipo de marcador no Mapa",
                    choices = c("Nenhum" = "nenhum", "Simples" = "simples", "Colorido" = "colorido"),
                    selected = "colorido"),
        
        # TODO: Estações?
        selectInput(inputId = "angulo",
                    label = "Ângulo Escolhido do Variograma",
                    choices = c("0", "45", "90", "135"),
                    selected = "0")
        
      ), # sidebar
    
      layout_columns(
        col_widths = c(9, 3),
        
        card(
          
          layout_columns(
            
            card(
              h1("Krigagem geor"),
              
              plotOutput("mapa_krig"),
              
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
    
    bancos = gsub("\\.parquet$", "", bancos)
    
    selectInput("ano_selecionado_tab_2", "Selecione o Ano em Questão",
                choices = bancos, selected = bancos[1])
    
  })
  
  output$seletor_dias_tab_2 = renderUI({
    
    inicio = paste0(input$ano_selecionado_tab_2, "-1-1")
    fim = paste0(input$ano_selecionado_tab_2, "-12-31")
    
    dateInput(inputId = "dia_selecionado_tab_2", label = "Escolha a Data", value = inicio, language = "pt-BR",
              min = inicio, max = fim)
    
  })
  
  dados_tab_2 = reactive({
    
    # Para testes:
    input = data.frame(ano_selecionado_tab_2 = "2023", dia_selecionado_tab_2 = "2023-01-01")
    
    dados_tab_2 = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_2, ".parquet"))
    
    dados_tab_2 = dados_tab_2 |> subset(data_dia %in% input$dia_selecionado_tab_2)
    
    coordinates(dados_tab_2) = ~lon + lat
    
    proj4string(dados_tab_2) = CRS("+init=epsg:4674")
    
    return(dados_tab_2)
    
  })
  
  output$mapa_krig = renderPlot({
  
    grade = readRDS("./grade.rds")
    
    aux = dados_tab_2()
    
    aux = read_parquet(paste0("./dados_shiny/", input$ano_selecionado_tab_2, ".parquet"))
    
    aux = aux |> subset(data_dia %in% input$dia_selecionado_tab_2)
    
    geo <- as.geodata(aux, coords.col = c("lon", "lat"), data.col = "chuva")
    # Colocar covars dps
    #, covar.col = c("temp","amp","press"))
    v <- variog(geo)
  
    ini <- c(sigma2 = var(geo$data), phi = 0.5 * max(dist(geo$coords)))
  
    fit <- likfit(geodata = geo, ini.cov.pars = ini, nugget = 0.1 * ini["sigma2"], cov.model = "matern",
                  kappa = 0.5, method = "ML") # add covar dps: trend = ~ temp + amp + press,
    
    v = switch(input$angulo,
               "0" = variog(geo, direction = 0),
               "45" = variog(geo, direction = pi/4),
               "90" = variog(geo, direction = pi/2),
               "135" = variog(geo, direction = 3*pi/4))
    
    modelo = variofit(v, ini.cov.pars = fit, cov.model = "matern")
    
    coords_df <- as.data.frame(coordinates(grade))
    
    coords <- coordinates(grade)

    grade_pix <- SpatialPixels(grade)
    
    
    krigagem_geo = krige.conv(geo, locations = coords_df,
                              krige = krige.control(obj.model = modelo))
    
    grade_pred <- SpatialPixelsDataFrame(
      grade_pix,
      data = data.frame(pred = krigagem_geo$predict))
    
    spplot(grade_pred, "pred",
           main = "Krigagem",
           col.regions = viridis::viridis(100))
      
  
  })
  
}