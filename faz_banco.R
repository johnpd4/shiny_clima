##################################################################################################################################
##################################################################################################################################
##### Nota: para esse arquivo funcionar propiamente a pasta dados_estacoes deve ter todos os arquivos unzippados do INMET ########
##### dos anos relevantes, retirados de sua página de dados históricos.                                                   ########
##### Link: https://portal.inmet.gov.br/dadoshistoricos                                                                   ########
##################################################################################################################################
##################################################################################################################################


if(!require(pacman)){install.packages("pacman")}

pacman::p_load(lubridate, dplyr, stringr, geosphere, phylin, nanoparquet)

trunc_max = function(vet){if(all(is.na(vet))){return(0)} else{return(max(vet, na.rm = T))}}
trunc_min = function(vet){if(all(is.na(vet))){return(0)} else{return(min(vet, na.rm = T))}}

carrega_csv = function(path){
  # Funcao q le e faz todas as manipulacoes necessarias pra um arquivo
  # de dados historicos do INMET
  
  arquivo = file(path)
  
  if (file.size(path) == 0L){print(paste0("Arquivo ", path, " vazio!")); return()}
  
  # Extrair das 8 primeiras linhas as infos de cabecalho
  for (linha in readLines(arquivo)){
    
    # O base::sub() ta fazendo o trabalho que comumente seria feito por
    # stringr::str_extract pq o regex dele n funciona muito bem com ";"
    if (grepl("REGIAO", linha)){regiao = sub(".*;", "", linha)}
    if (grepl("UF", linha)){uf = sub(".*;", "", linha)}
    if (grepl("ESTACAO:", linha)){estacao = sub(".*;", "", linha)}
    if (grepl("CODIGO", linha)){codigo = sub(".*;", "", linha)}
    if (grepl("LATITUDE", linha)){lat = sub(".*;", "", linha)}
    if (grepl("LONGITUDE", linha)){lon = sub(".*;", "", linha)}
    if (grepl("ALTITUDE", linha)){altitude = sub(".*;", "", linha)}
    
    # Quando chegamos nos nomes das variaveis queremos sair do loop
    if (grepl("UTC", linha)){break}
    
  }
  
  close(arquivo)
  
  # Aqui lemos o resto do banco depois das infos extras
  banco = read.csv(path, skip = 8, sep = ";", dec = ",", header = T)
  
  # Renomear as variaveis
  banco = banco |> dplyr::rename(data_dia = Data,
                                 hora_num = Hora.UTC,
                                 temp_med = TEMPERATURA.DO.AR...BULBO.SECO..HORARIA..C.,
                                 temp_max = TEMPERATURA.MXIMA.NA.HORA.ANT...AUT...C.,
                                 temp_min = TEMPERATURA.MNIMA.NA.HORA.ANT...AUT...C.,
                                 umi_med = UMIDADE.RELATIVA.DO.AR..HORARIA....,
                                 umi_max = UMIDADE.REL..MAX..NA.HORA.ANT...AUT.....,
                                 umi_min = UMIDADE.REL..MIN..NA.HORA.ANT...AUT.....,
                                 pto_orvalho_med = TEMPERATURA.DO.PONTO.DE.ORVALHO..C.,
                                 pto_orvalho_max = TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT...C.,
                                 pto_orvalho_min = TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT...C.,
                                 press_med = PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.,
                                 press_max = PRESSO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB.,
                                 press_min = PRESSO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB.,
                                 vento_vel = VENTO..VELOCIDADE.HORARIA..m.s.,
                                 vento_dir = VENTO..DIREO.HORARIA..gr.....gr..,
                                 vento_rajada = VENTO..RAJADA.MAXIMA..m.s.,
                                 radiacao = RADIACAO.GLOBAL..Kj.m.,
                                 chuva = PRECIPITAO.TOTAL..HORRIO..mm.) |> dplyr::select(-X)

  # Criacao de variaveis de tempo
  banco$data_dia =  banco$data_dia |> as.Date(format = "%Y/%m/%d")
  banco$data_semana = paste0(banco$data_dia |> year(), "-", banco$data_dia |> week())
  banco$data_mes = paste0(banco$data_dia |> year(), "-", banco$data_dia |> month())
  banco$data_ano = banco$data_dia |> year()
  
  # Agrupa os dados por dia
  niveis = unique(banco$data_dia)
  
  # Coloca as variaveis do cabecalho na tabela
  banco$regiao = regiao
  banco$uf = uf
  banco$codigo = codigo
  banco$estacao = estacao
  banco$lat = lat |> sub(pattern = ",", replacement = ".") |> as.numeric()
  banco$lon = lon |> sub(pattern = ",", replacement = ".") |> as.numeric()
  banco$altitude = altitude |> sub(pattern = ",", replacement = ".") |> as.numeric()
  
  dados_agrupdos = data.frame()
  
  for (i in niveis){
    
    aux = banco |> subset(data_dia == i)
    
    aux = aux |> group_by(codigo) |> summarise(data_dia = unique(data_dia),
                                               data_semana = unique(data_semana),
                                               data_mes = unique(data_mes),
                                               data_ano = unique(data_ano),
                                               lon = unique(lon),
                                               lat = unique(lat),
                                               chuva = sum(chuva, na.rm = T),
                                               vento = mean(vento_vel, na.rm = T),
                                               press = mean(press_med, na.rm = T),
                                               temp = mean(temp_med, na.rm = T),
                                               umidade = mean(umi_med, na.rm = T),
                                               rajada = trunc_max(vento_rajada),
                                               amp = round(trunc_max(temp_max) - trunc_min(temp_min), 2),
                                               regiao = unique(regiao),
                                               uf = unique(uf),
                                               estacao = unique(estacao),
                                               altitude = unique(altitude))
    
    
    dados_agrupdos = rbind(dados_agrupdos, aux)
    
  }
  
  # Retorna banco bonito :)
  return(dados_agrupdos)
  
}

grp_csv_por_pasta = function(nome_pasta){
  
  # Pega tds os anos q tem pastas
  nomes_dirs = dir(nome_pasta, pattern = "[0-9]+$")
  
  # Entra em cada pasta de ano presente
  for (dir in nomes_dirs){
    
    arquivos = list.files(file.path(nome_pasta, dir))
    
    banco = data.frame()
    
    # Pega todos os arquivos nessa pasta
    for (arquivo in arquivos){
      
      # E vai dando append em um arquivo do ano com todos da pasta
      aux = carrega_csv(file.path(nome_pasta, dir, arquivo))
      
      banco = rbind(banco, aux)
      
      print(paste0("Arquivo ", arquivo, " lido"))
      
    }
    
    banco = idw_banco(banco)
    
    write.table(file = paste0("./dados_shiny/", dir, ".csv"), x = banco,
                sep = ",", row.names = F)
    
  }
  
}

#banco = carrega_csv("dados_estacoes/2020/INMET_CO_DF_A001_BRASILIA_01-01-2020_A_31-12-2020.CSV")
#grp_csv_por_pasta("dados_estacoes")

idw_calc = function(dists, valores, vizinhos){
  
  pnts_prox = data.frame(dists = dists, valores = valores)
  
  names(pnts_prox) = c("dists", "valores")
  
  # Remove o ponto em questão, reconhecido por ter distancia 0
  pnts_prox = pnts_prox |> subset(dists != 0)
  
  vizinhos = min(vizinhos, nrow(pnts_prox))
  
  # Pega os pontos mais proximos, n precisamos nos preocupar em selecionar o valor
  # de dist == 0 pois ele ja eh NA
  # pnts_prox = pnts_prox[which(!is.na(pnts_prox$valores)), ]
  
  # Ordena os pontos por menor distancia
  pnts_prox = pnts_prox[order(pnts_prox$dists), ]
  
  # Pega os n primeiros pontos prox
  pnts_prox = pnts_prox[1:vizinhos, ]
  
  print("pnts_prox")
  print(pnts_prox)
  
  # Calculo IDW
  valor_novo = sum(pnts_prox$valores / pnts_prox$dists) / sum(1 / pnts_prox$dists)
  
  print("valor_novo")
  print(valor_novo)
  
  return(valor_novo)
   
}

euclidean = function(lat1, lon1, lat2, lon2){
  
  return(sqrt((lat1 - lat2)^2 + (lon1 - lon2)^2))
  
}

aplica_idw = function(lat, lon, var, vizinhos){

  # Pegar o primeiro NA
  indx_interp = which(is.na(var))
  
  if (length(indx_interp) != 1){stop(paste0("Aplica idw recebeu um banco c numero errado de NA's: ",
                                            length(indx_interp)))}
  
  # Calcular a distancia de todos os pontos pra esse
  dists = c()
  
  for (i in 1:length(lat)){
  
    dists[i] = euclidean(lat[indx_interp], lon[indx_interp], lat[i], lon[i])
    # print(lat[i])
    # print(lon[i])
    # print(lon[indx_interp])
    # print(lon[indx_interp])
    
  }
  
  # print("=======")
  # print(sum(is.na(var)))
  # print(var[indx_interp])
  # print(idw_calc(dists = dists, valores = var, vizinhos = vizinhos))
  # print(dists)
  # print(valores)
  # print(vizinhos)
  # print(var)
  # Interpolar o valor e colocar no vetor
  
  idw = idw_calc(dists = dists, valores = var, vizinhos = vizinhos)
  print(paste0("idw: ", idw))
  
  return(idw)

}

idw_banco = function(banco, vizinhos = 5){
  
  # print(head(banco))
  
  # Pra cada coluna no banco
  for (col in names(banco)){
    
    # Achar todos os NA
    indx_na = which(is.na(banco[col]))
    print(length(indx_na))
    
    # Pula o laço se n tem NA
    if (length(indx_na) == 0){next}
    
    # Para cada NA na coluna
    for (i in 1:nrow(banco[indx_na,])){
      
      # Achar os n vizinhos mais próximos q n são NA
      dia_da_obs = banco[indx_na[i], ]$data_dia
      # print(dia_da_obs)
      
      obs_dia = banco |> subset(data_dia == dia_da_obs)
      
      obs_dia_completa = obs_dia[complete.cases(obs_dia), ]
      
      obs_com_na = rbind(obs_dia_completa, banco[indx_na[i], ])
      
      print(obs_com_na)
      
      # Resgatar o valor de uma aplica idw modificada pra n retornar colunas
      resultado_idw = aplica_idw(obs_com_na$lat, obs_com_na$lon, obs_com_na[col], vizinhos)
      print(paste0("resultado_idw: ", resultado_idw))
      
      banco[indx_na[i], col] = resultado_idw
      # print(col_sub)  
      
    }
    
  }
  
  return(banco)
  
}

# teste = data.frame(lat = rnorm(10, 48, 22), lon = rnorm(10, 48, 22), values = rnorm(10, 0, 10))
# teste[sample(1:nrow(teste), 2),3] = NA
# teste
# teste$values = aplica_idw(teste$lat, teste$lon, teste$values, 5)
# teste

dados = grp_csv_por_pasta("dados_teste")
# dados[sample(1:nrow(dados), 5), c(8, 9, 10)] = NA
# teste = idw_banco(dados)
# teste
