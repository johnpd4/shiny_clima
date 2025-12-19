if(!require(pacman)){install.packages("pacman")}

pacman::p_load(shiny, leaflet, dplyr, htmltools, sp, sf, gstat, ggplot2,
               terra, shinyWidgets, plotly, rlang, leafsync, geobr, stringr,
               gridExtra, scales, magrittr, geoR, raster, nanoparquet, viridis,
               tidyr)

source("abas/NNGLS.R")
source("abas/exploratorio.R")

server = function(input, output, session){
  
  nngls_server(input, output, session)
  exploratorio_server(input, output, session)
  
}