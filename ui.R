if(!require(pacman)){install.packages("pacman")}

pacman::p_load(shiny, bslib, leaflet, shinyWidgets, plotly)

source("abas/NNGLS.R")
source("abas/exploratorio.R")

ui = page_navbar(
  title = "Metereologia Brasil",
  #bootstrap = 5,
  theme = bs_theme(preset = "pulse", version = 5),
  
  tags$style(HTML(".bootstrap-select .dropdown-menu li a span.text {
                  white-space: normal !important;
                  word-wrap: break-word !important;
                  white-space: pre-wrap !important;}")),
  
  # Em abas/exploratorio.R
  exploratorio_tab(),
  
  # Em abas/NNGLS.R
  nngls_tab(),
  
) # page_navbar
