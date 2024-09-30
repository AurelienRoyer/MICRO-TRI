library(devtools)
setwd("C:/Users/Aurélien Admin/Desktop/MICRO-TRI-main/")
install("package")
library(microTRI)
microTRI()


detach("package:microTRI", unload = TRUE)
remove.packages("microTRI")



shinyApp(app_ui, app_server)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(reshape2)
library(tools)
library(readxl)
library(dplyr)
library(vegan)
library(iNEXT)