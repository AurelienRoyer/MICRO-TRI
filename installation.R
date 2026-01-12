library(devtools)
# setwd("C:/Users/Aurélien Admin/Desktop/MICRO-TRI-main/") #tablette
setwd("C:/Users/aurelien/Desktop/R shiny software/TRIS/package")
setwd("C:/Users/Aurélien ROYER/Desktop/R shiny software/TRIS/") ## Dijon

## installation locale
install("package")
library(microTRI)
microTRI()

## suppression
detach("package:microTRI", unload = TRUE)
remove.packages("microTRI")


## test local sans installation
setwd("C:/Users/aurelien/Desktop/R shiny software/TRIS/")

source("package/R/PalBER_Vshiny.R")
source("package/R/incertitude.wilson.R")
source("package/R/list_bones.R")
load("package/data/list_bones.RData")

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
library(ggplot2)
library(shinyBS)
library(stringr)
library(MASS)
library(rmarkdown)

options(shiny.maxRequestSize = 10 * 1024^2) ##a mettre directement dans les files ? nécessaire si file sup à 5 MO

rm(app_server)
rm(app_ui)
source("package/R/app_server.R")
source("package/R/app_ui.R")
shinyApp(app_ui, app_server)

## installation github
devtools::install_github("AurelienRoyer/MICRO-TRI")
library(microTRI)
microTRI()


##
setwd("C:/Users/aurelien/Desktop/Gatzarria/BDD ufaune")
setwd("C:/Users/Aurélien Royer/Desktop/Combe Grenal/BDD ufaune")

setwd("D:/Combe Grenal/BDD ufaune")
setwd("D:/Ferrassie/BDD ufaune")
