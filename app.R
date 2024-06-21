### trace encoche + trace patine+infosuppleanat +pb obs suppl qui reste
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
setwd("C:/Users/aurelien/Desktop/R shiny software/TRIS")

source("PalBER.R")
source("incertitude.wilson.R")
load("data_species_biozone.RData")
load("bioclimatic_spectra_and_climate.RData")



load(file = "list_faune.RData")
list_species_all<-c("not_selected",list_faune$Taxon)
list_perso_rod<-c("not_selected","pm","am","Microtus_sp.","Microtus_arvalis","Microtus_agrestis",
              "Arvicola_sp.","Arvicola_amphibius","Lasiopodomys_gregalis","Apodemus_sp.")
list_perso_euli<-c("not_selected","Talpa_sp.","Erinaceus_europaeus","Crocidura_sp.","Sorex_sp.")
list_perso_chiro<-c("not_selected","Pipistrellus_pipistrellus")
list_perso_herpeto<-c("not_selected","Salamandra_salamandra")
list_perso_others<-c("not_selected","small Passeriforme", "Large Birds","Cervid")
list_species_rod<-c("list_perso_rod","list_species_all")
list_species_euli<-c("list_perso_euli","list_species_all")
list_species_chiro<-c("list_perso_chiro","list_species_all")
list_species_herpeto<-c("list_perso_herpeto","list_species_all")
list_species_others<-c("list_perso_others","list_species_all")

list_info_suppl<-c("T6","T9","supplementary triangle","Rhombe pitymien")


list_bone_1<-c("not_selected","m1inf","Mol","Mand","Max","Hum","Fem","Rad","Ulna","Tin",
             "Iinf","Isup","Bassin")
list_bone<-c("list_bone_1")
jsc <- '
$(document).ready(function () {
  $(".sidebar-menu").children("li").on("click", function() {
    $("#mult, #single").toggle();
  });
});12345
'

sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs",
        menuItem("Dashboard database", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Acquisition", icon = icon("list"), tabName = "SMALLvert"
                 # ,
                 # badgeLabel = "new"
                 # , badgeColor = "green"
                 ),
        menuItem("Exploration", tabName = "data2", icon = icon("chart-line")),
        div(id = "single", style="display: none;", 
            actionButton(inputId = "refresh",label="refresh"),
             uiOutput("liste.sector"),
             uiOutput("liste.UAS"),
             uiOutput("liste.passe"),
             uiOutput("liste.square"),
            uiOutput("liste.year"),
            ),
        menuItem("Note", tabName = "data3", icon = icon("comment"))
    )
)

body <- dashboardBody(
  tags$head(tags$script(jsc)),
  useShinyjs(),
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Microvertebrate Toughscreen R Interface"),
                tabsetPanel(
                  id="tab1",
                    tabPanel(h4("Loading Database"),
                             fileInput("file1", "Choose File (.rds)",
                                         accept = c(".rds")),
                               actionButton(inputId = "getData",label="Get Data"),
                             
                    ),#end of tabpanel
                tabPanel(h4("New Database"),
                         textInput("name_site", label="Name of  the site", value = "", width = NULL,
                                   placeholder = NULL),
                         uiOutput ("liste.faun4"),
                         uiOutput ("liste.faun4.eulipo"),
                         uiOutput ("liste.faun4.chiro"),
                         uiOutput ("liste.faun4.herpeto"),
                         uiOutput ("liste.faun4.others"),
                         actionButton("create_bdd", "Create the BDD"),
                         
                ),#end of tabpanel
                tabPanel(h4("Load a field Database"),
                         br(),
                         fileInput("file.fieldBDD", "Choose File (.csv/.xls/.xlsx)",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values",
                                              ".csv",
                                              ".xlsx",".xls")),
                         selectInput(inputId = "worksheet", label="Worksheet Name", choices =''),
                         actionButton(inputId = "getData.fieldBDD",label="Get Data"),
                         # actionButton('reset.BDD', 'Reset Input'),
                         br(),
                         uiOutput("set.ID.dec"),
                         uiOutput("set.square"), 
                         uiOutput("set.dec"),
                         uiOutput("set.levels") ,
                         uiOutput("set.year"), 
                         uiOutput("set.sector") 
                         
                         
                ),#end of tabpanel
                tabPanel(h4("About"),
                                               br(),
                         HTML(
                           paste0(" <div style=width:100%;, align=left>
    <font size=3>
   <span style='text-transform:none'>
   
   <i>Micro-TRI </i> (v.1) is an application dedicated to the record of small vertebrate archaeological and palaeontological remains .</p>
   <p>It makes it possible to record easily small vertebrate remains with taxonomical and taphonomical information,
   ssociated with field data information. It makes also possible to visualise quick information recorded by levels, square or spit.</p>
   <p>This is an open and free software, 
   <ul>
      <li> its source code is published on a <a href=https://github.com/AurelienRoyer/MICRO-TRI/ target=_blank>github repository</a>.</li>
    </ul>
    </p>
    <br>
   
    </span> 
    </font>
                                  </p> </div> " ))
                         
                         ) #end of tabpanel
                )#end of tabsetpanel
        ), # end of  tabItem
        
        tabItem(tabName = "SMALLvert",
                tabsetPanel(
                  id="maintab",
                  tabPanel( h4("Record Interface"),
                           fluidRow(
                             
                  fluidRow(
                    id = "mytitle",
                    div(
                  box(width = 6,background = "blue", 
                      # title="aaa",
                      # solidHeader = TRUE,
                      # collapsible = TRUE,
                actionButton("submit", "New bag"),
                actionButton("submit2", "New line"),
                  ),# end of box  
                box(width = 6,background = "olive",
                    uiOutput ("site")
                )
                ), # end of fluidRow
                             ),   # end of   div       
                # tags$script(HTML(
                #   "
                #     $(window).scroll(function() {
                #         var height = $(window).scrollTop();
                #         var el = $('#mytitle');
                #         if(height  > 50) {
                #             el.addClass('fix-top');
                #         } else {
                #              el.removeClass('fix-top');
                #         }   
                #     });
                #     "
                # )),
                # tags$style(
                #   "
                #      .fix-top {
                #         position: fixed;
                #         height: 80px;
                #         width: 100%;
                #         background-color: #ecf0f5;
                #         top: 0;
                #      }
                #     "
                # ),
                # div(style = "width: 100%; height: 90vh",
                # div(   
                # hr(),
                fluidRow(
                  box(width=12,
                  title="Quick data table",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  box(width=1,
                             br(),
               
                             br(),
                         actionButton("goButton1", "line1", icon = icon("fa-solid fa-plus")),
                         br(),
                         actionButton("goButton2", "line2", icon = icon("fa-solid fa-plus")),
                         br(),
                         actionButton("goButton3", "line3", icon = icon("fa-solid fa-plus")),br(),
                         actionButton("goButton4", "line4", icon = icon("fa-solid fa-plus")),br(),
                         actionButton("goButton5", "line5", icon = icon("fa-solid fa-plus")),br(),
                         ),
                  box(width=11,
                DT::dataTableOutput("responses", width = 700),  style = "overflow-x: scroll;",
                  ),# end of box 
                  ),# end of box 
                ),# end of fluidRow 
                tags$hr(),
                # br(),
                fluidRow(
                  box(title="sector",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_sector"),),
                  box(title="Year",width = 2,background = "light-blue",
                      verbatimTextOutput("value_year_exca"),),
                  box(title="ID_dec",width = 2,background = "light-blue",
                      verbatimTextOutput("value_ID_dec"),),
                  box(title="square",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_square"),),
                  box(title="name_dec",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_dec"),),
                  box(title="level",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_level"),
                  
                  ), # end of box 
                ),
                hr(),
                fluidRow(
                #   box(width = 3,),
                #   box(width = 3,
                # numericInput("nb_remains", label = h5("Numeric input"), value = 1),
                #   ),# end of box
                ),# end of fluidRow
                fluidRow(
                  box(width = 6,
                      selectInput(inputId="name_taxa", 
                                  label= "order level" , 
                                  choices=c("Rodentia","Eulipotyphla","Chiroptera","others"), 
                                  selected = "Rodentia", 
                                  multiple = FALSE, 
                                  selectize = TRUE),
                      uiOutput("species_pickerinput"),

                ),# end of box
                
                box(width = 3,
                    br(),
                    numericInput("nb_remains", label = h5("Numeric input"), value = 1),
                   br(),
                    br()
                    ),# end of box
                # box(width = 3,),
                box(width = 6,
                    pickerInput(
                      inputId = "name_anat",
                      label = "Anatomy", 
                      choices = get(list_bone[1]),
                      options = list(
                        `live-search` = TRUE)),
                    
                    radioGroupButtons(
                  inputId = "infos_lat",
                  label = "Lateralisation",
                  choices = c("Left","IND","Right"),
                  selected =("IND"),
                  status = "primary",
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"),
                    no = icon("remove",
                              lib = "glyphicon"))
                ),
                
                    prettySwitch(
                      inputId = "infos_completude",
                      label = "Complet or broke",
                      status = "success",
                      fill = TRUE
                    ),
                    
                    uiOutput ("completude"),
                ),
               
                box(width=6,
                    prettySwitch(
                      inputId = "infos_obs",
                      label = "Observation",
                      status = "success",
                      fill = TRUE
                    ),
                    uiOutput ("obs"),
                    uiOutput ("obs2"),
                    prettySwitch(
                      inputId = "infos_photo",
                      label = "photo",
                      status = "success",
                      fill = TRUE
                    ),
                    uiOutput ("photo"),
                )
                ),
                
                fluidRow(
                #   box(width = 4,
                # prettySwitch(
                #      inputId = "infos_completude",
                #      label = "Complet or broke",
                #      status = "success",
                #      fill = TRUE
                #    ),
                # 
                # uiOutput ("completude"),
                #   ),
                ),
                fluidRow(
                box(width = 4,
                    radioGroupButtons(
                      inputId = "trace_dig",
                      label = "Digestion marks",
                      choices = c("IND","0","1","2","3","4"),
                      selected =("0"),
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon"))
                    ),
                # sliderInput(
                #   inputId = "trace_dig",
                #   label = "Digestion marks",
                #   min = 0, max = 4,
                #   value = 0,step=1
                # ),
                  radioGroupButtons(
                    inputId = "trace_heat",
                    label = "Burnt marks",
                    choices = c("IND","no","brown","black","white"),
                    selected =("no"),
                    status = "primary",
                    checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon"),
                      no = icon("remove",
                                lib = "glyphicon"))
                  ),
                  ),
                box(width=4,
                    prettySwitch(
                      inputId = "infos_tm",
                      label = "Tooth marks",
                      status = "success",
                      fill = TRUE
                    ),
                    uiOutput ("tm_output"),
            
                    radioGroupButtons(
                      inputId = "trace_root",
                      label = "Root marks",
                      choices = c("IND","0","<50%",">50%"),
                      selected =("IND"),
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon"))
                    ),
                    ),
                    )
                  ) # end of fluidpage
                  ),#end of tabpanel 
                tabPanel(h4("Table"),
                DT::dataTableOutput("responses2", width = 700),  style = "overflow-x: scroll;", 
                tags$hr(),     
                actionButton("deleteRows", "Delete Rows")
                )#end of tabpanel 
                )#end of tabsetpanel     
        ), # end of  tabItem
        tabItem(tabName = "data2",
                # h2("analyses2 tab content"),
                tabsetPanel(
                  tabPanel(tags$h5("Basic data"),
                           # uiOutput("liste.sector"),
                           # uiOutput("liste.UAS"),
                           # uiOutput("liste.passe"),
                           # uiOutput("liste.square"),
                           
                           tags$h4(style = "color: blue;","summary of basic data"), 
                           tags$br(),
                           uiOutput("sum.species2"),
                           tags$br(),
                           uiOutput("sum.bucket"),
                           tags$br(),
                           uiOutput("sum.remain"),
                           tags$br(),
                           uiOutput("sum.remain2")
                           
                  ),#end tabpanel 
                  tabPanel(tags$h5("Data material table per levels"),
                           fluidRow(
                             tags$br(),
                             # tags$h5(style = "color: blue;","Correspond to nature object data per level, as defined in both select input in 'Data upload' window"),
                             
                             tags$br(),
                             uiOutput("sum.species"),
                             tags$br(),
                             tags$br(),
                             tags$hr(),
                             column(5,
                                    
                                    DTOutput("table.species")),
                             column(11, downloadButton("downloadData_speciesdata", "Download")),
                           ) #end fluidrow
                  ), #end tabpanel
                tabPanel(tags$h5("Pivot table"),
                         fluidRow(
                           uiOutput("liste.summary"),
                           column(5,
                                  h4("Remains class "),
                                  tableOutput("summary")),
                           column(11, downloadButton("downloadData_pivotdata", "Download")),
                         ) #end fluidrow
                ), #end tabpanel
                tabPanel(tags$h5("Ratio data"),
                         uiOutput("Ratio.data.list"),
                         tags$br(),
                         uiOutput("name.of.dig.element"),
                         uiOutput("dig.col"),
                         tags$br(),
                         tags$br(),
                         uiOutput("Ratio.data.graph"),
                ),#end tabpanel 
                tabPanel(tags$h5("rarity curves"),
                         tags$br(),
                         materialSwitch(
                           inputId = "Id075",
                           label = "Inext graph"),
                         tags$br(),
                         uiOutput("rarefactionplotui"),
                         tags$br(),
                         tags$br(),
                         column(11, downloadButton("downloadData_rarefactiongraph", "Download")),
                         tags$br(),
                         tags$br(),
                         DTOutput("table.species.perlevels"),
                         column(11, downloadButton("downloadData_rarefactiondata", "Download")),
                ),#end tabpanel    
                tabPanel(tags$h5("Bioclim data"),
                         uiOutput("sum.bucket2"),
                         tags$br(),
                         tags$br(),
                         radioButtons("var.bioclim", "",
                                      choices = c("Rodent" = FALSE,
                                                  "Rodent + Eulipotyphla" = TRUE),
                                      selected = TRUE, inline=TRUE),
                         tags$br(),
                         DTOutput("bioclim.react"),
                         tags$br(),
                         column(11, downloadButton("downloadData_bioclim.react", "Download"),
                                tags$br(),),
                         tags$br(),
                         DTOutput("bioclim.react2"),
                         tags$br(),
                         column(11, downloadButton("downloadData_bioclim.react2", "Download")),
                         tags$br(),
                         tags$h5(style = "color: black;","species name(s) not included:"), 
                         uiOutput("bioclim.names_noused"),
                         tags$h5(style = "color: blue;","Be careful to have well written the species name"), 
                         tags$h5(style = "color: blue;","exemple: Microtus_arvalis"), 
                         
                ),#end tabpanel    
                tabPanel(tags$h5("Bioclim graph"),
                )#end tabpanel 


                )#end of tabsetpanel  
        ),
        tabItem(tabName = "data3",
                h2("Global note from microvertebrate observation"),
                textAreaInput("note.obs", label="observation to add",rows=5, value = "", width = NULL,
                          placeholder = "Leave an comment/observation..."),
                uiOutput("obs.note.torender"),
                actionButton("Record_the_observation", "Record the observation")
                
        )
    )
)

server <- function(input, output, session) {
  font.size <- "8pt"
  fields_theor <- c("date_record","year_exca","name_sector","ID_dec","name_square","name_dec",
                    "name_level","name_taxa" ,"name_species","name_anat", 
                    "infos_suppl_anat","nb_remains","infos_lat",
                    "infos_completude","infos_completude_detailled",
                    "trace_dig","trace_root","traces_patine","trace_heat",
                    "trace_tooth_mark","trace_encoche",
                    "observation","observation_suppl","txt_photo")  
    ID_record<-reactiveVal(1)
    input_file1.name<-reactiveVal()
    input_file1.datapath<-reactiveVal()
    getdata.launch<-reactiveVal()  
    fileisupload<-reactiveVal(NULL)
    e<-reactiveVal(NULL)
    df<-reactiveValues( #creation df 
      df=NULL,save1=NULL,save2=NULL,save3=NULL,save4=NULL,save5=NULL) # end reactivevalues
    global.load<-reactiveValues(df=NULL,save5=NULL,site.archaeo=NULL)
    df_rapid_line<-reactiveVal(0)
    input_infos_suppl_anat<-reactiveVal(NULL)
    session_store <- reactiveValues()  ## for save  plot 
    number.species.per.levels<-reactiveVal(NULL)
    # save.df.last.lines<-reactiveValuesToList(NULL)
    font_size<-reactiveVal(12)
    font_tick<-reactiveVal(12)
    legendplotlyfig<-reactiveVal("right") ##for legends.
    
## NEW BDD ----
output$liste.faun4=renderUI({
      # selectInput("liste.newgroup3", label = h5("Select the variable"), 
      #             choices = factor(df$df[,input$liste.newgroup.rename]))
      selectInput("rod.list.select", label = h5("Rodentia List"), 
                  choices = list_species_rod, 
                   selected = list_species_rod[[1]]) 
    })
output$liste.faun4.eulipo=renderUI({
  # selectInput("liste.newgroup3", label = h5("Select the variable"), 
  #             choices = factor(df$df[,input$liste.newgroup.rename]))
  selectInput("euli.list.select", label = h5("Eulipotyphla List"), 
              choices = list_species_euli, 
              selected = list_species_euli[[1]]) 
})
output$liste.faun4.chiro=renderUI({
  # selectInput("liste.newgroup3", label = h5("Select the variable"), 
  #             choices = factor(df$df[,input$liste.newgroup.rename]))
  selectInput("chiro.list.select", label = h5("Chiroptera List"), 
              choices = list_species_chiro, 
              selected = list_species_chiro[[1]]) 
})
output$liste.faun4.herpeto=renderUI({
  # selectInput("liste.newgroup3", label = h5("Select the variable"), 
  #             choices = factor(df$df[,input$liste.newgroup.rename]))
  selectInput("herpeto.list.select", label = h5("Herpetofauna List"), 
              choices = list_species_herpeto, 
              selected = list_species_herpeto[[1]]) 
})
output$liste.faun4.others=renderUI({
  # selectInput("liste.newgroup3", label = h5("Select the variable"), 
  #             choices = factor(df$df[,input$liste.newgroup.rename]))
  selectInput("other.list.select", label = h5("Other List"), 
              choices = list_species_others, 
              selected = list_species_others[[1]]) 
})
observeEvent(ignoreInit = TRUE,input$rod.list.select,{
  global.load$rod.list.select<-input$rod.list.select
})
observeEvent(ignoreInit = TRUE,input$euli.list.select,{
  global.load$euli.list.select<-input$euli.list.select
})
observeEvent(ignoreInit = TRUE,input$chiro.list.select,{
  global.load$chiro.list.select<-input$chiro.list.select
})
observeEvent(ignoreInit = TRUE,input$herpeto.list.select,{
  global.load$herpeto.list.select<-input$herpeto.list.select
})
observeEvent(ignoreInit = TRUE,input$other.list.select,{
  global.load$other.list.select<-input$other.list.select
})

output$liste.faun1=renderUI({
pickerInput(
  inputId = "Id_bones",
  label = "Select bone remains used", 
  choices = list_bone, 
  options = list(
    `actions-box` = TRUE), 
  multiple = TRUE
)
})

observeEvent(ignoreInit = TRUE,input$create_bdd,{
  global.load$site.archaeo<-input$name_site
  to_save <- reactiveValuesToList(global.load)
  saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
  write.table(as.data.frame(fields_theor), file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
  fileisupload(1)
  showModal(
    modalDialog(
      title = tags$h4(style = "color: blue;","Database create!"),
      easyClose = T,
      HTML("The database is created"),
      
    ))
  
})

##loading ----
output$site=renderPrint({
  tags$p("Archaeological site")
  HTML(paste0("Archaeological site: ",global.load$site.archaeo))
  
})
observeEvent(input$file1, {
  input_file1.name(input$file1$name)
  input_file1.datapath(input$file1$datapath)
})
observeEvent(input$getData, {
  getdata.launch(input$getData)
})
 observeEvent(input$refresh, {
   req(!is.null(global.load$df))
   # df$df<-df$df
   df$df<-global.load$df
   df.sub()
 })
observeEvent(getdata.launch(), {
  req(!is.null(input_file1.datapath()))
  extension <- tools::file_ext(input_file1.name())
  validate(need(extension == "rds", "Please upload a rds file"))
  global<-readRDS(file=input_file1.name())

  df$df<-global$df
  ID_record(global$k)
  # responses <<- global$df
  global.load$df<-global$df
  df$save1<-global$df_save1
  df$save2<-global$df_save2
  df$save3<-global$df_save3
  df$save4<-global$df_save4
  df$save5<-global$df_save5
  rV$ID_dec<-global$ID_dec
  rV$name_square<-global$name_square
  rV$name_dec<-global$name_dec
  rV$name_level<-global$name_level
  rV$name_sector<-global$name_sector
  rV$year_exca<-global$year_exca
  
  last.id.dec(global$last.id.dec)
  last.name.square(global$last.name.square)
  last.name.dec(global$last.name.dec)
  last.name.level(global$last.name.level)
  last.name.sector(global$last.name.sector)
  last.year_exca(global$last.year_exca)

  global.load$site.archaeo<-global$site.archaeo
  
  global.load$other.list.select<-global$other.list.select
  global.load$herpeto.list.select<-global$herpeto.list.select
  global.load$chiro.list.select<-global$chiro.list.select
  global.load$euli.list.select<-global$euli.list.select
  global.load$rod.list.select<-global$rod.list.select
  global.load$note.obs<-global$note.obs
  fileisupload(1)
  
})# end observe of df$df2
observeEvent(fileisupload(), {   
  updateTabItems(session, "tabs", "SMALLvert")
  
  updateTabsetPanel(inputId="tab1",
                    selected = "About")
})

##### loading field database a finir ----
file.field.BDD.isupload<-reactiveVal(NULL)
getdata.fieldBDD.launch<-reactiveVal()
input_fieldBDD.name<-reactiveVal()
input_fieldBDD.datapath<-reactiveVal()

observeEvent(input$file.fieldBDD, {
  input_fieldBDD.name(input$file.fieldBDD$name)
  input_fieldBDD.datapath(input$file.fieldBDD$datapath)
})
observeEvent(input$getData.fieldBDD, {
  getdata.fieldBDD.launch(input$getData.fieldBDD)
})
observe({
  req(!is.null(input_fieldBDD.datapath()))
  extension <- tools::file_ext(input_fieldBDD.name())
  switch(extension,
         csv = {updateSelectInput(session, "worksheet", choices = input_fieldBDD.name())},
         xls =   {    selectionWorksheet <-excel_sheets(path = input_fieldBDD.datapath())
         updateSelectInput(session, "worksheet", choices = selectionWorksheet)},
         xlsx =  {      selectionWorksheet <-excel_sheets(path = input_fieldBDD.datapath())
         updateSelectInput(session, "worksheet", choices = selectionWorksheet)})
})
observeEvent(getdata.fieldBDD.launch(), {
  req(!is.null(input_fieldBDD.datapath()))
  extension <- tools::file_ext(input_fieldBDD.name())
  global.load$BDD.field<- switch(extension,
                   csv =  {    
                     sep2 <- if( ";" %in% strsplit(readLines(input_fieldBDD.datapath(), n=1)[1], split="")[[1]] ){";"
                     } else if( "," %in% strsplit(readLines(input_fieldBDD.datapath(), n=1)[1], split="")[[1]] ){","
                     } else if ( "\t" %in% strsplit(readLines(input_fieldBDD.datapath(), n=1)[1], split="")[[1]] ){"\t"
                     } else {";"}
                     utils::read.csv(input_fieldBDD.datapath(),
                                     header = input$header,
                                     sep = sep2, stringsAsFactors = F,  fileEncoding="latin1",
                                     dec=".")},
                   xls = readxl::read_xls(input_fieldBDD.datapath(), sheet=input$worksheet),
                   xlsx = readxl::read_xlsx(input_fieldBDD.datapath(), sheet=input$worksheet))
  print('aa')
  print(global.load$BDD.field)
  file.field.BDD.isupload(1)
})# 
liste.set.square<-reactiveVal(c("Square","null","square","Carré"))
liste.set.ID.dec<-reactiveVal(c("ID","null","ID.object","numero"))
liste.set.dec<-reactiveVal(c("dec","null"))
liste.set.levels<-reactiveVal(c("Levels","null","Couche","levels"))
liste.set.sector<-reactiveVal(c("sector","null"))
  liste.set.year.exca<-reactiveVal(c("year","null"))
output$set.ID.dec=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setIDdec", h4("ID of split (Default name: ID)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.ID.dec())
}) 
output$set.year=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setyear", h4("Year (Default name: Year)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.year.exca())
}) 
output$set.sector=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setsector", h4("Sector (Default name: Sector)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.sector())
}) 

output$set.square=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setsquare", h4("Square (Default name: Square)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.square())
}) 
output$set.dec=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setdec", h4("Splits/decapage (Default name: dec)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.dec())
}) 

output$set.levels=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setlevels", h4("Levels (Default name: Levels)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.levels())
}) 

##end of loading field database

##incrementation ----
    output$value_ID_dec <- renderText({ input$ID_dec })
    output$value_name_square <- renderText({ input$name_square })
    output$value_name_dec <- renderText({ input$name_dec })
    output$value_name_level <- renderText({ input$name_level })
    output$value_name_sector <- renderText({ input$name_sector })
    output$value_year_exca <- renderText({ input$year_exca })
    
    rV <- reactiveValues(ID_dec = "")
    rV <- reactiveValues(name_square = "")
    rV <- reactiveValues(name_dec = "")
    rV <- reactiveValues(name_level = "")
    rV <- reactiveValues(name_sector = "")
    rV <- reactiveValues(year_exca = "")
    last.id.dec<-reactiveVal("not_selected")
    last.name.square<-reactiveVal("not_selected")
    last.name.sector<-reactiveVal("not_selected")
    last.name.dec<-reactiveVal("not_selected")
    last.year_exca<-reactiveVal("not_selected")
    last.name.level<-reactiveVal("not_selected")
    
    output$completude=renderUI({
      if (input$infos_completude==TRUE) {
      checkboxGroupButtons(
        inputId = "infos_completude_detailled",
        label = "Label",
        choices = c("Prox", 
                    "diaphyse", "Dist"),
        selected =c("Prox", 
                    "diaphyse", "Dist"),
        status = "primary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon"))
      )
      }
      
    })
    output$obs=renderUI({
      if (input$infos_obs==TRUE) {
        textInput("observation", label="observation", value = "", width = NULL,
                  placeholder = NULL)
      }
      
    })

    output$obs2=renderUI({
      pickerInput(
        inputId = "observation_suppl",
        label = "Supplementary observation", 
        choices = list_info_suppl,
        multiple = TRUE
      )
    })
    output$photo=renderUI({
      if (input$infos_photo==TRUE) {
        textInput("txt_photo", label="photo information", value = "Photo:", width = NULL,
                  placeholder = "No photo")
      } else {
        
      }
      
    })
    
    observeEvent(input$txt_photo,{
      global.load$photo<-input$txt_photo
    })
    
    output$species_pickerinput=renderUI({
      switch(input$name_taxa,

             Rodentia = {
               # species.menu<-get(list_species_rod[1])
               species.menu<-get(global.load$rod.list.select)
               },
             Eulipotyphla =   {
               # species.menu<-get(list_species_euli[1])
                 species.menu<-get(global.load$euli.list.select)
             },
             others =   {
               # species.menu<-get(list_species_others[1])
               species.menu<-get(global.load$other.list.select)
             },
             Herpetofauna =   {
               # species.menu<-get(list_species_herpeto[1])
               species.menu<-get(global.load$herpeto.list.select)
             },
             Chiroptera =  { 
               # species.menu<-get(list_species_chiro[1])
               species.menu<-get(global.load$chiro.list.select)
             })
       
      selectizeInput(
      inputId = "name_species",
      label = "Name species", 
      choices = species.menu,
      options = list(create = TRUE,
        `live-search` = TRUE)
    )
    })
    
    output$tm_output=renderUI({
      if (input$infos_tm==TRUE) {
        radioGroupButtons(
          inputId = "trace_tooth_mark",
          label = "Tooth marks",
          choices = c("?","1","multiple","opposite"),
          selected =("oui"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"),
            no = icon("remove",
                      lib = "glyphicon"))
        )
      }
      
    }) 
    
    output$mand_output=renderUI({
      if (input$Id_mand_empty==TRUE) {
        pickerInput(
          inputId = "Id_mand",
          label = "Select/deselect all options", 
          choices = c("Iinf","m1inf","m2inf","m3inf"),
          options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
        )
      }
      
    })    
    output$max_output=renderUI({
      if (input$Id_max_empty==TRUE) {
        pickerInput(
          inputId = "Id_max",
          label = "Select/deselect all options", 
          choices = list(
            left=c("Isup","M1sup","M2sup","M3sup"),
            right=c("Isup","M1sup","M2sup","M3sup")
          ),
          options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
        )
      }
      
    })   

    
    observeEvent(input$name_anat,{
      input_infos_suppl_anat(NULL)
      if(input$name_anat=="MOL"){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: blue;","Mandible choice"),
            easyClose = T,
            HTML("Select molar"),
            pickerInput(
              inputId = "Id_mol",
              label = "Molars", 
              choices = c("m1inf","m2inf","m3inf","M1sup","M2sup","M3sup"),
              multiple = FALSE
            )
          ))

        

      }
      
      if(input$name_anat=="Mand"){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: blue;","Mandible choice"),
            easyClose = T,
            
            materialSwitch(
              inputId = "Id_mand_empty",
              label = "Empty mandible/Teeth within mandible", 
              value = FALSE, 
              right = TRUE,
              status = "primary"
            ),
            HTML("Select teeth within the mandible"),
        uiOutput("mand_output")
          ))

      }
      if(input$name_anat=="Max"){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: blue;","Maxillary choice"),
            easyClose = T,
            materialSwitch(
              inputId = "Id_max_empty",
              label = "Empty maxillary/Teeth within maxillary", 
              value = FALSE, 
              right = TRUE,
              status = "primary"
            ),
            HTML("Select teeth within the Maxillary"),
            uiOutput("max_output")
            
            
          ))
      }

    })
    observeEvent(input$Id_mol, {
      input_infos_suppl_anat(input$Id_mol)
      
    })
    observeEvent(input$Id_max, {
      input_infos_suppl_anat(input$Id_max)
      
    })
    observeEvent(input$Id_mand, {
      input_infos_suppl_anat(input$Id_mand)
      
    })
    
    observeEvent(input$submit, {
      
      showModal(
        modalDialog(
        title = tags$h4(style = "color: red;","Load file"),
        easyClose = T,
        HTML("Size options are not available without unique ID"),
        selectizeInput("name_sector","name of sector", choices = c(rV$name_sector),selected = last.name.sector(), options = list(create = TRUE)),
        selectizeInput("year_exca","year", choices = c(rV$year_exca),selected = last.year_exca(), options = list(create = TRUE)),
        
        selectizeInput("ID_dec","ID of split/decapage", choices = c(rV$ID_dec),selected = last.id.dec(), options = list(create = TRUE)),
        selectizeInput("name_square","name of square", choices = c(rV$name_square),selected = last.name.square(), options = list(create = TRUE)),
        selectizeInput("name_dec","name of dec", choices = c(rV$name_dec),selected = last.name.dec(), options = list(create = TRUE)),
        selectizeInput("name_level","name of levels", choices = c(rV$name_level),selected = last.name.level(), options = list(create = TRUE)),
        if (!is.null(file.field.BDD.isupload())) {
          
          uiOutput("txt.field.data")     

        }
        ))
      
      
      last.id.dec(input$ID_dec)
      last.name.square(input$name_square)
      last.name.sector(input$name_sector)
      last.name.dec(input$name_dec)
      last.name.level(input$name_level)
      last.year_exca(input$year_exca)
    
    })
  
    output$txt.field.data<-renderUI({
      aa<-global.load$BDD.field[global.load$BDD.field[,input$setIDdec] == input$ID_dec, ]
      temp.square.ID<-as.character(aa[1,c(input$setIDdec,input$setsquare,input$set.dec,input$setlevels,input$setsector,input$setyear)])
      HTML(paste(" Field information from the database: <br>"))
      HTML(temp.square.ID)
    })
    
    observeEvent(input$ID_dec, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$ID_dec) && !(input$ID_dec %in% rV$ID_dec)) {
        rV$ID_dec <- c(rV$ID_dec, input$ID_dec)
      }})
    observeEvent(input$name_square, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$name_square) && !(input$name_square %in% rV$name_square)) {
        rV$name_square <- c(rV$name_square, input$name_square)
      }})
    observeEvent(input$year_exca, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$year_exca) && !(input$year_exca %in% rV$year_exca)) {
        rV$year_exca <- c(rV$year_exca, input$year_exca)
      }})
    observeEvent(input$name_dec, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$name_dec) && !(input$name_dec %in% rV$name_dec)) {
        rV$name_dec <- c(rV$name_dec, input$name_dec)
      }})
    observeEvent(input$name_level, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$name_level) && !(input$name_level %in% rV$name_level)) {
        rV$name_level <- c(rV$name_level, input$name_level)
      }})
    observeEvent(input$name_sector, {
      # nchar check, because emptying the text field results in "" choice.
      if (nchar(input$name_sector) && !(input$name_sector %in% rV$name_sector)) {
        rV$name_sector <- c(rV$name_sector, input$name_sector)
      }})
    
    observeEvent(ignoreInit = T,input$setIDdec, {
      req(!is.null(file.field.BDD.isupload()))
      rV$ID_dec <- c(rV$ID_dec,global.load$BDD.field[,input$setIDdec])
    })
    observeEvent(ignoreInit = T,input$setsector, {
      req(!is.null(file.field.BDD.isupload()))
      rV$name_square <- c(rV$name_sector,global.load$BDD.field[,input$setsector])
    })
    
    observeEvent(ignoreInit = T,input$setyear, {
      req(!is.null(file.field.BDD.isupload()))
      rV$year_exca <- c(rV$year_exca,global.load$BDD.field[,input$setyear])
    })
    observeEvent(ignoreInit = T,input$setsquare, {
      req(!is.null(file.field.BDD.isupload()))
      rV$name_sector <- c(rV$name_square,global.load$BDD.field[,input$setsquare])
    })
    
    
    observeEvent(ignoreInit = T,input$setdec, {
      req(!is.null(file.field.BDD.isupload()))
      rV$name_dec <- c(rV$name_dec,global.load$BDD.field[,input$setdec])
    })
    observeEvent(ignoreInit = T,input$setlevels, {
      req(!is.null(file.field.BDD.isupload()))
        rV$name_level <- c(rV$name_level,global.load$BDD.field[,input$setlevels])
      })
      
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- sapply(fields_theor, function(x) input[[x]])
        data<-c(ID_record(),data)
        names(data)<-c("ID_record",fields_theor)
        data$date_record<-Sys.time()
        if(!is.null(input_infos_suppl_anat())){
         data$infos_suppl_anat<-input_infos_suppl_anat()}
         data$dig_I<-"NA"
         data$dig_MOL<-"NA"
         data$dig_m1inf<-"NA"
         data$dig_bone<-"NA"
         if(length(data$name_sector)==1 && data$name_sector=="") {
           data$name_sector<-"empty"} 
         if(length(data$year_exca)==1 && data$year_exca=="") {
           data$year_exca<-"empty"} 
         if(length(data$ID_dec)==1 && data$ID_dec=="") {
           data$ID_dec<-"empty"} 
         if(length(data$name_square)==1 && data$name_square=="") {
           data$name_square<-"empty"} 
         if(length(data$name_dec)==1 && data$name_dec=="") {
           data$name_dec<-"empty"} 
         if(length(data$name_level)==1 && data$name_level=="") {
           data$name_level<-"empty"}
         
        #"Mand","Max",
        if (!input$trace_dig=="IND"){
         switch(input$name_anat,

                Iinf = {
                  data$dig_I<-input$trace_dig
                },
                Isup =   {
                  data$dig_I<-input$trace_dig
                },
                m1inf =   {
                  data$dig_MOL<-input$trace_dig
                },
                MOL =   {
                  data$dig_MOL<-input$trace_dig
                },
                FEM =   {
                  data$dig_bone<-input$trace_dig
                },
                HUM =  { 
                  data$dig_bone<-input$trace_dig
                })
          } #end of if
         data
        
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(ignoreInit = TRUE, input$submit2, {
      if (is.null(input$ID_dec)){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: red;","Are you sur? "),
            easyClose = T,
            HTML("No ID for splits/decapage has been given. Please provide an ID for splits/decapages ? "),
          ))
      
      }
      if (input$name_species=="not_selected"){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: red;","Are you sur? "),
            easyClose = T,
            HTML("No species name has been selected. Please provide a species name"),
          ))
      }
      if (input$name_anat=="not_selected"){
        showModal(
          modalDialog(
            title = tags$h4(style = "color: red;","Are you sur? "),
            easyClose = T,
            HTML("No bone/dental name has been selected. Please provide an anatomical identification ? "),
          ))

      }
      updateSelectizeInput(session = session,inputId = "ID_dec", selected = last.id.dec())
      updateSelectizeInput(session = session,inputId = "name_square",selected = last.name.square())
      updateSelectizeInput(session = session,inputId = "name_dec",selected = last.name.dec())
      updateSelectizeInput(session = session,inputId = "name_level",selected = last.name.level())
      updateSelectizeInput(session = session,inputId = "name_sector",selected = last.name.sector())
      updateSelectizeInput(session = session,inputId = "year_exca",selected = last.year_exca())
     
      
        # saveData(formData())
        data <- as.data.frame(t(formData()))
        global.load$df<-rbind(global.load$df, data)
        
        k<-ID_record()+1
        ID_record(k)
        global.load$k<-k
        df$save1<-df$save2
        df$save2<-df$save3
        df$save3<-df$save4
        df$save4<-df$save5
        df$save5<-data
        # global.load$df<-responses
        global.load$df_save1<-df$save1
        global.load$df_save2<-df$save2
        global.load$df_save3<-df$save3
        global.load$df_save4<-df$save4
        global.load$df_save5<-df$save5
        global.load$ID_dec<-rV$ID_dec
        global.load$name_sector<-rV$name_sector
        global.load$name_square<-rV$name_square
        global.load$year_exca<-rV$year_exca
        global.load$name_dec<-rV$name_dec
        global.load$name_level<-rV$name_level
        global.load$last.id.dec<-last.id.dec()
        global.load$last.name.square<-last.name.square()
        global.load$last.name.sector<-last.name.sector()
        global.load$last.name.dec<-last.name.dec()
        global.load$last.year_exca<-last.year_exca()
        global.load$last.name.level<-last.name.level()
        global.load$input_infos_suppl_anat<-input_infos_suppl_anat()
        input_infos_suppl_anat(NULL)
         updateSelectizeInput(session = session, inputId = "name_species",selected ="not_selected")
        updatePickerInput(session = session, inputId = "name_anat",choices = get(list_bone[1]))
        updatePickerInput(session = session, inputId = "infos_suppl_anat",choices = NULL)
        updateNumericInput(session = session, inputId = "nb_remains",value =1)
        updateRadioGroupButtons(session = session, inputId = "infos_lat", selected = "IND")  
        updatePrettySwitch(session = session, inputId = "infos_completude",value = FALSE)
        updateCheckboxGroupButtons(session = session, inputId = "infos_completude_detailled",selected = c(""))  
        updateRadioGroupButtons(session = session, inputId = "trace_dig",selected = "0")
        updateRadioGroupButtons(session = session, inputId ="trace_root",selected = "IND")
        updateRadioGroupButtons(session = session, inputId ="trace_heat",selected = "no")
        updatePrettySwitch(session = session, inputId = "infos_tm",value = FALSE)
        updateRadioGroupButtons(session = session, inputId ="trace_tooth_mark",selected = "oui")
        updateTextInput(session = session, inputId = "observation",value = "")
        updateTextInput(session = session, inputId = "txt_photo",value = "No photo")
        updatePrettySwitch(session = session, inputId = "infos_photo",value = FALSE)
        updatePickerInput(session = session, inputId = "observation_suppl",choices = NULL)
          # $ infos_suppl_anat          : NULL
          # $ traces_patine             : NULL
          # $ trace_encoche             : NULL
          updatePrettySwitch(session = session, inputId = "infos_obs",value = FALSE)
          to_save <- reactiveValuesToList(global.load)
          saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
          test<-data.frame(apply(global.load$df,2,as.character))
          write.table(test, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 

          })
    
    
    observeEvent(ignoreInit = TRUE, c(input$goButton1),{
      req(!is.null(df$save1))
      k<-ID_record()+1
      df$save1[[1]][[1]]<-k
      ID_record(k)
      df_rapid_line(df$save1)
    })
    observeEvent(ignoreInit = TRUE, c(input$goButton2),{
      req(!is.null(df$save2))
      k<-ID_record()+1
      df$save2[[1]][[1]]<-k
      ID_record(k)
      df_rapid_line(df$save2)
    })
    observeEvent(ignoreInit = TRUE, c(input$goButton3),{
      req(!is.null(df$save3))
      k<-ID_record()+1
      df$save3[[1]][[1]]<-k
      ID_record(k)
      df_rapid_line(df$save3)
    })
    observeEvent(ignoreInit = TRUE, c(input$goButton4),{
      req(!is.null(df$save4))
      k<-ID_record()+1
      df$save4[[1]][[1]]<-k
      ID_record(k)
      df_rapid_line(df$save4)
    })
    observeEvent(ignoreInit = TRUE, c(input$goButton5),{
      req(!is.null(df$save5))
      k<-ID_record()+1
      df$save5[[1]][[1]]<-k
      ID_record(k)
      df_rapid_line(df$save5)
    })

    observeEvent(ignoreInit = TRUE, df_rapid_line(),{
      temp<-df_rapid_line()
      df$save1<-df$save2
      df$save2<-df$save3
      df$save3<-df$save4
      df$save4<-df$save5
      df$save5<-temp
      
      data <- as.data.frame(t(formData()))
      global.load$df<-rbind(global.load$df, temp)
      
      # responses <<- rbind(responses, temp) #### a modif
      refresh_dtoutput(rnorm(1,mean=100,sd=100))
      
      to_save <- reactiveValuesToList(global.load)
      saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
      test<-data.frame(apply(global.load$df,2,as.character))
      write.table(test, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
      
    })
   
    
   refresh_dtoutput<-reactiveVal(0)
    observeEvent(input$submit2,{
      refresh_dtoutput(rnorm(1,mean=100,sd=100))
    })

    output$responses <- DT::renderDataTable({
        # input$submit2
      refresh_dtoutput()
            # loadData()
      global.load$df
    }, server = FALSE,
    callback = JS(c("table.page('last').draw(false);")),
  
    options = list(lengthMenu = c(1, 2,3, 5,10,25,50), pageLength = 5)
    )
    
    output$responses2 <- DT::renderDataTable({
       datatable(
         # loadData(), 
         global.load$df,
         editable = TRUE)
    } ,   options = list(lengthMenu = c(1, 2,3, 5,10,25,50), pageLength = 5)
    )
    observeEvent(input$responses2_cell_edit, {
      # data2<-loadData()
      data2<-global.load$df
      row  <- input$responses2_cell_edit$row
      clmn <- input$responses2_cell_edit$col
      data2[row, clmn] <- input$responses2_cell_edit$value
      # responses <<- data2
      to_save <- reactiveValuesToList(global.load)
      saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
      write.table(as.data.frame(fields_theor), file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
      
    })
    observeEvent(input$deleteRows,{
        if (!is.null(input$responses2_rows_selected)) {
        # data2<-loadData()
         data2<-global.load$df
         data2 <-  data2[-as.numeric(input$responses2_rows_selected),]
         global.load$df<-data2
         # responses <<- data2
         to_save <- reactiveValuesToList(global.load)
         saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
         write.table(as.data.frame(fields_theor), file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
        
      }
    })
    

         output$obs.note.torender <- renderText({ global.load$note.obs })
         
         observeEvent(input$Record_the_observation,{
     global.load$note.obs<-paste0(global.load$note.obs,"<p>",input$note.obs,"</p>")
           
         })
#### Microfauna treatment ----
  
##### df.sub and co   ----  
somme.indiv<-reactiveVal()
df.species.table<-reactiveVal()
number.total.of.species<-reactiveVal(1)
output$liste.sector=renderUI({
  sector.list<-unique(unlist((df$df[["name_sector"]])))
  checkboxGroupInput("localisation", h6("Secteur"),
                      choices = levels(as.factor(sector.list)),selected = levels(as.factor(sector.list)))
})
output$liste.UAS=renderUI({
  lvl.list<-unique(unlist((df$df[["name_level"]])))
  checkboxGroupInput("UAS", h6("Levels"),
                     choices = levels(as.factor(lvl.list)),selected = levels(as.factor(lvl.list)))
})
output$liste.passe=renderUI({
  dec.list<-unique(unlist((df$df[["name_dec"]])))
  checkboxGroupInput("Passe", h6("Split"),
                     choices = levels(as.factor(dec.list)),selected = levels(as.factor(dec.list)))
})
output$liste.square=renderUI({
  sq.list<-unique(unlist((df$df[["name_square"]])))
  checkboxGroupInput("Square", h6("Square"),
                     choices = levels(as.factor(sq.list)),selected = levels(as.factor(sq.list)))
})
output$liste.year=renderUI({
  y.list<-unique(unlist((df$df[["year_exca"]])))
  checkboxGroupInput("Year", h6("Year"),
                     choices = levels(as.factor(y.list)),selected = levels(as.factor(y.list)))
})

df.sub <- reactive({ 
           req(!is.null(fileisupload))
          df.sub<-global.load$df
           # if (input$setdate!="null"){
           #   df.sub[,input$setdate] <-as.numeric(df.sub[,input$setdate])
           #   df.sub[,input$setdate][is.na(df.sub[,input$setdate])]<-0
           #   if (!is.null(input$Date2)) {
           #     df.sub<-df.sub %>%
           #       filter(df.sub[,input$setdate] >= input$Date2[1], df.sub[,input$setdate] <= input$Date2[2])}}
             # df.sub <- df.sub[df.sub[,input$setsector] %in% input$localisation, ]
             # df.sub <- df.sub[df.sub[,input$setlevels] %in% input$UAS, ]
             # df.sub <- df.sub[df.sub[,input$setpasse]%in% input$Passe, ]
             # df.sub <- df.sub[df.sub[,input$setnature] %in% input$Square, ]
             df.sub <- df.sub[df.sub[["name_sector"]] %in% input$localisation, ]
             df.sub <- df.sub[df.sub[["name_level"]] %in% input$UAS, ]
             df.sub <- df.sub[df.sub[["name_dec"]]%in% input$Passe, ]
             df.sub <- df.sub[df.sub[["name_square"]] %in% input$Square, ]
             df.sub <- df.sub[df.sub[["year_exca"]] %in% input$Year, ]
        
             # df.sub<-df.sub %>% 
           #   filter(.data[[setXX()]] >= input$xslider[1], .data[[setXX()]] <= input$xslider[2]) %>% 
           #   filter(.data[[setYY()]] >= input$yslider[1], .data[[setYY()]] <= input$yslider[2]) %>% 
           #   filter(.data[[setZZ()]] >= input$zslider[1], .data[[setZZ()]] <= input$zslider[2])
             
             df.sub[,1:29][df.sub[,1:29]=="NULL"] <- "NA"
               # df.sub<-as.data.frame(t(apply(df.sub,2, function(x) unlist(x))))
             if(nrow(df.sub)>1){
               df.sub<-as.data.frame(apply(df.sub,2, function(x) as.character(x)))}
               assign("temppp",df.sub,envir = .GlobalEnv)
               
             validate(need(length(df.sub)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
             df.sub
             
             # assign("temppp",df.sub,envir = .GlobalEnv)
             df.sub$nb_remains<-as.numeric(df.sub$nb_remains)
             
           # df.sub
           # validate(need(nrow(df.sub)==0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
           # 
           #### creation de df.species.table
           # data.df.tot<-as.data.frame(t(apply(df.sub,1, function(x) unlist(x))))
           # data.df.tot$nb_remains<-as.numeric(data.df.tot$nb_remains)
           data.df.tot<-df.sub
           data.df.tot2<-data.df.tot %>% group_by(.data[["name_level"]],.data[["name_species"]])  %>% 
             dplyr::summarise(total = sum(!!(as.numeric(input$nb_remains))))
           myFormula <- as.formula(paste0("name_level", " ~ ","name_species"))
           df.species.table<-reshape2::dcast(data.df.tot2,myFormula, fill = 0L)
           df.species.table(df.species.table)

           number.total.of.species(ncol(df.species.table)-1)
           ####
           df.sub
         })  # end of df.sub reactive
         

         ###### Basic data ----
         
         output$sum.species=renderUI({
           req(!is.null(fileisupload()))
           tagList(HTML(paste("Total number of different type of objects: ",number.total.of.species())))
           })

         
         output$sum.bucket=renderUI({
           req(!is.null(fileisupload()))
           aa<-df.sub()[["ID_dec"]]
           tagList(HTML(paste("Number of bucket selected: ",length(unique(aa)))))
         })
         
         output$sum.bucket2=renderUI({
           req(!is.null(fileisupload()))
           aa<-df.sub()[,"ID_dec"]
           tagList(h4(style = "color: blue;",HTML(paste("Bioclim data"))))
         })
         output$sum.remain=renderUI({
           req(!is.null(fileisupload()))
           aa<-as.data.frame(global.load$df[["nb_remains"]])
           tagList(HTML(paste("Number total of all remains: ",sum(aa))))
         })
         output$sum.remain2=renderUI({
           req(!is.null(fileisupload()))
           aa<-df.sub()
           tagList(HTML(paste("Number of remains selected: ",sum(aa[["nb_remains"]]))))
         })  
         
         
         ###### rarefaction curves ----
         output$rarefactionplotui <- renderUI({
           plotOutput("rarefactionplot"
                      # , height = height.size(), width = width.size()
                      )
         })

         output$rarefactionplot <- renderPlot({
           tab.raref_fossil<-df.species.table()
           rownames(tab.raref_fossil)<-tab.raref_fossil[,1]
           tab.raref_fossil.2<-tab.raref_fossil[2:ncol(tab.raref_fossil)]
           S <- vegan::specnumber(tab.raref_fossil.2)
           number.species.per.levels(S)
           if (input$Id075==FALSE){
             raremax <- min(rowSums(tab.raref_fossil.2))
             Srare <- vegan::rarefy(tab.raref_fossil.2, raremax)
             plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
             vegan::rarecurve(tab.raref_fossil.2, step = 1, sample = raremax, col = "blue", cex = 0.6)
             #session_store$plotrarecurve <- recordPlot()                                      ## record this graph version is not working 
           } else {
             tab.raref_fossil3<-t(tab.raref_fossil.2)
             x<-iNEXT::iNEXT(tab.raref_fossil3,q=0,datatype="abundance")
             p<-iNEXT::ggiNEXT(x, type=1, se=TRUE, facet.var="None", color.var="Assemblage", grey=FALSE) 
             session_store$plotrarecurve <- p
             p
           }
         })
         
         ####### Export table nb species per levels for rarefaction curve ----
         output$table.species.perlevels <-  DT::renderDataTable({
           req(!is.null(number.species.per.levels()))
           data.df.tot2<-as.data.frame(number.species.per.levels())
           DT::datatable(
             data= data.df.tot2, 
             extensions = 'Buttons', options = list(
               initComplete = htmlwidgets::JS(
                 "function(settings, json) {",
                 paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                 "}")
             ))
         })#end renderDataTable
         
         #######  save rarefaction table ----
         output$downloadData_rarefactiondata<- downloadHandler( 
           filename = function() {
             paste0(Sys.Date(),"rarefaction.data.table",".csv")
           },
           content = function(file) {
             write.table(as.data.frame(number.species.per.levels()), file, row.names = FALSE, sep=";",dec=".") 
           }
         ) 
         
         #######  save rarefaction.curve ----
         output$downloadData_rarefactiongraph<- downloadHandler( 
           filename = function(){
             paste("rarefaction.curve - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
           content = function(file) {
             ggsave(session_store$plotrarecurve,filename=file, device = "pdf")
           }
         ) 
         
         
         ###### Bioclim methods  ----
         # to fix : issue with data.species and bioclim data
         BCI_LVLn_of_siteS2<-reactiveVal(NULL)
         res_lda<-reactiveVal(NULL)
         
         output$bioclim.react <-  DT::renderDataTable({  
           tab.raref_fossil<-df.species.table()
           assign("temp3",tab.raref_fossil,envir=.GlobalEnv)
           names(tab.raref_fossil)<-stringr::str_to_lower(names(tab.raref_fossil))
           rownames(tab.raref_fossil)<-tab.raref_fossil[,1]
           data.df.tot2<-tab.raref_fossil[2:ncol(tab.raref_fossil)]
           data.df.tot3<-as.data.frame(t(data.df.tot2))
           data.df.tot3<-mutate_all(data.df.tot3, function(x) as.numeric(as.character(x)))
           list.of.site<-vector("list", ncol(data.df.tot3))
           if(ncol(data.df.tot3)>1){
             for (i in 1:ncol(data.df.tot3)) {
               list.of.site[[i]]<-rownames(data.df.tot3[data.df.tot3[,i]!=0,])
             } # creation of list for several cases
           } else {
             
             list.of.site[[1]]<-rownames(data.df.tot3)
           }
           
           BCI_LVLn_of_siteS <- Func_BCI_Calcul(list.of.site, EUL = input$var.bioclim, verif = F)
           names(BCI_LVLn_of_siteS)<-colnames(data.df.tot3)
           BCI_LVLn_of_siteS2<-as.data.frame(do.call(rbind, BCI_LVLn_of_siteS))
           BCI_LVLn_of_siteS2(BCI_LVLn_of_siteS2)
           BCI_LVLn_of_siteS[sapply(BCI_LVLn_of_siteS, is.null)] <- NULL ## to remove null element
           res_lda<-func_LDA(BCI_LVLn_of_siteS, quantiv = TRUE)
           res_lda<-as.data.frame(do.call(rbind, res_lda))
           res_lda(res_lda)
           DT::datatable(
             data= as.data.frame(BCI_LVLn_of_siteS2), 
             extensions = 'Buttons', options = list(
               lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
               pageLength = 15,
               initComplete = htmlwidgets::JS(
                 "function(settings, json) {",
                 paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                 "}")
             )) 
         }) 
         
         output$bioclim.react2 <-  DT::renderDataTable({
           req(!is.null(res_lda()))
           res_lda<-as.data.frame(res_lda())
           DT::datatable(
             data= res_lda, 
             extensions = 'Buttons', options = list(
               lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
               pageLength = 15,
               initComplete = htmlwidgets::JS(
                 "function(settings, json) {",
                 paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                 "}"),
               scrollX = TRUE,
               fixedColumns = list(leftColumns = 10)
             ))%>%
             formatRound(columns= c(2,4:31),digits=3)
           
         }) 
         
         output$bioclim.names_noused=renderUI({
           req(!is.null(fileisupload()))
           data.sp.used<-levels(as.factor(df.sub()[["name_species"]]))

           # data_allspecies <- data_species_biozone                         #### faire attention au chargement de ce jeu de donnée avec le script palber
           taxNamesTot <- as.character(unlist(data_species_biozone["Taxon"]))
           # data.sp.used<- mutate_all(data.sp.used, .funs=stringr::str_to_lower)
           data.sp.used<-stringr::str_to_lower(data.sp.used)
           id_noused <- which(!is.element(data.sp.used, taxNamesTot))
           names_noused <- data.sp.used[id_noused]
           tagList(h5(style = "color: red;",HTML(paste(names_noused))))
           
         })
         
         ######  Ratio graphs ---- 
         output$Ratio.data.graph <- renderUI({
           plotOutput("Ratiodatagraph"
                      # , height = height.size(), width = width.size()
                      )
         })
         
         output$Ratiodatagraph <- renderPlot({
           plot(Ratiodatagraph.plot())
           session_store$Ratiodatagraph.plot<- Ratiodatagraph.plot()
         })   
         
         Ratiodatagraph.plot<-reactive({
           df.sub<-df.sub()
           # setlevels<-input$setlevels
           # setanat<-input$setanat
           # setnb<-input$setnb
           digcol<-input$digcol
           nameofdigelement<-input$nameofdigelement
           data.df.calcul<-df.sub %>% group_by(.data[["name_level"]],.data[["name_anat"]])%>%
             summarize(nb_total = sum(!!sym("nb_remains")))
           myFormula <- as.formula(paste0("name_level", " ~ ","name_anat"))
           data.df.calcul.2<-reshape2::dcast(data.df.calcul, myFormula , fill = 0L)
           
           switch(input$select.ratio,
                  "1"={
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Fem"))) > 0 ,"No 'fem'or 'femur' elements found in the database"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Hum"))) > 0 ,"No 'hum'or 'humerus' elements found in the database"))
                    
                    axis.var.name<-"CRA/POSTCRA%"
                    ratio<-dplyr::select(data.df.calcul.2,starts_with("Mand"))/(dplyr::select(data.df.calcul.2,starts_with("Mand"))+dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum")))
                    #ratio<-data.df.calcul.2$mand/(data.df.calcul.2$mand+data.df.calcul.2$fem+data.df.calcul.2$hum)
                    #somme.ratio<-data.df.calcul.2$mand+data.df.calcul.2$fem+data.df.calcul.2$hum
                    somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("Mand"))+dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    axis.var.name<-"ratio CRA/POSTCRA %"
                  },
                  "2"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    
                    axis.var.name<-"AN/PO%"
                    ratio<-dplyr::select(data.df.calcul.2,starts_with("Hum"))/(dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum")))
                    somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    axis.var.name<-"ratio AN/PO %"
                  },
                  "3"={
                    data.df.calcul.gh<-df.sub %>% group_by(.data[[setlevels]],.data[[setanat]],.data[[digcol]])%>%
                      summarize(nb_total = sum(!!sym(setnb)))
                    data.df.calcul.gh.2 <- data.df.calcul.gh %>% filter(.data[[setanat]] == nameofdigelement)
                    somme.ratio<-data.df.calcul.gh.2 %>% group_by(.data[[setlevels]])%>%
                      summarize(nb_total = sum(!!sym("nb_total")))
                    myFormula <- as.formula(paste0(setlevels, " ~ ",digcol))
                    data.df.calcul.2<-reshape2::dcast(data.df.calcul.gh.2, myFormula , fill = 0L)
                    print(data.df.calcul.2)
                    
                    ## ici faire calcul dig vs pas dig. 
                    ## + rajouter dans les choix digelement: fem,hum etc
                    # et dans digcol: des noms de colonne commencant par dig
                    ratio<-data.df.calcul.2$hum/(data.df.calcul.2$hum+data.df.calcul.2$fem) # A changer
                    somme.ratio<-data.df.calcul.2$hu
                    
                    
                    axis.var.name<- paste0("Proportion of digested ", nameofdigelement)
                  },
                  "4"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("Fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("Rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Ulna"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("Ulna"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("Tib"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
                    mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
                    max<-dplyr::select(data.df.calcul.2,starts_with("max"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
                    mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
                    inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
                    mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
                    mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Sca"))) > 0 ,"No 'sca' or 'scapulae' elements found in the database"))
                    sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
                    tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
                    car<-dplyr::select(data.df.calcul.2,starts_with("car"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
                    pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
                    pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Bassin"))) > 0 ,"No 'bas' or 'bassin' elements found in the database"))
                    bas<-dplyr::select(data.df.calcul.2,starts_with("bas"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Vert"))) > 0 ,"No 'vert' or 'vertebrae' elements found in the database"))
                    vert<-dplyr::select(data.df.calcul.2,starts_with("vert"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Rib"))) > 0 ,"No 'rib' or 'ribs' elements found in the database"))
                    rib<-dplyr::select(data.df.calcul.2,starts_with("rib"))
                    
                    ratio<-((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib)*32)+((mand+max+mol+inc)*184))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib
                    axis.var.name<-"ratio PCRT/CR%"
                  },
                  "5"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
                    mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
                    max<-dplyr::select(data.df.calcul.2,starts_with("max"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
                    mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
                    inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
                    mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
                    mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("sca"))) > 0 ,"No 'sca' or 'scapulae' elements found in the database"))
                    sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
                    tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
                    car<-dplyr::select(data.df.calcul.2,starts_with("car"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
                    pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
                    pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("bas"))) > 0 ,"No 'bas' or 'bassin' elements found in the database"))
                    bas<-dplyr::select(data.df.calcul.2,starts_with("bas"))
                    
                    ratio<-((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas)*32)+((mand+max+mol+inc)*114))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtc+mtt+car+tar+pha+bas
                    axis.var.name<-"ratio PCRAP/CR%"
                  },
                  "6"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
                    mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
                    max<-dplyr::select(data.df.calcul.2,starts_with("max"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
                    mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
                    inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
                    
                    
                    ratio<-((rad+tib+fem+hum+uln)*32)/(((rad+tib+fem+hum+uln)*32)+((mand+max+mol+inc)*10))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc
                    
                    axis.var.name<-"ratio PCRLB/CR%"
                  },
                  "7"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
                    mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
                    mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
                    
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
                    tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
                    car<-dplyr::select(data.df.calcul.2,starts_with("car"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
                    pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
                    pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
                    
                    ratio<-(mtc+mtp+tar+pha+car)*12/(((mtc+mtt+tar+pha+car)*12)+(tib+rad+uln+hum+fem+pat)*18)
                    somme.ratio<-mtc+mtp+tar+pha+car+tib+rad+uln+fem+pat
                    axis.var.name<-"ratio AUT/ZE%"
                  },
                  "8"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("Rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Ulna"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("Ulna"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("Tib"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("Fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("Hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    
                    ratio<-((rad+tib+uln)*4)/((tib+rad+uln)*4+((hum+fem)*6))
                    somme.ratio<-rad+tib+uln+hum+fem
                    axis.var.name<-"ratio Z/E%"
                  },
                  "9"={
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
                    fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
                    hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("sca"))) > 0 ,"No 'sca' or 'scapula' elements found in the database"))
                    sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
                    rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
                    uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
                    tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
                    mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
                    validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
                    mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
                    
                    ratio<-(hum+sca+rad+mtc)*12/(((hum+sca+rad+uln+mtc)*12)+(fem+tib+mtt)*16)
                    somme.ratio<-hum+sca+rad+uln+mtc+fem+tib+mtt
                    axis.var.name<-"AN/PO%"
                    axis.var.name<-"ratio AN/PO%"
                  }
           )
           
           f_vec <-Vectorize(WilsonBinCI, vectorize.args = c("n","p"), SIMPLIFY = FALSE)
           
           data.df.calcul.anpo<-matrix(unlist(f_vec(c(somme.ratio),c(ratio))),ncol=2, byrow=F)
           
           df.ratio<-cbind.data.frame(data.df.calcul.2[,setlevels],ratio,data.df.calcul.anpo)
           
           colnames(df.ratio)<-c(setlevels,"ratio","lower","upper")
           
           if (!is.null(factor.order.level.activation())){
             df.ratio[[setlevels]]<-factor(df.ratio[[setlevels]], levels = factor.order.level())
           }
           
           print(df.ratio)
           p <- ggplot2::ggplot(df.ratio, 
                                aes(x = .data[["ratio"]]*100, y = .data[["name_level"]], xmin = .data[["lower"]]*100, xmax = .data[["upper"]]*100))+ 
             scale_x_continuous(limits=c(0,100))
           p<-p+geom_pointrange()+
             xlab(paste(axis.var.name))+ylab(paste("name_level"))+
             do.call(themeforfigure.choice(), list()) +
             theme(axis.title.x = element_text(size=font_size()),
                   axis.title.y = element_text(size=font_size()),
                   axis.text.x = element_text(size=font_tick()),
                   axis.text.y = element_text(size=font_tick()),
                   legend.title = element_blank())+
             theme(legend.position=legendplotlyfig())
           p
           
           
         }) 
         
         output$Ratio.data.list=renderUI({
           req(!is.null(fileisupload()))
           selectInput("select.ratio", label = h5("Select the ratio to plot"), 
                       choices = list("CRA/POSTCRA%" = 1, "AN/PO% (1)" = 2,"AN/PO% (2)" = 9,"AUT/ZE%"=7,"Z/E%" = 8, "PCRLB/CR%"=6, "PCRAP/CR%"=5,"PCRT/CR%"=4,"Proportion digested element" = 3 ), 
                       selected = 1)
         })
         
         output$liste.summary=renderUI({
           req(!is.null(fileisupload()))
           checkboxGroupInput("listesum", h4("Variables for summary table"),
                              choices = names(df$df)[c(1:ncol(df$df))])
         })
         
         output$summary <- renderTable({
           Pivotdatatable()
       
           
         },digits=0)
         
         output$downloadData_pivotdata<- downloadHandler( 
           filename = function() {
             paste0(Sys.Date(),"_pivot.table",".csv")
           },
           content = function(file) {
             write.table(Pivotdatatable(), file, row.names = FALSE, sep=";",dec=".")
           }
         )
         

    Pivotdatatable<-reactive({req(input$listesum)
           df.sub<-df.sub()
           liste.sum<-c(input$listesum,"nb_remains") # creation d'une liste
           table_matos<-df.sub %>% group_by(across(liste.sum)) %>% summarize(n_row=n()    )
           str(table_matos)
           colnames(table_matos)<-c(unlist(liste.sum),"n_row")
           table_matos$nb_pt_tot<-table_matos[["nb_remains"]]*table_matos[["n_row"]]
           table_matos<-table_matos %>% dplyr::select (-c("nb_remains"))
           table_matos})
         
         
         output$table.species <-  DT::renderDataTable({
           data.df.tot2<-df.species.table()
           DT::datatable(
             data= data.df.tot2, 
             extensions = 'Buttons', options = list(
               lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
               pageLength = 15,
               initComplete = htmlwidgets::JS(
                 "function(settings, json) {",
                 paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                 "}")
             ))
         })#end renderDataTable
         output$downloadData_speciesdata<- downloadHandler( 
           filename = function() {
             paste0(Sys.Date(),"_species.table",".csv")
           },
           content = function(file) {
             write.table(df.species.table(), file, row.names = FALSE, sep=";",dec=".") ## to MODIF !!!
           }
         ) 
} ## end of server 


ui <-dashboardPage(
  dashboardHeader(title = "MICRO-TRI"),
  sidebar,
  body )## end of ui
saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
        responses <<- rbind(responses, data)
      } else {
        responses <<- data
    }
    
    
}

loadData <- function() {
    if (exists("responses")) {
      # get("data_df", envir=e())
        responses
        
    }
}
saveData.line <- function() {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
