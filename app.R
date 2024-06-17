library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tools)
library(readxl)
setwd("C:/Users/aurelien/Desktop/R shiny software/TRIS")
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


list_bone_1<-c("not_selected","m1inf","MOL","Mand","Max","HUM","FEM","RAD","ULNA","TIB",
             "Iinf","Isup","bassin")
list_bone<-c("list_bone_1")
sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs",
        menuItem("Dashboard database", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Acquisition", icon = icon("list"), tabName = "SMALLvert"
                 # ,
                 # badgeLabel = "new"
                 # , badgeColor = "green"
                 ),
        menuItem("Exploration", tabName = "data2", icon = icon("chart-line")),
        menuItem("Note", tabName = "data3", icon = icon("comment"))
    )
)

body <- dashboardBody(
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
                         uiOutput("set.levels") 
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
                h2("analyses2 tab content"),
                
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
  fields_theor <- c("date_record","ID_dec","name_square","name_dec",
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
    
    # save.df.last.lines<-reactiveValuesToList(NULL)

    
## NEW BDD
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
  
  showModal(
    modalDialog(
      title = tags$h4(style = "color: blue;","Database create!"),
      easyClose = T,
      HTML("The database is created"),
      
    ))
  
})

##loading
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
observeEvent(getdata.launch(), {
  req(!is.null(input_file1.datapath()))
  extension <- tools::file_ext(input_file1.name())
  validate(need(extension == "rds", "Please upload a rds file"))
  global<-readRDS(file=input_file1.name())

  df$df<-global$df
  ID_record(global$k)
  responses <<- global$df
  df$save1<-global$df_save1
  df$save2<-global$df_save2
  df$save3<-global$df_save3
  df$save4<-global$df_save4
  df$save5<-global$df_save5

  
  rV$ID_dec<-global$ID_dec
  rV$name_square<-global$name_square
  rV$name_dec<-global$name_dec
  rV$name_level<-global$name_level
  last.id.dec(global$last.id.dec)
  last.name.square(global.load$last.name.square)
  last.name.dec(global.load$last.name.dec)
  last.name.level(global.load$last.name.level)
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
output$set.ID.dec=renderUI({
  req(!is.null(file.field.BDD.isupload()))
  selectInput("setIDdec", h4("ID of split (Default name: ID)"),
              choices = names(global.load$BDD.field)[c(1:ncol(global.load$BDD.field))],
              selected = liste.set.ID.dec())
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

##incrementation
    output$value_ID_dec <- renderText({ input$ID_dec })
    output$value_name_square <- renderText({ input$name_square })
    output$value_name_dec <- renderText({ input$name_dec })
    output$value_name_level <- renderText({ input$name_level })
    
    rV <- reactiveValues(ID_dec = "")
    rV <- reactiveValues(name_square = "")
    rV <- reactiveValues(name_dec = "")
    rV <- reactiveValues(name_level = "")
    last.id.dec<-reactiveVal(NULL)
    last.name.square<-reactiveVal(NULL)
    last.name.dec<-reactiveVal(NULL)
    last.name.level<-reactiveVal(NULL)
    
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
        textInput("txt_photo", label="photo information", value = "", width = NULL,
                  placeholder = NULL)
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
      last.name.dec(input$name_dec)
      last.name.level(input$name_level)
    })
  
    output$txt.field.data<-renderUI({
      aa<-global.load$BDD.field[global.load$BDD.field[,input$setIDdec] == input$ID_dec, ]
      temp.square.ID<-as.character(aa[1,c(input$setIDdec,input$setsquare,input$set.dec,input$setlevels)])
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
    observeEvent(ignoreInit = T,input$setIDdec, {
      req(!is.null(file.field.BDD.isupload()))
      rV$ID_dec <- c(rV$ID_dec,global.load$BDD.field[,input$setIDdec])
    })
    observeEvent(ignoreInit = T,input$setsquare, {
      req(!is.null(file.field.BDD.isupload()))
      rV$name_square <- c(rV$name_square,global.load$BDD.field[,input$setsquare])
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
         data$dig_I<-"NULL"
         data$dig_MOL<-"NULL"
         data$dig_m1inf<-"NULL"
         data$dig_bone<-"NULL"
     
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
        saveData(formData())
        data <- as.data.frame(t(formData()))
        k<-ID_record()+1
        ID_record(k)
        global.load$k<-k
        df$save1<-df$save2
        df$save2<-df$save3
        df$save3<-df$save4
        df$save4<-df$save5
        df$save5<-data
        global.load$df<-responses
        global.load$df_save1<-df$save1
        global.load$df_save2<-df$save2
        global.load$df_save3<-df$save3
        global.load$df_save4<-df$save4
        global.load$df_save5<-df$save5
        global.load$ID_dec<-rV$ID_dec
        global.load$name_square<-rV$name_square
        global.load$name_dec<-rV$name_dec
        global.load$name_level<-rV$name_level
        global.load$last.id.dec<-last.id.dec()
        global.load$last.name.square<-last.name.square()
        global.load$last.name.dec<-last.name.dec()
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
          # $ infos_suppl_anat          : NULL
          # $ traces_patine             : NULL
          # $ trace_encoche             : NULL
          updatePrettySwitch(session = session, inputId = "infos_obs",value = FALSE)
          to_save <- reactiveValuesToList(global.load)
          saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
          # test <- data.frame(sapply(responses,unlist))
          test<-data.frame(apply(responses,2,as.character))
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
      responses <<- rbind(responses, temp)
      refresh_dtoutput(rnorm(1,mean=100,sd=100))
      
    })
   
    
   refresh_dtoutput<-reactiveVal(0)
    observeEvent(input$submit2,{
      refresh_dtoutput(rnorm(1,mean=100,sd=100))
    })

    output$responses <- DT::renderDataTable({
        # input$submit2
      refresh_dtoutput()
            loadData()
    }, server = FALSE,
    callback = JS(c("table.page('last').draw(false);")),
  
    options = list(lengthMenu = c(1, 2,3, 5,10,25,50), pageLength = 5)
    )
    
    output$responses2 <- DT::renderDataTable({
       datatable(loadData(), editable = TRUE)
    } ,   options = list(lengthMenu = c(1, 2,3, 5,10,25,50), pageLength = 5)
    )
    observeEvent(input$responses2_cell_edit, {
      data2<-loadData()
      row  <- input$responses2_cell_edit$row
      clmn <- input$responses2_cell_edit$col
      data2[row, clmn] <- input$responses2_cell_edit$value
      responses <<- data2
    })
    observeEvent(input$deleteRows,{
      if (!is.null(input$responses2_rows_selected)) {
        data2<-loadData()
        data2 <-  data2[-as.numeric(input$responses2_rows_selected),]
        responses <<- data2
      }
    })
    
#     output$obs.note.torender <- renderText({ global.load$note.obs })
#     observeEvent(input$note.obs,{
# global.load$note.obs<-c(global.load$note.obs,input$note.obs)
#       
#     })
    
         output$obs.note.torender <- renderText({ global.load$note.obs })
         
         observeEvent(input$Record_the_observation,{
     global.load$note.obs<-paste0(global.load$note.obs,"<p>",input$note.obs,"</p>")
           
         })
    
} ## end of server 
ui <-dashboardPage(
  dashboardHeader(title = "MICRO-TRI"),
  sidebar,
  body )## end of ui
saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
print(responses)
      print(data)
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
# Run the application 
shinyApp(ui = ui, server = server)
