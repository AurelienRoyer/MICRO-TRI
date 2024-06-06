library(shiny)
library(shinydashboard)
library(shinyWidgets)
# library(shinydashboardPlus)
library(shinyjs)
library(DT)
library(tools)
setwd("C:/Users/aurelien/Desktop/R shiny software/TRIS")
load(file = "list_faune.RData")
list_species_1<-c("not_selected",list_faune$Taxon)
list_species<-c("list_species_1")
list_bone_1<-c("not_selected","m1inf","Mand","Max","HUM","FEM","RAD","ULNA","TIB",
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
        menuItem("Exploration", tabName = "data2", icon = icon("chart-line"))
    )
)

body <- dashboardBody(
  useShinyjs(),
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Microvertebrate Toughscreen R Interface"),
                tabsetPanel(
                  id="tab1",
                    tabPanel(h4("Loading BDD"),
                             fileInput("file1", "Choose File (.RData)",
                                         accept = c(".RData")),
                               actionButton(inputId = "getData",label="Get Data"),
                             
                    ),#end of tabpanel
                tabPanel(h4("New BDD"),
  
                         uiOutput ("liste.faun4"),
                         actionButton("create_bdd", "Create the BDD"),
                         
                ),#end of tabpanel
                tabPanel(h4("About"),
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
                  box(width = 6,),
                  box(width = 3,
                numericInput("nb_remains", label = h5("Numeric input"), value = 1),
                  ),# end of box
                ),# end of fluidRow
                fluidRow(
                  box(width = 6,
                pickerInput(
                  inputId = "name_species",
                  label = "Name species", 
                  choices = get(list_species),
                  options = list(
                    `live-search` = TRUE)
                ),
                ),# end of box
                box(width = 6,
                    pickerInput(
                      inputId = "name_anat",
                      label = "Anatomy", 
                      choices = get(list_bone),
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
                ), #end of box
                ),
                
                fluidRow(
                  box(width = 4,
                prettySwitch(
                     inputId = "infos_completude",
                     label = "Complet or broke",
                     status = "success",
                     fill = TRUE
                   ),
              
                uiOutput ("completude"),
                  ),
                ),
                fluidRow(
                box(width = 4,
                sliderInput(
                  inputId = "trace_dig",
                  label = "Digestion marks",
                  min = 0, max = 4,
                  value = 0,step=1
                ),
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
            
                    
                    ),
                box(width=6,
                  prettySwitch(
                    inputId = "infos_obs",
                    label = "Observation",
                    status = "success",
                    fill = TRUE
                  ),
                  uiOutput ("obs"),
                )
                  )
                # ) # end of div
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
                    "observation")  
    ID_record<-reactiveVal(1)
    input_file1.name<-reactiveVal()
    input_file1.datapath<-reactiveVal()
    getdata.launch<-reactiveVal()  
    fileisupload<-reactiveVal(NULL)
    e<-reactiveVal(NULL)
    df<-reactiveValues( #creation df 
      df=NULL,save1=NULL,save2=NULL,save3=NULL,save4=NULL,save5=NULL) # end reactivevalues
    global.load<-reactiveValues(df=NULL,save5=NULL,site.archaeo="La Balutie")
    
    df_rapid_line<-reactiveVal(0)
    # save.df.last.lines<-reactiveValuesToList(NULL)
    ## NEW BDD
output$liste.faun4=renderUI({
      # selectInput("liste.newgroup3", label = h5("Select the variable"), 
      #             choices = factor(df$df[,input$liste.newgroup.rename]))
      selectInput("select", label = h5("Faunal List"), 
                  choices = list_species, 
                   selected = list_species[[1]]) 
    })

output$site=renderPrint({
  tags$p("Archaeological site")
  HTML(paste0("Archaeological site: ",global.load$site.archaeo))
 
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

##loading
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

  
  fileisupload(1)
  
})# end observe of df$df2
observeEvent(fileisupload(), {   
  updateTabItems(session, "tabs", "SMALLvert")
  
  updateTabsetPanel(inputId="tab1",
                    selected = "About")
})

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
      ))
      last.id.dec(input$ID_dec)
      last.name.square(input$name_square)
      last.name.dec(input$name_dec)
      last.name.level(input$name_level)
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
    
      
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
         # date<-Sys.time()
        data <- sapply(fields_theor, function(x) input[[x]])
       
        data<-c(ID_record(),data)
        names(data)<-c("ID_record",fields_theor)
        data$date_record<-Sys.time()
        str(data)
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
        updatePickerInput(session = session, inputId = "name_species",choices = get(list_species))
        updatePickerInput(session = session, inputId = "name_anat",choices = get(list_bone))
        updatePickerInput(session = session, inputId = "infos_suppl_anat",choices = NULL)
        updateNumericInput(session = session, inputId = "nb_remains",value =1)
        updateRadioGroupButtons(session = session, inputId = "infos_lat", selected = "IND")  
        updatePrettySwitch(session = session, inputId = "infos_completude",value = FALSE)
        updateCheckboxGroupButtons(session = session, inputId = "infos_completude_detailled",selected = c(""))  
        updateSliderInput(session = session, inputId = "trace_dig",value = 0)
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
    
    
    # observeEvent(input$goButton1,{
    #   temp<-df$save1
    #   df$save1<-df$save2
    #   df$save2<-df$save3
    #   df$save3<-df$save4
    #   df$save4<-df$save5
    #   df$save5<-temp
    #   responses <<- rbind(responses, temp)
    #   refresh_dtoutput(rnorm(1,mean=100,sd=100))
    # })
    
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
    
} ## end of server 
ui <-dashboardPage(
  dashboardHeader(title = "MICRO-TRI"),
  sidebar,
  body )## end of ui
saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {

        responses <<- rbind(responses, data)
        # assign("data_df",responses,envir=e())
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
