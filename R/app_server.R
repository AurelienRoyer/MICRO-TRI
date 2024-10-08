app_server <- function(input, output, session) {
  font.size <- "8pt"
  fields_theor <- c("date_record","year_exca","name_sector","ID_dec","name_square","name_dec",
                    "name_level","name_taxa" ,"name_species","name_anat", 
                    "infos_suppl_anat","nb_remains","infos_lat",
                    "infos_completude","infos_completude_detailled",
                    "trace_dig","trace_root","color_patine","trace_heat",
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
    list_info_suppl<-reactiveVal(c("No infos","T6","T9","supplementary triangle","Rhombe pitymien"))
    patine.list<-reactiveVal(c("","doubtful","pollution-white","pollution-recent"))
    themeforfigure.choice<-reactiveVal(c("theme_minimal"))
    font_size<-reactiveVal(12)
    font_tick<-reactiveVal(12)
    legendplotlyfig<-reactiveVal(TRUE) ##for legends.
    
    
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
  # global.load$patine.list.select<-global.load$patine.list.select
  global.load$other.list.select<-global$other.list.select
  global.load$herpeto.list.select<-global$herpeto.list.select
  global.load$chiro.list.select<-global$chiro.list.select
  global.load$euli.list.select<-global$euli.list.select
  global.load$rod.list.select<-global$rod.list.select
  global.load$note.obs<-global$note.obs
  list_info_suppl(global$list_info_suppl)
  patine.list(global$patine.list)
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
      list_info2<-list_info_suppl()
       selectizeInput(
        inputId = "observation_suppl",
        label = "Supplementary observation", 
        choices = list_info2,
        selected = "",
        multiple = TRUE,
        options = list(create = TRUE,
                                       `live-search` = TRUE)
      
        
      )
      
    })
    
    observeEvent(input$observation_suppl, {
      list_info_suppl(levels(as.factor(c(list_info_suppl(),input$observation_suppl))))
 
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
 
output$patine_pickerinput=renderUI({
      # patine.menu<-get(global.load$patine.list.select)
  patine.list<-patine.list()
       selectizeInput(
        inputId = "color_patine",
        label = "color patine", 
        choices = patine.list,
        options = list(create = TRUE,
                       `live-search` = TRUE)
      )
    })

observeEvent(input$color_patine, {
  patine.list(levels(as.factor(c(patine.list(),input$color_patine))))
  
})      

    output$tm_output=renderUI({
      if (input$infos_tm==TRUE) {
        radioGroupButtons(
          inputId = "trace_tooth_mark",
          label = "Tooth marks",
          choices = c("?","1","multiple","opposite"),
          selected =("1"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"),
            no = icon("remove",
                      lib = "glyphicon"))
        )
      }
      
    }) 
    
    output$enc_output=renderUI({
      if (input$infos_enc==TRUE) {
        radioGroupButtons(
          inputId = "trace_encoche",
          label = "Pit",
          choices = c("?","1","multiple","opposite"),
          selected =("1"),
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
      if(input$name_anat=="Mol"){
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
        input_infos_suppl_anat("0")
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
        input_infos_suppl_anat("0")
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
      # print(input$Id_max)
      # if(!is.null(input$Id_max))
      # {input_infos_suppl_anat(input$Id_max)}
      # else{input_infos_suppl_anat("0")}
    })
    observeEvent(input$Id_mand, {
      input_infos_suppl_anat(input$Id_mand)
      
    })
###saving ----
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
        
        if (length(levels(as.factor(ID_record())))==0){
          ID_record2<-1
        } else {ID_record2<-ID_record()}
        
        data<-c(ID_record2,data)
        
        # data<-c(ID_record(),data)
        names(data)<-c("ID_record",fields_theor)
        data$date_record<-Sys.time()
        if(!is.null(input_infos_suppl_anat())){
         data$infos_suppl_anat<-input_infos_suppl_anat()}
         data$dig_I<-"NA"
         data$dig_MOL<-"NA"
         data$dig_m1inf<-"NA"
         data$dig_bone<-"NA"
         data$dig_bone_others<-"NA"
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
         if (input$infos_enc==F) {data$trace_encoche<-"no"}
         if (input$infos_tm==F) {data$trace_tooth_mark<-"no"}
           
        #"Mand","Max",
        if (!input$trace_dig=="IND"){
          data$dig_bone_others<-input$trace_dig
         switch(input$name_anat,

                Iinf = {
                  data$dig_I<-input$trace_dig
                  data$dig_bone_others<-"NA"
                },
                Isup =   {
                  data$dig_I<-input$trace_dig
                  data$dig_bone_others<-"NA"
                },
                m1inf =   {
                  data$dig_m1inf<-input$trace_dig
                  data$dig_bone_others<-"NA"
                },
                MOL =   {
                  data$dig_MOL<-input$trace_dig
                  data$dig_bone_others<-"NA"
                },
                FEM =   {
                  data$dig_bone<-input$trace_dig
                  data$dig_bone_others<-"NA"
                },
                HUM =  { 
                  data$dig_bone<-input$trace_dig
                  data$dig_bone_others<-"NA"
                })
          } #end of if
         if (input$name_anat=="m1inf"){
           data$name_anat<-"Mol"
           data$infos_suppl_anat<-"m1inf"
         }
         if (input$name_anat=="Isup"){
           data$name_anat<-"Incisor"
           data$infos_suppl_anat<-"Isup"
         }         
         if (input$name_anat=="Iinf"){
           data$name_anat<-"Incisor"
           data$infos_suppl_anat<-"Iinf"
         }
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
        global.load$list_info_suppl<-list_info_suppl()
        input_infos_suppl_anat(NULL)
        global.load$patine.list<-patine.list()

        updateSelectizeInput(session = session, inputId = "name_species",selected ="not_selected")
        updatePickerInput(session = session, inputId = "infos_suppl_anat",selected =NULL,choices = NULL)
        updatePickerInput(session = session, inputId = "name_anat",choices = get(list_bone[1]))

        updateNumericInput(session = session, inputId = "nb_remains",value =1)
        updateRadioGroupButtons(session = session, inputId = "infos_lat", selected = "IND")  
        updatePrettySwitch(session = session, inputId = "infos_completude",value = FALSE)
        updateCheckboxGroupButtons(session = session, inputId = "infos_completude_detailled",selected = c(""))  
        updateRadioGroupButtons(session = session, inputId = "trace_dig",selected = "IND")
        updateRadioGroupButtons(session = session, inputId ="trace_root",selected = "0")
        updateRadioGroupButtons(session = session, inputId ="trace_heat",selected = "no")
        updatePrettySwitch(session = session, inputId = "infos_tm",value = FALSE)
        updatePrettySwitch(session = session, inputId = "infos_enc",value = FALSE)
        updateRadioGroupButtons(session = session, inputId ="trace_tooth_mark",selected = "")
        updateRadioGroupButtons(session = session, inputId ="trace_encoche",selected = "")
        updateTextInput(session = session, inputId = "observation",value = "")
        updateTextInput(session = session, inputId = "txt_photo",value = "No photo")
        updatePrettySwitch(session = session, inputId = "infos_photo",value = FALSE)
        updatePickerInput(session = session, inputId = "observation_suppl",selected = "")
        updateSelectizeInput(session = session, inputId = "color_patine",selected ="")

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
    
    curPgInd<-reactiveVal(1)  
    observeEvent(global.load$df, {
      user<-nrow(global.load$df)
      if(user == 0) {
        return ()
      }
      curPgInd(ceiling(user / 5))
    })
    
    output$responses <- DT::renderDataTable({
      # curPgInd <- ceiling(curRowInd() / defaultPgRows)
      # curPgInd <-5
      # pgLoadJS <- paste0('setTimeout(function() {table.page(', curPgInd - 1,').draw(false);}, 100);')
      refresh_dtoutput()
      global.load$df
     
    }, 
     # server = FALSE,
    # callback = JS("table.page('last').draw(false);"),
    # callback = JS(pgLoadJS),
    callback = JS(paste0('setTimeout(function() {table.page(', curPgInd() - 1,').draw(false);}, 100);')),
    options = list(lengthMenu = c(1,2,3,5,10,25,50), pageLength = 5)
    )
    
    output$responses2 <- DT::renderDataTable({
       datatable(
         global.load$df,
         editable = TRUE)
    } ,   
    options = list(lengthMenu = c(1, 2,3, 5,10,25,50), pageLength = 5)
    )
    observeEvent(input$responses2_cell_edit, {
      data2<-global.load$df
      row  <- input$responses2_cell_edit$row
      clmn <- input$responses2_cell_edit$col
      data2[row, clmn] <- input$responses2_cell_edit$value
      global.load$df<-data2
      # responses <<- data2
      to_save <- reactiveValuesToList(global.load)
      saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
      # write.table(as.data.frame(fields_theor), file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
      test<-data.frame(apply(global.load$df,2,as.character))
      write.table(test, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
      
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
         # write.table(as.data.frame(fields_theor), file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
         test<-data.frame(apply(global.load$df,2,as.character))
         write.table(test, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
         
      }
    })
    

         output$obs.note.torender <- renderText({ global.load$note.obs })
         
observeEvent(input$Record_the_observation,{
     global.load$note.obs<-paste0(global.load$note.obs,"<p>",input$note.obs,"</p>")
     to_save <- reactiveValuesToList(global.load)
     saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf",".rds"))
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

             df.sub <- df.sub[df.sub[["name_sector"]] %in% input$localisation, ]
             df.sub <- df.sub[df.sub[["name_level"]] %in% input$UAS, ]
             df.sub <- df.sub[df.sub[["name_dec"]]%in% input$Passe, ]
             df.sub <- df.sub[df.sub[["name_square"]] %in% input$Square, ]
             df.sub <- df.sub[df.sub[["year_exca"]] %in% input$Year, ]

             df.sub[,1:29][df.sub[,1:29]=="NULL"] <- "NA"
               # df.sub<-as.data.frame(t(apply(df.sub,2, function(x) unlist(x))))
             if(nrow(df.sub)>1){
               df.sub<-as.data.frame(apply(df.sub,2, function(x) as.character(x)))}
               assign("temppp",df.sub,envir = .GlobalEnv)
               
             validate(need(nrow(df.sub)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
             df.sub
             df.sub$nb_remains<-as.numeric(df.sub$nb_remains)

           #### creation de df.species.table
           # data.df.tot<-as.data.frame(t(apply(df.sub,1, function(x) unlist(x))))
           # data.df.tot$nb_remains<-as.numeric(data.df.tot$nb_remains)
           data.df.tot<-df.sub
           # data.df.tot2<-data.df.tot %>% group_by(.data[["name_level"]],.data[["name_species"]])  %>% 
           #   dplyr::summarise(total = sum(!!(as.numeric(data.df.tot$nb_remains))))
           data.df.tot2<-data.df.tot %>% group_by(name_level,name_species)  %>% 
             dplyr::summarise(total = sum((as.numeric(nb_remains))))
           
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
           res_lda<-func_LDA(BCI_LVLn_of_siteS, quantiv = TRUE) ########################################probleme ici
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
           taxNamesTot <- as.character(unlist(data_species_biozone()["Taxon"]))
           
           
           # data.sp.used<- mutate_all(data.sp.used, .funs=stringr::str_to_lower)
           data.sp.used<-stringr::str_to_lower(data.sp.used)
           id_noused <- which(!is.element(data.sp.used, taxNamesTot))
           names_noused <- data.sp.used[id_noused]
           tagList(h5(style = "color: red;",HTML(paste(names_noused))))
           
         })
         
######  Ratio graphs ---- 
         #option for ratio
         output$themeforfigure=renderUI({
           req(!is.null(fileisupload()))
           themes <- c("theme_bw", "theme_classic", "theme_dark", "theme_grey", "theme_light", "theme_linedraw", "theme_minimal")
           selectInput("themeforfigure.list", h4("Theme for 'Simple 2Dplot'"),
                       choices = themes,
                       selected = "theme_minimal")
         })
         
         observeEvent(input$themeforfigure.list,{
           themeforfigure.choice(c(input$themeforfigure.list))
           
         })
         
         observeEvent(input$fontsizeaxis, {
           font_size(input$fontsizeaxis)
         }) 
         observeEvent(input$fontsizetick, {
           font_tick(input$fontsizetick)
         }) 
         
         observeEvent(input$optioninfosfigplotly, {
           legendplotlyfig(input$optioninfosfigplotly)
         })
         #
         
         
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
            setlevels<-input$setlevels
            setanat<-input$setanat
            setnb<-input$setnb
           digcol<-input$digcol
           # nameofdigelement<-input$nameofdigelement
           df.sub$nb_remains<-as.numeric(df.sub$nb_remains)
           data.df.calcul<-df.sub %>% group_by(.data[["name_level"]],.data[["name_anat"]])%>%
             summarize(nb_total = sum(!!sym("nb_remains")))
           myFormula <- as.formula(paste0("name_level", " ~ ","name_anat"))
           data.df.calcul.verif<-reshape2::dcast(data.df.calcul, myFormula , fill = 0L)
           nom.col<-colnames(data.df.calcul.verif)
           
           list.element<-c(list_bone_1)
           new.element <-setdiff(list.element,nom.col)
           new.element.tab<-matrix(data=0,ncol
                                   =length(new.element))
           colnames(new.element.tab)<-new.element
           data.df.calcul.2<- cbind(data.df.calcul.verif,new.element.tab)
           
           fem<-dplyr::select(data.df.calcul.2,starts_with("Fem"))
           hum<-dplyr::select(data.df.calcul.2,starts_with("Hum"))
           rad<-dplyr::select(data.df.calcul.2,starts_with("Rad"))
           uln<-dplyr::select(data.df.calcul.2,starts_with("Ulna"))
           tib<-dplyr::select(data.df.calcul.2,starts_with("Tib"))
           mand<-dplyr::select(data.df.calcul.2,starts_with("Mand"))
           max<-dplyr::select(data.df.calcul.2,starts_with("Max"))
           # mol<-dplyr::select(data.df.calcul.2,starts_with("Mol"))+dplyr::select(data.df.calcul.2,starts_with("m1inf"))
           # inc<-dplyr::select(data.df.calcul.2,starts_with("Iinf"))+dplyr::select(data.df.calcul.2,starts_with("Isup"))
           mol<-dplyr::select(data.df.calcul.2,starts_with("Mol"))
           inc<-dplyr::select(data.df.calcul.2,starts_with("Incisor"))
           mtpod<-dplyr::select(data.df.calcul.2,starts_with("Mtpod"))
           
           sca<-dplyr::select(data.df.calcul.2,starts_with("Scap"))
           tar<-dplyr::select(data.df.calcul.2,starts_with("Tars"))
           car<-dplyr::select(data.df.calcul.2,starts_with("Carp"))
           pha<-dplyr::select(data.df.calcul.2,starts_with("Pha"))
           pat<-dplyr::select(data.df.calcul.2,starts_with("Pat"))
           bas<-dplyr::select(data.df.calcul.2,starts_with("Innominate"))
           vert<-dplyr::select(data.df.calcul.2,starts_with("Vert"))
           rib<-dplyr::select(data.df.calcul.2,starts_with("Rib"))
           
           switch(input$select.ratio,
                  "1"={

                    valide.sup.base<-length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                    valide.sup.0<-length(dplyr::select(data.df.calcul.verif,starts_with("Mand")))+length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                    
                    validate(need(valide.sup.base > 0 ,"No 'Hum' or 'Fem'  elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No 'mand', 'Hum' and 'Fem'  elements found in the database"))
          
                    axis.var.name<-"CRA/POSTCRA%"
                    # ratio<-dplyr::select(data.df.calcul.2,starts_with("Mand"))/(dplyr::select(data.df.calcul.2,starts_with("Mand"))+dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum")))
                    ratio<-mand/(mand+fem+hum)
                    # somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("Mand"))+dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    somme.ratio<-mand+fem+hum
                    axis.var.name<-"ratio CRA/POSTCRA %"
                  },
                  "2"={
                    valide.sup.base<-length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                    valide.sup.0<-length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                    
                    validate(need(valide.sup.base > 0 ,"No 'Hum'and 'Fem' elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No 'Hum' elements found in the database"))
                    
                    axis.var.name<-"AN/PO%"
                    # ratio<-dplyr::select(data.df.calcul.2,starts_with("Hum"))/(dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum")))
                    # somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("Fem"))+dplyr::select(data.df.calcul.2,starts_with("Hum"))
                    ratio<-hum/(fem+hum)
                     somme.ratio<-fem+hum
                    axis.var.name<-"ratio AN/PO %"
                  },
                  "3"={},
                  "4"={
                    
                    valide.sup.base<-length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Scap")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mtpod")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pat")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Carp")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tars")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pha")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Innominate")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Vert")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rib")))
                    valide.sup.0<-valide.sup.base+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mol")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("m1inf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mand")))+
                    length(dplyr::select(data.df.calcul.verif,starts_with("Max")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Iinf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Isup")))
                    
                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone  elements found in the database"))
                    
                    ratio<-((rad+tib+fem+hum+uln+sca+pat+mtpod+car+tar+pha+bas+vert+rib)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtpod+car+tar+pha+bas+vert+rib)*32)+((mand+max+mol+inc)*184))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtpod+car+tar+pha+bas+vert+rib
                    axis.var.name<-"ratio PCRT/CR%"
                    
                  },
                  "5"={
                    valide.sup.base<-length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Scap")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mtpod")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pat")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Carp")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tars")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pha")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Innominate")))
                    valide.sup.0<-valide.sup.base+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mol")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("m1inf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mand")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Max")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Iinf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Isup")))
                    
                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone elements found in the database"))
                    
                    ratio<-((rad+tib+fem+hum+uln+sca+pat+mtpod+car+tar+pha+bas)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtpod+car+tar+pha+bas)*32)+((mand+max+mol+inc)*114))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtpod+car+tar+pha+bas
                    axis.var.name<-"ratio PCRAP/CR%"
                  },
                  "6"={
                    valide.sup.base<-length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))
                    valide.sup.0<-valide.sup.base+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mol")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("m1inf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mand")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Max")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Iinf")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Isup")))
                    
                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone elements found in the database"))
                    
                    
                    ratio<-((rad+tib+fem+hum+uln)*32)/(((rad+tib+fem+hum+uln)*32)+((mand+max+mol+inc)*10))
                    somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc
                    
                    axis.var.name<-"ratio PCRLB/CR%"
                  },
                  "7"={
                    valide.sup.base<-
                      length(dplyr::select(data.df.calcul.verif,starts_with("Scap")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mtpod")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Carp")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tars")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pha")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Innominate")))
                    valide.sup.0<-valide.sup.base+length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Pat")))
                      
                    
                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone elements found in the database"))
                    ratio<-((mtpod/2)+tar+pha+car)*12/((((mtpod/2)+tar+pha+car)*12)+(tib+rad+uln+hum+fem+pat)*18)
                    somme.ratio<-mtpod+tar+pha+car+tib+rad+uln+fem+pat
                    axis.var.name<-"ratio AUT/ZE%"
                  },
                  "8"={
                    valide.sup.base<-
                      length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))
                    valide.sup.0<-valide.sup.base+length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                     
                    
                    
                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone elements found in the database"))
                    
                    ratio<-((rad+tib+uln)*4)/((tib+rad+uln)*4+((hum+fem)*6))
                    somme.ratio<-rad+tib+uln+hum+fem
                    axis.var.name<-"ratio Z/E%"
                  },
                  "9"={
                    valide.sup.base<-   length(dplyr::select(data.df.calcul.verif,starts_with("Rad")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Ulna")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Scap")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Mtpod")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Hum")))
                    valide.sup.0<-valide.sup.base+length(dplyr::select(data.df.calcul.verif,starts_with("Fem")))+
                      length(dplyr::select(data.df.calcul.verif,starts_with("Tib")))

                    validate(need(valide.sup.base > 0 ,"No bone elements found in the database"))
                    validate(need(valide.sup.0 > 0 ,"No bone elements found in the database"))
                    
                    ratio<-(hum+uln+sca+rad+(mtpod/2))*12/(((hum+sca+rad+uln+(mtpod/2))*12)+(fem+tib+(mtpod/2))*16)
                    somme.ratio<-hum+sca+rad+uln+fem+tib+mtpod
                    axis.var.name<-"AN/PO%"
                    axis.var.name<-"ratio AN/PO%"
                  }
           )
           ## test de somme.ratio si =0
            # somme.ratio<-somme.ratio[-(which(rowSums(somme.ratio)==0)),]
          ##
           ratio<-unlist(ratio)
           somme.ratio<-unlist(somme.ratio)
           
           f_vec <-Vectorize(WilsonBinCI, vectorize.args = c("n","p"), SIMPLIFY = FALSE)
           
           data.df.calcul.anpo<-matrix(unlist(f_vec(c(somme.ratio),c(ratio))),ncol=2, byrow=F)
           
           df.ratio<-cbind.data.frame(data.df.calcul.2["name_level"],ratio,data.df.calcul.anpo)
           
           colnames(df.ratio)<-c("name_level","ratio","lower","upper")
           
           ############################################################################################a creer pour ordonner niveau
           # if (!is.null(factor.order.level.activation())){
           #   df.ratio[[setlevels]]<-factor(df.ratio[[setlevels]], levels = factor.order.level())
           # }
           
           p <- ggplot2::ggplot(df.ratio, 
                                ggplot2::aes(x = .data[["ratio"]]*100, y = .data[["name_level"]], xmin = .data[["lower"]]*100, xmax = .data[["upper"]]*100))+ 
             scale_x_continuous(limits=c(0,100))
           p<-p+geom_pointrange()+
             xlab(paste(axis.var.name))+ylab(paste("name_level")) +
             do.call(themeforfigure.choice(), list()) +
             theme(axis.title.x = element_text(size=font_size()),
                   axis.title.y = element_text(size=font_size()),
                   axis.text.x = element_text(size=font_tick()),
                   axis.text.y = element_text(size=font_tick()),
                   legend.title = element_blank())+
             theme(legend.position='none')
           p
           
           
         }) 
         
         output$Ratio.data.list=renderUI({
           req(!is.null(fileisupload()))
           selectInput("select.ratio", label = h5("Select the ratio to plot"), 
                       choices = list("CRA/POSTCRA%" = 1, "AN/PO% (mand+hum+fem)" = 2,"AN/PO% (2)" = 9,"AUT/ZE%"=7,"Z/E%" = 8, "PCRLB/CR%"=6, "PCRAP/CR%"=5,"PCRT/CR%"=4,"Proportion digested element" = 3 ), 
                       selected = 1)
         })
         output$Ratio.data.dig.graph <- renderUI({
           plotOutput("Ratiodatagraph.dig"
                      # , height = height.size(), width = width.size()
           )
         })
         
         output$Ratiodatagraph.dig <- renderPlot({
           plot(Ratiodatagraph.dig.plot())
           session_store$Ratiodatagraph.dig.plot<- Ratiodatagraph.dig.plot()
         })   
         
         Ratiodatagraph.dig.plot<-reactive({
           df.sub<-df.sub()
           setlevels<-input$setlevels
           setanat<-input$setanat
           setnb<-input$setnb
           # digcol<-c("dig_I","dig_MOL","dig_m1inf","dig_bone","dig_bone_others")
           df.sub$nb_remains<-as.numeric(df.sub$nb_remains)
           data.df.calcul.gh<-df.sub %>% group_by(.data[["name_level"]],.data[["name_anat"]],.data[["dig_I"]],
                                                  .data[["dig_MOL"]],.data[["dig_m1inf"]],
                                                  ,.data[["dig_bone"]],.data[["dig_bone_others"]])%>%
             summarize(nb_total = sum(!!sym("nb_remains")))
           
          
           list.element<-c("0","1","2","3","4","IND")
           
          
           switch(input$select.ratio.dig,
                  "1"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Fem")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Fem' elements found in the database"))

                    FEM.dig<-FEM.dig[,c(1,2,7,8)]
                    digcol<-"dig_bone_others"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% Fem dig"
                    
                  },
                  "2"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Hum")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Hum' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,7,8)]
                    digcol<-"dig_bone_others"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% Hum dig"
                  },
                  "3"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Hum" | name_anat == "Fem" )
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Hum' and 'Fem' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,7,8)]
                    digcol<-"dig_bone_others"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% Hum dig"
                  },
                  "4"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Iinf")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Iinf' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,3,8)]
                    digcol<-"dig_I"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% Iinf dig"
                  },
                  "5"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Isup")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Isup' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,3,8)]
                    digcol<-"dig_I"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% Isup dig"
                   
                  },
                  "6"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "Isup"| name_anat == "Iinf")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'Incisor' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,3,8)]
                    digcol<-"dig_I"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% incisor dig"
                    
                  },
                  "7"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "m1inf")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'm1inf' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,5,8)]
                    digcol<-"dig_m1inf"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% m1inf dig"
                    
                  },
                  "8"={
                    FEM.dig<-subset(data.df.calcul.gh, name_anat == "MOL")
                    validate(need(nrow(FEM.dig) > 0 ,"No 'MOL' elements found in the database"))
                    
                    FEM.dig<-FEM.dig[,c(1,2,4,8)]
                    digcol<-"dig_MOL"
                    myFormula <- as.formula(paste0("name_level", " ~ ",digcol))
                    data.df.calcul.verif<-reshape2::dcast(FEM.dig, myFormula , fill = 0L)
                    new.element <-setdiff(list.element,colnames(data.df.calcul.verif))
                    new.element.tab<-matrix(data=0,ncol
                                            =length(new.element),dimnames =list(c(),new.element))
                    
                    data.df.calcul.verif<- cbind(data.df.calcul.verif,new.element.tab)
                    somme.ratio<-rowSums(data.df.calcul.verif[2:ncol(data.df.calcul.verif)])
                    ratio<-rowSums(data.df.calcul.verif[3:6])/(rowSums(data.df.calcul.verif[2:6])+somme.ratio)
                    axis.var.name<-"% MO dig"
                  }
           )
           ## test de somme.ratio si =0
           # somme.ratio<-somme.ratio[-(which(rowSums(somme.ratio)==0)),]
           ##
           print(data.df.calcul.verif)
           
           f_vec <-Vectorize(WilsonBinCI, vectorize.args = c("n","p"), SIMPLIFY = FALSE)
           
           data.df.calcul.anpo<-matrix(unlist(f_vec(c(somme.ratio),c(ratio))),ncol=2, byrow=F)
           
           df.ratio<-cbind.data.frame(data.df.calcul.2["name_level"],ratio,data.df.calcul.anpo)
           
           colnames(df.ratio)<-c("name_level","ratio","lower","upper")
           
           ############################################################################################a creer pour ordonner niveau
           # if (!is.null(factor.order.level.activation())){
           #   df.ratio[[setlevels]]<-factor(df.ratio[[setlevels]], levels = factor.order.level())
           # }
           
           p <- ggplot2::ggplot(df.ratio, 
                                ggplot2::aes(x = .data[["ratio"]]*100, y = .data[["name_level"]], xmin = .data[["lower"]]*100, xmax = .data[["upper"]]*100))+ 
             scale_x_continuous(limits=c(0,100))
           p<-p+geom_pointrange()+
             xlab(paste(axis.var.name))+ylab(paste("name_level")) +
             do.call(themeforfigure.choice(), list()) +
             theme(axis.title.x = element_text(size=font_size()),
                   axis.title.y = element_text(size=font_size()),
                   axis.text.x = element_text(size=font_tick()),
                   axis.text.y = element_text(size=font_tick()),
                   legend.title = element_blank())+
             theme(legend.position='none')
           p
           
           
         }) 
         
         output$Ratio.dig.list=renderUI({
           req(!is.null(fileisupload()))
           selectInput("select.ratio.dig", label = h5("Select the ratio to plot"), 
                       choices = list("Proportion digested hum" = 1,
                                      "Proportion digested fem" = 2,
                                      "Proportion digested bones" = 3,
                                      "Proportion digested Iinf" = 4,
                                      "Proportion digested Isup" = 5,
                                      "Proportion digested I" = 6,
                                      "Proportion digested m1inf" = 7,
                                      "Proportion digested Mol" = 8
                                      ), 
                       selected = 7)
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
         
         
## fusion old BDD ----
file.old.BDD.isupload<-reactiveVal(NULL)
getdata.oldBDD.launch<-reactiveVal()
input_oldBDD.name<-reactiveVal()
input_oldBDD.datapath<-reactiveVal()
         
observeEvent(input$file.oldBDD, {
           input_oldBDD.name(input$file.oldBDD$name)
           input_oldBDD.datapath(input$file.oldBDD$datapath)
         })
observeEvent(input$getData.old.BDD, {
           getdata.oldBDD.launch(input$getData.old.BDD)
         })
observe({
           req(!is.null(input_oldBDD.datapath()))
           extension <- tools::file_ext(input_oldBDD.name())
           switch(extension,
                  csv = {updateSelectInput(session, "worksheet.old", choices = input_oldBDD.name())},
                  xls =   {    selectionWorksheet <-excel_sheets(path = input_oldBDD.datapath())
                  updateSelectInput(session, "worksheet.old", choices = selectionWorksheet)},
                  xlsx =  {      selectionWorksheet <-excel_sheets(path = input_oldBDD.datapath())
                  updateSelectInput(session, "worksheet.old", choices = selectionWorksheet)})
         })
         observeEvent(getdata.oldBDD.launch(), {
           req(!is.null(input_oldBDD.datapath()))
           extension <- tools::file_ext(input_oldBDD.name())
           global.load$BDD.old<- switch(extension,
                                          csv =  {    
                                            sep2 <- if( ";" %in% strsplit(readLines(input_oldBDD.datapath(), n=1)[1], split="")[[1]] ){";"
                                            } else if( "," %in% strsplit(readLines(input_oldBDD.datapath(), n=1)[1], split="")[[1]] ){","
                                            } else if ( "\t" %in% strsplit(readLines(input_oldBDD.datapath(), n=1)[1], split="")[[1]] ){"\t"
                                            } else {";"}
                                            utils::read.csv(input_oldBDD.datapath(),
                                                            header = TRUE,
                                                            sep = sep2, stringsAsFactors = F,  fileEncoding="latin1",
                                                            dec=".")},
                                          xls = readxl::read_xls(input_oldBDD.datapath(), sheet=input$worksheet.old),
                                          xlsx = readxl::read_xlsx(input_oldBDD.datapath(), sheet=input$worksheet.old))

           # fields_theor2<-c("ID_record",fields_theor,"dig_I"	,"dig_MOL",	"dig_m1inf",	"dig_bone",	"dig_bone_others")
           # assign(x="df",value=global.load$BDD.old, envir = .GlobalEnv)

           file.old.BDD.isupload(1)
         })#          
         observeEvent(ignoreInit = TRUE,file.old.BDD.isupload(),{ 
           
           global.load$df<-rbind.data.frame(global.load$df,global.load$BDD.old)
           global.load$k<-global.load$k+nrow(global.load$BDD.old)
           print(global.load$k)
           to_save <- reactiveValuesToList(global.load)
           saveRDS(to_save, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf.fusion",".rds"))
           test<-data.frame(apply(global.load$df,2,as.character))
           write.table(test, file =  paste0(Sys.Date(),".",global.load$site.archaeo,".BDD.uf.fusion",".csv",sep=""), row.names = FALSE, sep=";",dec=".") 
           
           
           
           print("BDD upload")
           } )

         
} ## end of server 
