

app_ui <- function(){
  # shiny::addResourcePath("microTRI", system.file("R", package="microTRI"))
  
# load(file = "list_faune.RData")
# list_species_all<-c("not_selected",list_faune$Taxon)
# list_perso_rod<-c("not_selected","pm","am","Microtus_sp.","Microtus_arvalis","Microtus_agrestis",
#               "Arvicola_sp.","Arvicola_amphibius","Lasiopodomys_gregalis","Apodemus_sp.")
# list_perso_euli<-c("not_selected","Talpa_sp.","Erinaceus_europaeus","Crocidura_sp.","Sorex_sp.")
# list_perso_chiro<-c("not_selected","Pipistrellus_pipistrellus")
# list_perso_herpeto<-c("not_selected","Salamandra_salamandra")
# list_perso_others<-c("not_selected","small Passeriforme", "Large Birds","Cervid")
# list_species_rod<-c("list_perso_rod","list_species_all")
# list_species_euli<-c("list_perso_euli","list_species_all")
# list_species_chiro<-c("list_perso_chiro","list_species_all")
# list_species_herpeto<-c("list_perso_herpeto","list_species_all")
# list_species_others<-c("list_perso_others","list_species_all")
# 
# # list_info_suppl<-c("No infos","T6","T9","supplementary triangle","Rhombe pitymien")
# # patine.list<-c("","doubtful","pollution-white","pollution-recent")
# 
 list_bone_1<-c("not_selected","m1inf","Mol","Mand","Max","Hum","Fem","Rad","Ulna","Tin",
              "Iinf","Isup","Bassin")
# list_bone<-c("list_bone_1")
jsc <- '
$(document).ready(function () {
  $(".sidebar-menu").children("li").on("click", function() {
    $("#mult, #single").toggle();
  });
});12345
'

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(id = "tabs",
                
        menuItem("Dashboard database", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Acquisition", icon = icon("list"), tabName = "SMALLvert"
                 ),
        menuItem("Exploration", tabName = "data2", icon = icon("chart-line")),
        div(id = "single", style="display: none;", 
            actionButton(inputId = "refresh",label="refresh"),
            shiny::radioButtons(
              "bt2", h4("QUICK SIDEBAR"),
              choices = c("Species" = 1,
                          "Level " = 2,
                          "US"=6,
                          "Sector " = 3,
                          "Square" = 4,
                          "Year"=5,
                          
                          "Spit"=7),
              selected = "1", inline=TRUE), style = "font-size:70%",
            shinyWidgets::prettySwitch(
              inputId = "flip2",
              label = "Level or US",
              status = "success",
              fill = F
            ),
            tags$hr(),
            shiny::conditionalPanel(condition="input.bt2==1",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.species")),
            shiny::conditionalPanel(condition="input.bt2==4",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.square")),
            shiny::conditionalPanel(condition="input.bt2==2",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.UAS")),
            shiny::conditionalPanel(condition="input.bt2==3",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.sector")),
            shiny::conditionalPanel(condition="input.bt2==5",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.year")),
            shiny::conditionalPanel(condition="input.bt2==6",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.US")),
            shiny::conditionalPanel(condition="input.bt2==7",
                             h4(style = "font-size:70%","color: red;","Subsetting dataset"),
                             tags$br(),
                             shiny::uiOutput("liste.passe")),

            ),
        shinydashboard::menuItem("Note", tabName = "data3", icon = icon("comment"))
    )
)

body <- shinydashboard::dashboardBody(
  tags$head(tags$script(jsc)),
  useShinyjs(),
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Microvertebrate Toughscreen R Interface"),
                tabsetPanel(
                  id="tab1",
                    tabPanel(h4("Loading Database"),
                             shiny::fileInput("file1", "Choose File (.rds)",
                                         accept = c(".rds")),
                               actionButton(inputId = "getData",label="Get Data"),
                             
                    ),#end of tabpanel
                tabPanel(h4("New Database"),
                         column(4 ,textInput("name_site", label="Name of the site", value = "", width = NULL,
                                   placeholder = NULL),),
                         column(5 ,actionButton("create_bdd", "Create the BDD"),),
                         tags$br(),
                         column(12 ,  
                                
                                 h5("settings: lists of species name"), 
                                ),  
                         column(10 , uiOutput ("liste.faun4"),
                         uiOutput ("liste.faun4.eulipo"),
                         uiOutput ("liste.faun4.chiro"),
                         uiOutput ("liste.faun4.herpeto"),
                         uiOutput ("liste.faun4.lago"),
                         uiOutput ("liste.faun4.others"),
                         ),#end of column
                         column(2 ,
                                shiny::fileInput("list.extraspecies", "Choose File to import species list (.csv)",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")), ),
                         column(2 ,
                                actionButton("go.ng4", "load it"), ),
                         
                         tags$br(),
                         # ),#end of column
                ),#end of tabpanel
                tabPanel(h4("Import an old database"),
                         br(),
                         HTML(
                           paste0(" <div style=width:100%;, align=left>
    <font size=3>
   <span style='text-transform:none'>
   
   <p>It require a dataframe with the exact same column names 
    <br>
    </span> 
    </font>
                                  </p> </div> " )),
                         br(),
                         fileInput("file.oldBDD", "Choose File (.csv/.xls/.xlsx)",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values",
                                              ".csv",
                                              ".xlsx",".xls")),
                         selectInput(inputId = "worksheet.old", label="Worksheet Name", choices =''),
                         actionButton(inputId = "getData.old.BDD",label="Fusion Database")
                         
                         ),
                tabPanel(h4("Correct US data with a field Database"),
                         br(),
                         h4("not yet finish"),
                         br(),
                         fileInput("file.fieldBDD", "Choose File (.csv/.xls/.xlsx)",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values",
                                              ".csv",
                                              ".xlsx",".xls")),
                         selectInput(inputId = "worksheet.field", label="Worksheet Name", choices =''),
                         actionButton(inputId = "getData.fieldBDD",label="Get Data"),
                         # actionButton('reset.BDD', 'Reset Input'),
                         br(),
                         uiOutput("liste.col.ID"),
                         uiOutput("liste.col.US"), 
                         actionButton("go.ng3", "Modify name_US")
                         
                         
                ),#end of tabpanel
                tabPanel(h4("About"),
                                               br(),
                         HTML(
                           paste0(" <div style=width:100%;, align=left>
    <font size=3>
   <span style='text-transform:none'>
   
   <i>Micro-TRI </i> (v.1.1.5) is an application dedicated to the record of small vertebrate archaeological and palaeontological remains .</p>
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
                  box(width = 5,background = "blue",
                actionButton("submit", "New bag"),
                actionButton("submit2", "New line"),
                  ),# end of box
                box(width = 4,background = "blue",
                uiOutput ("previous"),
                #actionButton("next2", "Empty bag")
                 uiOutput ("next2"),
                  ),# end of box
                box(width = 3,background = "olive",
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
                         actionButton("goButton1", "line1", icon = icon("fas fa-plus"),lib = "font-awesome"),
                         br(),
                         actionButton("goButton2", "line2", icon = icon("fas fa-plus"),lib = "font-awesome"),
                         br(),
                         actionButton("goButton3", "line3", icon = icon("fas fa-plus"),lib = "font-awesome"),br(),
                         actionButton("goButton4", "line4", icon = icon("fas fa-plus"),lib = "font-awesome"),br(),
                         actionButton("goButton5", "line5", icon = icon("fas fa-plus"),lib = "font-awesome"),br(),
                         ),
                  box(width=11,
                DT::dataTableOutput("responses", width = 700),  style = "overflow-x: scroll;",
                  ),# end of box
                  ),# end of box
                ),# end of fluidRow
                tags$hr(),
                # br(),
                fluidRow(
                  box(title="sector",width = 1,background = "light-blue",
                      verbatimTextOutput("value_name_sector"),),
                  box(title="Year",width = 1,background = "light-blue",
                      verbatimTextOutput("value_year_exca"),),
                  box(title="ID_dec",width = 2,background = "light-blue",
                      verbatimTextOutput("value_ID_dec"),),
                  box(title="square",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_square"),),
                  box(title="name_dec",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_dec"),),
                  box(title="level",width = 2,background = "light-blue",
                      verbatimTextOutput("value_name_level"),), # end of box
                    box(title="US",width = 2,background = "light-blue",
                          verbatimTextOutput("value_name_us"),), # end of box

                ),
                hr(),
                fluidRow(
                ),# end of fluidRow
                fluidRow(
                  box(width = 6,
                      selectInput(inputId="name_taxa",
                                  label= "order level" ,
                                  choices=c("Rodentia","Eulipotyphla","Herpetofauna","Chiroptera","Lagomorpha","others"),
                                  selected = "Rodentia",
                                  multiple = FALSE,
                                  selectize = TRUE),
                      uiOutput("species_pickerinput"),

                ),# end of box

                box(width = 4,
                    br(),
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
                    br(),
                    shinyWidgets::sliderTextInput(
                      inputId = "specimen_age",
                      label = "specimen age",
                      choices = c("Young","Adult","Old"),
                      selected =c("Adult")
                    ),
                ),# end of box
                box(width = 2,
                    br(),
                    numericInput("nb_remains", label = h5("Numeric input"), value = 1,width ='1000px'),

                    br()
                    ),# end of box

                ),# end of fluidRow
                fluidRow(
                box(width = 6,
                    box(width = 8,
                        uiOutput("name_anat_list_boneteeth"),
                    # pickerInput(
                    #   inputId = "name_anat",
                    #   label = "Anatomy",
                    #   choices = get(list_bone[1]),
                    #   options = list(
                    #     `live-search` = TRUE)),

                    ),# end of box
                    box(width = 4,
                   shinyWidgets::sliderTextInput(
                     inputId = "name_anat2",
                     label = "bone/teeth",
                     choices = c("bone","teeth"),
                     selected =c("teeth"),
                   ),
                    ),# end of box
                   box(width = 7,
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
                   ),# end of box
                box(width = 5,
                    prettySwitch(
                      inputId = "infos_completude",
                      label = "Complet or broke",
                      status = "success",
                      value= FALSE,
                      fill = FALSE
                    ),

                    uiOutput ("completude"),
                ),# end of box
                ),

                box(width=5,
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
                ), # end of fluirdow

                fluidRow(
                
                ),
                fluidRow(
                box(width = 4,
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
                uiOutput("patine_pickerinput"),
                  ),
                box(width=4,
                    prettySwitch(
                      inputId = "infos_tm",
                      label = "Tooth marks",
                      status = "success",
                      fill = TRUE
                    ),
                    uiOutput ("tm_output"),
                    prettySwitch(
                      inputId = "infos_enc",
                      label = "pit marks",
                      status = "success",
                      fill = TRUE
                    ),
                    uiOutput ("enc_output"),
                    radioGroupButtons(
                      inputId = "trace_root",
                      label = "Root marks",
                      choices = c("IND","0","<50%",">50%"),
                      selected =("0"),
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
                ),#end of tabpanel
                tabPanel(h4("Options"),
                         tabsetPanel(
                           tabPanel(tags$h5("rename column modality"),
                                    tags$h3("Rename new group modality"),
                                    uiOutput("liste.newgroup2"),
                                    uiOutput("liste.newgroup4"),
                                    textInput("text.new.group2", label=h5("New name of the modality"),value = "new.modality"),
                                    actionButton("go.ng2", "Modify"),),

                           tabPanel(tags$h5("rename column"),
                           )
                           )#end of tabsetpanel   , ou poas ?
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

                             # prettySwitch(
                             #   inputId = "flip2",
                             #   label = "Level or US",
                             #   status = "success",
                             #   fill = F
                             # ),

                             tags$hr(),
                             column(5,

                                    DTOutput("table.species")),
                             column(11,
                                    prettySwitch(
                                      inputId = "flip",
                                      label = "flip: level per species",
                                      status = "success",
                                      fill = TRUE
                                    ),
                                    downloadButton("downloadData_speciesdata", "Download")),
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
                tabPanel(tags$h5("Tapho Ratio"),
                         tags$br(),
                         column(9,uiOutput("Ratio.data.list"),),
                         column(3,
                                shinyWidgets::actionBttn(
                                  inputId = "chr_setting",
                                  label = "Graphical options",
                                  style = "unite",
                                  color = "danger",
                                  size = "xs",
                                  icon = icon("fas fa-cogs",lib = "font-awesome")
                                ),
                                tags$style("#bsmodal_param .modal-dialog{ width:1200px}
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }

                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }

                                                                                "),

                                bsModal(
                                  id = "bsmodal_param",
                                  title = tags$h4(style = "color: red;","Graphical options"),
                                  trigger = "chr_setting",size = "large",

                                   checkboxInput("optioninfosfigplotly", "Show figure legend", TRUE),
                                  numericInput("fontsizetick", "tick font size",12, min = 1, max=40),
                          numericInput("fontsizeaxis", "Axis font size",12, min = 1, max=40),
                              uiOutput("themeforfigure2")
                                 ),),
                         column(12,
                         tags$br(),
                           tags$br(),
                         uiOutput("Ratio.data.graph"),),
                         tags$br(),
                         tags$br(),
                          column(11, downloadButton("downloadData_ratio.graph", "Download")),
                          tags$br(),
                          tags$br(),
                          DTOutput("table.Data_ratio"),
                          column(11, downloadButton("downloadData_ratio", "Download"))
                ),#end tabpanel

                tabPanel(tags$h5("Digestion Ratio"),
                         tags$br(),
                         column(9,uiOutput("Ratio.dig.list"),),
                         column(3,
                                shinyWidgets::actionBttn(
                                  inputId = "chr_setting_dig",
                                  label = "Graphical options",
                                  style = "unite",
                                  color = "danger",
                                  size = "xs",
                                  icon = icon("fas fa-cogs",lib = "font-awesome")
                                ),
                                tags$style("#bsmodal_param2 .modal-dialog{ width:1200px}
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }

                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }

                                                                                "),

                                bsModal(
                                  id = "bsmodal_param2",
                                  title = tags$h4(style = "color: red;","Graphical options"),
                                  trigger = "chr_setting_dig",size = "large",

                                  # checkboxInput("optioninfosfigplotly", "Show figure legend", TRUE),
                                  # numericInput("fontsizetick", "tick font size",12, min = 1, max=40),
                                  # numericInput("fontsizeaxis", "Axis font size",12, min = 1, max=40),
                                  uiOutput("themeforfigure")
                                ),),
                         column(12,

                                tags$br(),
                                uiOutput("Ratio.data.dig.graph"),),
                         tags$br(),
                         tags$br(),
                         column(11, downloadButton("downloadData_dig.graph", "Download")),
                         tags$br(),
                         tags$br(),
                         DTOutput("table.Data_dig"),
                         column(11, downloadButton("downloadData_dig", "Download"))

                ),#end tabpanel
                tabPanel(tags$h5("Completude"),
                         tags$br(),
                         uiOutput("select.ratio.comp.list"),
                         tags$br(),
                         uiOutput("Ratio.completude.graph"),
                         tags$br(),
                         tags$br(),
                         column(11, downloadButton("downloadData_comp.graph", "Download")),
                         tags$br(),
                         tags$br(),
                         DTOutput("table.Data_comp"),
                         column(11, downloadButton("downloadData_comp", "Download"))
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
                         column(11,tags$h5(style = "color: black;","species name(s) not included:")),
                         uiOutput("bioclim.names_noused"),
                         column(11,tags$h5(style = "color: blue;","Be careful to have well written the species name")),
                         column(11,tags$h5(style = "color: blue;","exemple: Microtus_arvalis")),

                ),#end tabpanel
                tabPanel(tags$h5("Bioclim graph"),
                         tags$br(),
                         radioButtons("var.bioclim2", "Variable estimated",
                                      choices = c("MAT" = "MAT",
                                                  "Tmax" = "Tmax",
                                                  "Tmin" = "Tmin",
                                                  "Mta" = "Mta",
                                                  "P" = "P"
                                                  ),
                                      selected = "MAT", inline=TRUE),
                         tags$br(),
                         uiOutput("bioclim.graph"),
                         column(11, downloadButton("downloadbioclim.graph", "Download")),
                )#end tabpanel


                )#end of tabsetpanel
        ),
        tabItem(tabName = "data3",
                h2("Global note from microvertebrate observation"),
                textAreaInput("note.obs", label="observation to add",rows=5, value = "", width = NULL,
                          placeholder = "Leave an comment/observation..."),
                uiOutput("obs.note.torender"),
                actionButton("Record_the_observation", "Record the observation"),
                br(),
                radioButtons("docpdfhtml", "Export format",
                             choices = c(html = "html"),
                             
                             selected = "html", inline=TRUE),
                br(),
                fluidRow(
                  column(6, downloadButton("export.Rmarkdown", "Export settings as Rmarkdown document")),
                ),
                
        )
    )
)

dbHeader <- dashboardHeader(title = "MICRO-TRI",
                            tags$li(a(href = 'https://github.com/AurelienRoyer/MICRO-TRI',
                                      img(src = 'www/logo1.png',
                                          title = "MICRO-TRI", height = "40px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))
ui <-dashboardPage(
  # dashboardHeader(title = "MICRO-TRI"),
  dbHeader,
  sidebar,
  body )## end of ui

}
# Run the application 

