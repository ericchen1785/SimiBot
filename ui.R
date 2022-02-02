dashboardPage(
  title = "Simi Bot",
  # skin = "blue-light",
  options = list(sidebarExpandOnHover = TRUE),
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
  header = dashboardHeader(titleWidth = 200,
                           title = div(
                                       img(src = 'Simi Bot Logo2 - Remastered.png', height = "35px",  style = "align:left; margin-left:-5px; margin-top:-5px"),
                                       div("Simi Bot",  style = "margin-left:5px; display:inline-block; float:right; font-family: Consolas; font-size: 25px;")
                                       )
                           ),
  sidebar = dashboardSidebar(minified = TRUE,
                             collapsed = FALSE,
                             width = 200,
                             # sidebarMenuOutput("menu")
                             sidebarMenu(id = "tabs",
                                         menuItem("Home", tabName = "home", icon = icon("home")),
                                         
                                         # menuItem("Scoring Result", tabName = "scoringresult", icon = icon("chart-bar")),
                                         # menuItem("Clustering Result", tabName = "clusteringresult", icon = icon("chart-bar")),
                                         
                                         menuItemOutput("scoringresultMenu"),
                                         menuItemOutput("clusteringresultMenu"),
                                         menuItem("Help", tabName = "help", icon = icon("info"))
                                         )
                             ),
  controlbar = dashboardControlbar(
    id = NULL,
    disable = TRUE,
    collapsed = TRUE,
    overlay = TRUE,
    skin = "dark"
    ),
  scrollToTop = TRUE,
  body = dashboardBody(
  
    # Dark Blue #204D74 Light Blue #3C8CBC Vivid Blue #1D89FF Transparent #ECF0F5 Grey Blue #8EACC5
    
    
      tags$head(tags$style(HTML('
              /* logo */
                .skin-blue .main-header .logo {
                  background-color: #204D74;
                  color: #8EACC5;
                }
              
              /* logo when hovered */
                .skin-blue .main-header .logo:hover {
                  background-color: #204D74;
                  color: #ECF0F5;
                }

              /* toggle button  */                    
                .skin-blue .main-header .navbar .sidebar-toggle{
                  background-color: #204D74;
                  color: #8EACC5;
                } 
                
              /* toggle button when hovered  */                    
                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                  background-color: #122A40;
                  color: #ECF0F5
                }   
              
              /* navbar (rest of the header) */
                .skin-blue .main-header .navbar {
                  background-color: #204D74;
                }        
              
              /* main sidebar */
                .skin-blue .main-sidebar {
                  background-color: #ECF0F5;
                }

              /* active selected tab in the sidebarmenu */
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                  background-color: #ECF0F5;
                    color: #204D74;
                }

              /* other links in the sidebarmenu */
                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                  background-color: #ECF0F5;
                    color: #A0ABBE;
                }

              /* other links in the sidebarmenu when hovered */
                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                  background-color: #8EACC5;
                    color: #FFFFFF;
                }
              /* footer  */                    
                .skin-blue .main-footer{
                  background-image: url("footer.png");
                  # width : 100%;
                  background-repeat: no-repeat;
                  background-position: center;
                  background-size: cover; 
                  background-color: #ECF0F5;
                }

            '))),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "home",

              fluidRow(
                column(width = 12,
                       div(style = "display:inline-block; float:right; margin:20px",
                           useShinyjs(),
                           actionBttn(
                             inputId = "helpButton",
                             label = "Help",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("info")
                           ),
                           actionBttn(
                             inputId = "reloadButton",
                             label = "Reset",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("undo-alt")
                           )
                       )
                       )
              ),
              fluidRow(
                column(width = 9, offset = 1,
                       box(
                         title = "Comparison Source", 
                         closable = FALSE, 
                         width = NULL,
                         status = "primary", 
                         solidHeader = FALSE, 
                         collapsible = FALSE,
                         icon = icon("chevron-right"),
                         label = NULL,
                         fluidRow(
                           column(10, offset = 1,
                             div(style = "display:inline-block; float:right",
                                 radioGroupButtons(
                                   inputId = "source_type",
                                   label = "", 
                                   choices = c("Upload", "Paste"),
                                   status = "primary",
                                   checkIcon = list(yes = icon("check")),
                                   justified = TRUE)
                             )
                           )
                         ),
                         
                         conditionalPanel(
                           condition = "input.source_type == 'Upload'",
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-top:10px; margin-bottom:20px",
                                    span("Upload Your .txt Source File", style = "font-weight:bold;")
                             )
                           ),
                           fluidRow(
                              column(offset = 1, width = 5,
                                fileInput("uploaded_txt",
                                               label = NULL,
                                               accept = c('txt', 'text/plain', '.txt'),
                                               placeholder = "No .txt file selected")
                                     ),
                              column(width = 5,
                                     downloadButton("download_txt_template",
                                                    "Download .txt Template",
                                                    style="width:100%; color: #fff; background-color: #3C8CBC;")
                                     )
                            )
                         ),
                         conditionalPanel(
                           condition = "input.source_type == 'Paste'",
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-top:10px; margin-bottom:20px",
                                    span("Paste Your Text Source", style = "font-weight:bold;")
                             )
                           ),
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-bottom:10px",
                                    textAreaInput("paste_source",
                                                  label = NULL, 
                                                  value = "", 
                                                  width = NULL, 
                                                  height = NULL,
                                                  cols = NULL, 
                                                  rows = NULL, 
                                                  placeholder = NULL, 
                                                  resize = "vertical")
                             )
                           )
                         )
                       ),
                       box(
                         title = "Comparison Target", 
                         closable = FALSE, 
                         width = NULL,
                         status = "primary", 
                         solidHeader = FALSE, 
                         collapsible = FALSE,
                         icon = icon("th-list"),
                         label = NULL,
                         fluidRow(
                           column(10, offset = 1,
                                  div(style = "display:inline-block; float:right",
                                      radioGroupButtons(
                                        inputId = "target_type",
                                        label = "", 
                                        choices = c("Upload", "Paste"),
                                        status = "primary",
                                        checkIcon = list(yes = icon("check")),
                                        justified = TRUE)
                                  )
                           )
                         ),

                         conditionalPanel(
                           condition = "input.target_type == 'Upload'",
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-top:10px; margin-bottom:20px",
                                    span("Upload Your .csv Source File", style = "font-weight:bold;")
                             )
                           ),
                           fluidRow(
                             column(offset = 1, width = 5,
                                    fileInput("uploaded_csv",
                                              label = NULL,
                                              accept = c('csv', 'comma-separated-values','.csv'),
                                              placeholder = "No .csv file selected")
                             ),
                             column(width = 5,
                                    downloadButton("download_csv_template",
                                                   "Download .csv Template",
                                                   style="width:100%; color: #fff; background-color: #3C8CBC;")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.target_type == 'Paste'",
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-top:10px; margin-bottom:20px",
                                    span("Paste Your Text Target", style = "font-weight:bold;")
                             )
                           ),
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-bottom:10px",
                                    textAreaInput("paste_target",
                                                  label = NULL, 
                                                  value = "", 
                                                  width = NULL, 
                                                  height = NULL,
                                                  cols = NULL, 
                                                  rows = NULL, 
                                                  placeholder = "Paste text targets you need to compare and separate them with backslash \" \\ \" ", 
                                                  resize = "vertical")
                             )
                           )
                         )
                         ),

                       box(
                         title = "Configuration", 
                         closable = FALSE, 
                         width = NULL,
                         status = "primary", 
                         solidHeader = FALSE, 
                         collapsible = FALSE,
                         icon = icon("sliders-h"),
                         label = NULL,
                         fluidRow(style = "margin-top:20px",
                                  column(offset = 1, width = 5,
                                         
                                           div(style = "display:inline-block;",
                                             span("Number of Word Combinations (n-gram)", style = "font-weight: bold"),
                                             br(),
                                             span("determine how many contiguous words should be analyzed together", style = "color: #AAAAAA")
                                           )
                                  ),
                                  column(width = 5, align="center",
                                           div(
                                             autonumericInput(
                                               inputId = "n_gram_length",
                                               label = NULL,
                                               value = 1,
                                               width = "200px",
                                               currencySymbol = "-gram",
                                               currencySymbolPlacement = "s",
                                               decimalPlaces = 0,
                                               maximumValue = 3,
                                               minimumValue = 1,
                                             )
                                           )
                                           
                                         
                                           
                                           
                                  )
                         ),
                         fluidRow(style = "margin-top:10px; margin-bottom:10px;",
                                  column(offset = 1, width = 5, 
                                         div(
                                           span("Number of Most Frequent Words", style = "font-weight: bold"),
                                           br(),
                                           span("determine how many word to display for each cluster", style = "color: #AAAAAA")
                                         )
                                  ),
                                  column(width = 5, align="center",
                                         autonumericInput(
                                           inputId = "number_of_top_words",
                                           label = NULL,
                                           value = 2,
                                           width = "200px",
                                           currencySymbol = " Words",
                                           currencySymbolPlacement = "s",
                                           decimalPlaces = 0,
                                           maximumValue = 20,
                                           minimumValue = 2,
                                         )
                                  )
                         ),
                         fluidRow(style = "margin-top:10px; margin-bottom:10px",
                                  column(offset = 1, width = 5, 
                                         div(
                                           span("Number of Clusters", style = "font-weight: bold"),
                                           br(),
                                           span("determine the granularity of your grouping process", style = "color: #AAAAAA")
                                         )
                                  ),
                                  column(width = 5, align="center",
                                         autonumericInput(
                                           inputId = "number_of_clusters",
                                           label = NULL,
                                           value = 2,
                                           width = "200px",
                                           currencySymbol = " Clusters",
                                           currencySymbolPlacement = "s",
                                           decimalPlaces = 0,
                                           maximumValue = 20,
                                           minimumValue = 2
                                         )
                                  )
                         ),
                         uiOutput("show_plot_cluster")
                       ),
  
                       box(
                         title = "Advanced Setting", 
                         closable = FALSE, 
                         width = NULL,
                         status = "primary", 
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         icon = icon("cogs"),
                         label = NULL,
                         
                         fluidRow(style = "margin-top:20px; margin-bottom:10px;",
                                  column(offset = 1, width = 10, 
                                         awesomeCheckbox( 
                                           inputId = "customize_stopword",
                                           label = "Customize Stopword", 
                                           value = FALSE,
                                           status = "primary"
                                         )
                                  )
                         ),
                         conditionalPanel(
                           condition = "input.customize_stopword == '1'",
                           
                           fluidRow(
                             column(offset = 1, width = 5,
                                    selectInput("stopwords_source",
                                                NULL,
                                                c("snowball", "marimo", "nltk", "stopwords-iso", "smart"),
                                                selected = "snowball",
                                                width = "100%"
                                    )
                             ),
                             column(offset = 0, width = 5,
                                    switchInput(
                                      inputId = "show_list",
                                      label = "List",
                                      onLabel = "<i class=\"fas fa-eye\"></i>",
                                      offLabel = "<i class=\"fas fa-eye-slash\"></i>",
                                      labelWidth = "fit"
                                    )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.show_list == '1'",
                             fluidRow(
                               column(offset = 1, width = 10, style = "margin-bottom:10px",
                                      textOutput("show_stopwords_source")
                               )
                             )
                             
                           ),
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-top:10px; margin-bottom:10px",
                                    textAreaInput("delete_words",
                                                  label = NULL, 
                                                  value = "", 
                                                  width = NULL, 
                                                  height = NULL,
                                                  cols = NULL, 
                                                  rows = NULL, 
                                                  placeholder = "Paste words you would like to REMOVE from the stopword list, separate with any punctuation mark", 
                                                  resize = "vertical")
                             )
                           ),
                           fluidRow(
                             column(offset = 1, width = 10, style = "margin-bottom:10px",
                                    textAreaInput("extra_words",
                                                  label = NULL, 
                                                  value = "", 
                                                  width = NULL, 
                                                  height = NULL,
                                                  cols = NULL, 
                                                  rows = NULL, 
                                                  placeholder = "Paste words you would like to ADD to the stopword list, separate with any punctuation mark", 
                                                  resize = "vertical")
                             )
                           )
                         )
                         
                       ),
                       fluidRow( style = "margin-top:20px; margin-bottom:20px",
                         column(12, align="center",
                                actionButton("anaylze_data",label = "Analyze Data", 
                                             width = "200px",
                                             icon("bolt"), 
                                             style="color: #fff; background-color: #3C8CBC;")
                         )
                       )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "scoringresult",
              fluidRow(
                div(style = "display:inline-block; float:left; margin:20px",
                  span(textOutput("group_number"), style="font-size:1.5em; color:#3C8CBC; font-family: Consolas, Courier New, monospace")
                ),
                div(style = "display:inline-block; float:right; margin:20px",
                    useShinyjs(),
                    actionBttn(
                      inputId = "clusterButton",
                      label = "Clustering Result",
                      style = "stretch", 
                      color = "primary",
                      icon = icon("chart-bar")
                    )
                )
              ),
              fluidRow(
                withLoader(DTOutput("scoring_result"), loader = "dnaspin")
                # withLoader(DTOutput("show_target_df"), loader = "dnaspin")
              )
      ),
      
      # Third tab content
      tabItem(tabName = "clusteringresult",
              fluidRow(
                div(style = "display:inline-block; float:right; margin:20px",
                    useShinyjs(),
                    actionBttn(
                      inputId = "scoreButton",
                      label = "Scoring Result",
                      style = "stretch", 
                      color = "primary",
                      icon = icon("chart-bar")
                    )
                )
              ),
              fluidRow(
                withLoader(DTOutput("clustering_result"), loader = "dnaspin")
              )
      ),
      
      # Forth tab content
      tabItem(tabName = "help",
              fluidRow(style = "margin-top:20px;margin-bottom:10px",
                column(width = 11,
                       span("FAQ", style = "font-size:2em; color: #204D74; font-family: Consolas")
                       )
                ),
              fluidRow(
                
                column(width = 11,
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_1",
                             label = "How do I upload my .txt source files?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                             )
                           ),
                       conditionalPanel(
                         condition = "input.faq_1 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                           column(width = 12, style = "margin-top:10px;",
                                  p(" 1. Create a new .txt file or download the .txt file template"),
                                  p(" 2. Paste your text content in the .txt file"),
                                  p(" 3. Browse and upload the .txt file")
                                  )
                           )
                         ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_2",
                             label = "How do I upload my .csv target files?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_2 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p(" 1. Create a new .csv file or download the .csv file template"),
                                         p(" 2. Open the .csv file, input \"title\" in cell A1 and \"description\" in cell A2 as headers of data. The headers are critical to the following analysis."),
                                         p(" 3. Paste your text content in column A and B below the headers; make sure your title and description are matched respectively"),
                                         p(" 4. Browse and upload the .csv file")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_3",
                             label = "Is there a guide for configuration?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_3 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p("For the number of word combinations, 1 and 2 are common; 3 is acceptable if necessary."),
                                         p("The number of most frequent words should not exceed the number of identical words in any target. "),
                                         p("The number of clusters should not exceed the number of your targets. A practical advice is keeping it under half of the number of your targets.")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_4",
                             label = "What's a stopword list and how to customize one?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_4 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p("Stop words are common words without concrete meaning. They could be the noise in text mining. As a part of the preprocessing, Simi Bot filters your text source and target with a stopword list (snowball)."),
                                         p("You can always access your stopword list by checking \"Customize Stopword\" box. If there're any words that you want to compare but in the stopword list, you may remove them by pasting them in the first text area; if there're any words that you don't want to compare, you may add them by pasting them in the second text area. Remember, the order of the process matters. If an identical word is pasted into both text areas, it would first be removed then added to the stopword list.")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_5",
                             label = "Why doesn't it work after I click Analyze Data?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_5 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p(" Please make sure you provide your source and target.")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_6",
                             label = "What does a cluster number mean?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_6 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p(" Simi Bot groups text targets with hierarchical clustering and ranks clusters by size. A cluster with more items is labeled with a larger cluster number.")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_7",
                             label = "What does it mean when my source belongs to a cluster group?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_7 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p(" Simi Bot returns a similarity score between your source and each target, clusters your traget into groups, and calculates the mean of similarity score by group."),
                                         p(" The group that achieves highest average similarity score is the group that your source belongs to.")
                                  )
                         )
                       ),
                       div(style = "margin-top:5px;margin-bottom:5px",
                           actionBttn(
                             inputId = "faq_8",
                             label = "Can I see an example?",
                             style = "stretch", 
                             color = "primary",
                             icon = icon("question-circle")
                           )
                       ),
                       conditionalPanel(
                         condition = "input.faq_8 % 2 == 1",
                         fluidRow(style = "display:inline-block; margin:5px",
                                  column(width = 12, style = "margin-top:10px;",
                                         p("Of course! We provide a sample source .txt file (the resume of the author of work) and a sample target .csv file (a list of 1000+ occupations) below."),
                                         p("Let's try it out and see which occupation matches the author most!")
                                  )
                         )
                       )
                        
                       )),
              fluidRow(style = "margin:10px",
                column(width = 11,
                       div(style = "display:inline-block; float:left; margin:5px",
                           downloadButton("download_txt_sample",
                                          "Download .txt Sample",
                                          style="color: #fff; background-color: #3C8CBC;")
                           ),
                       div(style = "display:inline-block; float:left; margin:5px",
                           downloadButton("download_csv_sample",
                                          "Download .csv Sample",
                                          style="color: #fff; background-color: #3C8CBC;")
                           ),
                       div(style = "display:inline-block; float:left; margin:5px",
                           downloadButton("download_tutorial",
                                          "Download Tutorial",
                                          style="color: #fff; background-color: #3C8CBC;")
                           )
                       )
              ),
              fluidRow(style = "margin:20px",
                column(width = 11,
                  span(" * If you perform all the steps above and still encounter a problem, please contact the author at ericchen1785@gmail.com", style = "color: #AAAAAA;"))
              )
              
      )
    )
  )
  ,
  footer = dashboardFooter(left = span("Copyright © 2021 Eric Chen. All Rights Reserved.",
                                       style = "font-family: Helvetica; bottom:0; width:100%; color: #AAAAAA; background-color: transparent; z-index: 1000;")
  )
)