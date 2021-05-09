library("DT")
library("shinycustomloader")

navbarPage(
  "Simi Bot",
  tabPanel(
    "Upload Data",
    fluidPage(
      fluidRow(
        column(offset = 1, wellPanel(
                p("Upload the source .txt text file and target .csv table using the input buttons below."),
                p("Download and use the template provided below. When finished, upload your files and click Analyze Data.")),
             width = 8),
        column(offset = 1, img(src='Simi Bot Logo2.png', align = "right", width = "116px"),
          width = 1)
      ),

      fluidRow(
        column(offset = 1,
          fileInput("uploaded_txt",
                         label = "Upload your txt source",
                         accept = c('txt', 'text/plain', '.txt'),
                         placeholder = "No .txt file selected"),
               width = 3),
        column(style = "margin-top:25px",
               downloadButton("download_txt_template",
                              "Download .txt template",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               width = 3)
      ),
      
      fluidRow(
        column(offset = 1,
          fileInput("uploaded_csv",
                    label = "Upload your .csv target",
                    accept = c('csv', 'comma-separated-values','.csv'),
                    placeholder = "No .csv file selected"),
          width = 3),
        column(style = "margin-top:25px",
          downloadButton("download_csv_template",
                         "Download .csv template",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          width = 3)
      ),

      fluidRow(
        column(11, offset = 1,
        sliderInput("number_of_clusters",
                    label = "Choose a number of clusters for the text cluster analysis",
                    min = 2,
                    max = 20,
                    value = 2,
                    width = "61.8%"),
        sliderInput("number_of_top_words",
                    label = "Choose a number of most frequent words to display for each text cluster",
                    min = 2,
                    max = 20,
                    value = 2,
                    width = "61.8%"),
        sliderInput("n_gram_length",
                    label = "Choose a level of word conbinations",
                    min = 1,
                    max = 3,
                    value = 1,
                    width = "38.2%")
        )
      ),
      fluidRow(
        column(6, align="center", offset = 3,
               actionButton("anaylze_data",label = "Analyze Data", 
                            width = "61.8%",
                            icon("bolt"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
               ), 

    )
  ),

  tabPanel(
    "Scoring Result",
    fluidPage(
      span(textOutput("group_number"), style="font-size:1.5em; color:#337ab7; font-family:Lucida Console, Courier New, monospace"),
      br(),
      withLoader(DTOutput("scoring_result"), loader = "dnaspin")
    )
  ),
  tabPanel(
    "Clustering Result",
    fluidPage(
      withLoader(DTOutput("clustering_result"), loader = "dnaspin")
    )
  ),
  tabPanel(
    "Help",
    fluidPage(
      fluidRow(
        column(width = 10, offset = 1,
               img(src='Simi Bot Logo2.png', align = "right", width = "116px"),
               h1("FAQ"))),
      fluidRow(
        column(width = 10, offset = 1,
                h4("How do I upload my .txt source files?"),
                    p(" 1. Create a new .txt file or download the .txt file template"),
                    p(" 2. Paste your text content in the .txt file"),
                    p(" 3. Browse and upload the .txt file"),
                    br(),
                h4("How do I upload my .csv target files?"),
                    p(" 1. Create a new .csv file or download the .csv file template"),
                    p(" 2. Open the .csv file, input \"title\" in cell A1 and \"description\" in cell A2 as headers of data. The headers are critical to the following analysis."),
                    p(" 3. Paste your text content in column A and B below the headers; make sure your title and description are matched respectively"),
                    p(" 4. Browse and upload the .csv file"),
                    br(),
                h4("What does it mean when the program tells me my source belongs to a cluster group?"),
                    p(" Simi Bot returns a similarity score between your source and each target, clusters your traget into groups, and calculates the mean of similarity score by group."),
                    p(" The group that achieves highest average similarity score is the group that your source belongs to."),
                    br(),
                h4("Can I see an example?"),
                    p("Of Course! We provide a sample source .txt file (the resume of the author of work) and a sample target .csv file (a list of 1000+ jobs) below."),
                    p("Let's try it out and see which job matches the author most!"),
                    br()
               )),
      fluidRow(
        column(offset = 1, downloadButton("download_txt_sample",
                              "Download .txt Sample",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               width = 1),
        column(offset = 1, downloadButton("download_csv_sample",
                                          "Download .csv Sample",
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               width = 1),
        column(offset = 1, downloadButton("download_tutorial",
                                          "Download Tutorial",
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               width = 6)
      ),
      fluidRow(
        br(),
        br(),
        column(offset = 1, width = 10,
          p(" * If you perform all the steps above and still encounter a problem, please download the tutorial or contact the author")),
        br(),
        br()
      ),
              
    )
  ),
  collapsible = TRUE
)
