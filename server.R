function(input, output, session) {
  
  uploaded_txt <- eventReactive(c(input$uploaded_txt),
                                {
                                  txt_source <- read_file(input$uploaded_txt$datapath)
                                  txt_source
                                })
  
  uploaded_csv <- eventReactive(c(input$uploaded_csv),
                                {
                                  csv_target <- read.csv(input$uploaded_csv$datapath)
                                  csv_target
                                         })
  
  observeEvent(input$reloadButton, {
    session$reload()
  })

  output$scoringresultMenu <- renderMenu({
    if(
      c((!is.null(input$uploaded_txt)|nchar(input$paste_source)>0) & (!is.null(input$uploaded_csv)|nchar(input$paste_target)>0) & input$anaylze_data>0)
      )
      menuItem("Scoring Result", tabName = "scoringresult", icon = icon("chart-bar"))
    })


  output$clusteringresultMenu <- renderMenu({
    if(
      c((!is.null(input$uploaded_txt)|nchar(input$paste_source)>0) & (!is.null(input$uploaded_csv)|nchar(input$paste_target)>0) & input$anaylze_data>0)
    )
      menuItem("Clustering Result", tabName = "clusteringresult", icon = icon("chart-bar"))
  })
  
  
  
  output$show_plot_cluster <- renderUI({
    
    if(
      c((!is.null(input$uploaded_txt)|nchar(input$paste_source)>0) & (!is.null(input$uploaded_csv)|nchar(input$paste_target)>0))
    )
    
    fluidRow(style = "margin-bottom:10px;",
             column(offset = 1, width = 10,
                    awesomeCheckbox(
                      inputId = "plot_cluster",
                      label = "Optimal Number of Clusters Suggestion",
                      value = FALSE,
                      status = "primary"
                    ),
                    conditionalPanel(
                      condition = "input.plot_cluster == '1'",
                      withSpinner(
                        plotOutput("plot1",
                                   width = "100%",
                                   height = "200px"),
                        type = 1,
                        color = "#759EC3",
                        size = 1)
                    )
             )
    )
  })
  
    
    
  

  
  
  
  observeEvent(input$anaylze_data, {
    hideTab(inputId = "tabs", target = "home")
  })
  
  
  observeEvent(input$helpButton, {
    updateTabItems(session, "tabs", "help")
  })
  
  observeEvent(input$anaylze_data, {
    updateTabItems(session, "tabs", "scoringresult")
  })
  
  observeEvent(input$clusterButton, {
    updateTabItems(session, "tabs", "clusteringresult")
  })
  
  observeEvent(input$scoreButton, {
    updateTabItems(session, "tabs", "scoringresult")
  })
  
  output$show_stopwords_source <- renderText({
    stopwords::stopwords("en", source = input$stopwords_source)
  })
  
  # FOR DOWNLOAD #
  output$download_txt_template <- downloadHandler(
    filename = function(){
      paste0("Simi Bot Template_Source.txt")
    },
    content = function(file){
      file.copy("data/Simi Bot Template_Source.txt", file)
    }
  )
  
  output$download_csv_template <- downloadHandler(
    filename = function(){
      paste0("Simi Bot Template_Target.csv")
    },
    content = function(file){
      file.copy("data/Simi Bot Template_Target.csv", file)
    }
  )
  
  output$download_txt_sample <- downloadHandler(
    filename = function(){
      paste0("Simi Bot Sample_Source Resume.txt")
    },
    content = function(file){
      file.copy("data/Simi Bot Sample_Source Resume.txt", file)
    }
  )
  
  output$download_csv_sample <- downloadHandler(
    filename = function(){
      paste0("Simi Bot Sample_Target Job Descriptions.csv")
    },
    content = function(file){
      file.copy("data/Simi Bot Sample_Target Job Descriptions.csv", file)
    }
  )

  output$download_tutorial <- downloadHandler(
    filename = function(){
      paste0("Simi Bot Tutorial.pdf")
    },
    content = function(file){
      file.copy("data/Simi Bot Tutorial.pdf", file)
    }
  )
  # DOWNLOAD END
  
  #PLOP OUTPUT
  output$plot1 <- renderPlot({
    
    input$plot_cluster
    
    cdist_data_scale <- optimize_fun(input$source_type, uploaded_txt, input$paste_source, input$stopwords_source, input$delete_words, input$extra_words, input$target_type, uploaded_csv, input$paste_target, input$number_of_clusters, input$n_gram_length)
    
    optimal_number_plot <- fviz_nbclust(cdist_data_scale, kmeans, method = "silhouette", k.max = 20) +
      labs(subtitle = "Silhouette Method")
    
    plot(optimal_number_plot)
    
  })
  
  
  # FIRST OUTPUT
  output$group_number <- renderText({

    input$anaylze_data

    source_target_cluster <- main_fun(input$source_type, uploaded_txt, input$paste_source, input$stopwords_source, input$delete_words, input$extra_words, input$target_type, uploaded_csv, input$paste_target, input$number_of_clusters, input$n_gram_length)
    
    # calculate the mean similarity scoring by group; determine the cluster of max mean
    mean_by_cluster <- aggregate(source_target_cluster[, 3], list(source_target_cluster$clustering), mean)
    mean_by_cluster <- subset(mean_by_cluster, select = -c(1))
    max_cluster <- apply(mean_by_cluster,2,which.max)

    paste("Your source belongs to Cluster Group ", max_cluster)
  })
    
  #SECOND OUTPUT
  output$scoring_result <- renderDT({
    
    input$anaylze_data
    
    source_target_cluster <- main_fun(input$source_type, uploaded_txt, input$paste_source, input$stopwords_source, input$delete_words, input$extra_words, input$target_type, uploaded_csv, input$paste_target, input$number_of_clusters, input$n_gram_length)
      
    # sort the dataframe by similarity score, saved as a new csv file (optional)
    result_sorted <- source_target_cluster[order(-source_target_cluster[,3]),]
    result_sorted <- mutate(result_sorted,similarity_score=percent(similarity_score, accuracy = 0.01))

    result_sorted %>%
      datatable(
        rownames = NULL,
        colnames = c("Title", "Description", "Similarity Score", "Cluster Group"),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          autoWidth = TRUE,
          columnDefs = list(list(width = '20%', targets = 0),list(className = 'dt-center', targets = 2:3)),
          pageLength = 20,
          buttons = list(
            list(
              extend = "collection",
              text = 'Show Top 100',
              action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(100);
                                    dt.ajax.reload();
                                }")),
            list(
              extend = "collection",
              text = 'Show All Rows',
              action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")),
            c('copy', 'csv', 'excel', 'pdf')
            )
            )
      )
  })
  
  # THIRD OUTPUT
  output$clustering_result <- renderDT({

    input$anaylze_data

    cluster_summary <- summary_fun(input$source_type, uploaded_txt, input$paste_source, input$stopwords_source, input$delete_words, input$extra_words, input$target_type, uploaded_csv, input$paste_target, input$number_of_clusters, input$n_gram_length, input$number_of_top_words)
    
    cluster_summary %>%
      datatable(
        rownames = NULL,
        colnames = c(
          "Cluster Group",
          "Average Similarity Score",
          "Number of Targets in Group",
          "Most Frequent Words in Group"
        ),
        extensions = "Buttons",
        options = list(
          dom = 'Bfrtip',
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:2)),
          pageLength = 20,
          buttons = c('copy', 'csv', 'excel', 'pdf')
          )
        )
  })
}
