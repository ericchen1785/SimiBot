library("tidyverse")
library("DT")
library("readxl")
library("writexl")
library("dplyr")
library("tidytext")
library("text2vec")
library("readr")
library("stringr")
library("stopwords")
library("textstem")
library("tm")
library("NLP")
library("tibble")
library("gsubfn")
library("textmineR")


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
  
  output$group_number <- renderText({
    
    input$anaylze_data
    
    uploaded_txt <- isolate(uploaded_txt())
    uploaded_csv <- isolate(uploaded_csv())   
    
    # create source dataframe, row combined
    source_df <- tibble(title = "Source", description=uploaded_txt)
    source_n_target <- rbind(source_df,uploaded_csv)
    
    # lower case, remove symbols/numbers/stopwords/single char/spaces, lemmatization
    prep_fun = function(x) {
      x = str_to_lower(x)
      x = str_replace_all(x, "[^[:alnum:]]", " ")
      x = gsub(patter="\\d", replace=" ", x)
      x = removeWords(x, stopwords())
      x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
      x= str_replace_all(x, "\\s+", " ")
      x = lemmatize_strings(x)}
    
    # clean the source and target description, save as a new column
    source_n_target$description_clean = prep_fun(source_n_target$description)
    
    # use vocabulary_based vectorization
    itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
    v_source <- create_vocabulary(itoken_source)
    vectorizer_source <- vocab_vectorizer(v_source)
    
    # apply TF-IDF transformation
    dtm_source <- create_dtm(itoken_source, vectorizer_source)
    tfidf <- TfIdf$new()
    dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
    
    # compute similarity-score against each row, saved as a new column
    source_tfidf_cos_sim <- round(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), digits = 4)
    source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
    
    # drop the source row
    target_w_score <- source_n_target[-c(1), ] 
    
    # cluster starts; create a document term matrix 
    dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                     doc_names = target_w_score$title, # document names
                     ngram_window = c(1, input$n_gram_length), # minimum and maximum n-gram length
                     verbose = FALSE) # Turn off status bar for this demo
    
    # construct the matrix of term counts to get the IDF vector
    tf_mat <- TermDocFreq(dtm)
    
    # TF-IDF and cosine similarity
    tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
    tfidf <- t(tfidf)
    csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
    csim <- csim %*% t(csim)
    cdist <- as.dist(1 - csim)
    hc <- hclust(cdist, "ward.D")
    clustering <- cutree(hc, k = input$number_of_clusters)
    
    # cluster ends; format as dataframe and bind with previous scoring result
    df_cluster <- as.data.frame(clustering,stringsAsFactors = default.stringsAsFactors)
    source_target_cluster <- cbind(target_w_score, df_cluster)
    
    # drop col description_clean, drop row names (tentative)
    source_target_cluster <- subset(source_target_cluster, select = -c(3))
    # source_target_cluster <- source_target_cluster[2:nrow(source_target_cluster), ]
    
    # calculate the mean similarity scoring by group; determine the cluster of max mean
    mean_by_cluster <- aggregate(source_target_cluster[, 3], list(source_target_cluster$clustering), mean)
    mean_by_cluster <- subset(mean_by_cluster, select = -c(1))
    max_cluster <- apply(mean_by_cluster,2,which.max)
    
    paste("Your source belongs to Cluster Group ", max_cluster)

  })
  
  

  output$scoring_result <- renderDT({
    
    input$anaylze_data
    
    uploaded_txt <- isolate(uploaded_txt())
    uploaded_csv <- isolate(uploaded_csv())   
    
    # create source dataframe, row combined
    source_df <- tibble(title = "Source", description=uploaded_txt)
    source_n_target <- rbind(source_df,uploaded_csv)
    
    # lower case, remove symbols/numbers/stopwords/single char/spaces, lemmatization
    prep_fun = function(x) {
      x = str_to_lower(x)
      x = str_replace_all(x, "[^[:alnum:]]", " ")
      x = gsub(patter="\\d", replace=" ", x)
      x = removeWords(x, stopwords())
      x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
      x= str_replace_all(x, "\\s+", " ")
      x = lemmatize_strings(x)}
    
    # clean the source and target description, save as a new column
    source_n_target$description_clean = prep_fun(source_n_target$description)
    
    # use vocabulary_based vectorization
    itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
    v_source <- create_vocabulary(itoken_source)
    vectorizer_source <- vocab_vectorizer(v_source)
    
    # apply TF-IDF transformation
    dtm_source <- create_dtm(itoken_source, vectorizer_source)
    tfidf <- TfIdf$new()
    dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
    
    # compute similarity-score against each row, saved as a new column
    source_tfidf_cos_sim <- round(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), digits = 4)
    source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
    
    # drop the source row
    target_w_score <- source_n_target[-c(1), ] 
    
    # cluster starts; create a document term matrix 
    dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                     doc_names = target_w_score$title, # document names
                     ngram_window = c(1, input$n_gram_length), # minimum and maximum n-gram length
                     verbose = FALSE) # Turn off status bar for this demo
    
    # construct the matrix of term counts to get the IDF vector
    tf_mat <- TermDocFreq(dtm)
    
    # TF-IDF and cosine similarity
    tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
    tfidf <- t(tfidf)
    csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
    csim <- csim %*% t(csim)
    cdist <- as.dist(1 - csim)
    hc <- hclust(cdist, "ward.D")
    clustering <- cutree(hc, k = input$number_of_clusters)
    
    # cluster ends; format as dataframe and bind with previous scoring result
    df_cluster <- as.data.frame(clustering,stringsAsFactors = default.stringsAsFactors)
    source_target_cluster <- cbind(target_w_score, df_cluster)
    
    # drop col description_clean, drop row names (tentative)
    source_target_cluster <- subset(source_target_cluster, select = -c(3))
    # source_target_cluster <- source_target_cluster[2:nrow(source_target_cluster), ]
    
    # calculate the mean similarity scoring by group; determine the cluster of max mean
    mean_by_cluster <- aggregate(source_target_cluster[, 3], list(source_target_cluster$clustering), mean)
    mean_by_cluster <- subset(mean_by_cluster, select = -c(1))
    max_cluster <- apply(mean_by_cluster,2,which.max)
    
    # sort the dataframe by similarity score, saved as a new csv file (optional)
    result_sorted <- source_target_cluster[order(-source_target_cluster[,3]),]

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
  
  
  output$clustering_result <- renderDT({
    
    input$anaylze_data
    
    uploaded_txt <- isolate(uploaded_txt())
    uploaded_csv <- isolate(uploaded_csv())   
    
    # create source dataframe, row combined
    source_df <- tibble(title = "Source", description=uploaded_txt)
    source_n_target <- rbind(source_df,uploaded_csv)
    
    # lower case, remove symbols/numbers/stopwords/single char/spaces, lemmatization
    prep_fun = function(x) {
      x = str_to_lower(x)
      x = str_replace_all(x, "[^[:alnum:]]", " ")
      x = gsub(patter="\\d", replace=" ", x)
      x = removeWords(x, stopwords())
      x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
      x= str_replace_all(x, "\\s+", " ")
      x = lemmatize_strings(x)}
    
    # clean the source and target description, save as a new column
    source_n_target$description_clean = prep_fun(source_n_target$description)
    
    # use vocabulary_based vectorization
    itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
    v_source <- create_vocabulary(itoken_source)
    vectorizer_source <- vocab_vectorizer(v_source)
    
    # apply TF-IDF transformation
    dtm_source <- create_dtm(itoken_source, vectorizer_source)
    tfidf <- TfIdf$new()
    dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
    
    # compute similarity-score against each row, saved as a new column
    source_tfidf_cos_sim <- round(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), digits = 4)
    source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
    
    # drop the source row
    target_w_score <- source_n_target[-c(1), ] 
    
    # cluster starts; create a document term matrix 
    dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                     doc_names = target_w_score$title, # document names
                     ngram_window = c(1, input$n_gram_length), # minimum and maximum n-gram length
                     verbose = FALSE) # Turn off status bar for this demo
    
    # construct the matrix of term counts to get the IDF vector
    tf_mat <- TermDocFreq(dtm)
    
    # TF-IDF and cosine similarity
    tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
    tfidf <- t(tfidf)
    csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
    csim <- csim %*% t(csim)
    cdist <- as.dist(1 - csim)
    hc <- hclust(cdist, "ward.D")
    clustering <- cutree(hc, k = input$number_of_clusters)
    
    # cluster ends; format as dataframe and bind with previous scoring result
    df_cluster <- as.data.frame(clustering,stringsAsFactors = default.stringsAsFactors)
    source_target_cluster <- cbind(target_w_score, df_cluster)
    
    # drop col description_clean, drop row names (tentative)
    source_target_cluster <- subset(source_target_cluster, select = -c(3))
    # source_target_cluster <- source_target_cluster[2:nrow(source_target_cluster), ]
    
    # calculate the mean similarity scoring by group; determine the cluster of max mean
    mean_by_cluster <- aggregate(source_target_cluster[, 3], list(source_target_cluster$clustering), mean)
    mean_by_cluster <- subset(mean_by_cluster, select = -c(1))
    max_cluster <- apply(mean_by_cluster,2,which.max)
    
    # generate cluster summary
    p_words <- colSums(dtm) / sum(dtm)
    
    # drop all words that don't appear in the cluster
    cluster_words <- lapply(unique(clustering), function(x){
      rows <- dtm[ clustering == x , ]
      rows <- rows[ , colSums(rows) > 0 ]
      colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
    })
    
    # create summary with top words in each cluster, saved as a new csv file (optional)
    cluster_summary <- data.frame(cluster = unique(clustering),
                                  mean = round(mean_by_cluster[,1], digits = 4),
                                  size = as.numeric(table(clustering)),
                                  top_words =
                                    sapply(cluster_words, 
                                           function(d){
                                             paste(names(d)[order(d, decreasing = TRUE) ][ 1:input$number_of_top_words ],collapse = ", ")
                                           }),
                                  stringsAsFactors = FALSE)

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
