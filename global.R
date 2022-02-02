library(dplyr)
library(DT)
library(factoextra)
library(gsubfn)
library(NLP)
library(readr)
library(readxl)
library(scales)
library(shiny)
library(shinycustomloader)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinymanager)
library(shinyjs)
library(shinyWidgets)
library(stopwords)
library(stringr)
library(textmineR)
library(textstem)
library(text2vec)
library(tibble)
library(tidytext)
library(tidyverse)
library(tm)
library(waiter)

# prep_fun = function(x, y) {
#   x = str_to_lower(x)
#   x = str_replace_all(x, "[^[:alnum:]]", " ")
#   x = gsub(patter="\\d", replace=" ", x)
#   x = removeWords(x, stopwords())
#   x = removeWords(x, unlist(strsplit(y, " ")))
#   x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
#   x = str_replace_all(x, "\\s+", " ")
#   x = lemmatize_strings(x)}

prep_fun = function(x){
  x = str_to_lower(x)
  x = str_replace_all(x, "[^[:alpha:]]", " ")
  x = gsub(patter="\\b[A-z]\\b{1}", replace=" ", x)
  x = str_replace_all(x, "\\s+", " ")
}

stopwords_fun = function(stopwords_source, delete_words, extra_words){
  delete_words = unlist(strsplit(prep_fun(delete_words), " "))
  extra_words = unlist(strsplit(prep_fun(extra_words), " "))
  sw = stopwords::stopwords("en", source = stopwords_source)
  sw = sw[!(sw %in% delete_words)]
  sw = c(sw, extra_words)
  sw = unique(sw)
  sw
}

target_fun = function(paste_target){
  target_spilt = strsplit(paste_target,"\\\\")[[1]]
  target_df = tibble(title = paste("Target", 1:length(target_spilt)), description=target_spilt)
  target_df
}

optimize_fun = function(source_type, uploaded_txt, paste_source, stopwords_source, delete_words, extra_words, target_type, uploaded_csv, paste_target, number_of_clusters, n_gram_length){
  
  uploaded_txt <- ifelse(source_type == 'Upload', isolate(uploaded_txt()), paste_source)
  
  if (target_type == 'Upload') {
    target_df <- isolate(uploaded_csv())
  } else {
    target_df <- target_fun(paste_target)
  }
  
  # create source dataframe, row combined
  source_df <- tibble(title = "Source", description=uploaded_txt)
  source_n_target <- rbind(source_df,target_df)
  
  # clean the source and target description, save as a new column
  stopwords_original <- stopwords_fun(stopwords_source, delete_words, extra_words)
  source_n_target$description_clean <- 
    lemmatize_strings(
      removeWords(
        prep_fun(source_n_target$description
        ),stopwords_original
      )
    )
  
  # use vocabulary_based vectorization
  itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
  v_source <- create_vocabulary(itoken_source)
  vectorizer_source <- vocab_vectorizer(v_source)
  
  # apply TF-IDF transformation
  dtm_source <- create_dtm(itoken_source, vectorizer_source)
  tfidf <- TfIdf$new()
  dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
  
  # compute similarity-score against each row, saved as a new column
  source_tfidf_cos_sim <- sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2")
  source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
  
  # drop the source row
  target_w_score <- source_n_target[-c(1), ] 
  
  # cluster starts; create a document term matrix 
  dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                   doc_names = target_w_score$title, # document names
                   ngram_window = c(1, n_gram_length), # minimum and maximum n-gram length
                   verbose = FALSE) # Turn off status bar for this demo
  
  # construct the matrix of term counts to get the IDF vector
  tf_mat <- TermDocFreq(dtm)
  
  # TF-IDF and cosine similarity
  tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
  tfidf <- t(tfidf)
  csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
  csim <- csim %*% t(csim)
  cdist <- as.dist(1 - csim)
  cdist_data_scale <- scale(cdist)
  cdist_data_scale
}

main_fun = function(source_type, uploaded_txt, paste_source, stopwords_source, delete_words, extra_words, target_type, uploaded_csv, paste_target, number_of_clusters, n_gram_length ){
  
  uploaded_txt <- ifelse(source_type == 'Upload', isolate(uploaded_txt()), paste_source)

  if (target_type == 'Upload') {
    target_df <- isolate(uploaded_csv())
  } else {
    target_df <- target_fun(paste_target)
  }
  
  # create source dataframe, row combined
  source_df <- tibble(title = "Source", description=uploaded_txt)
  source_n_target <- rbind(source_df,target_df)
  
  # clean the source and target description, save as a new column
  stopwords_original <- stopwords_fun(stopwords_source, delete_words, extra_words)
  source_n_target$description_clean <- 
    lemmatize_strings(
      removeWords(
        prep_fun(source_n_target$description
                 ),stopwords_original
        )
    )
  
  # use vocabulary_based vectorization
  itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
  v_source <- create_vocabulary(itoken_source)
  vectorizer_source <- vocab_vectorizer(v_source)
  
  # apply TF-IDF transformation
  dtm_source <- create_dtm(itoken_source, vectorizer_source)
  tfidf <- TfIdf$new()
  dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
  
  # compute similarity-score against each row, saved as a new column
  # source_tfidf_cos_sim <- percent(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), accuracy = 0.01)
  source_tfidf_cos_sim <- round(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), digits = 4)
  source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
  
  # drop the source row
  target_w_score <- source_n_target[-c(1), ] 
  
  # cluster starts; create a document term matrix 
  dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                   doc_names = target_w_score$title, # document names
                   ngram_window = c(1, n_gram_length), # minimum and maximum n-gram length
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
  clustering <- cutree(hc, k = number_of_clusters)
  
  # cluster ends; format as dataframe and bind with previous scoring result
  df_cluster <- as.data.frame(clustering,stringsAsFactors = default.stringsAsFactors)
  source_target_cluster <- cbind(target_w_score, df_cluster)
  
  # drop col description_clean, drop row names (tentative)
  source_target_cluster <- subset(source_target_cluster, select = -c(3))
  # source_target_cluster[,3] <- percent(source_target_cluster[,3], accuracy = 0.01)
  
  source_target_cluster
}

summary_fun = function(source_type, uploaded_txt, paste_source, stopwords_source, delete_words, extra_words, target_type, uploaded_csv, paste_target, number_of_clusters, n_gram_length, number_of_top_words) {
  
  uploaded_txt <- ifelse(source_type == 'Upload', isolate(uploaded_txt()), paste_source)
  
  if (target_type == 'Upload') {
    target_df <- isolate(uploaded_csv())
  } else {
    target_df <- target_fun(paste_target)
  }
  
  # create source dataframe, row combined
  source_df <- tibble(title = "Source", description=uploaded_txt)
  source_n_target <- rbind(source_df,target_df)
  
  # clean the source and target description, save as a new column
  stopwords_original <- stopwords_fun(stopwords_source, delete_words, extra_words)
  source_n_target$description_clean <- 
    lemmatize_strings(
      removeWords(
        prep_fun(source_n_target$description
        ),stopwords_original
      )
    )
  
  # use vocabulary_based vectorization
  itoken_source <- itoken(source_n_target$description_clean, progressbar = FALSE)
  v_source <- create_vocabulary(itoken_source)
  vectorizer_source <- vocab_vectorizer(v_source)
  
  # apply TF-IDF transformation
  dtm_source <- create_dtm(itoken_source, vectorizer_source)
  tfidf <- TfIdf$new()
  dtm_tfidf_source <- fit_transform(dtm_source, tfidf)
  
  # compute similarity-score against each row, saved as a new column
  # source_tfidf_cos_sim <- round(sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2"), digits = 4)
  source_tfidf_cos_sim <- sim2(x = dtm_tfidf_source, method = "cosine", norm = "l2")
  source_n_target["similarity_score"] <- source_tfidf_cos_sim[1:nrow(source_tfidf_cos_sim)]
  
  # drop the source row
  target_w_score <- source_n_target[-c(1), ] 
  
  # cluster starts; create a document term matrix 
  dtm <- CreateDtm(doc_vec = target_w_score$description, # character vector of documents
                   doc_names = target_w_score$title, # document names
                   ngram_window = c(1, n_gram_length), # minimum and maximum n-gram length
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
  clustering <- cutree(hc, k = number_of_clusters)
  
  # cluster ends; format as dataframe and bind with previous scoring result
  df_cluster <- as.data.frame(clustering,stringsAsFactors = default.stringsAsFactors)
  source_target_cluster <- cbind(target_w_score, df_cluster)
  
  # drop col description_clean, drop row names (tentative)
  source_target_cluster <- subset(source_target_cluster, select = -c(3))
  
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
                                # mean = round(mean_by_cluster[,1], digits = 4),
                                mean = mean_by_cluster[,1],
                                size = as.numeric(table(clustering)),
                                top_words =
                                  sapply(cluster_words, 
                                         function(d){
                                           paste(names(d)[order(d, decreasing = TRUE) ][ 1:number_of_top_words ],collapse = ", ")
                                         }),
                                stringsAsFactors = FALSE)
  
  # rank column (optional)
  cluster_summary <- cluster_summary[order(-cluster_summary[,2]),]
  cluster_summary <- mutate(cluster_summary, mean=percent(mean, accuracy = 0.01))
  # cluster_summary$rank <- rank(-cluster_summary$mean, ties.method = "min")
  
  cluster_summary
}
