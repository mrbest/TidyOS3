library(dplyr)
library(readr)
library(tm)
library(tidytext)



main <- function()
{
normalized_descriptions <<- export_description_norm()
#sorted_1_grams <<- tidy_format_process(normalized_descriptions)
#sorted_tri_grams <<- tidy_format_process_trigram(normalized_descriptions)
source_tidy <<- tidy_format_process(normalized_descriptions)
tidy_dtm <<- create_dtm_from_tidy_dataset(source_tidy)

}

export_description_norm <- function()
{
  df <- read_csv("office_management_training_data.csv")
  #df <- df %>% select(PROD_DESC_BY_VENDOR)
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub(",", " ", PROD_DESC_BY_VENDOR))
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub(":", " ", PROD_DESC_BY_VENDOR_WS))
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub("  ", " ", PROD_DESC_BY_VENDOR_WS))
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub("[0-9]+", "", PROD_DESC_BY_VENDOR_WS))
  df <- df %>% mutate(level_3_category = SUB_CATEGORY)
  return_df <- df %>% select(PROD_DESC_BY_VENDOR_WS, level_3_category) %>% distinct()
  return_df
  }

tidy_format_process <- function(norm_desc)
{
  
 om_stopword <- read_csv("OM_stopword.csv")
 text_df <- data_frame(document = 1:nrow(norm_desc), text = norm_desc$PROD_DESC_BY_VENDOR_WS, level_3_category = norm_desc$level_3_category)
 #text_df <- data_frame( text = norm_desc$PROD_DESC_BY_VENDOR_WS, level_3_category = norm_desc$level_3_category)
 text_df <- text_df %>%
   unnest_tokens(word, text)
 
 data("stop_words")
 
 text_df <- text_df %>% 
   anti_join(stop_words)
#sorted_text_df <- text_df %>% count(word, sort = TRUE)
 text_df <- text_df %>% 
   anti_join(stop_words) %>%
   anti_join(om_stopword) %>%
   na.omit()
 
 text_df <- text_df %>%  count(document, word, sort = TRUE) %>%
   ungroup() %>% 
   rename(count=n)
 
 
   #bind_tf_idf(word, count)
 text_df
}

create_dtm_from_tidy_dataset <- function(training_tidy)
{
  training_tidy <- training_tidy %>% count(document, word, sort = TRUE) %>%
    ungroup() %>%
    cast_dtm(document, word, n) 
}


model_work <- function(source_tidy, source_dtm)
{
  k = tidy_df %>% select(level_3_category) %>% distinct() %>% count()
  seed = 3304
  lda <- LDA(source_dtm, k = k, method = "GIBBS", control = list(seed = seed))
  class(lda)
}


tidy_format_process_trigram <- function(norm_desc)
{
  text_df_bigram <- data_frame(line = 1:nrow(norm_desc), text = norm_desc$PROD_DESC_BY_VENDOR_WS)
  text_df_bigram <- text_df_bigram %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3)
    
  sorted_text_df <- text_df_bigram %>% count(trigram, sort = TRUE)
}
