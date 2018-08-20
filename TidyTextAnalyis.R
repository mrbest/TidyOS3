library(dplyr)
library(readr)
library(tidytext)

export_description_norm <- function()
{
  df <- read_csv("OS3_FY17_Prices_Paid_Data_05172017.csv")
  df <- df %>% select(PROD_DESC_BY_VENDOR)
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub(",", " ", PROD_DESC_BY_VENDOR))
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub(":", " ", PROD_DESC_BY_VENDOR_WS))
  df <- df %>% mutate(PROD_DESC_BY_VENDOR_WS = gsub("  ", " ", PROD_DESC_BY_VENDOR_WS))
  df <- df %>% select(PROD_DESC_BY_VENDOR_WS)
  df
  }

tidy_format_process <- function(norm_desc)
{
 text_df <- data_frame(line = 1:nrow(norm_desc), text = norm_desc$PROD_DESC_BY_VENDOR_WS)
 text_df %>%
   unnest_tokens(word, text)
 
 data("stop_words")
 
 text_df <- text_df %>% 
   anti_join(stop_words)
sorted_text_df <- text_df %>% count(word, sort = TRUE)
}

tidy_format_process_trigram <- function(norm_desc)
{
  text_df_bigram <- data_frame(line = 1:nrow(norm_desc), text = norm_desc$PROD_DESC_BY_VENDOR_WS)
  text_df_bigram %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 3)
    
  sorted_text_df <- text_df %>% count(word, sort = TRUE)
}
