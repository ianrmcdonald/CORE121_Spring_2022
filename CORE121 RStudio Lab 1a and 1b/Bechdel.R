library(tidyverse)
library(jsonlite)

raw_json <- jsonlite::parse_json(url("http://bechdeltest.com/api/v1/getAllMovies"))

all_movies <- raw_json %>% 
  map_dfr(~as.data.frame(.x, stringsAsFactors = FALSE)) %>% 
  rename(imdb_id = imdbid) %>% 
  tibble()

all_movies %>% 
  filter(year >= 1970) 



cleaned_bechdel <- all_movies %>% 
  mutate(title = case_when(
    str_detect(title, ", The") ~ str_remove(title, ", The") %>% paste("The", .),
    TRUE ~ str_replace(title, "&#39;", "’")
  ))

cleaned_bechdel %>% 
  write_csv("raw_bechdel.csv")

# IMDB data ---------------------------------------------------------------


imdb_json <- jsonlite::parse_json(url("https://raw.githubusercontent.com/brianckeegan/Bechdel/master/imdb_data.json"))

all_imdb <- imdb_json %>%
  map_dfr(~as.data.frame(.x, stringsAsFactors = FALSE))

cleaned_imdb <- all_imdb %>% 
  janitor::clean_names() %>% 
  mutate(metascore = parse_number(metascore),
         imdb_rating = parse_number(imdb_rating),
         year = as.integer(year)) %>% 
  mutate(imdb_id = str_remove(imdb_id, "tt")) %>% 
  tibble()

all_imdb

# 538 Data ----------------------------------------------------------------

movies <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/bechdel/movies.csv")

cleaned_movies <- movies %>% 
  mutate(imdb_id = str_remove(imdb, "tt")) 

combo_movies <- cleaned_movies %>% 
  left_join(cleaned_imdb) %>% 
  janitor::clean_names() 

combo_movies

combo_movies %>% 
  write_csv("2021/2021-03-09/movies.csv")
