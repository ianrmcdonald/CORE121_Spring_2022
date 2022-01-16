knitr::opts_chunk$set(include = FALSE)
library(tidycensus)
library(magrittr)

st_codes_f <- function(code_name = "", full_name = "", state_only = FALSE, state_DC = FALSE) {
     
     st_df <- tidycensus::fips_codes %>% 
          dplyr::select(stcd = state, st_name = state_name) %>% 
          dplyr::distinct()
     
     
     if(state_only) {
          st_df <- st_df %>% dplyr::filter(stcd %in% datasets::state.abb)
          
     }
     
     if(state_DC) {
          st_df <- st_df %>% dplyr::filter(stcd %in% datasets::state.abb | stcd == "DC")
          print("state_DC true")
     }
     if(!missing(full_name)) {
          st_df <- st_df %>% 
               dplyr::filter(st_name %in% full_name)
     }
     
     if(missing("full_name") & !missing(code_name)) {
          st_df <- st_df %>% 
               dplyr::filter(stcd %in% code_name)
     }
     
     
     return(st_df)
     
}
