library(tidycensus)
library(dplyr)
library(data.table)
library(janitor)

# TODO: set working directory

rm(list=ls()[!ls() %in% c("vars21")])

if(!"vars21" %in% ls()){
  vars21 <- tidycensus::load_variables(year=2021, 
                                       dataset = "acs5")
}


c.ptrn <- "^language spoken at home for the population"
#l.ptrn <- "language"

possible.concepts <- grep(c.ptrn, 
                          vars21$concept, 
                          ignore.case = T, 
                          value = T) %>% unique()


# grep(l.ptrn, 
#      vars21$label[vars21$concept %in% possible.concepts], 
#      ignore.case = T, 
#      value = T) %>% unique()

lang.list <- unique(vars21$label[vars21$concept %in% possible.concepts]) %>% 
  #gsub("\"", "", .) %>%
  .[!grepl("\"very well\"$", .)] 

lang.list


get.tbls <- vars21[vars21$label %in% lang.list & 
         vars21$concept %in% possible.concepts,]

languages.2021 <- tidycensus::get_acs(geography = "county", 
                    variables = get.tbls$name[2:14], 
                    year = 2021, 
                    state = "NC")


lang21 <- languages.2021
lang21$county <- lang21$NAME %>% gsub(" County,.*$", "", .)
lang21 <- select(lang21, county, variable, estimate)
lang21 <- left_join(lang21, get.tbls, 
          by = c("variable" = "name")) %>%
  mutate(., 
         year = 2021)

lang21_2 <- lang21 %>%
  select(., year, county, label, estimate) %>%
  mutate(., 
         label = gsub("^Estimate!!Total:!!|:", "", label)) %>%
  as.data.table() %>%
  dcast(., 
        year + county ~ label, 
        value.var = "estimate", 
        fun.aggregate = sum, fill = 0) %>% 
  as.data.frame() %>% as_tibble() %>%
  janitor::clean_names()

library(readr)

readr::write_csv(x = lang21_2, 
                 file = "languages_spoken_nc_counties_2021.csv")
