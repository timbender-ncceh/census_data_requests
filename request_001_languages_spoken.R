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

#cw.coc <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/crosswalks/county_district_region_crosswalk.csv")
cw.coc <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/crosswalks/coc_county_crosswalk.csv")


lang21_3 <- as.data.table(lang21_2) %>%
  melt(., 
       id.vars = c("year", "county"), 
       value.name = "estimate", 
       variable.name = "language") %>%
  as.data.frame() %>%
  as_tibble() %>%
  left_join(., cw.coc) %>%
  #.[.$coc_short == 503,] %>%
  .[.$estimate > 0,] %>%
  group_by(language, 
           bos_county = coc_short == 503) %>%
  summarise(t_hh = sum(estimate), 
            pct_hh = NA,
            n_bos_counties = n_distinct(county)) %>%
  .[order(.$bos_county,.$t_hh, decreasing = T),] %>%
  ungroup() %>%
  group_by(bos_county) %>%
  mutate(., 
         pct_hh = round(t_hh / sum(t_hh),3), 
         pct_bos_counties = round(n_bos_counties / max(n_bos_counties),3))

colnames(lang21_3) <- c("language_spoken_at_home", 
                        "CoC", 
                        "total_CoC_households", 
                        "percent_of_CoC_households", 
                        "number_of_CoC_counties_language_spoken_in",  
                        "percent_of_CoC_counties_language_spoken_in")
lang21_3$CoC <- ifelse(T, "NC-503", "All Others")

lang21_3

write_csv(lang21_3, 
          "summary_languages_spoken_nc_counties_2021.csv")
