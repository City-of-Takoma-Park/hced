library(tidycensus)
library(dplyr)
library(tidyverse)
# library(conflicted)

library(danfuncts)
library(acsprocess)

# conflict_prefer("filter", "dplyr")

library(sf)
library(leaflet)
library(htmltools)
# place_view <- st_read("./data/nhgis/usplaces_2019/US_place_2019.shp")

md_place <- place_view %>%
  filter(grepl(24, STATEFP)) %>%
  st_transform(4326)

leaflet(md_place) %>%
  addTiles %>%
  addPolygons(label = map(md_place$NAME, ~ HTML(text = paste0("Place: ", .x))))

variables <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

variables_2018 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)

### create functions
# function to copy labels in and filter to tp
puller_funct <- function(df, place_string, no_pull = FALSE) {
  
  if (!no_pull){
    df %>%
      rename_all(tolower) %>%
      filter(grepl(place_string, name)) %>%
      left_join(variables, by = c("variable" = "name"))
  }
  
  else if (no_pull){
    df %>%
      rename_all(tolower) %>%
      left_join(variables, by = c("variable" = "name"))
  }
  
}

data_puller <- function(root_string, data_string){
  file_name <- paste0("data/", root_string, data_string, ".rds")
  print(file_name)
  
  read_rds(file_name)
  
}


data_saver <- function(object, place_string, data_string, desc_year = "acs5_2019", place_dir = "all"){
  if (!dir.exists(paste0("data/output_data/",
                         desc_year))){
    
    dir.create(paste0("./data/output_data/",
                      desc_year))
  }
  
  saveRDS(object, file = paste0("data/output_data/",
                                desc_year,
                                "/",
                                place_dir,
                                "/",
                                data_string,
                                "_",
                                place_string,
                                ".rds"))
  
}

race_ethn_processor <- function(df, place, no_pull){
  
  race_ethn_processed <- df %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "race_ethnicity", "two"))
  
  race_ethn_processed <- race_ethn_processed %>%
    mutate(race_ethnicity = ifelse(two == "White alone" & !is.na(two), "NH White alone", race_ethnicity),
           two = ifelse(two == "White alone", NA, two)) %>%
    filter(is.na(two) & (!is.na(race_ethnicity) | !grepl("HISPANIC", concept, ignore.case = T)))
  
  # browser()
  
  race_ethn_processed <- race_ethn_processed %>%
    total_col_add(total_cols = list("pop_total" = "race_ethnicity")) %>%
    select(-c(concept, variable)) %>%
    distinct() %>%
    filter(!(grepl("Two or more", race_ethnicity) & !is.na(two))) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "pop_total",
                       aggregate_moe = "pop_total_moe") %>%
    select(-c(label, two))
  
}

computer_overall_process <- function(df, place, no_pull){
  df %>%
    rename_all(tolower) %>%
    puller_funct("Takoma Park", no_pull = T) %>%
    separate_label(c(NA, NA, "comp_access", "int_type")) %>%
    race_pull() %>%
    mutate(place_race = paste0(name, "_", race)) %>%
    mutate(int_type = ifelse(grepl("No computer", comp_access, ignore.case = T), "No computer", int_type)) %>%
    total_col_add(c("tot_people" = "comp_access", "tot_comp" = "int_type"), join_col = c("place_race")) %>%
    derive_pct_est_moe(proportion_col = "percent_race",
                       aggregate_est = "tot_people", 
                       aggregate_moe = "tot_people_moe")
}


# function to process education data overall
educ_overall_process <- function(df, place, no_pull){
  df %>% 
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "education")) %>%
    total_col_add(total_cols = list("population" = "education")) %>%
    derive_pct_est_moe(proportion_col = "percent_pop", 
                       aggregate_est = "population",
                       "population_moe")
}

# function to process race/household income
income_race <- function(df, place, no_pull){
  
  df %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    race_pull() %>%
    separate_label(c(NA, NA, "income_range")) %>%
    filter(race != "white alone") %>%
    total_col_add(total_cols = list("race_households" = "income_range"), join_col = c("name", "race")) %>%
    derive_pct_est_moe(proportion_col = "percent_race_households",
                       aggregate_est = "race_households",
                       aggregate_moe = "race_households_moe") 
}

race_income_household <- data_puller("all_municip_acs5_2019_", "Household income by race") %>%
  puller_funct(place_string = "Takoma", no_pull = T) %>%
  race_pull() %>%
  separate_label(c(NA, NA, "income_range")) %>%
  filter(race != "white alone householder") %>%
  ungroup %>%
  total_col_add(total_cols = list("race_households" = "income_range"), join_col = c("name", "race")) %>%
  derive_pct_est_moe(proportion_col = "percent_race_households",
                     aggregate_est = "race_households",
                     aggregate_moe = "race_households_moe") 

# function to process median income for race
income_race_median <- function(df, place, no_pull) {
  df %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    race_pull() %>%
    mutate(race = ifelse(grepl("in 2019 inflation", race), NA, race)) %>%
    separate_label(c(NA, "Median household income")) %>%
    total_col_add(total_cols = list("overall_median_income" = "race")) %>%
    filter(race != "white alone") %>%
    derive_pct_est_moe(proportion_col = "ratio_median_income",
                       aggregate_est = "overall_median_income",
                       aggregate_moe = "overall_median_income_moe", 
                       type_moe = "ratio")
}

employment_age_gender_process <- function(df){
  df %>%
    filter(grepl("In labor force", labor_force_status)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force)) %>%
    total_col_add(list("total_workers" = "type_labor_force"), join_col = c("name", "gender", "age_range")) %>%
    filter(!(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    est_moe_derive(group_cols = c("name", "gender", "age_range", "type_labor_force", "employment_status"), 
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_gender_age",
                       aggregate_est = "total_workers",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, estimate, moe, label, concept, race)) %>%
    distinct()
}

educ_race_process <- function(df, place, no_pull){
  
  # education/race overall
  educ_race_df_overall <- df %>%
    filter(!is.na(education) | is.na(education) & is.na(gender)) %>%
    total_col_add(list("race_total" = "gender"), join_col = c("name", "race")) %>%
    est_moe_derive(c("name", "race", "education")) %>%
    select(-c(variable, gender, label, concept, estimate, moe)) %>%
    distinct()
  
  educ_race_df_overall <- educ_race_df_overall %>%
    derive_pct_est_moe(proportion_col = "race_education_pct", 
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe",
                       component_est = "name_race_education_est",
                       component_moe = "name_race_education_moe") 
  
  educ_race_df_overall
  
} 

tenure_race_process <- function(df){
  
  df %>%
    filter(!is.na(tenure)) %>%
    total_col_add(join_col = c("name", "tenure"), total_cols = list("tenure_overall" = "race")) %>%
    select(-c(variable, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "race_pct_tenure",
                       aggregate_est = "tenure_overall",
                       aggregate_moe = "tenure_overall_moe") %>%
    arrange(tenure, race)
}

burden_owners_process <- function(df){
  
  burden_owners_overall <- df %>%
    filter(!(!is.na(mortgage_status) & is.na(percent_household_income))) %>%
    total_col_add(total_cols = list("total_homeowners" = "mortgage_status"), join_col = c("name")) %>%
    filter(!is.na(percent_household_income)) %>%
    est_moe_derive(group_cols = c("name", "percent_household_income")) %>%
    select(-c(concept, mortgage_status, variable, label, estimate, moe)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners",
                       aggregate_est = "total_homeowners",
                       aggregate_moe = "total_homeowners_moe",
                       component_est = "name_percent_household_income_est",
                       component_moe = "name_percent_household_income_moe") %>%
    mutate(cumulpct = cumsum(pct_homeowners))
  
  return(burden_owners_overall)
}

burden_owners_mortgage_process <- function(df){
  df %>%
    filter(!is.na(mortgage_status)) %>%
    total_col_add(total_cols = list("total_homeowners_mortgage" = "percent_household_income"), join_col = c("name", "mortgage_status")) %>%
    select(-c(concept, variable, label)) %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners_mortgage",
                       aggregate_est = "total_homeowners_mortgage",
                       aggregate_moe = "total_homeowners_mortgage_moe")
}

race_employ_gender_u65_process <- function(df) {
  df %>%
    filter(!grepl("sex by age", race) & 
             grepl("In labor force", labor_force_status) & 
             grepl("16 to 64", age_range) & 
             !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status)) %>%
    total_col_add(list("total_labor_force" = "type_labor_force"), c("name", "gender", "race")) %>%
    est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "total_labor_force",
                       aggregate_moe = "total_labor_force_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    distinct()
}

race_employment_u65_process <- function(df, tot_df) {
  
  df %>%
    left_join(tot_df) %>%
    filter(!is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "race", "type_labor_force", "employment_status"), 
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "total_workers_est",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, gender, age_range, estimate, moe, label, concept)) %>%
    distinct()
}

# owner housing costs by income
process_household_income_ownercosts <- function(df) {
  df %>%
    total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    filter(housing_costs != "Not computed") %>%
    est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")
  
} 

process_household_income_rentercosts <- function(df){
  df %>%
    total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    filter(housing_costs != "Not computed") %>%
    est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")
}

# ancestry <- data_puller(root_string = "acs52019/all_municip_acs5_", "Ancestry") %>%
#   puller_funct("Takoma", T) 
# 
# ancestry_overall <- ancestry %>%
#   separate_label(c(NA, NA, "ancestry", "country")) %>%
#   filter(is.na(country)) %>%
#   select(-country) %>%
#   total_col_add(total_cols = list("tot_pop" = "ancestry")) %>%
#   derive_pct_est_moe("pct_ancestry",
#                      "tot_pop",
#                      "tot_pop_moe")
# 
# 
# ancestry_sub <- ancestry %>%
#   separate_label(c(NA, NA, "ancestry", "country")) %>%
#   filter(ancestry %in% ancestry_combos) %>%
#   total_col_add(list("tot_ancestry" = "country"), join_col = c("name", "ancestry")) %>%
#   derive_pct_est_moe("pct_country",
#                      "tot_ancestry",
#                      "tot_ancestry_moe")

process_ancestry <- function(df, sub = F){
  
  ancestry <- df %>%
    separate_label(c(NA, NA, "ancestry", "country"))
  
  if (sub){
    ancestry_combos <- ancestry %>%
      filter(!is.na(country)) %>%
      pull(ancestry) %>%
      unique()
    
    ancestry <- ancestry %>%
      filter(ancestry %in% ancestry_combos) %>%
      total_col_add(list("tot_ancestry" = "country"), join_col = c("name", "ancestry")) %>%
      derive_pct_est_moe("pct_country",
                         "tot_ancestry",
                         "tot_ancestry_moe")
    
  }
  
  else{
    ancestry <- df %>%
      separate_label(c(NA, NA, "ancestry", "country")) %>%
      filter(is.na(country)) %>%
      select(-country) %>%
      total_col_add(total_cols = list("tot_pop" = "ancestry")) %>%
      derive_pct_est_moe("pct_ancestry",
                         "tot_pop",
                         "tot_pop_moe")
  }

  return(ancestry)
  
}

process_health_race <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "health")) %>%
    race_pull() %>%
    mutate(name_race = paste0(name, "_", race)) %>%
    total_col_add(total_cols = list("race_total" = "age", "race_age_total" = "health"), join_col = c("name_race", "age")) %>%
    derive_pct_est_moe(proportion_col = "pct_age", 
                       aggregate_est = "race_age_total",
                       aggregate_moe = "race_age_total_moe")
    
  
}


total_col_add <- function(df,
                          total_cols,
                          join_col = "name",
                          est_col = "estimate") {
  
  return_df <- df %>%
    dplyr::rename_all(tolower)
  
  total_cols_vec <- total_cols
  
  total_cols_name <- names(total_cols)
  
  # loop through list of total columns to pull
  purrr::walk(seq_along(total_cols_vec), ~ {
    
    # browser()
    
    select_tot_col <- total_cols_vec[[.x]]
    select_cols_name <- total_cols_name[[.x]]
    
    if (length(join_col) > 1 & length(total_cols) > 1){
      # subset join columns
      join_col <-join_col[1:.x]
    }
    
    # create name of moe for total
    group_moe <- paste0(select_cols_name, "_moe")
    
    # filter to total
    total_df <- return_df %>%
      dplyr::filter(is.na(!!sym(select_tot_col)))
    
    # update moe to 0 if missing - per this discussion - https://github.com/walkerke/tidycensus/issues/29
    
    # pull total value and moe for that (na value of subgroup column = total for that group
    total_df <- total_df %>%
      dplyr::mutate(!!sym(select_cols_name) := !!sym(est_col),
                    !!sym(group_moe) := ifelse(is.na(moe), 0, moe))
    
    # filter to total column and its moe as well as column to rejoin data by
    total_df <- total_df %>%
      dplyr::select(c(join_col, select_cols_name, group_moe))
    
    # join total data to df and filter out total-row
    return_df <<- return_df %>%
      left_join(total_df) %>%
      dplyr::filter(!is.na(!!sym(select_tot_col)))
    
  })
  
  group_vec <- c(join_col, unname(unlist(total_cols, use.names = F)), "variable", est_col)
  
  df_check <- return_df %>%
    dplyr::group_by(dplyr::across(group_vec)) %>%
    dplyr::mutate(freq = n())
  
  if (any(df_check %>% pull(freq) > 1) | nrow(df_check) > nrow(df)){
    browser()
    
    stop("Observations that are supposed to be unique repeat; check whether join columns and total columns align")
  }
  
  return(return_df)
}



process_disability_overall <- function(df){
  df %>%
    separate_label(c(NA, NA, "sex", "age", "disability")) %>%
    total_col_add(c("tot_pop" = "sex",
                    "tot_sex" = "age",
                    "tot_age" = "disability"), 
                  c("name",
                    "sex",
                    "age")) %>%
    derive_pct_est_moe(proportion_col = "pct_disability", 
                       aggregate_est = "tot_age", 
                       aggregate_moe = "tot_age_moe")
}


# health_root <- data_puller("acs52019/all_municip_acs5_", "Health insurance by age") %>%
#   puller_funct("Takoma", T)%>%
#   separate_label(c(NA, NA, "age", "num_plans", "type_coverage")) %>%
#   process_health_age()
# 
#   
process_health_age <- function(df) {
  # browser()
  df <- df %>%
    separate_label(c(NA, NA, "age", "num_plans", "type_coverage")) 
  
  # correct for no insurance
  df_none <- df %>%
    filter(grepl("No health insurance", num_plans)) %>%
    distinct() %>%
    # correct for msissing
    mutate(type_coverage = ifelse(grepl("No health insurance", num_plans), "No insurance", type_coverage)) 
  
  
  df %>% 
    rbind(df_none) %>%
    total_col_add(total_cols = c("total" = "age", "age_total" = "num_plans", "age_num_plans_total" = "type_coverage"), join_col = c("name", "age", "num_plans")) %>%
    derive_pct_est_moe("pct_hc",
                       "age_num_plans_total",
                       "age_num_plans_total_moe")
}

process_gender <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender")) %>%
    total_col_add(total_cols = c("total" = "gender")) %>%
    derive_pct_est_moe("pct_gender", "total", "total_moe") 
}

process_age_gender <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender", "age")) %>%
    total_col_add(c("tot_people" = "gender", "tot_gender" = "age"), c("name", "gender")) %>%
    derive_pct_est_moe("pct_age",
                       "tot_gender",
                       "tot_gender_moe")
}

process_race_age_gender <- function(df){
  
  df %>%
    separate_label(c(NA, NA, "gender", "age")) %>%
    acsprocess::race_pull() %>%
    mutate(race_name = paste0(name, "_", race)) %>%
    total_col_add(c("race_tot"= "gender", "gender_tot" = "age"), c("race_name", "gender")) %>%
    derive_pct_est_moe("pct_age",
                       "gender_tot",
                       "gender_tot_moe")
  
}  


process_computer_age <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "comp", "int")) %>%
    mutate(int = ifelse(grepl("No", comp), comp, int)) %>%
    total_col_add(c("pop_tot" = "age", "age_tot" = "comp", "comp_tot" = "int"), 
                  c("name", "age", "comp")) %>%
    comp_recode(comp_col = comp, int) %>%
    derive_pct_est_moe("pct_int",
                       "age_tot",
                       "age_tot_moe")
}
# 
# test <- data_puller(root_string = "acs52019/all_municip_acs5_", "Poverty by race and age") %>%
#   puller_funct("Takoma", no_pull = T) %>%
#   separate_label(c(NA, NA, "poverty", "age")) %>%
#   race_pull() %>%
#   mutate(name_race = paste0(name, "_", race)) %>%
#   total_col_add(c("race_tot" = "poverty", "poverty_tot" = "age"),
#                 c("name_race", "poverty")) %>%
#   est_moe_derive(c("name", "age", "race"), name_col = "age_race_tot") %>%
#   derive_pct_est_moe("pct_age",
#                      "age_race_tot_est",
#                      "age_race_tot_moe")
# 
#   
process_poverty_race_age <- function(df) {
  df %>%
    separate_label(c(NA, NA, "poverty", "age")) %>%
    race_pull() %>%
    mutate(name_race = paste0(name, "_", race)) %>%
    total_col_add(c("race_tot" = "poverty", "poverty_tot" = "age"),
                  c("name_race", "poverty")) %>%
    est_moe_derive(c("name", "age", "race"), name_col = "age_race_tot") %>%
    derive_pct_est_moe("pct_age",
                       "age_race_tot_est",
                       "age_race_tot_moe")
}

asian_disag <- data_puller("acs52019/all_municip_acs5_", "Asian disaggregated") %>%
  puller_funct("", T)

process_asian_disaggregated <- function(df){
  df %>%
    separate_label(c(NA, NA, "ethnicity")) %>%
    total_col_add(list("tot_asian" = "ethnicity")) %>%
    derive_pct_est_moe("pct_asian",
                       "tot_asian",
                       "tot_asian_moe")
}



# poverty_detail <- data_puller("acs52019/all_municip_acs5_", "Poverty status detailed") %>%
#   puller_funct("", T) 

process_poverty_detail <- function(df){
  df %>%
    separate_label(c(NA, NA, "incpov_ratio")) %>%
    total_col_add(c("pop_tot" = "incpov_ratio")) %>%
    derive_pct_est_moe("pct_incpov",
                       "pop_tot",
                       "pop_tot_moe")
}


tenure_age <- data_puller("acs52019/all_municip_acs5_", "Tenure by age") %>%
  puller_funct("", T)

process_tenure_age <- function(df){
  df %>%
    separate_label(c(NA, NA, "tenure", "age")) %>%
    total_col_add(c("pop_tot" = "tenure", "tenure_tot" = "age"),
                  c("name", "tenure")) %>%
    derive_pct_est_moe("pct_age",
                       "tenure_tot",
                       "tenure_tot_moe")
}

age_lang <- data_puller("acs52019/all_municip_acs5_", "Language by age") %>%
  puller_funct("", T)


process_age_lang <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "homelang")) %>%
    total_col_add(c("pop_tot" = "age", "age_tot" = "homelang"), c("name", "age")) %>%
    derive_pct_est_moe("pct_age",
                       "age_tot",
                       "age_tot_moe")
}

disab_pull <- function(df, new_col = disability, concept_col = concept){
  df %>%
    mutate({{new_col}} := {{concept_col}} %>%
             tolower %>%
             gsub("sex by age by ", replacement = "", x = .) %>%
             gsub(" difficulty", "", .) %>%
             stringr::str_to_title(.))
}

read_disab_type <- function(df, pop_tot_df){
  df %>%
    disab_pull(disability, concept) %>%
    mutate(name_disab = paste0(name, "_", disability)) %>%
    separate_label(c(NA, NA, "sex", "age", "disab_type")) %>%
    total_col_add(total_cols = 
                    c("pop_tot" = "sex", 
                      "sex_tot" = "age", 
                      "age_tot" = "disab_type"), 
                  join_col = c("name_disab", "sex", "age")) %>%
    mutate(disab_pop = ifelse(grepl("With a", 
                             disab_type),
                             "Has a disability",
                             "No disability")) %>%
    # drop population total and join population total data because some disability categories dont include all pop
    select(-c(pop_tot, pop_tot_moe)) %>%
    left_join(pop_tot_df)
}

process_pop_tot <- function(race_ethn_processed_df){
  race_ethn_processed_df %>%
    select(geoid, name, pop_total, pop_total_moe) %>%
    distinct()
}

# disability overall over pop
process_disability_pop <- function (df_disability){
  df_disability %>%
    est_moe_derive(c("name", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, name_disability_est, name_disability_moe) %>%
    distinct()
} 

# disability over age
process_disability_age <- function (df_disability){
  df_disability %>%
    est_moe_derive(c("name", "age", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, age, name_age_disability_est, name_age_disability_moe) %>%
    distinct()
} 

# disability by type out of pop
process_disab_type <- function(df, df_disab_pop){
  df %>%
    select(-c(sex, age, variable, label)) %>%
    est_moe_derive(group_cols = c("name", "disab_type"), 
                   est_col = "estimate", 
                   moe_col = "moe") %>%
    select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    distinct() %>%
    left_join(df_disab_pop) %>%
    filter(disab_pop == "Has a disability") %>%
    derive_pct_est_moe("name_disab_type_pct_pop", 
                       "pop_total",
                       "pop_total_moe",
                       "name_disab_type_est",
                       "name_disab_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("name_disab_type_pct_disab", 
                       "name_disability_est",
                       "name_disability_moe",
                       "name_disab_type_est",
                       "name_disab_type_moe") %>%
    rename(pct_moe_disab = pct_moe)
}

# df <- data_puller("acs52019/all_municip_acs5_", "Disability by age and type") %>%
#   puller_funct(place_string = "", no_pull = T)

# race_ethn_processed <- data_puller("acs52019/all_municip_acs5_", "Race_ethnicity") %>%
#   race_ethn_processor(place = "place", no_pull = T)

# disability <- data_puller(root_string = "acs52019/all_municip_acs5_", "Disability overall") %>%
#   puller_funct(place_string = "Takoma", no_pull = T) %>%
#   process_disability_overall

# age_gender <- data_puller(root_string = "acs52019/all_municip_acs5_", "Age by gender") %>%
#   puller_funct("place", no_pull = T) %>%
#   process_age_gender
# 
# process_gender_overall <- function(age_df){
#   age_gender %>%
#     select(geoid, name, gender, tot_gender, tot_gender_moe) %>%
#     distinct()
# }
# 
# age_gender_processed <- process_gender_overall(age_gender)

process_sex_disab_overall <- function(df){
  df %>%
    est_moe_derive(c("name", "sex", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, sex, tot_sex, tot_sex_moe, name_sex_disability_est, name_sex_disability_moe) %>%
    distinct()
}

process_disab_sex <- function(df, df_disab_sex){
  # browser()
  
  df %>%
    est_moe_derive(c("name", "sex", "disability", "disab_type"), 
                   name_col = "sex_disability_type") %>%
    filter(grepl("With a", disab_type)) %>%
    select(geoid, name, sex, disability, pop_total, pop_total_moe, sex_disability_type_est, sex_disability_type_moe) %>%
    distinct() %>%
    left_join(df_disab_sex) %>%
    derive_pct_est_moe("sex_disability_type_pct_disability", 
                       "name_sex_disability_est",
                       "name_sex_disability_est",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_disab = pct_moe) %>%
    derive_pct_est_moe("sex_disability_type_pct_pop", 
                       "pop_total",
                       "pop_total_moe",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("sex_disability_type_pct_sex", 
                       "tot_sex",
                       "tot_sex_moe",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_sex = pct_moe)
}

# disability by type out of age
process_disab_age <- function(df, disab_age_df) {
  df %>%
    select(-c(sex, variable, label)) %>%
    left_join(disab_age_df) %>%
    est_moe_derive(group_cols = c("name_disab", "age"), 
                   est_col = "estimate", 
                   moe_col = "moe", 
                   name_col = "age_pop") %>%
    # est_moe_derive(group_cols = c("name", "age", "disab_pop"),
    #                est_col = "estimate",
    #                moe_col = "moe",
    #                name_col = "age_disab_pop") %>%
    est_moe_derive(group_cols = c("name", "age", "disab_type"), 
                   est_col = "estimate", 
                   moe_col = "moe",
                   "age_disab_type") %>%
    select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    distinct() %>%
    filter(disab_pop == "Has a disability") %>%
    derive_pct_est_moe("age_disab_type_pct_age", 
                       "age_pop_est",
                       "age_pop_moe",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_age = pct_moe) %>%
    derive_pct_est_moe("age_disab_type_pct_pop", 
                       "pop_total",
                       "pop_total_moe",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("age_disab_type_pct_disab", 
                       "name_age_disability_est",
                       "name_age_disability_est",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_disab = pct_moe)
}

# df <- data_puller("acs52019/all_municip_acs5_", "Poverty by sex") %>%
#   puller_funct("Takoma", no_pull = T)

process_poverty_sex <- function(df){
  return <- df %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"), 
                  join_col = c("name", "pov_status", "sex")) %>%
    est_moe_derive(c("name", "sex"), name_col = "pop_sex") %>%
    select(-c(variable, estimate, moe, label, age)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_sex_pov", 
                       "pop_sex_est",
                       "pop_sex_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
                       ) %>%
    rename(pct_moe_sex = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pop", 
                       "pop",
                       "pop_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("pct_pov_all", 
                       "pov_tot",
                       "pov_tot_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    rename(pct_moe_pov_all = pct_moe)
}

# df <- data_puller("acs52019/all_municip_acs5_", "Poverty by sex and race") %>%
#   puller_funct("Takoma", no_pull = T)

process_poverty_race_sex <- function(df){
  return <- df %>%
    race_pull() %>%
    mutate(name_race = paste0(name, race)) %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("race_pop" = "pov_status",
                    "pov_race_tot" = "sex",
                    "pov_race_sex_tot" = "age"), 
                  join_col = c("name_race", "pov_status", "sex")) %>%
    est_moe_derive(c("name_race", "sex"), name_col = "race_sex")
  
  overall <- return %>%
    # filter out overlapping ethnicity
    filter(!grepl("hispanic", race, ignore.case = T)) %>%
    est_moe_derive(c("name", "sex"), name_col = "sex") %>%
    est_moe_derive(c("name"), name_col = "pop") %>%
    est_moe_derive(c("name", "pov_status"), name_col = "pov") %>%
    est_moe_derive(c("name", "sex", "pov_status"), name_col = "sex_pov") %>% 
    select(geoid, name, sex, pov_status, pop_est, pop_moe, sex_est, sex_moe, pov_est, pov_moe, sex_pov_est, sex_pov_moe) %>%
    distinct()
  
  return_final <- return %>%
    left_join(overall) %>%
    select(-c(variable, estimate, moe, label, age)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_race_sex_pov", 
                       "race_sex_est",
                       "race_sex_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_race_sex_pov = pct_moe) %>%
    derive_pct_est_moe("pct_race_pov", 
                       "race_pop",
                       "race_pop_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_race_pov = pct_moe) %>%
    derive_pct_est_moe("pct_pov", 
                       "pov_est",
                       "pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_pov = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pov", 
                       "sex_pov_est",
                       "sex_pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_sex_pov = pct_moe)
}


process_poverty_sex_age <- function(df){
  return <- df %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"), 
                  join_col = c("name", "pov_status", "sex")) %>%
    est_moe_derive(c("name", "sex"), name_col = "sex_pop") %>%
    est_moe_derive(c("name", "sex", "age"), name_col = "sex_age_pop") %>%
    est_moe_derive(c("name", "pov_tot"), name_col = "pov_tot") %>%
    select(-c(variable, label)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_sex_pov", 
                       "pov_sex_tot",
                       "pov_sex_tot_moe"
    ) %>%
    rename(pct_moe_sex_pov = pct_moe) %>%
    derive_pct_est_moe("pct_pop", 
                       "pop",
                       "pop_moe",
    ) %>% 
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pop", 
                       "sex_pop_est",
                       "sex_pop_moe") %>%
    rename(pct_moe_sex_pop = pct_moe) %>%
    derive_pct_est_moe("pct_pop_pov", 
                       "pov_tot_est",
                       "pov_tot_moe") %>%
    rename(pct_moe_pop_pov= pct_moe) %>%
    derive_pct_est_moe("pct_sex_age",
                       "sex_age_pop_est",
                       "sex_age_pop_moe")
}



#### build datasets
## function to build and save all datasets of interest
data_creator <- function(root_string, place, no_pull = T, desc_year = "acs5_2019"){
  # computer use
  comp_race <- data_puller(root_string, "Computer overall and by race") %>%
    computer_overall_process(place = place, no_pull = T)
  
  
  data_saver(comp_race, 
             place, 
             "comp_race",
             desc_year = desc_year)
  
  # overall educational attainment
  educ_overall <- data_puller(root_string, "Education overall") %>%
    educ_overall_process(place = place, no_pull = no_pull)
  
  data_saver(educ_overall, 
             place, 
             "educ_overall",
             desc_year = desc_year)
  
  # household income by race
  race_income_household <- data_puller(root_string, "Household income by race") %>%
    income_race(place = place, no_pull = no_pull)
  
  data_saver(race_income_household, 
             place, 
             "race_income_household",
             desc_year = desc_year)
  
  # median income by race
  race_income_med <- data_puller(root_string, "Median income by race") %>%
    income_race_median(place = place, no_pull = no_pull)
  
  data_saver(race_income_med, 
             place, 
             "race_income_med",
             desc_year = desc_year)
  
  educ_race_df <- data_puller(root_string, "Educational attainment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "gender", "education")) %>%
    race_pull() %>%
    filter(race != "white alone")
  
  ## educ attainment by race
  educ_race_df_overall <- educ_race_df %>%
    educ_race_process(place = place, no_pull= no_pull)
  
  data_saver(educ_race_df_overall, 
             place, 
             "educ_race_df_overall",
             desc_year = desc_year)
  
  # education/race/gender
  educ_race_gender_df <- educ_race_df %>%
    filter(!is.na(gender)) %>%
    mutate(race_place = paste0(name, "_", race)) %>%
    total_col_add(list("race_gender_total" = "education"), join_col = c("race_place", "gender")) %>%
    filter(!is.na(education)) %>%
    est_moe_derive(c("name", "race", "education", "gender")) %>%
    select(-c(variable, label, concept, estimate, moe)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "race_education_gender_pct", 
                       aggregate_est = "race_gender_total",
                       aggregate_moe = "race_gender_total_moe",
                       component_est = "name_race_education_gender_est",
                       component_moe = "name_race_education_gender_moe") 
  
  data_saver(educ_race_df_overall, 
             place, 
             "educ_race_gender_df",
             desc_year = desc_year)
  
  # foreign born by birth
  foreign_born_birth <- data_puller(root_string = root_string, "Foreign born place of birth") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "continent", "americas")) %>%
    mutate(continent = ifelse(!is.na(americas), americas, continent)) %>%
    select(-americas)
  
  foreign_born_birth_processed <- foreign_born_birth  %>%
    total_col_add(join_col = "name", total_cols = list("continent_overall" = "continent")) %>%
    select(-c(variable, label, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "continent_pct", 
                       aggregate_est = "continent_overall",
                       aggregate_moe = "continent_overall_moe",
                       component_est = "estimate",
                       component_moe = "moe") 
  
  data_saver(foreign_born_birth_processed,
             place,
             "foreign_born_birth_processed",
             desc_year = desc_year)
  
  # foreign born overall
  foreign_born <- data_puller(root_string = root_string, 
                              "Foreign born") %>%
    puller_funct(place_string = place, no_pull = no_pull)
  
  foreign_born_processed <- foreign_born %>%
    separate_label(c(NA, NA, "birthplace")) %>%
    total_col_add(join_col = "name", total_cols = list("birth_overall" = "birthplace")) %>%
    derive_pct_est_moe(proportion_col = "birth_pct", 
                       aggregate_est = "birth_overall",
                       aggregate_moe = "birth_overall_moe",
                       component_est = "estimate",
                       component_moe = "moe") 
  
  data_saver(foreign_born_processed, 
             place, 
             "foreign_born_processed",
             desc_year = desc_year)
  
  # home ownership by race
  race_home_own <- data_puller(root_string = root_string, 
                               "Home ownership by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "tenure")) %>%
    race_pull() %>%
    mutate(race = ifelse(race == "tenure", NA, race))
  
  ## race as % of renter/owner
  race_home_own_race_tenure <- race_home_own %>%
    filter(!is.na(race)) %>%
    total_col_add(join_col = c("name", "race"), 
                  total_cols = list("race_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "race_pct", 
                       aggregate_est = "race_overall",
                       aggregate_moe = "race_overall_moe", 
                       component_est = "estimate", component_moe = "moe") 
  
  
  data_saver(race_home_own_race_tenure, 
             place,
             "race_tenure",
             desc_year = desc_year)
  
  ## renter/owner as % of race
  tenure_race <- race_home_own %>%
    tenure_race_process()
  
  data_saver(tenure_race,
             place,
             "tenure_race",
             desc_year = desc_year)
  
  ## housing tenure
  tenure_df <- data_puller(root_string = root_string, "Housing tenure") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "tenure")) %>%
    total_col_add(total_cols = list("tenure_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "pct_tenure",
                       aggregate_est = "tenure_overall",
                       aggregate_moe = "tenure_overall_moe")
  
  data_saver(tenure_df, place, "tenure_df", desc_year = desc_year)
  
  # income by tenure
  tenure_income_median <- data_puller(root_string = root_string, "Income by tenure") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, NA, "tenure")) %>%
    total_col_add(total_cols = list("median_income_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "ratio_income",
                       aggregate_est = "median_income_overall",
                       aggregate_moe = "median_income_overall_moe",
                       type_moe = "ratio") %>%
    select(-c(label, concept, variable))
  
  data_saver(tenure_income_median, place, "tenure_income_median", desc_year = desc_year)
  
  # income last twelve months
  income_last12 <- data_puller(root_string = root_string, "Income last twelve months") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "income_range"))
  
  income_last12_processed <- income_last12 %>%
    total_col_add(total_cols = list("pop_overall" = "income_range")) %>%
    derive_pct_est_moe(proportion_col = "pct_income_range",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe") %>%
    group_by(name) %>%
    mutate(pct_cumul = cumsum(pct_income_range))
  
  data_saver(income_last12_processed, place, "income_last12", desc_year = desc_year)
  
  # language at home
  home_lang <- data_puller(root_string = root_string, "Language at home") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "language")) %>% 
    total_col_add(total_cols = list("pop_overall" = "language")) %>%
    derive_pct_est_moe(proportion_col = "pct_language",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe")
  
  data_saver(home_lang, place, "home_lang", desc_year = desc_year)
  
  # poverty
  poverty <- data_puller(root_string = root_string, "Poverty") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "income_rel_poverty")) %>%
    total_col_add(total_cols = list("pop_overall" = "income_rel_poverty")) %>%
    derive_pct_est_moe(proportion_col = "pct_income",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe") %>%
    mutate(pct_cumul_income = cumsum(pct_income))
  
  data_saver(poverty, place,  "poverty", desc_year = desc_year)
  
  # assistance by race
  race_assist <- data_puller(root_string = root_string, "Race assist") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "public_assist")) %>%
    race_pull()
  
  race_assist_byrace <- race_assist %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "public_assist")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe") 
  
  data_saver(race_assist_byrace, place, "race_assist_byrace", desc_year = desc_year)
  
  # foreign born race
  race_foreign_born <- data_puller(root_string = root_string, "Race by foreign born") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "birthplace")) %>%
    race_pull() 
  
  race_foreign_born_byrace <- race_foreign_born %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "birthplace")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
  
  data_saver(race_foreign_born_byrace, place, "race_foreign_born_byrace", desc_year = desc_year)
  
  # race by disability
  race_disability_age <- data_puller(root_string = root_string, "Race disability") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "age", "disability")) %>%
    race_pull()
  
  race_disability <- race_disability_age %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "age")) %>%
    filter(!is.na(disability)) %>%
    est_moe_derive(group_cols = c("name", "race", "disability")) %>%
    select(-c(variable, estimate, moe, label, age, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_race_disability",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe",
                       component_est = "name_race_disability_est",
                       component_moe = "name_race_disability_moe") 
  
  data_saver(race_disability, place, "race_disability", desc_year = desc_year)
  
  # race internet 
  race_internet <- data_puller(root_string = root_string, "Race Internet") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "computer_access", "internet_access")) %>%
    race_pull()
  
  race_computer <- race_internet %>%  
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    filter(!is.na(computer_access) & is.na(internet_access)) %>%
    select(-c(variable, label, internet_access, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_race_computer",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe") 
  
  data_saver(race_computer, place, "race_computer", desc_year = desc_year)
  
  
  # race internet
  race_internet_processed <- race_internet %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    mutate(internet_access = ifelse(computer_access == "No Computer", computer_access, internet_access)) %>%
    filter(!is.na(internet_access)) %>%
    derive_pct_est_moe(proportion_col = "pct_internet",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe") 
  
  data_saver(race_internet_processed, place, "race_internet_processed", desc_year = desc_year)
  
  # race tenure
  # race_tenure <- data_puller(root_string = root_string, "Race Tenure") %>%
  #   puller_funct(place_string = place, no_pull = no_pull) %>%
  #   separate_label(c(NA, NA, "tenure")) %>%
  #   race_pull()
  
  
  # race transport
  race_transport <- data_puller(root_string = root_string, "Race transport") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "transport_to_work")) %>%
    race_pull()
  
  race_transport_processed <- race_transport %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "transport_to_work")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe") 
  
  data_saver(race_transport_processed, place, "race_transport_processed", desc_year = desc_year)
  
  # race ethnicity
  race_ethn_processed <- data_puller(root_string = root_string, "Race_ethnicity") %>%
    race_ethn_processor(place = place, no_pull = no_pull)

  
  data_saver(race_ethn_processed, place, "race_ethn_processed", desc_year = desc_year)
  
  # rent burden owners
  burden_owners_mortgage <- data_puller(root_string = root_string, "Rent burden - owners") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "mortgage_status", "percent_household_income"))
  
  burden_owners_overall <- burden_owners_mortgage %>%
    burden_owners_process
  
  data_saver(burden_owners_overall, place, "burden_owners_overall", desc_year = desc_year)
  
  burden_owners_bymortgage <- burden_owners_mortgage %>%
    burden_owners_mortgage_process
  
  
  data_saver(burden_owners_bymortgage, place, "burden_owners_bymortgage", desc_year = desc_year)
  
  # rent burden renters 
  burden_renters <- data_puller(root_string = root_string, "Rent burden - renters") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "percent_income")) 
  
  burden_renters_processed <- burden_renters %>%
    total_col_add(list("total_renters"="percent_income"))  %>%
    derive_pct_est_moe(proportion_col = "pct_renters",
                       aggregate_est = "total_renters",
                       aggregate_moe = "total_renters_moe") %>%
    mutate(cumul_pct = cumsum(pct_renters))
  
  data_saver(burden_renters_processed, place, "burden_renters_processed", desc_year = desc_year)
  
  # overall dataset - race, gender, employment satus, age - contains detailed and non-detailed age-categories
  race_employment_age_gender <- data_puller(root_string = root_string, "Unemployment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "gender", "age_range", "labor_force_status", "type_labor_force", "employment_status")) %>%
    race_pull()
  
  # non-detailed results
  race_employment_age_gender_general <- race_employment_age_gender %>%
    filter(grepl("C23002", variable))
  
  employment_age_gender_detailed <- race_employment_age_gender %>%
    filter(!grepl("C23002", variable))
  
  # unemployment overall by race
  unique(race_employment_age_gender_general$type_labor_force)
  
  # caclulate total values
  race_employment_age_gender_general_pre_process <- race_employment_age_gender_general  %>%
    filter(!grepl("sex by age", race) & grepl("In labor force", labor_force_status) & !is.na(type_labor_force)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           # employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force)) 
  
  # browser()
  # calculate totals for race/gender - weird grouping because of different age classifications
  total_df <- race_employment_age_gender_general_pre_process %>%
    filter(is.na(employment_status) | grepl("65 years", age_range)) %>%
    est_moe_derive(group_cols = c("name","race", "gender"), name_col = "race_gender_laborforce") %>%
    select(name, race, gender, race_gender_laborforce_est, race_gender_laborforce_moe) %>%
    distinct()
  
  ## fix so armged forces shows up
  race_gender_employment_general <- race_employment_age_gender_general_pre_process %>%
    left_join(total_df) %>%
    # calculate total across age groups in labor force
    filter(!is.na(employment_status) | grepl("Armed Forces", type_labor_force)) %>%
    mutate(employment_status= ifelse(is.na(employment_status), type_labor_force, employment_status)) %>%
    est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_gender_laborforce_est",
                       aggregate_moe = "race_gender_laborforce_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    distinct()
  
  data_saver(race_gender_employment_general, place, "race_gender_employment_general", desc_year = desc_year)
  
  #### race employment - under 65 only
  race_employment_gender_u65 <- race_employment_age_gender_general %>%
    race_employ_gender_u65_process
  
  data_saver(race_employment_gender_u65, place, "race_employment_gender_u65", desc_year = desc_year)
  
  #### race employment - no gender, u65 only
  race_employment_u65_preprocess <- race_employment_age_gender_general  %>%
    filter(!grepl("sex by age", race) & 
             grepl("In labor force", labor_force_status) & 
             grepl("16 to 64", age_range) & 
             !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status))
  
  total_df <- race_employment_u65_preprocess %>%
    filter(is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "race"), name_col = "total_workers") %>%
    select(name, race, total_workers_est, total_workers_moe) %>%
    distinct()
  
  race_employment_u65 <- race_employment_u65_preprocess %>%
    race_employment_u65_process(tot_df = total_df)
  
  data_saver(race_employment_u65, place, "race_employment_u65", desc_year = desc_year)
  
  # race gender employment age - detailed categories in labor force
  employment_age_gender <- employment_age_gender_detailed %>%
    employment_age_gender_process
  
  data_saver(employment_age_gender, place, "employment_age_gender", desc_year = desc_year)
  
  # employment age
  employment_age_preprocess <- employment_age_gender_detailed %>%
    filter(grepl("In labor force", labor_force_status)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force))
  
  total_df <- employment_age_preprocess %>%
    filter(is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "age_range"), name_col = "total_workers") %>%
    select(name, age_range, total_workers_est, total_workers_moe) %>%
    distinct()
  
  employment_age <- employment_age_preprocess %>%
    filter(!is.na(type_labor_force) & !is.na(employment_status)) %>%
    left_join(total_df) %>%
    est_moe_derive(group_cols = c("name", "age_range", "type_labor_force", "employment_status"), 
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_age",
                       aggregate_est = "total_workers_est",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, estimate, moe, label, concept, race, gender)) %>%
    distinct()
  
  data_saver(employment_age, place, "employment_age", desc_year = desc_year)
  
  
  # race/gender/age - labor force participation
  race_laborforce_gender_u65 <- race_employment_age_gender_general %>%
    filter(grepl("16 to 64", age_range) & is.na(type_labor_force)) %>%
    total_col_add(list("total_persons" = "labor_force_status"), c("name", "race", "gender")) %>%
    est_moe_derive(c("name", "race", "gender", "labor_force_status"), name_col = "persons") %>%
    derive_pct_est_moe(proportion_col = "pct_race_gender",
                       aggregate_est = "total_persons",
                       aggregate_moe = "total_persons_moe",
                       component_est = "persons_est",
                       component_moe = "persons_moe") %>%
    select(-c(variable, estimate, moe, label, concept, age_range, type_labor_force, employment_status)) %>%
    distinct()
  
  data_saver(race_laborforce_gender_u65, place,  "race_laborforce_gender_u65", desc_year = desc_year)
  
  # owner housing costs by income
  household_income_ownercosts <- data_puller(root_string = root_string, "Household income by owner housing costs") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "household_income", "housing_costs"))
  
  household_income_ownercosts_processed <- process_household_income_ownercosts(household_income_ownercosts)
  
  data_saver(household_income_ownercosts_processed, place, "household_income_ownercosts_processed", desc_year = desc_year)
  
  # number owner income
  owner_income_num <- household_income_ownercosts %>% 
    filter(is.na(housing_costs)) %>%
    total_col_add(list("num_homeowners" = "household_income")) %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners",
                       "num_homeowners",
                       "num_homeowners_moe")
  
  data_saver(owner_income_num, place, "owner_income_num", desc_year = desc_year)
  
  # renter housing costs by income
  household_income_rentercosts <- data_puller(root_string = root_string, "Household income by renter housing costs") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "household_income", "housing_costs"))
  
  household_income_rentercosts_processed <- process_household_income_ownercosts(household_income_rentercosts)
  
  data_saver(household_income_rentercosts_processed, place, "household_income_rentercosts_processed", desc_year = desc_year)
  
  # number renters by income
  renter_income_num <- household_income_rentercosts %>% 
    filter(is.na(housing_costs)) %>%
    total_col_add(list("num_renters" = "household_income")) %>%
    derive_pct_est_moe(proportion_col = "pct_renters",
                       "num_renters",
                       "num_renters_moe")
  
  data_saver(renter_income_num, place, "renter_income_num", desc_year = desc_year)
  
  # disability
  disability <- data_puller(root_string = root_string, "Disability overall") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_disability_overall
  
  data_saver(disability, place, "disability_overall_processed", desc_year = desc_year)
    
  health_age <- data_puller(root_string = root_string, "Health insurance by age") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_health_age
  
  data_saver(health_age, place, "health_age_processed", desc_year = desc_year)
  
  gender <- data_puller(root_string = root_string, "Gender") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_gender
  
  data_saver(gender, place, "gender", desc_year = desc_year)
  
  age_gender <- data_puller(root_string = root_string, "Age by gender") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_age_gender
  
  data_saver(age_gender, place, "age_gender", desc_year = desc_year)
  
  race_age_gender <- data_puller(root_string = root_string, "Race age gender") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_race_age_gender
  
  data_saver(race_age_gender, place, "race_age_gender", desc_year = desc_year)
  
  computer_age <- data_puller(root_string = root_string, "Computer by age") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_computer_age
  
  data_saver(computer_age, place, "computer_age", desc_year = desc_year)
  
  poverty_race_age <- data_puller(root_string = root_string, "Poverty by race and age") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_poverty_race_age
  
  data_saver(poverty_race_age, place, "poverty_race_age", desc_year = desc_year)
  
  ancestry_sub <- data_puller(root_string = root_string, "Ancestry") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_ancestry(T)
  
  data_saver(ancestry_sub, place, "ancestry_sub", desc_year = desc_year)
  
  ancestry_overall <- data_puller(root_string = root_string, "Ancestry") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_ancestry(F)
  
  data_saver(ancestry_overall, place, "ancestry_overall", desc_year = desc_year)
  
  asian_disag <- data_puller(root_string = root_string, "Asian disaggregated") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_asian_disaggregated()
  
  data_saver(asian_disag, place, "asian_disag", desc_year = desc_year)
  
  poverty_detail <- data_puller(root_string = root_string, "Poverty status detailed") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_poverty_detail()
  
  data_saver(poverty_detail, place, "poverty_detail", desc_year = desc_year)
  
  # health insurance by race and age
  health_race <- data_puller(root_string = root_string, "Health insurance by race and age") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_health_race
  
  data_saver(health_race, place, "health_race_age", desc_year = desc_year)
  
  tenure_age <- data_puller(root_string = root_string, "Tenure by age") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_tenure_age()
  
  data_saver(tenure_age, place, "tenure_age", desc_year = desc_year)
  
  
  age_lang <- data_puller(root_string = root_string, "Language by age") %>%
    puller_funct(place, no_pull = no_pull) %>%
    process_age_lang()
  
  data_saver(age_lang, place, "lang_age", desc_year = desc_year)
  
  # disability by type of disability
  disab_root <- data_puller(root_string = root_string, "Disability by age and type") %>%
    puller_funct(place_string = place, no_pull = no_pull)
  
  # process population data and age/disability data because can have multiple disabilities within age groups
  pop_tot <- process_pop_tot(race_ethn_processed)
  
  disab_overall <- process_disability_pop(disability)
  
  age_disab_overall <- process_disability_age(disability)
  
  disab_sex_overall <- process_sex_disab_overall(disability)
  
  disab_type_root <- disab_root %>%
    read_disab_type(pop_tot_df = pop_tot)
  
  disab_type_pop <- process_disab_type(disab_type_root, df_disab_pop = disab_overall)
  
  data_saver(disab_type_pop, place, "disab_type_pop", desc_year = desc_year)

  disab_type_age <- process_disab_age(disab_type_root, disab_age_df = age_disab_overall)
  
  data_saver(disab_type_age, place, "disab_type_age", desc_year = desc_year)
  
  disab_sex_type <- process_disab_sex(disab_type_root, disab_sex_overall)
  
  data_saver(disab_sex_type, place, "disab_sex_type", desc_year = desc_year)
  
  poverty_sex <- data_puller(root_string = root_string, "Poverty by sex") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_poverty_sex()
  
  data_saver(poverty_sex, place, "poverty_sex", desc_year = desc_year)
  
  poverty_sex_race <- data_puller(root_string = root_string, "Poverty by sex and race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_poverty_race_sex()
  
  data_saver(poverty_sex_race, place, "poverty_sex_race", desc_year = desc_year)
  
  poverty_sex_age <- data_puller(root_string = root_string, "Poverty by sex") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_poverty_sex_age()
  
  data_saver(poverty_sex_age, place, "poverty_age_sex", desc_year = desc_year)
  
  
}

# save all
data_creator(root_string = "acs52019/all_municip_acs5_", place = "mdplaces")

data_creator("acs52019/maryland2019_acs5_", place = "Maryland", no_pull = TRUE)

data_creator("acs52019/montcounty2019_acs5_", place = "mdcounties")

# read in/save
place_dir <- grep("mdplaces", dir("./data/output_data/acs5_2019/all"), value = T)

county_dir <- grep("mdcounties", dir("./data/output_data/acs5_2019/all"), value = T)

state_dir <- grep("Maryland", dir("./data/output_data/acs5_2019/all"), value = T)

# output tp
place_vector <- read_rds("./data/output_data/acs5_2019/all/burden_owners_bymortgage_mdplaces.rds") %>%
  pull(name) %>%
  unique()

county_vector <- read_rds("./data/output_data/acs5_2019/all/burden_owners_bymortgage_mdcounties.rds") %>%
  pull(name) %>%
  unique()

places <- "Takoma Park"

county_vector <- "(Montgomery County)"

walk(place_dir, ~ read_rds(paste0("./data/output_data/acs5_2019/all/", .x)) %>%
       filter(grepl(places, name)) %>%
       write_rds(file = paste0("./data/output_data/acs5_2019/tp_spec/", 
                 gsub("mdplaces", "tp", .x))))

walk(county_dir, ~ read_rds(paste0("./data/output_data/acs5_2019/all/", .x)) %>%
       filter(grepl(county_vector, name)) %>%
       write_rds(file = paste0("./data/output_data/acs5_2019/tp_spec/", 
                 gsub("mdcounties", "montpg", .x))))

walk(state_dir, ~ read_rds(paste0("./data/output_data/acs5_2019/all/", .x)) %>%
       write_rds(file = paste0("./data/output_data/acs5_2019/tp_spec/", 
                               .x)))

