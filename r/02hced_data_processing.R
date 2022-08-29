library(tidycensus)
library(dplyr)
library(tidyverse)
# library(conflicted)

library(tpfuncts)
library(acsprocess)

# conflict_prefer("filter", "dplyr")

library(sf)
library(leaflet)
library(htmltools)
# place_view <- st_read("./data/nhgis/usplaces_2019/US_place_2019.shp")

# md_place <- place_view %>%
#   filter(grepl(24, STATEFP)) %>%
#   st_transform(4326)
# 
# leaflet(md_place) %>%
#   addTiles %>%
#   addPolygons(label = map(md_place$NAME, ~ HTML(text = paste0("Place: ", .x))))

variables <- tidycensus::load_variables(2020, "acs5", cache = TRUE)


# variables <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

variables_2018 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)

# total_col_add <- function(df,
#                           total_cols,
#                           join_col = "name",
#                           est_col = "estimate") {
#   
#   return_df <- df %>%
#     dplyr::rename_all(tolower)
#   
#   total_cols_vec <- total_cols
#   
#   total_cols_name <- names(total_cols)
#   
#   # loop through list of total columns to pull
#   purrr::walk(seq_along(total_cols_vec), ~ {
#     
#     # browser()
#     
#     select_tot_col <- total_cols_vec[[.x]]
#     select_cols_name <- total_cols_name[[.x]]
#     
#     if (length(join_col) > 1 & length(total_cols) > 1){
#       # subset join columns
#       join_col <-join_col[1:.x]
#     }
#     
#     # create name of moe for total
#     group_moe <- paste0(select_cols_name, "_moe")
#     
#     # filter to total
#     total_df <- return_df %>%
#       dplyr::filter(is.na(!!sym(select_tot_col)))
#     
#     # update moe to 0 if missing - per this discussion - https://github.com/walkerke/tidycensus/issues/29
#     
#     # pull total value and moe for that (na value of subgroup column = total for that group
#     total_df <- total_df %>%
#       dplyr::mutate(!!sym(select_cols_name) := !!sym(est_col),
#                     !!sym(group_moe) := ifelse(is.na(moe), 0, moe))
#     
#     # filter to total column and its moe as well as column to rejoin data by
#     total_df <- total_df %>%
#       dplyr::select(c(join_col, select_cols_name, group_moe))
#     
#     # join total data to df and filter out total-row
#     return_df <<- return_df %>%
#       left_join(total_df) %>%
#       dplyr::filter(!is.na(!!sym(select_tot_col)))
#     
#   })
#   
#   group_vec <- c(join_col, unname(unlist(total_cols, use.names = F)), "variable", est_col)
#   
#   df_check <- return_df %>%
#     dplyr::group_by(dplyr::across(group_vec)) %>%
#     dplyr::mutate(freq = n())
#   
#   if (any(df_check %>% pull(freq) > 1) | nrow(df_check) > nrow(df)){
#     browser()
#     
#     stop("Observations that are supposed to be unique repeat; check whether join columns and total columns align")
#   }
#   
#   return(return_df)
# }
# 


# health_root <- data_puller("acs52019/all_municip_acs5_", "Health insurance by age") %>%
#   puller_funct("Takoma", T)%>%
#   separate_label(c(NA, NA, "age", "num_plans", "type_coverage")) %>%
#   process_health_age()
# 
#   

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

# disability overall over pop

# disability over age

# disability by type out of pop

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

# disability by type out of age

# df <- data_puller("acs52019/all_municip_acs5_", "Poverty by sex") %>%
#   puller_funct("Takoma", no_pull = T)


# df <- data_puller("acs52019/all_municip_acs5_", "Poverty by sex and race") %>%
#   puller_funct("Takoma", no_pull = T)


# df <- data_puller("acs52019/all_municip_acs5_", "Family overall") %>%
#   puller_funct("Takoma", no_pull = T)
# 
# 
# df <- data_puller("acs52019/all_municip_acs5_", "Family by poverty") %>%
#   puller_funct("Takoma", no_pull = T)
# 
# 
# # df <- data_puller("acs52019/all_municip_acs5_", "Family by poverty and race") %>%
# #   puller_funct("Takoma", no_pull = T)
# 
# 
# # df <- data_puller("acs52019/all_municip_acs5_", "Family type and income") %>%
# #   puller_funct("Takoma", no_pull = T)
# 
# 
# # df <- data_puller("acs52019/all_municip_acs5_", "Families by tenure") %>%
# #   puller_funct("Takoma", no_pull = T)
# # 
# # df <- data_puller("acs52019/all_municip_acs5_", "Families by poverty and race") %>%
# #   puller_funct("Takoma", no_pull = T)

# family_poverty <- data_puller(root_string = "acs52019/all_municip_acs5_", "Family by poverty") %>%
#   puller_funct("") %>%
#   process_family_poverty


# process_family_poverty <- function(df){
#   process <- df %>%
#     acsprocess::separate_label(c(NA, NA, "pov_status", "fam_type", "fam_occupants", "occupants_age", "other_fam_extra"))
#   
#   other_process <- process %>%
#     dplyr::filter(grepl("Other family", fam_type)) %>%
#     dplyr::filter(!is.na(fam_occupants)) %>%
#     dplyr::mutate(fam_type = fam_occupants,
#                   fam_occupants = occupants_age,
#                   occupants_age = other_fam_extra) %>%
#     dplyr::select(-other_fam_extra)
#   
#   process_final <- process %>%
#     dplyr::filter(!grepl("Other family", fam_type)) %>%
#     dplyr::select(-other_fam_extra) %>%
#     rbind(other_process) %>%
#     dplyr::mutate(occupants_age = ifelse(grepl("No related", fam_occupants), "No children", occupants_age)) %>%
#     acsprocess::total_col_add(c("households" = "pov_status", "pov_status_tot" = "fam_type", "pov_type_tot" = "fam_occupants", "pov_type_occ_tot" = "occupants_age"), join_col = c("name", "pov_status", "fam_type", "fam_occupants"))
# }

process_hous_famtyp <- function(df){
  process <- df %>%
    dplyr::filter(grepl("HOUSEHOLD TYPE \\(INCLUDING LIVING ALONE\\)$", concept)) %>%
    acsprocess::separate_label(c(NA, NA, "houstype", "famtype", "othertype"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", famtype)) %>%
    dplyr::mutate(famtype = othertype) %>%
    dplyr::filter(!is.na(famtype)) %>%
    dplyr::select(-othertype)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", famtype)) %>%
    dplyr::select(-othertype) %>%
    rbind(other_process) %>%
    # filter(grepl("Takoma", name)) %>%
    acsprocess::total_col_add(c("households" = "houstype", "type_households" = "famtype"), join_col = c("name", "houstype"))

  process_final
}

# df <- data_puller(root_string = "acs52019/all_municip_acs5_", data_string = "Households by family type and age and poverty") %>%
#   puller_funct(place_string = "Takoma", no_pull = T)

process_hous_povage <- function(df){
  process <- df %>%
    acsprocess::separate_label(c(NA, NA, "povlev", "famtype", "householder", "age", "otherextra"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", householder)) %>%
    dplyr::mutate(householder = age,
                  age = otherextra) %>%
    dplyr::filter(!is.na(householder)) %>%
    dplyr::select(-otherextra)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", householder)) %>%
    dplyr::select(-otherextra) %>%
    rbind(other_process) %>%
    # filter(grepl("Takoma", name)) %>%
    acsprocess::total_col_add(c("households" = "povlev", "pov" = "famtype", "povfam" = "householder", "povfamhous" = "age"), join_col = c("name", "povlev", "famtype", "householder"))

  process_final
}

process_famtype_incomelev <- function(df){

  process <- df %>%
    acsprocess::separate_label(names_vector = c(NA, NA, "famtype", "childstatus", "income", "other"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", famtype)) %>%
    dplyr::mutate(famtype = childstatus,
                  childstatus = income,
                  income = other) %>%
    dplyr::filter(!is.na(famtype)) %>%
    dplyr::select(-other)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", famtype)) %>%
    dplyr::select(-other) %>%
    rbind(other_process) %>%
    # filter(grepl("Takoma", name)) %>%
    acsprocess::total_col_add(c("families" = "famtype", "type_fams" = "childstatus", "fam_child" = "income"), join_col = c("name", "famtype", "childstatus"))


}

# famtype_incomepov <- data_puller("acs52019/all_municip_acs5_", "Family type by income to poverty ratio") %>%
#   puller_funct()
# 
# process <- df %>%
#   acsprocess::separate_label(names_vector = c(NA, NA, "famtype", "childstatus", "income", "other"))
# 
# other_process <- process %>%
#   dplyr::filter(grepl("Other family", famtype)) %>%
#   dplyr::mutate(famtype = childstatus,
#                 childstatus = income,
#                 income = other) %>%
#   dplyr::filter(!is.na(famtype)) %>%
#   dplyr::select(-other)
# 
# process_final <- process %>%
#   dplyr::filter(!grepl("Other family", famtype)) %>%
#   dplyr::select(-other) %>%
#   rbind(other_process) %>%
#   # filter(grepl("Takoma", name)) %>%
#   acsprocess::total_col_add(c("families" = "famtype", "type_fams" = "childstatus", "fam_child" = "income"), join_col = c("name", "famtype", "childstatus"))
# 
# 
# process_famtype_incomepov <- function(df){
#   
# }

# process_costburden_owner_age

# 
# process_own_costburden_age <- function(df){
#   df %>%
#     separate_label(names_vector = c(NA, NA, "age", "cost_burden")) %>%
#     acsprocess::total_col_add(total_cols = c("households" = "age", "hous_age" = "cost_burden"), join_col = c("name", "age")) %>%
#     dplyr::mutate(tenure = "Owner")
# }
# 
# process_rent_costburden_age <- function(df){
#   
#   df %>%
#     acsprocess::separate_label(names_vector = c(NA, NA, "age", "cost_burden")) %>%
#     acsprocess::total_col_add(total_cols = c("households" = "age", "hous_age" = "cost_burden"), join_col = c("name", "age")) %>%
#     dplyr::mutate(tenure = "Renter")
#     
# }
# 
# process_rent_own_costburden_age <- function(rent_df, own_df){
#   
#   rent <- rent_df %>%
#     process_rent_costburden_age
#   
#   own <- own_df %>%
#     process_own_costburden_age
#   
#   join <- rbind(rent, own) %>%
#     est_moe_derive(group_cols = c("name"), name_col = "tot_households")
#   
# }
# 
# df_own <- data_puller(root_string = "acs52019/all_municip_acs5_", "Renter cost burden by age") %>%
#   puller_funct(place_string = "Takoma", no_pull = T)
# 
# df_rent <- data_puller(root_string = "acs52019/all_municip_acs5_", "Renter cost burden by age") %>%
#   puller_funct(place_string = "Takoma", no_pull = T)

process_pull_save <- function(datapth, savefile, processfunct, rs, plc, nop, yr){
  # browser()
  
  
  
  data <- data_puller(root_string = rs, data_string = datapth) %>%
    puller_funct(place_string = plc, no_pull = nop) %>%
    processfunct() 
  
    data_saver(object = data, place_string = plc, data_string = savefile, desc_year = yr)
  
}
# 
# df_age_tenure <- data_puller(root_string = "acs52019/all_municip_acs5_", "Age by tenure by household type") %>%
#   puller_funct(place_string = "Takoma", no_pull = T) %>%
#   separate_label(c(NA, NA, "tenure", "fam_type", "livalone", "houseage", "other_fam_extra"))
# 
# other_process <- df_age_tenure %>%
#   dplyr::filter(grepl("Other", livalone)) %>%
#   # exclude totals
#   # dplyr::filter(!is.na(houseage)) %>%
#   dplyr::mutate(fam_type = livalone,
#                 livalone = houseage,
#                 houseage = other_fam_extra) %>%
#   dplyr::select(-other_fam_extra)
# 
# process_final <- df_age_tenure %>%
#   dplyr::filter(!grepl("Other", livalone)) %>%
#   dplyr::select(-other_fam_extra) %>%
#   rbind(other_process) %>%
#   total_col_add(list("households" = "tenure", "tenuretot" = "fam_type", "famtot" = "livalone", "occ" = "houseage"), join_col = c("name", "tenure", "fam_type", "livalone"))



# df_medage_sex <- data_puller(root_string = "acs52019/all_municip_acs5_", "Median age by sex") %>%
#   puller_funct(place_string = "Takoma", no_pull = T) %>%
#   separate_label(c(NA, NA, "sex"))



# df_vetstatus_agesex <- data_puller(root_string = "acs52019/all_municip_acs5_", "Veteran status by age and sex") %>%
#   acsprocess::puller_funct(place_string = "Takoma", no_pull = T) %>%
#   acsprocess::separate_label(c(NA, NA, "sex", "age", "veteran")) 

# base_process_vetagesex <- function(df){
#   df %>%
#     acsprocess::separate_label(c(NA, NA, "sex", "age", "veteran"))
# }

# process_vetage <- function(df){
#   df %>%
#     base_process_vetagesex() %>%
#     dplyr::filter(!grepl("veteran", sex, ignore.case = T) & !grepl("veteran", age, ignore.case = T)) %>%
#     acsprocess::total_col_add(list("poptot" = "sex", "sextot" = "age", "agetot" = "veteran"), join_col = c("name", "sex", "age")) %>%
#     acsprocess::process_df(group_cols = c("name", "age", "veteran"), overall_cols = c("name", "veteran"), name_col = "agevet", bind_overall = "age") %>%
#     acsprocess::age_recode(age)
# }


#### build datasets
## function to build and save all datasets of interest
data_creator <- function(root_string, place, no_pull = T, desc_year = "acs5_2020"){
  prc_pull_save <- function(datapth, savefile, processfunct){
    process_pull_save(datapth = datapth, savefile = savefile, processfunct = processfunct, rs = root_string, plc = place, nop = no_pull, yr = desc_year)
  }

  # computer use
  comp_race <- data_puller(root_string, "Computer overall and by race") %>%
    rename_all(tolower) %>%
    puller_funct(place = place, no_pull = no_pull) %>%
    process_computer_overall()

  data_saver(comp_race,
             place,
             "comp_race",
             desc_year = desc_year)

  # overall educational attainment
  educ_overall <- data_puller(root_string, "Education overall") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_educ_overall()

  data_saver(educ_overall,
             place,
             "educ_overall",
             desc_year = desc_year)

  # household income by race
  race_income_household <- data_puller(root_string, "Household income by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_income_race()

  data_saver(race_income_household,
             place,
             "race_income_household",
             desc_year = desc_year)

  # median income by race
  race_income_med <- data_puller(root_string, "Median income by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_income_race_median()

  data_saver(race_income_med,
             place,
             "race_income_med",
             desc_year = desc_year)


  educ_race_df <- data_puller(root_string, "Educational attainment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_educ_race()

  ## educ attainment by race
  educ_race_df_overall <- data_puller(root_string, "Educational attainment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull)  %>%
    educ_race_process()

  data_saver(educ_race_df_overall,
             place,
             "educ_race_df_overall",
             desc_year = desc_year)

  # education/race/gender
  educ_race_gender_df <- data_puller(root_string, "Educational attainment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_educ_race_gender()

  data_saver(educ_race_df_overall,
             place,
             "educ_race_gender_df",
             desc_year = desc_year)

  # foreign born by birth
  foreign_born_birth_processed <- data_puller(root_string = root_string, "Foreign born place of birth") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_foreign_born_birth()

  data_saver(foreign_born_birth_processed,
             place,
             "foreign_born_birth_processed",
             desc_year = desc_year)

  # foreign born overall
  foreign_born_processed <- data_puller(root_string = root_string,
                              "Foreign born") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_foreign_born()

  data_saver(foreign_born_processed,
             place,
             "foreign_born_processed",
             desc_year = desc_year)

  base_race_home_own <- data_puller(root_string = root_string,
                                    "Home ownership by race") %>%
    puller_funct(place_string = place, no_pull = no_pull)

  # home ownership by race
  race_home_own <- base_race_home_own %>%
    process_race_home_own()

  ## race as % of renter/owner
  race_home_own_race_tenure <- base_race_home_own %>%
    process_race_home_own_race_tenure()

  data_saver(race_home_own_race_tenure,
             place,
             "race_tenure",
             desc_year = desc_year)

  ## renter/owner as % of race
  tenure_race <- base_race_home_own %>%
    process_tenure_race()

  data_saver(tenure_race,
             place,
             "tenure_race",
             desc_year = desc_year)

  ## housing tenure
  tenure_df <- data_puller(root_string = root_string, "Housing tenure") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_tenure_df()

  data_saver(tenure_df, place, "tenure_df", desc_year = desc_year)

  # income by tenure
  tenure_income_median <- data_puller(root_string = root_string, "Income by tenure") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_tenure_income_median()

  data_saver(tenure_income_median, place, "tenure_income_median", desc_year = desc_year)

  # income last twelve months
  income_last12_processed <- data_puller(root_string = root_string, "Income last twelve months") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_income_last12

  data_saver(income_last12_processed, place, "income_last12", desc_year = desc_year)

  # language at home
  home_lang <- data_puller(root_string = root_string, "Language at home") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_home_lang()

  data_saver(home_lang, place, "home_lang", desc_year = desc_year)

  # poverty
  poverty <- data_puller(root_string = root_string, "Poverty") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_poverty()

  data_saver(poverty, place,  "poverty", desc_year = desc_year)

  # assistance by race
  race_assist_byrace <- data_puller(root_string = root_string, "Race assist") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_assist()

  data_saver(race_assist_byrace, place, "race_assist_byrace", desc_year = desc_year)

  # foreign born race
  race_foreign_born_byrace <- data_puller(root_string = root_string, "Race by foreign born") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_foreign_born()

  data_saver(race_foreign_born_byrace, place, "race_foreign_born_byrace", desc_year = desc_year)

  # race by disability
  race_disability <- data_puller(root_string = root_string, "Race disability") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_disability()

  data_saver(race_disability, place, "race_disability", desc_year = desc_year)

  race_internet_base <- data_puller(root_string = root_string, "Race Internet") %>%
    puller_funct(place_string = place, no_pull = no_pull)

  # race internet
  race_internet <- race_internet_base %>%
    process_race_internet_base()

  race_computer <- race_internet_base %>%
    process_race_computer()

  data_saver(race_computer, place, "race_computer", desc_year = desc_year)

  # race internet
  race_internet_processed <- race_internet_base %>%
    process_race_internet_complete()

  data_saver(race_internet_processed, place, "race_internet_processed", desc_year = desc_year)

  # race tenure
  # race_tenure <- data_puller(root_string = root_string, "Race Tenure") %>%
  #   puller_funct(place_string = place, no_pull = no_pull) %>%
  #   separate_label(c(NA, NA, "tenure")) %>%
  #   race_pull()


  # race transport
  race_transport <- data_puller(root_string = root_string, "Race transport") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_transport_base()

  race_transport_processed <- data_puller(root_string = root_string, "Race transport") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_transport_complete()

  data_saver(race_transport_processed, place, "race_transport_processed", desc_year = desc_year)

  # race ethnicity
  race_ethn_processed <- data_puller(root_string = root_string, "Race_ethnicity") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_race_ethn()

  data_saver(race_ethn_processed, place, "race_ethn_processed", desc_year = desc_year)

  # rent burden owners
  burden_owners_mortgage <- data_puller(root_string = root_string, "Rent burden - owners") %>%
    puller_funct(place_string = place, no_pull = no_pull)

  burden_owners_overall <- burden_owners_mortgage %>%
    process_burden_owners()

  data_saver(burden_owners_overall, place, "burden_owners_overall", desc_year = desc_year)

  burden_owners_bymortgage <- burden_owners_mortgage %>%
    process_burden_owners_mortgage()

  data_saver(burden_owners_bymortgage, place, "burden_owners_bymortgage", desc_year = desc_year)

  # rent burden renters
  burden_renters <- data_puller(root_string = root_string, "Rent burden - renters") %>%
    puller_funct(place_string = place, no_pull = no_pull)

  burden_renters_processed <- burden_renters %>%
    process_burden_renters()

  data_saver(burden_renters_processed, place, "burden_renters_processed", desc_year = desc_year)

  # overall dataset - race, gender, employment satus, age - contains detailed and non-detailed age-categories

  race_employment_age_gender_base <- data_puller(root_string = root_string, "Unemployment by race") %>%
    puller_funct(place_string = place, no_pull = no_pull)

  # race_employment_age_gender <- race_employment_age_gender_base %>%
  #   process_race_employment_age_gender_base()

  # non-detailed results
  # race_employment_age_gender_general <- race_employment_age_gender %>%
  #   filter(grepl("C23002", variable))

  # employment_age_gender_detailed <- race_employment_age_gender %>%
  #   filter(!grepl("C23002", variable))

  # unemployment overall by race
  # unique(race_employment_age_gender_general$type_labor_force)

  ## fix so armged forces shows up
  race_gender_employment_general <- race_employment_age_gender_base %>%
    process_race_gender_employment_general()

  data_saver(race_gender_employment_general, place, "race_gender_employment_general", desc_year = desc_year)

  #### race employment - under 65 only
  race_employment_gender_u65 <- race_employment_age_gender_base %>%
    process_race_employ_gender_u65()

  data_saver(race_employment_gender_u65, place, "race_employment_gender_u65", desc_year = desc_year)

  race_employment_u65 <- race_employment_age_gender_base %>%
    process_race_employment_u65()

  data_saver(race_employment_u65, place, "race_employment_u65", desc_year = desc_year)

  # race gender employment age - detailed categories in labor force
  employment_age_gender <- race_employment_age_gender_base %>%
    process_employment_age_gender()

  data_saver(employment_age_gender, place, "employment_age_gender", desc_year = desc_year)

  # employment age
  employment_age <- race_employment_age_gender_base %>%
    process_employment_age()

  data_saver(employment_age, place, "employment_age", desc_year = desc_year)

  # race/gender/age - labor force participation
  race_laborforce_gender_u65 <- race_employment_age_gender_base %>%
    process_race_laborforce_gender_u65()

  data_saver(race_laborforce_gender_u65, place,  "race_laborforce_gender_u65", desc_year = desc_year)

  # owner housing costs by income
  household_income_ownercosts <- data_puller(root_string = root_string, "Household income by owner housing costs") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "household_income", "housing_costs"))

  household_income_ownercosts_processed <- process_household_income_ownercosts(household_income_ownercosts)

  data_saver(household_income_ownercosts_processed, place, "household_income_ownercosts_processed", desc_year = desc_year)

  # number owner income
  owner_income_num <- household_income_ownercosts %>%
    process_owner_income_num()

  data_saver(owner_income_num, place, "owner_income_num", desc_year = desc_year)

  # renter housing costs by income
  household_income_rentercosts <- data_puller(root_string = root_string, "Household income by renter housing costs") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    separate_label(c(NA, NA, "household_income", "housing_costs"))

  household_income_rentercosts_processed <- process_household_income_ownercosts(household_income_rentercosts)

  data_saver(household_income_rentercosts_processed, place, "household_income_rentercosts_processed", desc_year = desc_year)

  # number renters by income
  renter_income_num <- household_income_rentercosts %>%
    process_renter_income_num()

  data_saver(renter_income_num, place, "renter_income_num", desc_year = desc_year)

  # disability
  disability <- data_puller(root_string = root_string, "Disability overall") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_disability_overall()

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
    process_read_disab_type(pop_tot_df = pop_tot)

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

  ### family analyses
  family_overall <- data_puller(root_string = root_string, "Family overall") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_family()

  data_saver(family_overall, place, "family_overall", desc_year = desc_year)

  family_poverty <- data_puller(root_string = root_string, "Family by poverty") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_family_poverty()

  data_saver(family_poverty, place, "family_poverty", desc_year = desc_year)

  family_poverty_race <- data_puller(root_string = root_string, "Family by poverty and race") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_family_poverty_race()

  data_saver(family_poverty_race, place, "family_poverty_race", desc_year = desc_year)

  family_type_income <- data_puller(root_string = root_string, "Family type and income") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_family_type_income()

  data_saver(family_type_income, place, "family_type_income", desc_year = desc_year)

  family_tenure <- data_puller(root_string = root_string, "Families by tenure") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_family_tenure

  data_saver(family_tenure, place, "family_tenure", desc_year = desc_year)
  #
  hous_famtype <-data_puller(root_string = root_string, "Households by family v nonfamily") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_hous_famtyp

  data_saver(hous_famtype, place, "hous_famtype_overall", desc_year = desc_year)

  hous_famtype_pov_age <-data_puller(root_string = root_string, "Households by family type and age and poverty") %>%
    puller_funct(place_string = place, no_pull = no_pull) %>%
    process_hous_povage

  data_saver(hous_famtype_pov_age, place, "hous_famtype_pov_age", desc_year = desc_year)

  # family type by income-level
  famtype_income_lev <- data_puller(root_string = root_string, data_string = "Family type by income-level") %>%
    puller_funct(place_string = place, no_pull = no_pull)  %>%
    process_famtype_incomelev
  
  data_saver(famtype_income_lev, place, "famtype_income_lev", desc_year = desc_year)
  
  # browser()
  
  # renter/owner cost burden by age
  df_own <- data_puller(root_string = root_string, "Owner cost burden by age") %>%
    puller_funct(place_string = place, no_pull = no_pull) 
  
  df_rent <- data_puller(root_string = root_string, "Renter cost burden by age") %>%
    puller_funct(place_string = place, no_pull = no_pull)
  
  tenure_costburden_age <- process_rent_own_costburden_age(df_own, df_rent)
  
  data_saver(tenure_costburden_age, place, "tenure_costburden_age", desc_year = desc_year)
  
  # age by income
  prc_pull_save(datapth = "Age by income", savefile = "age_income", processfunct = process_age_income)
  
  # family status by age and tenure
  prc_pull_save(datapth = "Age by tenure by household type", savefile = "tenure_family_age", processfunct = proces_family_age_tenure)
  
}

# process_pull_save(datapth = "Age by income", savefile = "age_income", processfunct = process_age_income, rs = "acs52019/all_municip_acs5_", "mdplaces", nop = T, yr = "acs5_2019")
# 
# process_pull_save(datapth = "Age by income", savefile = "age_income", processfunct = process_age_income, rs = "acs52019/maryland2019_acs5_", plc  = "Maryland", nop = T, yr = "acs5_2019")


# save all
data_creator(root_string = "acs52020/all_municip_acs5_", place = "mdplaces")

data_creator("acs52019/maryland2019_acs5_", place = "Maryland", no_pull = TRUE)

data_creator("acs52019/montcounty2019_acs5_", place = "mdcounties")

process_pull_save_loop <- function(datapath, savefile, processfunct, desc_year = "acs5_2019"){
  
  process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "acs52020/all_municip_acs5_", plc = "mdplaces", nop = T, yr = desc_year)
  process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "acs52020/maryland_acs5_", plc = "Maryland", nop = T, yr = desc_year)
  process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "acs52020/montcounty_acs5_", plc = "mdcounties", nop = T, yr = desc_year)
  
}

process_pull_save_loop("Veteran status by age and sex", savefile = "vetage", processfunct = process_vetage)


# read in/save
place_dir <- grep("mdplaces", dir("./data/output_data/acs5_2020/all"), value = T)

county_dir <- grep("mdcounties", dir("./data/output_data/acs5_2020/all"), value = T)

state_dir <- grep("Maryland", dir("./data/output_data/acs5_2020/all"), value = T)

# output tp
place_vector <- read_rds("./data/output_data/acs5_2020/all/burden_owners_bymortgage_mdplaces.rds") %>%
  pull(name) %>%
  unique()

county_vector <- read_rds("./data/output_data/acs5_2020/all/burden_owners_bymortgage_mdcounties.rds") %>%
  pull(name) %>%
  unique()

places <- "Takoma Park"

county_vector <- "(Montgomery County)"

if (!dir.exists("./data/acs52020/tpspec")){

}

quickdircreate("./data/output_data/acs5_2020/tpspec")

walk(place_dir, ~ {
  # browser()
  a <- read_rds(paste0("./data/output_data/acs5_2020/all/", .x)) %>%
       filter(grepl(places, name))
  
  write_rds(x = a,
                 file = paste0("./data/output_data/acs5_2020/tpspec/", 
                 gsub("mdplaces", "tp", .x)))})

walk(county_dir, ~ read_rds(paste0("./data/output_data/acs5_2020/all/", .x)) %>%
       filter(grepl(county_vector, name)) %>%
       write_rds(file = paste0("./data/output_data/acs5_2020/tpspec/", 
                 gsub("mdcounties", "montpg", .x))))

walk(state_dir, ~ read_rds(paste0("./data/output_data/acs5_2020/all/", .x)) %>%
       write_rds(file = paste0("./data/output_data/acs5_2020/tpspec/", 
                               .x)))

