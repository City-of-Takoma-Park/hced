# purpose - process data for hced analysis using acsprocess package
# last run: 09/19/2022

library(tidycensus)
library(dplyr)
library(tidyverse)

# downloaded from github - download with 
# devtools::install_github("dpowerstp/tpfuncts")
# devtools::install_github("dpowerstp/acsprocess")
library(tpfuncts)
library(acsprocess)

# update this parameter
year_downloading <- 2020

# read in varialbe names from tidycensus
variables <- tidycensus::load_variables(year_downloading, "acs5", cache = TRUE)

# function to read in data, apply a process function, and save to given directory
process_pull_save <- function(datapth, savefile, processfunct, rs, plc, nop, yr){
  # browser()
  
  data <- data_puller(root_string = rs, data_string = datapth) %>%
    puller_funct(place_string = plc, no_pull = nop) %>%
    processfunct() 
  
  data_saver(object = data, place_string = plc, data_string = savefile, desc_year = yr)
  
}

# build datasets ----
## function to build and save all datasets of interest
data_creator <- function(root_string, place, no_pull = T, desc_year = glue::glue("acs5_{year_downloading}")){
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

#
acsname <- paste0("acs5", year_downloading)

# save all
data_creator(root_string = glue::glue("{acsname}/all_municip_acs5_"), place = "mdplaces")

data_creator(glue::glue("{acsname}/maryland_acs5_"), place = "Maryland", no_pull = TRUE)

data_creator(glue::glue("{acsname}/montcounty_acs5_"), place = "mdcounties")

# run for verterans
# process_pull_save_loop <- function(datapath, savefile, processfunct, desc_year = glue::glue("acs5_{year_downloading}")){
#   
#   process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "{acsname}/all_municip_acs5_", plc = "mdplaces", nop = T, yr = desc_year)
#   process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "{acsname}/maryland_acs5_", plc = "Maryland", nop = T, yr = desc_year)
#   process_pull_save(datapth = datapath, savefile = savefile, processfunct = processfunct, rs =  "{acsname}/montcounty_acs5_", plc = "mdcounties", nop = T, yr = desc_year)
#   
# }
# 
# # process vets by age and sex
# process_pull_save_loop("Veteran status by age and sex", savefile = "vetage", processfunct = process_vetage)
# 

# pull all files in output directories
place_dir <- grep("mdplaces", dir(glue::glue("./data/output_data/acs5_{year_downloading}/all")), value = T)

county_dir <- grep("mdcounties", dir(glue::glue("./data/output_data/acs5_{year_downloading}/all")), value = T)

state_dir <- grep("Maryland", dir(glue::glue("./data/output_data/acs5_{year_downloading}/all")), value = T)

# # pull all place and county names in data
# place_vector <- read_rds(glue::glue("./data/output_data/acs5_{year_downloading}/all/burden_owners_bymortgage_mdplaces.rds")) %>%
#   pull(name) %>%
#   unique()
# 
# county_vector <- read_rds(glue::glue("./data/output_data/acs5_{year_downloading}/all/burden_owners_bymortgage_mdcounties.rds")) %>%
#   pull(name) %>%
#   unique()

places <- "Takoma Park"

county_vector <- "(Montgomery County)"

# create takoma park specific directory
quickdircreate(glue::glue("./data/output_data/acs5_{year_downloading}/tpspec"))

purrr::walk(place_dir, ~ {
  # write takoma park specific directories - subsetting places to tp and counties to montgomery county
  a <- read_rds(paste0(glue::glue("./data/output_data/acs5_{year_downloading}/all/"), .x)) %>%
    filter(grepl(places, name))
  
  
  write_rds(x = a,
            file = paste0(glue::glue("./data/output_data/acs5_{year_downloading}/tpspec/"), 
            gsub("mdplaces", "tp", .x)))})

walk(county_dir, ~ read_rds(paste0(glue::glue("./data/output_data/acs5_{year_downloading}/all/"), .x)) %>%
       filter(grepl(county_vector, name)) %>%
       # write file 
       write_rds(file = paste0(glue::glue("./data/output_data/acs5_{year_downloading}/tpspec/"), 
                               gsub("mdcounties", "montpg", .x))))

# run for state of md as well
walk(state_dir, ~ read_rds(
  paste0(glue::glue("./data/output_data/acs5_{year_downloading}/all/"), .x)) %>%
    write_rds(
      file = paste0(glue::glue("./data/output_data/acs5_{year_downloading}/tpspec/"), 
                    .x)))

