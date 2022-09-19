# purpose - load in data for hced data explorer using tidycensus
# last run: 09/19/2022

library(tidycensus)
library(tidyverse)
library(conflicted)

# install with devtools::install_github("dpowerstp/acsprocess")
library(acsprocess)

# update this parameter
year_downloading <- 2020

conflict_prefer("filter", "dplyr")

# read in variable names for given census year
variables_2020 <- tidycensus::load_variables(year_downloading, "acs5", cache = TRUE)

quickdircreate("./data")

# pull variables of interest ----

# variables on age
age_vars <- variables_2020 %>%
  pull(concept) %>%
  grep("( age)|(^age)", ., ignore.case = T, value = T) %>%
  unique()

# median age
medage_2020 <- (age_vars %>%
  grep("MEDIAN", ., value = T))[1]

vars_medage_2020 <- variables_2020 %>%
  # dplyr::filter(concept == medage_2020) %>%
  dplyr::filter(grepl("^B01002_", name)) %>%
  dplyr::pull(name)


variables <- variables_2020

# variables <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

age_vars <- variables %>%
  pull(concept) %>%
  grep("( age)|(^age)", ., ignore.case = T, value = T) %>%
  unique

age_inc <- grep("income", age_vars, ignore.case = T, value = T)

vars_age_inc <- variables %>%
  # filter(grepl("AGE OF HOUSEHOLDER BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS", x = concept) & grepl("DOLLARS\\)$", concept)) %>%
  filter(grepl("^B19037_", name)) %>%
  pull(name)

vars_age_owner_costburden <- variables %>%
  # filter(grepl("AGE OF HOUSEHOLDER BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS", concept)) %>%
  filter(grepl("^B25093_", name)) %>%
  pull(name)

vars_age_renter_costburden <- variables %>%
  # filter(grepl("AGE OF HOUSEHOLDER BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS", concept)) %>%
  filter(grepl("^B25072_", name)) %>%
  pull(name)

age_tenure <- grep("(tenure)|(owner)|(rent)", age_vars, ignore.case = T, value = T)

vars_age_tenure <- variables %>%
  # filter(grepl("TENURE BY AGE OF HOUSEHOLDER", concept)) %>%
  filter(grepl("^B25007_", name)) %>%
  pull(name)

vars_age_tenure_houstype <- variables %>%
  # filter(grepl("TENURE BY HOUSEHOLD TYPE \\(INCLUDING LIVING ALONE\\) AND AGE OF HOUSEHOLDER", concept)) %>%
  filter(grepl("^B25011_", name)) %>%
  pull(name)

# veteran variables
concept_vet <- variables %>%
  filter(grepl("veteran", label) | grepl("veteran", concept)) %>%
  pull(concept) %>%
  unique()
    
vars_veteran_age <- variables %>%
  # filter(grepl("SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER$", concept)) %>%
  filter(grepl("^B21001_", name)) %>%
  pull(name)

vars_veteran_employment <- variables %>%
  # filter(grepl("AGE BY VETERAN STATUS BY EMPLOYMENT STATUS FOR THE CIVILIAN POPULATION 18 TO 64 YEARS", concept)) %>%
  filter(grepl("^B21005", name)) %>%
  pull(name)

# variables_2018 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)

variables_unemploy <- variables %>%
  filter(grepl("unemploy", label, ignore.case = TRUE)) 

variables_tenure <- variables_unemploy %>%
  filter(grepl("tenure", concept, ignore.case = TRUE))

variables_tenure_second <- variables_unemploy %>%
  filter(grepl("renter", concept, ignore.case = TRUE))

#### maybe to add
# age by disability status by poverty status (aggregated to age/disability)
# race by age (without disability)
# computer overall - B28003_001

# household by owner/renter occupied
vars_housing_tenure <- c("B25013_001", "B25013_007", "B25013_002")

# household income by tenure
vars_income_tenure <- c("B25119_001", "B25119_002", "B25119_003")

# cost burden/severly cost burdened - renter
vars_rent_burden <- grep("B25070_", variables[["name"]], value = TRUE)

# cost burden/severely cost burden - housing units w/ and w/o a mortgage
vars_own_burden <- c("B25091_001", 
                     "B25091_002",
                     "B25091_003", 
                     "B25091_004",
                     "B25091_005",
                     "B25091_006",
                     "B25091_007",
                     "B25091_008",
                     "B25091_009",
                     "B25091_010", 
                     "B25091_011",
                     "B25091_012",
                     "B25091_013",
                     "B25091_014",
                     "B25091_015", 
                     "B25091_016", 
                     "B25091_017",
                     "B25091_018", 
                     "B25091_019", 
                     "B25091_020", 
                     "B25091_021", 
                     "B25091_022",
                     "B25091_023")

# foreign born - total, place of birth, citizenship
vars_foreign_born <- c("B05002_001",
                       "B05002_002",
                       "B05002_013")

vars_birth_foreign_born <- c("B05006_001",
                             "B05006_002",
                             "B05006_047",
                             "B05006_091",
                             "B05006_122",
                             "B05006_130",
                             "B05006_166")


# race by foreign born
vars_race_foreign_born <- variables %>%
  filter(grepl("B06004", name) & !grepl("PUERTO RICO", concept)) %>%
  pull(name)

# language spoken at home
vars_lang_home <- c("B06007_001", "B06007_002", "B06007_003", "B06007_006")

# prevailing language other than english

# change in hispanic/latinx population

## educational attainment overall
educ_vars <- paste0("B06009_00", 1:6)

variables %>%
  filter(name %in% educ_vars)
  

## educational attainment by race/ethnicity
# identify unique codes for race
educ_race_concept <- variables %>%
  filter(grepl("SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER", concept, ignore.case = TRUE)) %>% 
  pull(concept) %>% 
  unique()

educ_race_concept <- educ_race_concept[-c(1, 2)]

# pull codes corresponding to race/eduational attainment
vars_educ_race <- variables %>%
  # filter(concept %in% educ_race_concept) %>%
  filter(grepl("C15002[A-Z]_", name)) %>%
  pull(name)

#unemployment by race
unemploy_concept <- variables %>%
  filter(grepl("SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER", concept, ignore.case = TRUE)) %>%
  pull(concept) %>%
  unique()

vars_unemploy_race <- variables %>%
  # filter(concept %in% unemploy_concept) %>%
  filter(grepl("^(B23001_)|(C23002[A-Z])", name)) %>%
  pull(name)

# homeownership by race
home_own_concept <- variables %>%
  filter(grepl("^Tenure", concept, ignore.case = TRUE)) %>%
  pull(concept) %>%
  unique()

home_own_concept <- home_own_concept[1:10]

vars_home_own_race <- variables %>%
  # filter(concept %in% home_own_concept) %>%
  filter(grepl("^B25003", name)) %>%
  pull(name)

# poverty level
vars_poverty <- c("B07012_001", "B07012_002", "B07012_003", "B07012_004")

poverty_vars <- variables %>%
  filter(grepl("poverty", concept, ignore.case = T)) %>%
  pull(concept) %>%
  unique

vars_poverty_sex <- variables %>%
  filter(name %in% c(
    paste0("B17001_00", 1:9),
    paste0("B17001_0", 10:59)
    )) %>%
  pull(name)

vars_poverty_sex_race <- variables %>%
  # filter(grepl("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(", concept)) %>%
  filter(grepl("^B17001[A-Z]_", name)) %>%
  pull(name)


vars_poverty_detail <- variables %>%
  filter(grepl("C17002", name, ignore.case = T)) %>%
  pull(name)

# # household income last 12 months - homeowners by house value
# vars_income_last12 <- c("B25121_001", "B25121_002", "B25121_017", "B25121_032", "B25121_047", "B25121_062", "B25121_077", "B25121_092")

household_income_concept <- variables %>%
  filter(grepl("Household income in the past 12 months", concept, ignore.case = TRUE)) %>%
  pull(concept) %>%
  unique()

household_income_actual <- variables %>%
  filter(grepl("HOUSEHOLD INCOME IN THE PAST 12 MONTHS", concept))

# household income
vars_household_income_actual <- variables %>%
  filter(grepl("B19001_", name)) %>%
  pull(name)

# household income by race
variables %>%
  filter(grepl("black", concept, ignore.case = TRUE) & grepl("income", concept, ignore.case = TRUE)) %>%
  pull(concept) %>%
  unique

variables_2019 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

vars_race_income <- variables_2019 %>%
  filter(grepl("^HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)",
               concept))

vars_race_income %>%
  pull(concept) %>%
  unique()

vars_race_income <- variables %>%
  # filter(!grepl("(BY VALUE$)|(BY GROSS RENT)", concept) & !grepl("\\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)$", concept))
  filter(grepl("^B19001[A-Z]", name))

vars_race_income <- vars_race_income %>%
  pull(name)

# median income by race
vars_race_median_income <- variables_2019 %>%
  filter(grepl("^MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(IN 2019 INFLATION-ADJUSTED DOLLARS\\)",
               concept))

unique(vars_race_median_income$concept)

vars_race_median_income <- vars_race_median_income %>%
  filter(!grepl("DOLLARS\\) BY ", concept))

vars_race_median_income <- variables %>%
  filter(grepl("^B19013", name)) %>%
  pull(name)

# race of population
vars_race_pop <- c("B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B02001_009", "B02001_010", "B03002_003")

# hispanic/latin
vars_hisp_lat <- c("B03002_001", "B03002_012")

# foreign_born_birth <- foreign_born_birth %>%
#   separate_label(c(NA, NA, "continent", "americas")) %>%
#   mutate(continent = ifelse(!is.na(americas), americas, continent)) %>%
#   select(-americas) %>%
#   total_val() %>%
#   total_col_add(total_cols = list("continent_overall" = "continent"))
# 
# test_funct(foreign_born_birth, "continent_overall", "continent_overall_moe", component_moe = "moe", "pct_tp") %>%
#   as.data.frame()

# vars race tenure
vars_race_tenure_white <- c("B25003A_001", "B25003A_002", "B25003A_003")
vars_race_tenure_black <- c("B25003B_001", "B25003B_002", "B25003B_003")
vars_race_tenure_ai <- c("B25003C_001", "B25003C_002", "B25003C_003")
vars_race_tenure_asian <- c("B25003D_001", "B25003D_002", "B25003D_003")
vars_race_tenure_pi <- c("B25003E_001", "B25003E_002", "B25003E_003")
vars_race_tenure_other <- c("B25003F_001", "B25003F_002", "B25003F_003")
vars_race_tenure_two <- c("B25003G_001", "B25003G_002", "B25003G_003")
vars_race_tenure_white_nh <- c("B25003H_001", "B25003H_002", "B25003H_003")
vars_race_tenure_hisp <- c("B25003I_001", "B25003I_002", "B25003I_003")

vars_race_tenure <- c(vars_race_tenure_white, vars_race_tenure_black, vars_race_tenure_ai, vars_race_tenure_asian, vars_race_tenure_pi, vars_race_tenure_other, vars_race_tenure_two, vars_race_tenure_white_nh, vars_race_tenure_hisp)

# vars internet race
vars_race_internet_white <- c("B28009A_001", "B28009A_002", "B28009A_003", "B28009A_004", "B28009A_005",  "B28009A_006")
vars_race_internet_black <- c("B28009B_001", "B28009B_002", "B28009B_003", "B28009B_004", "B28009B_005",  "B28009B_006")
vars_race_internet_ai <- c("B28009C_001", "B28009C_002", "B28009C_003", "B28009C_004", "B28009C_005", "B28009C_006")
vars_race_internet_asian <- c("B28009D_001", "B28009D_002", "B28009D_003", "B28009D_004", "B28009D_005",  "B28009D_006")
vars_race_internet_pi <- c("B28009E_001", "B28009E_002", "B28009E_003", "B28009E_004", "B28009E_005", "B28009E_006")
vars_race_internet_other <- c("B28009F_001", "B28009F_002", "B28009F_003", "B28009F_004", "B28009F_005",  "B28009F_006")
vars_race_internet_two <- c("B28009G_001", "B28009G_002", "B28009G_003", "B28009G_004", "B28009G_005",  "B28009G_006")
vars_race_internet_white_nh <- c("B28009H_001", "B28009H_002", "B28009H_003", "B28009H_004", "B28009H_005",  "B28009H_006")
vars_race_internet_hisp <- c("B28009I_001", "B28009I_002", "B28009I_003", "B28009I_004", "B28009I_005",  "B28009I_006")

vars_race_internet <- c(vars_race_internet_white, vars_race_internet_black, vars_race_internet_ai, vars_race_internet_asian, vars_race_internet_pi, vars_race_internet_other, vars_race_internet_two, vars_race_internet_white_nh, vars_race_internet_hisp)

# vars age/race/disability
vars_race_disability_white <- c("B18101A_001", "B18101A_002", "B18101A_003", "B18101A_004", "B18101A_005", "B18101A_006", "B18101A_007", "B18101A_008", "B18101A_009", "B18101A_010")
vars_race_disability_black <- c("B18101B_001", "B18101B_002", "B18101B_003", "B18101B_004", "B18101B_005", "B18101B_006", "B18101B_007", "B18101B_008", "B18101B_009", "B18101B_010")
vars_race_disability_ai <- c("B18101C_001", "B18101C_002", "B18101C_003", "B18101C_004", "B18101C_005", "B18101C_006", "B18101C_007", "B18101C_008", "B18101C_009", "B18101C_010")
vars_race_disability_asian <- c("B18101D_001", "B18101D_002", "B18101D_003", "B18101D_004", "B18101D_005", "B18101D_006", "B18101D_007", "B18101D_008", "B18101D_009", "B18101D_010")
vars_race_disability_pi <- c("B18101E_001", "B18101E_002", "B18101E_003", "B18101E_004", "B18101E_005", "B18101E_006", "B18101E_007", "B18101E_008", "B18101E_009", "B18101E_010")
vars_race_disability_other <- c("B18101F_001", "B18101F_002", "B18101F_003", "B18101F_004", "B18101F_005", "B18101F_006", "B18101F_007", "B18101F_008", "B18101F_009", "B18101F_010")
vars_race_disability_two <- c("B18101G_001", "B18101G_002", "B18101G_003", "B18101G_004", "B18101G_005", "B18101G_006", "B18101G_007", "B18101G_008", "B18101G_009", "B18101G_010")
vars_race_disability_white_nh <- c("B18101H_001", "B18101H_002", "B18101H_003", "B18101H_004", "B18101H_005", "B18101H_006", "B18101H_007", "B18101H_008", "B18101H_009", "B18101H_010")
vars_race_disability_hisp <- c("B18101I_001", "B18101I_002", "B18101I_003", "B18101I_004", "B18101I_005", "B18101I_006", "B18101I_007", "B18101I_008", "B18101I_009", "B18101I_010")

vars_race_disability <- c(vars_race_disability_white, vars_race_disability_black, vars_race_disability_ai, vars_race_disability_asian, vars_race_disability_pi, vars_race_disability_other, vars_race_disability_two, vars_race_disability_white_nh, vars_race_disability_hisp)


# work transport by race
vars_race_transport_white <- c("B08105A_001", "B08105A_002", "B08105A_003", "B08105A_004", "B08105A_005", "B08105A_006", "B08105A_007")
vars_race_transport_black <- c("B08105B_001", "B08105B_002", "B08105B_003", "B08105B_004", "B08105B_005", "B08105B_006", "B08105B_007")
vars_race_transport_ai <- c("B08105C_001", "B08105C_002", "B08105C_003", "B08105C_004", "B08105C_005", "B08105C_006", "B08105C_007")
vars_race_transport_asian <- c("B08105D_001", "B08105D_002", "B08105D_003", "B08105D_004", "B08105D_005", "B08105D_006", "B08105D_007")
vars_race_transport_pi <- c("B08105E_001", "B08105E_002", "B08105E_003", "B08105E_004", "B08105E_005", "B08105E_006", "B08105E_007")
vars_race_transport_other <- c("B08105F_001", "B08105F_002", "B08105F_003", "B08105F_004", "B08105F_005", "B08105F_006", "B08105F_007")
vars_race_transport_two <- c("B08105G_001", "B08105G_002", "B08105G_003", "B08105G_004", "B08105G_005", "B08105G_006", "B08105G_007")
vars_race_transport_white_nh <- c("B08105H_001", "B08105H_002", "B08105H_003", "B08105H_004", "B08105H_005", "B08105H_006", "B08105H_007")
vars_race_transport_hisp <- c("B08105I_001", "B08105I_002", "B08105I_003", "B08105I_004", "B08105I_005", "B08105I_006", "B08105I_007")

vars_race_transport <- c(vars_race_transport_white, vars_race_transport_black, vars_race_transport_ai, vars_race_transport_asian, vars_race_transport_pi, vars_race_transport_other, vars_race_transport_two, vars_race_transport_white_nh, vars_race_transport_hisp)

#- receipt public assistance by race
vars_race_assist_white <- c("B22005A_001", "B22005A_002", "B22005A_003")
vars_race_assist_black <- c("B22005B_001", "B22005B_002", "B22005B_003")
vars_race_assist_ai <- c("B22005C_001", "B22005C_002", "B22005C_003")
vars_race_assist_asian <- c("B22005D_001", "B22005D_002", "B22005D_003")
vars_race_assist_pi <- c("B22005E_001", "B22005E_002", "B22005E_003")
vars_race_assist_other <- c("B22005F_001", "B22005F_002", "B22005F_003")
vars_race_assist_two <- c("B22005G_001", "B22005G_002", "B22005G_003")
vars_race_assist_white_nh <- c("B22005H_001", "B22005H_002", "B22005H_003")
vars_race_assist_hisp <- c("B22005I_001", "B22005I_002", "B22005I_003")

vars_race_assist <- c(vars_race_assist_white, vars_race_assist_black, vars_race_assist_ai, vars_race_assist_asian, vars_race_assist_pi, vars_race_assist_other, vars_race_assist_two, vars_race_assist_white_nh, vars_race_assist_hisp)


# variables income by owner costs
vars_income_owner_costs <- variables %>%
  filter(grepl("B25095", name)) %>%
  pull(name)

# variables income by renter costs
vars_income_renter_costs <- variables %>%
  filter(grepl("B25074", name)) %>%
  pull(name)

# vars computer
concept_computer <- variables %>%
  filter(grepl("internet", ignore.case = T, concept)) %>%
  pull(concept) %>%
  unique

df_computer <- variables %>%
  filter(grepl("PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD", ignore.case = T, concept))

vars_computer <- variables %>%
  filter(grepl("^(B28003_)|(B28009[A-Z])", name)) %>%
  pull(name)

# vars_computer <- df_computer %>%
#   filter(!grepl("B28008", name)) %>%
#   pull(name)

vars_disabilty <- variables %>%
  filter(grepl("B18101_", name)) %>%
  pull(name)

vars_health_insurance_age <- variables %>%
  filter(grepl("B27010", name)) %>%
  pull(name)

vars_health_insurance_race <- variables %>%
  filter(grepl("C27001", name)) %>%
  pull(name)

vars_gender <- c("B01001_001", "B01001_002", "B01001_026")

vars_age_gender <- variables %>%
  filter(grepl("B01001_", name)) %>%
  pull(name)

vars_race_age_gender <- variables %>%
  filter(grepl("B01001[A-Z]", name)) %>%
  pull(name)

vars_computer_age <- variables %>%
  filter(grepl("B28005", name)) %>%
  pull(name)

vars_poverty_race <- variables %>%
  filter(grepl("B17020[A-Z]", name)) %>%
  pull(name)

vars_ancestry <- variables %>%
  filter(grepl("B04006", name)) %>%
  pull(name)

vars_money <- variables %>%
  filter(grepl("\\$", label)) %>%
  pull(concept) %>%
  unique()

vars_asian_country <- variables %>%
  filter(grepl("B02015", name)) %>%
  pull(name)

vars_tenure_age <- variables %>%
  filter(grepl("B25007_", name)) %>%
  # filter(grepl("TENURE BY AGE OF HOUSEHOLDER$", concept)) %>%
  pull(name)

#  [7] "MEANS OF TRANSPORTATION TO WORK BY WORKERS' EARNINGS IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)"                                                                                            
# [58] "VALUE"

vars_lang_home_detail <- variables %>%
  filter(grepl("B16001", name)) %>%
  pull(name)

vars_lang_home_age <- variables %>%
  filter(grepl("B16007", name)) %>%
  pull(name)

vars_disab_type <- variables %>%
  filter(grepl("B1810[2-7]", name)) %>%
  pull(name)

vars_family <- variables %>%
  filter(grepl("B11012_", name)) %>%
  # filter(grepl("^HOUSEHOLDS BY TYPE", concept)) %>%
  pull(name)

concepts_child <- variables %>%
  filter(grepl("child", label)) %>% 
  pull(concept) %>%
  unique()

vars_family_poverty_overall <- variables %>%
  filter(grepl("B17010_", name)) %>%
  # filter(grepl("POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN$", concept)) %>%
  pull(name)

vars_family_poverty_race <- variables %>%
  filter(grepl("B17010[A-Z]", name)) %>%
  # filter(grepl("POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN \\(", concept)) %>%
  pull(name)

vars_family_poverty_numchildren <- variables %>%
  filter(grepl("B17012_", name)) %>%
  # filter(grepl("POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF RELATED CHILDREN UNDER 18 YEARS", concept))%>%
  pull(name)

vars_family_incomepoverty <- variables %>%
  filter(grepl("^B17022_", name)) %>%
  # filter(grepl("RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN", concept)) %>%
  pull(name)

vars_family_income <- variables %>%
  filter(grepl("B19125_", name)) %>%
  # filter(grepl("MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS \\(IN 2019 INFLATION-ADJUSTED DOLLARS\\) BY PRESENCE OF OWN CHILDREN UNDER 18 YEARS", concept)) %>%
  pull(name)

vars_family_income_familytype <- variables %>%
  filter(grepl("B19126_", name)) %>%
  # filter(grepl("MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS \\(IN 2019 INFLATION-ADJUSTED DOLLARS\\) BY FAMILY TYPE BY PRESENCE OF OWN CHILDREN UNDER 18 YEARS", concept)) %>%
  pull(name)

vars_family_publicassist <- variables %>%
  filter(grepl("B22002_", name)) %>%
  # filter(grepl("RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY PRESENCE OF CHILDREN UNDER 18 YEARS BY HOUSEHOLD TYPE FOR HOUSEHOLDS", concept)) %>%
  pull(name)

vars_family_employstatus <- variables %>%
  filter(grepl("B23007_", name)) %>%
  # filter(grepl("PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY EMPLOYMENT STATUS", concept)) %>%
  pull(name)

vars_family_tenure <- variables %>%
  filter(grepl("B25012_", name)) %>%
  # filter(grepl("TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN", concept)) %>%
  pull(name)

vars_familynonfamilyhous <- variables %>%
  filter(grepl("B11001", name)) %>%
  pull(name)

vars_familynonfamily_65plus <- variables %>%
  filter(grepl("B09020", name)) %>%
  pull(name)

vars_houstype_poverty <- variables %>%
  filter(grepl("B17017_", name)) %>%
  # filter(grepl("POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER", concept)) %>%
  pull(name)

vars_fantype_povratio <- variables %>%
  filter(grepl("B17026_0", name)) %>%
  pull(name)

vars_famtype_income <- variables %>%
  filter(grepl("B19131_", name)) %>%
  pull(name)

#load datasets using constructed variables
dfs_list <- list("Unemployment by race" = vars_unemploy_race,
                 "Race_ethnicity" = c(vars_race_pop, vars_hisp_lat),
                 "Poverty" = vars_poverty,
                 "Income last twelve months" = vars_household_income_actual,
                 "Home ownership by race" = vars_home_own_race,
                 "Educational attainment by race" = vars_educ_race,
                 "Language at home" = vars_lang_home,
                 "Race by foreign born" = vars_race_foreign_born,
                 "Foreign born" = vars_foreign_born,
                 "Foreign born place of birth" = vars_birth_foreign_born,
                 "Rent burden - owners" = vars_own_burden,
                 "Rent burden - renters" = vars_rent_burden,
                 "Income by tenure" = vars_income_tenure,
                 "Housing tenure" = vars_housing_tenure,
                 "Race Tenure" = vars_race_tenure,
                 "Race Internet"= vars_race_internet,
                 "Race assist" = vars_race_assist,
                 "Race disability" = vars_race_disability,
                 "Race transport" = vars_race_transport,
                 "Household income by owner housing costs" = vars_income_owner_costs,
                 "Household income by renter housing costs" = vars_income_renter_costs,
                 "Education overall" = educ_vars,
                 "Household income by race" = vars_race_income,
                 "Median income by race" = vars_race_median_income,
                 "Computer overall and by race" = vars_computer,
                 "Health insurance by age" = vars_health_insurance_age,
                 "Gender" = vars_gender,
                 "Age by gender" = vars_age_gender,
                 "Race age gender" = vars_race_age_gender,
                 "Computer by age" = vars_computer_age,
                 "Poverty by race and age" = vars_poverty_race,
                 "Disability overall" = vars_disabilty,
                 "Ancestry" = vars_ancestry,
                 "Asian disaggregated" = vars_asian_country,
                 "Poverty status detailed" = vars_poverty_detail,
                 "Health insurance by race and age" = vars_health_insurance_race,
                 "Tenure by age" = vars_tenure_age,
                 "Detailed language" = vars_lang_home_detail,
                 "Language by age" = vars_lang_home_age,
                 "Disability by age and type" = vars_disab_type,
                 "Poverty by sex" = vars_poverty_sex,
                 "Poverty by sex and race" = vars_poverty_sex_race,
                 "Family overall" = vars_family,
                 "Family by poverty" = vars_family_poverty_overall,
                 "Family by poverty and race" = vars_family_poverty_race,
                 "Families by tenure" = vars_family_tenure,
                 "Families by poverty and race" = vars_family_poverty_race, 
                 "Families by public assistance"= vars_family_publicassist,
                 "Family type and income" = vars_family_income_familytype,
                 "Households by family v nonfamily" = vars_familynonfamilyhous,
                 "Households by family type and age and poverty" = vars_houstype_poverty,
                 "Family type by income to poverty ratio" = vars_fantype_povratio,
                 "Family type by income-level" = vars_famtype_income,
                 "Age by income" = vars_age_inc,
                 "Owner cost burden by age" = vars_age_owner_costburden,
                 "Renter cost burden by age" = vars_age_renter_costburden,
                 "Age by tenure by household type" = vars_age_tenure_houstype,
                 "Median age by sex" = vars_medage_2020,
                 "Veteran status by age and sex" = vars_veteran_age
                 )

# dfs_list_vector <- unlist(dfs_list, use.names = FALSE, recursive = TRUE)

# vars_load <- map(grep("vars_", ls(), value = TRUE), ~ eval(sym(.x))) %>%
#   unlist()

tp_load <- function(geog, vars_load, year = 2020){
  print(vars_load)
  
  tp_data <- tidycensus::get_acs(geography = geog, 
                     year = year, 
                     moelevel = 90,
                     cache_table = TRUE, 
                     state = "Maryland", 
                     survey = "acs5",
                     variables = vars_load, 
                     show_call = TRUE)
  
  return(tp_data)
  
}
# dfs_list[23:24]

multi_year_process <- function(dfs_arg = dfs_list,
                               place_type,
                               year){
  
  place_prefix <- case_when(place_type == "place" ~ "all_municip_acs5_",
                            place_type == "county" ~ "montcounty_acs5_",
                            place_type == "state" ~ "maryland_acs5_")
  
  
  output_dir <- paste0("data/acs5", year, "/")
  
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  walk2(dfs_arg, names(dfs_arg), ~ {
    # print(.y)
    # print(paste0(output_dir, place_prefix, .y, ".rds"))
    tp_load(place_type, .x, year = year) %>%
      saveRDS(file = paste0(output_dir, place_prefix, .y, ".rds"))
  })
  
  return(output_dir)
}


# 2015-2019 acs
multi_year_process(dfs_list, place_type = "place", year = 2019)

multi_year_process(dfs_list, place_type = "state", year = 2019)


multi_year_process(dfs_list, place_type = "county", year = 2019)



# 2016-2020 acs - update year parameter for new year when updating data explorer
multi_year_process(dfs_list, place_type = "place", year = year_downloading)

multi_year_process(dfs_list, place_type = "state", year = year_downloading)

multi_year_process(dfs_list, place_type = "county", year = year_downloading)



# 
# walk2(dfs_list[25:25], names(dfs_list)[25:25], ~ {
#   print(.y)
#   tp_load("place", .x) %>%
#     saveRDS(file = paste0("data/all_municip_acs5_2019_", .y, ".rds"))
# })
# 
# test_data <- read_rds("./data/output_data/acs5_2019/renter_income_num_Takoma.rds")
# 
# # save montgomery county
# walk2(dfs_list[25:25], names(dfs_list)[25:25], ~ {
#   print(.y)
#   tp_load("county", .x) %>%
#     saveRDS(file = paste0("data/montcounty2019_acs5_", .y, ".rds"))
# })
# 
# 
# # save maryland
# walk2(dfs_list[25:25], names(dfs_list)[25:25], ~ {
#   print(.y)
#   tp_load("state", .x) %>%
#     saveRDS(file = paste0("data/maryland2019_acs5_", .y, ".rds"))
# })
# 
