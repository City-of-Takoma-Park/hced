# create map showing racial demographics of tp at block group levels

library(dplyr)
library(tidyverse)
# library(conflicted)
library(plotly)
library(readxl)
library(rlang)
library(tidycensus)
library(spdep)
library(sf)
library(leaflet)

library(leafletwrappers)
library(glue)
library(danfuncts)

content_dir <- "./data/"

data_path <- function(...){
  paste0(content_dir, ...)
}

bg_data <- read.csv("./data/nhgis/hced_request_vars/nhgis0009_ds244_20195_2019_blck_grp.csv")


# rename columns to descriptive names and remove extraneous 
bg_data_rename <- bg_data %>%
  rename_all(~ bg_rename(.x)) %>%
  select(-contains("ALWY")) %>%
  rename_all(tolower) %>%
  select(-c(stusab, regiona, divisiona, state, statea, countya, cousuba, placea, concita, aianhha, res_onlya, trusta, aihhtli, aits, anrca, cbsaa, csaa, metdiva, memi, nectaa, cnectaa, nectadiva, uaa, cdcurra, sldua, sldla, zcta5a, submcda, sdelma_moe, sdseca, sdunia, ur, pci, puma5a, bttra, btbga))

# create columns 
# - less high school
# - below median income
# - limited english

bg_data_process <- bg_data_rename %>%
  rowwise() %>%
  mutate(
    # less than high school education
    educ_less_hs = sum(educ_no_school,
                       educ_nurse_school,
                       educ_kindgar,
                       educ_1st_grade,
                       educ_2nd_grade,
                       educ_3rd_grade,
                       educ_4th_grade,
                       educ_5th_grade,
                       educ_6th_grade,
                       educ_7th_grade,
                       educ_8th_grade,
                       educ_9th_grade,
                       educ_10th_grade,
                       educ_11th_grade,
                       educ_12th_grade),
    educ_less_hs_pct = pct_round(educ_less_hs, educ_tot_bg),
    # limited english proficiency
    lang_limeng = sum(lang_span_limeng,
                      lang_otherindoeuro_limeng,
                      lang_asiapacific_limeng,
                      lang_otherlang_limeng),
    lang_limeng_pct = pct_round(lang_limeng, lang_tot),
    # households income less than 75k
    houseinc_below75 = sum(houseinc_less10k,
                           houseinc_1014,
                           houseinc_1519,
                           houseinc_2024,
                           houseinc_2529,
                           houseinc_3034,
                           houseinc_3539,
                           houseinc_4045,
                           houseinc_4549,
                           houseinc_5059,
                           houseinc_6074),
    # household income less than 100k
    houseinc_below100 = sum(houseinc_below75,
                            houseinc_7599),
    houseinc_below75_pct = pct_round(houseinc_below75, houseinc_total),
    houseinc_below100_pct = pct_round(houseinc_below100, houseinc_total),
    # household income less than 25k
    houseinc_below25 = sum(houseinc_less10k,
                           houseinc_1014,
                           houseinc_1519,
                           houseinc_2024),
    houseinc_below25_pct = pct_round(houseinc_below25, houseinc_total),
    # household income less than 30k
    houseinc_below30 = sum(houseinc_below25, houseinc_2529),
    houseinc_below30_pct = pct_round(houseinc_below30, houseinc_total),
    # income poverty ratio under 1.25
    incpovratio_below125 = sum(incpovratio_under5,
                               incpovratio_599,
                               incpovratio_1124),
    incpovratio_below150 = sum(incpovratio_below125,
                               incpovratio_125149),
    incpovratio_below125_pct = pct_round(incpovratio_below125, incpovratio_total),
    incpovratio_below150_pct = pct_round(incpovratio_below150, incpovratio_total),
    # public assistance
    pubassist_yes_pct = pct_round(pubassist_yes, pubassist_total)) %>%
  # round numeric variables
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))

# remove extraneous columns
bg_data_process_select <- bg_data_process %>%
  select(gisjoin, year, county, tracta, blkgrpa, name_e, educ_tot_bg, houseinc_total, incpovratio_total, lang_tot, educ_less_hs, educ_less_hs_pct, lang_limeng, lang_limeng_pct, houseinc_below75, houseinc_below75_pct, houseinc_below100, houseinc_below100_pct, houseinc_below30, houseinc_below30_pct, houseinc_below25, houseinc_below25_pct, incpovratio_below125, incpovratio_below125_pct, incpovratio_below150, incpovratio_below150_pct, pubassist_total, pubassist_yes, pubassist_yes_pct)

# identify block groups in takoma park
bgs_2019 <- list("Block Group" = c("2", "3", "1", "2", "4", "3", "2", "1", "1", "3", "2", "1", "3", "1", "2", "1"),
                 "Census Tract" = c("7017.04", "7017.01", "7017.04", "7017.01", "7018", "7018", "7018", "7018", "7017.02", "7019", "7020", "7017.03", "7017.03", "7017.04", "7017.03", "7017.01"))

bgs_2019_match <- glue::glue("Block Group {bgs_2019[['Block Group']]}, Census Tract {bgs_2019[['Census Tract']]}, Montgomery County, Maryland")

write.csv(bg_data_process_select, "./data/output_data/shapefiles/bg_data_2019_process_select.csv")

bg_data_process_select_tp <- bg_data_process_select %>%
  filter(name_e %in% bgs_2019_match)

write.csv(bg_data_process_select_tp, "./data/output_data/shapefiles/bg_data_2019_process_select_tp.csv")

bg_shapes <- st_read("./data/nhgis/md_bg/MD_blck_grp_2019.shp") %>%
  rename_all(tolower) %>%
  st_transform(4326)

# join data to shape
bg_shapes_join <- bg_shapes %>%
  left_join(bg_data_process_select)


# confirm all joined
bg_shapes %>%
  anti_join(bg_data_process_select)

color_pal <- function(df, var, funct_return = F) {
  funct <- colorNumeric("viridis", range(bg_md[[var]], na.rm = T))
  
  if (funct_return == T){
    return(funct)
  }
  return(funct(bg_md[[var]]))
}

bg_shapes_join_data <- st_drop_geometry(bg_shapes_join)

# leaflet(bg_merge) %>%
#   addPolygons(group = "Black", fill = T, color = ~ color_pal(bg_merge_data$ALUCE003))

bg_shapes_join_montcounty <- bg_shapes_join %>%
  filter(grepl("Montgomery", county))

st_write(bg_shapes_join_montcounty, "./data/output_data/shapefiles/bg_shapes_2019_join_montcounty.geojson", delete_dsn = T)

# wards function
add_city <- function(basemap){
  basemap %>%
    addPolygons(data = city_boundaries, 
                fill = FALSE,
                smoothFactor = 0.5,
                weight = 1, 
                opacity = 1,
                color = "#646464", 
                label = "Takoma Park",
                # options = pathOptions(pane = "borders"),
                labelOptions = labelOptions(noHide = TRUE,
                                            direction = 'top',
                                            textOnly = TRUE,
                                            style = list("font-weight" = "bold", padding = "1px 1px"),
                                            textsize = "10.25px"))
}



# # vars add: race percent, youth percent, elderly percent, percent disability, percent below poverty line, percent no vehicle
# 
# variables <- tidycensus::load_variables("2019", "acs5")
# 
# # pull vars race
# vars_race <- variables %>%
#   filter(grepl("B02001", name))
# 
# vars_hisp <- variables %>%
#   filter(grepl("B03002", name))
# 
# # pull vars youth/edlerly
# vars_youth <- variables %>%
#   filter(grepl("B01001_", name))
# 
# # vars disability
# vars_disabilty <- variables %>%
#   filter(grepl("B18101_", name))
# 
# # below poverty line
# vars_poverty <- variables %>%
#   filter(grepl("B06012_00[1-4]", name))
# 
# # no vehicle
# vars_vehicle <- variables %>%
#   filter(grepl("B08014_00[1-2]", name))
# 
# dfs_list <- list("race" = vars_race,
#                  "hisp" = vars_hisp,
#                  "disability" = vars_disabilty,
#                  "poverty" = vars_poverty,
#                  "vehicle" = vars_vehicle)
# 
# # read in/save dfs and assign to global environ
# walk2(dfs_list, names(dfs_list), ~ {
#   
#   df <- tidycensus::get_acs(geography = "block group",
#                       variables = pull(.x, name), 
#                       cache_table = T, 
#                       year = 2019, 
#                       state = "MD", 
#                       geometry = T, 
#                       moe_level = 90, 
#                       survey = "acs5")
#   
#   # save df
#   write_rds(df, glue("./data/source/acs/{.y}_acs5_2019.rds"))
#   
#   assign(x = glue("acs_{.y}"), df, envir = globalenv())
#   
# })
# 
# 


############### 2018 data

bg_data_2018 <- read.csv("./data/nhgis/hced_request_vars/nhgis0010_ds239_20185_2018_blck_grp.csv")

# rename columns to descriptive names and remove extraneous 
bg_data_2018_rename <- bg_data_2018 %>%
  rename_all(~ bg_rename(.x, 2018)) %>%
  select(-contains("AJY7")) %>%
  rename_all(tolower) %>%
  select(-c(regiona, divisiona, state, statea, countya, cousuba, placea, concita, aianhha, res_onlya, trusta,  anrca, cbsaa, csaa, metdiva, nectaa, cnectaa, nectadiva, uaa, cdcurra, sldua, sldla, zcta5a, submcda, sdelma_moe, sdseca, sdunia, puma5a, bttra, btbga))

# create columns 
# - less high school
# - below median income
# - limited english

bg_data_2018_process <- bg_data_2018_rename %>%
  rowwise() %>%
  mutate(educ_less_hs = sum(educ_no_school,
                            educ_nurse_school,
                            educ_kindgar,
                            educ_1st_grade,
                            educ_2nd_grade,
                            educ_3rd_grade,
                            educ_4th_grade,
                            educ_5th_grade,
                            educ_6th_grade,
                            educ_7th_grade,
                            educ_8th_grade,
                            educ_9th_grade,
                            educ_10th_grade,
                            educ_11th_grade,
                            educ_12th_grade),
         educ_less_hs_pct = pct_round(educ_less_hs, educ_tot_bg),
         lang_limeng = sum(lang_span_limeng,
                           lang_otherindoeuro_limeng,
                           lang_asiapacific_limeng,
                           lang_otherlang_limeng),
         lang_limeng_pct = pct_round(lang_limeng, lang_tot),
         houseinc_below75 = sum(houseinc_less10k,
                                houseinc_1014,
                                houseinc_1519,
                                houseinc_2024,
                                houseinc_2529,
                                houseinc_3034,
                                houseinc_3539,
                                houseinc_4045,
                                houseinc_4549,
                                houseinc_5059,
                                houseinc_6074),
         houseinc_below100 = sum(houseinc_below75,
                                 houseinc_7599),
         houseinc_below75_pct = pct_round(houseinc_below75, houseinc_total),
         houseinc_below100_pct = pct_round(houseinc_below100, houseinc_total),
         incpovratio_below125 = sum(incpovratio_under5,
                                    incpovratio_599,
                                    incpovratio_1124),
         houseinc_below25 = sum(houseinc_less10k,
                                houseinc_1014,
                                houseinc_1519,
                                houseinc_2024),
         houseinc_below25_pct = pct_round(houseinc_below25, houseinc_total),
         houseinc_below30 = sum(houseinc_below25, houseinc_2529),
         houseinc_below30_pct = pct_round(houseinc_below30, houseinc_total),
         incpovratio_below150 = sum(incpovratio_below125,
                                    incpovratio_125149),
         incpovratio_below125_pct = pct_round(incpovratio_below125, incpovratio_total),
         incpovratio_below150_pct = pct_round(incpovratio_below150, incpovratio_total),
         pubassist_yes_pct = pct_round(pubassist_yes, pubassist_total)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))

# remove extraneous columns
bg_data_2018_process_select <- bg_data_2018_process %>%
  select(gisjoin, year, county, tracta, blkgrpa, name_e, educ_tot_bg, houseinc_total, incpovratio_total, lang_tot, educ_less_hs, educ_less_hs_pct, lang_limeng, lang_limeng_pct, houseinc_below75, houseinc_below75_pct, houseinc_below100, houseinc_below100_pct, houseinc_below30, houseinc_below30_pct, houseinc_below25, houseinc_below25_pct, incpovratio_below125, incpovratio_below125_pct, incpovratio_below150, incpovratio_below150_pct, pubassist_total, pubassist_yes, pubassist_yes_pct)

write.csv(bg_data_2018_process_select, "./data/output_data/shapefiles/bg_data_2018_process_select.csv")

bg_data_2018_process_select_tp <- bg_data_2018_process_select %>%
  filter(name_e %in% bgs_2019_match)

write.csv(bg_data_2018_process_select_tp, "./data/output_data/shapefiles/bg_data_2018_process_select_tp.csv")

bg_shapes_2018 <- st_read("./data/nhgis/md_bg/MD_blck_grp_2019.shp") %>%
  rename_all(tolower) %>%
  st_transform(4326)

# join data to shape
bg_shapes_2018_join <- bg_shapes_2018 %>%
  left_join(bg_data_2018_process_select)

# confirm all joined
bg_shapes_2018 %>%
  anti_join(bg_data_2018_process_select)

color_pal <- function(df, var, funct_return = F) {
  funct <- colorNumeric("viridis", range(bg_md[[var]], na.rm = T))
  
  if (funct_return == T){
    return(funct)
  }
  return(funct(bg_md[[var]]))
}

bg_shapes_2018_join_data <- st_drop_geometry(bg_shapes_2018_join)

# leaflet(bg_merge) %>%
#   addPolygons(group = "Black", fill = T, color = ~ color_pal(bg_merge_data$ALUCE003))


bg_shapes_2018_join_montcounty <- bg_shapes_2018_join %>%
  filter(grepl("Montgomery", county))


st_write(bg_shapes_2018_join_montcounty, "./data/output_data/shapefiles/bg_shapes_2018_join_montcount.geojson", delete_dsn = T)



############# map making

### define palette functions
# one to color based on range of percent values
pal_numeric <- function(var, colors = "Blues", df = bg_md, reverse = FALSE) {
  colorNumeric(palette = colors, domain = df[[var]], reverse = reverse)
}

# define list of colors corresponding to each theme of social vulnerability
colors_select <- list("overall" = "RdPu", "socioeconomic" = "Greens", "households" = "Oranges", "minority" = "Blues", "housing" = "Purples")

# function for percentiles
pal_pctile <- function(colors = "Blues"){
  colorBin(palette = colors, domain = c(0, 100), bins = seq(0, 100, 10))
}

# define common labels
labels_funct <- function(df_as_data){
  map(seq(nrow(df_as_data)), function(i){
    return(
      glue::glue("{df_as_data[i, 'name_e']}<p></p>
        Total residents without high school or GED: {df_as_data[i, 'educ_less_hs']}<p></p>
        Total households with limited English proficiency: {df_as_data[i, 'lang_limeng']}<p></p>
        Total households below $25k: {df_as_data[i, 'houseinc_below25']}<p></p>
        Total households below $30k: {df_as_data[i, 'houseinc_below30']}<p></p>
        Total residents with income-poverty ratio below 1.25: {df_as_data[i, 'incpovratio_below125']}<p></p>
        Total households receiving public assistance: {df_as_data[i, 'pubassist_yes']}<p></p>
        Percent residents without high school or GED: {df_as_data[i, 'educ_less_hs_pct']}%<p></p>
        Percent households with limited English proficiency: {df_as_data[i, 'lang_limeng_pct']}%<p></p>
        Percent households below $25k: {df_as_data[i, 'houseinc_below25_pct']}%<p></p>
        Percent households below $30k: {df_as_data[i, 'houseinc_below30_pct']}%<p></p>
        Percent residents with income-poverty ratio below 1.25: {df_as_data[i, 'incpovratio_below125_pct']}%<p></p>
        Percent households receiving public assistance: {df_as_data[i, 'pubassist_yes_pct']}%")
    )
  })
}

### Define 
addpoly_standard <- function(basemap,  
                             df, 
                             pal_funct, 
                             variable, 
                             group, 
                             title, 
                             labels){
  # browser()
  basemap %>%
    addPolygons(color = "transparent",
                weight = 1, 
                fill = T,
                opacity = 1, 
                fillOpacity = 0.4,
                fillColor = ~ pal_funct(df[[variable]]),
                group = group, 
                highlight = highlightOptions(stroke = TRUE,
                                             weight = 3.5, 
                                             fillOpacity = 0.6, 
                                             color = "#555EE7", 
                                             opacity = 1, 
                                             bringToFront = TRUE),
                label = map(labels, htmltools::HTML),
                # options = pathOptions(pane = "polygons"),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", 
                               "padding" = "0.2px 0.2px",
                               "line-height" = 0.8),
                  textsize = "10px",
                  direction = "auto", 
                  opacity = 0.8))
}

addlegend_standard <- function(basemap, df, pal_funct, variable, group, title){
  basemap %>%
    addLegend("topright", 
              pal = pal_funct, 
              values = ~ df[[variable]], 
              opacity = 0.7, 
              group = group, 
              title = title)
}

addpoly_legend <- function(basemap_select, df_select = bg_shapes_2018_join_data, pal_funct_select, variable_select, group_select, title_select = "Percent", labels_select){
  
  basemap_select %>%
    addpoly_standard(df_select, pal_funct_select, variable_select, group_select, labels = labels_select) %>%
    addlegend_standard(df_select, pal_funct_select, variable_select, group_select, title_select)
}

city_boundaries <- st_read("./data/citydata/City Base Data.gdb", layer = "Boundary") %>%
  st_transform(4326)

add_wards <- function(basemap){
  basemap %>%
    addPolygons(data = wards, 
                fill = FALSE,
                smoothFactor = 0.5,
                weight = 1, 
                opacity = 1,
                color = "#646464", 
                label = ~ str_to_title(WARD),
                # options = pathOptions(pane = "borders"),
                labelOptions = labelOptions(noHide = TRUE,
                                            direction = 'center',
                                            textOnly = TRUE,
                                            style = list("font-weight" = "bold", padding = "1px 1px"),
                                            textsize = "10.25px"))
}


wards <- st_read("./data/wards/buffer_fix.shp") %>%
  st_transform(4326)



# create leaflet map with overall vulnerability and components
map_funct <- function(basemap = bg_shapes_join_montcounty, 
                      group_cols = c("Residents without high-school education: total",
                                     "Residents without high-school education: percent",
                                     "Households with limited English proficiency: total",
                                     "Households with limited Enlgish proficiency: percent",
                                     "Households below $25k: total",
                                     "Households below $25k: percent",
                                     "Households below $30k: total",
                                     "Households below $30k: percent",
                                     "Residents below income-poverty ratio of 1.25: total",
                                     "Residents below income-poverty ratio of 1.25: percent",
                                     "Households receiving public assistance: total",
                                     "Households receiving public assistance: percent"),
                      collapsed_val = FALSE){
  
  as_data_file <- st_drop_geometry(basemap)
  # browser()
  label_result <- labels_funct(as_data_file)
  
  group_cols <- group_cols
  
  map <- leaflet(basemap) %>%
    addTiles(options = tileOptions(opacity = 0.5)) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("educ_less_hs", df = as_data_file, "Blues"), 
                   "educ_less_hs", 
                   group_select = "Residents without high-school education: total", 
                   "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("educ_less_hs_pct", df = as_data_file, "Blues"), 
                   "educ_less_hs_pct", 
                   group_select = "Residents without high-school education: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("lang_limeng", 
                               df = as_data_file, 
                               "Greys"), 
                   "lang_limeng",
                   group_select = "Households with limited English proficiency: total", 
                   "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("lang_limeng_pct", 
                                             df = as_data_file, "Greys"), 
                   "lang_limeng_pct",
                   group_select = "Households with limited Enlgish proficiency: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("houseinc_below25", 
                               df = as_data_file, 
                               "Oranges"), 
                   "houseinc_below25",
                   group_select = "Households below $25k: total", "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("houseinc_below25_pct", df = as_data_file, "Oranges"), 
                   "houseinc_below25_pct",
                   group_select = "Households below $25k: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("houseinc_below30", 
                                             df = as_data_file, 
                                             "Oranges"), 
                   "houseinc_below30",
                   group_select = "Households below $30k: total", 
                   "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("houseinc_below30_pct", 
                                             df = as_data_file, 
                                             "Oranges"), 
                   "houseinc_below30_pct",
                   group_select = "Households below $30k: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("incpovratio_below125", df = as_data_file, "Purples"), 
                   "incpovratio_below125",
                   group_select = "Residents below income-poverty ratio of 1.25: total", 
                   "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("incpovratio_below125_pct", df = as_data_file, 
                               "Purples"), 
                   "incpovratio_below125_pct",
                   group_select = "Residents below income-poverty ratio of 1.25: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("pubassist_yes", 
                               df = as_data_file, "Greens"), 
                   "pubassist_yes",
                   group_select = "Households receiving public assistance: total", 
                   "Total", 
                   labels_select = label_result) %>%
    addpoly_legend(as_data_file, 
                   pal_numeric("pubassist_yes_pct", df = as_data_file, "Greens"), 
                   "pubassist_yes_pct",
                   group_select = "Households receiving public assistance: percent", 
                   "Percent", 
                   labels_select = label_result) %>%
    addLayersControl(overlayGroups = group_cols, 
                     options = layersControlOptions(collapsed = collapsed_val)) %>%
    hideGroup(group_cols[-1])
  
  return(map) 
}


mont_county_2019 <- map_funct(bg_shapes_join_montcounty) %>%
  add_city()

mont_county_2018 <- map_funct(bg_shapes_2018_join_montcounty) %>%
  add_city()

bg_shapes_join_tp_2019 <- bg_shapes_join_montcounty %>%
  filter(name_e %in% bgs_2019_match)


tp_2019 <- map_funct(bg_shapes_join_tp_2019) %>%
  add_city()

bg_shapes_join_tp_2018 <- bg_shapes_2018_join %>%
  filter(name_e %in% bgs_2019_match)

tp_2018 <- map_funct(bg_shapes_join_tp_2018) %>%
  add_city()

htmlwidgets::saveWidget(mont_county_2019, "./data/output_data/leaflet_maps/planning_vars_mont_county_2019.html", selfcontained = T)

htmlwidgets::saveWidget(mont_county_2018, "./data/output_data/leaflet_maps/planning_vars_mont_county_2018.html", selfcontained = T)

htmlwidgets::saveWidget(tp_2019, "./data/output_data/leaflet_maps/planning_vars_tp_2019.html", selfcontained = T)

htmlwidgets::saveWidget(tp_2018, "./data/output_data/leaflet_maps/planning_vars_tp_2018.html", selfcontained = T)



mont_county_2019_wards <- map_funct(bg_shapes_join_montcounty) %>%
  add_wards()

mont_county_2018_wards <- map_funct(bg_shapes_2018_join_montcounty) %>%
  add_wards()

tp_2019_wards <- map_funct(bg_shapes_join_tp_2019) %>%
  add_wards()

tp_2018_wards <- map_funct(bg_shapes_join_tp_2018) %>%
  add_wards()

htmlwidgets::saveWidget(mont_county_2019_wards, "./data/output_data/leaflet_maps/planning_vars_mont_county_2019_wards.html", selfcontained = T)

htmlwidgets::saveWidget(mont_county_2018_wards, "./data/output_data/leaflet_maps/planning_vars_mont_county_2018_wards.html", selfcontained = T)

htmlwidgets::saveWidget(tp_2019_wards, "./data/output_data/leaflet_maps/planning_vars_tp_2019_wards.html", selfcontained = T)

htmlwidgets::saveWidget(tp_2018_wards, "./data/output_data/leaflet_maps/planning_vars_tp_2018_wards.html", selfcontained = T)


# leaflet(bg_md_test) %>%
#   addTiles(options = tileOptions(opacity = 0.5)) %>%
#   addPolygons(color = "transparent",
#               weight = 1, 
#               fill = T, 
#               opacity = 1,
#               fillOpacity = 0.4, 
#               fillColor = ~ pal_numeric(var = "ALUCE003", df = st_drop_geometry(bg_md_test), colors = "Blues")(st_drop_geometry(bg_md_test)[["ALUCE003"]]), 
#               group = "Black", 
#               highlight = highlightOptions(stroke = TRUE,
#                                            weight = 3.5, 
#                                            fillOpacity = 0.6, 
#                                            color = "#555EE7", 
#                                            opacity = 1, 
#                                            bringToFront = TRUE),
#               label = map(labels_funct(st_drop_geometry(bg_md_test)), htmltools::HTML))
# 
# dc_md_map <- map_funct() %>%
#   add_city()
# 

####### race data map

race_data_md <- read.csv("./data/nhgis/race_vars/nhgis0012_ds244_20195_2019_blck_grp.csv")

race_data_md_rename <- race_data_md %>%
  rename_all(~ bg_rename(.x)) %>%
  rename_all(tolower)

race_data_md_sub <- race_data_md_rename %>%
  select(gisjoin, state, county, countya, tracta, blkgrpa, name_e, race_tot, race_black, race_aian, race_asian, race_nhpi, race_other, race_two, hispr_tot, hispr_nothisp_white, hispr_hisplat) %>%
  mutate(race_black_pct = pct_round(race_black, race_tot),
         race_aian_pct = pct_round(race_aian, race_tot),
         race_asian_pct = pct_round(race_asian, race_tot),
         race_nhpi_pct = pct_round(race_nhpi, race_tot),
         race_other_pct = pct_round(race_other, race_tot),
         race_two_pct = pct_round(race_two, race_tot),
         hispr_nothisp_white_pct = pct_round(hispr_nothisp_white, hispr_tot),
         hispr_hisplat_pct = pct_round(hispr_hisplat, hispr_tot))

bg_shapes_racedata <- bg_shapes %>%
  left_join(race_data_md_sub)

bg_shapes %>%
  anti_join(race_data_md_sub)

bg_shapes_racedata_mont <- bg_shapes_racedata %>%
  filter(grepl("Montgomery", county))

bg_shapes_racedata_tp <- bg_shapes_racedata %>%
  filter(name_e %in% bgs_2019_match)


map_funct_racedata <- function(basemap = bg_shapes_racedata_mont, 
                               group_cols = c("White (non-Hispanic) residents: total",
                                              "Black residents: total",
                                              "Hispanic residents: total",
                                              "Asian residents: total",
                                              "'Other' residents: total",
                                              "White (non-Hispanic) residents: percent",
                                              "Black residents: percent",
                                              "Hispanic residents: percent",
                                              "Asian residents: percent",
                                              "'Other' residents: percent"),
                               collapsed_val = FALSE){
  
  as_data_file <- st_drop_geometry(basemap)
  # browser()
  label_result <- labels_funct(as_data_file)
  
  group_cols <- group_cols
  
  map <- leaflet(basemap) %>%
    addTiles(options = tileOptions(opacity = 0.5)) %>%
    addpoly_legend(as_data_file, pal_numeric("hispr_nothisp_white", df = as_data_file, "Reds"), "hispr_nothisp_white", group_select = "White (non-Hispanic) residents: total", "Total", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_black", df = as_data_file, "Blues"), "race_black", group_select = "Black residents: total", "Total", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("hispr_hisplat", df = as_data_file, "Greens"), "hispr_hisplat", group_select = "Hispanic residents: total", "Total", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_asian", df = as_data_file, "Oranges"), "race_asian", group_select = "Asian residents: total", "Total", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_other", df = as_data_file, "Greys"), "race_other", group_select = "'Other' residents: total", "Total", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("hispr_nothisp_white_pct", df = as_data_file, "Reds"), "hispr_nothisp_white_pct", group_select = "White (non-Hispanic) residents: percent", "Percent", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_black_pct", df = as_data_file, "Blues"), "race_black_pct", group_select = "Black residents: percent", "Percent", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("hispr_hisplat_pct", df = as_data_file, "Greens"), "hispr_hisplat_pct", group_select = "Hispanic residents: percent", "Percent", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_asian_pct", df = as_data_file, "Greys"), "race_asian_pct", group_select = "Asian residents: percent", "Percent", labels_select = label_result) %>%
    addpoly_legend(as_data_file, pal_numeric("race_other_pct", df = as_data_file, "Oranges"), "race_other_pct", group_select = "'Other' residents: percent", "Percent", labels_select = label_result) %>%
    addLayersControl(overlayGroups = group_cols, 
                     options = layersControlOptions(collapsed = collapsed_val)) %>%
    hideGroup(group_cols[-1])
  
  return(map) 
}

st_write(bg_shapes_racedata_tp,"./data/output_data/shapefiles/bg_shapes_racedata_tp.geojson", delete_dsn = T)


map_racedata_tp_2019 <- map_funct_racedata(bg_shapes_racedata_tp) %>%
  add_wards()

htmlwidgets::saveWidget(map_racedata_tp_2019, "./data/output_data/leaflet_maps/map_racedata_tp_2019.html", selfcontained = T)
