library(sf)
library(leafletwrappers)
library(spdep)
library(leaflet)
library(glue)
library(tidyverse)
library(tpfuncts)

# read in cdbg
# from https://opendata-mcgov-gis.hub.arcgis.com/datasets/mcgov-gis::cdbg-eligible-block-groups/explore?location=38.988876%2C-76.997865%2C14.35
cdbg_elig <- st_read("./data/cdbg/CDBG-eligible_block_groups.geojson") %>% mutate(Lowmod_pct = Lowmod_pct* 100)


pal_lowmod <- leafletwrappers::pal_numeric(var = "Lowmod_pct", colors = "Blues", df = st_drop_geometry(cdbg_elig))


labs <- map(leafletwrappers::label_standardize(st_drop_geometry(cdbg_elig), label_text = "Tract {Tract}, Block-Group {BLKGRP}
                              <p></p>Low-income residents: {as.numeric(Low)  %>% commafy}
                              <p></p>Low-moderate income residents: {as.numeric(Lowmod) %>% commafy}
                              <p></p>Percent low-moderate income residents: {round(Lowmod_pct, 2)}%"),
            htmltools::HTML)

cdbg_map <- leaflet(cdbg_elig) %>%
  addProviderTiles(providers$CartoDB) %>%
  # addTiles %>%
  addpoly_legend(df_select = st_drop_geometry(cdbg_elig), pal_funct_select  = pal_lowmod, variable_select = "Lowmod_pct", group_select  = "pctlowmod", labels_select = labs,title_select = "CDBG-Eligible Block Groups<br>Percent low-moderate income", .pollinecolor = "black", .polweight = 0.5, .polopacity = 0.5, .polfillopacity = 0.6) %>%
  add_wards_new()

htmlwidgets::saveWidget(cdbg_map, file = "./data/output_data/leaflet_maps/cdbg_map.html")

# addpoly_legend()

# add_wards()
