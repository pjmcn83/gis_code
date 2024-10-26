# Packages
library(sf)
library(dplyr)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(stringr)
library(tmap)

## Read the data
# read gender data and prepare columns
gii_raw <- readxl::read_xlsx(here::here("data", "GenderInequalityIndex2010_2019.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::select(country_iso_code,country, year, value)

# read geometries and prepare columns
world <- sf::st_read("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/World_Countries_(Generalized)/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
  janitor::clean_names() %>%
  dplyr::select(country, iso, geometry)

## Prepare gender data
# create data frame and filter to only countries (exclude regions)  
gii_filtered <- data.frame(gii_raw) %>%
  filter(str_detect(gii_raw$country_iso_code,"ZZ",TRUE))

# pivot gii data
gii_wider <- gii_filtered %>%
  pivot_wider(names_from = "year",values_from = "value")

#rename value columns for ease of use
gii_wider <- gii_wider %>% dplyr::rename(value2010 = "2010", value2019 = "2019")

# create new column showing difference between 2019 and 2010 values
gii_new <- gii_wider %>%
  mutate(., difference=(value2019-value2010))

# spatialise the data by joining to world file
world_gii_spatial <- world %>%
  left_join(., 
            gii_new,
            by = c("country"="country"))

# view the data on a map
tmap_mode("plot")
tm_shape(world_gii_spatial) + 
  tm_polygons("difference", 
              style="jenks",
              palette="Greens",
              midpoint=NA,
              title="GII difference",
              alpha = 0.5) + 
  tm_basemap(server = "OpenStreetMap") +
  tm_layout(title = "Difference between Gender Inequlaity Index between 2010 and 2019")

# write data csv and shapefile
write.csv(gii_new, file = here::here("output", "GenderInequalityIndexWorld.csv"))
st_write(world_gii_spatial, "output/GenderInequalityIndexMap.shp", append=FALSE)


