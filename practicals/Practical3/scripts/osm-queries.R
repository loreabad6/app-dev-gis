# Don't get confused about the use of <- or = .
# They are both used for assignment and although they are not entirely the same
# in most cases using = is fine. 
# This is just my coding style and reduces typing so feel free to use it too!

# Also note that using the `osmdata` package is a call to the OSM overpass API
# Doing big queries will result in timeouts but also calling the queries 
# over and over will probably get your API locked for some time
# I recommend doing bigger queries with the `osmextract` package

# Call libraries
library(here)
library(osmdata)
library(sf)
library(tidyverse)

# Set the directory, note I use the here package, which is cleaner than setwd()
dir_data = here("data")

# get the bounding box of Salzburg
bb = getbb("Salzburg, Austria", format_out = "sf_polygon")
# Since Salzburg is a city and a province I got two features
# from the previous line so I computed the area to get the biggest
# polygon as the province
bb = bb |> 
  mutate(area = st_area(geometry)) |> 
  filter(area == max(area, na.rm = TRUE))

# I create a 10km buffer around it since I want to 
# get huts also in the surrounding region
sbg_buffer = st_bbox(st_buffer(bb, 10000))

# Huts ----
# I search for alpine hut and wilderness hut as values for the tourism key
# the last line transforms the output into sf objects
huts_query = opq(bbox = sbg_buffer) |> 
  add_osm_features(features = list (
    "tourism" = "alpine_hut",
    "tourism" = "wilderness_hut"
  )) |> 
  osmdata_sf()

# The output has points, lines and polygons. 
# I am interested in the polygons/multipolygons which is the usual way to map
# buildings in OSM
# I computed the centroid of the polygons to work with points later
hpoly = huts_query$osm_polygons |> 
  filter(!is.na(name)) |> 
  mutate(geometry = st_centroid(geometry))

hmpoly = huts_query$osm_multipolygons |> 
  filter(!is.na(name)) |> 
  mutate(geometry = st_centroid(geometry))

# And I combine both sers
huts = bind_rows(hpoly, hmpoly)

# Regions ----
# I also provided you with a regions dataset, which was also extracted from OSM
# I obtained the boundary value of the key type
# and then refined the search by only getting admin level 6
boundaries = opq(bbox = sbg_buffer) |> 
  add_osm_feature(key = "type", value = "boundary") |> 
  add_osm_feature(key = "admin_level", value = "6") |> 
  osmdata_sf()

# Finally I wrangled the data to create a new column 
# with the country for each region
regions = boundaries$osm_multipolygons |> 
  transmute(
    region = name,
    country = case_when(
      str_detect(region, "Bolzano") ~ "Italy",
      str_detect(region, "Salzburg") ~ "Austria",
      str_detect(region, "Wels") ~ "Austria",
      str_detect(region, "Rosenheim") ~ "Germany",
      str_detect(region, "Bezirk") ~ "Austria",
      str_detect(region, "Landkreis") ~ "Germany",
      TRUE ~ NA_character_
      
    )
  )

# and saved the results to a geopackage file
write_sf(huts, dsn = here(dir_data, "p5_huts.gpkg"))
write_sf(regions, dsn = here(dir_data, "p5_regions.gpkg"))
