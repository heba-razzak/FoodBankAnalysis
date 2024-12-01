SecondHarvest
================

## Load required libraries

``` r
library(dplyr)      # Data manipulation
library(pdftools)   # Read pdf file
library(stringr)    # Text manipulation
library(tidycensus) # US census data
library(tigris)     # Counties map data
library(sf)         # Spatial data manipulation
library(leaflet)    # Interactive maps
library(htmlwidgets)# Saving interactive maps
library(htmltools)  # HTML Tools
library(ggmap)      # geocode addresses
library(units)      # convert units
```

## FIPS State and County code for Orange County, CA

``` r
# Extract FIPS codes for Orange County, CA
orangecounty <- tidycensus::fips_codes %>% 
  filter(state == "CA", county == "Orange County") %>% 
  select(state_code, county_code)

# Extract state and county codes
ca_code <- orangecounty$state_code
oc_code <- orangecounty$county_code
```

## Extract Partner Names and Addresses from PDF

``` r
# Read the partners PDF file
partners_file <- file.path(data_dir, "SHFB_Partners.pdf")
text <- pdf_text(partners_file)

# Clean text
text_lines <- c()
for (page in text) {
  col1 <- c()
  col2 <- c()
  page_lines = unlist(strsplit(page, "\n"))
  # Split columns by position of bullet points
  bullet_positions <- gregexpr("•", page_lines)
  split_position <- max(unlist(bullet_positions))
  for (line in page_lines) {
    col1 <- c(col1, substr(line, 1, split_position - 1))
    col2 <- c(col2, substr(line, split_position, nchar(line)))
  }
  text_lines <- c(text_lines, col1, col2)
}
# Remove lines: empty lines or start with white space, bullets or "CITY OF"
text_lines <- text_lines[!grepl("^\\s|^•|^CITY OF|^$", text_lines)]
text_df <- data.frame(line = text_lines)
text_df <- text_df %>% 
  # Remove specific lines manually
  filter(!(line %in% c("ay","Anaheim Independencia KC"))) %>%
  # Correct specific address manually
  mutate(line = ifelse(str_trim(line)=="Plumosa Dr., Yorba Linda 92886",
                       "4672 Plumosa Dr., Yorba Linda 92886",
                       str_trim(line)),
         # Classify lines as "Address" or "Name"
         type = ifelse(grepl("^[0-9]", line), "Address", "Name"),
         # Combine consecutive address lines
         line = ifelse(type == "Address" & lead(type) == "Address" & !is.na(lead(type)),
                       paste(line, lead(line), sep = " "),
                       line)) %>%
  # Exclude redundant address rows (combined address)
  filter(lag(type) != type | row_number() == 1)

partners_df <- data.frame(Name = text_df$line[text_df$type == "Name"],
                          Address = text_df$line[text_df$type == "Address"])
```

## Geocode Partner Addresses (Get longitude and latitude)

``` r
# Get partners longitude and latitude from address
if (!file.exists(file.path(data_dir, "partners_locations.csv"))) {
  partners_df <- partners_df %>%
    rowwise() %>%
    mutate(
      geocode_result = suppressMessages(geocode(Address)),
      lon = geocode_result$lon,
      lat = geocode_result$lat
    ) %>%
    ungroup() %>%
    select(-geocode_result)
  
  # Save as CSV
  write.csv(partners_df, file.path(data_dir, "partners_locations.csv"), row.names = FALSE)
}
```

## Extract TEFAP Names and Addresses from PDF

``` r
# Read the TEFAP PDF file
pdf_file <- file.path(data_dir, "TEFAP.pdf")
text <- pdf_text(pdf_file)

# Clean text
text_lines <- c()
for (page in text) {
  col1 <- c()
  col2 <- c()
  page_lines = unlist(strsplit(page, "\n"))
  # Split columns by position of bullet points
  bullet_positions <- gregexpr("•", page_lines)
  split_position <- max(unlist(bullet_positions))
  for (line in page_lines) {
    col1 <- c(col1, substr(line, 1, split_position - 1))
    col2 <- c(col2, substr(line, split_position, nchar(line)))
  }
  text_lines <- c(text_lines, col1, col2)
}
# Remove lines: empty lines or start with white space, bullets or "CITY OF"
text_lines <- text_lines[!grepl("^\\s|^•|^CITY OF|^$", text_lines)]
text_df <- data.frame(line = text_lines)
text_df <- text_df %>% 
  mutate(# Classify lines as "Address" or "Name"
    type = ifelse(grepl("^[0-9]", line), "Address", "Name"),
    # Combine consecutive address lines
    line = ifelse(type == "Address" & lead(type) == "Address" & !is.na(lead(type)),
                  paste(line, lead(line), sep = " "),
                  line)) %>%
  # Exclude redundant address rows (combined address)
  filter(lag(type) != type | row_number() == 1)

tefap_df <- data.frame(Name = text_df$line[text_df$type == "Name"],
                       Address = text_df$line[text_df$type == "Address"])
```

## Geocode TEFAP Addresses (Get longitude and latitude)

``` r
# Get tefap longitude and latitude from address
if (!file.exists(file.path(data_dir, "tefap_locations.csv"))) {
  tefap_df <- tefap_df %>%
    rowwise() %>%
    mutate(
      geocode_result = suppressMessages(geocode(Address)),
      lon = geocode_result$lon,
      lat = geocode_result$lat
    ) %>%
    ungroup() %>%
    select(-geocode_result)
  
  # Save as CSV
  write.csv(tefap_df, file.path(data_dir, "tefap_locations.csv"), row.names = FALSE)
}
```

## Download Orange County Shapefiles

### OC Cities, Tracts, Block Groups

``` r
# Download shapefiles and save them as layers in a GeoPackage
if (!file.exists(file.path(data_dir, "orange_county.gpkg"))) {
  crs <- 4326
  oc_county <- counties(ca_code) %>% st_transform(crs) %>% filter(COUNTYFP == oc_code) %>% select(geometry)
  ca_cities <- places(ca_code) %>% st_transform(crs) %>% rename(City = NAME) %>% select(City, GEOID)
  oc_cities <- st_intersection(ca_cities, oc_county)
  oc_tracts <- tracts(ca_code, oc_code) %>% st_transform(crs) %>%
    filter(TRACTCE != "990100") %>% # (water tract)
    rename(CensusTract = NAME) %>% select(CensusTract, GEOID)
  oc_blockgroups <- block_groups(ca_code, oc_code) %>% st_transform(crs) %>% 
    filter(TRACTCE != "990100") %>% # (water tract)
    rename(BlockGroup = BLKGRPCE) %>% select(BlockGroup, GEOID)
  
  # Add tract info to block groups
  tract_info <- oc_tracts %>% rename(tract_GEOID = GEOID) %>% select(CensusTract, tract_GEOID)
  oc_blockgroups <- st_join(oc_blockgroups, tract_info, join = st_within)
  
  # Save files
  st_write(oc_cities, dsn = file.path(data_dir, "orange_county.gpkg"), layer = "oc_cities")
  st_write(oc_tracts, dsn = file.path(data_dir, "orange_county.gpkg"), layer = "oc_tracts")
  st_write(oc_blockgroups, dsn = file.path(data_dir, "orange_county.gpkg"), layer = "oc_blockgroups")
}
```

## Convert Partner Locations to Shapefile

``` r
# Save geojson file
if (!file.exists(file.path(data_dir, "partners.geojson"))) {
  # Load and clean partner locations
  partners_df <- read.csv(file.path(data_dir, "partners_locations.csv")) %>%
    mutate(Name = str_trim(Name), Address = str_trim(Address)) %>% 
    distinct() %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    arrange(geometry, grepl("KC", Name, ignore.case = TRUE)) %>%
    group_by(geometry) %>% slice(1) %>% ungroup()
  
  # Load and clean TEFAP locations
  tefap_sf <- read.csv(file.path(data_dir, "tefap_locations.csv")) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    distinct(geometry, .keep_all = TRUE)
  
  # Add TEFAP column to partners shapefile
  partners_sf <- partners_sf %>%
    mutate(tefap = ifelse(lengths(st_equals(geometry, tefap_sf$geometry)) > 0, 1, 0))
  
  # Add block group and city info
  partners_sf <- st_join(partners_sf, oc_blockgroups, join = st_within) %>% rename(bg_GEOID = GEOID)
  partners_sf <- st_join(partners_sf, oc_cities, join = st_within)
  
  # Save file
  st_write(partners_sf, dsn = file.path(data_dir, "partners.geojson"))
}
```

## Add geographic areas and distance to nearest partner

``` r
# For cities, tracts, block groups
geometries <- list(cities = oc_cities, tracts = oc_tracts, blockgroups = oc_blockgroups)

# Transform partners' coordinates to EPSG:3857 (for distance calculation)
partners_sf <- partners_sf %>% st_transform(3857)

# Get minimum distance to partners (mi) and area (mi^2)
for (g in c("cities", "tracts", "blockgroups")) {
  geometries[[g]] <- geometries[[g]] %>% st_transform(3857)
  calc <- geometries[[g]] %>%
    mutate(area_mi2 = as.numeric(set_units(st_area(geom), "mi^2")),
           partner = apply(st_contains(geometries[[g]], partners_sf, sparse = FALSE), 1, any)) %>%
    st_centroid() %>% 
    mutate(min_dist = apply(st_distance(., partners_sf), 1, min),
           min_dist = ifelse(partner, 0, min_dist),
           min_dist = set_units(min_dist, "m"),
           min_dist_mi = as.numeric(set_units(min_dist, "miles"))) %>% 
    st_drop_geometry() %>% select(min_dist_mi, area_mi2, GEOID)
  
  geometries[[g]] <- geometries[[g]] %>%
    left_join(calc, by = "GEOID") 
}
```

    ## Warning: st_centroid assumes attributes are constant over geometries

    ## Warning: st_centroid assumes attributes are constant over geometries

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
# Assign results back to the original variables
oc_cities <- geometries$cities
oc_tracts <- geometries$tracts
oc_blockgroups <- geometries$blockgroups
```

## Selected Census variables

``` r
vars <- load_variables(year = yr, dataset = c("acs5"), cache = TRUE)

select_vars <- vars %>% filter(
  name %in% c("B22008_001", "B19013_001", "B17017_002", "B22010_002", "B11001_001")
) %>%
  mutate(variable = case_when(
    name == "B22008_001" ~ "median_income_tract",
    name == "B19013_001" ~ "median_income",
    name == "B22010_002" ~ "foodstamp_households",
    name == "B17017_002" ~ "poverty_households",
    name == "B11001_001" ~ "n_households",
    TRUE ~ "")) %>%
  select(name, geography, variable, label, concept) %>% 
  arrange(variable, geography)

knitr::kable(data.frame(select_vars),
             row.names = FALSE,
             format = "markdown")
```

| name       | geography   | variable             | label                                                                                                 | concept                                                                                                                                 |
|:-----------|:------------|:---------------------|:------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------|
| B22010_002 | block group | foodstamp_households | Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months:                          | RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY DISABILITY STATUS FOR HOUSEHOLDS                                                   |
| B19013_001 | block group | median_income        | Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)          | MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)                                                      |
| B22008_001 | tract       | median_income_tract  | Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)–!!Total: | MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS) BY RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS |
| B11001_001 | block group | n_households         | Estimate!!Total:                                                                                      | HOUSEHOLD TYPE (INCLUDING LIVING ALONE)                                                                                                 |
| B17017_002 | block group | poverty_households   | Estimate!!Total:!!Income in the past 12 months below poverty level:                                   | POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER                                                            |

## Download ACS data for select variables

``` r
# Loop through the selected variables and get ACS data
for (i in 1:nrow(select_vars)) {
  name <- select_vars$name[i]
  v <- select_vars$variable[i]
  geography <- select_vars$geography[i]
  
  # Fetch the ACS data for the specific variable
  data <- get_acs(geography = geography,
                  variables = name,
                  state = ca_code,
                  county = oc_code,
                  year = 2021,
                  survey = "acs5",
                  cache_table = TRUE) %>%
    select(GEOID, estimate) %>%
    rename(!!v := estimate)
  
  # Join based on the geography
  if (geography == "block group") {
    oc_blockgroups <- left_join(oc_blockgroups, data, by = "GEOID")
  } else if (geography == "tract") {
    oc_blockgroups <- left_join(oc_blockgroups, data, by = c("tract_GEOID" = "GEOID"))
    oc_tracts <- left_join(oc_tracts, data, by = "GEOID")
  }
}
```

## Add Number of Partners and Foodstamp Households by 5-Mile Radius (Block Groups)

``` r
# Block Group centroids and buffers
bg_centroids <- oc_blockgroups %>% st_centroid()
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
bg_buffers <- bg_centroids %>% st_buffer(dist = set_units(5, "mi")) %>%
  rename(buffer_GEOID = GEOID) %>% select(buffer_GEOID)

# Number of partners within 5 miles
partners_5mi <- apply(st_intersects(bg_buffers, partners_sf, sparse = FALSE), 1, sum)

# Buffer number of foodstamp households, use proportion of block group intersecting buffer
buffer_nfoodstamps <- st_intersection(bg_buffers, oc_blockgroups) %>%
  mutate(intersection_area = as.numeric(set_units(st_area(geom), "mi^2")),
         area_proportion = intersection_area / area_mi2,
         foodstamps_prop = foodstamp_households * area_proportion) %>%
  group_by(buffer_GEOID) %>%
  summarise(foodstamp_households_5mi = sum(foodstamps_prop, na.rm = TRUE)) %>%
  st_drop_geometry()
```

    ## Warning: attribute variables are assumed to be spatially constant throughout
    ## all geometries

``` r
# Combine results back into oc_blockgroups
oc_blockgroups <- oc_blockgroups %>%
  mutate(partners_5mi = partners_5mi) %>%
  left_join(buffer_nfoodstamps, by = c("GEOID" = "buffer_GEOID"))
```

## Add Number of Partners and Foodstamp Households by 5-Mile Radius (Census Tract)

``` r
# Census Tract centroids and buffers
ct_centroids <- oc_tracts %>% st_centroid()
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
ct_buffers <- ct_centroids %>% st_buffer(dist = set_units(5, "mi")) %>%
  rename(buffer_GEOID = GEOID) %>% select(buffer_GEOID)

# Get tract num of foodstamp households from block groups
tract_foodstamps <- oc_blockgroups %>% 
  group_by(tract_GEOID) %>% 
  summarise(foodstamp_households = sum(foodstamp_households)) %>% 
  st_drop_geometry()
oc_tracts <- oc_tracts %>% left_join(tract_foodstamps, by = c("GEOID" = "tract_GEOID"))

# Number of partners within 5 miles
partners_5mi <- apply(st_intersects(ct_buffers, partners_sf, sparse = FALSE), 1, sum)

# Buffer number of foodstamp households, use proportion of tract intersecting buffer
buffer_nfoodstamps <- st_intersection(ct_buffers, oc_tracts) %>%
  mutate(intersection_area = as.numeric(set_units(st_area(geom), "mi^2")),
         area_proportion = intersection_area / area_mi2,
         foodstamps_prop = foodstamp_households * area_proportion) %>%
  group_by(buffer_GEOID) %>%
  summarise(foodstamp_households_5mi = sum(foodstamps_prop, na.rm = TRUE))  %>% 
  st_drop_geometry()
```

    ## Warning: attribute variables are assumed to be spatially constant throughout
    ## all geometries

``` r
# Combine results back into oc_tracts
oc_tracts <- oc_tracts %>%
  mutate(partners_5mi = partners_5mi) %>%
  left_join(buffer_nfoodstamps, by = c("GEOID" = "buffer_GEOID"))
```

``` r
# Save tracts and block groups as GeoJSON
if (!file.exists(file.path(data_dir, "blockgroups.geojson"))) {
  st_write(oc_blockgroups, dsn = file.path(data_dir, "blockgroups.geojson"))
}
if (!file.exists(file.path(data_dir, "tracts.geojson"))) {
  st_write(oc_tracts, dsn = file.path(data_dir, "tracts.geojson"))
}
if (!file.exists(file.path(data_dir, "cities.geojson"))) {
  st_write(oc_cities, dsn = file.path(data_dir, "cities.geojson"))
}
```
