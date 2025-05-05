# Reading and cleaning data

# Libraries
library(tidyverse)
library(readxl)
library(here)
library(sf)
library(renv)

# Read names of sheets
sheet_list <-
  readxl::excel_sheets(here::here("raw_data",
                                  "Productivity_bark_beetles.xlsx"))

# Read all sheets into a list
og_beetles <-
  purrr::map(sheet_list, ~readxl::read_excel(here::here("raw_data",
                                                        "Productivity_bark_beetles.xlsx"),
                                             sheet = .x)) %>%
  ## Add names
  purrr::set_names(sheet_list)

# Get sheets I want
## Specimens
specimens <-
  og_beetles$specimens
## Sites
sites <-
  og_beetles$sites
## Logs
logs <-
  og_beetles$logs

## UTM_update (updates on coordinates)
UTM_update <-
  og_beetles$UTM_update

# Clean specimens ---------------------------------------------------------
# Remove id_person and notes column
specimens <-
  specimens %>%
  dplyr::select(-id_person, -notes)

# Make sure there are no typos in the columns
## Subphylum
specimens %>%
  dplyr::select(Subphylum) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Subphylum)
### OK

## Order
specimens %>%
  dplyr::select(Order) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Order)
### Fix typos
specimens <-
  specimens %>%
  dplyr::mutate(Order = ifelse(Order == "Hemiptra",
                               "Hemiptera", Order))
### Check
specimens %>%
  dplyr::select(Order) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Order)
### OK

## Family
specimens %>%
  dplyr::select(Family) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Family) %>%
  dplyr::pull()
### Remove typo
specimens <-
  specimens %>%
  dplyr::mutate(Family = ifelse(Family == "ci",
                                NA, Family))
### One wrong family for a cucujidae larva
specimens <-
  specimens %>%
  dplyr::mutate(Family = ifelse(Species == "cucularva_A",
                                "Cucujidae", Family))

### Check
specimens %>%
  dplyr::select(Family) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Family) %>%
  dplyr::pull()
### OK

## Genus
specimens %>%
  dplyr::select(Genus) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Genus) %>%
  dplyr::pull()
### OK

## Species
specimens %>%
  dplyr::select(Species) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Species) %>%
  dplyr::pull()
### Remove typo
specimens <-
  specimens %>%
  dplyr::mutate(Species = ifelse(Species == "staph_ b",
                                "staph_b", Species))


# Make sure abundance has realistic values
range(specimens$Abundance)


# Make sure there are no weird log names
specimens %>%
  dplyr::select(log) %>%
  dplyr::distinct() %>%
  dplyr::anti_join(logs,
                   by = c("log" = "log"))
## Well it looks like TUN here is TUNN in the log tibble
## Modify the log tibble to keep the shorter name

# Clean logs --------------------------------------------------------------
# Modify TUNN to TUN, to match names with the specimen tibble
logs <-
  logs %>%
  dplyr::mutate(log = stringr::str_replace_all(string = log,
                                               pattern = "TUNN",
                                               replacement = "TUN"))

# Do the same in the UTM_update tibble
UTM_update <-
  UTM_update %>%
  dplyr::mutate(log = stringr::str_replace_all(string = log,
                                               pattern = "TUNN",
                                               replacement = "TUN"))

# Make sure there are no weird site names
logs %>%
  dplyr::select(site) %>%
  dplyr::distinct() %>%
  dplyr::anti_join(sites,
                   by = c("site" = "site"))
## OK, but we should still avoid empty spaces
logs <-
  logs %>%
  dplyr::mutate(site = stringr::str_replace_all(site,
                                                " ", "_"))
## Do the same in the UTM_update tibble
UTM_update <-
  UTM_update %>%
  dplyr::mutate(site = stringr::str_replace_all(site,
                                                " ", "_"))

# Replace UTM coordinates in logs with those in UTM_update
logs <-
  UTM_update %>% 
  dplyr::left_join(logs %>% select(-northing,-easting)) %>%
  dplyr::relocate(c(northing,easting,UTM_zone),
                  .after=site)

# Rename columns
## Vector of column names
name_it <- 
  c("log","site","northing","easting","UTM_zone","siteID",
    "dbh_live_cm","dbh_interim","dbh_cm_compiled2","attack_den","disk_circum","disk_dia",
    "age","mean_phloem_mm","growth_5avg_mm","growth_10avg_mm","log_length_cm","area_m2",
    "mpb_exit_holes_s1","mpb_exit_holes_s2","other_exit_holes_s1","other_exit_holes_s2",
    "bait","burn","mpb_m2")
## Replace in the tibble
names(logs) <- 
  name_it
## Drop UTM_zone
logs <- 
  logs %>% 
  dplyr::select(-UTM_zone)

# Convert coordinates from UTM to WGS 84
logs <-
  sf::st_as_sf(x = logs,
               coords = c("easting", "northing"),
               crs = "+proj=utm +zone=11") %>%
  #Projection transformation
  sf::st_transform(.,
                   crs = "+proj=longlat +datum=WGS84") %>%
  # Convert back to tibble
  tibble::as_tibble() %>%
  ## Clean coordinates column
  dplyr::mutate(geometry = as.character(geometry)) %>%
  dplyr::mutate(geometry = stringr::str_remove_all(geometry,
                                                   pattern = "c\\(|\\)")) %>%
  ## Separate at the comma space
  tidyr::separate(geometry,
                  into = c("longitude", "latitude"),
                  sep = ", ")

# Round location data to 4 digits, ~ 10 meter accuracy
logs <-
  logs %>% 
  dplyr::mutate(longitude = round(as.numeric(longitude),4)) %>%
  dplyr::mutate(latitude = round(as.numeric(latitude),4))


# Clean sites -------------------------------------------------------------
# Fill site names with _
sites <-
  sites %>%
  dplyr::mutate(site = stringr::str_replace_all(site,
                                                " ", "_"))

# Recalculate site mean_easting and mean_northing with updated coordinates
sites <-
  sites %>% 
  dplyr::select(-`Mean(easting)`,-`Mean(northing)`) %>% 
  dplyr::left_join(logs %>% 
                     dplyr::group_by(site) %>% 
                     dplyr::summarise(mean_longitude = mean(longitude),
                                      mean_latitude = mean(latitude)))


# Round location data to 4 digits, ~ 10 meter accuracy
sites <-
  sites %>% 
  dplyr::mutate(mean_longitude = round(as.numeric(mean_longitude),4)) %>%
  dplyr::mutate(mean_latitude = round(as.numeric(mean_latitude),4))

# Save clean data ---------------------------------------------------------
## Specimens
readr::write_csv(specimens,
                 here::here("clean_data",
                            "specimens.csv"))
## Logs
readr::write_csv(logs,
                 here::here("clean_data",
                            "logs.csv"))
## Sites
readr::write_csv(sites,
                 here::here("clean_data",
                            "sites.csv"))

