
# Clean space and load data for the app----
rm(list=ls())
library(tidyverse)
library(psych)
library(dplyr)
library(sf)
library(here)
library(plotly)
library(htmlwidgets)
library(shiny)
library(shinylive)
library(shiny)
library(httpuv)


# Import relevant data----
SPL_1823 <- read.csv(here("data","SPL_1823.csv"))
SPL_1823_geo <- st_read(here("data", "SPL_1823_location.geojson")) # location_id shapefiles and lat/long coordinates
SEA_map <- st_read(here("data/raw","04_SPL_Seattle_Map.geojson")) # base SEA neighborhood map

# Recode gender column: ----
SPL_1823 <- SPL_1823 %>%
  mutate(
    gender=recode(staying_gender,
                  "Female"="Fem",
                  "Feminine presenting"="Fem",
                  "Gender-non-conforming presenting" = "non_conforming",
                  "Male"="Masc",
                  "Masculine presenting"="Masc",
                  "Other_Unsure"="Unsure",
                  "Unsure"="Unsure"))


# Calculate proportion women or men by neighborhood (S_HOOD)----
## instead of doing a f:m ratio, calculate the proportion of total observed that are women/men
FM_prop_shood <- SPL_1823 %>%
  group_by(S_HOOD, gender, study_id) %>%
  dplyr::summarise(count= n()) %>%
  pivot_wider(
    id_cols = c(S_HOOD, study_id),
    names_from = gender,
    values_from = count,
    names_prefix = "Observed_"
  ) %>%
  rowwise() %>% #this is to make sure the total is calculated correctly in the next line
  dplyr::mutate(total = sum(c_across(Observed_Fem:Observed_non_conforming), na.rm=T)) %>% #calculate total observed, including observations where no one was observed!
  mutate_at(vars(Observed_Fem, Observed_Masc, Observed_non_conforming, Observed_Unsure), ~replace_na(., 0)) %>% #replace NAs with 0s so that the calculated proportions return 0 and not NA
  mutate(F_prop = Observed_Fem/total,
         M_prop = Observed_Masc/total) %>%
  select(S_HOOD, study_id, F_prop, M_prop, Observed_Fem, Observed_Masc, total)

FM_prop_shood
## note that the total is the sum of fem, masc, non_conforming, unsure, and empty observations (i.e., empty locations where no one was staying at all). this is why e.g., Bitter Lake in 2018 has 1 fem, 0 masc, and 16 total observations



# Calculate proportion women or men by observation site (location_id)----
## instead of doing a f:m ratio, calculate the proportion of total observed that are women/men
FM_prop_locid <- SPL_1823 %>%
  group_by(location_id, gender, study_id) %>%
  dplyr::summarise(count= n()) %>%
  pivot_wider(
    id_cols = c(location_id, study_id),
    names_from = gender,
    values_from = count,
    names_prefix = "Observed_"
  ) %>%
  rowwise() %>% #this is to make sure the total is calculated correctly in the next line
  dplyr::mutate(total = sum(c_across(Observed_Fem:Observed_non_conforming), na.rm=T)) %>% #calculate total observed
  mutate_at(vars(Observed_Fem, Observed_Masc, Observed_non_conforming, Observed_Unsure), ~replace_na(., 0)) %>% #replace NAs with 0s so that the calculated proportions return 0 and not NA
  mutate(F_prop = Observed_Fem/total,
         M_prop = Observed_Masc/total) %>%
  select(location_id, study_id, F_prop, M_prop, Observed_Fem, Observed_Masc, total)

# append S_HOOD back----
FM_prop_locid <- SPL_1823 %>%
  select(location_id, S_HOOD) %>%
  distinct() %>%
  right_join(FM_prop_locid, by = "location_id")

FM_prop_locid
# again, note that the total is the sum of fem, masc, non_conforming, unsure, and empty observations (i.e., empty locations where no one was staying at all). this is why e.g., BLV1 in 2018 has 1 fem, 0 masc, and 8 total observations

# Filter 2023 data ----
FM_prop_23_locid <- FM_prop_locid %>% filter(study_id == "2023_Seattle_Citywide")

# Join location_id coordinates with 2023 filtered data ----
FM_prop_23_locid_geo <- left_join(FM_prop_23_locid, SPL_1823_geo, by = "location_id")