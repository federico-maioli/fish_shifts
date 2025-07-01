# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(tidylog)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rfishbase)
library(tidyterra)

# load survey data --------------------------------------------------------
fishglob_url <- "https://github.com/AquaAuma/FishGlob_data/raw/d71dfa03c2912b4e9d9cd10412ae2af52ba56ae5/outputs/Compiled_data/FishGlob_public_std_clean.RData"
options(timeout = 300)
load(url(fishglob_url))

# remove excluded surveys -------------------------------------------------
excl_surveys <- c('GSL-N','GSL-S','ROCKALL','AI','DFO-SOG','WCTRI','SEUS','FR-CGFS','SP-PORC')
data <- data %>% 
  filter(!survey %in% excl_surveys) %>% 
  mutate(survey = case_when(
    survey %in% c("DFO-HS", "DFO-QCS", "DFO-WCHG", "DFO-WCVI") ~ "BC",
    TRUE ~ survey
  ))

# remove bad hauls --------------------------------------------------------
goa_del <- data %>% filter(survey == 'GOA' & year < 2003) %>% pull(haul_id)
gmex_del <- data %>% filter(survey == 'GMEX' & year %in% c(1987, 2020)) %>% pull(haul_id)

neus_del <- data %>%
  filter((survey == "NEUS" & year < 2009 & (haul_dur < 0.42 | haul_dur > 0.58)) |
           (survey == "NEUS" & year >= 2009 & (haul_dur < 0.25 | haul_dur > 0.42)) & (year < 1968 | year > 2019)) %>%
  pull(haul_id)

bits_del <- data %>% filter(survey == 'BITS' & year <= 1998) %>% pull(haul_id)
nor_bts_del <- c(
  data %>% filter(survey == 'Nor-BTS' & year < 1989) %>% pull(haul_id),
  data %>% filter(survey == 'Nor-BTS' & latitude < 68) %>% pull(haul_id)
)
evhoe_del <- data %>% filter(survey == 'EVHOE' & year == 2017) %>% pull(haul_id)
nigfs_del <- data %>% filter(survey == 'NIGFS' & year < 2009) %>% pull(haul_id)
ns_ibts_del <- data %>% filter(survey == 'NS-IBTS' & year < 1980) %>% pull(haul_id)
swc_ibts_del <- data %>% filter(survey == 'SWC-IBTS' & year < 1990) %>% pull(haul_id)
pt_ibts_del <- data %>% filter(survey == 'PT-IBTS' & year %in% c(2002, 2018)) %>% pull(haul_id)

haul_ids_del <- c(goa_del, gmex_del, neus_del, bits_del, nor_bts_del,
                  evhoe_del, nigfs_del, ns_ibts_del, swc_ibts_del, pt_ibts_del) %>% unique()

data <- data %>% filter(!haul_id %in% haul_ids_del)

# fix missing or zero catch weights ---------------------------------------
data <- data %>%
  mutate(wgt_cpua = if_else(survey == "NEUS", wgt / area_swept, wgt_cpua)) %>%
  filter(wgt_cpua != 0)

# remove pelagic families -------------------------------------------------
pelagic_families <- c("Clupeidae", "Osmeridae", "Exocoetidae", "Atherinidae",
                      "Engraulidae", "Hemiramphidae", "Inermiidae", "Belonidae",
                      "Scomberesocidae", "Echeneidae", "Carangidae", "Bramidae",
                      "Scombridae", "Centrolophidae", "Istiophoridae", "Ammodytidae")

data <- data %>% filter(!family %in% pelagic_families)

# remove duplicates -------------------------------------------------------
group_by_cols <- c("survey", "source", "timestamp", "haul_id", "country", "sub_area",
                   "continent", "stat_rec", "station", "stratum", "year", "month", "day",
                   "quarter", "latitude", "longitude", "haul_dur", "area_swept", "gear",
                   "sbt", "sst", "depth", "accepted_name")

data <- data %>%
  group_by(across(all_of(group_by_cols))) %>%
  summarize(
    wgt_cpua = sum(wgt_cpua, na.rm = TRUE),
    wgt = sum(wgt, na.rm = TRUE),
    .groups = "drop"
  )

# add region info ---------------------------------------------------------
data$month <- as.numeric(data$month)

data <- data %>%
  mutate(region = case_when(
    survey %in% c('GOA','BC') ~ "Gulf of Alaska",
    survey %in% c('WCANN') ~ "California Current",
    survey %in% c('BITS') ~ "Baltic",
    survey %in% c('NS-IBTS') ~ "North Sea",
    survey %in% c('Nor-BTS') ~ "Barents Sea",
    survey %in% c("EBS") ~ "East Bering Sea",
    survey %in% c("GMEX") ~ "Gulf of Mexico",
    survey %in% c("EVHOE","IE-IGFS","NIGFS","SWC-IBTS") ~ "Celtic-Biscay Shelf",
    survey %in% c("SP-NORTH") ~ "North Iberian Coast",
    survey %in% c("SCS","NEUS") ~ "Northeast U.S. and Scotian Shelf",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region)) %>%
  mutate(region_short = case_when(
    region == "Gulf of Alaska" ~ "GOA",
    region == "California Current" ~ "COW",
    region == "Baltic" ~ "BAL",
    region == "North Sea" ~ "NS",
    region == "Barents Sea" ~ "BS",
    region == "East Bering Sea" ~ "EBS",
    region == "Gulf of Mexico" ~ "GMX",
    region == "Celtic-Biscay Shelf" ~ "CBS",
    region == "North Iberian Coast" ~ "NIC",
    region == "Northeast U.S. and Scotian Shelf" ~ "NEUS",
    TRUE ~ NA_character_
  ))

# reorder columns ---------------------------------------------------------
data <- data %>%
  select(survey, region, region_short, source, timestamp, haul_id, country,
         sub_area, continent, stat_rec, year, month, day, quarter, latitude,
         longitude, haul_dur, area_swept, gear, sbt, sst, depth, accepted_name,
         wgt_cpua, wgt)

# complete dataset with all haul-species combos ---------------------------
data_complete <- data %>%
  select(-wgt) %>%
  group_by(survey) %>%
  complete(haul_id, accepted_name, fill = list(wgt_cpua = 0)) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  filter(year >= 1994)

# filter for common species -----------------------------------------------
survey_threshold <- 15
region_threshold <- 5

haul_summary <- data_complete %>%
  group_by(year, survey, region) %>%
  summarize(total_hauls = n_distinct(haul_id), .groups = "drop")

species_presence <- data_complete %>%
  group_by(accepted_name, haul_id, year, survey, region) %>%
  summarize(total_wgt_cpua = sum(wgt_cpua), .groups = "drop") %>%
  mutate(occurrence = as.integer(total_wgt_cpua > 0)) %>%
  group_by(year, accepted_name, survey, region) %>%
  summarize(hauls_with_species = sum(occurrence), .groups = "drop") %>%
  left_join(haul_summary, by = c("year", "survey", "region")) %>%
  mutate(presence_percent_survey = hauls_with_species / total_hauls * 100) %>%
  group_by(accepted_name, survey, region) %>%
  summarize(mean_presence_survey = mean(presence_percent_survey), .groups = "drop") %>%
  filter(mean_presence_survey >= survey_threshold) %>%
  group_by(accepted_name, region) %>%
  summarize(mean_presence_region = mean(mean_presence_survey), .groups = "drop") %>%
  filter(mean_presence_region >= region_threshold)

selected_species <- species_presence %>%
  filter(str_detect(accepted_name, "\\s"))

filtered_data <- data_complete %>%
  inner_join(selected_species %>% select(-mean_presence_region),
             by = c("region", "accepted_name"))

# add depth if missing ----------------------------------------------------
depth_raster <- terra::rast(here("data/environment/depth/gebco/GEBCO_2023_sub_ice_topo.nc"))

filtered_data$depth_gebco <- terra::extract(depth_raster$elevation,
                                            filtered_data %>% select(longitude, latitude))$elevation %>%
  abs()

filtered_data <- filtered_data %>%
  mutate(depth = coalesce(depth, depth_gebco)) %>%
  select(-depth_gebco)

# final selection and save -----------------------------------------------
data <- filtered_data %>%
  select(survey, region, region_short, haul_id, country, continent,
         year, month, day, quarter, latitude, longitude, haul_dur,
         area_swept, gear, sbt, sst, depth, accepted_name, wgt_cpua)

write_rds(data, here("data/trawl_surveys/fishglob_clean.rds"))