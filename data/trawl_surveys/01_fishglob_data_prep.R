# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(tidylog)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)

# load survey data --------------------------------------------------------
fishglob_url <- "https://github.com/AquaAuma/FishGlob_data/raw/d71dfa03c2912b4e9d9cd10412ae2af52ba56ae5/outputs/Compiled_data/FishGlob_public_std_clean.RData"
options(timeout = 300)
load(url(fishglob_url))

# remove excluded surveys -------------------------------------------------
excl_surveys <- c('GSL-N','GSL-S','ROCKALL','AI','DFO-SOG','WCTRI','SEUS','FR-CGFS','SP-PORC') # sampled inconsistently or rarely or started recently
data <- data %>% 
  filter(!survey %in% excl_surveys) %>% 
  mutate(survey = case_when(
    survey %in% c("DFO-HS", "DFO-QCS", "DFO-WCHG", "DFO-WCVI") ~ "BC",
    TRUE ~ survey
  )) # British Columbia survey go together

# remove bad hauls --------------------------------------------------------
goa_del <- data %>% filter(survey == 'GOA' & year < 1994) %>% pull(haul_id) |> unique()

gmex_del <- data %>% filter(survey == 'GMEX' & longitude > -87.9) %>% pull(haul_id) |> unique()# remove hauls on the east part of GMEX, they started in 2008

neus_del <- data %>%
  filter((survey == "NEUS" & year < 2009 & (haul_dur < 0.42 | haul_dur > 0.58)) |
           (survey == "NEUS" & year >= 2009 & (haul_dur < 0.25 | haul_dur > 0.42)) & (year < 1968 | year > 2019)) %>%
  pull(haul_id) |> unique()

bits_del <- data %>% filter(survey == 'BITS' & year < 1999) %>% pull(haul_id) |> unique()

nor_bts_del <- c(
  data %>% filter(survey == 'Nor-BTS' & year < 2004) %>% pull(haul_id),
  data %>% filter(survey == 'Nor-BTS' & latitude < 68) %>% pull(haul_id)
) |> unique()

evhoe_del <- data %>% filter(survey == 'EVHOE' & year == 2017) %>% pull(haul_id) |> unique()

nigfs_del <- data %>% filter(survey == 'NIGFS' & year < 2008) %>% pull(haul_id) |> unique()

ns_ibts_del <- data %>% filter(survey == 'NS-IBTS' & year < 1980) %>% pull(haul_id) |> unique()

swc_ibts_del <- data %>% filter(survey == 'SWC-IBTS' & year == 1995 & longitude > -5 & latitude < 50.5 & latitude > 49) %>% pull(haul_id) |> unique()

haul_ids_del <- c(goa_del, gmex_del, neus_del, bits_del, nor_bts_del,
                  evhoe_del, ns_ibts_del, swc_ibts_del, nigfs_del) |> unique()

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
    survey %in% c('GOA') ~ "Gulf of Alaska",
    survey %in% c('BC') ~ "British Columbia",
    survey %in% c('WCANN') ~ "California Current",
    survey %in% c('BITS') ~ "Baltic",
    survey %in% c('NS-IBTS') ~ "North Sea",
    survey %in% c('Nor-BTS') ~ "Barents Sea",
    survey %in% c("EBS") ~ "East Bering Sea",
    survey %in% c("GMEX") ~ "Gulf of Mexico",
    survey %in% c("EVHOE","IE-IGFS","SWC-IBTS", "NIGF") ~ "Celtic-Biscay Shelf",
    survey %in% c("SP-NORTH") ~ "North Iberian Coast",
    survey %in% c("SCS","NEUS") ~ "Northeast U.S. and Scotian Shelf",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region)) %>%
  mutate(region_short = case_when(
    region == "Gulf of Alaska" ~ "GOA",
    region == "British Columbia" ~ "BC",
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
  group_by(region) %>%
  complete(haul_id, accepted_name, fill = list(wgt_cpua = 0)) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  filter(year >= 1994)

# filter for common species -----------------------------------------------

# parameters 
min_freq_threshold <- 15   # minimum % frequency of occurrence in a region
#min_years <- 10            # minimum number of years present
cumulative_biomass_threshold <- 0.99  # 99% of biomass per region

# hauls per region
haul_summary <- data_complete %>%
  distinct(region, year, survey, haul_id)

total_hauls_per_region <- haul_summary %>%
  group_by(region) %>%
  summarize(total_hauls_region = n_distinct(haul_id), .groups = "drop")

# species occurrence and total biomass
species_occurrence <- data_complete %>%
  group_by(region, year, haul_id, accepted_name) %>%
  summarize(wgt_cpua = sum(wgt_cpua), .groups = "drop") %>%
  mutate(present = as.integer(wgt_cpua > 0)) # quite slow, might switch to data.table

# metrics per region
species_metrics <- species_occurrence %>%
  group_by(region, accepted_name) %>%
  summarize(
    years_present = n_distinct(year[present == 1]),
    positive_hauls = sum(present),
    mean_wgt_cpua = mean(wgt_cpua[present == 1], na.rm = TRUE),
    total_biomass = sum(wgt_cpua, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(total_hauls_per_region, by = "region") %>%
  mutate(freq_occurrence = 100 * positive_hauls / total_hauls_region)

# biomass ranking + cumulative threshold
species_biomass_selected <- species_metrics %>%
  group_by(region) %>%
  arrange(region, desc(total_biomass)) %>%
  mutate(
    cumulative_biomass = cumsum(total_biomass) / sum(total_biomass)
  ) %>%
  filter(cumulative_biomass <= cumulative_biomass_threshold) %>%
  ungroup()

# apply threshold
species_filtered <- species_biomass_selected %>%
  filter(
    #years_present >= min_years,
    freq_occurrence >= min_freq_threshold
  )

# retained species metadata
species_summary <- species_filtered %>%
  select(region, accepted_name, years_present, freq_occurrence, mean_wgt_cpua, total_biomass)

# remove taxa with only genus information
species_summary <- species_summary %>%
  filter(str_detect(accepted_name, "\\s"))

# save it
write_csv(species_summary,here('data/trawl_surveys/selected_species.csv'))

# filter dataset
filtered_data <- data_complete %>%
  semi_join(species_summary, by = c("region", "accepted_name"))

# add depth if missing ----------------------------------------------------
# depth_raster <- terra::rast(here("data/environmental/depth/GEBCO_2023_sub_ice_topo.nc"))
# 
# filtered_data$depth_gebco <- terra::extract(depth_raster$elevation,
#                                             filtered_data %>% select(longitude, latitude))$elevation %>%
#   abs()
# 
# # number of trawls with missing depth info
# sum(is.na(filtered_data$depth))
# 
# filtered_data <- filtered_data %>%
#   mutate(depth = coalesce(depth, depth_gebco)) %>%
#   select(-depth_gebco)

# final selection and save -----------------------------------------------
data <- filtered_data %>%
  select(survey, region, region_short, haul_id, country, continent,
         year, month, day, quarter, latitude, longitude, haul_dur,
         area_swept, gear, sbt, sst, depth, accepted_name, wgt_cpua)

write_rds(data, here("data/trawl_surveys/fishglob_clean.rds"))

