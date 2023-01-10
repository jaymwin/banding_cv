
library(tidyverse)
library(readxl)
library(sf)

bio_samples <- read_excel(here::here('Biological_Samples.xlsx')) %>%
  select(latitude, longitude, year = yearSampled, month = monthSampled, 
         day = daySampled, band_num = bandNumber, bander = observerName) %>%
  select(-year:-day)
bio_samples

bio_samples <- bio_samples %>%
  filter(bander == 'Jason Winiarski')
bio_samples

serdp_banding <- read_excel(here::here('SERDP_Banding.xlsx')) %>%
  select(latitude, longitude, year, month, day, band_num = markerID, bander = banderName) %>%
  select(-year:-day)
serdp_banding

serdp_banding <- serdp_banding %>%
  filter(bander == 'Jason Winiarski')
serdp_banding

dnr_banding <- read_csv(here::here('2022_mallard_banding_data.csv')) %>%
  rename(longitude = site_long, latitude = site_lat) %>%
  mutate(band_num = str_c(band_prefix, band_suffix)) %>%
  select(band_num, banding_site, transmitter_id, banders, latitude, longitude, blood_sample_id, feather_sample_id) %>%
  filter(str_detect(banders, 'Winiarski'))
dnr_banding

dnr_banding %>%
  filter(!is.na(blood_sample_id))

dnr_banding %>%
  filter(!is.na(feather_sample_id))

dnr_banding %>%
  filter(!is.na(transmitter_id))

all_banding <- serdp_banding %>%
  bind_rows(., bio_samples) %>%
  bind_rows(., dnr_banding %>% select(latitude, longitude, band_num, bander = banders)) %>%
  distinct()
all_banding

all_banding <- all_banding %>%
  drop_na()
all_banding

all_banding %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  mapview::mapview()

library(tigris)
# WA, KS, CA, NM, ID, FL
wa_counties <- list_counties(c('WA')) %>% mutate(state = 'WA')
ks_counties <- list_counties(c('KS')) %>% mutate(state = 'KS')
ca_counties <- list_counties(c('CA')) %>% mutate(state = 'CA')
nm_counties <- list_counties(c('NM')) %>% mutate(state = 'NM')
id_counties <- list_counties(c('ID')) %>% mutate(state = 'ID')
fl_counties <- list_counties(c('FL')) %>% mutate(state = 'FL')
wi_counties <- list_counties(c('WI')) %>% mutate(state = 'WI')


county_names <- wa_counties %>%
  bind_rows(., ks_counties) %>%
  bind_rows(., ca_counties) %>%
  bind_rows(., nm_counties) %>%
  bind_rows(., id_counties) %>%
  bind_rows(., fl_counties) %>%
  bind_rows(., wi_counties) %>%
  distinct() %>%
  as_tibble()
county_names

WA <- tracts(state = c('WA')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'WA') 
KS <- tracts(state = c('KS')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'KS') 
CA <- tracts(state = c('CA')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'CA') 
NM <- tracts(state = c('NM')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'NM') 
ID <- tracts(state = c('ID')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'ID') 
FL <- tracts(state = c('FL')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'FL') 
WI <- tracts(state = c('WI')) %>% st_as_sf() %>% select(COUNTYFP) %>% group_by(COUNTYFP) %>% summarise() %>% mutate(state = 'WI')


tracts <- WA %>%
  bind_rows(., KS) %>%
  bind_rows(., CA) %>%
  bind_rows(., NM) %>%
  bind_rows(., ID) %>%
  bind_rows(., FL) %>%
  bind_rows(., WI) %>%
  select(state, county_code = COUNTYFP) %>%
  left_join(., county_names)
tracts

tracts %>% st_geometry() %>% plot()

all_banding_sf <- all_banding %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  st_transform(., st_crs(tracts))
all_banding_sf

# find points within polygons
bird_in_county <- st_join(all_banding_sf, tracts, join = st_within)
bird_in_county

mapview::mapview(bird_in_county, zcol = 'county')

bird_in_county %>%
  filter(state == 'WI') %>%
  st_drop_geometry() %>%
  distinct(county) %>%
  arrange(county) %>%
  pull(county)
