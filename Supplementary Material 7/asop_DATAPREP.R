library(tidyverse)
library(patchwork)
library(ggsn)
library(ggspatial)
library(sf)
library(speciesgeocodeR)

cntr <- st_read("Shapes/GADM_COUNTRIES_OK.shp")
cntr_spat <- as_Spatial(cntr)
regions <- st_read("Shapes/Ecoregions2017.shp", options = "ENCODING=WINDOWS-1252")
regions_spat <- as_Spatial(regions)
realms <- st_read("Shapes/Ecoregions_Realms.shp", options = "ENCODING=WINDOWS-1252")
realms_spat <- as_Spatial(realms)

asop_raw <- read.delim("dados_brutos.tsv") %>%
  unique() %>%
  na.omit()
  
colnames(asop_raw) <- c("genus", "Species","Local_Label", "decimalLongitude", "decimalLatitude", "Refs")

setdiff(asop_raw, raw_references)

asop_countries <- SpGeoCod(asop_raw, cntr_spat, areanames = "NAME_0")
asop_raw_1 <- cbind(asop_raw, asop_countries$samples$homepolygon)
colnames(asop_raw_1) <- c("genus", "Species","Local_Label", "decimalLongitude", "decimalLatitude", "Refs", "country")

asop_continents_sp_geo <- SpGeoCod(asop_raw_1, cntr_spat, areanames = "CONTINENT")
asop_continents <- cbind(asop_raw_1, asop_continents_sp_geo$samples$homepolygon)
colnames(asop_continents) <- c("genus", "Species","Local_Label", "decimalLongitude", "decimalLatitude", "Refs", "country", "continent")

asop_ecoregions_sp_geo <- SpGeoCod(asop_continents, regions_spat, areanames = "ECO_NAME")
asop_ecoregions <- cbind(asop_continents, asop_ecoregions_sp_geo$samples$homepolygon)
colnames(asop_ecoregions) <- c("genus", "Species","Local_Label", "decimalLongitude", "decimalLatitude", "Refs", "country", "continent", "ecoregion")


asop_realms <- SpGeoCod(asop_ecoregions, realms_spat, areanames = "REALM")
asop <- cbind(asop_ecoregions, asop_realms$samples$homepolygon)
colnames(asop) <- c("genus", "Species","Local_Label", "decimalLongitude", "decimalLatitude", "Refs", "country", "continent", "ecoregion", "realm")

asop <- asop %>%
  distinct()

write_excel_csv(asop, "Asopinae_Analises_R_Preparado.csv")

## Manueal check
# Cermatulus	Cermatulus nasalis	151.12	-33.51	Australia	Australia	Eastern Australian temperate forests	Australasia
# Perillus	Perillus splendidus	-117.9092	33.6084	United States	North America	California coastal sage and chaparral	Nearctic
# Platynopus	Platynopus calliger	120.7514	-4.4601	Indonesia	Asia	not_classified	not_classified

