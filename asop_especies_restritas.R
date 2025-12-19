asop <- read.csv("asopinae_v_1_0.csv")

library(tidyverse)


registers <- asop %>%
  group_by(Species) %>%
  summarise(n = n())


## Espécies restritas aos países ----
restricted_cntr <- asop %>%
  group_by(Species) %>%
  summarize(is_endemic = ifelse(n_distinct(country) == 1, "Endemic", "Not Endemic")) %>%
  merge(registers, by = "Species")
  

endemic_cntr <- restricted_cntr %>%
  count(is_endemic) %>%
  mutate(percentage = n / sum(n) * 100) 


endemic_species_per_country <- asop %>%
  group_by(Species) %>%
  filter(n_distinct(country) == 1) %>%  # Keep only endemic species (those found in one country)
  ungroup() %>%
  group_by(country, Species) %>%
  summarize(endemic_species_count = n_distinct(Species)) %>%
  group_by(country) %>%
  summarize(endemic_species_count = n()) 


write.csv(restricted_cntr, "Stats/species_end_cntr.csv")
write.csv(endemic_cntr, "Stats/percent_endemic_cntr.csv")
write.csv(endemic_species_per_country, "Stats/asop_end_cntr.csv")






## Espécies restritas aos continentes ----
restricted_cont <- asop %>%
  group_by(Species) %>%
  summarize(is_endemic = ifelse(n_distinct(continent) == 1, "Endemic", "Not Endemic")) 


endemic_cont <- restricted_cont %>%
  count(is_endemic) %>%
  mutate(percentage = n / sum(n) * 100) 


endemic_species_per_cont <- asop %>%
  group_by(Species) %>%
  filter(n_distinct(continent) == 1) %>%  # Keep only endemic species (those found in one country)
  ungroup() %>%
  group_by(country, Species) %>%
  summarize(endemic_species_count = n_distinct(Species)) %>%
  group_by(country) %>%
  summarize(endemic_species_count = n()) 


write.csv(restricted_cont, "Stats/species_end_cont.csv")
write.csv(endemic_cont, "Stats/percent_endemic_cont.csv")
write.csv(endemic_species_per_cont, "Stats/asop_end_cont.csv")





## Em quais locais ocorre

number_of_continents <- asop %>%
  distinct(Species, continent) %>%
  group_by(Species) %>%
  summarise(Number_Cont = n())

which_continents <- asop %>%
  group_by(Species) %>%
  summarise(Conts = paste(unique(continent), collapse = " - "))

number_of_countries <- asop %>%
  distinct(Species, country) %>%
  group_by(Species) %>%
  summarise(Number_Cntr = n())

which_countries <- asop %>%
  group_by(Species) %>%
  summarise(Cntrs = paste(unique(country), collapse = " - "))

list_df <- list(number_of_continents, which_continents, number_of_countries, which_countries)

continents_countries <- list_df %>%
  reduce(full_join, by = "Species") %>%
  cbind(registers$n) %>%
  rename(Count = "registers$n")

write.csv(continents_countries, "Stats/continents_countries.csv")




