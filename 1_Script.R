# library ---------
library(peacesciencer)
library(wbstats)
library(tidyverse)
library(ggpubr)
library(countrycode)
library(states)
library(lubridate)
library(messydates)
library(democracyData)
library(scales)
library(zoo)
library(owidR)
library(readxl)
# Create Base Tables -----

## state-year data frame for all years since 1945 -------

state_years <- create_stateyears(system = "cow", subset_years = 1945:2023)


## Polity IV democracy scores --------
polity

## Matchtable major power -----
cow_majors$gwcode <- countrycode(cow_majors$ccode, origin = 'cown', destination = 'gwn', custom_match = c('300' = '300'))
cow_majors$countryname <- countrycode(as.numeric(cow_majors$gwcode), origin = 'gwn', destination = 'country.name', custom_match = c('300' = 'Austria-Hungary'))

## Match table alliances -----
alliance_by_member_yearly <- read.csv("Data/alliance_v4.1_by_member_yearly.csv")



## Match Table War Names ----
cow_wars_match_tables <- read_csv("Data/Inter-StateWarData_v4.0.csv")%>%
  select(WarNum, WarName, WarType)%>%
  mutate(WarName = as.factor(WarName))%>%
  unique()
## colonial History match table ----

library(readr)
Colonial_Origin_RAW <- read_delim("Data/Colonial Origin.csv", 
                              delim = ";", escape_double = FALSE, col_types = cols(ht_colonial = col_number()), 
                              trim_ws = TRUE)



# 00: Never colonized by a Western overseas colonial power
# 01: Dutch
# 02: Spanish
# 03: Italian
# 04: US
# 05: British
# 06: French
# 07: Portuguese
# 08: Belgian
# 09: British-French
# 10: Australian

Colonial_Origin <- Colonial_Origin_RAW%>%
  mutate(dutch_colony =            as.factor(ifelse(ht_colonial == 100,1,0) )
         , spanish_colony =        as.factor(ifelse(ht_colonial == 200,1,0))
         , italian_colony =        as.factor(ifelse(ht_colonial == 300,1,0))
         , US_colony =             as.factor(ifelse(ht_colonial == 400,1,0))
         , british_colony =        as.factor(ifelse(ht_colonial == 500,1,0))
         , french_colony =         as.factor(ifelse(ht_colonial == 600,1,0))
         , portuguese_colony =     as.factor(ifelse(ht_colonial == 700,1,0))
         , belgian_colony =        as.factor(ifelse(ht_colonial == 800,1,0))
         , british_french_colony = as.factor(ifelse(ht_colonial == 900,1,0))
         , australian_colony =     as.factor(ifelse(ht_colonial == 1000,1,0))
         , former_colony =         as.factor(ifelse(ht_colonial > 0, 1,0)))%>%
  select(-ccode_qog, -ccodealp, -ccodealp_year, -version, -cname_qog, -cname_year, -ccodecow)%>%
  select(ccode, cname, year, former_colony)

## War informations -----

war_stats<- cow_war_inter%>%
  select(warnum, ccode1, ccode2, year, initiator1, initiator2, outcome1, outcome2, batdeath1, batdeath2)%>%
  mutate(aggressor = case_when(initiator1 == 1 ~ ccode1
                               , initiator2 == 1 ~ ccode2)
         , looser = case_when(outcome1 == 2 ~ ccode1
                              , outcome2 == 2 ~ ccode2)
         , winner = case_when(outcome1 == 1 ~ ccode1
                              , outcome2 == 1 ~ ccode2)
         , is_tied = case_when(outcome1 == 3 ~ 1
                               , outcome1 != 3 ~ 0)
         , ongoing_2007 = case_when(outcome1 == 5 ~ 1
                                    , outcome1 != 5 ~ 0)
         , stalemate = case_when(outcome1 == 6 ~ 1
                                 , outcome1 != 6 ~ 0)
         , conflict_minor = case_when(outcome1 == 7 ~ 1
                                      , outcome1 != 7 ~ 0)
         , aggressor_won = case_when(aggressor == winner ~ 1
                                     , aggressor != winner ~ 0)
         , gwcode1 = countrycode(ccode1, origin = 'cown', destination = 'gwn', custom_match = c('300' = '300'))
         , gwcode2 = countrycode(ccode2, origin = 'cown', destination = 'gwn', custom_match = c('300' = '300'))
  )

## Amount of previous wars ---- 


wars_before <- war_stats%>%
  select(ccode1, year, warnum)%>%
  group_by(ccode1, year) %>%
  dplyr::summarize(count = n_distinct(warnum)) %>% 
  ungroup() %>%
  right_join(war_stats%>%
               select(ccode1, year)%>%
               unique(), by = join_by(ccode1 == ccode1
                                      , year > year))%>%
  group_by(ccode1, year.x)%>%
  dplyr::summarize(previous_wars = sum(count, na.rm = T ))%>%
  ungroup()

## GDP per capita & Income inequality ------

gini_coef <- read_csv("Data/economic-inequality-gini-index.csv")
## Recoding gini_coef$Entity
gini_coef$Entity <- gini_coef$Entity %>%
  fct_recode(
    "Argentina" = "Argentina - urban"
  )
gini_coef <- gini_coef%>%
  mutate(Entity = as.character(Entity)
         , Code = as.character(Code))%>%
  mutate(Code = as.character(case_when(Entity == "Argentina" ~ "ARG"
                          , .default = as.character(Code)))
         )%>%
  rename(year = Year
         , code = Code
         , entity = Entity)



gdp_per_cap <- read_csv("Data/gdp-per-capita-maddison.csv")%>%
  rename(year = Year
         , code = Code
         , entity = Entity)

wb_economics <-  gdp_per_cap%>%
  left_join(gini_coef, join_by(code, closest(year >= year)))%>%
  select(-entity.y, -year.y)%>%
  mutate(entity = entity.x
         , year = year.x
         , code = countrycode(code,'iso3c','cown')
         , gdp_per_capita = `GDP per capita`
         , gini = na.locf0(`Gini coefficient`, fromLast = T))%>%
  select(entity, code, year, gdp_per_capita, gini)

## geographic Data ----
geographic_data <- create_stateyears(system = "cow", subset_years = 1945:2023) %>%
  add_rugged_terrain() # terrain ruggedness index + logarithmic transformation of how mountainous the state

natural_ressource_rents <- wb_data("NY.GDP.TOTL.RT.ZS")%>%
  rename(natural_ressource_rents = NY.GDP.TOTL.RT.ZS
         , year = date)%>%
  mutate(ccode = countrycode(iso3c, "iso3c","cown"))%>%
  select(-iso2c, -iso3c, -unit, -obs_status, -footnote,-last_updated)%>%
  drop_na()%>%
  select(ccode, country, year, natural_ressource_rents)

##  Human development index ------
hdi <- read_csv("Data/human-development-index-escosura.csv")%>%
  rename(year = Year
         , code = Code
         , entity = Entity)
hdi <- hdi%>%
  mutate(ccode = countrycode(code, origin = "iso3c", destination = "cown")
         , hdi = `Historical Index of Human Development (Prados de la Escosura)`)%>%
  select(-`Historical Index of Human Development (Prados de la Escosura)`)

## Goldstein Score ----

Goldenstein_Score_raw <- read_csv("Data/Goldstein Score.csv")

Goldenstein_Score <- Goldenstein_Score_raw%>%
  mutate(ccode = countrycode(ActionGeo_CountryCode,"fips","cown")
         , entity = countrycode(ActionGeo_CountryCode,"fips","country.name")
         , region = countrycode(ActionGeo_CountryCode,"fips","region"))%>%
  rename(year = Year)%>%
  select(ccode, entity, year, region, goldstein_score, tone)%>%
  drop_na()

WEIS_DATA_RAW <- read_csv("Data/05211-0002-Data.txt", 
                          col_names = FALSE, col_types = cols(X1 = col_character()))

WEIS_DATA_BRONCE <- tibble(
  year = as.integer(paste0("19",str_sub(WEIS_DATA_RAW$X1,12,13)))
  , month = as.integer(str_sub(WEIS_DATA_RAW$X1,14,15))
  , day = as.integer(str_sub(WEIS_DATA_RAW$X1,16,17))
  , actor_code = as.integer(str_sub(WEIS_DATA_RAW$X1,18,20))
  , event_code = as.character(str_sub(WEIS_DATA_RAW$X1,23,25))
  , target_code = as.integer(str_sub(WEIS_DATA_RAW$X1,26,28))
  , arena_code = as.integer(str_sub(WEIS_DATA_RAW$X1,29,31))
)



actor_match <- read_excel("Data/WEIS Matching.xlsx")
events_match <- read_excel("Data/WEIS Matching.xlsx", 
                           sheet = "Events")%>%
  rename(base_code = `Base Code`
         , event_code = `Event Code`
         , description = Description)%>%
  group_by(base_code,  event_code) %>%
  summarise(description = paste(description, collapse = ","))%>%
  ungroup()%>%
  drop_na()
  
arena_match <- read_excel("Data/WEIS Matching.xlsx", 
                            sheet = "Arenas")

goldstein_points <- read_excel("Data/WEIS Matching.xlsx", 
                               sheet = "Goldstein Scale")

WEIS_DATA_SILVER <- WEIS_DATA_BRONCE%>%
  left_join(actor_match, join_by(actor_code == Code_N))%>%
  select(-actor_code, -ISO3C)%>%
  rename(actor = Entity)%>%
  left_join(events_match, join_by(event_code == event_code), relationship = "many-to-many")%>%
  rename(base_action = base_code
         , action_description = description)%>%
  left_join(actor_match, join_by(target_code == Code_N), relationship = "many-to-many")%>%
  rename(target = Entity)%>%
  left_join(arena_match, join_by(arena_code == Code))%>%
  mutate(Definition = replace_na(Definition, "")
         , event_code = as.integer(event_code))%>%
  left_join(goldstein_points, join_by(event_code))%>%
  select(-target_code, -arena_code, -description)


goldstein_score_full <- rbind( 
  WEIS_DATA_SILVER%>%
    select(year, target, points)%>%
    group_by(year, target)%>%
    summarise(goldstein_score = mean(points))%>%
    ungroup()%>%
    mutate(country = target
           , ccode = countrycode(country,"country.name","cown")
           , region = countrycode(country,"country.name", "region"))%>%
    rename(entity = country)%>%
    drop_na()%>%
    select(ccode, entity, year, region, goldstein_score)
  ,
  Goldenstein_Score%>%
    select(ccode, entity, year, region, goldstein_score)
)%>%
  mutate(year = year+1)


# Create Factor Tables ------
## Political Factors ------
politcal_factors <- state_years%>%
  left_join(polity, by = join_by(ccode  == ccode , year == year))%>%
  left_join(alliance_by_member_yearly, by = join_by(ccode  == ccode , year == year), multiple = "all")%>%
  mutate(polityIV = polity
         , is_democracy = as.factor(ifelse(polity >=6,1,0))
  )%>%
  group_by(ccode, statenme, year, polity, is_democracy)%>%
  summarise(defense_pact = as.factor(max(defense))
            , neutrality_pact = as.factor(max(neutrality))
            , nonaggression_pact = as.factor(max(nonaggression))
            , entente_pact = as.factor(max(entente))
            )%>%
  select(ccode, statenme, year, polity, is_democracy, defense_pact, neutrality_pact, nonaggression_pact, entente_pact)%>%
  unique()%>%
  ungroup()

## Historical Factors ----
historical_factors <- Colonial_Origin%>%
  mutate(ccode = countrycode(cname, "country.name", "cown"
         , custom_match = c('Serbia' = '345'))
         , ccode = as.double(ccode))%>%
  left_join(wars_before, by = join_by(ccode == ccode1 
                                      , year == year.x))

## Socioeconomic Factors ----

socioeconomic_factors <- wb_economics%>%
  left_join(hdi%>%select(-code), by = join_by(code == ccode
                                              , closest(year >= year)
                                              , entity == entity))%>%
  select(code, entity, year.x, gdp_per_capita, gini, hdi)%>%
  mutate(year = year.x+1)%>%
  select(code, entity, year, gdp_per_capita, gini, hdi)

## Geographic Factors ----
geographic_factors <- geographic_data%>%
  left_join(natural_ressource_rents, by = join_by(ccode, year))%>%
  select(-country, -statenme)%>%
  group_by(ccode)%>%
  mutate(natural_ressource_rents = na.locf0(natural_ressource_rents, fromLast = TRUE))

## RTA Data ----
rtas <- read_csv("Data/rta_20221214.csv")

net_rta <- rtas%>%
  mutate(region_reporter = countrycode(exporter, "iso3c","region")
         , region_partner = countrycode(importer, "iso3c","region"))

country_distance <- create_dyadyears(system = "cow")%>%
  filter(year > 1945)%>%
  add_minimum_distance()%>%
  mutate(ccode1 = countrycode(ccode1, "cown","iso3c")
         , ccode2 = countrycode(ccode2, "cown","iso3c"))

rta_ref <- net_rta%>%
  mutate(ccode1 = exporter
         , ccode2 = importer)%>%
  select(-exporter, -importer)%>%
  left_join(country_distance, by = join_by(ccode1, ccode2, year))%>%
  mutate(regional = case_when(mindist == 0 ~ 1
                              , region_reporter == region_partner ~ 1
                              , .default = 0))%>%
  filter(regional == 1)%>%
  rename(ccode = ccode1)%>%
  group_by(ccode, year)%>%
  summarise(rta_deep = sum(ifelse(cu + eia + cueia + ftaeia + psaeia >= 1,1,0))
            , rta_shallow = sum(ifelse(fta + psa >= 1 & cu + eia + cueia + ftaeia + psaeia == 0,1,0)))%>%
  mutate(ccode = countrycode(ccode, "iso3c","cown"))


  



# Full Data ----

attacks_against <- war_stats%>%
  filter(initiator2 == 1 & year >= 1946)%>%
  mutate(attacked = 1)%>%
  select(ccode1, ccode2, year, attacked)


rta_dyd <- net_rta%>%
  mutate(ccode1 = countrycode(exporter,"iso3c","cown")
         , ccode2 = countrycode(importer,"iso3c","cown")
         , rta_deep = (cu + eia + cueia + ftaeia + psaeia)
         , rta_shalloe = (fta + psa))%>%
  select(-exporter, -importer, -rta, -cu, -fta, -psa, -eia, -cueia
         , -psaeia, -ftaeia, -region_reporter, -region_partner)


full_data_1 <- create_dyadyears(system = "cow")%>%
  mutate(region1 = countrycode(ccode1, "cown", "region")
         , region2 = countrycode(ccode2, "cown", "region"))%>%
  filter(year > 1945)%>%
  add_minimum_distance()%>%
  filter(region1 == region2 | mindist == 0)%>%
  add_cow_wars(type = "inter")%>%
  rename(attacked = cowinteronset)%>%
  select(-cowinterongoing, -sidea1, -sidea2, -initiator1
         , -initiator2, -outcome1, -outcome2, -batdeath1, -batdeath2, -resume)%>%
  add_cow_majors()%>%
  rename(is_major_power = cowmaj1
         , partner_is_major_power = cowmaj2)%>%
  left_join(rta_dyd, join_by(ccode1,ccode2,year))%>%
  inner_join(socioeconomic_factors, join_by(ccode1 == code
                                            , year == year))%>%
  rename(gdp_per_capita_attacked = gdp_per_capita
         , gini_attacked = gini
         , hdi_attacked = hdi)%>%
  inner_join(socioeconomic_factors, join_by(ccode2 == code
                                            , year == year))%>%
  rename(gdp_per_capita_attacker = gdp_per_capita
         , gini_attacker = gini
         , hdi_attacker = hdi)%>%
  left_join(historical_factors, join_by(ccode1 == ccode
                                        , year == year))%>%
  rename(former_colony_attacked = former_colony 
         , previous_wars_attacked = previous_wars
         )%>%
  left_join(historical_factors, join_by(ccode2 == ccode
                                        , year == year))%>%
  rename(former_colony_attacker = former_colony 
         , previous_wars_attacker = previous_wars
         )%>%
  left_join(politcal_factors, join_by(ccode1 == ccode
                                      , year == year))%>%
  rename(is_democracy_attacked = is_democracy 
         , defense_pact_attacked = defense_pact
         , neutrality_pact_attacked = neutrality_pact
         , nonaggression_attacked = nonaggression_pact
         , entente_pact_attacked = entente_pact
         , polity_attacked = polity)%>%
  left_join(politcal_factors, join_by(ccode2 == ccode
                                      , year == year))%>%
  rename(is_democracy_attacker = is_democracy 
         , defense_pact_attacker = defense_pact
         , neutrality_pact_attacker = neutrality_pact
         , nonaggression_attacker = nonaggression_pact
         , entente_pact_attacker = entente_pact
         , polity_attakcer = polity)%>%
  left_join(geographic_factors, join_by(ccode1 == ccode
                                        , year == year))%>%
  rename(rugged_attacked = rugged 
         , dnewlmtnest_attacked = newlmtnest
         , natural_ressource_rents_attacked = natural_ressource_rents)%>%
  left_join(geographic_factors, join_by(ccode2 == ccode
                                        , year == year))%>%
  rename(rugged_attacker = rugged 
         , dnewlmtnest_attacker = newlmtnest
         , natural_ressource_rents_attacker = natural_ressource_rents)%>%
  group_by(ccode1, ccode2, year)%>%
  na.locf()%>%
  ungroup()%>%
  # left_join(attacks_against, join_by(ccode1 == ccode1
  #                                    , ccode2 == ccode2
  #                                    , year == year))%>%
  mutate(former_colony_attacked = as.factor(replace_na(as.integer(former_colony_attacked), 0))
         , former_colony_attacker = as.factor(replace_na(as.integer(former_colony_attacker), 0))
         , previous_wars_attacker = replace_na(previous_wars_attacker, 0)
         , previous_wars_attacked = replace_na(previous_wars_attacked, 0)
         , era = as.factor(case_when(year >= 1945 & year < 1960 ~ 'Early Cold War Era'
                                     , year >= 1960 & year < 1980 ~ 'Vietnam Era'
                                     , year >= 1980 & year < 1992 ~ 'Late Cold War Era'
                                     , year >= 1992 & year < 2001 ~ 'Post Cold War Era'
                                     , year >= 2001 ~ 'Post 9/11 War Era'))
         , region = as.factor(region1)
         , attacked = replace_na(attacked, 0)
         , attacked = as.factor(if_else(attacked >= 1, 1,0))
         , gdp_per_capita_attacked = log(gdp_per_capita_attacked)
         , gdp_per_capita_attacker = log(gdp_per_capita_attacker)
         , rta_deep = as.integer(replace_na(rta_deep, 0))
         , rta_shalloe = as.integer(replace_na(rta_shalloe, 0))
  )%>%
  left_join(goldstein_score_full, join_by(ccode1 == ccode
                                          , year == year))%>%
  mutate(goldstein_score = replace_na(goldstein_score,0))%>%
  rename(region = region.x)%>%
  select(-contains(".x"), -contains(".y")
         , -region2, -region1, -polity_attakcer
         , -polity_attacked)%>%
  unique()




# Visualistaions ----
## Democracies in the world ----
democracies <- politcal_factors%>%
  mutate(region = countrycode(ccode, origin = "cown", destination = "continent"))%>%
  group_by(year, region)%>%
  summarise(democracies = sum(as.integer(is_democracy), na.rm = T ))%>%
  ungroup()%>%
  select(year, region, democracies)%>%
  drop_na()



library(dplyr)
library(ggplot2)

democracies %>%
  filter(!is.na(region)) %>%
  ggplot() +
  aes(x = year, y = democracies, fill = region) +
  geom_col() +
  scale_colour_owid() +
  labs(x = " ", y = " ", title = "Democracies by World Region", 
       subtitle = "Democracies by continents measured through the polityIV score. 
Country is measured as democracy with a score >= 6", 
       caption = "Source: Marshall et al. (2019), Graphic: own visualisation", color = "Region") +
  owidR::theme_owid()



## deaths in the world graph ----
deaths_in_conflicts<- owidR::owid("deaths-in-state-based-conflicts-by-world-region")
deaths_in_conflicts <- deaths_in_conflicts%>%
  mutate(deaths_in_conflicts = `Deaths in all state-based conflict types`)%>%
  select(-`Deaths in all state-based conflict types`)


deaths_in_conflicts %>%
  filter(!(entity %in% "Asia & Oceania")) %>%
  ggplot() +
  aes(x = year, y = deaths_in_conflicts, fill = entity) +
  geom_col(alpha = 0.5) +
  scale_y_continuous(labels = comma) +
  scale_colour_owid() +
  labs(x = " ", y = " ", title = "Deaths in state-based conflicts & number of democracies in the world", 
       subtitle = "Direct deaths of both military personnel and civilians. Deaths from disease or famine are not included.
The region does not always relate to where the fighting occurred – it sometimes refers to the region of the participants in the conflict.", 
       caption = "Source: OWID based on PRIO and UCDP Note: 'State-based' conflicts are between states, or a state and a non-state armed organised group. 
The 'best' estimates from the sources are used, or - if unavailable - the mid-point between high and low estimates.", 
       color = "Region") +
  owidR::theme_owid()


## Deaths in conflicts and Democracies ----
deaths_and_democracies <- deaths_in_conflicts%>%
  left_join(democracies, by = join_by(year == year, entity == region), multiple = "all")%>%
  unique()%>%
  filter(deaths_in_conflicts <= 200000)

library(ggpmisc)

deaths_and_democracies%>%
  group_by(year)%>%
  summarise(deaths_in_conflicts = sum(deaths_in_conflicts)
            , democracies = sum(democracies, na.rm = T))%>%
  ggplot()+
  aes(y = deaths_in_conflicts, x = democracies, color = year) +
  geom_jitter()+
  stat_poly_line(se = F) +
  stat_poly_eq(use_label(c("eq"))) +
  geom_smooth(method = lm, linetype = 'dashed', se = F, color = "grey30")+
  scale_y_continuous(labels = comma)+
  labs(x = " ", y = " ", title = "Deaths in state-based conflicts & number of democracies in the world", 
       subtitle = "
Direct deaths of both military personnel and civilians. Deaths from disease or famine are not included.
The region does not always relate to where the fighting occurred – it sometimes refers to the region of the participants in the conflict.
Democracies are calculated from number of countries with a PolityIV score >= 6."
       ,caption = "Source: OWID based on PRIO and UCDP & Polity IV index Note: 'State-based' conflicts are between states, or a state and a non-state armed organised group. 
The 'best' estimates from the sources are used, or - if unavailable - the mid-point between high and low estimates.", 
       color = "Year") +
  owidR::theme_owid()

## Democracies at war ----
democracies_at_war <- war_stats%>%
  mutate(region1 = countrycode(ccode1,"cown","region")
         , region2 = countrycode(ccode2,"cown","region")
         , regional = ifelse(region1 == region2,T,F)
         , aggressor = if_else(initiator1 == 0, ccode2, ccode1)
  )%>%
  filter(year > 1945)%>%
  mutate(deaths = batdeath1 + batdeath2)%>%
  select(warnum, year, aggressor, looser, winner, deaths, regional)%>%
  group_by(warnum, aggressor)%>%
  summarise(start_year = min(year)
            , end_year = max(year)
            , regional = as.factor(min(regional)))%>%
  mutate(regional = ifelse(regional == 1, "Regional", "Interregional"))%>%
  left_join(cow_wars_match_tables, by = join_by(warnum == WarNum))%>%
  arrange(start_year)%>%
  drop_na()%>%
  left_join(politcal_factors%>%drop_na(), by = join_by(aggressor == ccode, closest(start_year <= year)))


library(dplyr)
library(ggplot2)

democracies_at_war %>%
  filter(!is.na(statenme)) %>%
  ggplot() +
  aes(x = is_democracy, fill = is_democracy) +
  geom_bar() +
  owidR::scale_fill_owid() +
  labs(x = "Democracy by Polity IV", title = "Democracies go to war"
       , subtitle = "How often are democracies part of wars?", 
       caption = "Data: ", fill = "Democracy") +
  owidR::theme_owid()

war_stats%>%
  filter(year >= 1946)%>%
  mutate(attacked = if_else(aggressor == ccode1, ccode2, ccode1)
         , aggressor = if_else(aggressor == ccode1, ccode1, ccode2))%>%
  select(warnum, year, aggressor, attacked)%>%
  unique()%>%
  group_by(warnum, aggressor, attacked)%>%
  summarise(start_year = max(year))%>%
  ungroup()%>%
  left_join(politcal_factors%>%drop_na()%>%select(ccode, year, is_democracy), by = join_by(aggressor == ccode, closest(start_year >= year)))%>%
  left_join(politcal_factors%>%drop_na()%>%select(ccode, year, is_democracy), by = join_by(attacked == ccode, closest(start_year >= year)))%>%
  mutate(aggressor_dem = as.integer(is_democracy.x)
         , attacked_dem = as.integer(is_democracy.y))%>%
  select(-year.x, -year.y, -is_democracy.x, -is_democracy.y)%>%
  group_by(warnum, aggressor, attacked)%>%
  summarise(aggressor_dem = max(as.integer(aggressor_dem)-1)
            , attacked_dem = max(as.integer(attacked_dem)-1)
  )%>%
  ungroup()%>%
  drop_na()%>%
  select(-aggressor, -attacked)%>%
  mutate(both_dem = if_else(aggressor_dem ==1 & attacked_dem == 1,1,0)
         , dem_attacks = if_else(aggressor_dem == 1 & attacked_dem != 1, 1,0)
         , dem_attacked = if_else(attacked_dem == 1 & aggressor_dem != 1, 1,0)
  )%>%
  group_by(both_dem, dem_attacks, dem_attacked)%>%
  count()%>%
  ungroup()%>%
  group_by(both_dem, dem_attacks, dem_attacked)%>%
  gather("key", "value", -n)%>%
  filter(value == 1)%>%
  mutate(key = case_when(key == "both_dem" ~ "Both sides democratic"
                         , key == "dem_attacked" ~ "Attacked by nondemocracy"
                         , key == "dem_attacks" ~ "Democracy started")
  )%>%
  ggplot() +
  aes(x = key, y = n, fill = key) +
  geom_col() +
  owidR::scale_fill_owid() +
  labs(x = "War Party Democracy Status"
       , title = "Democratic War Participation"
       , subtitle = "How often are democracies the one to to start a war?", 
       caption = "
* Only shows data for wars that had a democratic state as a party
Data: Marshall et. al. (2002) & Sarkees and Wayman (2010)", fill = "Attack Type") +
  owidR::theme_owid()

## Trade data ----


trade_of_gdp_by_region <- wb_data(indicator = "NE.TRD.GNFS.ZS", country = "regions_only")
trade_of_gdp_by_world <- wb_data(indicator = "NE.TRD.GNFS.ZS", country = "world")

p <- ggplot(rbind(trade_of_gdp_by_region))+
  aes(x = date , y = NE.TRD.GNFS.ZS/100, color = country)+
  geom_line()+
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 4))+
  scale_colour_owid()+
  expand_limits(y=c(0, 1)) +
  labs(x = "Year", y = "Trade (% of GDP)", title = "Trade Development by world regions", 
       subtitle = "Graph 4: Trade is the sum of exports and imports of goods and services measured as a share of gross domestic product.", 
       color = "Region") +
  owidR::theme_owid()

p2 <- ggplot(rbind(trade_of_gdp_by_world))+
  aes(x = date , y = NE.TRD.GNFS.ZS/100)+
  geom_line(color = "black")+
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 4))+
  expand_limits(y=c(0, 1)) +
  labs(x = "Year", y = "Trade (% of GDP)", caption = "Source: Worldbank Data, indicator: 'NE.TRD.GNFS.ZS' all regions", 
       color = "Region") +
  owidR::theme_owid()

## Trade Network ----
library(tradestatistics)
library(tibble)
library(tidyverse)
library(igraph)
library(ggraph)
library(networkD3)
library(tidygraph)

yrpc2 <- ots_create_tidy_data(
  years = 2002:2019,
  table = "yrp"
)


net_2019 <- yrpc2%>%
  filter(year == 2019
         & grepl("not elsewhere specified", partner_name) == F )%>%
  group_by(reporter_iso)%>%
  filter(trade_value_usd_exp == max(trade_value_usd_exp))%>%
  ungroup()%>%
  select(-year, -reporter_iso, -partner_iso )


nodes_2019 <- c(net_2019$reporter_name, net_2019$partner_name) %>%
  unique() %>%
  tibble(label = .) %>%
  rowid_to_column("id")

edges_2019 <- net_2019 %>%
  left_join(nodes_2019, by = c("reporter_name"="label")) %>%
  rename(from = "id") %>%
  left_join(nodes_2019, by = c("partner_name"="label")) %>%
  rename("to" = "id") %>%
  mutate(N = trade_value_usd_exp)%>%
  select(from, to, N)

graph_tidy_2019 <- tbl_graph(nodes = nodes_2019, edges = edges_2019, directed = FALSE)

graph_tidy_2019 %>%
  mutate(Centrality = centrality_authority()) %>%
  ggraph(layout = "graphopt") +
  geom_node_point(aes(size=Centrality, colour = label), show.legend = FALSE) +
  geom_edge_link(aes(width = N), alpha = 0.8, show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  labs(title = "World Trade Network by biggest Export Trade Partner 2019", 
       subtitle = "
Network of export partners. Connections are made for each reporting state to the partner with the biggest export value in USD", 
       caption = "Data based on the year 2002. Source: Vargas M (2023), own visualisation"
       , x = NULL
       , y = NULL) +
  ggthemes::theme_map()  + theme(plot.subtitle = element_text(family = "serif"),
                              panel.grid.major = element_line(linetype = "blank"),
                              panel.grid.minor = element_line(linetype = "blank"),
                              axis.text = element_text(colour = "white"),
                              plot.title = element_text(family = "serif",
                                                        size = 14), panel.background = element_rect(fill = "white")) +labs(subtitle = "Network of export partners. Connections are made for each reporting state to the partner with the biggest export value in USD")
## War Networks ----

war_nets <- war_stats%>%
  mutate(country_1 = countrycode(ccode1,"cown","country.name")
       , country_2 = countrycode(ccode2,"cown","country.name")
       , deaths = batdeath1 + batdeath2) %>%
  filter(year > 1945)%>%
  select(country_1, country_2, deaths)%>%
  tidyr::crossing()


nodes_war<- c(war_nets$country_1, war_nets$country_2) %>%
  unique() %>%
  tibble(label = .) %>%
  rowid_to_column("id")

edges_war <- war_nets %>%
  ungroup()%>%
  left_join(nodes_war, by = c("country_1"="label")) %>%
  rename(from = "id") %>%
  left_join(nodes_war, by = c("country_2"="label")) %>%
  rename("to" = "id") %>%
  group_by(country_1, country_2, from, to)%>%
  dplyr::summarise(N = sum(deaths))%>%
  ungroup()%>%
  select(from, to, N)

graph_tidy_2019 <- tbl_graph(nodes = nodes_war, edges = edges_war, directed = FALSE)

summary(edges_war)

graph_tidy_2019 %>%
  mutate(Centrality = centrality_authority()) %>%
  ggraph(layout = "graphopt") +
  geom_node_point(aes(colour = label)
                  , size = 3
                  , alpha = 0.5
                  , show.legend = FALSE) +
  geom_edge_link(aes(size = N, alpha = edge_weights)
                 , alpha = 0.5
                 , show.legend = FALSE) +
  scale_edge_width(range = c(0, 1)) +
  geom_node_text(aes(label = label)
                 , repel = TRUE)+
  labs(title = "Connections of States through Wars waged against each other"
       , subtitle = "Network of states, that waged war against each other since 1945. 
Connections are made for each state to any opponent faced since 1945"
       , caption = "Source: Correlates Of War Interstate Data, Gibler 2009, own visualisation"
       , x = NULL
       , y = NULL) +
  ggthemes::theme_map()  + theme(plot.subtitle = element_text(family = "serif"),
                                 panel.grid.major = element_line(linetype = "blank"),
                                 panel.grid.minor = element_line(linetype = "blank"),
                                 axis.text = element_text(colour = "white"),
                                 plot.title = element_text(family = "serif",
                                                           size = 14), panel.background = element_rect(fill = "white")) +
  labs(subtitle = "Network of export partners. Connections are made for each reporting state to the partner with the biggest export value in USD")




## War Gantt ----
war_stats%>%
  mutate(region1 = countrycode(ccode1,"cown","region")
         , region2 = countrycode(ccode2,"cown","region")
         , regional = ifelse(region1 == region2,T,F)
  )%>%
  filter(year > 1945)%>%
  mutate(deaths = batdeath1 + batdeath2)%>%
  select(warnum, year, aggressor, looser, winner, deaths, regional)%>%
  group_by(warnum)%>%
  summarise(start_year = min(year)
            , end_year = max(year)
            , regional = as.factor(min(regional)))%>%
  mutate(regional = ifelse(regional == 1, "Regional", "Interregional"))%>%
  left_join(cow_wars_match_tables, by = join_by(warnum == WarNum))%>%
  arrange(start_year)%>%
  ggplot()+
  aes(x=start_year
      , xend=end_year+1
      , y=fct_reorder(WarName, start_year, .desc = T)
      , yend=fct_reorder(WarName, start_year, .desc = T)
      , color = regional
  )+
  geom_segment(size = 5, alpha = 0.5)+
  scale_color_discrete()+
  owidR::theme_owid() + 
  theme(plot.subtitle = element_text(family = "serif"),
        plot.title = element_text(family = "serif")) +
  labs(title = "Interstate Wars since World-War 2"
       , x = "Year"
       , y = "War Names"
       , subtitle = "Gant chart of all interstate wars as categorized by Correlates Of War with the provided name. 
Chart shows beginning and end year of the wars since the end of World War 2, World War 2 not included.",
       caption = "Source: "
       , color = "Regional War") + theme(panel.grid.major = element_line(colour = "gray70",
                                                                         linetype = "solid"))
# Models ----

library(fastDummies)

full_data_dumm <- dummy_cols(full_data_1, select_columns = c("era", "region"))

## Reordering full_data_dumm$attacked
full_data_dumm$attacked <- full_data_dumm$attacked %>%
  fct_relevel(
    "0", "1"
  )

## Reordering full_data_dumm$defense_pact_attacked
full_data_dumm$defense_pact_attacked <- full_data_dumm$defense_pact_attacked %>%
  fct_relevel(
    "0", "1"
  )

## Reordering full_data_dumm$defense_pact_attacker
full_data_dumm$defense_pact_attacker <- full_data_dumm$defense_pact_attacker %>%
  fct_relevel(
    "0", "1"
  )

## Reordering full_data_dumm$entente_pact_attacked
full_data_dumm$entente_pact_attacked <- full_data_dumm$entente_pact_attacked %>%
  fct_relevel(
    "0", "1"
  )

## Reordering full_data_dumm$entente_pact_attacker
full_data_dumm$entente_pact_attacker <- full_data_dumm$entente_pact_attacker %>%
  fct_relevel(
    "0", "1"
  )


full_data_2 <- full_data_1%>%
  select(-ccode1, -ccode2, -year, -gdp_per_capita_attacker
         , -gdp_per_capita_attacked, -entity, -era, -region, -mindist
         , -dnewlmtnest_attacked, -dnewlmtnest_attacker, -defense_pact_attacker
         , -neutrality_pact_attacker, -nonaggression_attacker, -entente_pact_attacker
         , -region
  )

result_1 <- glm(full_data_2, formula = attacked ~ .
    , family = binomial(link = "logit"))

full_data_3 <- full_data_1%>%
  mutate(goldstein_score = goldstein_score - ave(goldstein_score, ccode1)
       , gini_attacked = gini_attacked - ave(gini_attacked, ccode1)
       , hdi_attacked = hdi_attacked - ave(hdi_attacked, ccode1)
       , natural_ressource_rents_attacked = natural_ressource_rents_attacked - ave(natural_ressource_rents_attacked, ccode1))%>%
  mutate(gini_attacker = gini_attacker - ave(gini_attacker, ccode2)
         , hdi_attacker = hdi_attacker - ave(hdi_attacker, ccode2)
         , natural_ressource_rents_attacker = natural_ressource_rents_attacker - ave(natural_ressource_rents_attacker, ccode1))%>%
  select(-ccode1, -ccode2, -year, -gdp_per_capita_attacker
         , -gdp_per_capita_attacked, -entity, -era, -region, -mindist
         , -dnewlmtnest_attacked, -dnewlmtnest_attacker, -defense_pact_attacker
         , -neutrality_pact_attacker, -nonaggression_attacker, -entente_pact_attacker
         , -region
         )


result_2 <- glm(full_data_3, formula = attacked ~ .
                , family = binomial(link = "logit"))

library(DescTools)
summ_1 <- summary(result_1)
summ_2 <- summary(result_2)


r2result1 <- round(1 - summ_1$deviance/summ_1$null.deviance,3)
r2result2 <- round(1 - summ_2$deviance/summ_2$null.deviance,3)
library(stargazer)



stargazer(result_1, result_2
          , type = "text"
          , out = "reg_table.html"
          , style = "apsr"
          , single.row = TRUE
          , digits=2
          #, header = TRUE
          , title = "Table 2: Regression Results"
          #, selection.equation	= T
          , table.layout = "ldcm#-t-s-a=n"
          , column.labels = c("GLM", "Fixed Effects")
          , covariate.labels = c("Is Major Power", "Partner is Major Power", "RTA Deep", "RTA Shallow", 
                                 "Gini Attacked", "HDI Attacked", "Gini Attacker"
                                 , "HDI Attacker", "Former Colony Attacked",
                                 "Previous Wars Attacked", "Former Colony Attacker",
                                 "Previous Wars Attacker", "Is Democracy Attacked",
                                 "Defense Pact", "Neutrality Pact", "Nonaggression"
                                 , "Entente Pact", "Is Democracy Attacker"
                                 , "Rugged Attacked", "Natural Resource Rents Attacked",
                                 "Rugged Attacker", "Natural Resource Rents Attacker", "Goldstein Score")
          , dep.var.labels = c("State being Attacked by Neigbouring State")
          , add.lines = list(c("Pseudo R2", r2result1, r2result2))
)

summ(result_1)

# Replace "dependent_var" with the name of your dependent variable and "independent_vars" with the names of your independent variables
# New variable names (Initial caps, underscores replaced with white spaces, and subscript added)
new_names <- c("Mindist", "Is Major Power", "Partner is Major Power", "RTA Deep", "RTA Shalloe", "GDP per Capita Attacked",
               "Gini Attacked", "HDI Attacked", "GDP per Capita Attacker", "Gini Attacker", "HDI Attacker", "Former Colony Attacked",
               "Previous Wars Attacked", "Former Colony Attacker", "Previous Wars Attacker", "Polity Attacked", "Is Democracy Attacked",
               "Defense Pact Attacked", "Neutrality Pact Attacked", "Nonaggression Attacked", "Entente Pact Attacked",
               "Polity Attacker", "Is Democracy Attacker", "Defense Pact Attacker", "Neutrality Pact Attacker", "Nonaggression Attacker",
               "Entente Pact Attacker", "Rugged Attacked", "Dnewlmtnest Attacked", "Natural Resource Rents Attacked",
               "Rugged Attacker", "Dnewlmtnest Attacker", "Natural Resource Rents Attacker", "Region", "Era",
               "Goldstein Score")

library(correlation)

correlation(full_data_2)%>%
  knitr::kable()
