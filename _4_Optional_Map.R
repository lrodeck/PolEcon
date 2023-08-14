library(BBmisc)
library(tidyverse)
library(hablar)
library(ggbump)
library(sf)
library(rnaturalearth)
library(feather)
library(janitor)
library(lubridate)
library(countrycode)
library(owidR)
library(wbstats)
library(eurostat)
options(stringsAsFactors = F)

library(readr)
Goldenstein_Score_raw <- read_csv("Data/Goldstein Score.csv")

Goldenstein_Score <- Goldenstein_Score_raw%>%
  mutate(code = countrycode(ActionGeo_CountryCode,"fips","iso3c")
         , entity = countrycode(ActionGeo_CountryCode,"fips","country.name")
         , region = countrycode(ActionGeo_CountryCode,"fips","region"))%>%
  rename(year = Year)%>%
  select(code, entity, year, region, goldstein_score, tone)%>%
  drop_na()





df <- Goldenstein_Score%>%
  group_by(year)%>%
  filter(rank(goldstein_score) <= 20)%>%
  filter(year == 2022)%>%
  arrange(rank(goldstein_score))
  

sdf <- rnaturalearthdata::countries110 %>% 
  st_as_sf() %>% 
  #st_make_valid()%>%
  #st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = 180) %>% 
  filter(iso_a3 %in% df$code) %>% 
  left_join(df, by = c("iso_a3" = "code"))

sdf_all <-rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  #st_make_valid()%>%
  #st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = 180) %>% 
  #filter(admin %in% df ) %>% 
  left_join(df, by = c("iso_a3" = "code"))



ranking <- st_geometry(sdf) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(fine_cap = normalize(rank(sdf$goldstein_score),
                   range = c(50, -10), method = "range"),
                   country = sdf$admin,
                   xend = 130,
                   x_axis_start = xend + 10,
                   fine_cap_x = normalize(sdf$goldstein_score, range = c(first(x_axis_start), 120), method = "range"),
                   val_txt = paste0(format(sdf$goldstein_score, digits = 3, nsmall = 2)),
                   val_txt2 = paste0(case_when(sdf$admin == "El Salvador" ~ sdf$goldstein_score -.1
                                        , sdf$admin == "Somalia" ~ sdf$goldstein_score -.1
                                        , sdf$admin == "Sudan" ~ sdf$goldstein_score -.1
                                        , .default =  sdf$goldstein_score)
                   )
  )
  )


sdf <- sdf %>% 
  bind_cols(ranking %>% select(fine_cap))

ggplot() + 
  geom_sf(data = sdf_all, size = 20.9, fill = "transparent", color = "#66545e") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start + .2, yend = fine_cap, group = country, color = fine_cap), 
               alpha = .6, smooth = 10, size = 1, na.rm = T) + 
  # Line from xstart to value
  geom_segment(data = ranking, 
               aes(x = fine_cap_x+40, y = fine_cap, xend = x_axis_start, yend = fine_cap, color = fine_cap), alpha = .6, size = 1, 
               lineend = "round", na.rm = T) + 
  # Y axis - black line
  geom_segment(data = ranking, 
               aes(x = fine_cap_x, y = 0, xend = x_axis_start, yend = 70), alpha = 0, size = 1.3, color = "transparent", na.rm = T) +
  # dot on centroid of country in map
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = fine_cap), size = 2, na.rm = T) +
  # Country text
  geom_text(data = ranking, aes(x = x_axis_start+20, y = fine_cap, label = country, color = fine_cap), hjust = 1, size = 2.5, nudge_y = 1.5, na.rm = T) +
  # Value text
  geom_text(data = ranking, aes(x = x_axis_start+40, y = fine_cap, label = val_txt2, color = fine_cap), hjust = 0, size = 2, nudge_x = .4, na.rm = T) +
  coord_sf(clip = "off") +
  scale_fill_gradient(low = "#5e6a58",
                      high = "#9e391a") +
  scale_colour_gradient(low = "#5e6a58",
                        high = "#9e391a") +
  theme_void() +
  labs(title = "World Instability Ranking (2023)",
       subtitle = str_wrap("Average of Regions Goldstein Score by year. Goldstein score is an index indicating
the potential of news events to influence the stability of the country 
the event happened in. Goldstein Score goes from -10 (Very destabilizing) to 10.", 100),
       caption = "Source: Leetaru and Schrodt (2013)") + 
  theme(plot.margin = margin(0, 0, 0.1, 0.1, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#c4ac7c"),
        plot.caption = element_text(color = "#66545e"),
        plot.title = element_text(color = "#66545e", size = 16, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "#66545e", size = 8))

