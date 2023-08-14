


Goldenstein_Score%>%
  select(year, region, goldstein_score)%>%
  group_by(year, region)%>%
  summarise(goldstein_score = mean(goldstein_score))%>%
  ggplot() +
  aes(x = year, y = goldstein_score, color = region) + 
  geom_line() +
  owidR::scale_color_owid() +
  owidR::theme_owid() +
  labs(x = "Year"
       , y = "Goldstein Score"
       , color = "Region"
       , title = "Mean Goldstein Score over the Years"
       , subtitle = "Average of Regions Goldstein Score by year. Goldstein score is an index indicating
the potential of news events to influence the stability of the country 
the event happened in. Goldstein Score goes from -10 (Very destabilizing) to 10"
       , caption = "Source: Leetaru and Schrodt (2013)" )

instable_20 <- Goldenstein_Score%>%
  group_by(entity)%>%
  summarise(goldstein_score_mean = mean(goldstein_score))%>%
  ungroup()%>%
  filter(rank(goldstein_score_mean) <= 20)%>%
  select(entity)

Goldenstein_Score%>%
  group_by(entity)%>%
  summarise(gs_start = first(goldstein_score)
          , gs_end = last(goldstein_score)
  )%>%
  filter(entity %in% instable_20$entity)%>%
  mutate(direction = case_when(gs_start > gs_end+0.2 ~ '<--'
                               , gs_start < gs_end-0.2 ~ '-->')) %>%
  ggplot() +
  aes(y = as.factor(entity), x = (gs_start+gs_end)/2, label = direction) +
  geom_linerange(aes(xmin = gs_start, xmax = (gs_start+gs_end)/2, color='1979'))+
  geom_point(aes(y = entity, x = gs_start, color='1979')) +
  geom_linerange(aes(xmin = (gs_start+gs_end)/2, xmax = gs_end, color='2023'))+
  geom_point(aes(y = entity, x = gs_end, color='2023')) +
  geom_text(nudge_y = 0.2) +
  scale_color_manual(name='Regression Model',
                     breaks=c('1979', '2023'),
                     values=c('1979'='#d7a99b', '2023'='#9dc1c6'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))+
  owidR::theme_owid()
