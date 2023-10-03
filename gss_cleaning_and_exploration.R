#cleaning and exploring data from the Ghana Statistical Service

library(readr)
library(tidyverse)
df <- gss_experiment_Sheet1 <- read_csv("data/gss_experiment - Sheet1.csv", col_names = FALSE)

df <- df %>% 
  drop_na(X1) %>% 
  filter(!row_number() %in% 695) %>% 
  filter(!str_detect(X1, 'Source')) 

df <- df %>% 
  mutate(Year = rep(c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022),each=46))

df <- df %>% 
  filter(!str_detect(X1, 'Table'))

df_pre_2019 <- df %>% 
  filter(Year < 2019) %>% 
  select(-c(14,15,16,17,18,19,20,21,22,23)) %>% 
  row_to_names(row_number = 1) %>% 
  filter(!str_detect(No., 'No.')) %>%
  fill(Crops, .direction = "downup") %>% 
  rename(Year = `2008`) %>% 
  pivot_longer(cols = Western:`Upper West`, names_to = "Region") %>% 
  group_by(Year) %>% 
  select(-No.) %>%
  pivot_wider(names_from = Topic, values_from = value)

df_post_2019 <- df %>% 
  filter(Year >= 2019) %>% 
  select(-X20) %>% 
  select(-X21) %>% 
  select(-X22) %>% 
  select(-X23) %>% 
  row_to_names(row_number = 1) %>% 
  fill(Crops, .direction = "downup") %>% 
  rename(Year = `2019`) %>% 
  pivot_longer(cols = Western:`Upper West`, names_to = "Region") %>% 
  group_by(Year) %>% 
  select(-No.) %>%
  pivot_wider(names_from = Topic, values_from = value) %>% 
  select(-Topic) %>% 
  filter(Crops != "Crops")

df_all <- full_join(df_post_2019, df_pre_2019)

df_all$Former_Region <- with(df_all, ifelse(Region == "Bono", 'Brong Ahafo',
                              ifelse(Region == "Bono East", 'Brong Ahafo',
                                     ifelse(Region == "Ahafo", 'Brong Ahafo',
                                            ifelse(Region == "Savannah", 'Northern',
                                                   ifelse(Region == "North East", 'Northern',
                                                          ifelse(Region == "Oti", 'Volta',
                                                                 ifelse(Region == "Western North", 'Western', Region))))))))
df_all <- df_all %>% 
  relocate(Year) %>% 
  group_by(Year) %>% 
  relocate(Former_Region, .after = Year)

df_all$Crop <- with(df_all, ifelse(str_detect(Crops, "Rice"), 'Rice', Crops))

df_all <- df_all %>% 
  relocate(Crop, .after = Former_Region) %>% 
  select(-Crops)

df_all <- df_all %>% replace_with_na_all(condition = ~.x == "-")

df_all <- df_all %>% 
  mutate(Area_planted= as.numeric(as.numeric(gsub(",", "", `Area planted (Ha)`))))

df_all %<>% mutate(`Yield (Mt/Ha)`= as.numeric(`Yield (Mt/Ha)`))%>% 
  select(-`Area planted (Ha)`)


df_all %>% 
  filter(Crop == "Groundnut" | Crop == "Soya Bean") %>%
  group_by(Year) %>% 
  ggplot(aes(x=Year, y=Area_planted, color=Crop)) +
  geom_col(position = 'dodge') #+
  geom_point() #+
  facet_wrap(~Region)
