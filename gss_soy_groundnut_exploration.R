#visualizing soybean and groundut data collected by the Ghana Statistical Service

soy <- read_csv("data/soy_groundnut_230427 - Sheet1 (1).csv")

soy <- soy %>% 
  select(-c(Western, `Western North`, Central, `Greater Accra`, Ahafo)) %>% 
  pivot_longer(cols = Volta:`Upper West`,
               names_to = "Region") %>% 
  spread(key=Topic,value = value) 
  


soy %>% filter(Crops == "Soya Bean") %>%  
  filter(Year >=  2020) %>% 
  ggplot(aes(x=Year, y=`Area planted (Ha)`, color=Region)) +
    geom_smooth() +
  scale_x_continuous(n.breaks=3) +
  scale_y_continuous(n.breaks=6) +
  labs(title = "Land Area Planted with Soy Beans",
       subtitle = "Top six regions in Ghana, 2020-2022",
       y = "Hectares Planted",
       caption = "Source: Ghana Statistical Service") +
  theme(plot.caption.position = "plot")

  
soy %>% filter(Crops == "Groundnut") %>% 
  filter(Year >=  2020) %>% 
  filter(Region != "Eastern") %>% 
  filter(`Area planted (Ha)` >= 10000) %>% 
  ggplot(aes(x=Year, y=`Area planted (Ha)`, color=Region)) +
  geom_smooth() +
  scale_x_continuous(n.breaks=4) +
  labs(title = "Land Area Planted with Groundnuts",
       subtitle = "Top six regions in Ghana, 2020-2022",
       y = "Hectares Planted",
       caption = "Source: Ghana Statistical Service") +
  theme(plot.caption.position = "plot")

soy_ground <- FAOSTAT_2018_2021_final_FAOSTAT_data_en_4_25_2023 

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Production") %>% 
    ggplot(aes(x=Year, y=Value, fill = Item)) +
    geom_col(position = 'dodge') +
    scale_y_continuous(labels = label_comma()) +
    labs(title = "Total Production of Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "mt",
       caption = "Source: FAO Crops and livestock products database",
       fill = "Crop") +
    theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 2.5,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Yield") %>% 
  ggplot(aes(x=Year, y=Value, fill = Item)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Average Yield of Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "kg/ha",
       caption = "Source: FAO Crops and livestock products database",
       fill = "Crop") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 2.5,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Area harvested") %>% 
  ggplot(aes(x=Year, y=Value, fill = Item)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Total Area Harvested for Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "ha",
       caption = "Source: FAO Crops and livestock products database",
       fill = "Crop") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 2.5,
    vjust = 1.5, position = position_dodge(.9)
  )
