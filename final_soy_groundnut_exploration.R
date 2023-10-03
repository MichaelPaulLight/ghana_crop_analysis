#exploring and visualizing soybean and groundnut data from the FAO and GSS

soy_ground <- read_csv("data/FAOSTAT_2018-2021_230428_3.csv")

soy_ground <- soy_ground %>% 
  pivot_longer(
    cols = starts_with("Value"),
               names_to = "Organization",
               names_prefix = "Value_",
               values_to = "Value",
               values_drop_na = FALSE)

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Production") %>% 
  filter(Organization == "FAO") %>% 
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
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Yield") %>% 
  #filter(Item == "Groundnuts") %>% 
  filter(Organization == "SRID") %>%
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
    colour = "white", size = 3,
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
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  filter(Element == "Area harvested") %>% 
  filter(Item == "Groundnuts") %>% 
  ggplot(aes(x=Year, y=Value, fill=Organization)) +
  geom_col(position = "dodge") 

soy_ground %>% 
  filter(Element == "Yield") %>% 
  filter(Item == "Groundnuts") %>% 
  ggplot(aes(x=Year, y=Value, fill=Organization)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Data Variability: Groundnut Yield",
       subtitle = "Comparing FAO, SRID, and USDA data for 2018-2021",
       y = "kg/ha",
       caption = "Source: FAOSTAT, USDA, SRID",
       fill = "Data Source") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 2,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  filter(Element == "Production") %>% 
  filter(Item == "Groundnuts") %>% 
  ggplot(aes(x=Year, y=Value, fill=Organization)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Data Variability: Groundnut Production",
       subtitle = "Comparing FAO, SRID, and USDA data for 2018-2021",
       y = "mt",
       caption = "Source: FAOSTAT, USDA, SRID",
       fill = "Data Source") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 1.7,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  filter(Element == "Area harvested") %>% 
  filter(Item == "Groundnuts") %>% 
  ggplot(aes(x=Year, y=Value, fill=Organization)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Data Variability: Groundnut Area Harvested",
       subtitle = "Comparing FAO, SRID, and USDA data for 2018-2021",
       y = "ha",
       caption = "Source: FAOSTAT, USDA, SRID",
       fill = "Data Source") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 1.7,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Production") %>% 
  filter(Organization == "SRID") %>% 
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
  filter(Element == "Area harvested") %>% 
  filter(Item == "Soybeans") %>%
  filter(Organization != "USDA") %>% 
  ggplot(aes(x=Year, y=Value, fill=Organization)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Data Variability: Soybean Production",
       subtitle = "Comparing FAO, SRID, and USDA data for 2018-2021",
       y = "mt",
       caption = "Source: FAOSTAT, USDA, SRID",
       fill = "Data Source") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 1.7,
    vjust = 1.5, position = position_dodge(.9)
  )

soy_ground %>% 
  group_by(Item) %>% 
  filter(Element == "Production") %>% 
  filter(Organization == "SRID") %>% 
  ggplot(aes(x=Year, y=Value, fill = Item)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Total Production, Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "mt",
       caption = "Source: SRID",
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
  filter(Organization == "SRID") %>%
  ggplot(aes(x=Year, y=Value, fill = Item)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Average Yields, Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "kg/ha",
       caption = "Source: SRID",
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
  filter(Organization == "SRID") %>% 
  ggplot(aes(x=Year, y=Value, fill = Item)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Total Area Harvested, Soybeans and Groundnuts in Ghana",
       subtitle = "2018-2021",
       y = "ha",
       caption = "Source: SRID",
       fill = "Crop") +
  theme(plot.caption.position = "plot") +
  geom_text(
    aes(label = scales::comma(Value)),
    colour = "white", size = 2.5,
    vjust = 1.5, position = position_dodge(.9)
  )

soy <- soy_groundnut_230427_Sheet1_1_

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