# Load libraries
library(tidyverse)
library(lubridate)

# Load TC storms
tc_storms <- storms
# summary stats
summary(tc_storms)
#### Data wrangling
# mutate verb
# parse date time
tc_storms2 <- tc_storms %>%
  mutate(date_time = paste(year, month, day, hour, sep = "-")) %>%
  mutate(date_time = ymd_h(date_time))

# filter verb (filtering hurricanes for example)
hurricanes <- tc_storms2 %>%
  filter(status == "hurricane")

# filtering using multiple conditions
hurricanes <- tc_storms2 %>%
  filter(status == "hurricane", category > 1,
    str_detect(name, c("^D")))

# filter rows with no missing
noNa_diameter <- tc_storms2 %>%
  filter(!is.na(ts_diameter))

# select verb (select relevant variables)
some_vars <- tc_storms2 %>%
  select(name, date_time, status, wind, ts_diameter)

# all together
tc_storms %>%
  mutate(date_time = ymd_h(paste(year, month, day, hour, sep = "-"))) %>%
  filter(status == "hurricane", category > 1) %>%
  filter(!is.na(ts_diameter)) %>%
  select(name, date_time, status, wind, ts_diameter) %>%
  arrange(desc(wind))

##### data summary
# number of storms per year
nb_storms <- tc_storms2 %>%
  group_by(year) %>%
  summarise(nb.storms = n_distinct(name))
# number of storms per year by status
all <- tc_storms2 %>%
  group_by(year, status) %>%
  summarise(nb.storms = n_distinct(name))
# number of storms per year by category
categories <- tc_storms2 %>%
  group_by(year, category) %>%
  summarise(nb.storms = n_distinct(name))

# plots ####
# storm frequency
p1 <- ggplot(nb_storms, aes(x = year, y = nb.storms))
p1 + geom_point()
p1 + geom_point() + geom_line()
p1 + geom_point() +
  geom_smooth(method = "lm", color = "red")
p1 + geom_point() +
  geom_smooth(method = "loess", se = T, span = 0.30, color = "blue")

# storm frequency (each storm type)
ggplot() +
  geom_point(data = all, aes(x = year, y = nb.storms)) +
  geom_smooth(data = all, aes(x = year, y = nb.storms), method = "loess", se = T, span = 0.30, color = "blue") +
  geom_smooth(data = all, aes(x = year, y = nb.storms), method = "lm", color = "red") +
  theme_minimal() +
  facet_grid(status ~ ., scales = "free_y")

# storm frequency by category (each storm type)
ggplot(categories, aes(x = year, y = nb.storms)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  facet_wrap(~category, scales = "free_y")

# map the storm locations -> how to step-by-step build a map
WORLD <- map_data("world")
ggplot() +
  geom_polygon(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(category > 3), aes(x = long, y = lat, color = name))


ggplot() + # other data
  geom_polygon(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(year == 2005, category > 0), aes(x = long, y = lat, color = category))

ggplot() + # reduce viewing frame
  geom_polygon(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(year == 2005, category > 0), aes(x = long, y = lat, color = category)) +
  xlim(-100, -40) +
  ylim(10, 40)

ggplot() + # fix lines, other projection and labels
  geom_path(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(year == 2005, category > 0), aes(x = long, y = lat, color = category)) +
  xlim(-100, -40) +
  ylim(10, 40) +
  coord_map(projection = "stereographic") +
  theme_minimal() +
  labs(x = "longitude", y = "latitude", title = "Hurricanes in 2005")

ggplot() + # fix label
  geom_path(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(year == 2005, category > 0), aes(x = long, y = lat, color = category)) +
  xlim(-100, -40) +
  ylim(10, 40) +
  coord_map(projection = "stereographic") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude", title = "Hurricanes in 2005")

ggplot() + # add paths for full time of storm/hurricane
  geom_path(data = WORLD, aes(x = long, y = lat, group = group)) +
  geom_point(data = tc_storms2 %>% filter(year == 2005, category > 0), aes(x = long, y = lat, color = category)) +
  geom_path(data = tc_storms2 %>% filter(year == 2005), aes(x = long, y = lat, group = name)) +
  xlim(-100, -40) +
  ylim(10, 40) +
  coord_map(projection = "stereographic") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude", title = "Hurricanes in 2005")


# making the plot conform with corporate guidelines - colours and font

windowsFonts(DIN = windowsFont("Din"))

DG.colour <- "#A89B91" # Dove Grey
AS.colour <- "#525E66" # Asphalt
MR.colour <- "#A70531" # Mercia Red
BR.colour <- "#F26A38" # Bronze
NT.colour <- "#006167" # Neptune
PBL.colour <- "#009BC9" # Petrol Blue Light
AVL.colour <- "#6F3092" # African Violet Light
colour_sequence <- c(PBL.colour, NT.colour, AVL.colour, BR.colour, MR.colour)

ggplot() +
  geom_path(data = WORLD, aes(x = long, y = lat, group = group), colour = DG.colour) +
  geom_point(data = tc_storms2 %>% filter(year == 2005) %>% filter(category > 0), aes(x = long, y = lat, color = category)) +
  geom_path(data = tc_storms2 %>% filter(year == 2005), aes(x = long, y = lat, group = name), colour = AS.colour) +
  coord_map(projection = "mercator") +
  scale_color_manual(values = colour_sequence) +
  theme_minimal() +
  xlim(-100, -40) +
  ylim(10, 40) +
  theme(text = element_text(family = "DIN")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "longitude", y = "latitude", title = "Hurricanes in 2005")

# applied to a graph from earlier
# original
ggplot(categories, aes(x = year, y = nb.storms)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  facet_wrap(~category, scales = "free_y")

# adapted
ggplot(categories, aes(x = year, y = nb.storms)) +
  geom_point(colour = AS.colour) +
  geom_line(colour = AS.colour) +
  geom_smooth(method = "lm", color = MR.colour) +
  theme_minimal() +
  theme(text = element_text(family = "DIN")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "number of storms", y = "year", title = "Yearly number of storms and hurricanes per category") +
  facet_wrap(~category, scales = "free_y")

# plot storms in some sort of interactive map
library(leaflet)
leaflet(tc_storms2[1:5000, ]) %>%
  addTiles() %>%
  addCircles(lng = ~long, lat = ~lat, fill = ~wind, group = ~name)