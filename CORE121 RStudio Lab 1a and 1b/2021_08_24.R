

library(readxl)
library(tigris)
library(sf)
library(tidyverse)
library(plotly)
options(tigris_use_cache = TRUE)

lab_4_data <- read_excel("data/CELHouse93to116.xlsx")



x <- lab_4_data %>% 
  filter(!is.na(`ICPSR number, according to Poole and Rosenthal`) & `Congress number` == 116) %>% 
  select(st_code = `Two-letter state code`, cd = `Congressional district number`, dw1 = `First-dimension DW-NOMINATE score`) %>% 
  group_by(st_code, cd) %>% 
  summarize(dw1 = mean(dw1)) %>% 
  ungroup()



x <- x %>% 
  group_by(st_code) %>% 
  mutate(cv1 = n()) %>% 
  ungroup()


lab_1_data1 <- read_excel("Excel Lab 1 copy.xlsx") %>% 
  mutate(state = ifelse(state != "District of Columbia", str_to_title(state), state))
lab_1_data1 <- lab_1_data1 %>% 
  rename(pop_2014 = `2014_citizen_population`,
         voted_2014 = `2014_total_voted`,
         pop_2018 = `2018_citizen_population`,
         voted_2018 = `2018_total_voted`,
         `Pres 2020` = dem_rep)
lab_1_data1 <- lab_1_data1 %>% 
  mutate(`Turnout pct increase` = (voted_2018 - voted_2014) / voted_2014 * 100) %>% 
  arrange(desc(`Turnout pct increase`)) 

l <- lab_1_data1 %>% 
  ggplot(aes(x = pop_2018, y = `Turnout pct increase`, label = state, color = `Pres 2020`)) +
  
  geom_point() +
  scale_color_manual(values=c("blue", "green", "red")) 

l
ggplotly(l)


x <- x %>% 
  mutate(cv2 = ifelse(cv1 == 1, "00", formatC(cd, width = 2, flag = "0")))


terrs <- c("11", "60", "66", "69", "72", "74", "78")

cd_sf <- congressional_districts(cb=TRUE, year = 2019) %>% shift_geometry() %>% 
  mutate(cdr = str_c(STATEFP, "-", CD116FP)) %>% 
  filter(!STATEFP %in% terrs)

fips <- tidycensus::fips_codes %>%
  select(state, state_code) %>%
  filter(!state_code %in% terrs) %>% 
  distinct()

y <- inner_join(x, fips, by=c("st_code" = "state")) %>% 
  rename(STATEFP = state_code, CD116FP = cv2) %>%
  mutate(cdr = str_c(STATEFP, "-", CD116FP))


lab_4_sf <- inner_join(cd_sf, y, by="cdr")

us_counties <- counties(cb=TRUE) %>% 
  shift_geometry()


us_states <- states(cb = TRUE) %>% 
  shift_geometry()


lab_4_sf_1 <- lab_4_sf %>% 
  filter(STATEFP.x %in% c("41","06","53")) 






#lab_4_sf = st_transform(lab_4_sf, crs = 4326)

p <- lab_4_sf %>% 
  ggplot(aes(text = str_c(st_code,CD116FP.x," ",format(dw1, digits = 3)))) +
  geom_sf(aes(fill = dw1), color = "black") +
  theme_void(base_size = 16) +
  labs(title = "DW1 by CD",
       fill = "Increase %",
       caption = "Note: Alaska, Hawaii, and Puerto Rico are shifted and not to scale.") +
  theme(plot.title = element_text(hjust = 0.0)) +
  scale_colour_gradient(low = "blue", high = "red")



Sys.time()
p
Sys.time()


gg_2 <- ggplotly(p, tooltip = "text")
gg_2 %>%
  style(
    hoveron = "fills",
    # override the color mapping
    line.color = toRGB("gray40"),
    # don't apply these style rules to the first trace, which is the background graticule/grid
    #traces = seq.int(2, length(gg_2$x$data))
    traces = c(2, 347)
  ) %>%
  hide_legend()

