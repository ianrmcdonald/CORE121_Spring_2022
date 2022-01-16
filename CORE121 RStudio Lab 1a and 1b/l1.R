library(readxl)
library(tigris)
library(sf)
library(tidyverse)
library(plotly)

source("lab_setup.R")
print("It's not your world, it's not my world, it's R world")

lab_1_data <- read_excel("Excel Lab 1.xlsx")
lab_1_data <- lab_1_data %>% 
  mutate(state = ifelse(state != "District of Columbia", str_to_title(state), state))

lab_1_data <- lab_1_data %>% 
  rename(pop_2014 = `2014 Citizen Population`,
         voted_2014 = `2014 Total voted`,
         pop_2018 = `2018 citizen Population`,
         voted_2018 = `2018 total voted`)

sums_of_lab1_data <- lab_1_data %>% 
  summarize(pop_2014 = sum(pop_2014), 
            voted_2014 = sum(voted_2014), 
            pop_2018 = sum(voted_2018),  
            voted_2018 = sum(voted_2018))
sums_of_lab1_data <- lab_1_data %>% 
  summarize(across(where(is.numeric), sum))

lab_1_data <- lab_1_data %>% 
  mutate(turnout_pct_increase = (voted_2018 - voted_2014) / voted_2014 * 100) %>% 
  arrange(desc(turnout_pct_increase)) 

lab_1_data <- lab_1_data %>% 
  mutate(turnout_pct_2014 = voted_2014 / pop_2014 * 100) %>% 
  mutate(turnout_pct_2018 = voted_2018 / pop_2018 * 100)

lab_1_data <- lab_1_data %>% 
  mutate(pct_pop_change = turnout_pct_2018 - turnout_pct_2014) %>% 
  arrange(desc(pct_pop_change)) 

lab_1_data %>% 
  select(state, pct_pop_change)
lab_1_data <- lab_1_data %>% 
  mutate(pct_total_2018 = voted_2018 / sum(voted_2018) * 100)

lab_1_data <- lab_1_data %>% 
  mutate(pct_dif_us_mean_2018 = sum(voted_2018)/sum(pop_2018)*100 - turnout_pct_2018)

lab_1_data %>% summarise(median(voted_2014), median(pop_2014), median(voted_2018), median(pop_2018))
lab_1_data %>% summarize((sum(voted_2018) - sum(voted_2014)) / sum(voted_2014)) * 100

lab_1_data %>% summarize(sum(voted_2014) / sum(pop_2014) * 100)
lab_1_data %>% summarize(sum(voted_2018) / sum(pop_2018) * 100)

lab_1_data %>% summarize(sum(voted_2018) / sum(pop_2018) * 100) - lab_1_data %>% summarize(sum(voted_2014) / sum(pop_2014) * 100)

state_codes <- st_codes_f(full_name = lab_1_data$state)
lab_1_data <- inner_join(lab_1_data, state_codes, by=c("state" = "st_name"))
us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()
lab_1_sf <- inner_join(us_states, lab_1_data, by=c("STUSPS" = "stcd"))

m <- lab_1_sf %>% 
  ggplot(aes(text = str_c(STUSPS," ",format(turnout_pct_increase, digits = 3)))) +
  geom_sf(aes(fill = turnout_pct_increase), color = "black", size = 0.1) +
  scale_fill_viridis_c() +
  theme_void(base_size = 16) +
  labs(title = "Percent increase in Voter Turnout",
       subtitle = "From 2014 to 2018",
       fill = "Increase %",
       caption = "Note: Alaska, Hawaii, and Puerto Rico are shifted and not to scale.") +
  theme(plot.title = element_text(hjust = 0))

gg_2 <- ggplotly(m, tooltip = "text")
gg_2 %>%
  style(
    hoveron = "fills",
    # override the color mapping
    line.color = toRGB("gray40"),
    # don't apply these style rules to the first trace, which is the background graticule/grid
    traces = seq.int(2, length(gg_2$x$data))
  ) %>%
  hide_legend()

lab_1b_data <- read_excel("Excel Lab 1.xlsx", sheet = "Campaign Finance") %>% 
  rename(cost = `Cost of Senate Elections (Winner)`) %>% 
  arrange(Year)

cpi_2018 <- lab_1b_data[lab_1b_data$Year == 2018,][["CPI"]]

lab_1b_data <- lab_1b_data %>% mutate(inflation_rate = (CPI - lag(CPI)) / lag(CPI) * 100) %>% 
  mutate(real_cost = cpi_2018 / CPI * cost)


lab_1b_data %>% ggplot(aes(x = Year, y = cost)) + 
  geom_point(aes(x = Year, y = cost)) + 
  stat_smooth(method = loess) +
  scale_y_continuous(name="Nominal Cost $M", labels = scales::comma)

lab_1b_data %>% ggplot(aes(x = Year, y = real_cost)) + 
  geom_point() + 
  geom_point(aes(x = Year, y = cost)) +
  stat_smooth(method = loess, fullrange = TRUE) +
  scale_y_continuous(name = "Real Cost $M", labels = scales::label_comma())


