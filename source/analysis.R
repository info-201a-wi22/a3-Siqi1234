library("tidyverse")
library("stringr")
library("ggplot2")
library("usmap")
library("plotly")


data <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration <- read.csv(data, header = TRUE, stringsAsFactors = FALSE)
View(incarceration)


#1 the proportion of different races in the jail and population of different 
#races 

incarceration <- mutate(
  incarceration, 
  prop_jail_aapi = aapi_jail_pop / total_jail_pop, na.rm = TRUE,
  prop_jail_black = black_jail_pop / total_jail_pop, na.rm = TRUE,
  prop_jail_latinx = latinx_jail_pop / total_jail_pop, na.rm = TRUE,
  prop_jail_native = native_jail_pop / total_jail_pop, na.rm = TRUE,
  prop_jail_white = white_jail_pop / total_jail_pop, na.rm = TRUE,
  prop_aapi_pop = aapi_pop_15to64 / total_pop_15to64, na.rm = TRUE,
  prop_black_pop = black_pop_15to64 / total_pop_15to64, na.rm = TRUE,
  prop_latinx_pop = latinx_pop_15to64 / total_pop_15to64, na.rm = TRUE,
  prop_white_pop = white_pop_15to64 / total_pop_15to64, na.rm = TRUE,
  prop_native_pop = native_pop_15to64 / total_pop_15to64, na.rm = TRUE
)

ave_prop_races_jail <- incarceration %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(ave_jail_white = mean(prop_jail_white, na.rm = TRUE), 
            ave_prop_white_pop = mean(prop_white_pop, na.rm = TRUE),
            ave_jail_aapi = mean(prop_jail_aapi, na.rm = TRUE), 
            ave_prop_aapi_pop = mean(prop_aapi_pop, na.rm = TRUE),
            ave_jail_black = mean(prop_jail_black, na.rm = TRUE), 
            ave_prop_black_pop = mean(prop_black_pop, na.rm = TRUE),
            ave_jail_latinx = mean(prop_jail_latinx, na.rm = TRUE), 
            ave_prop_latinx_pop = mean(prop_latinx_pop, na.rm = TRUE),
            ave_jail_native = mean(prop_jail_native, na.rm = TRUE), 
            ave_prop_native_pop = mean(prop_native_pop, na.rm = TRUE)) %>%
  select(year, ave_jail_white, ave_prop_white_pop, ave_jail_aapi,
       ave_prop_aapi_pop, ave_jail_black, ave_prop_black_pop, 
       ave_jail_latinx, ave_prop_latinx_pop, ave_jail_native, 
       ave_prop_native_pop)
            

View(ave_prop_races_jail)




#2 highest number of jail in different races


highest_black_jail <- incarceration %>%
  filter(year == 2018) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  select(year, county_name, state, black_jail_pop) 
View(highest_black_jail)

highest_white_jail <- incarceration %>%
  filter(year == 2018) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>%
  select(year, county_name, state, white_jail_pop)
View(highest_white_jail)

highest_latinx_jail <- incarceration %>%
  filter(year == 2018) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>%
  select(year, county_name, state, latinx_jail_pop)
View(highest_latinx_jail)

highest_native_jail <- incarceration %>%
  filter(year == 2018) %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE)) %>%
  select(year, county_name, state, native_jail_pop)
View(highest_native_jail)

highest_aapi_jail <- incarceration %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>%
  select(year, county_name, state, aapi_jail_pop)
View(highest_aapi_jail)


#3 


state_black_jail <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE))
View(state_black_jail)

state_white_jail <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE))
View(state_white_jail)

state_latinx_jail <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE))
View(state_latinx_jail)

state_native_jail <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_native_jail_pop = sum(native_jail_pop, na.rm = TRUE))
View(state_native_jail)


#4 


year_state_black_jail <- incarceration %>%
  filter(year >= 1985) %>%
  group_by(year, state) %>%
  summarise(total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(state == "CA" | state == "FL" | state == "GA" | state == "TX" | state == "LA")

View(year_state_black_jail)





#chart1


c <- plot_ly(
  data = year_state_black_jail,
  x = ~year,      
  y = ~total_black_jail_pop, 
  linetype = "lines",
  name = ~state, 
  color = ~state,
  alpha = .7,     
  hovertext = "y"
) %>%
  layout(
    title = "Jail Population of Black People in Top 5 5tates (1985-2018)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Jail Population of black people"))

c <- c %>% 
  layout(legend = list(title = list(text = "<b> state </b>")))

    



#chart2

urbanicity_black_jail_pop <- incarceration %>%
  filter(year >= 1985) %>%
  group_by(year, urbanicity) %>%
  summarise(total_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(urbanicity == "urban" | urbanicity == "rural") %>%
  mutate(race = "black")

urbanicity_white_jail_pop <- incarceration %>%
  filter(year >= 1985) %>%
  group_by(year, urbanicity) %>%
  summarise(total_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  filter(urbanicity == "urban" | urbanicity == "rural") %>%
  mutate(race = "white")
  

two <- ggplot(NULL, mapping = aes(year, total_jail_pop,
                                color = urbanicity, shape = race)) +
  geom_line(data = urbanicity_black_jail_pop) +
  geom_point(data = urbanicity_white_jail_pop) +
  labs(
    title = "Jail Population of White and Black People in Rural and Urban",
    x = "year", 
    y = "population of races in jail"
    )

#map

d <- incarceration %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(black_pop = sum(black_jail_pop, na.rm = TRUE)) 
  
map <- plot_usmap(data = d, values = "black_pop", color = "gray") +
  scale_fill_continuous(
    low = "white", high = "red", name = "black jail population (2018)", 
    label = scales::comma) +  
  theme(legend.position = "right",
             plot.background = element_blank(),  
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             panel.border = element_blank()) +
  labs(title = "Black Jail Population in Each US State in 2018")
                                            