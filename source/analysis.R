library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")



## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#





## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)


get_year_jail_pop <- function() {
  get_jail_pop_year_data <- incarceration_trends %>%
    select(state,
           total_jail_pop,
           year
        ) %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(get_jail_pop_year_data)
}
View(get_year_jail_pop ())


# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(get_year_jail_pop)  {
  plot_jail_pop_data_for_us <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping=aes(x = year, y = total_jail_pop)) + 
    labs(
      x = "Years",
      y = "Total Population in Jail",
      title = "Total Incarceration Population from 1970-2018",
      caption = "In this graph, we are looking at the total incarceration population from years 1970-2018."
    )
  return(plot_jail_pop_data_for_us)   
} 

plot(plot_jail_pop_for_us(get_year_jail_pop))  

Between the years 1970 and 2018, we have seen a general increase in total incarcerated population
within the United States. We can see that the lowest that the incarcerated population has been was in the 
1970s, and the highest was in the 2010s. We can conclude that there has been a significant increase in 
incarcerated populations, but why has there been such a dramatic increase in jail population? 
  
  
  # TODO: Implement this function 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_jail_pop_by_states <- function(states) {
  states_data <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(states_data)
}

plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, group=state)) +
    geom_line(aes(linetype=state)) + 
    labs(
      x = "Years",
      y = "Total Population in Jail",
      title = "Total Incarceration Population from 1970-2018",
      caption = "In this graph, we are looking at the total incarceration population between years 1970-2018 in the specific states: California, Florida, North Carolina, New York, and Washington."
    )
}

plot(plot_jail_pop_by_states(c("WA", "NC", "CA", "NY", "FL")))

In this line graph we are looking at the total incarceration population from 1970-2018
in five states: California, Florida, North Carolina, New York, and Washington. From
this chart, we can observe a general increase in all five states, except for New York when around
2000 they start to see a general decrease in total numbers. California has had the highest amounts
of incarceration, but that is not surprising due to their population being the largest. 
A question to ask from this chart is was caused New Yorks incarceration decline around
the 2000s? 



#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_king_county_jail_pop_data <- incarceration_trends %>%
    select(
      year,
      state,
      female_jail_pop,
      male_jail_pop,
      county_name,
    ) 
View(get_king_county_jail_pop_data)

plot_king_county_data_1970_to_2018 <- function() { 
  king_county_plot <- ggplot(data = get_king_county_jail_pop_data, aes (x = year)) +
  geom_line(aes(y=female_jail_pop, color="female")) +
  geom_line(aes(y=male_jail_pop, color="male")) +
    labs(
      x = "Years (1970-2018)", 
      y = "Jail Population in King County by Male and Female",
      title = "Jail Population in King County by Gender from 1970-2018",
      caption = "In this chart the number of people incarcerated in King County, WA, from 1970 to 2018 by gender."
    )
  return(king_county_plot)
}
plot(plot_king_county_data_1970_to_2018())

In this chart we are looking at King County, Washingtons total jail population by gender 
from 1970-2018. In this chart we can see an increase in jail population for both genders until 
the 2000s, however, there has been a decline in total numbers since. Another thing we can 
observe is how the population has a significanlty higher incarcerated male population
compared to females. Why do men have a much higher rate of incarceration? 
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
us_total_jail_pop_county_state_year <- incarceration_trends %>%
  select(
    year, 
    county_name, 
    state,
    total_pop,
    total_jail_pop
  )
View(us_total_jail_pop_county_state_year)

us_total_jail_pop_county_state_year_2018 <- us_total_jail_pop_county_state_year %>%
  filter(year == 2018) %>%
  filter(state %in% c("CA", "TX", "FL", "NY", "PA", "IL", "OH", "GA", "NC", "MI")) %>%
  group_by(state) %>%
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE), total_pop=sum(total_pop, na.rm = TRUE), jail_pop_ratio=(sum(total_jail_pop/total_pop)))

View(us_total_jail_pop_county_state_year_2018)

jail_pop_ratio <- us_total_jail_pop_county_state_year_2018 %>%
  select(
    jail_pop_ratio,
    state
  )
View(jail_pop_ratio)

states_for_plot <- us_total_jail_pop_county_state_year_2018(
    state = ("CA", "TX", "FL", "NY", "PA", "IL", "OH", "GA", "NC", "MI"),
    lat = c(36.7783, 31.9686, 27.6648, 40.7128, 41.2033, 40.6331, 40.4173, 32.1656, 35.7596, 44.3148),
    long = c(119.4179, 99.9018, 81.5158, 74.0060, 77.1945, 89.3985, 82.9071, 82.9001, 79.0193, 85.6024)
) 

state_shape <- map_data("state")
ggplot(state_shape) +
  geom_polygon(
    mapping  = aes(x = long, y = lat, state = state), 
      geom_point(
        data = jail_pop_ratio,
        mapping = aes(x = long, y = lat),
    color = "blue",
    size = .2
    caption = "This map is showing the highest populated states with the ratio of their population incarcerated."
  )) + 
  
  coord_map()

This map is showing the top ten most populated states in the United States, and there
ratio of population in jail compared to the total population. From this map is it visible 
that states with the highest populations, for example, California, do not always have the 
highest percentage of amount of people in jail. For example, Georgia, has the highest
percentage of people in jail even though it is not the highest populated states. Why
do some states have a higher ratio of imprisoned people than other states? 




## Load data frame ---- 


