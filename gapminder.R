library(dslabs)
library(tidyverse)
library(gridExtra)
library(ggthemes)
library(ggrepel)
data(gapminder)
str(gapminder)

fake_mortality_year <- gapminder %>% filter(!is.na(infant_mortality)) %>% mutate(continent=reorder(continent,infant_mortality)) %>% 
  group_by(continent, year) %>% summarize(tot_inf_mort=sum(infant_mortality))
fake_line_mort_year <- fake_mortality_year %>% ggplot(aes(x=year, y=tot_inf_mort, col=continent)) + geom_line() + 
  ylab("Total infant mortality") +  xlab("Year") + ggtitle("Total child mortality per million people") + theme_economist()
fake_stacked_mort_year <- fake_mortality_year %>% ggplot(aes(x=year, y=tot_inf_mort, fill=continent)) + geom_area(position="stack") + 
  ylab("Total infant mortality") + xlab("Year") + ggtitle("Stacked up total child mortality per million people") +
  theme_economist()

grid.arrange(fake_line_mort_year, fake_stacked_mort_year, ncol=1)

#mortality_year <- gapminder %>% filter(!is.na(infant_mortality)) %>% group_by(continent, year) %>% 
#  summarize(inf_mort_rate=sum(infant_mortality)/sum(population)*1000000) %>% arrange(inf_mort_rate) %>%
#  mutate(continent = reorder(continent, inf_mort_rate, FUN=median))
mortality_year <- gapminder %>% filter(!is.na(infant_mortality)) %>% mutate(mort_rate=infant_mortality/population*1000000) %>% 
  mutate(continent=reorder(continent, mort_rate)) %>% group_by(continent, year) %>% 
  summarize(inf_mort_rate=sum(infant_mortality)/sum(population)*1000000)
line_mort_year <- mortality_year %>% ggplot(aes(x=year, y=inf_mort_rate, col=continent)) + geom_line() + 
  ylab("Total infant mortality") +  xlab("Year") + ggtitle("Total child mortality per million people") + theme_economist()
stacked_mort_year <- mortality_year %>% ggplot(aes(x=year, y=inf_mort_rate, fill=continent)) + geom_area(position="stack") + 
  ylab("Total infant mortality") + xlab("Year") + ggtitle("Stacked up total child mortality per million people") +
  theme_economist()

grid.arrange(line_mort_year, stacked_mort_year, ncol=1)


