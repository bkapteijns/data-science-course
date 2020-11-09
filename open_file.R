library(tidyverse)

this_path <- getwd()
file_path <- system.file("extdata", package="dslabs")
olive_name <- "olive.csv"
gapminder_name <- "life-expectancy-and-fertility-two-countries-example.csv"
olive_path <- file.path(file_path, olive_name)
gapminder_path <- file.path(file_path, gapminder_name)
if (!file.exists(olive_name)) {
  file.copy(olive_path, this_path)
}
if (!file.exists(gapminder_name)) {
  file.copy(gapminder_path, this_path)
}
olive_data <- read.csv(file.path(this_path, olive_name))
gapminder_data_wide <- read_csv(file.path(this_path, gapminder_name))

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
murder_data <- read_csv(tmp_filename)
file.remove(tmp_filename)

gapminder_data_wide

gapminder_data_tidy <- gapminder_data_wide %>% gather("key", "value", -"country") %>% 
  separate(key, c("year", "variable_name"), "_", extra="merge") %>% spread(variable_name, value)
head(gapminder_data_tidy)

#gapminder_data_wide <- gapminder_data_tidy %>% spread(year, fertility)

gapminder_data_tidy %>% group_by(country) %>% ggplot(aes(year, fertility, col=country)) + geom_point()
