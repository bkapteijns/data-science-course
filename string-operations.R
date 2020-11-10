library(dslabs)
library(tidyverse)
data("raw_data_research_funding_rates")
tab <- str_split(raw_data_research_funding_rates, "\n")[[1]]

names_1 <- tab[3] %>% str_trim() %>% str_replace_all(",\\s.", "") %>% str_split("\\s{2,}", simplify=TRUE)
names_2 <- tab[4] %>% str_trim() %>% str_split("\\s+", simplify=TRUE)

tmp_names <- str_c(rep(names_1, each=3), names_2[-1], sep="_")
the_names <- c(names_2[1], tmp_names) %>% str_to_lower() %>% str_replace_all("\\s", "_")

the_data <- tab[6:14] %>% str_trim() %>% str_split("\\s{2,}", simplify=TRUE) %>% data.frame(stringsAsFactors=FALSE) %>% setNames(the_names) %>% mutate_at(-1, parse_number)
the_data %>% summarize(men=mean(success_rates_men), women = mean(success_rates_women), total=mean(success_rates_total))

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE) %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
length(polls[["remain"]] %>% str_subset("%"))
polls[["undecided"]]
