library(rvest)
library(dslabs)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- co2_wide %>% gather(month, co2, -year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data(admissions)
dat_tidy <- admissions %>% select(-applicants) %>% spread(gender, admitted)
dat_tidy

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
col_names <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1, -1]
tab_2 <- tab_2[-1,]
names(tab_2) <- col_names
names(tab_1) <- col_names
full_join(tab_1,tab_2, by = "Team")


url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- url %>% read_html() %>% html_nodes("table")
tab[5] %>% html_table(fill=TRUE)
