library(dslabs)
library(tidyverse)
library(lubridate)
data("brexit_polls")
sum(month(brexit_polls$startdate) == 4)
table(weekdays(brexit_polls$enddate))

data(movielens)
movielens %>% mutate(hour=hour(as_datetime(timestamp))) %>% group_by(hour) %>% summarize(len=length(timestamp)) %>% arrange(desc(len))

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
names(gutenberg_metadata)
gutenberg_works(language=="en") %>% filter(str_detect(title, 'Pride and Prejudice'))
gutenberg_download(gutenberg_id=1342) %>% unnest_tokens(word, text) %>% 
  select(word) %>% anti_join(stop_words) %>% filter(!str_detect(word, "[0-9]")) %>% 
  group_by(word) %>% summarize(len=length(word)) %>% filter(len>100) %>% summarize(length(len))
stop_words

afinn <- get_sentiments("afinn")
length(afinn)
