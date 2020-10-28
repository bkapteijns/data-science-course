# As we can see here, the result of the election is not between the 95% confidence interval. This is due to pollster-biases.

library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state=="U.S." & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)

d_hat <- polls %>% summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat
p_hat <- (d_hat+1)/2
moe <- 2*qnorm(.975)*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe
"This is the actual spread: 2.1"

polls %>%
  group_by(pollster) %>%
  filter(n()>=2) %>%
  ggplot(aes(x=pollster, y=spread)) +
  geom_point()

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)
