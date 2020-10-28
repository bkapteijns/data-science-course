# Monte carlo simulation of the certainty of an election, given the spread
library(ggplot2)
library(dplyr)

d <- 0.039 # This is the spread towards obama voters
sizes <- c(453,1365,348,5831,4372,483,978,5467,2144,999) # These are the 10 poll sizes
p <- (d+1)/2 # This is the probability, given the spread. The spread is 2p-1, so this is just arithmetic

confidence_intervals <- sapply(sizes, function(sample_size) {
  X <- sample(c(1, 0), sample_size, replace=TRUE, prob=c(p, 1-p)) # create a sample of voters for obama voters
  X_hat <- mean(X) # Take the proportion of obama voters
  SE_hat <- sqrt(X_hat*(1-X_hat)/sample_size) # Take the standard deviation of obama voters
  2 * c(X_hat, X_hat-2*SE_hat, X_hat+2*SE_hat) - 1 # Take the average spread, the lower bound and the upper bound
})

polls <- data.frame(poll=1:length(sizes),
                    t(confidence_intervals),
                    sample_size=sizes)
names(polls) <- c("polls", "estimate", "low", "high", "poll_size")

d_hat <- polls %>% summarize(avg=sum(estimate*poll_size)/sum(poll_size)) %>% .$avg

p_hat <- (1+d_hat)/2

margin_of_error <- 2 * qnorm(.975) * sqrt(p_hat*(1-p_hat)/sum(polls$poll_size))

polls %>%
  mutate(size=sum(poll_size)) %>%
  mutate(avg_d=sum(estimate*poll_size)/size) %>%
  mutate(avg_p=(1+avg_d)/2) %>%
  mutate(moe=2*sqrt(p/(1-p)/size)) %>%
  mutate(avg_min=avg_d-moe, avg_max = avg_d+moe) %>%
  ggplot(aes(y=polls, x=estimate, title="Polls")) +
    geom_point(aes(col="blue")) +
    geom_errorbar(aes(xmin=low, xmax=high, color="blue")) +
    geom_vline(xintercept=0) +
    geom_point(aes(x=avg_d, y=11, color="red")) +
    geom_errorbar(aes(y=11, xmin=avg_min, xmax=avg_max, color="red"))

p_hat
margin_of_error
