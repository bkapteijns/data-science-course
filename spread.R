# Difference/spread of 4%: 2% on both sides
p <- 0.50
results <- replicate(500000, {
  mean(sample(c(0, 1), 100, replace=TRUE, prob=c(p, 1-p)))
})
hist(results)

1-(mean(results<.52)-mean(results<.48))
mean(results >= .52)+mean(results < .48)

1-(pnorm(.52, mean=.50, sd=.50/sqrt(100))-pnorm(.48, mean=.50, sd=.50/sqrt(100)))
