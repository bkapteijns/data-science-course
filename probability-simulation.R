repetitions <- 10000
box <- rep(c("red", "blue"), times=c(2, 3))
amounts <- replicate(repetitions, sample(box,1))
tab <- table(amounts)
# proportions using replicate
prop.table(tab)
# proportions using sample (so not replicate)
prop.table(table(sample(box,repetitions,replace=TRUE)))
# proportions without using table
mean(sample(box, repetitions, replace=TRUE)=="red")
