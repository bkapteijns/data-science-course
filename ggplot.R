library(tidyverse)
library(dslabs)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(gridExtra)
data(murders)

r <- murders %>% summarize(rate=sum(total)/sum(population) * 10^6) %>% .$rate
params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd=sd(height))

p <- murders %>% ggplot(aes(population/10^6, total))
p <- p + geom_abline(intercept=log10(r), lty=2, color="darkgrey") + geom_point(aes(col=region),size=2) + scale_color_discrete("Region")
p <- p + geom_text_repel(aes(label=abb))
#p <- p + scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10")
p <- p + scale_x_log10() + scale_y_log10()
p <- p + xlab("Population in millions") + ylab("Total number of murders") + ggtitle("US Gun Murders in US 2010")
p <- p + theme_economist()
p

h_ <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height, sample=height))
h <- h_ + geom_histogram(binwidth=1, fill="blue", col="black") + xlab("Male heights in inches") + ggtitle("Heights")
d <-  heights %>% filter(sex=="Male") %>% ggplot(aes(x=height, sample=height))
d <- d + geom_density(fill="blue")
q <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height)) + geom_qq(dparams=params) + geom_abline()

grid.arrange(p,h,d,q,ncol=2)
