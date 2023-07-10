library(tidyverse)

data.himanshu = read.csv("Bandhavgarh_Final.csv")

hist(data.himanshu$Canopy, breaks = 100)

data.himanshu$Season = factor(data.himanshu$Season,levels = c("Summer","Winter","Monsoon"))

fit = glm(TYPED ~ Season + Sample.Type, data = data.himanshu, family = 'poisson')
summary(fit)

newdata = data.frame(Season = rep(unique(data.himanshu$Season),2), Sample.Type = rep(unique(data.himanshu$Sample.Type), each = 3))

pred = predict(fit, newdata = newdata, type = "link", se.fit = T)

newdata$mean = pred$fit
newdata$se = pred$se.fit

newdata$mean.bt = exp(newdata$mean)
newdata$lci = exp(newdata$mean - 1.96*newdata$se)
newdata$rci = exp(newdata$mean + 1.96*newdata$se)

