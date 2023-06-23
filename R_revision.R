library(tidyverse)

data(iris)

iris.long = iris %>%
  pivot_longer(cols = -Species, names_to = "temp", values_to = "Value") %>%
  #separate(temp, into = c("part", "measure"))
  separate_wider_delim(temp, ".", names = c("Part", "Measurement"))

data = iris$Sepal.Length

mn = numeric(1000)

for (i in 1:1000)
{
  temp = sample(data,replace = T)
  mn[i] = mean(temp)
}

lci = quantile(mn,0.025)
mean = median(mn)
rci = quantile(mn,0.975)

mean(data)
