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


## Make this into a 'function' for tidyverse

bootst = function(vec) {
  mn = numeric(1000)
  
  for (i in 1:1000)
  {
    temp = sample(vec, replace = T)
    mn[i] = mean(temp)
  }
  
  lci = quantile(mn,0.025)
  med = median(mn)
  rci = quantile(mn,0.975)
  
  res = c(lci,med,rci)
  
  return(res)
}


iris.summary = iris.long %>%
  group_by(Species,Part,Measurement) %>%
  reframe(lci = mean(Value) - 1.96*sd(Value)/sqrt(n()),
          Mean = mean(Value),
          rci = mean(Value) + 1.96*sd(Value)/sqrt(n()),
          lci.boot = bootst(Value)[1],
          Mean.boot = bootst(Value)[2],
          rci.boot = bootst(Value)[3])


ggp = ggplot(data = iris.long, aes(x = Value)) +
  facet_wrap(.~Species + Part + Measurement, scales = 'free') +
  geom_histogram(bins = 20) +
  theme_bw() +
  xlab("Measurement") +
  ylab("Count") +
  theme(strip.text.x = element_text(size = 15))

ggp +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")

ggp = ggplot(data = iris.long, aes(x = Value)) +
  facet_grid(Species + Part ~ Measurement, scales = 'free') +
  geom_histogram(bins = 20) +
  theme_bw() +
  xlab("Measurement") +
  ylab("Count") +
  theme(strip.text.x = element_text(size = 15))

ggp +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")

iris.long = iris.long %>%
  group_by(Species,Part,Measurement) %>%
  mutate(type = paste(Species,Part,Measurement)) 

ggp = ggplot(data = iris.long, aes(x = Value)) +
  facet_wrap(.~type, scales = 'free') +
  geom_histogram(bins = 20) +
  theme_bw() +
  xlab("Measurement") +
  ylab("Count") +
  theme(strip.text.x = element_text(size = 10))

ggp +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")





################ plot means and SEs

library(extrafont)

pd = position_dodge(0.2)

iris.summary$Species = factor(iris.summary$Species, levels = c("versicolor","virginica","setosa"))

ggp = ggplot(data = iris.summary, aes(x = Measurement, y = Mean, col = Species)) +
  facet_wrap(.~Part, nrow = 2, scales = 'free') +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = lci.boot, ymax = rci.boot), width = 0.05, linewidth = 1, position = pd) +
  theme_bw() +
  xlab("Measurement") +
  ylab("Length/Width (cm)") +
  theme(strip.text.x = element_text(size = 15))

ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_color_manual(breaks = c("versicolor","virginica","setosa"), values = c("red","green","blue")) +
  #scale_x_discrete(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")



