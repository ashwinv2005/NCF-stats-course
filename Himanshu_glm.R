library(tidyverse)

data.himanshu = read.csv("Bandhavgarh_Final.csv")

hist(data.himanshu$Canopy, breaks = 100)

data.himanshu$Season = factor(data.himanshu$Season,levels = c("Summer","Winter","Monsoon"))
data.himanshu = data.himanshu %>%
  mutate(Canopy = Canopy/100)

fit = glm(TYPED ~ Sample.Type + Season:Sample.Type, 
          data = data.himanshu, family = 'poisson')
summary(fit)

newdata = data.frame(Season = rep(unique(data.himanshu$Season),2), Sample.Type = rep(unique(data.himanshu$Sample.Type), each = 3))

pred = predict(fit, newdata = newdata, type = "link", se.fit = T)

newdata$mean = pred$fit
newdata$se = pred$se.fit

newdata$mean.bt = exp(newdata$mean)
newdata$lci = exp(newdata$mean - 1.96*newdata$se)
newdata$rci = exp(newdata$mean + 1.96*newdata$se)




### plotting


library(extrafont)

pd = position_dodge(0.2)

ggp = ggplot(data = newdata, aes(x = Season, y = mean.bt, col = Sample.Type)) +
  geom_boxplot(data = data.himanshu, aes(x = Season, y = TYPED, col = Sample.Type), position = pd) +
  #facet_wrap(.~, nrow = 2, scales = 'free') +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.05, linewidth = 1, position = pd) +
  theme_bw() +
  xlab("Season") +
  ylab("No. of Snips") +
  theme(strip.text.x = element_text(size = 15))

ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_color_manual(breaks = c("SHED HAIR","SALIVA"), values = c("red","blue")) +
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





