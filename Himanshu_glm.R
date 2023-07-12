library(tidyverse)
library(boot)

data.himanshu = read.csv("Bandhavgarh_Final.csv")

hist(data.himanshu$Canopy, breaks = 100)

data.himanshu$Season = factor(data.himanshu$Season,levels = c("Summer","Winter","Monsoon"))
data.himanshu = data.himanshu %>%
  mutate(Canopy = Canopy/100)

fit = glm(Individual.Identified ~ Season*Sample.Type, 
          data = data.himanshu, family = 'binomial')
summary(fit)

newdata = expand.grid(Season=unique(data.himanshu$Season),
                      Sample.Type=unique(data.himanshu$Sample.Type))

pred = predict(fit, newdata = newdata, type = "link", se.fit = T)

newdata$mean = pred$fit
newdata$se = pred$se.fit

newdata$mean.bt = inv.logit(newdata$mean)
newdata$lci = inv.logit(newdata$mean - 1.96*newdata$se)
newdata$rci = inv.logit(newdata$mean + 1.96*newdata$se)




### plotting


library(extrafont)

pd = position_dodge(0.2)

ggp = ggplot(data = newdata, aes(x = Canopy, y = mean.bt, col = Sample.Type)) +
  geom_point(data = data.himanshu, aes(x = Canopy, y = TYPED, col = Sample.Type)) +
  facet_wrap(.~Season, nrow = 3, scales = 'free') +
  geom_ribbon(aes(ymin = lci, ymax = rci), alpha = 0.5, fill = "grey", linetype = 0) +
  geom_line(data = newdata, aes(x = Canopy, y = mean.bt, col = Sample.Type)) +
  theme_bw() +
  xlab("Canopy") +
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





