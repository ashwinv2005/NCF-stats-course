library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(extrafont)

timegroups = c("before 2000","2000-2006","2007-2010","2011-2012",
                              "2013","2014","2015","2016","2017","2018","2019",
                              "2020","2021","2022")
years = c(1992, 2003, 2009, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
          2021, 2022)

data2 = c(7884, 7293, 7642, 7123, 12146, 35322, 64899, 96052, 129610, 143715, 186179,
         226262, 266450, 305984)

data1 = c(4606, 5131, 5787, 5459, 11002, 34926, 64662, 96012, 131280, 144533, 186179,
          226262, 266450, 305984)

a = data.frame(timegroups = timegroups, years = years, soib1 = data1, soib2 = data2)
a$ratio = a$soib2/a$soib1

tg = c("before 2000", "2000-2006", "2007-2010", "2011-2012", "2013", "2014", "2015", 
       "2016", "2017", "2018", "2019", "2020", "2021", "2022")
x_tick_pre2000Bas = seq(1991, 2022) + 0.5

ggp = ggplot(a, aes(x = years, y = ratio)) +
  geom_point(size = 3, color = "black") +
  geom_label(aes(label = soib2)) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(1990, 2000, 2006, 2010, 2012, seq(2013, 2021)) + 0.5, 
    xmax = c(2000, 2006, 2010, 2012, seq(2013, 2022)) + 0.5,
    y.position = -0.01,
    bracket.shorten = 0.15,
    tip.length = 0.03,
    vjust = 2.5,
    label = tg,
    label.size = 3) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(2015) - 0.5, 
    xmax = c(2022) + 0.5,
    y.position = -0.1,
    bracket.shorten = 0.15,
    tip.length = 0.02,
    vjust = 2.1,
    label = "Current Trend",
    label.size = 3) +
  scale_x_continuous(
    expand=c(0,0),
    breaks = c(seq(1991, 2022), x_tick_pre2000Bas),
    labels = c("", "1992", "1993", rep(c(""), 2000-1992-2),
               "2000", "2001", rep(c(""), 2006-2000-2),
               "2006", "2007", rep(c(""), 2010-2006-2), 
               "2010", "2011", rep(c(""), 2012-2010-2), 
               paste0(seq(2012, 2022)), rep(c(""), length(x_tick_pre2000Bas))),
    limits = c(1990, 2024.7)) +
  geom_segment(x = 1992, y = 1, xend = 2022, yend = 1, 
               linetype = "dotted", linewidth = 0.7, col = "black") +
  xlab("Time-steps") +
  ylab("Proportional change in complete lists between SoIB 1 and 2")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20, colour = "#56697B")) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),position = "left")+
  coord_cartesian(ylim = c(-0.2,2.2), clip="off")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  guides(colour = "none")

jpeg("soib.comp.jpg", units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx)
dev.off()
