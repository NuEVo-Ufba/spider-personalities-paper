ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)

spiders = read.csv("chuchu.csv", header=T)
plot(spiders$action.prob~spiders$danger.level)
plot(aggregate(x = spiders$action.prob, by = list(spiders$danger.level), FUN=mean ))
library(ggplot2)
library(viridis)
myplot=ggplot(spiders, aes(x=danger.level, y=action.prob, color= action.prob)) + geom_point() + scale_color_viridis() + theme_minimal() +
  ylim(0, 1) +
  xlim(0, 1) +
   theme(
     axis.line.y =  element_line(colour = "black"),
     axis.line.x =  element_line(colour = "black"),
     axis.ticks.y = element_line(colour = "black"),
     axis.ticks.x = element_line(colour = "black"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank(),
     panel.background = element_blank()) +
  labs(x="Nível de perigo",y="Personalidade",color="Pers.")

ggsave("myplot.jpg",device = "jpg")
modelo=lm(spiders$action.prob~spiders$danger.level)
abline(modelo)

summary(modelo)
plot(modelo)
