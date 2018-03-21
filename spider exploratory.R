library(ggplot2)
library(viridis)


spiders = read.csv("chuchu.csv", header=T)
head(spiders)
str(spiders)

#linear = read.table("output.csv", header=F,sep=",")
files.linear <- list.files(path="output",full.names = T,pattern = "linear")
tmp.linear=  lapply(files.linear, read.table,  header=F,sep=",")
linear = do.call(rbind, tmp.linear)
colnames(linear)=c("ticks" , "who" , "action_prob" , "danger_level", "danger_type", "replicate_number")
head(linear)
str(linear)


#plot(spiders$action.prob~spiders$danger.level)
#plot(aggregate(x = spiders$action-prob, by = list(spiders$danger-level), FUN=mean ))

agg= aggregate( linear$action_prob, by = list(linear$danger_level,linear$replicate_number), FUN=mean )
plot(x=agg$Group.1,y=agg$x,col=agg$Group.2)

linear.means =

myplot.linear=  ggplot(linear, aes(x=danger_level, y=action_prob, color= danger_type)) + geom_point()  + theme_minimal() +
  # ylim(0, 1) +
  #xlim(0, 1) +
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
x11(); myplot.linear
ggsave("myplot.jpg",device = "jpg")
modelo=lm(spiders$action.prob~spiders$danger.level)
abline(modelo)

summary(modelo)
plot(modelo)
