library(ggplot2)
library(viridis)


#linear = read.table("output.csv", header=F,sep=",")
files.linear <- list.files(path="output",full.names = T,pattern = "linear")
tmp.linear=  lapply(files.linear, read.table,  header=F,sep=",")
linear = do.call(rbind, tmp.linear)
colnames(linear)=c("ticks" , "who" , "action_prob" , "danger_level", "danger_type", "replicate_number")
head(linear)
str(linear)

agg= aggregate( linear$action_prob, by = list(linear$danger_level,linear$replicate_number), FUN=mean )

myplot.linear=  ggplot(linear, aes(x=danger_level, y=action_prob,aplha=0.3)) + geom_point()  + theme_minimal() +
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
  labs(x="Nível de perigo",y="Personalidade",color="Pers.")+scale_fill_viridis()
#x11(); myplot.linear
ggsave("myplot2.jpg",device = "jpg")
head(linear)
mean.linear= aggregate(x = linear$action_prob, by=list(linear$danger_level), FUN= mean)
plot(mean.linear)
sd.linear= aggregate(x = linear$action_prob, by=list(linear$danger_level), FUN= sd)
plot(sd.linear)


files <- list.files(path="output",full.names = T,pattern = ".csv")
tmp =  lapply(files, read.table,  header=F,sep=",")
spiders = do.call(rbind, tmp)
colnames(spiders)=c("ticks" , "who" , "action_prob" , "danger_level", "danger_type", "replicate_number","par")
spiders$danger_type=as.factor(spiders$danger_type)
str(spiders)
p <- ggplot(spiders, aes(x=spiders$danger_type, y=spiders$action_prob)) + 
  #geom_boxplot(notch=TRUE) +
  scale_color_viridis() + geom_violin() + ggtitle("several sigmoid")
ggsave("several sigmoid.jpg",device = "jpg")
x11();p

modelo=lm(spiders$action_prob ~ spiders$danger_type)
mod= aov(spiders$action_prob ~ spiders$danger_type)
tk=TukeyHSD(modelo) 
print(tk)
summary(modelo)
rm(ls="modelo")
gc()
