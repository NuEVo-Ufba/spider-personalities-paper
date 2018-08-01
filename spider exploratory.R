library(ggplot2)
library(viridis)


#linear = read.table("output.csv", header=F,sep=",")


# files.linear <- list.files(path=path,full.names = T,pattern = "linear")
# tmp.linear=  lapply(files.linear, read.table,  header=F,sep=",")
# linear = do.call(rbind, tmp.linear)
# colnames(linear)=c("ticks" , "who" , "action_prob" , "danger_level", "danger_type", "replicate_number")
# head(linear)
# str(linear)
# 
# agg= aggregate( linear$action_prob, by = list(linear$danger_level,linear$replicate_number), FUN=mean )
# 
# myplot.linear=  ggplot(linear, aes(x=danger_level, y=action_prob,aplha=0.3)) + geom_point()  + theme_minimal() +
#   # ylim(0, 1) +
#   #xlim(0, 1) +
#   theme(
#     axis.line.y =  element_line(colour = "black"),
#     axis.line.x =  element_line(colour = "black"),
#     axis.ticks.y = element_line(colour = "black"),
#     axis.ticks.x = element_line(colour = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank()) +
#   labs(x="Nível de perigo",y="Personalidade",color="Pers.")+scale_fill_viridis()
# #x11(); myplot.linear
# ggsave("myplot2.jpg",device = "jpg")
# head(linear)
# mean.linear= aggregate(x = linear$action_prob, by=list(linear$danger_level), FUN= mean)
# plot(mean.linear)
# sd.linear= aggregate(x = linear$action_prob, by=list(linear$danger_level), FUN= sd)
# plot(sd.linear)
getwd()
path = file.path(paste0(getwd(),"/output"))
files <- list.files(path=path,full.names = T,pattern = ".csv")
tmp =  lapply(files, read.table,  header=F,sep=",")
spiders = do.call(rbind, tmp)
str(spiders)
summary(spiders)
colnames(spiders)=c("ticks" , "who" , "action_prob" , "danger_level", "danger_shape","distribution", "replicate_number", "k", "x0","a","b")#,"par")
spiders$danger_shape=as.factor(spiders$danger_shape)
spiders$distribution=as.factor(spiders$distribution)
str(spiders)

l=levels(spiders$danger_shape)
l   
spiders$danger_shape<- factor(spiders$danger_shape, levels = c("k -10 x0 0.3", "k -10 x0 0.5", "k -10 x0 0.7", "k -10 x0 0.9",  "k -7 x0 0.3" , "k -7 x0 0.5" ,"k -7 x0 0.7" , "k -7 x0 0.9" ,  "k -5 x0 0.5", "k -5 x0 0.7", "k -5 x0 0.9", "k -3 x0 0.5",   "k -3 x0 0.7" ,"k -3 x0 0.9" ))

  



# p <- ggplot(spiders, aes(x=spiders$danger_shape, y=spiders$action_prob)) + 
#   scale_y_continuous(limits = c(0, 1))+
#  # geom_boxplot(notch=TRUE) +
#  # scale_fill_viridis() + 
#   #geom_violin(adjust = 1.5) + #ggtitle("several sigmoid")+
#    geom_boxplot(notch=TRUE) +
#   theme(
#     axis.line.y =  element_line(colour = "black"),
#     axis.line.x =  element_line(colour = "black"),
#     axis.ticks.y = element_line(colour = "black"),
#     axis.ticks.x = element_line(colour = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank()) +
#   labs(x="Estrutura da teia",y="Indice de Personalidade",color="Pers.") #+   scale_fill_viridis()
# x11();p
# ggsave("novas.jpg",device = "jpg")
install.packages("ggpubr")
library("ggpubr")
quartz();ggboxplot(spiders, x  = "distribution", y = "action_prob", color="danger_shape")
x11();ggviolin(spiders, x  = "distribution", y = "action_prob", color="danger_shape")
,add = "boxplot")

modelo=lm(spiders$action_prob ~ spiders$danger_type)
mod= aov(spiders$action_prob ~ spiders$danger_type)
tk=TukeyHSD(modelo) 
print(tk)
summary(modelo)
rm(ls="modelo")
gc()
