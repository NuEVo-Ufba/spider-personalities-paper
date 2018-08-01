library("ggplot2")
sigmoid = function(x, k, x0) {
  1 / ( 1 + (exp (k * (x - x0))))
}

k = c(-10, -7, -5, -3, -1)
x0 = c(0.1, 0.3, 0.5, 0.7, 0.9)
x = seq(0,1,0.001)
y=sigmoid(x,k[5],x0[5])
xy=data.frame(x,y)

ggplot(data=xy,aes(x,y))+geom_line()+
  theme(
    axis.line.y =  element_line(colour = "black"),
    axis.line.x =  element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  labs(x="",y="Risco")
i=1
j=1
shape=NA
ifelse(!dir.exists(file.path(getwd(), "plots")), dir.create(file.path(getwd(), "plots")), FALSE) # verifica se existe diret?rio, coloca um volume em cada diret?rio

for (i in 1:length(k)){
  for(j in 1:length(x0)){
    y=sigmoid(x,k[i],x0[j])
    xy=data.frame(x,y)
    shape = paste0("Risco ","k = ", k[i]," x0 = ", x0[j])
    # p=ggplot(data=xy,aes(x,y))+geom_line()+
    #   theme(
    #     axis.line.y =  element_line(colour = "black"),
    #     axis.line.x =  element_line(colour = "black"),
    #     axis.ticks.y = element_line(colour = "black"),
    #     axis.ticks.x = element_line(colour = "black"),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.border = element_blank(),
    #     panel.background = element_blank())+
    #   labs(x="",y=shape)
    # print(p)
    filename=paste0(getwd(),"/plots/",shape,".jpg")
    jpeg(filename)
      plot(xy$x,xy$y)
    dev.off()
    #ggsave(filename= paste0(file.path(getwd(), "plots"),shape,".jpg"),plot = p, device = "jpg")
    
  }
}
