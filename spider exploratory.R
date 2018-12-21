library(ggplot2)
library(ggridges)
library(viridis)
library(dplyr)
library(broom)
library(forcats)
#library(stargazer)
#library(knitr)
#library(rmarkdown)
#library(data.table)
#library(psycho)
#library(lme4)
getwd()
path <- file.path(paste0(getwd(), "/output"))
files <- list.files(path = path, full.names = T, pattern = ".csv")
#tmp <- lapply(files, read.table, header = F, sep = ",")
#spiders <- do.call(rbind, tmp)
spiders=data.table::rbindlist(lapply(files,function(file){
  dt=fread(file)}
)
)
str(spiders)
summary(spiders)
colnames(spiders) <- c("ticks", "who", "personality", "danger_level", "danger_shape", "distribution", "replicate_number", "k", "x0", "a", "b") # ,"par")
spiders$danger_shape <- as.factor(spiders$danger_shape)
spiders$distribution <- as.factor(spiders$distribution)
spiders$x0 <- as.factor(spiders$x0)
spiders$k <- as.factor(spiders$k)

str(spiders)

load("rdata.xuxu.Rdata")
str(spiders)
write.csv(spiders,"spiders.csv")
spd <- dplyr::select(spiders, personality, danger_level, danger_shape, distribution,k,x0)
str(spd)
spd <- dplyr::select(spd, personality,danger_shape, distribution,k,x0)
str(spd)
levels(spd$distribution)
spd$distribution <- forcats::fct_recode(spd$distribution,
                                        "All bold (0.9)" = "all-bold",
                                        "Normal (mean 0.9)" = "all-bold-normal",
                                        "Normal (mean 0.5)" = "normal",
                                        "Uniform" = "uniform"
)
levels(spd$danger_shape)

spd$danger_shape <- fct_recode(spd$danger_shape,
                               "k = -10\nx0 = 0.5" = "k -10 x0 0.5",
                               "k = -10\nx0 = 0.7" = "k -10 x0 0.7",
                               "k = -10\nx0 = 0.9" = "k -10 x0 0.9",
                               "k = -05\nx0 = 0.7" = "k -5 x0 0.7",
                               "k = -05\nx0 = 0.9" = "k -5 x0 0.9",
                               "k = -07\nx0 = 0.7" = "k -7 x0 0.7",
                               "k = -07\nx0 = 0.9" = "k -7 x0 0.9",
                               "k = -03\nx0 = 0.9" = "k -3 x0 0.9",
                               "k = -10\nx0 = 0.3" = "k -10 x0 0.3",
                               "k = -03\nx0 = 0.7" = "k -3 x0 0.7",
                               "k = -05\nx0 = 0.5" = "k -5 x0 0.5",
                               "k = -07\nx0 = 0.3" = "k -7 x0 0.3",
                               "k = -07\nx0 = 0.5" = "k -7 x0 0.5",
                               "k = -03\nx0 = 0.5" = "k -3 x0 0.5"
)
levels(spd$danger_shape)
summary(spd)
p <- ggplot(spiders, aes(x = danger_shape, y = personality, fill = danger_shape)) + geom_boxplot()
x11()
p + facet_wrap(~distribution, ncol = 2)

z <- ggplot(spd, aes(x = danger_shape, y = personality)) +
  facet_wrap(~distribution, ncol = 2) +
  geom_boxplot(position = position_dodge(1)) +
  theme_light() +
  theme( # text=element_text(family="mono"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  xlab("Danger distribution parameters") +
  ylab("Personality")
x11(type="cairo")
z

ggsave(file = "a4_output2.jpg", plot = z, height = 210, width = 297, units = "mm")
# spd$danger_shape <- fct_recode(spd$danger_shape,
#                                "k = -10 x0 = 0.5" = "k = -10\nx0 = 0.5",
#                                "k = -10 x0 = 0.7" = "k = -10\nx0 = 0.7",
#                                "k = -10 x0 = 0.9" = "k = -10\nx0 = 0.9",
#                                "k = -05 x0 = 0.7" = "k = -05\nx0 = 0.7",
#                                "k = -05 x0 = 0.9" = "k = -05\nx0 = 0.9",
#                                "k = -07 x0 = 0.7" = "k = -07\nx0 = 0.7",
#                                "k = -07 x0 = 0.9" = "k = -07\nx0 = 0.9",
#                                "k = -03 x0 = 0.9" = "k = -03\nx0 = 0.9",
#                                "k = -10 x0 = 0.3" = "k = -10\nx0 = 0.3",
#                                "k = -03 x0 = 0.7" = "k = -03\nx0 = 0.7",
#                                "k = -05 x0 = 0.5" = "k = -05\nx0 = 0.5",
#                                "k = -07 x0 = 0.3" = "k = -07\nx0 = 0.3",
#                                "k = -07 x0 = 0.5" = "k = -07\nx0 = 0.5",
#                                "k = -07 x0 = 0.5" = "k = -07\nx0 = 0.5"
# )

sp2norm <- dplyr::filter(spd, distribution == "Normal (mean 0.5)")
sp2bold <- dplyr::filter(spd, distribution == "All bold (0.9)")
sp2normbol <- dplyr::filter(spd, distribution == "Normal (mean 0.9)")
sp2uni <- dplyr::filter(spd, distribution == "Uniform")

table(sp2norm[,4:5])
table(sp2bold[,4:5])
table(sp2normbol[,4:5])
table(sp2uni[,4:5])

table(sp2norm[,3:5])
table(sp2bold[,4:5])
table(sp2normbol[,4:5])
table(sp2uni[,4:5])


str(sp2norm)
nor <- aov(personality ~ x0 + k, data = sp2norm)
nor2=lmer(personality ~ k +(1|x0), data=sp2norm)
nor2
tidynorm=tidy(nor2)
analyze(nor2)
#%>% 
TukeyHSD() %>% tidy() %>% filter(adj.p.value <= 0.05)
nor
analyze(nor)
summary(nor)
nor$comparison <- gsub(nor$comparison, pattern = "-k", replacement = "   k")
nor$term <- gsub(nor$term, pattern = "danger_shape", replacement = " Danger shape")
write.csv(nor, "tukey.sp2norm.csv")
writeLines(kable(nor,format = "latex", digits=3), "tukey.sp2norm.tex")
render("tukey.sp2norm.tex","pdf_document")


bold <- aov(personality ~ danger_shape, data = droplevels(sp2bold)) %>% TukeyHSD() %>% tidy() %>% filter(adj.p.value <= 0.05)
bold$comparison <- gsub(bold$comparison, pattern = "-k", replacement = "  k")
write.csv(bold, "tukey.sp2bold.csv")
write.table(kable(bold), "tukey.sp2bold.tex")

normbol <- aov(personality ~ danger_shape, data = droplevels(sp2normbol)) %>% TukeyHSD() %>% tidy() %>% filter(adj.p.value <= 0.05)
normbol$comparison <- gsub(normbol$comparison, pattern = "-k", replacement = "  k")
write.csv(normbol, "tukey.sp2normbold.csv")
write.table(stargazer(normbol), "tukey.sp2normbold.tex")

uni <- aov(personality ~ danger_shape, data = droplevels(sp2uni)) %>% TukeyHSD() %>% tidy() %>% filter(adj.p.value <= 0.05)
uni$comparison <- gsub(uni$comparison, pattern = "-k", replacement = "  k")
write.csv(uni, "tukey.sp2uni.csv")
write.table(stargazer(uni), "tukey.sp2uni.tex")




### ridgeplots

# install.packages("ggridges")
# library(ggridges)
# data_url = 'http://bit.ly/2cLzoxH'
# # read data from url as dataframe
# gapminder = read.csv(data_url)
# ggplot(gapminder, aes(y=as.factor(year),
#                       x=lifeExp)) +
#   geom_density_ridges(alpha=0.5) +
#   scale_y_discrete(expand = c(0.01, 0)) +  
#   scale_x_continuous(expand = c(0, 0))+
#   theme(axis.text=element_text(size=20))

#Plotando  distribuições finais
#pallete=c("magma", "inferno", "plasma", "viridis", "cividis")

# for (i in 1:length(pallete)) {
#   x11(type = "cairo")
#   p = ggplot(spd, aes( y = as.factor(x0), x = personality
#                        , fill = k
#                        , height = ..density..)) +
#     facet_wrap(~spd$distribution, ncol = 2) +
#     #ggtitle(pallete[i]) +
#     geom_density_ridges(alpha = 0.3, scale = 1, stat = "density") +
#     scale_y_discrete(name = "Refuge Size", expand = c(0.01, 0)) +
#     scale_x_continuous(name = "Personality Index", breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
#     theme(text = element_text(size = 30)) +
#     facet_wrap(~spd$distribution, ncol = 2) +
#     theme_ridges(center_axis_labels = T) +
#     scale_fill_viridis_d(name = "Refuge\nProtection", option = "viridis")
#   x11(type="cairo");print(p)
# }
#  facet_grid(rows = spd$distribution)

#creating initial distributions
#if (pers-dist = "uniform")         [set personality random-float 1]
#if (pers-dist = "all-bold")        [set personality 0.9]
#if (pers-dist = "normal")          [set personality random-normal 0.5 0.1]
#if (pers-dist = "all-bold-normal") [set personality random-normal 0.9 0.1]


set.seed(1)
init.uniform <- rep(  rep(runif(100, min = 0, max = 1), 501) # 501 réplicas
  , 16) # 16 combinaçoes de parâmetros
init.all.bold <- rep(  rep(rep(0.9, 100), 501) # 501 réplicas
  , 16) # 16 combinaçoes de parâmetros
init.normal <- rep(  rep(rnorm(100, mean = 0.5, sd = 0.1), 501) # 501 réplicas
  , 16) # 16 combinaçoes de parâmetros
init.all.bold.normal <- rep(  rep(rnorm(100, mean = 0.9, sd = 0.1), 501) # 501 réplicas
  , 16) # 16 combinaçoes de parâmetros


initials=data.frame("Uniform" = init.uniform,"All bold (0.9)" = init.all.bold,"Normal (mean 0.5)" = init.normal,"Normal (mean 0.9)" = init.all.bold.normal)
initials=tidyr::gather(initials, key = "distribution", value = "personality")
initials$personality[initials$personality >= 0.9 ] = 0.9
initials$personality[initials$personality <= 0.1 ] = 0.1
initials$distribution[initials$distribution == "All.bold..0.9."] ="All bold (0.9)"
initials$distribution[initials$distribution == "Normal..mean.0.5."] ="Normal (mean 0.5)"
initials$distribution[initials$distribution == "Normal..mean.0.9."] ="Normal (mean 0.9)"
unique(initials$distribution)

initials$danger_shape="Initial"
initials$k= "Initial"
initials$x0="Initial"
initials2=data.frame("personality" =initials$personality,"danger_shape" =initials$danger_shape,"distribution" =initials$distribution,"k" =initials$k,"x0" =initials$x0)

ini_spd= rbind(spd,initials2)
rm(ini)
ini_spd$danger_shape <- as.factor(ini_spd$danger_shape)
ini_spd$distribution <- as.factor(ini_spd$distribution)
ini_spd$x0 <- as.factor(ini_spd$x0)
ini_spd$k <- as.factor(ini_spd$k)

ini_spd$x0=fct_relevel(ini_spd$x0, c("Initial","0.3","0.5","0.7","0.9"))
ini_spd$x0=fct_recode(ini_spd$x0,
                       "Initial\ndistribution" = "Initial"
)
ini_spd$k=fct_relevel(ini_spd$k, c("Initial","-10","-7","-5","-3"))
ini_spd$k=fct_recode(ini_spd$k,
                      "Initial distribution" = "Initial"
                     ,"10"="-10"
                     ,"7"="-7"
                     ,"5"="-5"
                     ,"3"="-3"
)

#x11(type = "cairo")
p1=ggplot(ini_spd, aes( y = as.factor(x0), x = personality
                     , fill = k
                     , height = ..density..
                     )) +
  facet_wrap(~ini_spd$distribution, ncol = 2) +
  #ggtitle(pallete[i]) +
  geom_density_ridges(alpha = 0.5
                      , scale = 1
                     # ,stat="density"
                     #,calc_ecdf = T
                     #,jittered_points = TRUE, position = "raincloud"
                     # ,quantile_lines=T
                     # ,vline_size = 1, vline_color = "red"
                     # ,position = position_raincloud(adjust_vlines = TRUE)
                      ) +
  scale_y_discrete(name = "Refuge Size", expand = c(0.01, 0)) +
  scale_x_continuous(name = "Personality Index", breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  theme(text = element_text(size = 20)) +
  facet_wrap(~ini_spd$distribution, ncol = 2) +
  theme_ridges(center_axis_labels = T, font_size = 10) +
  scale_fill_grey(name = "Refuge Protection",start = 1, end = 0)
#scale_fill_viridis_d(name = "Refuge\nProtection", option = "inferno")

ggsave(file = "final_dists_bw.tiff", plot = p1, height = 210, width = 297, units = "mm", dpi="print", device= "tiff")

