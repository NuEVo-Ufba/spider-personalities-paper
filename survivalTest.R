#retirado de 
#https://sebastiansauer.github.io/EDIT-multiple_lm_purrr_EDIT/
library(purrr)  
library(ggplot2)
library(dplyr)
library(broom)
library(knitr)  # for kable



str(survivors)
survivors.fact=survivors
survivors.fact$k=as.factor(survivors.fact$k)
survivors.fact$x0=as.factor(survivors.fact$x0)
str(survivors.fact)
colnames(survivors.fact) = c("Initial_distribution","Refuge_protection","Refuge_size","Surviving_colonies")
lm.sobrevivencia= lm(survivors.fact$Surviving_colonies ~ survivors.fact$Initial_distribution + survivors.fact$Refuge_protection + survivors.fact$Refuge_size)
kw.sobrevivencia=
plot(lm.sobrevivencia)
summary(lm.sobrevivencia)
broom::tidy(lm.sobrevivencia)
psycho::analyze(aov(lm.sobrevivencia))

survivors.fact %>% 
  dplyr::select(-Surviving_colonies)%>% 
  map(~lm(survivors.fact$Surviving_colonies ~ .x, data = survivors.fact)) %>% 
  map(summary) %>%
  #round(3)
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x) -> r2s.survivors
kable(r2s.survivors)

ggplot(r2s.survivors, aes(x = reorder(names, r.squared), y = r.squared)) + 
  geom_point(size = 5, color = "red") +
  ylab(expression(R^{2})) +
  xlab("predictors") +
  ggtitle("Explained variance per predictor from simple regressions")

glance(lm.sobrevivencia)
glance(r2s.survivors)

tidy(lm.sobrevivencia)


