
all_bold_k_10_x0_0.5 = length(list.files(path = path, full.names = T, pattern = "all-bold__k_-10_x0_0.5"))
combis=c(
  "_normal__k_-3_x0_0.3",
  "_normal__k_-3_x0_0.5",
  "_normal__k_-3_x0_0.7",
  "_normal__k_-3_x0_0.9",
  "_normal__k_-5_x0_0.3",
  "_normal__k_-5_x0_0.5",
  "_normal__k_-5_x0_0.7",
  "_normal__k_-5_x0_0.9",
  "_normal__k_-7_x0_0.3",
  "_normal__k_-7_x0_0.5",
  "_normal__k_-7_x0_0.7",
  "_normal__k_-7_x0_0.9",
  "_normal__k_-10_x0_0.3",
  "_normal__k_-10_x0_0.5",
  "_normal__k_-10_x0_0.7",
  "_normal__k_-10_x0_0.9",
  "_all-bold__k_-3_x0_0.3",
  "_all-bold__k_-3_x0_0.5",
  "_all-bold__k_-3_x0_0.7",
  "_all-bold__k_-3_x0_0.9",
  "_all-bold__k_-5_x0_0.3",
  "_all-bold__k_-5_x0_0.5",
  "_all-bold__k_-5_x0_0.7",
  "_all-bold__k_-5_x0_0.9",
  "_all-bold__k_-7_x0_0.3",
  "_all-bold__k_-7_x0_0.5",
  "_all-bold__k_-7_x0_0.7",
  "_all-bold__k_-7_x0_0.9",
  "_all-bold__k_-10_x0_0.3",
  "_all-bold__k_-10_x0_0.5",
  "_all-bold__k_-10_x0_0.7",
  "_all-bold__k_-10_x0_0.9",
  "_uniform__k_-3_x0_0.3",
  "_uniform__k_-3_x0_0.5",
  "_uniform__k_-3_x0_0.7",
  "_uniform__k_-3_x0_0.9",
  "_uniform__k_-5_x0_0.3",
  "_uniform__k_-5_x0_0.5",
  "_uniform__k_-5_x0_0.7",
  "_uniform__k_-5_x0_0.9",
  "_uniform__k_-7_x0_0.3",
  "_uniform__k_-7_x0_0.5",
  "_uniform__k_-7_x0_0.7",
  "_uniform__k_-7_x0_0.9",
  "_uniform__k_-10_x0_0.3",
  "_uniform__k_-10_x0_0.5",
  "_uniform__k_-10_x0_0.7",
  "_uniform__k_-10_x0_0.9",
  "_all-bold-normal__k_-3_x0_0.3",
  "_all-bold-normal__k_-3_x0_0.5",
  "_all-bold-normal__k_-3_x0_0.7",
  "_all-bold-normal__k_-3_x0_0.9",
  "_all-bold-normal__k_-5_x0_0.3",
  "_all-bold-normal__k_-5_x0_0.5",
  "_all-bold-normal__k_-5_x0_0.7",
  "_all-bold-normal__k_-5_x0_0.9",
  "_all-bold-normal__k_-7_x0_0.3",
  "_all-bold-normal__k_-7_x0_0.5",
  "_all-bold-normal__k_-7_x0_0.7",
  "_all-bold-normal__k_-7_x0_0.9",
  "_all-bold-normal__k_-10_x0_0.3",
  "_all-bold-normal__k_-10_x0_0.5",
  "_all-bold-normal__k_-10_x0_0.7",
  "_all-bold-normal__k_-10_x0_0.9"
)
i=1
contagem= data.frame(combis=combis,sobreviventes=NA)
for(i in 1:length(combis)){
  contagem[i,2] = length(list.files(path = path, full.names = T, pattern = combis[i]))
                         }
write.csv2(contagem,"contagem.csv", row.names=F,dec=".")


survivors= read.csv("survivors.csv", header=T)
str(survivors)
levels(survivors$distribution)
no=filter(survivors, distribution == " Normal  ")
no = select(no, k, x0, survivors)
no=reshape(no, idvar = "k", timevar = "x0", direction = "wide")
no 

abo=filter(survivors, distribution == " All bold  ")
abo = select(abo, k, x0, survivors)
abo=reshape(abo, idvar = "k", timevar = "x0", direction = "wide")
abo


nobo=filter(survivors, distribution == " Normal-bold  ")
nobo = select(nobo, k, x0, survivors)
nobo=reshape(nobo, idvar = "k", timevar = "x0", direction = "wide")
nobo

un=filter(survivors, distribution == " Unifrom  ")
un = select(un, k, x0, survivors)
un=reshape(un, idvar = "k", timevar = "x0", direction = "wide")
un



