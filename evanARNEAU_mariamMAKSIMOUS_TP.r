library(plyr)
library(FactoMineR)
library("corrplot")
library(factoextra)

##### PARTIE 2 #####

### SECTION 2.2
# Question 2
prostate <- read.table("prostate.txt", header = TRUE)
summary(prostate["psa"])

# Question 3
## calcul des corrélations avec la variable psa
cor(prostate$vol,prostate$psa,use="complete.obs")
cor(prostate$wht,prostate$psa,use="complete.obs")
cor(prostate$age,prostate$psa,use="complete.obs")
cor(prostate$bh,prostate$psa,use="complete.obs")
cor(prostate$pc,prostate$psa,use="complete.obs")

# Question 4
## dessin du graphe
plot(prostate)

## transformation logarithmique
prostate <- transform(prostate, vol = log(vol))
prostate <- transform(prostate, wht = log(wht))
prostate <- transform(prostate, bh = log(bh))
prostate <- transform(prostate, pc = log(pc))
prostate <- transform(prostate, psa = log(psa))

## renommage des colonnes
prostate <- rename(prostate, c("vol" = "lvol", "wht" = "lwht", "bh" = "lbh", "pc" = "lpc", "psa" = "lpsa"))
prostate

## dessin du nouveau graphe
plot(prostate)

##### PARTIE 3 #####

### SECTION 3.1
# Question 2
apply(prostate,2,var)

# Question 4
## réalisation de l'ACP
res.pca = PCA(prostate, scale.unit = TRUE, ncp = 6, graph = TRUE)

# Question 6
## réalisation du PVE
PVE= fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 45))
PVE

## calcul des corrélations avec la variable lpsa
cor(prostate$lvol,prostate$lpsa,use="complete.obs")
cor(prostate$lwht,prostate$lpsa,use="complete.obs")
cor(prostate$age,prostate$lpsa,use="complete.obs")
cor(prostate$lbh,prostate$lpsa,use="complete.obs")
cor(prostate$lpc,prostate$lpsa,use="complete.obs")

##### PARTIE 4 #####

# Question 2
new_prostate <- data.frame(prostate$lpsa,prostate$lvol,prostate$lwht,prostate$lbh,prostate$lpc,prostate$age)
cor_lpsa <- cor(as.matrix(new_prostate[,1]),as.matrix(new_prostate[,-1]))
cor_lpsa
model <- lm(prostate$lpsa~prostate$lvol)

# Question 3
summary(model)

# Question 4/5
plot(prostate$lvol,prostate$lpsa, main='Formula : lpsa = b0 + b1*lvol', pch = 4)
abline(model,col=1,lwd=3)

# Question 6
coeff_determination <- summary(model)$r.squared 
coeff_determination 
