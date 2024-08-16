#//!ELABORAZIONI DATI EMPORIO 15-8-2024
#//! Giuseppe Capella 

#//?LIBRERIE NECESSARIE
library(tidyverse)
library(factoextra)
library(readxl)
library(plotrix)
library(psych)
library(scatterplot3d)

#//?ACQUISIZIONE DATI
#//library(readxl)
DATIREMPORIO <- read_excel("DATIREMPORIO2023.xlsx", 
                              na = "na")
cartelle <- c(DATIREMPORIO$n_tessera)
DATIACP <- read_excel("DATIACP23.xlsx", na = "na")

DISTRIBUZIONI <- read_excel("DEVOLUZIONI.xlsx", #//TODO eliminare i dati percentuali
                               na = "na")

#//?FUNZIONI
gsumtable<-function (colonna, file, titolo){
a1<-c(summary(colonna))
anames<-c("Minimo","1°Q-perc","Mediana","Media","3°Q-perc","Massimo")
at<-paste(anames,a1, sep=" = ")
library(gridExtra)
jpeg(filename = file, width = 180, height = 200, units = "px", pointsize = 12,
     quality = 75,bg = "white")
grid.arrange(top=titolo,tableGrob(at))
dev.off()
}

#//!ANALISI DESCRITTIVA
#//?Analisi monovariata

#Residenza tab.2
a2 <- table(DATIREMPORIO$residenza)
write.csv(file = "Residenza.csv", a2)

barplot(table(DATIREMPORIO$residenza), col=c(1:6), cex.axis=0.8, cex.names=0.8, 
        main="EMPORIO famiglie Comune di residenza",ylab = "numero", xlab="Comuni afferenti", names.arg=c("Albareto", "Bedonia", 
        "Berceto", "BorgoTaro", "Compiano", "Solignano","Tornolo", "Valmozzola"))

#Anni primo colloquio tab.1
gsumtable( DATIREMPORIO$anni_emporio,"tab-1.jpg", "Anni dal primo colloquio")

anni<-round(table(DATIREMPORIO$anni_emporio)/length(DATIREMPORIO$anni_emporio)*100, digits = 2)
write.csv(file="anni.csv",anni )

boxplot(DATIREMPORIO$anni_emporio, main="EMPORIO Primo colloquio famiglie")

barplot(table(DATIREMPORIO$anni_emporio), col=c(1:9), cex.axis=0.8, cex.names=0.8, 
        main="EMPORIO Primo colloquio famiglie",ylab = "numero", xlab="Anni dal primo colloquio",names.arg=c("0", "1", 
                                                 "2", "3","4", "5", 
                                                 "6", "7","8","9","10"))

#Popolazione per residenza

write.csv(file="popolazione.csv", table(DATIREMPORIO$residenza, DATIREMPORIO$C_famiglia))
write.csv(file="minori.csv", table(DATIREMPORIO$residenza, DATIREMPORIO$minori))
write.csv(file="infanti.csv", table(DATIREMPORIO$residenza, DATIREMPORIO$infanti))
write.csv(file="anziani.csv", table(DATIREMPORIO$residenza, DATIREMPORIO$anziani))

#Tipologia abitativa
a3 <- table(DATIREMPORIO$abitazione)
write.csv(file = "Abitazione.csv", a3)

barplot(table(DATIREMPORIO$abitazione), col=c(0:5), cex.axis=0.8, cex.names=0.8, 
        main="EMPORIO famiglie Tipologia abitativa", names.arg=c("Acer-Agevolato", "Affitto", 
                                                                 "Precario-provvisorio", "Assente", "Proprietà"))
#ISEE Famiglie
gsumtable( DATIREMPORIO$isee,"tab-2.jpg", "Isee Famiglie")

boxplot(DATIREMPORIO$isee, main="EMPORIO Isee famiglie")

#certificazioni comunali
#//library(plotrix)
pie3D(table(DATIREMPORIO$certificazione), col=c(2:3), 
        main="EMPORIO famiglie certificate dai Servizi", labels=c("non Certificate", "Certificate"))
#migrante
table(DATIREMPORIO$migrante)
pie3D(table(DATIREMPORIO$migrante), col=c(2:3), 
    main="Famiglie con un progetto di migrazione", labels=c("Non-Migrante", "Migrante"))
#lavoro
table(DATIREMPORIO$lavoro)
pie3D(table(DATIREMPORIO$lavoro), col=c(2:3), 
    main="Famiglie con assenza di lavoro stabile o regolare", labels=c("Assenza di un lavoro", "Un lavoro"))
#nazionalita
a4 <- table(DATIREMPORIO$nazionalita)
write.csv(file = "Cittadinanza.csv", a4)
barplot(table(DATIREMPORIO$nazionalita), names.arg=c("AL","B","BL","CL","ESL","GH","I","KS","MC","MR","ML",                                         "NG","PK","RSD","RD","RM","SN","SR", "TN", "UK", "VE"), col=c(3:25), width=2, space = 1.8, cex.axis=1, cex.names=0.6,  
        main="EMPORIO famiglie nazionalita'", horiz = TRUE,
        )
legend("topright", legend = c("Venezuela","Ukraina","Tunisia","Siria","Serbia","Senegal","Romania",
"RDomingo","RSDomingo","Pakistan","Nigeria","Moldavia","Marocco",
 "Macedonia","Kosovo","Italia","Ghana","ElSalvador","Colombia","Bulgaria","Brasile","ALbania"), ncol = 2,cex = 0.5)

#Anni
plot(density(DATIREMPORIO$anni),xlab = "Anni capofamiglia", ylab = "Densità dati",main="EMPORIO Anni capofamiglia")

gsumtable( DATIREMPORIO$anni,"tab-5.jpg", "Anni capofamiglia ")

boxplot(DATIREMPORIO$anni, main="Anni capofamiglia")

#compenenti nuclEo famigliare
a5 <- table(DATIREMPORIO$C_famiglia)
write.csv(file = "componenti.csv", a5)

barplot(table(DATIREMPORIO$C_famiglia),col=1:6,xlab = "COMPONENTI NUCLEO FAMIGLIARE", ylab = "Presenze",main="EMPORIO COMPONENTI famiglia'")

gsumtable( DATIREMPORIO$C_famiglia,"tab-6.jpg", "Componenti nucleo famigliare")

boxplot(DATIREMPORIO$C_famiglia, main="Componenti nucleo")

#Servizi sanitaro sociali occupazione
par(mfrow=c(1,3))
pie(table(DATIREMPORIO$sociale), col = rainbow(2), radius = 1, 
    main="Servizio Sociale",labels=c("NO", "SI"))
pie(table(DATIREMPORIO$sanitario), col = rainbow(4), radius = 1, 
    main="Servizio Sanitario",labels=c("NO", "SI"))
pie(table(DATIREMPORIO$collocamento),col = rainbow(6), radius = 1, 
    main="Servizio Collocamento",labels=c("NO", "SI"))

#//!Analisi Bivariata
#Componenti nucleo - infanti
b1<-table(DATIREMPORIO$C_famiglia, DATIREMPORIO$infanti)
write.csv(file="componenti-infanti.csv", round(prop.table(b1)*100, digits = 2))
#Componenti nucleo - minori
b2<-table(DATIREMPORIO$C_famiglia, DATIREMPORIO$minori)
write.csv(file="componenti-minori.csv", round(prop.table(b1)*100, digits = 2))
#Componenti nucleo - anziani
b3<-table(DATIREMPORIO$C_famiglia, DATIREMPORIO$anziani)
write.csv(file="componenti-anziani.csv", round(prop.table(b1)*100, digits = 2))

#//!ACP sul set DATIACP
plot(DATIACP)
PCA <- prcomp(DATIACP, scale = TRUE)
print(PCA)
summary(PCA)
cor.PCA <- cor(DATIACP, PCA$x)
cor.PCA
screeplot(PCA,type=c("lines"))
biplot(PCA)
plot(PCA) #//TODO inserito

library(tidyverse)
library(psych)
describe(DATIACP,
         # non inserisco le misure di asimmetria
         skew = FALSE, 
         # aggiungo la differenza interquartile
         IQR = TRUE) 
cor(DATIACP) %>% 
  round(2)
# colori
col <- colorRampPalette(c("green", "white", "red"))(20)
# heatmap
heatmap(cor(DATIACP),
        col = col, 
        symm = TRUE)
PCA.rot <- principal(DATIACP, 
                     rotate="varimax", 
                     nfactors=3, 
                     scores=TRUE)
PCA.rot
print.psych(PCA.rot, sort = T, cut = 0.3)
plot(PCA.rot$values, type = "lines")
as.table(PCA.rot$loadings)^2 
  proportions(1) %>% round(2)

biplot(PCA.rot)

biplot(PCA.rot, choose = c(1,2),
       main = "Varimax")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

#sperimentazione 3d grafico sui dati ACP
library(scatterplot3d)
colors <- c( "red")
scatterplot3d(DATIACP[,1:3], pch=16, color=colors)

# Eseguire l'Analisi delle Componenti Principali (ACP)

library(plotly)
res.pca <- prcomp(DATIACP[, -5], scale = TRUE)

# Ottenere i risultati delle componenti principali
pca_df <- data.frame(res.pca$x)

# Creare un grafico 3D interattivo con plotly sulle tre componenti ACP
 plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~DATIACP$minori) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'PC1 Anziani disabili'),
                       yaxis = list(title = 'PC2 cronicità'),
                         zaxis = list(title = 'PC3 migranti famiglie')))

# matrice dei residui (uniqueness)
factor.residuals(r = cor(DATIACP), 
                 f = PCA.rot$loading)
resid(PCA.rot)
resid(PCA.rot) %>% 

#//!CLUSTER Analisys
utilities_std <- scale(DATIACP)
DATICA <- dist(utilities_std, method = "euclidean" )
CA <- hclust(DATICA, method = "complete")
plot(CA, main="Analisi dei cluster", xlab = "ALBERO DEI CLUSTER",  ylab = "Profondita'")

 
#//?analsi CA
num_clus= 5
gruppi <- cutree(CA, k=num_clus) # taglia l'albero in <num_clus> cluster
rect.hclust(CA, k=num_clus, border="red")
tabellagruppi <- data.frame(TESSSERE = cartelle, GRUPPO = gruppi)
write.csv(file="clustertab.csv",table(gruppi))
write.csv(file="clustertessere.csv",tabellagruppi, row.names = FALSE)
baricentri<-by(utilities_std,gruppi,colMeans)
#//TODO non funziona - write.table(file="baricentritab.csv", baricentri) 
M=colMeans(utilities_std[gruppi==1,])
for (i in 2:num_clus)+{ M=rbind(M,colMeans(utilities_std[gruppi==i,])) }
rownames(M)=rownames(table(gruppi))
round(M,2)
#//library(tidyverse)
#//library(factoextra)
# Rappresentazione cluster con due dimensioni
fviz_cluster(list(data = DATICA, dim=(2,3), cluster = gruppi))

#//! ANALISI CASSA
#//?Analisi descrittiva delle distribuzioni
#Media distribuzioni per tessera
#//library(readxl)
mediadist <- ceiling(mean(DISTRIBUZIONI$distribuzioni)) 
plot(density(DISTRIBUZIONI$distribuzioni), paste(main = "DISTRIBUZIONI PER TESSERA = ", mediadist), 
     ylab = "densita'", xlab="distribuzioni")
abline(v=mean(DISTRIBUZIONI$distribuzioni), col="red")
# Tessere e punti
hist(DISTRIBUZIONI$punti, col = 1:12, main = "distribuzione tessere",ylab = "numero'", xlab="PUNTI tessera ")

#Utilizzo delle tessere
mediapun <- mean(DISTRIBUZIONI$utilizzo)
plot(DISTRIBUZIONI$utilizzo, col = 1:12, main = "Utilizzo tessere media punti linea verde",
      ylab = "Uso in % 1.0=100%", xlab="Le tessere in distribuzione")
abline(h=mean(DISTRIBUZIONI$utilizzo), col="green")
abline(h=0, col="blue")
abline(h=100, col="red")




