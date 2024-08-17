#//!ELABORAZIONI DATI EMPORIO 15-8-2024
#//!  Giuseppe Capella 

#//?LIBRERIE NECESSARIE
library(tidyverse)
library(factoextra)
library(readxl)
library(plotrix)
library(psych)
library(scatterplot3d)
library(gridExtra)
library(plotly)

#//?ACQUISIZIONE DATI
#//library(readxl)
DATIREMPORIO <- read_excel("DATIEMPORIO23.xlsx", #//TODO controllare cartella doppia 262
                           na = "na")
cartelle <- c(DATIREMPORIO$n_tessera)
DATIACP <- read_excel("DATIACP23.xlsx", na = "na")

DISTRIBUZIONI <- read_excel("DEVOLUZIONI23.xlsx", #//TODO eliminare i dati percentuali
                            na = "na")

#//?FUNZIONI
gsumtable<-function (colonna, file, titolo){
  a1<-c(summary(colonna))
  anames<-c("Minimo","1°Q-perc","Mediana","Media","3°Q-perc","Massimo")
  at<-paste(anames,a1, sep=" = ")
  library(gridExtra)
  jpeg(filename = file, width = 180, height = 200, units = "px", pointsize = 12,
       quality = 100,bg = "white")
  grid.arrange(top=titolo,tableGrob(at))
  dev.off()
}

#//!ANALISI DESCRITTIVA
#//?Analisi monovariata

#Residenza tab.2
tabresidenza <- table(DATIREMPORIO$residenza)
write.csv(file = "Residenza.csv", tabresidenza)

jpeg("residenza.jpg", width = 800, height = 600, quality = 100)
barplot(tabresidenza, col=rainbow(length(tabresidenza)), cex.axis=1, cex.names=1, 
        main="FAMIGLIE IN ACCESSO PER COMUNE DI RESIDENZA", ylab = "Numero famiglie in accesso", xlab = "Comuni afferenti", legend.text = rownames(tabresidenza))
dev.off()
#Anni primo colloquio tab.1
gsumtable( DATIREMPORIO$anni_emporio,"tab-1.jpg", "Anni dal primo colloquio")

anni<-round(table(DATIREMPORIO$anni_emporio)/length(DATIREMPORIO$anni_emporio)*100, digits = 2)
write.csv(file="anni.csv",anni )

boxplot(DATIREMPORIO$anni_emporio, main="EMPORIO Primo colloquio famiglie")

jpeg("primo_colloquio.jpg", width = 800, height = 600, quality = 100)
barplot(anni, col=rainbow(length(tabresidenza)), cex.axis=1, cex.names=1, 
        main="PRIMO ACCESSO DELLA FAMIGLIA", ylab = "Numero famiglie in accesso", xlab = "Anni dal primo acceso", legend.text = rownames(anni))
dev.off()

#Popolazione per residenza
library(knitr)
library(kableExtra)

popol <- table(DATIREMPORIO$residenza, DATIREMPORIO$C_famiglia)
write.csv(file="popolazione.csv", popol )
totpopol <- sum(rowSums(popol[, 1:7]))
tab1 <- kable(popol, caption = paste("FAMIGLIE PER COMPONENTI TOT=", totpopol), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightgray")
save_kable(tab1, file="tabellapopolazione.html")

minori <- table(DATIREMPORIO$residenza, DATIREMPORIO$minori)
write.csv(file="minori.csv", minori)
totminori <- sum(rowSums(minori[, 2:5]))
tab2 <- kable(minori, caption = paste("FAMIGLIE CON MINORI >15aa TOT=", totminori), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightgray")
save_kable(tab2, file="tabellaminori.html")


infanti <- table(DATIREMPORIO$residenza, DATIREMPORIO$infanti)
totinfanti <- sum(rowSums(infanti[, 2:4]))
write.csv(file="infanti.csv", infanti)
tab3 <- kable(infanti, caption = paste("FAMIGLIE CON INFANTI <3aa TOT=", totinfanti), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightgray")
save_kable(tab3, file="tabellainfanti.html")


anziani <- table(DATIREMPORIO$residenza, DATIREMPORIO$anziani)
totanziani <- sum(rowSums(anziani[, 2:3]))
write.csv(file="anziani.csv", anziani)
tab4 <- kable(anziani, caption = paste("FAMIGLIE CON ANZIANI >65aa TOT=", totanziani), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightgray")
save_kable(tab4, file="tabellaanziani.html")

#Tipologia abitativa
abitazione <- table(DATIREMPORIO$abitazione)
write.csv(file = "Abitazione.csv", abitazione)

jpeg("abitazione.jpg", width = 800, height = 600, quality = 100)
barplot(abitazione, col=rainbow(length(tabresidenza)), cex.axis=1, cex.names=1, 
        main="FAMIGLIA TIPOLOGIA DI ABITAZIONE", ylab = "Numero famiglie in accesso", xlab = "Tipologia di abitazione", legend.text = rownames(abitazione))
dev.off()

barplot(table(DATIREMPORIO$abitazione), col=c(0:5), cex.axis=0.8, cex.names=0.8, 
        main="EMPORIO famiglie Tipologia abitativa", names.arg=c("Acer-Agevolato", "Affitto", 
                                                                 "Precario-provvisorio", "Assente", "Proprietà"))
#ISEE Famiglie
gsumtable( DATIREMPORIO$isee,"tab-2.jpg", "Isee Famiglie")
jpeg("iseeplot.jpg", width = 800, height = 600, quality = 100)
boxplot(DATIREMPORIO$isee, main="ISEE FAMIGLIE")
dev.off()
jpeg("isee_densita.jpg", width = 800, height = 600, quality = 100)
plot(density(DATIREMPORIO$isee),xlab = "DENSITA' ISEE FAMIGLIA", ylab = "Densità dati",main="ISEE")
dev.off()
#certificazioni comunali
library(plotrix)
jpeg("certificazioni.jpg", width = 800, height = 600, quality = 100)
pie3D(table(DATIREMPORIO$certificazione), col=c(2:3), explode = 0.1, 
      main="FAMIGLIE CON ACCESSO CERTIFICATO", labels=c("non Certificato", "Certificato"))
dev.off()
#migrante
jpeg("migranti.jpg", width = 800, height = 600, quality = 100)
pie3D(table(DATIREMPORIO$migrante), col=c(2:3), explode = 0.1, 
      main="FAMIGLIE CON PROGETTO DI MIGRAZIONE", labels=c("Non migrante", "Migrante"))
dev.off()

#lavoro
jpeg("lavoro.jpg", width = 800, height = 600, quality = 100)
pie3D(table(DATIREMPORIO$lavoro), col=c(2:3), explode = 0.1, 
      main="FAMIGLIE CON UN OCCUPATO", labels=c("Non occupati", "Occupato"))
dev.off()
#nazionalita
nazionalita <- table(DATIREMPORIO$nazionalita)
write.csv(file = "nazionalita.csv", nazionalita)

jpeg("nazionalita.jpg", width = 800, height = 600, quality = 100)
barplot(nazionalita, col=rainbow(length(nazionalita)), cex.axis=1, cex.names=1, 
        main="FAMIGLIA PER NAZIONALITA'", ylab = "Numero di famiglie", xlab = "Nazionalità", legend.text = rownames(nazionalita))
dev.off()

tab4 <- kable(nazionalita, caption = paste("FAMIGLIE per NAZIONALITA'"), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightgray")
save_kable(tab4, file="nazionalita.html")

#Anni
jpeg("anni.jpg", width = 800, height = 600, quality = 100)
plot(density(DATIREMPORIO$anni),xlab = "anni", ylab = "Densità dati",main="ANNI DI ANZIANITA' CAPOFAMIGLIA")
dev.off()
gsumtable( DATIREMPORIO$anni,"tab-5.jpg", "Anzianità capofamiglia ")
jpeg("anniplot.jpg", width = 800, height = 600, quality = 100)
boxplot(DATIREMPORIO$anni, main="Anni capofamiglia")
dev.off()
#compenenti nucleo famigliare
componenti <- table(DATIREMPORIO$C_famiglia)
write.csv(file = "componenti.csv", componenti)
jpeg("componenti.jpg", width = 800, height = 600, quality = 100)
barplot(componenti, col=rainbow(length(nazionalita)), cex.axis=1, cex.names=1, 
        main="FAMIGLIA PER COMPONENTI", ylab = "Numero di famiglie", xlab = "Numero di componenti", legend.text = rownames(componenti))
dev.off()

gsumtable( DATIREMPORIO$C_famiglia,"tab-6.jpg", "Componenti nucleo famigliare")
jpeg("componentiplot.jpg", width = 800, height = 600, quality = 100)
boxplot(DATIREMPORIO$C_famiglia, main="Componenti nucleo")
dev.off()

#Servizi sanitaro sociali occupazione
jpeg("servizi.jpg", width = 1000, height = 600, quality = 100)
par(mfrow=c(1,3))
pie3D(table(DATIREMPORIO$sociale), col = rainbow(2), radius = 1, explode = 0.1,
      main="SERVIZIO SOCIALE COMUNALE",labels=c("NO", "SI"))
pie3D(table(DATIREMPORIO$sanitario), col = rainbow(4), radius = 1, explode = 0.1,
      main="SERVIZIO SANITARIO",labels=c("NO", "SI"))
pie3D(table(DATIREMPORIO$collocamento),col = rainbow(6), radius = 1, explode = 0.1,
      main="SERVIZIO DI COLLOCAMNETO",labels=c("NO", "SI"))
dev.off()

#//!Analisi Bivariata
bv1 <- aggregate(DATIREMPORIO$infanti ~ DATIREMPORIO$residenza, data = DATIREMPORIO, sum)
bv2 <- aggregate(DATIREMPORIO$minori ~ DATIREMPORIO$residenza, data = DATIREMPORIO, sum)
bv3 <- aggregate(DATIREMPORIO$anziani ~ DATIREMPORIO$residenza, data = DATIREMPORIO, sum)

BV1 <- merge(bv1, bv2, by="DATIREMPORIO$residenza" )
BV <- merge(BV1, bv3, by="DATIREMPORIO$residenza" )
colnames(BV) <- c("Comuni","INFANTI", "MINORI", "ANZIANI")
tab9 <- kable(BV, caption = "NUMERO DI INFANTI, MINORI e ANZIANI PER COMUNE", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                full_width = F, 
                position = "center") %>%
  column_spec(1, bold = TRUE, background = "yellow") %>%
  row_spec(0, bold = TRUE, background = "lightblue")
save_kable(tab9, file="BVinfanti_minori_anziani.html")

#sperimentazione 3d sulle correlazioni tra variabili ACP
library(scatterplot3d)
jpeg("VARIABILIgrafico3d.jpg", width = 800, height = 800)
scatterplot3d(DATIACP[,3:5], 
              pch = 10,        
              color = "blue", # Colore dei punti
              xlab = "MIGRANTI",  # Etichetta dell'asse x
              ylab = "ANNI C.F.",  # Etichetta dell'asse y
              zlab = "MINORI",  # Etichetta dell'asse z
              main = "Grafico a Dispersione 3D") # Titolo del grafico
dev.off()


#//!ACP sul set DATIACP
plot(DATIACP)
PCA <- prcomp(DATIACP, scale = TRUE)
print(PCA)
summary(PCA)
cor.PCA <- cor(DATIACP, PCA$x)
print(cor.PCA)
jpeg("pca_autovalori.jpg", width = 800, height = 600)
screeplot(PCA,type=c("lines"), main = "Autovalori sulle componenti principali")
dev.off()
# Visualizzare il biplot della PCA
library(factoextra)
jpeg("pca_biplot.jpg", width = 800, height = 800)
fviz_pca_biplot(PCA, repel = TRUE, title = "RAPPRESENTAZIONE GRAFICA DEL PCA IN 2D",col.var = "red", col.ind = "blue")
dev.off()

library(tidyverse)
library(psych)
describe(DATIACP,
         # non inserisco le misure di asimmetria
         skew = FALSE, 
         # aggiungo la differenza interquartile
         IQR = TRUE) 
correlazioneACP <- cor(DATIACP) %>% 
  round(2)
print(correlazioneACP)
# heatmap
jpeg("pca_MATRICE-COR.jpg", width = 700, height = 700)
heatmap(correlazioneACP,
        main = "GRAFICO DELLA MATRICE DI CORRELAZIONE VAR",
        col = colorRampPalette(c("blue", "white", "red"))(100),
        margins = c(20,20), 
        symm = TRUE)
dev.off()

PCA.rot <- principal(DATIACP, 
                     rotate="varimax", 
                     nfactors=3, 
                     scores=TRUE)
print(PCA.rot)

# matrice dei residui (uniqueness)
tab11 <- factor.residuals(r = cor(DATIACP), 
                          f = PCA.rot$loading)
tab12 <- resid(PCA.rot)
print(tab11)
print(tab12)

#//! SIMULAZIONI PCA 3D
# Ottenere i risultati delle componenti principali
pca_df <- data.frame(PCA$x[, 1:3])

# Creare un grafico 3D interattivo con plotly sulle tre componenti ACP da esplorare
library(plotly)
plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~DATIACP$minori) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1 Anziani disabili'),
                      yaxis = list(title = 'PC2 cronicità'),
                      zaxis = list(title = 'PC3 migranti famiglie')))


#//!CLUSTER Analisys

utilities_std <- scale(DATIACP)
DATICA <- dist(utilities_std, method = "euclidean" )
CA <- hclust(DATICA, method = "complete")
num_cluster= 6
gruppi <- cutree(CA, k=num_cluster)
jpeg("cluster.jpg", width = 800, height = 600)
fviz_dend(CA, 
          k = num_cluster, # Numero di cluster
          rect = TRUE, # Aggiungere rettangoli attorno ai cluster
          rect_fill = TRUE, # Riempire i rettangoli con colore
          rect_border = "jco", # Colore dei bordi dei rettangoli
          cex = 0.6, # Dimensione del testo
          lwd = 0.4, # Spessore delle linee del dendrogramma
          main = "Dendrogramma del Clustering Gerarchico")
dev.off()


#//?analsi CA

tabellagruppi <- data.frame(TESSSERE = cartelle, GRUPPO = gruppi)
write.csv(file="clustertab.csv",table(gruppi))
write.csv(file="clustertessere.csv",tabellagruppi, row.names = FALSE)
#baricentri<-by(utilities_std,gruppi,colMeans)
#baricentri_df <- do.call(rbind, baricentri)
#write.csv(file="baricentritab.csv", baricentri_df, row.names = TRUE)

# Rappresentazione cluster con due dimensioni
library(tidyverse)
library(factoextra)

jpeg("clusterGRAFICO.jpg", width = 800, height = 800)
fviz_cluster(list(data = pca_df, cluster = gruppi),
             geom = "text",
             palette = "jco", 
             ggtheme = theme_classic(),
             main = "Visualizzazione dei Cluster con PCA")
dev.off()

#//! ANALISI CASSA
#//?Analisi descrittiva delle distribuzioni
#Media distribuzioni per tessera
library(readxl)
mediadist <- ceiling(mean(DISTRIBUZIONI$distribuzioni)) 
jpeg("distribuzioni.jpg", width = 800, height = 600)
plot(density(DISTRIBUZIONI$distribuzioni), paste(main = "DISTRIBUZIONI PER TESSERA = ", mediadist), 
     ylab = "densita'", xlab="distribuzioni")
abline(v=mean(DISTRIBUZIONI$distribuzioni), col="red")
dev.off()
# Tessere e punti
jpeg("tessere_punti.jpg", width = 800, height = 600)
hist(DISTRIBUZIONI$punti, col = rainbow(length(DISTRIBUZIONI$punti)), main = "TESSERE E PUNTI",ylab = "Numero di tessere", xlab="PUNTI TESSERE ")
dev.off()
#Utilizzo delle tessere
mediapun <- ceiling(mean(DISTRIBUZIONI$utilizzo))
jpeg("distribuzioni_utilizzo.jpg", width = 800, height = 600)
plot(DISTRIBUZIONI$utilizzo, col = rainbow(length(DISTRIBUZIONI$utilizzo)), pch = 10, main = paste("Utilizzo tessere media punti = " ,mediapun),
     ylab = "Uso in % 1.0=100%", xlab="Le tessere in distribuzione")
abline(h=mean(DISTRIBUZIONI$utilizzo), col="green")
abline(h=0, col="blue")
abline(h=100, col="red")
dev.off()