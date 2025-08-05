#Biblioteki 
install.packAGEs("corrplot")
install.packages("factoextra")
install.packages("ggfortify")
install.packages("ggplot2")
install.packages(c("cluster", "rattle","NbClust"))
install.packages("clusterSim")
install.packages("rgl")
library(readxl)
library(readr)
library(base)
library(ggplot2)
library(corrplot)
library(stats)
library(psych)
library(factoextra)
library(ggfortify)
library(cluster)
library(rattle)
library(NbClust)
library(gridExtra)
library(clusterSim)
library(MASS)
library(rgl)
library(psych)
dane <- read.csv2("C:/Users/Kuba/Desktop/projekt statystyczna analiza danych/players_stats120.csv")
dane_podstawowe<- dane
#zmienne 
#apps- wystepy 
#goals- gole strzelone 
#assist - asysty 
#yellow cards- zolte kartki 
#red cards - czerwone kartki 
#spg - strzaly na mecz 
#aerialswon - wygrane pojedynki powietrzne 
#motm - zawodnik meczu 
#rating - srednia ocena z meczow w sezonie 
#ps% - Pass success percentage
#age - wiek 
describe(dane[,-1])
summary(dane[,-1])

boxplot(dane$mins, main="Minuty spêdzone na boisku")
boxplot(dane$goals, main="Gole")
boxplot(dane$assist, main="Asysty")
boxplot(dane$spg, main="Strza³y na mecz")
boxplot(dane$aerialswon, main="Wygrane pojedynki powietrzne")
boxplot(dane$motm, main="Zawodnik meczu")
boxplot(dane$ps., main="Celnoœæ podañ")
boxplot(dane$age, main="Wiek")
boxplot(dane$yellow_card, main="¿ó³te kartki")
boxplot(dane$red_card, main="czerwone kartki")
#Zamiana zmiennych na stymulanty.
dane[,"yellow_card"]<- dane[,"yellow_card"]*(-1)
dane[,"red_card"]<- dane[,"red_card"]*(-1)
#dane[,"L"]<- dane[,"L"]*(-1)
# Age nominanta, wartosc optymalna 27 
for(i in 1:nrow(dane))
{
  if(dane[i,"age"]==27)
  {
    dane[i,"age"] <- 1
  }
  else if(dane[i,"age"] < 27)
  {
    dane[i,"age"]<- -1/(dane[i,"age"]-27-1)
  }
  else if(dane[i,"age"] > 27)
  {
    dane[i,"age"] <- 1/(dane[i,"age"]-27+1)
  }
}

#--------------------------Hellwig----------------------------------- 
#Standaryzacja danych 1 sposob 
srednia <- c()
odchylenie <- c() 
for (j in 2:ncol(dane)) {
  srednia[j-1] <- mean(dane[,j]) 
  odchylenie[j-1] <- sd(dane[,j]) 
}

for(j in 2:ncol(dane)){
  for(i in 1:nrow(dane))
    dane[i,j] <- (dane[i,j]- srednia[j-1])/odchylenie[j-1]
}
#drugi sposob
#dane <- scale(dane[,2:ncol(dane)])
#Utworzenie wzorca (“najlepszego” obiektu).
wzorzec <- c()
for(j in 2:ncol(dane))
{
  wzorzec[j-1] <- max(dane[,j])
}

#Obliczenie odleg³oœci poszczególnych obiektów od wzorca.
odleglosc <- c()
roznica <- c()
for(i in 1:nrow(dane)){
  for(j in 2:ncol(dane)){
    roznica[j-1] <- (dane[i,j]- wzorzec[j-1])^2
  }
  odleglosc[i] <-sqrt(sum(roznica))
}
odleglosc
#Stworzenie odleg³oœci “mo¿liwie dalekiej”.
antywzorzec <- c()
for(j in 2:ncol(dane))
{
  antywzorzec[j-1] <- min(dane[,j])
}
odleglosc_daleka1 <- mean(odleglosc)+2*sd(odleglosc)
odleglosc_daleka <- sqrt(sum((wzorzec-antywzorzec)^2))
#Wyznaczenie wartoœci miary dla ka¿dego obiektu.
wartosc_miary <- 1- (odleglosc/odleglosc_daleka1) 
#sortowanie 
Hellwig <- dane
Hellwig$miara <- wartosc_miary
Hellwig <- Hellwig[order(-Hellwig$miara),]


#---------------Metoda standaryzowanych sum---------------------
#zamiana zmiennych na stymulanty oraz standaryzacja tak samo jak w metodzie hellwiga 

#3Budowa syntetycznej miary, wersja bez wag 
s_rang <- c()

for(i in 1:nrow(dane)){
    s_rang[i] <- (1/(ncol(dane)-1))*sum(dane[i,-1]) 
}
#4Stadaryzacja wartoœci uzyskanych w poprzednim kroku.

st_rang <- c()
st_rang <- (s_rang - min(s_rang))/max(s_rang - min(s_rang))
st_sumy <- dane 
st_sumy$wskaznik <- st_rang
st_sumy <- st_sumy[order(-st_sumy$wskaznik),]
st_sumy1 <- st_sumy[,c(1,13)]
#--------------------------------------------------------------------------
#dane bez pierwszej kolumny 
dane_b <- dane[,-1]
#podstawowe statystyki opisowe 
summary(dane_podstawowe)
#macierz korelacji
m <- cor(dane_podstawowe[,-1])
corrplot(m,'color',type = "upper",tl.col = "black",tl.srt=45,addCoef.col = "black", insig = "blank", 
)
#usuwam zmienna apps poniewaz |r|>0.9 ze zmienna mins
dane_podstawowe <- dane_podstawowe[,c(-1,-2)]
m <- cor(dane_podstawowe)
corrplot(m,'color',type = "upper",tl.col = "black",tl.srt=45,addCoef.col = "black", insig = "blank", 
)
#wspolczynnik zmiennosci 
cv <- function(x){
  sd(x)*100/mean(x)
}
lapply(dane_podstawowe, cv)
dane_podstawowe<- scale(dane_podstawowe)
summary(dane_podstawowe)

#---------------------------Grupowanie podzia³owe----------------------------

#dzieki tej funkcji dziala wykres 
dev.off()
#optimal number of clusters elbow method 
set.seed(123)
fviz_nbclust(dane_b, kmeans, method = "wss")
#3 najlepiej bo tam jest zalamanie 
k2 <- kmeans(dane_b, centers = 2, nstart = 25)
k3 <- kmeans(dane_b, centers = 3, nstart = 25)
k4 <- kmeans(dane_b, centers = 4, nstart = 25)
k5 <- kmeans(dane_b, centers = 5, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = dane_b) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = dane_b) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = dane_b) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = dane_b) + ggtitle("k = 5")


grid.arrange(p2, p3, p4, p5, nrow = 2)
#3 clusters wygladaja najlepiej 

#---------------------------Grupowanie hierarchiczne---------------------------

odleglosc <- dist(dane_b,method = "euclidean")
a1 <- hclust(odleglosc, method ="ward.D")
a2 <- hclust(odleglosc, method ="ward.D2")
a3 <- hclust(odleglosc, method ="single")
a4 <- hclust(odleglosc, method ="average")
a5 <- hclust(odleglosc, method ="median")
a6 <- hclust(odleglosc, method ="centroid")

b1 <- plot(a1)
b2 <- plot(a2)
b3 <- plot(a3)
b4 <- plot(a4)
b5 <- plot(a5)
b6 <- plot(a6)


x<- pam(dane_b,3)
x$clustering
x$medoids
fviz_cluster(x)


indeksy <- matrix(0,10,4)
indeksy[,1]<-sapply(2:11, function(x){index.G1(dane_b,pam(dane_b,x)$clustering)})
indeksy[,2]<-sapply(2:11, function(x){index.G2(odleglosc,pam(dane_b,x)$clustering)})
indeksy[,3]<-sapply(2:11, function(x){index.G3(odleglosc,pam(dane_b,x)$clustering)})
indeksy[,4]<-sapply(2:11, function(x){index.S(odleglosc,pam(dane_b,x)$clustering)})
matplot(2:11, scale(indeksy), type="l",lwd=3)
legend("bottom",c("G1","G2","G3","S"),lwd=2,bg="white",col=1:4,cex=0.6)

#porownanie metod za pomoca indeksow 
indeksy2 = matrix(0,10,5)
indeksy2[,1] = sapply(2:11, function(x) {index.G1(dane_b,pam(dane_b, x)$clustering)})
indeksy2[,2] = sapply(2:11, function(x) {index.G1(dane_b,kmeans(dane_b, x)$cluster)})
indeksy2[,3] = sapply(2:11, function(x) {index.G1(dane_b,cutree(hclust(dist(dane_b),"average"), k = x))})
indeksy2[,4] = sapply(2:11, function(x) {index.G1(dane_b,cutree(hclust(dist(dane_b),"ward.D"), k = x))})
indeksy2[,5] = sapply(2:11, function(x) {index.G1(dane_b,cutree(hclust(dist(dane_b),"ward.D2"), k = x))})

matplot(2:11, indeksy2, type="l",lwd=3)
legend("topright",c("PAM","kmeans","hclust av","hclust D","hclust D2"),
       lwd=3,bg="white",col=1:5,cex=0.6)

#-----------------------Skalowanie wielowymiarowe--------------------------
odleglosc <- dist(dane_b,method = "euclidean")
#klasyczne skalowanie wielowymiarowe
od <-cmdscale(odleglosc,k=1)
print(od)
plot(od)

odleglosc2 <-dist(od,"euclidean")
roznica<-sum((odleglosc-odleglosc2)^2)
suma_od<-sum((odleglosc^2))
STRESS1<-sqrt(roznica/suma_od)
#duze roznice, stress = 0,611306 wiec skaluje na 2 wymiary 
od1 <- cmdscale(odleglosc,k=2)
odleglosc3 <-dist(od1,"euclidean")
plot(od1)
roznica<-sum((odleglosc-odleglosc3)^2)
suma_od<-sum((odleglosc^2))
STRESS2<-sqrt(roznica/suma_od)
#dalej spore roznice, stress=0,407
od2 <- cmdscale(odleglosc,k=3)
odleglosc4 <-dist(od2,"euclidean")
plot3d(od2)
roznica<-sum((odleglosc-odleglosc4)^2)
suma_od<-sum((odleglosc^2))
STRESS3<-sqrt(roznica/suma_od)
#dalej duza wartosc stress 
sammon(odleglosc)
sammon(odleglosc2)
sammon(odleglosc3)
sammon(odleglosc4)
