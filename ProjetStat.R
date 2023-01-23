rm(list=ls()) 

install.packages("dplyr")
library(dplyr)



setwd("C:/Users/favre bonvin/Documents/ProjetStatistique32")

cancer <- read.csv("DataCancer.csv")
summary(cancer)

cancer <- subset(cancer, select = -c(classe_d_age_de_10_ans))

ARA <- list("1", "3", "7", "15", "26", "38", "42", "43", "63", "69", "73", "74")
BFC <- list("21", "25", "39", "58", "70", "71", "89", "90")
Btg <- list("22", "29", "35", "56")
CVL <- list("18", "28", "37", "41", "45")
Crs <- list("2A", "2B")
GE <- list("8", "10", "51", "52", "54", "55", "57", "67", "68", "88")
HDF <- list("2", "59", "60", "62", "80")
IDF <- list("75", "77", "78", "91", "92", "93", "94", "95")
Nmd <- list("14", "27", "50", "61", "76")
NvlA <- list("16", "17", "19", "23", "24", "33", "40", "47", "64", "79", "86", "87")
Octn <- list("9", "11", "12", "30", "31", "32", "34", "46", "48", "65", "66", "81", "82")
PDL <- list("44", "49", "53", "72", "85")
PACA <- list("4", "5", "6", "13", "83", "84")
DROM <- list("971", "972", "973", "974", "976")

cancer$dep <- cancer$departement_de_domicile
cancer <- subset(cancer, select = -c(departement_de_domicile))

cancer$region <- ifelse(cancer$dep %in% ARA, cancer$region <- "Auvergne-Rh?nes-Alpes",
ifelse(cancer$dep %in% BFC, cancer$region <- "Bourgogne-Franche-Comt?",
ifelse(cancer$dep %in% Btg, cancer$region <- "Bretagne",
ifelse(cancer$dep %in% CVL, cancer$region <- "Centre-Val de Loire",
ifelse(cancer$dep %in% Crs, cancer$region <- "Corse",
ifelse(cancer$dep %in% DROM, cancer$region <- "DROM",
ifelse(cancer$dep %in% GE, cancer$region <- "Grand Est",
ifelse(cancer$dep %in% HDF, cancer$region <- "Hauts-de-France",
ifelse(cancer$dep %in% IDF, cancer$region <- "Ile-de-France",
ifelse(cancer$dep %in% Nmd, cancer$region <- "Normandie",
ifelse(cancer$dep %in% NvlA, cancer$region <- "Nouvelle-Aquitaine",
ifelse(cancer$dep %in% Octn, cancer$region <- "Occitanie",
ifelse(cancer$dep%in% PACA, cancer$region <- "Provence-Alpes-C?tes d'Azur",
cancer$region <- "Pays de la Loire"
)))))))))))))


deces_region <- aggregate(cancer$effectif_de_deces, by = list(Region=cancer$region), FUN=sum)
population <- c(8042936, 2805580, 3371200, 2573180, 340440, 0, 5556219, 6004947, 12262544, 3499280, 6010289, 5933185, 5081101, 3806461)
deces_region$deces <- deces_region$x
deces_region <- subset(deces_region, select = -c(x))
deces_region$population <- population
deces_region <- deces_region %>%  filter(!row_number() %in% c(6))
deces_region <- deces_region %>%  filter(!row_number() %in% c(5))
deces_region$mortalite <- (deces_region$deces / deces_region$population)*1000

obesite <- c(16.9, 18.8, 16.9, 18.5, 20.2, 22.1, 14.2, 19.8, 16.6, 15.5, 15.9, 14.4)
deces_region$obesite <- obesite*10
?cor
cor(deces_region$obesite, deces_region$mortalite)
reglin <- lm(deces_region$obesite~deces_region$mortalite)
plot(deces_region$obesite, deces_region$mortalite,xlab = "Ob?sit?",ylab = "Mortalit?")
deces_region_2 <- deces_region %>%  filter(!row_number() %in% c(12))
cor(deces_region_2$obesite, deces_region_2$mortalite)
?subset
cancerH <- subset(cancer, cancer$sexe == "Hommes")
cancerF <- subset(cancer, cancer$sexe == "Femmes")
cancerH <- cancerH %>% filter(!region %in% c("Corse", "DROM"))
cancerF <- cancerF %>% filter(!region %in% c("Corse", "DROM"))

deces_regionH <- aggregate(cancerH$effectif_de_deces, by = list(Region=cancerH$region), FUN=sum)
deces_regionF <- aggregate(cancerF$effectif_de_deces, by = list(Region=cancerF$region), FUN=sum)
deces_regionH$Region <- as.factor(deces_regionH$Region)
deces_regionF$Region <- as.factor(deces_regionF$Region)

deces_regionF$Region1 <- nombre
deces_regionH$Region1 <- nombre

plot(nombre, deces_regionH$x, col = "red", xlab = "Num?ro de la r?gion", ylab = "Nombre de d?c?s")
par(new = "TRUE")
plot(nombre, deces_regionF$x, col = "blue", xlab = "", ylab = "", axes = "FALSE")
nombre = seq(12)

dece