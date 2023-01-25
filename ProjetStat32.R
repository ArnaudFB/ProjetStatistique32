rm(list=ls())
#A exécuter pour clear l'environement de travail


#Installation des packages
install.packages("dplyr")
install.packages(Panda)
library(dplyr)
###

#Mise en place de work directory
setwd("C:/Users/favre bonvin/Documents/ProjetStatistique32")

#Lecture des fichiers
cancerregion <- read.csv2("DataCancerRegion.csv")
cancerdigestifregion <- read.csv2("DataCancerDigestifRegion.csv")
###

summary(cancerregion)
summary(cancerdigestifregion)

#Retrait des classes d'âge
cancerregion <- subset(cancerregion, select = -c(classe_d_age_de_10_ans))
cancerdigestifregion <- subset(cancerdigestifregion, select = -c(classe_d_age_de_10_ans))

#Mise en place de la correspondance par région
region <- list("Île de France","Centre-Val de Loire","Bourgogne-Franche-Comté",
               "Normandie","Hauts de France","Grand Est","Pays de la Loire",
               "Bretagne","Nouvelle Aquitaine","Occitanie",
               "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur")

numregion <- list(11, 24, 27, 28, 32, 44, 52, 53, 75, 76, 84, 93)

popregion <- list(12262544, 2573269, 2806725, 3325208, 6005013, 5556219,
                  3807788, 3355019, 6011738, 5933185, 8043402, 5081101)

#Les chiffres de la population viennent de l'INSEE (2019)


obesite <- c(142, 185, 188, 198, 221, 202, 144, 169, 166, 155, 169, 159)

#Les chiffres de l'obésité viennent d'un rapport du Sénat (Obépi-Roche, 2020)

correspondance <- cbind(region, numregion, popregion, obesite)

deces_region <- aggregate(cancerdigestifregion$effectif_de_deces, by = 
                          list(Region=cancerdigestifregion$region_de_domicile),
                          FUN=sum)

deces_region <- deces_region[(deces_region$Region %in% numregion),]

deces_region$deces <- deces_region$x

deces_region <- subset(deces_region, select = -c(x, Region))

plot1 <- cbind(correspondance, deces_region)

#Les données sont stockés et prêtes à être utilisé dans un plot

