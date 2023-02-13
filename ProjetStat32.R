rm(list=ls())
#A exécuter pour clear l'environement de travail


#Installation des packages
install.packages("dplyr")
install.packages("cartography")
install.packages("maps")
install.packages("mapdata")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("sp")
install.packages("tidyverse")
install.packages("stringr")
install.packages("stringi")
install.packages("mapproj")
library(dplyr)
library(cartography)
library(maps)
library(mapdata)
library(Hmisc)
library(ggplot2)
library(rnaturalearth)
library(sp)
library(tidyverse)
library(stringr)
library(stringi)
library(mapproj)
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
region <- list("Île-de-France","Centre-Val de Loire","Bourgogne-Franche-Comté",
               "Normandie","Hauts-de-France","Grand Est","Pays de la Loire",
               "Bretagne","Nouvelle-Aquitaine","Occitanie",
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

plot1$popregion <- as.integer(plot1$popregion)
plot1$obesite <- as.integer(plot1$obesite)

plot1$taux_mortalite = plot1$deces/plot1$popregion*100000

#Les données sont stockés et prêtes à être utilisé dans un plot

plot(plot1$obesite, plot1$taux_mortalite, col='red', xlab = 'Obésité', ylab='Mortalité')

reglin <- lm(plot1$obesite~plot1$taux_mortalite)

abline(lm(plot1$taux_mortalite ~ plot1$obesite))

cor(plot1$taux_mortalite, plot1$obesite)


#Test de cartographie sur la France

correspondance_dep_region <- read.csv2("departements-region.csv", sep=",")

map <- map_data("france")

map$departement <- map$region

###Correction de l'ortographe pour que cela colle entre les df

correspondance_dep_region[correspondance_dep_region == "Ardèche"] <- "Ardeche"
correspondance_dep_region[correspondance_dep_region == "Ariège"] <- "Ariege"
correspondance_dep_region[correspondance_dep_region == "Bouches-du-Rhône"] <- "Bouches-du-Rhone"
correspondance_dep_region[correspondance_dep_region == "Corrèze"] <- "Correze"
correspondance_dep_region[correspondance_dep_region == "Corse-du-Sud"] <- "Corse du Sud"
correspondance_dep_region[correspondance_dep_region == "Côte-d'Or"] <- "Cote-Dor"
correspondance_dep_region[correspondance_dep_region == "Côtes-d'Armor"] <- "Cotes-Darmor"
correspondance_dep_region[correspondance_dep_region == "Deux-Sèvres"] <- "Deux-Sevres"
correspondance_dep_region[correspondance_dep_region == "Drôme"] <- "Drome"
correspondance_dep_region[correspondance_dep_region == "Finistère"] <- "Finistere"
correspondance_dep_region[correspondance_dep_region == "Haute-Saône"] <- "Haute-Saone"
correspondance_dep_region[correspondance_dep_region == "Hautes-Pyrénées"] <- "Hautes-Pyrenees"
correspondance_dep_region[correspondance_dep_region == "Hérault"] <- "Herault"
correspondance_dep_region[correspondance_dep_region == "Isère"] <- "Isere"
correspondance_dep_region[correspondance_dep_region == "Lozère"] <- "Lozere"
correspondance_dep_region[correspondance_dep_region == "Nièvre"] <- "Nievre"
correspondance_dep_region[correspondance_dep_region == "Puy-de-Dôme"] <- "Puy-de-Dome"
correspondance_dep_region[correspondance_dep_region == "Pyrénées-Atlantiques"] <- "Pyrenees-Atlantiques"
correspondance_dep_region[correspondance_dep_region == "Pyrénées-Orientales"] <- "Pyrenees-Orientales"
correspondance_dep_region[correspondance_dep_region == "Rhône"] <- "Rhone"
correspondance_dep_region[correspondance_dep_region == "Saône-et-Loire"] <- "Saone-et-Loire"
correspondance_dep_region[correspondance_dep_region == "Val-d'Oise"] <- "Val-Doise"
correspondance_dep_region[correspondance_dep_region == "Vendée"] <- "Vendee"

###

correspondance_dep_region$departement <- correspondance_dep_region$dep_name

map <- merge(map, correspondance_dep_region, by="departement")

map <- subset(map, select = -c(region, num_dep))

#La table est prête pour la cartographie

plot1$region_name <- as.character(plot1$region)

cartographie <- left_join(x = map, y = plot1, by = "region_name")

map_theme <- theme(title=element_text(),
                   plot.title=element_text(margin=margin(20,20,20,20), size=18, hjust = 0.5),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major= element_blank(), 
                   panel.background= element_blank()) 

ggplot(cartographie, aes(long,lat, group = region_name, fill = taux_mortalite, )) +
  geom_polygon() +
  geom_map(data=cartographie, map=cartographie,
           aes(map_id=region_name, group=group,
               fill=taux_mortalite)) +
  geom_path(data = cartographie, aes(x = long, y = lat, group = group), 
            color = "black", size = .2) +
  coord_map() +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", name = "Taux de mortalité") +
  labs(x = "", 
       y = "", 
       title = "Lien obésité / décès par cancer du tube digestif" ) +
  map_theme

map$depfactor <- as.factor(map$departement)
summary(map$depfactor)

?coord_map



