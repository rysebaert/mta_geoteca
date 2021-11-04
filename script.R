###############################################################################
#                    SCRIPT R JOUE DANS LA PRESENTATION                       #
#                    Analyse Territoriale Multiscalaire                       #                  
#    Présentation du package R MTA, son contexte de production et son usage   #
#                        R. YSEBAERT, NOVEMBRE 2021                           #
###############################################################################

# Librairies utilisées
library(MTA)
library(sf)
library(mapsf)

# Données accessibles ici : https://github.com/rysebaert/mta_geoteca
# data/data.gpkg 


##############################################################################
#  1 - Import des données, initialisation de l'analyse 
##############################################################################

com <- st_read("data/data.gpkg", layer = "com", quiet = TRUE)
epci <- st_read("data/data.gpkg", layer = "epci", quiet = TRUE)

# Filtrer sur Paris
com <- com[com$LIB_EPCI == "Métropole du Grand Paris",]
epci <- epci[epci$LIB_EPCI == "Métropole du Grand Paris",]

head(com)

# Initialisation de l'analyse
num <- which(colnames(com) == "P16_EMPLT")
denom <- which(colnames(com) == "C16_ACTOCC1564")
colnames(com)[c(num,denom)] <- c("num", "denom") 

# Retirer les valeurs égales à 0
com <- com[com$num != 0 & com$denom != 0,]

# Calculer le ratio de référence
com$ratio <- com$num / com$denom  

# Représentation cartographique
mf_map(x = com, var = "ratio", type = "choro", 
       breaks = "quantile", nbreaks = 4,
       border = "white", 
       leg_title = paste0("Emploi au lieu de travail /\n", 
                          "Actifs occupés au lieu", 
                          "de résidence, 2016"))

# EPCI
mf_map(epci, col = NA, lwd = 1, add = TRUE)

# Sources 
credits <- paste0("Sources : INSEE, IGN, 2021 / ",
                  "Réalisation : R. Ysebaert, 2021")

mf_layout(title = paste0("Ratio d'intérêt : concentration ", 
                         "d'emploi au lieu de travail",
                         "dans la MGP"),
          credits = credits)


##############################################################################
#  2 - Déviations générale, territoriale et spatiale
##############################################################################

# Déviation générale
com$gdevrel <- gdev(x = com,  var1 = "num", var2 = "denom",  
                    type = "rel")

# Palette de couleurs pour déviations(origine Color Brewer)
devpal <-  c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", 
             "#FC8D59", "#D73027")

# Cartographie
mf_map(x = com, var = "gdevrel", type = "choro", 
       breaks = c(min(com$gdevrel), 75, 90, 100, 111, 133, 
                  max(com$gdevrel)),
       border = "white", pal = devpal, 
       leg_title = paste0("Déviation au contexte général",
                          "\n(100 = moyenne de la MGP)"))
mf_map(epci, col = NA, lwd = 1, add = TRUE)
mf_layout(title = paste0("Déviation générale (moyenne = ",
                         round(sum(com$num) / sum(com$denom), 2),
                         ")"),
          credits = credits)

# Labels 
mf_label(x = com[which.min(com$gdevrel),], var = "NOM_COM",
         halo = TRUE)
mf_label(x = com[which.max(com$ratio),], var = "NOM_COM",
         halo = TRUE)


# Déviation générale / Assymétrie numérateur - dénominateur
com$gdevrel2 <- gdev(x = com,  var1 = "num", var2 = "denom", 
                     type = "rel", ref = 1)

# Cartographie
mf_map(x = com, var = "gdevrel2", type = "choro", 
       breaks = c(min(com$gdevrel2), 75, 90, 100, 111, 133, 
                  max(com$gdevrel2)),
       border = "white", pal = devpal, 
       leg_title = paste0("Déviation au contexte général",
                          "\n(100 = moyenne de la MGP)"))
mf_map(epci, col = NA, lwd = 1, add = TRUE)
mf_layout(title = paste0("Déviation générale / ", 
                         "Assymétrie numérateur dénominateur"),
          credits = credits)

# Déviation territoriale
com$tdevrel <- tdev(x = com,  var1 = "num", var2 = "denom", 
                    key = "LIB_EPCI_SUB")

# Cartographie
mf_map(x = com, var = "tdevrel", type = "choro", 
       breaks = c(min(com$tdevrel), 75, 90, 100, 111, 133, 
                  max(com$tdevrel)),
       border = "white", pal = devpal, 
       leg_title = paste0("Déviation au contexte territorial",
                          "\n(100 = moyenne de l'EPCI",
                          " d'appartenance)"))

mf_layout(title = "Déviation territoriale", credits = credits)

# Extraction du maximum et du minimum par EPCI, puis affichage
df.agg <- aggregate(tdevrel ~ LIB_EPCI_SUB, com, FUN = max)
df.max <- merge(df.agg, com)
df.max <- st_as_sf(df.max)

df.agg <- aggregate(tdevrel ~ LIB_EPCI_SUB, com, FUN = min)
df.min <- merge(df.agg, com)
df.min <- st_as_sf(df.min)

mf_label(x = df.max, var = "NOM_COM", halo = TRUE, 
         col = "#8B1713")
mf_label(x = df.min, var = "NOM_COM",  halo = TRUE,
         col = "#135D89")

# Déviation spatiale
com$sdevrel <- sdev(x = com,  var1 = "num", var2 = "denom", 
                    order = 1)

# Cartographie
mf_map(x = com, var = "sdevrel", type = "choro", 
       breaks = c(min(com$sdevrel), 75, 90, 100, 111, 133, 
                  max(com$sdevrel)),
       border = "white", pal = devpal, 
       leg_title = paste0("Déviation au contexte territorial",
                          "\n(100 = moyenne des communes",
                          " contigües"))
mf_layout(title = "Déviation spatiale", credits = credits,
          arrow = FALSE)

# Extraction des limites communales 
borders <- st_intersection(st_buffer(com, 5), st_buffer(com, 5)) 
borders <- st_cast(borders,"MULTILINESTRING")
borders <- borders[borders$INSEE_COM != borders$INSEE_COM.1, ] 

# Calcul des discontinuités
borders$disc <- pmax(borders$ratio/borders$ratio.1,
                     borders$ratio.1/borders$ratio)

# Ne conserver que les 25 % les plus importantes
val <- as.numeric(quantile(borders$disc, probs = c(1 - 0.25)))
borders <- borders[borders$disc >=  val,]

mf_map(x = borders, var = "disc", type = "prop", lwd_max = 20,
       border = "black", col = "black", leg_pos = "topleft",
       leg_title = "Discontinuités relatives\n(max/min sur le ratio de référence)",
       add = TRUE)

# Extraire top 10 max/min 
df.max <- com[order(com$sdevrel, decreasing = TRUE), ]
df.max <- df.max[1:10,]

df.min <- com[order(com$sdevrel, decreasing = FALSE), ]
df.min <- df.min[1:10,]

mf_label(x = df.max, var = "NOM_COM",  halo = TRUE, col = "#8B1713")
mf_label(x = df.min, var = "NOM_COM",  halo = TRUE, col = "#135D89")


##############################################################################
#  3 - Redistributions
##############################################################################

# Déviation générale
com$gdevabs <- gdev(x = com, var1 = "num", var2 = "denom", 
                    type = "abs")

# Sens des déviationS
com$gdevsign <- ifelse(com$gdevabs > 0, "Excédent d'emplois", 
                       "Déficit d'emplois")

# Cartographie
mf_map(epci, col = "peachpuff", border = "black", lwd = 1)
mf_map(x = com, var = c("gdevabs", "gdevsign"), type = "prop_typo",
       pal = c("#F6533A","#515FAA"), inches = 0.2,
       val_order = c("Excédent d'emplois", "Déficit d'emplois"),
       val_max = max(abs(com$gdevabs)), leg_val_rnd = -2,
       border = "white", 
       leg_title = c(paste0("Emplois à redistribuer pour atteindre",
                            " l'équilibre"),
                     "Sens de la redistribution"))

mf_layout(title = "Redistributions, déviation générale",
          credits = credits, arrow = FALSE) 

# Déviation générale - top 10 des contributeurs...
df <- st_set_geometry(com, NULL)
# ... Au regard de leur masse de numérateur
df$gdevabsPerc <- df$gdevabs / df$num * 100
df <- df[order(df$gdevabsPerc, decreasing = TRUE), ]
df[1:10, c("INSEE_COM", "NOM_COM", "LIB_EPCI_SUB", "num", "gdevabs", 
           "gdevabsPerc")]  

# Déviation générale - top 10 des receveurs...
df <- st_set_geometry(com, NULL)
# ... Au regard de leur masse de numérateur
df$gdevabsPerc <- df$gdevabs / df$num * 100
df <- df[order(df$gdevabsPerc, decreasing = FALSE), ]
df[1:10, c("INSEE_COM", "NOM_COM", "LIB_EPCI_SUB", "num", "gdevabs", 
           "gdevabsPerc")]  


##############################################################################
#  4 - Synthèse multiscalaire
##############################################################################

# Calcul typologie de synthèse
mst <- map_mst(x = com, 
               gdevrel = "gdevrel", 
               tdevrel = "tdevrel",
               sdevrel = "sdevrel",
               threshold = 150, superior = TRUE)

# Extraction de la liste 
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val

# Cartographie
mf_map(x = com, var = "mst", type = "typo", border = "white", 
       lwd = 0.2, pal = cols, val_order = unique(com$mst),
       leg_pos = "n")
mf_map(epci, col = NA, lwd = 1, add = TRUE)

mf_legend(type = "typo", pos = "topleft", 
          val = leg_val, pal = cols, 
          title = paste0("Pour le contexte général (G)\n",
                         "et-ou territorial (T)\n",
                         "et-ou spatial (S)"))
mf_layout(title = paste0("Synthèse multiscalaire" ,
                         " (déviations 1.5 fois au-dessus ", 
                         "de la moyenne)"),
          credits = credits, arrow = FALSE)

# Ajouter des labels pour mst = 7
mf_label(x = com[com$mst == 7,], var = "NOM_COM", 
         halo = TRUE, overlap = FALSE)


# Cartographie
mf_map(x = com, var = "mst", type = "typo", border = "white", 
       lwd = 0.2, pal = cols, val_order = unique(com$mst),
       leg_pos = "n")
mf_map(epci, col = NA, lwd = 1, add = TRUE)

mf_legend(type = "typo", pos = "topleft", 
          val = leg_val, pal = cols, 
          title = paste0("Pour le contexte général (G)\n",
                         "et-ou territorial (T)\n",
                         "et-ou spatial (S)"))
mf_layout(title = paste0("Synthèse multiscalaire" ,
                         " (déviations 1.5 fois au-dessus ", 
                         "de la moyenne)"),
          credits = credits, arrow = FALSE)

# Ajouter des labels pour mst = 6
mf_label(x = com[com$mst == 6,], var = "NOM_COM", 
         halo = TRUE, overlap = FALSE)

# Calcul typologie de synthèse
mst <- map_mst(x = com, 
               gdevrel = "gdevrel", 
               tdevrel = "tdevrel",
               sdevrel = "sdevrel",
               threshold = 50, superior = FALSE)

# Extraction de la liste 
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val

# Cartographie
mf_map(x = com, var = "mst", type = "typo", border = "white", 
       lwd = 0.2, pal = cols, val_order = unique(com$mst),
       leg_pos = "n")
mf_map(epci, col = NA, lwd = 1, add = TRUE)

mf_legend(type = "typo", pos = "topleft", 
          val = leg_val, pal = cols, 
          title = paste0("Pour le contexte général (G)\n",
                         "et-ou territorial (T)\n",
                         "et-ou spatial (S)"))
mf_layout(title = paste0("Synthèse multiscalaire" ,
                         " (déviations 2 fois en-dessous ", 
                         "de la moyenne)"),
          credits = credits, arrow = FALSE)

# Ajouter des labels pour mst = 7
mf_label(x = com[com$mst == 7,], var = "NOM_COM", 
         halo = TRUE, overlap = FALSE)

plot_mst(x = com, gdevrel = "gdevrel", tdevrel = "tdevrel", sdevrel = "sdevrel", 
         lib.var = "NOM_COM", cex.names = .8,
         lib.val = c("Épinay-sur-Seine", "Pierrefitte-sur-Seine", "L'Île-Saint-Denis", "Le Pré-Saint-Gervais",
                     "Ablon-sur-Seine", "Ville-d'Avray", "Périgny"),
         legend.lab = paste0("G = Métropole du Grand Paris, T = EPCI d'appartenance,",
                             "S : Communes contigües (100 = moyenne du contexte)"))