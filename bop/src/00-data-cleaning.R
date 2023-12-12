## Mostreig Baròmetre d'Opinió Política

## Llibreries ---------------------------------------- 
## Paquets generals
library(readr) # importació
library(readxl) # importació excel
library(stringr) # string operations
library(tidyverse) # use the different tidyverse packages
library(yaml) # to read the yaml config

## Paquets específics
library(pps) #mostres
library(cluster)    # clustering 
library(factoextra) # clustering algorithms & visualization
library(modelsummary) # per fer les taules resum 


## Paquets geografia
library(sp) # for spatial data
library(sf) # encode spatial vector data
library(rgdal) # to import shapefiles
library(broom) # to convert shapefiles into the data frame structure we need
library(raster) # loads shapefile
library(Rcpp) # integration of R and C++
library(igraph) # build network
library(spdep) # builds network
library(spatialreg) # spatial regression analysis
library(tmap) # for interactive maps
library(tmaptools) # for interactive maps
library(spdep) # for adjacent polygons
library(rgeos) # geometries
library(splitstackshape) # for spliting
library(geosphere) # to calculate distances

library(geojsonio) # for converting data to geoJSON
library(htmlwidgets) # javascript visualizations to R
library(estimatr) # estimators for inference
library(RCT) # for designing and evaluating experiments

# Paquets tractament llistes
library(purrr) # functional programming tools

## Llegim configuracio ---------------------------------------- 
list2env(read_yaml("bop/config/config.yaml"), envir=globalenv())

## Dades sobre les seccions censals ---------------------------------------- 
df_sc <- read_excel(file.path(PROJECT, RAW_DTA_FOLDER, "dades_SC.xlsx"))
df_sc_mostra <- df_sc |>
  dplyr::select(CUSEC,
                renda_neta_persona, #Atlas de distribución de renta de los hogares - INE 2020
                renda_neta_llar, #Atlas de distribución de renta de los hogares - INE 2020
                renda_bruta_llar, #Atlas de distribución de renta de los hogares - INE 2020
                renda_bruta_persona, #Atlas de distribución de renta de los hogares - INE 2020
                #data_demografics
                edat_mitjana, perc_65_o_mes, #Censos de población i viviendas - INE 2021
                #data_padro
                poblacio, poblacio_municipi, #Censos de población i viviendas - INE 2021
                #data_cluster
                cluster21) # Cluster similitiud sociopolítica

## Dades electorals ---------------------------------------- 
# Carreguem les dades electorals per secció de les últimes Elecccions al Parlament de Catalunya
# Les últimes dades son del 2021, on es poden trobar a: https://eleccions.gencat.cat/ca/resultats-electorals/#/
A20211_Columnes_SE <- read_delim(file.path(PROJECT, RAW_DTA_FOLDER, "A20211-Columnes-SE.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Tractament de les dades per secció de les últimes Eleccions al Parlament de Catalunya
A20211_Columnes_SE <- A20211_Columnes_SE %>%
  mutate(
    # Afegim els zeros a l'esquerra dels id de districte i secció
    districte = sprintf("%02.0f", Districte),
    seccio = sprintf("%03.0f", Secció),
    cod_provincia = sprintf("%02.0f", `﻿Codi Província`),
    cod_comarca = sprintf("%02.0f", `Codi Comarca`),
    cod_municipi = sprintf("%05.0f", as.numeric(`Codi Municipi`)),
    # Canvi de nom a variables
    nom_provincia = `Nom Província`,
    nom_comarca = `Nom Comarca`,
    nom_municipi = `Nom Municipi`,
    # Calcul de percentatge de vot
    pabst21       = Abstenció/Cens*100,
    erc_vots21    = `ERC         Vots`/`Vots vàlids`*100,
    psc_vots21    = `PSC Vots`/`Vots vàlids`*100,
    cup_vots21    = `CUP-G Vots`/`Vots vàlids`*100,
    pp_vots21     = `PP          Vots` /`Vots vàlids`*100,
    comuns_vots21 = `ECP-PEC Vots`/`Vots vàlids`*100,
    cs_vots21     = `C's         Vots`/`Vots vàlids`*100,
    junts_vots21  = `JxCat Vots`/`Vots vàlids`*100,
    vox_vots21    = `VOX Vots`/`Vots vàlids`*100
  ) %>% 
  # Generació de codi CUSEC de 10 dígits. Es crea a partir de la combinació del codi de municipi, districte i secció
  mutate( CUSEC = paste(cod_municipi, districte, seccio, sep = "")  )%>% 
  dplyr::select(CUSEC, cod_provincia, nom_provincia, cod_comarca, nom_comarca, cod_municipi, nom_municipi, Cens, pabst21:vox_vots21) %>%
  # Eliminem els votants del CERA de la mostra
  filter(Cens < 10000)

dades_seccions21 <- A20211_Columnes_SE


## Dades padro ---------------------------------------- 
# Carreguem els fitxer de dades per a cada província referent a indicadors del padro continu d'habitants
# Estadística del Padrón Continuo a 1 de enero de 2022. Datos por secciones censales
# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&idp=1254734710990#!tabs-1254736195461
# Descàrrega el 27/11/2023 
X0805 <- read_delim(file.path(PROJECT, RAW_DTA_FOLDER, "0805.csv"), ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X1705 <- read_delim(file.path(PROJECT, RAW_DTA_FOLDER, "1705.csv"), ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X2505 <- read_delim(file.path(PROJECT, RAW_DTA_FOLDER, "2505.csv"), ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X4305 <- read_delim(file.path(PROJECT, RAW_DTA_FOLDER, "4305.csv"), ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

# Unim els quatre fitxer per tal d'obtenir conjunta la informació del padro
data_padro <- bind_rows(X0805, X1705, X2505, X4305)

# Tractem les dades per tal d'obtenir per a cada seccio censal el total del cens i informacio del lloc de naixement.
data_padro <- data_padro %>%
  filter(Sección != "TOTAL") %>%
  filter(Sexo == "Ambos Sexos") %>%
  dplyr::select(-Sexo)  %>%
  pivot_wider(names_from = `Relación lugar de nacimiento y residencia`, values_from = Total) %>%
  rename(CUSEC = Sección)


# Calculem el percentatge de cens que va neixer en diferent comunitat autonoma i dels que van neixer a l'estranger
# També afegim el codi de municipi
data_padro <- data_padro %>%
  mutate(pct_restaestat = `En distinta Comunidad Autónoma` / Total * 100,
         pct_extranger = `Nacidos en el Extranjero` / Total * 100)

## Dades municipi ----------------------------------------
# Creem una nova base de dades on guardarem el municipi i la seva poblacio (any 2021)
data_municipi <- data_padro %>%
  group_by(cod_municipi = str_sub(CUSEC, 1, 5) ) %>%
  summarise(pob_municipi = sum(Total))


# Anteriorment s'ha tractat les diferents dades: eleccions, renda mitjana, indicadors demogràfics i padro.
# Ara unim a les dades de les eleccions, la informacio que tenim a renda mitjana, indicadors demografics i padro.
# Les dades de referencia son els resultats de les eleccions, ja que és la variable dependent principal de la valoració política
dades_seccions21 <- dades_seccions21 %>% 
  left_join(df_sc_mostra, by = "CUSEC") %>% 
  left_join(data_padro, by = "CUSEC") %>% 
  # Afegim la poblacio que té cada municipi
  left_join(data_municipi, by = "cod_municipi")

# Calculem i guardem la proporció del cens que viu a cada estrat
dades_seccions21$propcens <- dades_seccions21$Cens / sum(dades_seccions21$Cens)


## Dades de localitzacio ---------------------------------------- 
# Carreguem les dades de localització --> es un proces lent i només carreguem dades si realitzem mapa
sf_local <- geojsonio::geojson_read(file.path(PROJECT, RAW_DTA_FOLDER, "seccionado_2021/SECC_CE_20210101.shp"), what = "sp")

# Ens quedem amb la sp de Catalunya, on tindrem en les files el nombre de seccions censals
sf_local_cat <- sf_local[sf_local$CCA == "09", ]
sf_local_cat <- spTransform(sf_local_cat, CRS("+init=epsg:4326"))
# Eliminem tota la informacio de les dades sp de tota Espanya
rm(sf_local)

# Creem una llista amb els polígons adjacents: proces lent
llista_nb <- gTouches(sf_local_cat, byid = TRUE, returnDense = FALSE)

# Afegim els codis de seccio dels adjacents a cada polígon
# Assignem com a nom dels elements de la llista el codi CUSEC del poligon mare (el qual estem)
llista_territoris <- lapply(llista_nb, function(x) sf_local_cat$CUSEC[x])
names(llista_territoris) <- sf_local_cat$CUSEC

# df_adjacent: conté les seccions adjacents de cada seccio. Això sempre serà igual si no és que es fan canvis de secció
# Ho convertim en un data.frame. 
# Convertim en variable el nom de la fila
# Creacio del codi CUSEC de la seccio mare de la qual volem les adjacents
# Creacio del codi CUSEC de les seccions adjacents a la mare. En els casos que nomes hi ha una adjacent, no afegeix un codi al final del rowname
# Afegim el numero de la adjancent (cada seccio té més o menys adjacents)
# Posem be el nom de les variables
df_adjacents <-  as.data.frame(unlist(llista_territoris),
                               ncol = max(sapply(llista_territoris, length)),
                               byrow = TRUE)

df_adjacents <- df_adjacents %>%
  rownames_to_column(var = "rowname") %>%
  mutate(
    CUSEC_mare = str_sub(rowname, 1, 10),
    adjacent = ifelse(str_sub(rowname, 11, 13) == "", 1, str_sub(rowname, 11, 13) ) 
  ) %>% 
  rename(CUSEC_adj = `unlist(llista_territoris)`)


# Obtenim les coordenades de cada secció (latitud i longitud)
df_coord_sec <- cbind(sf_local_cat@data$CUSEC, as.data.frame(coordinates(sf_local_cat)), LONGCEO, LATCEO )
colnames(df_coord_sec) <- c("CUSEC", "LONG", "LAT", "LONGCEO", "LATCEO")
# Calculem la distància de cada coordenada de secció al CEO
df_coord_sec$distCEO_km_h <- ( distHaversine(df_coord_sec[,2:3], df_coord_sec[,4:5]) ) / 1000.0
# Eliminem la longitud i latitud del CEO del df
df_coord_sec <- df_coord_sec[, -which(names(df_coord_sec) %in% c("LONGCEO", "LATCEO"))]


# Indiquem la distància a les dades de seccions
dades_seccions21 <- dades_seccions21 %>% 
  left_join(df_coord_sec[, c("CUSEC", "distCEO_km_h")], by = "CUSEC")


## Dades distància municipi - CEO ---------------------------------------- 
# Carreguem dades caps de municipis
# Aquestes dades contenen la distància en km i temps des del CEO fins al cap de municipi en cotxe
# time (in seconds) and travel distance (in meters) as metrics.
data_distancia <- read_excel(file.path(PROJECT, RAW_DTA_FOLDER, "mpiscatalunya_complet.xlsx"))


# Ajuntem la informació de distància en cotxe a les dades de les seccions censals
dades_seccions21 <- dades_seccions21 %>% 
  left_join(dplyr::select(data_distancia, cod_municipi, distancia_km_cotxe),
            by = "cod_municipi")

