#Selecció mostra BOP 2022
#Centre d'Estudis d'Opinió

#Preàmbul
setwd("~/OneDrive - Generalitat de Catalunya/Documents/Mostres")
setwd("~/Mostres")
#Carreguem paquets necessaris

#paquets generals
library(readr) #importació
library(stringr)
library(tidyverse) 

#paquets específics
library(pps) #mostres
library(cluster)    # clustering 
library(factoextra) # clustering algorithms & visualization
library(modelsummary) # per fer les taules resum 


#paquets geografia
library(sp)
library(sf)
library(rgdal) #to import shapefiles
library(broom) #to convert shapefiles into the data frame structure we need
library(raster) # loads shapefile
library(Rcpp)
library(igraph) # build network
library(spdep) # builds network
library(spatialreg)
library(tmap)
library(tmaptools)
library(spdep)
library(rgeos)
library(splitstackshape)

#Carreguem les dades

#Dades 2021 https://eleccions.gencat.cat/ca/resultats-electorals/#/
A20211_Columnes_SE <- read_delim("A20211-Columnes-SE.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

A20211_Columnes_SE$Districte<-sprintf("%02.0f",A20211_Columnes_SE$Districte)
A20211_Columnes_SE$Secció<-sprintf("%03.0f",A20211_Columnes_SE$Secció)

#Ara podem generar el codi de 10 dígits
A20211_Columnes_SE$CUSEC<- paste(A20211_Columnes_SE$`Codi Municipi`,A20211_Columnes_SE$Districte,A20211_Columnes_SE$Secció, sep ="")

#Calculem percentatges de vot
A20211_Columnes_SE <- A20211_Columnes_SE %>%
  mutate(
    pabst21=Abstenció/Cens*100,
    erc.vots21=`ERC         Vots`/`Vots vàlids`*100,
    psc.vots21=`PSC Vots`/`Vots vàlids`*100,
    cup.vots21=`CUP-G Vots`/`Vots vàlids`*100,
    pp.vots21=`PP          Vots` /`Vots vàlids`*100,
    comuns.vots21=`ECP-PEC Vots`/`Vots vàlids`*100,
    cs.vots21=`C's         Vots`/`Vots vàlids`*100,
    junts.vots21=`JxCat Vots`/`Vots vàlids`*100,
    vox.vots21=`VOX Vots`/`Vots vàlids`*100,
  )

#Ens quedem amb els identificadors, la mida i els percentatges
dades_seccions21<-A20211_Columnes_SE %>%
  dplyr::select(CUSEC, Cens, pabst21:vox.vots21)

#Treiem el CERA
dades_seccions21<-dades_seccions21%>%
  filter(Cens<10000)

#Clusters per vot 2021
set.seed(3456)
cluster21 <- kmeans(dades_seccions21[,3:11], centers = 6, nstart = 50, iter.max=30) 

#Visualitzem els clusters
print(cluster21)

# Afegim el cluster a les dades
dades_seccions21$cluster21 <- as.factor(cluster21$cluster)

#EXTREIEM UNA MOSTRA ESTRATIFICADA PER TIPOLOGIA DE SECCIÓ, PROPORCIONAL A LA MIDA

#Cal definir quantes seccions s'extreuen per estrat.
# Proporció del cens que viu a cada estrat
dades_seccions21$propcens<-dades_seccions21$Cens/sum(dades_seccions21$Cens)

resumestratscluster<-dades_seccions21 %>% 
  group_by(cluster21) %>% 
  summarise(Cens = sum(Cens), Propcens = sum(propcens), n = n() )
resumestratscluster$seccions<-resumestratscluster$Propcens*169
resumestratscluster
#Arrodonim i creem un vector amb la N de cada estrat
nsecestratcluster<-round(resumestratscluster$seccions, digits = 0)
nsecestratcluster


#Mostra PPS estratificada, per cluster

dadesc<-dades_seccions21[order(dades_seccions21$cluster21),] # ordenem per estrat

csample <- dadesc[ppssstrat(dadesc$Cens,dadesc$cluster21,nsecestratcluster),]
csample2 <- dadesc[ppssstrat(dadesc$Cens,dadesc$cluster21,nsecestratcluster),]
csample3 <- dadesc[ppssstrat(dadesc$Cens,dadesc$cluster21,nsecestratcluster),]
csample4 <- dadesc[ppssstrat(dadesc$Cens,dadesc$cluster21,nsecestratcluster),]

#Avaluem mostres

#Quants municipis es visiten?
csample$codmun<-str_sub(csample$CUSEC, 1, 5)
csample2$codmun<-str_sub(csample2$CUSEC, 1, 5)
csample3$codmun<-str_sub(csample3$CUSEC, 1, 5)
csample4$codmun<-str_sub(csample4$CUSEC, 1, 5)

n_distinct(csample$codmun)
n_distinct(csample2$codmun)
n_distinct(csample3$codmun)
n_distinct(csample4$codmun)


#Afegir vectors de mostra a les dades mare per avaluar mostres

csample$selected.csample<-1
csample.select<-csample %>%
  dplyr::select(CUSEC,selected.csample)

dades_seccions21<-dades_seccions21 %>%
  left_join(csample.select, by = "CUSEC")

dades_seccions21$selected.csample[is.na(dades_seccions21$selected.csample)] <- 0
dades_seccions21$selected.csample[dades_seccions21$selected.csample==0]<-"No seleccionades"
dades_seccions21$selected.csample[dades_seccions21$selected.csample==1]<-"Selecció"
table(dades_seccions21$selected.csample, useNA = "ifany")

#Mostra2
csample2$selected.csample2<-1
csample2.select<-csample2 %>%
  dplyr::select(CUSEC,selected.csample2)

dades_seccions21<-dades_seccions21 %>%
  left_join(csample2.select, by = "CUSEC")

dades_seccions21$selected.csample2[is.na(dades_seccions21$selected.csample2)] <- 0
dades_seccions21$selected.csample2[dades_seccions21$selected.csample2==0]<-"No seleccionades"
dades_seccions21$selected.csample2[dades_seccions21$selected.csample2==1]<-"Selecció"
table(dades_seccions21$selected.csample2, useNA = "ifany")

#Mostra3
csample3$selected.csample3<-1
csample3.select<-csample3 %>%
  dplyr::select(CUSEC,selected.csample3)

dades_seccions21<-dades_seccions21 %>%
  left_join(csample3.select, by = "CUSEC")

dades_seccions21$selected.csample3[is.na(dades_seccions21$selected.csample3)] <- 0
dades_seccions21$selected.csample3[dades_seccions21$selected.csample3==0]<-"No seleccionades"
dades_seccions21$selected.csample3[dades_seccions21$selected.csample3==1]<-"Selecció"
table(dades_seccions21$selected.csample3, useNA = "ifany")

#Mostra4
csample4$selected.csample4<-1
csample4.select<-csample4 %>%
  dplyr::select(CUSEC,selected.csample4)

dades_seccions21<-dades_seccions21 %>%
  left_join(csample4.select, by = "CUSEC")

dades_seccions21$selected.csample4[is.na(dades_seccions21$selected.csample4)] <- 0
dades_seccions21$selected.csample4[dades_seccions21$selected.csample4==0]<-"No seleccionades"
dades_seccions21$selected.csample4[dades_seccions21$selected.csample4==1]<-"Selecció"
table(dades_seccions21$selected.csample4, useNA = "ifany")

#balance table: comparar composició electoral de la mostra i del conjunt
datasummary_balance(~selected.csample, data = dplyr::select(dades_seccions21, selected.csample, pabst21, erc.vots21, psc.vots21, cup.vots21, pp.vots21, comuns.vots21, cs.vots21, junts.vots21, vox.vots21), output="taulacsample.html", dinm_statistic = "p.value", fmt =  '%.3f') 
datasummary_balance(~selected.csample2, data = dplyr::select(dades_seccions21, selected.csample2, pabst21, erc.vots21, psc.vots21, cup.vots21, pp.vots21, comuns.vots21, cs.vots21, junts.vots21, vox.vots21), output="taulacsample2.html", dinm_statistic = "p.value", fmt =  '%.3f') 
datasummary_balance(~selected.csample3, data = dplyr::select(dades_seccions21, selected.csample3, pabst21, erc.vots21, psc.vots21, cup.vots21, pp.vots21, comuns.vots21, cs.vots21, junts.vots21, vox.vots21), output="taulacsample3.html", dinm_statistic = "p.value", fmt =  '%.3f') 
datasummary_balance(~selected.csample4, data = dplyr::select(dades_seccions21, selected.csample4, pabst21, erc.vots21, psc.vots21, cup.vots21, pp.vots21, comuns.vots21, cs.vots21, junts.vots21, vox.vots21), output="taulacsample4.html", dinm_statistic = "p.value", fmt =  '%.3f') 

##Analitzem els mostres amb altres dades sociodemogràfiques

#Ara cal que afegim les dades de renda, provinents de l'INE
#https://www.ine.es/experimental/atlas/experimental_atlas.htm


#Baixem i importem les taules de Renda mitjana, una per província

X30896 <- read_delim("30896.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31016 <- read_delim("31016.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31079 <- read_delim("31079.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31223 <- read_delim("31223.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)

#Les ajuntem per tindre un sol df de Catalunya
rendamitjana <- bind_rows(X30896, X31016, X31079, X31223)

# Generar codi: extreiem els números de la primera cadena Unidades Terrtoriales
rendamitjana$codi<-  str_extract(rendamitjana$`Unidades territoriales`, "[[:digit:]]+")

#ens quedem només amb les seccions censals, i només amb el darrer any, les posem en columnes i deixen només el que necessitem
rendamitjana <- rendamitjana %>%
  filter(nchar(codi) == "10") %>%
  filter(Periodo == "2017") %>%
  pivot_wider(names_from = `Indicadores de renta media`, values_from = Total) %>%
  dplyr::select(-`Unidades territoriales`, -Periodo)

#Ara afegim variables de control. Comencem per les taules d'Indicadors demogràfics
#el procediment és el mateix que amb les de la renda

X30904 <- read_delim("30904.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31024 <- read_delim("31024.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31087 <- read_delim("31087.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)
X31231 <- read_delim("31231.csv", "\t", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."),  trim_ws = TRUE)

#Ajuntem les 4 províncies
demografics <- bind_rows(X30904, X31024, X31087, X31231)

# Extreure codi
demografics$codi<-  str_extract(demografics$`Unidades territoriales`, "[[:digit:]]+")

#Per si de cas, va bé tindre el codi i el nom de municipi
#Generem el codi de municipi
demografics$codmun<-str_sub(demografics$codi, 1, 5)
#Generem el nom de municipi
demografics$municipi<-str_sub(demografics$`Unidades territoriales`, 12, -14)

#Ara ens quedem només amb les seccions, i només amb el darrer any, les posem en columnes i deixen només el que necessitem
demografics <- demografics %>%
  filter(nchar(codi) == "10") %>%
  filter(Periodo == "2017") %>%
  pivot_wider(names_from = `Indicadores demográficos`, values_from = Total)

demografics <- demografics %>%
  dplyr::select(-`Unidades territoriales`, -Periodo)

#Afegim altres variables de control. Aquestes venen de les dades padró continu d'habitants
# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&idp=1254734710990#!tabs-1254736195461

X0805 <- read_delim("0805.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X1705 <- read_delim("1705.csv", ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X2505 <- read_delim("2505.csv", ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
X4305 <- read_delim("4305.csv", ";", escape_double = FALSE,  locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

#Preparem les dades
padro <- bind_rows(X0805, X1705, X2505, X4305)

#Ara cal treure les files del TOTAL, quedar-nos només amb ambos sexos i posar les variables en columnes, 
#i canviar el nom de la variable sección per poder-la enllaçar amb les altres!
padro<- padro %>%
  filter(Sección!="TOTAL") %>%
  filter(Sexo=="Ambos Sexos") %>%
  dplyr::select(-Sexo)  %>%
  pivot_wider(names_from = `Relación lugar de nacimiento y residencia`, values_from = Total) %>%
  rename(codi=Sección) 

#Afegim els percentatges que ens interessen
#de vegades no se'ns importen com a numèriques, es pot solucionar
padro$`En distinta Comunidad Autónoma`<-as.numeric(padro$`En distinta Comunidad Autónoma`)
padro$`Nacidos en el Extranjero`<-as.numeric(padro$`Nacidos en el Extranjero`)

padro<- padro%>%
  mutate(  pct.restaestat = `En distinta Comunidad Autónoma`/Total*100,
           pct.extranger = `Nacidos en el Extranjero`/Total*100  )

#Ara volem afegir la mida del municipi
#Primer, generem el codi de municipi
padro$codmun<-str_sub(padro$codi, 1, 5)
#Col·lapsem en un df de municipis amb la variable població (Total)
padro$Total<-as.numeric(padro$Total)

municipi<- padro %>%
  group_by(codmun) %>%
  summarise_at(vars(Total), sum) %>%
  rename(pobmun=Total)

#Ara ajuntem aquestes dades
padro<-padro %>%
  left_join(municipi, by = "codmun") %>%
  rename(pob2020=Total)

#Ajuntem totes les dades que hem anat important i preparant
#renda, eleccions i variables de control (padró i cens)
dades.sd<-demografics %>%
  left_join(rendamitjana, by = "codi") %>%
  left_join(padro, by="codi") 
dades.sd$CUSEC<-dades.sd$codi
dades_seccions21<-dades_seccions21 %>%
  left_join(dades.sd, by="CUSEC")


#Comparem distribucions variables sociodem

dades_seccions21$rendallar <- as.numeric(gsub(".", "", dades_seccions21$`Renta media por hogar`, fixed = TRUE))

datasummary_balance(~selected.csample, data = dplyr::select(dades_seccions21, selected.csample, `Edad media de la población`, `Porcentaje de población de 65 y más años`,
                                                            rendallar, pct.restaestat, pct.extranger), output="taula_sd_csample.html", dinm_statistic = "p.value", fmt =  '%.3f') 
datasummary_balance(~selected.csample2, data = dplyr::select(dades_seccions21, selected.csample2, `Edad media de la población`, `Porcentaje de población de 65 y más años`,
                                                            rendallar, pct.restaestat, pct.extranger), output="taula_sd_csample2.html", dinm_statistic = "p.value", fmt =  '%.3f') 
datasummary_balance(~selected.csample3, data = dplyr::select(dades_seccions21, selected.csample3, `Edad media de la población`, `Porcentaje de población de 65 y más años`,
                                                             rendallar, pct.restaestat, pct.extranger), output="taula_sd_csample3.html", dinm_statistic = "p.value", fmt =  '%.3f') 

datasummary_balance(~selected.csample4, data = dplyr::select(dades_seccions21, selected.csample4, `Edad media de la población`, `Porcentaje de población de 65 y más años`,
                                                             rendallar, pct.restaestat, pct.extranger), output="taula_sd_csample4.html", dinm_statistic = "p.value", fmt =  '%.3f') 

#Mapa mostres

#carregar coordenades (https://www.ine.es/uc/1dIJtjmE)
sf_local<- geojsonio::geojson_read(x = 'Mapa_seccions/SECC_CE_20210101.shp',what = "sp")
sf_localCAT <- sf_local[sf_local$CCA == "09",] #Ens quedem amb Catalunya
sf_localCAT<-spTransform(sf_localCAT, CRS("+init=epsg:4326"))

#Afegim dades al sf
#dades_seccions21$CUSEC<-dades_seccions21$codi
seccions_select <- dades_seccions21 %>% 
  dplyr::select(CUSEC, cluster21, selected.csample:selected.csample4) 
table(seccions_select$selected.csample, useNA = "ifany")


sf_localCAT@data<-left_join(sf_localCAT@data, seccions_select, by = "CUSEC")
table(sf_localCAT@data$selected.csample, useNA = "ifany")
##sf de la mostra per pintar vores
sf_mostra.csample1 <- sf_localCAT[sf_localCAT@data$selected.csample == "Selecció",]
sf_mostra.csample2 <- sf_localCAT[sf_localCAT@data$selected.csample2 == "Selecció",]
sf_mostra.csample3 <- sf_localCAT[sf_localCAT@data$selected.csample3 == "Selecció",]
sf_mostra.csample4 <- sf_localCAT[sf_localCAT@data$selected.csample4 == "Selecció",]


#MAPA tmap
tmap_options(check.and.fix = TRUE)
tmap_mode("view")
library(htmlwidgets)

t <- tm_shape ( sf_localCAT ) + 
  tm_fill ( col = "cluster21", style = "cat" , n = 6, alpha = .5, palette = get_brewer_pal(palette="Dark2", n=6, plot=FALSE) ,
            colorNA = " grey50 " , legend.reverse = TRUE ,
            title = " Cluster ", popup.vars=c(
              "Província" = "NPRO", 
              "Secció: " = "CUSEC",
              "Cluster" = "cluster21"),
            id = "NMUN")  + 
  tm_borders ( col = " black " , lwd = .4) +
  tm_shape(sf_mostra.csample1) +
  tm_symbols(col = "black", alpha= 0.5, scale = .01) +
  tm_borders ( col = " red " , lwd = .8)
t

tmap_save(t, "mapa_csample.html")

t2 <- tm_shape ( sf_localCAT ) + 
  tm_fill ( col = "cluster21", style = "cat" , n = 6, alpha = .5, palette = get_brewer_pal(palette="Dark2", n=6, plot=FALSE) ,
            colorNA = " grey50 " , legend.reverse = TRUE ,
            title = " Cluster ", popup.vars=c(
              "Província" = "NPRO", 
              "Secció: " = "CUSEC",
              "Cluster" = "cluster21"),
            id = "NMUN")  + 
  tm_borders ( col = " black " , lwd = .4) +
  tm_shape(sf_mostra.csample2) +
  tm_symbols(col = "black", alpha= 0.5, scale = .01) +
  tm_borders ( col = " red " , lwd = .8)
t2

tmap_save(t2, "mapa_csample2.html")

t3 <- tm_shape ( sf_localCAT ) + 
  tm_fill ( col = "cluster21", style = "cat" , n = 6, alpha = .5, palette = get_brewer_pal(palette="Dark2", n=6, plot=FALSE) ,
            colorNA = " grey50 " , legend.reverse = TRUE ,
            title = " Cluster ", popup.vars=c(
              "Província" = "NPRO", 
              "Secció: " = "CUSEC",
              "Cluster" = "cluster21"),
            id = "NMUN")  + 
  tm_borders ( col = " black " , lwd = .4) +
  tm_shape(sf_mostra.csample3) +
  tm_symbols(col = "black", alpha= 0.5, scale = .01) +
  tm_borders ( col = " red " , lwd = .8)
t3

tmap_save(t3, "mapa_csample3.html")

t4 <- tm_shape ( sf_localCAT ) + 
  tm_fill ( col = "cluster21", style = "cat" , n = 6, alpha = .5, palette = get_brewer_pal(palette="Dark2", n=6, plot=FALSE) ,
            colorNA = " grey50 " , legend.reverse = TRUE ,
            title = " Cluster ", popup.vars=c(
              "Província" = "NPRO", 
              "Secció: " = "CUSEC",
              "Cluster" = "cluster21"),
            id = "NMUN")  + 
  tm_borders ( col = " black " , lwd = .1) +
  tm_shape(sf_mostra.csample4) +
  tm_symbols(col = "black", alpha= 0.5, scale = .01) +
  tm_borders ( col = " red " , lwd = .5)
t4

tmap_save(t4, "mapa_csample4.html")

#Preparem per afegir substitutes: Identificar adjacències

#Creem una llista amb els polígons adjacents  
list.nb <- gTouches(sf_localCAT, byid = TRUE, returnDense = FALSE)
#Hi afegim els codis de secció
list.Territories <- lapply(list.nb, function(x) sf_localCAT$CUSEC[x])
names(list.Territories)<-sf_localCAT$CUSEC
#Ho convertim en un df
df_adjacency <- as.data.frame(unlist(list.Territories),ncol=20,byrow=TRUE)
#nom de fila en variable
df_adjacency <- df_adjacency %>%
  rownames_to_column(var = "rowname")
#extreiem codi secció
df_adjacency$CUSEC<-str_sub(df_adjacency$rowname, 1, 10)
#extreiem número adjacent
df_adjacency$nadjacent<-str_sub(df_adjacency$rowname, 11, 13)
#naming correcte
df_adjacency$CUSEC_m<-df_adjacency$CUSEC
df_adjacency$CUSEC<-NULL
df_adjacency$CUSEC<-df_adjacency$`unlist(list.Territories)`
#afegim el cluster de les adjacents
seccions_cluster<-dades_seccions21 %>%
  dplyr::select(CUSEC, cluster21)
df_adjacency2<-left_join(df_adjacency, seccions_cluster, by="CUSEC")
#afegim el cluster de la secció mare
seccions_cluster$CUSEC_m<-seccions_cluster$CUSEC
df_adjacency3<-left_join(df_adjacency2, seccions_cluster, by="CUSEC_m")
#preparem df
df_adjacency4 <-df_adjacency3 %>%
  dplyr::select(CUSEC_m, cluster21.y, CUSEC.x, nadjacent, cluster21.x)
df_adjacency4 <-df_adjacency4 %>%
  rename(cluster_m = cluster21.y,
         clusteradj = cluster21.x,
         CUSEC=CUSEC.x)
df_adjacency4$samecluster<-ifelse(df_adjacency4$cluster_m==df_adjacency4$clusteradj,"1","0")
#subset seccions que compleixen criteris (ara fem mateix cluster)
df_adjacencysame<-df_adjacency4 %>%
  dplyr::filter(samecluster != 0)

df_adjacencysame <- getanID(df_adjacencysame, id= "CUSEC_m")
df_adjacencysame <- dplyr::rename(df_adjacencysame, nadjacentsame = .id)

#escrivim en un csv
#write_csv(df_adjacencysame, "seccionsadjacents.csv")

#passem a wide
df_adjacencysame$nadjacent<-as.numeric(df_adjacencysame$nadjacent)

df_adjacencysame_wide <- df_adjacencysame %>% #, key = CUSEC_m, value = CUSEC)
  dplyr::select(CUSEC_m, CUSEC, nadjacentsame) %>%
  pivot_wider(names_from = nadjacentsame, values_from = CUSEC)

#afegim prefix
df_adjacencysame_wide <- df_adjacencysame_wide %>% 
  rename_with( ~ paste("adjacent", .x, sep = "_"))
#treiem el prefix del CUSEC
df_adjacencysame_wide <-df_adjacencysame_wide %>%
  rename(CUSEC = adjacent_CUSEC_m)
#write_csv(df_adjacencysame_wide, "seccionsadjacentswide.csv")


#Ara creem un df de la mostra amb adjacents
mostra1<-csample%>%
  left_join(df_adjacencysame_wide, by="CUSEC") %>%
  dplyr::select(CUSEC, Cens, codmun, cluster21, adjacent_1:adjacent_16)

#Afegim nom de municipi
mostra1 <- mostra1 %>% left_join(dades_seccions21 %>% dplyr::select(CUSEC, municipi)) %>%
  relocate(municipi, codmun) 

#Afegim id de província
mostra1$provincia <-str_sub(mostra1$CUSEC, 1, 2) 

mostra1 <- mostra1 %>%
 relocate(provincia) %>%
  mutate(provincia = fct_recode(provincia,
                            "Barcelona"    = "08",
                            "Girona" = "17",
                            "Lleida" = "25",
                            "Tarragona" = "43")) 

#Treiem columnes buides
mostra1<-mostra1[colSums(!is.na(mostra1)) > 0]

#Guardem
write_csv(mostra1, "mostra1.csv")

#Mostra 2
#Ara creem un df de la mostra amb adjacents
mostra2<-csample2%>%
  left_join(df_adjacencysame_wide, by="CUSEC") %>%
  dplyr::select(CUSEC, Cens, codmun, cluster21, adjacent_1:adjacent_16)

#Afegim nom de municipi
mostra2 <- mostra2 %>% left_join(dades_seccions21 %>% dplyr::select(CUSEC, municipi)) %>%
  relocate(municipi, codmun) 

#Afegim id de província
mostra2$provincia <-str_sub(mostra2$CUSEC, 1, 2) 

mostra2 <- mostra2 %>%
  relocate(provincia) %>%
  mutate(provincia = fct_recode(provincia,
                                "Barcelona"    = "08",
                                "Girona" = "17",
                                "Lleida" = "25",
                                "Tarragona" = "43")) 

#Treiem columnes buides
mostra2<-mostra2[colSums(!is.na(mostra2)) > 0]

#Guardem
write_csv(mostra2, "mostra2.csv")

#Mostra 3
#Ara creem un df de la mostra amb adjacents
mostra3<-csample3%>%
  left_join(df_adjacencysame_wide, by="CUSEC") %>%
  dplyr::select(CUSEC, Cens, codmun, cluster21, adjacent_1:adjacent_16)

#Afegim nom de municipi
mostra3 <- mostra3 %>% left_join(dades_seccions21 %>% dplyr::select(CUSEC, municipi)) %>%
  relocate(municipi, codmun) 

#Afegim id de província
mostra3$provincia <-str_sub(mostra3$CUSEC, 1, 2) 

mostra3 <- mostra3 %>%
  relocate(provincia) %>%
  mutate(provincia = fct_recode(provincia,
                                "Barcelona"    = "08",
                                "Girona" = "17",
                                "Lleida" = "25",
                                "Tarragona" = "43")) 

#Treiem columnes buides
mostra3<-mostra3[colSums(!is.na(mostra3)) > 0]

#Guardem
write_csv(mostra3, "mostra3.csv")

#Mostra 4
#Ara creem un df de la mostra amb adjacents
mostra4<-csample4%>%
  left_join(df_adjacencysame_wide, by="CUSEC") %>%
  dplyr::select(CUSEC, Cens, codmun, cluster21, adjacent_1:adjacent_16)

#Afegim nom de municipi
mostra4 <- mostra4 %>% left_join(dades_seccions21 %>% dplyr::select(CUSEC, municipi)) %>%
  relocate(municipi, codmun) 

#Afegim id de província
mostra4$provincia <-str_sub(mostra4$CUSEC, 1, 2) 

mostra4 <- mostra4 %>%
  relocate(provincia) %>%
  mutate(provincia = fct_recode(provincia,
                                "Barcelona"    = "08",
                                "Girona" = "17",
                                "Lleida" = "25",
                                "Tarragona" = "43")) 

#Treiem columnes buides
mostra4<-mostra4[colSums(!is.na(mostra4)) > 0]

#Guardem
write_csv(mostra4, "mostra4.csv")

#The end
         
         

  

