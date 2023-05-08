## Configuracio abans loop ---------------------------------------- 

# Variables que guardem de cada extracció de mostra: 
# nombre diferents de municipis que toca visitar
distinct_municipis <- numeric(NMOSTRES)
# distància des del CEO a la seccio seleccionada en km en cotxe
sum_distCEO_km_cotxe <- numeric(NMOSTRES)
# temps des del CEO a la seccio seleccionada en hores en cotxe (NO UTILITZAT)
# sum_tempsCEO_hores_cotxe <- numeric(NMOSTRES) (NO UTILITZAT)
# Creem una llista per guardar les diferencies electorals per partit per a cada mostra
ls_p_csample <- vector(mode = "list", length = NMOSTRES)
names(ls_p_csample) <- paste("mostra", 1:NMOSTRES, sep = "_")
# estadistics de la diferencia electoral
diff_electoral <- numeric(NMOSTRES)
# Creem una llista per guardar les diferencies sociodemografiques per partit per a cada mostra
ls_sd_csample <- vector(mode = "list", length = NMOSTRES)
names(ls_sd_csample) <- paste("mostra", 1:NMOSTRES, sep = "_")
# estadisitcs de la diferencia amb les variables sociodemogràfiques
diff_sociodemografic <- numeric(NMOSTRES)
# nombre de seccions no adjacents
n_no_adjacents <- numeric(NMOSTRES)
# Creem data.frame sobre la distribució de províncies
df_dist_provincies <- data.frame(matrix(nrow = 4, ncol = 0))
rownames(df_dist_provincies) <- c("Barcelona", "Girona", "Lleida", "Tarragona")
# Creem una llista per les seccions censals que tenen adjacencies del mateix cluster
ls_mostra_amb_adjacents <- vector(mode = "list", length = NMOSTRES)
names(ls_mostra_amb_adjacents) <- paste("mostra", 1:NMOSTRES, sep = "_")
# Creem una llista per les seccions censals que no tenen adjacencies del mateix cluster
ls_mostra_sense_adjacents <- vector(mode = "list", length = NMOSTRES)
names(ls_mostra_sense_adjacents) <- paste("mostra", 1:NMOSTRES, sep = "_")
# Creem una llista per guardar les seccions seleccionades i no seleccionades
ls_dades_seccions <- vector(mode = "list", length = NMOSTRES)
names(ls_dades_seccions) <- paste("mostra", 1:NMOSTRES, sep = "_")
# Creem una llista per guardar els resultats de la mostra seleccionada amb adjacents i sense adjacents
N_random_mostres <- vector(mode = "list", length = NMOSTRES)
names(N_random_mostres) <- paste("mostra", 1:NMOSTRES, sep = "_")
# Creem una llista per guardar tota la informació de totes les seccions censals quines
# son escollides, quines no i a quin cluster pertanyen
info_sc_general <- vector(mode = "list", length = NMOSTRES)
names(info_sc_general) <- paste("mostra", 1:NMOSTRES, sep = "_")



start_loop <- Sys.time()
## Inici loop ---------------------------------------- 
for(i in 1:NMOSTRES){
  
  dades_seccions <- dades_seccions21
  
  # Elaboració dels clusters a partir de les columnes del data.frame finalitzades amb 21
  # Per tal de tenir sempre la mateixa classificació de clústers, s'ha fixat una llavor, 
  # s'han guardat els resultats i es carreguen a partir del fitxer anomenat CUSEC_clusters.
  # Per obtenir els mateixos resultats, s'hauria d'aplicar les dues línies de codi següents:
  #set.seed(3456)
  #cluster21 <- kmeans(dplyr::select(dades_seccions21, ends_with("21")), centers = 6, nstart = 50, iter.max = 30)
  
  # Guardem els resultats del cluster
  #dades_seccions$cluster21 <- as.factor(cluster21$cluster)
  
  # Guardem en una llista les seccions amb el mateix cluster
  ls_dades_seccions_by_cluster <- lapply(split(dades_seccions["CUSEC"], dades_seccions$cluster21), transform, CUSEC = CUSEC)
  # Afegim la latitud i longitud de cada secció censal a la llista acabada de crear
  ls_dades_seccions_by_cluster <- lapply(ls_dades_seccions_by_cluster, function(x) left_join(x, df_coord_sec, by = "CUSEC"))
  
  # Calcul del nombre de seccions que es volen extreure 
  # Resum dels estadístics 
  resum_estrats_cluster <- dades_seccions %>% 
    group_by(cluster21) %>% 
    summarise(pabs = mean(pabst21), Cens = sum(Cens), Propcens = sum(propcens), nseccions = n() )
  
  
  # Calculem el nombre de seccions a agafar de cada cluster segons la seva proporció censal
  resum_estrats_cluster$seccions <- resum_estrats_cluster$Propcens * NSECCIONS
  
  
  # Arrodonim i creem un vector amb la N de cada estrat
  resum_estrats_cluster$n_sec_estrat_cluster <- round(resum_estrats_cluster$seccions, digits = 0)
  
  
  # Controlem que el nombre d'estrats seleccionats per a cada seccio censal sigui el que s'ha escollit inicialment (NSECCIONS)
  if( sum(resum_estrats_cluster$n_sec_estrat_cluster) > NSECCIONS) {
    while( sum(resum_estrats_cluster$n_sec_estrat_cluster) > NSECCIONS) {
      # Busquem arrodoniment que té major diferència
      pos <- which.max( abs(resum_estrats_cluster$seccions - resum_estrats_cluster$n_sec_estrat_cluster) )
      valor <- resum_estrats_cluster$n_sec_estrat_cluster[pos]
      resum_estrats_cluster$n_sec_estrat_cluster[pos] <- valor - 1
    }
  } else {
    if(sum(resum_estrats_cluster$n_sec_estrat_cluster) < NSECCIONS) {
      while(sum(resum_estrats_cluster$n_sec_estrat_cluster) < NSECCIONS){
        # Busquem arrodoniment que té menor diferència
        pos <- which.min(abs(resum_estrats_cluster$seccions - resum_estrats_cluster$n_sec_estrat_cluster))
        valor <- resum_estrats_cluster$n_sec_estrat_cluster[pos]
        resum_estrats_cluster$n_sec_estrat_cluster[pos] <- valor + 1
      }
    }
  }
  
  # Guardem el cluster amb mes abtencio:
  clust_max_pabs <- resum_estrats_cluster$cluster21[which.max(resum_estrats_cluster$pabs)]
  
  # Un cop fet el repartiment de les seccions, afegim seccions al clúster amb més percentatge d'abstenció en les últimes eleccions
  # Es tracta d'anar a més seccions i reduir el nombre d'entrevistes en aquelles seccions
  resum_estrats_cluster$n_sec_estrat_cluster_abs <- resum_estrats_cluster$n_sec_estrat_cluster
  resum_estrats_cluster$n_sec_estrat_cluster_abs[which.max(resum_estrats_cluster$pabs)] <- resum_estrats_cluster$n_sec_estrat_cluster_abs[which.max(resum_estrats_cluster$pabs)] + ABSCORRECCIO
  
  resum_estrats_cluster <- resum_estrats_cluster %>% 
    mutate(
      mean_enquestes = ifelse(cluster21 == clust_max_pabs, MEAN_ENQUESTES_SEC_COR,  MEAN_ENQUESTES_SEC),
      min_enquestes = ifelse(cluster21 == clust_max_pabs, MIN_ENQUESTES_SEC_COR,  MIN_ENQUESTES_SEC),
      max_enquestes = ifelse(cluster21 == clust_max_pabs, MAX_ENQUESTES_SEC_COR,  MAX_ENQUESTES_SEC), 
    ) %>% 
    mutate(
      sum_enquestes = n_sec_estrat_cluster_abs * mean_enquestes,
      sum_min_enquestes = n_sec_estrat_cluster_abs * min_enquestes,
      sum_max_enquestes = n_sec_estrat_cluster_abs * max_enquestes
    )
  
  
  # Guardem la informació una única vegada
  if(i == 1){
    write_csv2(resum_estrats_cluster, paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/resum_cluster.csv"))
  }
  
  # Afegim la mitjana d'enquestes que hi ha de tenir cada cluster
  # Donat que s'ha augmentat el nombre de seccions en el cluster amb més abtenció, reduirem la mitjana del nombre d'enquestes
  # a fer en aquesta secció
  dades_seccions <- dades_seccions %>% 
    mutate(
      mean_enquestes = ifelse(cluster21 == clust_max_pabs, MEAN_ENQUESTES_SEC_COR,  MEAN_ENQUESTES_SEC),
      min_enquestes = ifelse(cluster21 == clust_max_pabs, MIN_ENQUESTES_SEC_COR,  MIN_ENQUESTES_SEC),
      max_enquestes = ifelse(cluster21 == clust_max_pabs, MAX_ENQUESTES_SEC_COR,  MAX_ENQUESTES_SEC), 
    )
  
  # Ordenem la mostra pels clusters abans d'aplicar la funció ppssstrat
  dades_c <- dades_seccions[order(dades_seccions$cluster21), ]
  
  
  # Realitzem el mostreig per estrat, mitjançant l'ús de ppssstrat
  csample <- dades_c[ppssstrat(dades_c$Cens, dades_c$cluster21, resum_estrats_cluster$n_sec_estrat_cluster_abs), ]
  
  
  # Nombre de municipis que es visiten
  csample$cod_municipi <- str_sub(csample$CUSEC, 1, 5)
  distinct_municipis[i] <- n_distinct(csample$cod_municipi)
  
  # Distancia del CEO a la seccio seleccionada en km en cotxe
  sum_distCEO_km_cotxe[i] <- sum(csample$distancia_km_cotxe)
  # temps desl del CEO a la seccio seleccionada en hores en cotxe (NO UTILITZAT)
  # sum_tempsCEO_hores_cotxe[i] <- sum(csample$temps_hores_cotxe) (NO UTILITZAT)
  
  # Assignem valor 1 als municipis que han estat seleccionats
  csample$selected_csample <- 1
  
  # Unim al data.frame inicial la mostra de les mostres aleatòries per cluster
  dades_seccions <- merge(dades_seccions, csample[, c("CUSEC", "selected_csample")], by = "CUSEC", all.x = T)
  
  
  # Assignem valor 0 per les mostres que no han estat seleccionades
  dades_seccions$selected_csample[is.na(dades_seccions$selected_csample)] <- 0
  
  
  # Creem nova variable categòrica que indiqui si son seleccionades o no seleccionades
  dades_seccions$selected_csample_cat <- ifelse(dades_seccions$selected_csample == 1, "Seleccionades", "No seleccionades" )
  ds_balanced_partit <- datasummary_balance( ~ selected_csample,
                                             data = dplyr::select(dades_seccions, selected_csample, pabst21, erc_vots21, psc_vots21, cup_vots21, pp_vots21, comuns_vots21, cs_vots21, junts_vots21, vox_vots21),
                                             dinm_statistic = "p.value", fmt =  '%.3f') 
  
  # Guardem sortida en txt de la distribució ultimes eleccions
  datasummary_balance(~ selected_csample,
                      data = dplyr::select(dades_seccions,
                                           selected_csample, pabst21, erc_vots21, psc_vots21, cup_vots21, pp_vots21, comuns_vots21, cs_vots21, junts_vots21, vox_vots21),
                      output=paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/taula_p_csample.txt"), dinm_statistic = "p.value", fmt =  '%.3f')
  
  
  df_p_csample <- read.table(paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/taula_p_csample.txt"), header = F, skip = 2, sep = "|", dec = ".")
  df_p_csample <- df_p_csample[, which(is.na(df_p_csample[1, ]) == FALSE)]
  rownames(df_p_csample) <- trim(df_p_csample[,1])
  df_p_csample <- df_p_csample[, -1]
  colnames(df_p_csample) <- c("mean_nosec", "std_dev_nosec", "mean_sec", "std_dev_sec", "diff_means", "pvalue")
  
  # Guardem el resultat de la distribució de les últimes elecciones per partit
  ls_p_csample[[i]] <- tibble::rownames_to_column(df_p_csample, "row_names")
  
  
  # Diferencia amb les ultimes eleccions
  diff_electoral[i] <- sum(abs(df_p_csample$diff_means))
  
  
  # Guardem sortida en txt de la distribució variables sociodemogràfiques
  datasummary_balance(~ selected_csample,
                      data = dplyr::select(dades_seccions,
                                           selected_csample, `Edad media de la población`, `Porcentaje de población de 65 y más años`, 
                                           `Renta media por hogar`, pct_restaestat, pct_extranger),
                      output=paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/taula_sd_csample.txt"), dinm_statistic = "p.value", fmt =  '%.3f')
  
  df_sd_csample <- read.table(paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/taula_sd_csample.txt"), header = F, skip = 2, sep = "|", dec = ".")
  df_sd_csample <- df_sd_csample[, which(is.na(df_sd_csample[1, ]) == FALSE)]
  rownames(df_sd_csample) <- trim(df_sd_csample[,1])
  df_sd_csample <- df_sd_csample[, -1]
  colnames(df_sd_csample) <- c("mean_nosec", "std_dev_nosec", "mean_sec", "std_dev_sec", "diff_means", "pvalue")
  
  # Guardem el resultat de la distribució de les variables sociodemogràfiques
  ls_sd_csample[[i]] <- tibble::rownames_to_column(df_sd_csample, "row_names")
  
  # Diferencia amb les variables sociodemografiques de la poblacio
  diff_sociodemografic[i] <- sum(abs(df_sd_csample$diff_means))
  
  
  # Mirem distribució per provincies de la mostra
  dist_prov <- (table(csample$cod_provincia) / sum(table(csample$cod_provincia)) ) * 100
  dist_prov <- as.data.frame(dist_prov)
  dist_prov <- dist_prov[ order(dist_prov$Var1), ]
  
  # Ho guardem en el data frame creat anteriorment
  df_dist_provincies <- cbind(df_dist_provincies, dist_prov$Freq)
  colnames(df_dist_provincies)[ncol(df_dist_provincies)] <- paste("mostra", i, sep = "_")
  
  
  # Anem a afegir el cluster de les adjacents segons el kmeans realitzat anteriorment
  seccions_cluster <- dades_seccions %>%
    dplyr::select(CUSEC, cluster21)
  
  # Creacio df sobre la informacio del CUSEC mare i els seus adjacents pertinents
  df_adjacents_iguals <- df_adjacents %>% 
    left_join(seccions_cluster,  by = c("CUSEC_adj" = "CUSEC")) %>% 
    left_join(seccions_cluster,  by = c("CUSEC_mare" = "CUSEC")) %>%
    dplyr::select(CUSEC_mare, cluster_mare = cluster21.y, CUSEC_adj, adjacent, cluster_adj = cluster21.x) %>%
    # Ens quedem amb les seccions adjacents que pertanyen al mateix cluster que la secció mare
    filter(cluster_mare == cluster_adj) %>%
    # Creem un identificador per numerar en ordre les adjacents que son iguals
    getanID(id = "CUSEC_mare") %>% 
    # Canviem nom de la columna
    rename(mateix_adjacent = .id)
  
  
  # Convertim les variables factors a numeriques
  df_adjacents_iguals$adjacent <- as.numeric(df_adjacents_iguals$adjacent)
  df_adjacents_iguals$mateix_adjacent <- as.numeric(df_adjacents_iguals$mateix_adjacent)
  
  
  # Obtenim les adjacents com a valors en columnes i no en files
  # Fem correcions en el nom de les variables
  df_adjacents_iguals_col <- df_adjacents_iguals %>% 
    dplyr::select(CUSEC_mare, CUSEC_adj, mateix_adjacent) %>% 
    pivot_wider(names_from = mateix_adjacent, values_from = CUSEC_adj) %>% 
    rename_with(~ paste("adjacent", .x, sep = "_")) %>% 
    rename(CUSEC = adjacent_CUSEC_mare)
  
  
  # Creem un dataframe de la mostra amb les seves adjacents
  # Afegim el nom del municipi i ordenem les columnes
  # Afegim el codi de provincia i  el nom
  # Ens quedem amb les columnes que no estiguin buides
  mostra_amb_adjacents <- csample %>% 
    left_join(df_adjacents_iguals_col, by = "CUSEC") %>% 
    dplyr::select(CUSEC, Cens, cod_municipi, cod_comarca, nom_comarca, cod_provincia, nom_provincia, 
                  cluster21, mean_enquestes, min_enquestes, max_enquestes, pob_municipi,
                  adjacent_1:colnames(df_adjacents_iguals_col)[ncol(df_adjacents_iguals_col)]) %>% 
    left_join( dplyr::select(dades_seccions, CUSEC, nom_municipi), by = "CUSEC") %>% 
    relocate(nom_municipi, .after = cod_municipi)
  
  
  # Traiem de la mostra aquelles columnes que estiguin buides 
  mostra_amb_adjacents <- mostra_amb_adjacents[colSums(!is.na(mostra_amb_adjacents)) > 0]
  
  
  # Nombre de seccions de la mostra que no tenen adjacent que pertanyen al mateix cluster
  n_no_adjacents[i] <- sum(is.na(mostra_amb_adjacents$adjacent_1))
  
  
  # De les que no tenen adjacents, busquem subsitutes més properes
  mostra_sense_adjacents <- mostra_amb_adjacents[is.na(mostra_amb_adjacents$adjacent_1), ]
  mostra_sense_adjacents$substitutes <- 1
  
  # Actualitzem les que tenen adjacents amb el mateix cluster
  mostra_amb_adjacents <- mostra_amb_adjacents[!is.na(mostra_amb_adjacents$adjacent_1), ]
  mostra_amb_adjacents$substitutes <- 0
  
  # FALTA INICIALITZAR ELS DIFERENTS ELEMENTS DEL BUCLE 
    # Per a cada secció que no té adjacent que pertany al mateix cluster,  busquem la secció censal
  # que pertany al mateix clúster i els centroides entre aquestes son més properes 
  for(j in 1:nrow(mostra_sense_adjacents)){
    
    # De la llista amb les diferents seccions, anem a buscar al dataframe que pertany al mateix cluster de la CUSEC sense adjacents
    df_mateix_cluster <- ls_dades_seccions_by_cluster[[as.numeric( mostra_sense_adjacents[, c("CUSEC", "cluster21")][j,]["cluster21"] )]]
    
    # De les seccions censals amb el mateix cluster, eliminem de la tria les que han sortit en el mostreig
    # ja que no es vol repetir seccions censals
    df_mateix_cluster_no_csample <- df_mateix_cluster[df_mateix_cluster$CUSEC %in% csample$CUSEC == FALSE, ]
    
    # Busquem les coordenades de la seccio que no té adjacencia
    sec_no_adj <- df_coord_sec[df_coord_sec$CUSEC == as.character( mostra_sense_adjacents[, c("CUSEC", "cluster21")][j,]["CUSEC"] ), ][c("CUSEC", "LONG", "LAT")]
    colnames(sec_no_adj) <- c("CUSEC_no_adj", "LONG_no_adj", "LAT_no_adj")
    
    # Ajuntem les coordenades de les seccions possibles substitutes a les coordenades de la secció sense adjacencies
    df_mateix_cluster_no_csample <- cbind(df_mateix_cluster_no_csample, sec_no_adj)
    # Calculem la distància (en km) de cada coordenada de secció a la secció que estem buscant les seccions del mateix cluster més properes
    df_mateix_cluster_no_csample$distCUSEC_no_adj <- ( distHaversine(df_mateix_cluster_no_csample[,2:3], df_mateix_cluster_no_csample[,6:7]) ) / 1000.0
    
    # Ordenem per les seccions més properes segons distància en km de la secció sense adjacencia que pertany al mateix cluster
    adj_properes_ordre <- df_mateix_cluster_no_csample[order(df_mateix_cluster_no_csample$distCUSEC_no_adj), ]
    
    # Ens quedem amb el CUSEC de les seccions amb el mateix cluster, que no son adjacents però son més proximes
    adj_proximes <- adj_properes_ordre$CUSEC[1:ncol( dplyr::select(mostra_sense_adjacents, starts_with("adjacent_")) )]
    # Ho afegim al df de les mostres sense adjacents 
    mostra_sense_adjacents[j, colnames( dplyr::select(mostra_sense_adjacents, starts_with("adjacent_")) )] <- as.list(adj_proximes)
    
  }
  
  # Guardem en una llista de mida NMOSTRES les seccions censals de la mostra
  # Guardem tant les que tenen seccions censals adjacents del mateix cluster i les que no en tenen en guardem les més properes
  ls_mostra_amb_adjacents[[i]] <- mostra_amb_adjacents
  ls_mostra_sense_adjacents[[i]] <- mostra_sense_adjacents
  
  # Guardem la informació conjunta
  N_random_mostres[[i]] <- rbind(mostra_amb_adjacents, mostra_sense_adjacents)
  
  # Guardem les seccions a l'atzar que s'ha realitzat per a cada mostra
  ls_dades_seccions[[i]] <- dades_seccions

  # Abans de borrar elements guardem alguna informació pertinent:
  info_sc_general[[i]] <- dades_seccions
  
  
  ## ESBORREM ELEMENTS QUE HEM USAT DINS DEL LOOP. RESTAURAR VALORS PER INICIAR DE NOU EL LOOP
  # cluster21 
  # resum_estrats_cluster
  # dades_seccions no s'hauria d'usar dins del loop --> crear una copia abans de iniciar el loop
  # dades_c
  # csample
  # df_p_csample
  # df_sd_csample
  # dist_prov
  # seccions_cluster
  # df_adjacents_iguals
  # df_adjacents_iguals_col
  # mostra_amb_adjacents
  
  rm(list = c("dades_seccions", "ls_dades_seccions_by_cluster", 
              "resum_estrats_cluster", "dades_c", "csample", 
              "df_p_csample", "df_sd_csample", "dist_prov", "seccions_cluster",
              "df_adjacents_iguals", "df_adjacents_iguals_col", "mostra_amb_adjacents", "mostra_sense_adjacents",
              "df_mateix_cluster", "df_mateix_cluster_no_csample", "adj_properes_ordre","adj_proximes") )
  
}
## Fi loop ---------------------------------------- 
end_loop <- Sys.time()
temps_loop <- end_loop - start_loop
saveRDS(temps_loop, paste0(file.path(PROJECT, DTA_OUTPUT_RDS_FOLDER), "/temps_loop.rds"))

# Informació que és interessant guardar
saveRDS(info_sc_general, paste0(file.path(PROJECT, DTA_OUTPUT_RDS_FOLDER), "/mostres_informacio_sc_general.rds"))

# Guardem diferent informacio de totes les extraccions de la mostra
saveRDS(ls_p_csample, paste0(file.path(PROJECT, DTA_OUTPUT_RDS_FOLDER), "/mostres_res_partits.rds"))
saveRDS(ls_sd_csample, paste0(file.path(PROJECT, DTA_OUTPUT_RDS_FOLDER), "/mostres_res_sdemografiques.rds"))

