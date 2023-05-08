## Outputs ---------------------------------------- 

## Fitxer a passar a l'empresa que realitzara les enquestes presencials ---------------------------------------- 

# Per a cada mostra es crea una columna que indica a quina extracció correspon la mostra escollida
nom_mostra <- as.data.frame( matrix(rep( paste("mostra", 1:NMOSTRES, sep = "_"), (NSECCIONS+ABSCORRECCIO)), ncol=NMOSTRES, byrow=T) )
nom_llista <- list() 
for(i in 1:ncol(nom_mostra)) { # Using for-loop to add columns to list
  nom_llista[[i]] <-  as.data.frame(nom_mostra[, i])
  nom_llista[[i]][, 2] <- c(1:(NSECCIONS+ABSCORRECCIO))
  colnames(nom_llista[[i]]) <- c("mostra", "ruta")
}
N_random_mostres <- Map(cbind, N_random_mostres, nom_llista)

# Ordenem el resultat final de puntuació de mostres i ens quedem amb NMILLORS
nom_millors_mostres <- df_all[order(df_all$end_score_weights),]$id_mostra[1:NMILLORMOSTRES]
millors_mostres <- N_random_mostres[nom_millors_mostres]

# Fitxer que necessita l'empresa per realitzar el treball de camp.
# Es passa un forquilla del nombre d'entrevistes que han de fer.
openxlsx::write.xlsx(lapply(millors_mostres, function(x) dplyr::select(x, !(mean_enquestes))),
                     paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostra_seccions.xlsx"))

# Fitxer de les poblacions de la mostra amb menys de 10.000 habitants
# Ja que s'avisa als ajuntaments de que hi haurà un inici de camp
openxlsx::write.xlsx( lapply(millors_mostres, function(x) filter(x, pob_municipi < 10000)),
                      paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostra_seccions_menys_10000_hab.xlsx") )

# Fitxer dels resultats de la distibució dels partits i de les variables sociodemogràfiques pertinents de les mostres finals
openxlsx::write.xlsx(ls_p_csample[nom_millors_mostres], paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostra_res_partits.xlsx"))
openxlsx::write.xlsx(ls_sd_csample[nom_millors_mostres], paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostra_res_sociodemografiques.xlsx"))


## Mapa de les mostres guardades, un total de NMILLORSMOSTRES ----------------------------------------------------------

# En cas que anteriorment s'hagin guardat els mapes, eliminem els fitxers html de la carpeta
unlink( paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/", "*.html") )

# Mostres guardades en el fitxer per passar a l'empresa
ls_dades_seccions_save <- ls_dades_seccions[nom_millors_mostres]

# Volem realitzar tants mapes com mostres hem guardat
no_adj <- character()

for (i in 1:NMILLORMOSTRES){
  # Fem una copia de sf_local_cat
  sf_local_cat_copy <- sf_local_cat
  
  # Unim a les dades sf les variables que tenim sobre la pertinença del cluster i la seleccio de la mostra
  sf_local_cat_copy@data <- sf_local_cat_copy@data %>% 
    left_join(dplyr::select(ls_dades_seccions_save[[i]], CUSEC, cluster21, selected_csample_cat), by = "CUSEC") %>% 
    left_join(dplyr::select(millors_mostres[[i]], CUSEC, adjacent_1, substitutes, ruta), by = "CUSEC")
  
  # sf de la mostra per pintar les vores
  sf_mostra_csample <- sf_local_cat_copy[sf_local_cat_copy@data$selected_csample_cat == "Seleccionades", ]
  
  # sf de la mostra que la seva primera adjacencia no està al costat
  no_adj <- sf_mostra_csample@data[sf_mostra_csample@data$substitutes == 1, ]$adjacent_1
  sf_adj1_no_boundary <- sf_local_cat_copy[sf_local_cat_copy@data$CUSEC %in% no_adj, ]
  
  # Anem a crear el mapa visual
  tmap_options(check.and.fix = TRUE)
  tmap_mode("view")
  
  # Creacio del mapa definitiu
  t <- tm_shape (sf_local_cat_copy) + 
    tm_fill (
      col = "cluster21",
      style = "cat",
      n = 6,
      alpha = .5,
      palette = get_brewer_pal(palette="Dark2", n=6, plot=FALSE),
      colorNA = "grey50" ,
      legend.reverse = TRUE,
      title = "Cluster",
      popup.vars = c(
        "Provincia" = "NPRO",
        "Seccio: " = "CUSEC",
        "Cluster" = "cluster21",
        "Substituta" = "adjacent_1"
      ),
      id = "NMUN"
    )  + 
    tm_borders (col = "black" , lwd = .4) +
    # Pintem la vora de les seccions censals de la mostra
    tm_shape(sf_mostra_csample) +
    tm_symbols(col = "black", alpha = 0.5, scale = .01) +
    tm_borders (col = "red" , lwd = 1) +
    # Pintem la vora de les substitutes que no son adjacent a aquesta
    tm_shape(sf_adj1_no_boundary) +
    tm_symbols(col = "black", alpha = 0.5, scale = .01) +
    tm_borders (col = "blue" , lwd = 0.8)
  
  
  tmap_save(t, paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/", i, "_mapa_csample", "_", nom_millors_mostres[i], ".html"))
  
  
  rm(list = c("sf_local_cat_copy", "sf_mostra_csample", "sf_adj1_no_boundary", "t"))
  
}