## Funcions que s'usen en el codi ---------------------------------------- 

# Funcio per arrodonir sempre a 100 donat un vector de percentatges
round_percent <- function(x) {
  x <- x / sum(x) * 100
  res <- floor(x)
  rsum <- sum(res)
  if (rsum < 100) {
    o <- order(x %% 1, sample(length(x)), decreasing = TRUE)
    res[o[1:(100 - rsum)]] <- res[o[1:(100 - rsum)]] + 1
  }
  res
}

# Funció per crear un ordre de puntuació a partir d'uns resultats
# calcul_score <- readLines("bop/config/calcul_score.txt")
calcul_score <- function(x) {
  ## Creo vector on guardo la puntuacio
  score <- length(x)
  ## Guardem l'ordre
  index_ordre <- order(x)
  ## Guardem valors en ordre
  valor_ordre <- sort(x)
  
  for (i in 1:length(x)) {
    if(i == 1) {
      score[index_ordre[i]] <- i
    } else{
      if(valor_ordre[i] == valor_ordre[(i-1)]){
        score[index_ordre[i]] <- score[index_ordre[(i-1)]]
      } else {
        score[index_ordre[i]] <- i
      }
    }
  }
  return(score)
}


## Analisi resultats extraccio mostres aleatories ---------------------------------------------- 

# # nombre diferents de municipis que toca visitar
# distinct_municipis
# # km amb distancia h des del CEO de cada secció
# sum_distCEO_h
# # distància des del CEO a la seccio seleccionada en km en cotxe
# sum_distCEO_km_cotxe 
# # temps del del CEO a la seccio seleccionada en hores en cotxe
# sum_tempsCEO_hores_cotxe
# # estadistics de la diferencia electoral
# diff_electoral
# # estadisitcs de la diferencia amb les variables sociodemogràfiques
# diff_sociodemografic
# # nombre de seccions no adjacents
# n_no_adjacents

# Calculem la distribució real de les províncies a partir del cens de les últimes eleccions del Parlament
dist_cens_prov <- dades_seccions21 %>% 
  group_by(nom_provincia) %>% 
  summarise(Cens = sum(Cens), Propcens = sum(propcens)) %>% 
  mutate(rPropcens = round_percent(Propcens))

# estadístics de la diferencia amb la distribució per provincia
diff_dist_prov <- as.numeric(colSums( abs(df_dist_provincies - dist_cens_prov$rPropcens  ) ))

# Creacio resultats junts
df_analisi_mostra <- data.frame(distinct_municipis, #sum_distCEO_h, tenim altres indicadors
                                sum_distCEO_km_cotxe, sum_tempsCEO_hores_cotxe,
                                diff_electoral, diff_sociodemografic, #n_no_adjacents, ja no importa el nombre de no adjacences. S'ha automatitzat cerca.
                                diff_dist_prov)


# Creem data.frame sobre la distribució de províncies
df_all <- df_analisi_mostra %>% 
  mutate(
    id_mostra = paste("mostra", 1:NMOSTRES, sep = "_"),
    ## Per a cada variable que ens interessa es fa una puntuació del 1 al nombre mostres
    ## Millor mostra aquella que te un score baix
    ## Per als valors repetits no es penalitza puntuació
    score_municipis = calcul_score(distinct_municipis),
    #score_distCEO_h = calcul_score(sum_distCEO_h),
    score_distCEO_km_cotxe = calcul_score(sum_distCEO_km_cotxe),
    score_tempsCEO_hores_cotxe = calcul_score(sum_tempsCEO_hores_cotxe),
    score_electoral = calcul_score(diff_electoral),
    score_sociodemografic = calcul_score(diff_sociodemografic),
    #score_no_adjacents = calcul_score(n_no_adjacents),
    score_dist_provincies = calcul_score(diff_dist_prov)
  ) %>% 
  relocate(id_mostra, .before = distinct_municipis)

# Creem una nova variable anomenada score. Aquella mostra (fila) que tingui menor valor, es la millor mostra.
# Aquí es podria assignar diferents pesos per tal de penalitzar més o menys
df_all$end_score <- rowSums( dplyr::select(df_all, starts_with("score_")) )

# Creem una nova puntualizació de cada mostra a partir dels diferents score obtinguts
df_all$end_score_weights <- ( (0.2 * df_all$score_municipis) +
                              (2 * df_all$score_distCEO_km_cotxe) +
                              (0.2 * df_all$score_tempsCEO_hores_cotxe) +
                              (1.3 * df_all$score_electoral) +
                              (1.3 * df_all$score_sociodemografic) +
                              (1 * df_all$score_dist_provincies) )

write_csv2(df_all, paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostres_resum_score.csv"))


# Resum estadístics i nivells de confiança
alpha <- 1 - LEVEL
error <- qnorm(1 - (alpha/2))

IC_estadistic <- data.frame(
  estadistic = colnames(df_analisi_mostra),
  valor = apply(df_analisi_mostra, 2, mean),
  IC_inf = apply(df_analisi_mostra, 2, mean) - error * (apply(df_analisi_mostra, 2, sd) / sqrt(NMOSTRES)),
  IC_sup = apply(df_analisi_mostra, 2, mean) + error * (apply(df_analisi_mostra, 2, sd) / sqrt(NMOSTRES)),
  nsim = nrow(df_analisi_mostra)
)

write_csv2(IC_estadistic, paste0(file.path(PROJECT, DTA_OUTPUT_FOLDER), "/mostres_estadistics_ic.csv"))
