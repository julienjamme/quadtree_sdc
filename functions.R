#' Calcul le I de Moran sur plusieurs variables en même temps
#'
#' @param sf_data sf dataframe 
#' @param vars variables de sf_data pour chacune desquelles le I de Moran sera calculé
#'
#' @return tibble avec 2 colonnes: var et I_moran
#' 
#' @details
#' Attention le I de Moran est ici fourni sans le résultat du test et donc sans 
#' sa significativité. Utiliser la fonction spdep::moran.test() à la place de 
#' spdep::moran()
#' 
#'
#' @examples
#' select_com_compar <- st_read("data/select_com_compar_200m.gpkg")
#' calculer_I_moran(select_com_compar, c("ind","ind_bis","ind_ter","ind_quat","ind_cinq"))

calculer_I_moran <- function(sf_data, vars, dist = "queen", lim = 4000){
  library(dplyr)
  library(purrr)
  library(spdep)
  library(fields)
  
  # Voisins ------------------------------------------------------------------
  if(dist == "queen"){
    nb <- spdep::poly2nb(sf_data, queen = TRUE)
    # Poids des voisins avec standardisation par ligne
    lw <- spdep::nb2listw(nb, style="W", zero.policy=TRUE)
  } else{ # distance euclidienne
    coords <- st_coordinates(st_centroid(sf_data))
    distance <- rdist(coords[,1:2], coords[,1:2])
    diag(distance) <- 0
    distance[distance >= lim] <- 0
    dist <- 1.e12 %/% (distance*distance)
    dist[dist >= 1.e15] <- 0
    lw <- mat2listw(dist, row.names = NULL, style = "W", zero.policy = TRUE)
  }
  
  map(
    vars, 
    function(v){
      Im <- spdep::moran(
        sf_data[[v]],
        lw,
        length(lw$neighbours),
        spdep::Szero(lw),
        zero.policy=TRUE
      )
      return(tibble(var = v, I_moran = Im$I))
    }
  ) %>% list_rbind()
}
