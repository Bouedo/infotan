
points_gps <- function(dataframe) {
  lat_min <- min(dataframe$location.lat)
  lat_max <- max(dataframe$location.lat)
  lon_min <- min(dataframe$location.long)
  lon_max <- max(dataframe$location.long)
  return(data.frame(lat_min = lat_min, lat_max = lat_max, lon_min = lon_min, lon_max = lon_max))
}

map_indiv <- function(df, id){
  require(leaflet)
  sub_df <- df %>% filter(individual.local.identifier == id)
  leaflet() %>%
    addTiles() %>%
    addMarkers(data = sub_df, lng = sub_df$location.long, lat = sub_df$location.lat)
}

#' @title find_5th function
#' @description The \code{find_5th} La fonction regroupe la trame de données par individual.local.identifier, résume le nombre d'observations (n), organise le résultat dans l'ordre décroissant de n et renvoie une table de données avec 5 lignes uniquement.
#' @param df data frame
#' @return datatable avec 5 lignes
#' @import DT
#' @export
find_5th <- function(df) {
  df %>%
    group_by(individual.local.identifier) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    DT::datatable(options = list(pageLength = 5))
}
