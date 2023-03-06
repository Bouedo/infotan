library(devtools)
library(jsonlite)
library(dplyr)
library(tidyr)
library(usethis)

arrets_tan <- jsonlite::fromJSON("https://open.tan.fr/ewp/arrets.json")
arrets_tan <- arrets_tan[,-3]

arrets_tan_filtre <- arrets_tan %>% unnest(ligne, keep_empty = TRUE)

#' @title is_station
#' @description The function takes an alphabetic code as input and returns a logical value indicating whether the station exists.
#' @param code Character string representing the station code.
#' @return A logical value indicating whether the station exists.
#' @examples
#' is_station("ABCH")
#' is_station("MONERO")

is_station <- function(code) {
  exists <- code %in% arrets_tan$codeLieu
  return(exists)
}

#' @title find_closest_name
#' @description The function takes two inputs, input and list_stations. It calculates the Levenshtein distance between the input and all elements of list_stations. The station with the smallest distance is considered the closest station and is returned. If the distance to the nearest station is 0, it means that the input is exactly equal to the station name and the station name is returned. If the distance is less than or equal to the length of the entry, the nearest station is returned. Otherwise, NA is returned.
#' @import stringdist
#' @param input A string representing the input value.
#' @param list_stations A vector of strings representing station names.
#' @return A string or NA, representing the name of the nearest station or NA if no close match is found.

find_closest_name <- function(input, list_stations) {
  library(stringdist)
  closest_stations <- stringdist(input, list_stations, method = "lv")
  closest_station_index <- which.min(closest_stations)
  closest_station_name <- list_stations[closest_station_index]
  if (closest_stations[closest_station_index] == 0) {
    closest_station_name
  } else if (closest_stations[closest_station_index] <= nchar(input)) {
    closest_station_name
  } else {
    NA
  }
}

#' @title get_waiting_time
#' @description Retrieves information about the waiting time of the next trams
#' @param locationcode The station code for which the waiting time for the next trams is required
#' @return A dataframe with information on the terminus and the time of the next trams
get_waiting_time <- function(codeLieu) {
  library(jsonlite)
  api_url <- paste0("https://open.tan.fr/ewp/tempsattente.json/", codeLieu)
  api_response <- fromJSON(api_url)

  api_response_subset <- api_response[c("terminus", "temps")]
  return(api_response_subset)
}

#' @title get_next_transport at Nantes
#' @description This function allows you to obtain information on the schedules of the next trams
#' @param input A character string representing the full station name, the "libelle"
#' @examples get_next_transport("Bourgeonnière")
#' 1      Orvault Grand Val   2mn
#' @return A data frame of the next passages for all lines passing through the station



get_next_transport <- function(arret) {
  station_libelle <- find_closest_name(arret, arrets_tan$libelle)
  print(paste0("La station que vous cherchez semble être : ", station_libelle))
  output = as.character(unlist(station_libelle))
  output2 = subset(arrets_tan, libelle == output, select = c(codeLieu))
  output3 = lapply(output2, function(x) x[1])
  output4 = as.character(unlist(output3))
  next_passages <- get_waiting_time(output4)
  print('Voici la liste des prochains passages : ')
  return(next_passages)
}






