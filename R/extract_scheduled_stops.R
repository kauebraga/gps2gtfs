
#' Extract the most common set of scheduled stops for each route
#'
#' Returns the most common service (the sequence of stops on the `stop_times` file) for each shape
#'
#' @param gtfs A GTFS object.
#' @param service_id A character specifying a service (from `calendar`) to subset, if desired. This is specially
#' useful for large GTFS file where one is interested only on weekdays trips, for example.
#' @param route_id A character specifying a route (from `routes`) to subset, if desired
#'
#' @return A `data.table`.with the scheduled service (sequence of stops) for each `route` and `shape_id`
#'
#' @section Details:
#' Details here.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_geometry <- get_trip_geometry(gtfs)
#' head(trip_geometry)
#'
#' # the above is identical to
#' trip_geometry <- get_trip_geometry(gtfs, file = c("shapes", "stop_times"))
#' head(trip_geometry)
#'
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#' trip_geometry <- get_trip_geometry(gtfs, trip_id = trip_ids)
#' trip_geometry
#' plot(trip_geometry["origin_file"])
#'
#' @export
#' @import data.table



extract_scheduled_stops <- function(gtfs, service_id = NULL, route_id = NULL) {


  env <- environment()

  # get trips
  trips <- gtfs$trips

  # filter trips by route
  trips <- if (!is.null(route_id)) trips[route_id %in% get("route_id", envir = env)] else trips

  # filter trips by service
  trips <- if (!is.null(service_id)) trips[service_id %in%  get("service_id", envir = env)] else trips

  stops <- gtfs$stops[, .(stop_id, stop_name, stop_lon, stop_lat)]
  # stops_sf <- stops %>%
  #   st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
  #   mutate(id_stop = 1:n())

  # abrir stop times
  # get stoptimes
  stop_times <- gtfs$stop_times[trip_id %in% trips$trip_id]
  stop_times <- stop_times[, .(trip_id, stop_id, arrival_time, departure_time, stop_sequence)]

  # abrir trips

  trips <- trips[, .(trip_id, route_id, shape_id)]

  stops_linhas_vai <- merge(stop_times, trips, by = "trip_id", sort = FALSE)
  # identify which trip will have the max number of stops for that route
  stops_linhas_vai <- stops_linhas_vai[, .N, by = .(trip_id, shape_id)]
  # filter only the trip that will have the max number of stops
  stops_linhas_vai <- stops_linhas_vai[, .SD[which.max(N)], by = shape_id]

  stops_linhas <- stop_times[trip_id %in% stops_linhas_vai$trip_id]
  stops_linhas <- merge(stops_linhas, trips, by = "trip_id", sort  = FALSE)
  stops_linhas <- merge(stops_linhas, stops, by = "stop_id", sort  = FALSE)
  stops_linhas <- stops_linhas[, .(route_id, shape_id, stop_id, stop_name, stop_sequence, stop_lon, stop_lat)]


}
