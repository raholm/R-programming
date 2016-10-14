#' Visualization
#'
#' Visualize average airport delays in USA.
#'
#' @return A ggplot of north america with airports marked as points.
#'
#' @import dplyr
#' @import nycflights13
#' @import ggplot2
#' @import maps
#' @importFrom stats na.omit
#'
#' @export
visualize_airport_delays <- function() {
    world <- map_data("world")
    america <- world[world$region %in% c("Canada", "USA", "Mexico"), ]

    airports_dest <- airports %>% mutate(dest = faa) %>% semi_join(flights, by="dest")
    flights_dest <- flights %>% group_by(dest) %>%
        summarise(mean_dep_delay=mean(dep_delay, na.rm=TRUE),
                  mean_arr_delay=mean(arr_delay, na.rm=TRUE))

    airport_dest_data <- airports_dest %>% left_join(flights_dest, by="dest") %>% na.omit()

    gg <- ggplot() +
        geom_polygon(data=america, aes(x=long, y=lat, group=group), color="white") +
        geom_point(data=airport_dest_data, aes(x=lon, y=lat, size=mean_dep_delay), color="orange") +
        geom_point(data=airport_dest_data, aes(x=lon, y=lat, size=mean_dep_delay), color="black", shape=21) +
        scale_x_continuous(limits=c(-200, 0)) +
        labs(size="Mean Delay Time")
    gg
}
