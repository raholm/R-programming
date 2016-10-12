#' Visualization
#'
#' Visualize average airport delays in USA.
#'
#' @import dplyr
#' @import nycflights13
#' @import ggplot2
#' @import maps
#'
#' @export
visualize_airport_delays <- function() {
    world <- map_data("world")
    america <- world[world$region %in% c("Canada", "USA", "Mexico"), ]

    airports_dest <- airports %>% mutate(dest = faa) %>% semi_join(flights)
    flights_dest <- flights %>% group_by(dest) %>%
        summarise(mean_dep_delay=mean(dep_delay, na.rm=TRUE),
                  mean_arr_delay=mean(arr_delay, na.rm=TRUE)) %>%
        mutate(total_mean_delay = mean_dep_delay + mean_arr_delay)
    airport_dest_data <- airports_dest %>% left_join(flights_dest)

    ## airports_origin <- airports %>% mutate(origin = faa) %>% semi_join(flights)
    ## flights_origin <- flights %>% group_by(origin) %>%
    ##     summarise(mean_dep_delay=mean(dep_delay, na.rm=TRUE),
    ##               mean_arr_delay=mean(arr_delay, na.rm=TRUE)) %>%
    ##     mutate(total_mean_delay = mean_dep_delay + mean_arr_delay)
    ## airport_origin_data <- airports_origin %>% left_join(flights_origin)

    gg <- ggplot() +
        geom_polygon(data=america, aes(x=long, y=lat, group=group), color="white") +
        geom_point(data=airport_dest_data, aes(x=lon, y=lat, size=total_mean_delay), color="orange") +
        geom_point(data=airport_dest_data, aes(x=lon, y=lat, size=total_mean_delay), color="black", shape=21) +
        scale_x_continuous(limits=c(-200, 0)) +
        labs(size="Mean Delay Time")
    gg
}
