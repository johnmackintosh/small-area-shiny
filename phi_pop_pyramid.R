# Creates pyramid plot with male values reversed so they appear on left side of x axis

#' Create blue/ grey pyramid plot showing population counts by age band
#'
#' @param base_df source data
#' @param xcol age band
#' @param ycol totals for age band by sex / gender
#' @param fill_by the column used to determine the fill values, usually `Sex`
#' @param male_val character specifying the male text value e.g. Male or Males
#' @param female_val character specifying the female text value
#' @param male_col character specifying the desired fill colour for the male value
#' @param female_col character specifying the desired fill colour for the female value
#' @param nbreaks number of breaks on x axis
#' @param ylimit the maximum population value of all the age bands. Limits will not exceed this value
#' @return ggplot object
#' @export
#'
#' @examples
#'
#'
#'
phi_pop_pyramid <- function(base_df,
                            xcol,
                            ycol,
                            fill_by,
                            male_val ,
                            female_val,
                            male_col = "#0072B2",
                            female_col = "#56B4E9",
                            nbreaks = 8,
                            ylimit = 1000) {

  neg_y <- ylimit * -1

  #internal fill scale for manual use

  mycols <- c(male_col, female_col)
  myvals <- c(male_val, female_val)

  int_scale <- stats::setNames(mycols, myvals)


  # col_breaks so we have a known name for prettifying y axis breaks & labels

  female_data <-   base_df |>
    dplyr::filter({{fill_by}} == female_val) |>
    dplyr::mutate(col_breaks = {{ycol}})

  # negate the male column values

  male_data <-  base_df |>
    dplyr::filter({{fill_by}} == male_val) |>
    dplyr::mutate({{ycol}} := {{ycol}} * -1) |>
    dplyr::mutate(col_breaks = {{ycol}})


  tempdf <- dplyr::bind_rows(female_data, male_data)


  p <- ggplot2::ggplot(data = NULL,
                       ggplot2::aes({{xcol}},
                                    {{ycol}},
                                    fill = {{fill_by}})) +
    ggplot2::geom_col(data = male_data) +
    ggplot2::geom_col(data = female_data)

  p <- p + ggplot2::scale_fill_manual(name = "",
                                      breaks = c(male_val, female_val),
                                      values = c(male_col, female_col))

  p <- p +  ggplot2::scale_y_continuous(breaks = pretty(tempdf$col_breaks,
                                                        n = nbreaks),
                                        labels = abs(pretty(tempdf$col_breaks,
                                                            n = nbreaks)),
                                        limits = c(neg_y, ylimit))


  p <- p +  ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0),
                   plot.caption.position = "plot")

  p <- p +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  p <- p + labs(x = "",
                y = "Population")

  p <- p +  ggplot2::coord_flip()

  p

}








