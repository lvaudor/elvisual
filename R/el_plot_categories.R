#' Plots frequencies of categories
#' @param categories the data.frame with categories, obtained via el_categories()
#' @return a plot with counts of categories
#' @export
el_plot_categories=function(categories, color="#04a3bb"){
  freq_categories=categories %>%
    dplyr::group_by(categories) %>%
    dplyr::tally()
  ggplot2::ggplot(freq_categories,
    ggplot2::aes(x=forcats::fct_reorder(categories,n),y=n))+
    ggplot2::geom_col(fill=color) +
    ggplot2::coord_flip() +
    ggplot2::xlab("")
}
