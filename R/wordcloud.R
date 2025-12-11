#' Nuage des mots
#' @param table_keywords table_keywords, obtenues via el_table_keywords()
#' @param keywords_to_remove a vector of keywords to remove from the cloud
#' @param max_size the maximum size of a word
#' @param low_color the color of most frequent word (defaults to  "#442484")
#' @param high_color the color of least frequent word (defaults to "#04a3bb")
#' @return Un data.frame avec les mots-clés, leur type, leur fréquence
#' @export
#' @examples
#' metadata_keywords %>%
#' dplyr::filter(type=="FREE")
#' el_wordcloud(metadata_keywords_FREE,keywords_to_remove=c("OHM VR","Rhône"))
el_wordcloud=function(table_keywords, keywords_to_remove, max_size=10, low_color="#442484", high_color="#04a3bb"){
  ggplot2::ggplot(table_keywords %>%
          dplyr::filter(!(keywords %in% keywords_to_remove)),
            ggplot2::aes(label = keywords,
                         size=freq,
                         color=log10(freq))) +
    ggwordcloud::geom_text_wordcloud() +
    ggplot2::theme_minimal()+
    ggplot2::scale_color_gradient(low = low_color, high = high_color )+
    ggplot2::scale_size_area(max_size = max_size)
}



