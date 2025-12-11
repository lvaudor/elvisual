#' Create a table with time information for each flashcard
#' @param metadata A data frame containing metadata information
#' @export
el_table_time=function(metadata){
  table_time=metadata %>%
    dplyr::select(ID_fiche,debut,fin) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ydebut=lubridate::year(debut),
                  yfin=lubridate::year(fin)+1) %>%
    dplyr::arrange(ydebut,ID_fiche) %>%
    dplyr::select(ID_fiche,ydebut,yfin) %>%
    dplyr::mutate(rang=rank(ydebut))
  return(table_time)
}

#' Create a timeline plot
#' @param table_time the table returned by el_table_time()
#' @export
el_timeline=function(table_time){
  table_time=table_time %>%
    dplyr::mutate(rang2=order(rang))
  p=ggplot2::ggplot(table_time,ggplot2::aes(x=rang2))+
    ggplot2::geom_linerange(ggplot2::aes(ymin=ydebut,ymax=yfin), lwd=2) +
    ggplot2::coord_flip()+
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
  return(p)
}

#' Create a histogram of the number of flashcards per year
#' @param data the table returned by el_table_time()
#' @export
el_timehist=function(table_time){
  datat=data.frame(year=min(table_time$ydebut,na.rm=T):max(table_time$yfin,na.rm=T)) %>%
    dplyr::mutate(n=purrr::map_int(year,~length(which(.x>=table_time$ydebut & .x<=table_time$yfin))))
  p=ggplot2::ggplot(datat,ggplot2::aes(x=year,y=n))+
    ggplot2::geom_col()+
    ggplot2::xlab("année")+
    ggplot2::ylab("nombre de fiches par année")
  return(p)
}

#' Create a histogram of the duration of flashcard-related projects
#' @param data the table returned by el_table_time()
#' @export
el_durationhist=function(table_time){
  datat=table_time %>%
    dplyr::mutate(duration=yfin-ydebut)
  p=ggplot2::ggplot(datat,ggplot2::aes(x=duration))+
    ggplot2::geom_histogram(breaks=c(0,1,5,10,20,50,100,200))+
    ggplot2::xlab("longueur de période")
  return(p)
}
