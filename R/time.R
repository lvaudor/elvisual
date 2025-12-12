#' Create a table with time information for each flashcard
#' @param data A data frame containing metadata information
#' @export
el_time_table=function(data){
  time_table=data %>%
    dplyr::select(ID_fiche,debut,fin) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ydebut=lubridate::year(debut),
                  yfin=lubridate::year(fin)+1) %>%
    dplyr::arrange(ydebut,ID_fiche) %>%
    dplyr::select(ID_fiche,ydebut,yfin) %>%
    dplyr::mutate(rang=rank(ydebut))
  return(time_table)
}

#' Create a time_line plot
#' @param time_table the table returned by el_time_table()
#' @param color the color. Defaults to "#04a3bb"
#'
#' @export
el_time_line=function(time_table,color="#04a3bb"){
  time_table=time_table %>%
    dplyr::mutate(rang2=order(rang))
  p=ggplot2::ggplot(time_table,ggplot2::aes(x=rang2))+
    ggplot2::geom_linerange(ggplot2::aes(ymin=ydebut,ymax=yfin), lwd=2, col=color) +
    ggplot2::coord_flip()+
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
  return(p)
}

#' Create a histogram of the number of flashcards per year
#' @param time_table the table returned by el_time_table()
#' @param color the filling color. Defaults to "#04a3bb"
#' @export
el_time_hist=function(time_table,color="#04a3bb"){
  datat=data.frame(year=min(time_table$ydebut,na.rm=T):max(time_table$yfin,na.rm=T)) %>%
    dplyr::mutate(n=purrr::map_int(year,~length(which(.x>=time_table$ydebut & .x<=time_table$yfin))))
  p=ggplot2::ggplot(datat,ggplot2::aes(x=year,y=n))+
    ggplot2::geom_col(fill=color)+
    ggplot2::xlab("année")+
    ggplot2::ylab("nombre de fiches par année")
  return(p)
}

#' Create a histogram of the duration of flashcard-related projects
#' @param data the table returned by el_time_table()
#' @param color the filling color. Defaults to "#04a3bb"
#' @export
el_time_duration=function(time_table,color="#04a3bb"){
  datat=time_table %>%
    dplyr::mutate(duration=yfin-ydebut)
  p=ggplot2::ggplot(datat,ggplot2::aes(x=duration))+
    ggplot2::geom_histogram(breaks=c(0,1,5,10,20,50,100,200), fill=color)+
    ggplot2::xlab("longueur de période")
  return(p)
}
