#' Build graph data
#' @param data the dataset as obtained via el_data()
#' @param nodetype either "personne" or "organisation", depending on whether you want to build a graph of people or organizations
#' @return the graph data
#' @export
el_graph_data=function(data,nodetype){
  if(nodetype=="personne"){
      g_data=data %>%
        dplyr::select(ID_fiche,
                      linkingvar=R_personnes)
  }
  if(nodetype=="organisation"){
      g_data=data %>%
        dplyr::select(ID_fiche,
                      linkingvar=R_orgs)
  }
  g_data=g_data %>%
    dplyr::mutate(linkingvar=as.vector(linkingvar)) %>%
    unique()
  collaborations=g_data %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::tally()
  dat=c()
  for(i in 1:nrow(collaborations)){
    dat_tmp=g_data %>%
      dplyr::filter(ID_fiche==collaborations$ID_fiche[i]) %>%
      tidyr::expand(V1=linkingvar,V2=linkingvar) %>%
      dplyr::mutate(V2=dplyr::case_when(V2==V1~NA,
                                TRUE~V2)) %>%
      dplyr::mutate(check = purrr::map2_lgl(V1,V2,function(x,y){return(as.logical(x<y))}))
    dat=rbind(dat,dat_tmp)
  }
  dat=dat %>%
    dplyr::group_by(V1,V2)%>%
    dplyr::summarise(nfiches=dplyr::n()) %>%
    dplyr::arrange(desc(nfiches)) %>%
    dplyr::ungroup()
  return(dat)
}

#' Plot graph
#' @param graph_data the graph data as obtained via el_graph_data()
#' @param shorten_name logical, whether to shorten the names of nodes (default: FALSE)
#' @return the graph plot
#' @export
el_graph=function(graph_data, shorten_name=FALSE){
  if(shorten_name){
    graph_data=graph_data %>%
      dplyr::mutate(V1=stringr::str_replace(V1,"[^A-Z]*(?=\\s)","")) %>%
      dplyr::mutate(V1=stringr::str_replace(V1,"[^A-Z]*(?=\\-)","")) %>%
      dplyr::mutate(V2=stringr::str_replace(V2,"[^A-Z]*(?=\\s)","")) %>%
      dplyr::mutate(V2=stringr::str_replace(V2,"[^A-Z]*(?=\\-)",""))
  }
  nodes=graph_data %>%
    dplyr::filter(is.na(V2))
  edges=graph_data %>%
    dplyr::filter(!is.na(V2))
  g=tidygraph::tbl_graph(nodes=nodes,edges=edges,directed=FALSE)
  layout= ggraph::create_layout(g, layout="igraph",algorithm="nicely")
  g=ggraph::ggraph(layout) +
    ggraph::geom_edge_link(ggplot2::aes(edge_width=nfiches),color="grey") +
    ggraph::geom_node_point(ggplot2::aes(size=nfiches)) +
    ggraph::geom_node_text(ggplot2::aes(label=V1), repel=T, check_overlap=FALSE) +
    ggraph::scale_edge_width(range=c(2,6)) +
    ggplot2::scale_x_continuous(limits=extendrange(layout$x, f=0.1)) +
    ggplot2::scale_y_continuous(limits=extendrange(layout$y, f=0.1)) +
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                    axis.text.y=ggplot2::element_blank(),
                    axis.ticks.y=ggplot2::element_blank(),
                    axis.title.x=ggplot2::element_blank(),
                    axis.text.x=ggplot2::element_blank(),
                    axis.ticks.x=ggplot2::element_blank(),
                    panel.background=ggplot2::element_blank())
  return(g)
}
