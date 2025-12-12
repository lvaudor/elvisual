#' Collects keywords table with their frequency
#' @param data the metadata, obtained via el_data()
#' @return A data.frame with keywords and their frequency
#' @export
el_freq_keywords=function(data){
  keywords_data=data %>%
    dplyr::select(ID_fiche, keywords,type) %>%
    unique() %>%
    dplyr::group_by(keywords,type) %>%
    dplyr::summarise(freq=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(freq),keywords)
  return(keywords_data)
}

#' Summarizes metadata with counts of authors, organizations, keywords
#' @param data the metadata, obtained via el_data()
#' @return A data.frame with one row per flashcard and counts of authors, organizations, keywords
#' @export
el_data_summary=function(data){
  nkeywords=data %>%
    dplyr::select(ID_fiche,keywords) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(nkeywords=dplyr::n())
  npersonnes=data %>%
    dplyr::select(ID_fiche,R_personnes) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(npersonnes=dplyr::n())
  norganisations=data %>%
    dplyr::select(ID_fiche,R_orgs) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(norganisations=dplyr::n())
  data_summary=data %>%
    dplyr::select(-R_orgs,-R_personnes,-keywords,-type) %>%
    unique() %>%
    dplyr::left_join(nkeywords, by="ID_fiche") %>%
    dplyr::left_join(npersonnes,by="ID_fiche") %>%
    dplyr::left_join(norganisations,by="ID_fiche")
  return(data_summary)
}
