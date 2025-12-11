#' Création d'une bounding box en objet sf
#' @param nelat Latitude nord-est
#' @param nelng Longitude nord-est
#' @param swlat Latitude sud-ouest
#' @param swlng Longitude sud-ouest
fbox <- function(nelat, nelng, swlat, swlng){
  m <- matrix(c(swlng, nelng, nelng, swlng, swlng,
                swlat, swlat, nelat, nelat, swlat), nrow = 5)
  result=sf::st_polygon(list(m))
  return(result)
}

#' Génération d'une grille de densité de recouvrement des bounding box
#' @param metadata Dataframe de métadonnées contenant les colonnes xmin, xmax, ymin, ymax
#' @return Objet sf contenant une grille avec la densité de recouvrement des bounding box
#' @export
el_map_data=function(metadata){
  bounding_boxes=metadata %>%
    dplyr::select(ID_fiche,xmin,ymin,xmax,ymax) %>%
    unique() %>%
    na.omit()
  bb=bounding_boxes %>%
    dplyr::mutate(geometry = purrr::pmap(list(nelat=ymax,
                                     nelng=xmax,
                                     swlat=ymin,
                                     swlng=xmin),
                                fbox)) %>%
    sf::st_as_sf(sf_column_name = "geometry")

    # Calculer la densité de recouvrement des bounding box
    grid=sf::st_make_grid(bb, cellsize = c(0.1, 0.1)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(intersection=sf::st_intersects(geometry,bb))  %>%
      dplyr::mutate(density=purrr::map_int(intersection,~length(.x))) %>%
      dplyr::select(-intersection) %>%
      sf::st_as_sf(sf_column_name = "geometry")
    return(grid)
}


#' Génération d'une carte de densité de recouvrement des bounding box
#' @param map_data Objet sf contenant une grille avec la densité de recouvrement des bounding box
#' @param mode Mode de visualisation: "plot" pour une carte statique avec tmap (valeur par défaut), "view" pour une carte interactive avec leaflet.
#' @export
el_map=function(map_data, mode="plot"){
  if(mode=="plot"){
    tmap::tmap_mode("plot")
    data(fond_france)
    fond=fond_france

    map=fond +
      tmap::tm_shape(map_data) +
      tmap::tm_polygons(fill = "density",
                  fill.scale = tmap::tm_scale_continuous(values = "brewer.yl_or_rd"),
                  fill.legend = tmap::tm_legend(title = "Densité",
                                          orientation = "landscape",
                                          frame = FALSE),
                  lwd=0,
                  fill_alpha=0.5
      )
  }
  if(mode=="view"){
    map=leaflet::leaflet()  %>%
      leaflet::addProviderTiles(providers$OpenStreetMap) %>%
      leaflet::addPolygons(
        data = map_data,
        fillColor = ~leaflet::colorNumeric("YlOrRd", density)(density),
        fillOpacity = 0.5,
        weight = 0,
        color = NA
      ) %>%
      leaflet::addLegend(
        pal = leaflet::colorNumeric("YlOrRd", map_data$density),
        values = map_data$density,
        title = "Densité"
      )
  }
  return(map)
}
