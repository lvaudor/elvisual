fbox <- function(nelat, nelng, swlat, swlng){
  m <- matrix(c(swlng, nelng, nelng, swlng, swlng,
                swlat, swlat, nelat, nelat, swlat), nrow = 5)
  return(st_polygon(list(m)))
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
    grid=st_make_grid(bb, cellsize = c(0.1, 0.1)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(intersection=st_intersects(geometry,bb))  %>%
      dplyr::mutate(density=purrr::map_int(intersection,~length(.x))) %>%
      dplyr::select(-intersection) %>%
      sf::st_as_sf(sf_column_name = "geometry")
    return(grid)
}


#' Génération d'une carte de densité de recouvrement des bounding box
#' @param map_data Objet sf contenant une grille avec la densité de recouvrement des bounding box
#' @param mode Mode de visualisation: "plot" pour une carte statique avec tmap (valeur par défaut), "view" pour une carte interactive avec leaflet.
el_map=function(map_data, mode="plot", fond=fond_france){
  if(mode=="plot"){
    tmap_mode("plot")
    if(fond=="fond_france"){
      data(fond_france)
      fond=fond_france
    }

    map=fond +
      tm_shape(map_data) +
      tm_polygons(fill = "density",
                  fill.scale = tm_scale_continuous(values = "brewer.yl_or_rd"),
                  fill.legend = tm_legend(title = "Densité",
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
