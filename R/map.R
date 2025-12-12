#' Creation of a sf bounding box
#' @param nelat north-east latitude
#' @param nelng north-east longitude
#' @param swlat south-west latitude
#' @param swlng south-west longitude
fbox <- function(nelat, nelng, swlat, swlng){
  m <- matrix(c(swlng, nelng, nelng, swlng, swlng,
                swlat, swlat, nelat, nelat, swlat), nrow = 5)
  result=sf::st_polygon(list(m))
  return(result)
}

#' Generates a grid with the density of bounding box coverage
#' @param data Dataframe containing metadata with bounding box coordinates
#' @return an sf object containing a grid with the density of bounding box coverage
#' @export
el_map_data=function(data){
  bounding_boxes=data %>%
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


#' Generates a map visualizing the density of bounding box coverage
#' @param map_data an sf object containing a grid with the density of bounding box coverage
#' @param mode "plot" for static map (tmap) or "view" for interactive map (leaflet)
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
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
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
