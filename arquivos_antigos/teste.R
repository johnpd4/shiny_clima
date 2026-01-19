spdf = ASGS.foyer::STE_2016_simple

proj4string(spdf) = CRS("+init=epsg:4326")

grd <- makegrid(spdf, n = 10000)
colnames(grd) <- c("x", "y")

grd_pts <- SpatialPoints(
  coords      = grd, 
  proj4string = CRS(proj4string(spdf))
)

grd_pts_in <- grd_pts[spdf, ]

gridded(grd_pts_in) = TRUE

plot(grd_pts_in)


#rs = read_sf("./rs_shapefile/RS_UF_2023.shp")
rs = geobr::read_state("RS")

poligono = sp::Polygon(sf::st_coordinates(rs$geom)[, 1:2])
poligonos = sp::Polygons(list(poligono), ID = "1")

spatial_poligono = SpatialPolygons(list(poligonos))

#spatial_df = SpatialPolygonsDataFrame(Sr = spatial_poligono, data = dados |> select(-lat, -lon))

proj4string(spatial_poligono) = CRS("+init=epsg:4674")

grade <- makegrid(spatial_poligono, n = 10000)
colnames(grade) <- c("x", "y")

grade_pts <- SpatialPoints(
  coords      = grade, 
  proj4string = CRS(proj4string(spatial_df))
)

grade_pts_in <- grade_pts[spatial_poligono, ]

plot(grade_pts_in)
