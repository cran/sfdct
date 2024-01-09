## ----example------------------------------------------------------------------


library(sf)
library(sfdct)
nc <- read_sf(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_triangles <- ct_triangulate(nc)

plot(st_geometry(nc_triangles),   col = viridisLite::viridis(nrow(nc_triangles)))

## ----hone---------------------------------------------------------------------
i_feature <- 25
nc1 <- nc[c(i_feature, unlist(st_touches(nc[i_feature, ], nc))), ]
plot(st_geometry(nc1),col = viridisLite::viridis(nrow(nc1)))

## subvert st_area because we really don't want m^2
st_crs(nc1) <- NA
areas <- st_area(nc1)
st_crs(nc1) <- st_crs(nc)
nc1_triangles <- ct_triangulate(nc1, a = min(areas)/5)
bcol <- viridisLite::viridis(nrow(nc1_triangles))
plot(st_geometry(nc1_triangles), col = NA, border = bcol)

nc2_triangles <- ct_triangulate(nc1, a = min(st_area(st_set_crs(nc1, NA)))/25)
plot(st_geometry(nc2_triangles), col = NA, border = bcol)


## ----MULTIPOINT---------------------------------------------------------------
## manual cast to MULTIPOINT originally required
#st_geometry(nc1) <- st_sfc(lapply(unlist(unlist(st_geometry(nc1), recursive = FALSE), recursive = FALSE), st_multipoint), crs = st_crs(nc1))
mp_nc1 <- st_cast(nc1, "MULTIPOINT")
mtriangs <- ct_triangulate(nc1, a = 0.0005)
plot(st_geometry(mtriangs), col = viridisLite::viridis(nrow(mtriangs)), border = "#00000033")

## -----------------------------------------------------------------------------
plot(nc[4, ]$geometry)
## q, minimum angle
## D, Delaunay criterion is met
plot(ct_triangulate(nc[4, ]$geometry, q = 35, D = TRUE), add = TRUE, col = "transparent")


## -----------------------------------------------------------------------------
data("map_world", package= "sfdct")
library(dplyr)
g <-  map_world %>% dplyr::filter(startsWith(ID, "Indonesia")) %>% ct_triangulate() %>% st_geometry() 
plot(g, col = "aliceblue", main = "")

nc_triangles[1:2, c(1, 5)] %>% st_transform("+proj=laea") %>% ct_triangulate(a = 2e6) %>% plot()

## -----------------------------------------------------------------------------
data("antarctica")
plot(antarctica[0])
a <- ct_triangulate(st_difference(antarctica[1], antarctica[2, ]), a = 5e10)
plot(a[0], col = "firebrick")

## -----------------------------------------------------------------------------
m <- map_world %>% dplyr::filter(startsWith(ID, "Papua New Guinea"))
plotme <- function(x) {plot(st_geometry(x), col = "aliceblue"); x}
m %>% ct_triangulate(a = 0.2, D = TRUE) %>% plotme() 

