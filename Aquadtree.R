install.packages("AQuadtree")

library(AQuadtree)
library(sf)
library(dplyr)

data("BarcelonaPop", "BarcelonaCensusTracts")
aquadtree.Barcelona<-AQuadtree(BarcelonaPop, layers = 3)
plot(aquadtree.Barcelona)


data("BarcelonaPop")
BarcelonaPop.INSPIRE_GRID<-createGrid(BarcelonaPop, dim = 1000)
plot(BarcelonaPop.INSPIRE_GRID)

str(BarcelonaPop)
str(BarcelonaPop.INSPIRE_GRID, max.level = 2)

sf_barcelona_pop <- st_as_sf(BarcelonaPop)
str(sf_barcelona_pop)
plot(sf_barcelona_pop |> st_geometry())

sf_barcelona_pop_grid <- st_as_sf(createGrid(BarcelonaPop, dim = 250))
str(sf_barcelona_pop_grid)
plot(sf_barcelona_pop_grid |> st_geometry())
sf_barcelona_pop_grid_1km <- st_as_sf(createGrid(BarcelonaPop, dim = 8000))
plot(sf_barcelona_pop_grid_1km |> st_geometry(), col = NA, border = "red", add = TRUE)

aquadtree.Barcelona<-createAQuadtree(BarcelonaPop, dim = 8000, layers = 6, threshold = 10)
str(aquadtree.Barcelona, max.level = 2)
str(aquadtree.Barcelona@data, max.level = 2)
plot(st_as_sf(aquadtree.Barcelona) |> st_geometry(), border = "green", add = TRUE)

plot(aquadtree.Barcelona)

aquadtree_barcelona <- st_as_sf(aquadtree.Barcelona)
st_crs(aquadtree_barcelona)
plot(aquadtree_barcelona["total"], breaks = c(0,1,50,100,1000,5000))


aQuadtree.Charleston<-createAQuadtree(CharlestonPop, threshold=10,
                                      colnames="sex", thresholdField=c("sex.male", "sex.female"))