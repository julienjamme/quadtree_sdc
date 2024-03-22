install.packages("sdcSpatial")


sdcSpatial::protect_quadtree

library(raster)
library(sdcSpatial)

fined <- sdc_raster(enterprises, enterprises$fined)
plot(fined)
fined_qt <- protect_quadtree(fined)
plot(fined_qt)

x <- fined
max_zoom <- 5
# protect_quadtree code
sdc <- x
zoom <- seq_len(floor(log(min(nrow(x$value), ncol(x$value)), 2)))
zoom <- zoom[zoom < max_zoom]
for (fact in 2^zoom) {
  # fact = 4
  z1 <- sdc$value
  z1$sens <- is_sensitive(sdc)
  z1$area <- 1
  z2 <- raster::aggregate(z1[[c("sum", "count", "sens", 
                                "area")]], fact = fact, fun = sum)
  z2$sum <- z2$sum/z2$area
  z2$count <- z2$count/z2$area
  if (sdc$type == "numeric") {
    z2$max = raster::aggregate(sdc$value$max, fact = fact, 
                               fun = max)
    z2$max <- z2$max/z2$area
    z2$max2 = raster::aggregate(sdc$value$max2, fact = fact, 
                                fun = max)
    z2$max2 <- z2$max2/z2$area
  }
  z1 <- z1[[names(z2)]]
  z2 <- raster::mask(z2, z2$sens, maskvalue = 0)
  z2 <- raster::disaggregate(z2, fact = fact)
  z2 <- raster::crop(z2, z1)
  z2 <- raster::cover(z2, z1)
  sdc$value$sum <- z2$sum
  sdc$value$count <- z2$count
  sdc$value$scale <- sdc$value$scale/z2$area
  sdc$value$mean <- mean(sdc)
  if (sdc$type == "numeric") {
    sdc$value$max = z2$max
    sdc$value$max2 = z2$max2
  }
}
sdc


