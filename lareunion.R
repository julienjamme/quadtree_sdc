install.packages("archive")
install.packages("mapview")

url = "https://www.insee.fr/fr/statistiques/fichier/7655503/Filosofi2019_carreaux_nivNaturel_gpkg.zip"

temp <- tempfile()
temp2 <- "data"

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
archive::archive_extract(file.path(temp2,"Filosofi2019_carreaux_nivNaturel_gpkg.7z"),dir = "data")

url = "https://www.insee.fr/fr/statistiques/fichier/7655475/Filosofi2019_carreaux_200m_gpkg.zip"

temp <- tempfile()
temp2 <- "data"

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
archive::archive_extract(file.path(temp2,"Filosofi2019_carreaux_200m_gpkg.7z"),dir = "data")

url = "https://www.insee.fr/fr/statistiques/fichier/7655464/Filosofi2019_carreaux_1km_gpkg.zip"

temp <- tempfile()
temp2 <- "data"

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
archive::archive_extract(file.path(temp2,"Filosofi2019_carreaux_1km_gpkg.7z"),dir = "data")


library(sf)
library(dplyr)
library(mapview)

source("functions.R")

sf_metro_nat <- st_read(file.path("data", "carreaux_nivNaturel_met.gpkg"))
nrow(sf_metro_nat)
sf_metro_nat |> st_drop_geometry() |> count(tmaille)

sf_reun_nat <- st_read(file.path("data", "carreaux_nivNaturel_reun.gpkg"))
sf_reun_200 <- st_read(file.path("data", "carreaux_200m_reun.gpkg"))
sf_reun_1000 <- st_read(file.path("data", "carreaux_1km_reun.gpkg"))

str(sf_reun_nat)
sf_reun_nat |> st_drop_geometry() |> count(tmaille)

sf_reun_nat |> st_drop_geometry() |> 
  group_by(tmaille) |> 
  summarise(n = n(), hous = sum(men), hous_mean = mean(men), .groups = 'drop') |> 
  mutate(across(2:3, ~./sum(.)*100, .names = "{.col}_pc")) |>
  bind_rows(
    sf_reun_nat |> st_drop_geometry() |> 
      summarise(n = n(), hous = sum(men), hous_mean = mean(men), .groups = 'drop') |> 
      mutate(across(1:2, ~100, .names = "{.col}_pc")) |>
      mutate(tmaille = 9999)
  ) |>
  knitr::kable(format = "latex", digits = 0, row.names = FALSE, caption = 'po', booktabs = TRUE)

sf_metro_nat |> st_drop_geometry() |> 
  group_by(tmaille) |> 
  summarise(n = n(), hous = sum(men), hous_mean = mean(men), .groups = 'drop') |> 
  mutate(across(2:3, ~./sum(.)*100, .names = "{.col}_pc")) |>
  bind_rows(
    sf_metro_nat |> st_drop_geometry() |> 
      summarise(n = n(), hous = sum(men), hous_mean = mean(men), .groups = 'drop') |> 
      mutate(across(1:2, ~100, .names = "{.col}_pc")) |>
      mutate(tmaille = 9999)
  ) |>
  knitr::kable(format = "latex", digits = 0, row.names = FALSE, caption = 'po', booktabs = TRUE)


sf_reun_nat |> filter(tmaille == 200)

plot(sf_reun_nat |> st_geometry())

mapview(sf_reun_nat, 
        alpha.region = 0.6,
        zcol = "men",
        color = "white",
        at = c(0, 10, 100, 1000, 10000, 100000), 
        legend = TRUE,
        layer.name = "Households",
        label = "men"
) + mapview(sf_reun_1000, 
        alpha.region = 0.3,
        zcol = "men",
        color = "white",
        at = c(0, 10, 100, 1000, 10000, 100000), 
        legend = TRUE,
        layer.name = "Households 1km",
        label = "idcar_1km"
)

c("CRS2975RES16000mN7664000E320000","CRS2975RES1000mN7668000E335000", "CRS2975RES1000mN7671000E328000")




mapview(sf_reun_200, 
        alpha.region = 0.6,
        zcol = "men",
        color = NA,
        at = c(0, 10, 100, 1000, 10000, 100000), 
        legend = TRUE,
        layer.name = "Households"
)


car_nat <- sf_reun_nat |> filter(idcar_nat == "CRS2975RES16000mN7664000E320000")

sf_car_nat_1000 <- st_intersection(sf_reun_1000, car_nat)

coords_poly_8000 <- data.frame(
  Plot = sort(rep(LETTERS[1:4],4)),
  Easting = c(320000, 320000+8000, 320000+8000, 320000, 
              320000+8000, 320000+16000, 320000+16000, 320000+8000, 
              320000+8000, 320000+16000, 320000+16000, 320000+8000, 
              320000, 320000+8000, 320000+8000, 320000
  ),
  Northing = c(7664000, 7664000, 7664000+8000, 7664000+8000,
               7664000, 7664000, 7664000+8000, 7664000+8000,
               7664000+8000, 7664000+8000, 7664000+16000, 7664000+16000,
               7664000+8000, 7664000+8000, 7664000+16000, 7664000+16000
  )
)

polygon_8000 <- coords_poly_8000 %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 2975) %>%
  group_by(Plot) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(polygon_8000)


mapview(sf_reun_nat, 
        alpha.region = 0.6,
        zcol = "men",
        color = "white",
        at = c(0, 10, 100, 1000, 10000, 100000), 
        legend = TRUE,
        layer.name = "Households",
        label = "men"
) + mapview(car_nat, 
            alpha.region = 0.1,
            zcol = NULL,
            color = "red",
            lwd = 3,
            legend = FALSE
)



mapview(sf_car_nat_1000, 
        alpha.region = 0.7,
        zcol = "men",
        color = NA,
        at = c(0, 4, 10, 5000), 
        legend = TRUE,
        layer.name = "Households",
        label = "idcar_1km"
) + mapview(polygon_8000, 
          alpha.region = 0.1,
          lwd = 3,
          col.regions = NA,
          zcol = NULL,
          color = "red",
          legend = FALSE
  )

# Compute Moran's I ----------------------------

calculer_I_moran(sf_data = sf_reun_nat, vars = "men", dist = "queen")
calculer_I_moran(sf_reun_1000, vars = "men", dist = "queen")
calculer_I_moran(sf_data = sf_reun_nat, vars = "men", dist = "euclid", lim = 16000)
calculer_I_moran(sf_reun_1000, vars = "men", dist = "euclid", lim = 16000)


coords <- st_coordinates(st_centroid(sf_reun_nat))
distance <- rdist(coords[,1:2], coords[,1:2])
diag(distance) <- 0
distance[distance >= 16000] <- 0
dist <- 1.e12 %/% (distance*distance)
dist[dist >= 1.e15] <- 0
lw <- mat2listw(dist, row.names = NULL, style = "W", zero.policy = TRUE)

spdep::moran.test(
  sf_reun_nat[["men"]],
  lw,
  zero.policy=TRUE,
  randomisation = FALSE
)





