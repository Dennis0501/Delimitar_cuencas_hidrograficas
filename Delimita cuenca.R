#########################################################
#DELIMITACION DE CUENCAS HIDROGRAFICAS
#27/04/2022
#Dennis Alvarino Cieza Tarrillo
#########################################################

# Carga de paquetes -------------------------------------------------------
library(rgdal)
library(rgeos)
library(raster)
library(whitebox)
library(geosphere)
library(ggplot2)
#install_whitebox() #Ejecutar despues de instalar el paquete una unica vez


# Ruta de trabajo ---------------------------------------------------------
setwd('D:/GITHUB')
dem <- raster('Del. Cuencas/Archivos/dem_cos_wgs.tif')
plot(dem)
point_out <- shapefile('Del. Cuencas/Archivos/point_out.shp')


# Pre-procesamiento del DEM -----------------------------------------------
#Breached
wbt_breach_depressions_least_cost(
  dem='Del. Cuencas/Archivos/dem_cos_wgs.tif', #Ruta del DEM
  output = 'Del. Cuencas/Archivos/dem_corte.tif', #Guardado del dem corregido con recorte
  dist = 5) #Numero de celdas para hacer la correccion

#Fill
wbt_fill_depressions_wang_and_liu(
  dem='Del. Cuencas/Archivos/dem_cos_wgs.tif', #Ruta del DEM
  output = 'Del. Cuencas/Archivos/dem_fill.tif') #Guardado del dem corregido con recorte

#Pixeles rellenados
dem_fill<- raster('Del. Cuencas/Archivos/dem_fill.tif')  
diff <- dem-dem_fill
diff[diff==0] <- NA #Todos los valores iguales a 0 se representan con NA
plot(diff)

# Mapa de sombras ---------------------------------------------------------

wbt_hillshade(
  dem = 'Del. Cuencas/Archivos/dem_fill.tif',
  output = 'Del. Cuencas/Archivos/hillshade.tif',
  azimuth = 270) #Cambiable

dem_hillshade <- raster('Del. Cuencas/Archivos/hillshade.tif')
plot(dem_hillshade,
     col=gray.colors(2), #Pasar a color de grises
     legend=F) #Quitar leyenda


# Mapa de pendientes ------------------------------------------------------
#Mapa de pendientes en grados
slope_grados <- raster::terrain(dem_fill, opt='slope', unit='degrees')
plot(slope_grados)

#Pendientes en porcentaje
slope_porce <- tan(slope_grados*pi/180)*100
slope_porce[slope_porce>100] <- 100 #Igualar los valores mayores a 100
plot(slope_porce)
writeRaster(slope_porce, 'Del. Cuencas/Archivos/slop_porc.tif', overwrite=T)

#Aspect
aspect_grados <- raster::terrain(dem_fill, opt='aspect', units='degrees')
plot(aspect_grados)
writeRaster(slope_porce, 'Del. Cuencas/Archivos/aspect.tif', overwrite=T)


# Delimitacion de cuencas hidrograficas -----------------------------------

#Acumulacion de flujo
wbt_d8_flow_accumulation(input = 'Del. Cuencas/Archivos/dem_fill.tif',
                         output = 'Del. Cuencas/Archivos/flow_accum.tif')
wbt_d8_pointer(dem = 'Del. Cuencas/Archivos/dem_fill.tif',
               output = 'Del. Cuencas/Archivos/dir_pointer.tif')

flow_acucum <- raster('Del. Cuencas/Archivos/flow_accum.tif')
plot(flow_acucum)

#Visualizar punto de salida
plot(point_out, ad=T, pch=20, col= "red")

#Extraer red de drenaje
wbt_extract_streams(flow_accum = 'Del. Cuencas/Archivos/flow_accum.tif',
                    output = 'Del. Cuencas/Archivos/red_drenaje.tif',
                    threshold = 2000)
rio_all <- raster('Del. Cuencas/Archivos/red_drenaje.tif')
plot(rio_all, col="blue")
plot(point_out, ad=T, pch=20, col= "red")

#Sanap
wbt_jenson_snap_pour_points(pour_pts = 'Del. Cuencas/Archivos/point_out.shp',
                            streams ='Del. Cuencas/Archivos/red_drenaje.tif',
                            output ='Del. Cuencas/Archivos/point_out_snap.shp',
                            snap_dist = 0.001) #Umbral de ajuste en grados

#Convertir de tif a shapefile
wbt_raster_streams_to_vector(streams = 'Del. Cuencas/Archivos/red_drenaje.tif',
                             d8_pntr = 'Del. Cuencas/Archivos/dir_pointer.tif',
                             output = 'Del. Cuencas/Archivos/red_rio_all.shp')
red_rio_all <- shapefile('Del. Cuencas/Archivos/red_rio_all.shp')
plot(red_rio_all)


#Delimitación de cuenca
wbt_watershed(d8_pntr = 'Del. Cuencas/Archivos/dir_pointer.tif',
              pour_pts = 'Del. Cuencas/Archivos/point_out_snap.shp',
              output = 'Del. Cuencas/Archivos/cuenca.tif')

cuenca_del <- raster('Del. Cuencas/Archivos/cuenca.tif')
plot(cuenca_del)

#Convertir de raster a shapefile
cuenca_del_shp <- rasterToPolygons(cuenca_del, dissolve = T)
plot(cuenca_del_shp, border="red")
shapefile(cuenca_del_shp, 'Del. Cuencas/Archivos/cuencashp.shp', overwrite=T)


# Parametros geomorfologicos ----------------------------------------------

red_rio <- raster::intersect(red_rio_all, cuenca_del_shp)
plot(red_rio)
shapefile(red_rio, 'Del. Cuencas/Archivos/rio_cuenca.shp', overwrite=T)

#Area
(cuenca_del_shp$area_km2 <- area(cuenca_del_shp)/10**6)

#Perimetro
(cuenca_del_shp$per_km <- geosphere::perimeter(cuenca_del_shp)/10**3)

#Longitud
cuenca_utm <- spTransform(cuenca_del_shp, CRS('EPSG:32718'))
cuenca_coor <- raster::geom(cuenca_utm)[,5:6] #Seleccionando las columnas que contienen coordenadas
dist_cuenca <- data.frame(as.matrix(dist(cuenca_coor))) #Convertir a dataframe las distancias
#View(dist_cuenca)
(cuenca_del_shp$long_km <- max(dist_cuenca)/10**3)

#Pendiente promedio
slop_mean <- raster('Del. Cuencas/Archivos/slop_porc.tif')
plot(slop_mean)
cuenca_slop <- mask(crop(slop_mean, cuenca_del_shp),cuenca_del_shp)
plot(cuenca_slop)
writeRaster(cuenca_slop, 'Del. Cuencas/Archivos/cuenca_slop.tif', overwrite=T)
cuenca_del_shp$mean <- mean(values(cuenca_slop), na.rm=T)

#Factor de compacidad kc
cuenca_del_shp$kc = 0.28*cuenca_del_shp$per_km/cuenca_del_shp$area_km2**0.5

#Factor de forma
cuenca_del_shp$kf = cuenca_del_shp$area_km2/cuenca_del_shp$long_km**2

#Longitud de red
spplot(red_rio, "FID")
red_rio$long_km <- geosphere::lengthLine(red_rio)/10**3
red_rio$long_tot_km <- sum(red_rio$long_km)
cuenca_del_shp$long_tot_km <- sum(red_rio$long_km)

#Densidad de drenaje
cuenca_del_shp$densi_dren <- (red_rio$long_tot_km/cuenca_del_shp$area_km2)[1]

#Longitud de red principal
rio_prin <- subset(red_rio, FID %in% c(27.26,20)) #Tramos del rio principal
spplot(rio_prin, 'FID')
cuenca_del_shp$rio_principal_km <- sum(rio_prin$long_km)

#Desnivel altitudinal
cuenca_dem <- crop(mask(dem, cuenca_del_shp),cuenca_del_shp)
plot(cuenca_dem)
writeRaster(cuenca_dem, 'Del. Cuencas/Archivos/cuenca_dem.tif', 'GTiff', overwrite=T)
cuenca_del_shp$alt_min <-min(values(cuenca_dem), na.rm = T)
cuenca_del_shp$alt_max <-max(values(cuenca_dem), na.rm = T)
cuenca_del_shp$alt_dif <- cuenca_del_shp$alt_max - cuenca_del_shp$alt_min

#Tiempo de concentracion
cuenca_del_shp$tc <- 0.97*((cuenca_del_shp$rio_principal_km**3)/(cuenca_del_shp$alt_dif*3.28084))**0.385

#Exportar tabla
par_cuenca <- cuenca_del_shp@data
par_cuenca <- round(par_cuenca,2)
write.csv(par_cuenca, 'Del. Cuencas/Archivos/par_cuenca.csv', row.names = T)

#Perfil longitudinal de la red principal
red_rio <- shapefile('Del. Cuencas/Archivos/rio_cuenca.shp')
dem_cuenca <- raster('Del. Cuencas/Archivos/cuenca_dem.tif')
red_rio_prin <- subset(red_rio, FID %in% c(20,26,27))
plot(dem_cuenca)
plot(red_rio_prin, ad=T)
dem_rio <- mask(dem_cuenca, red_rio_prin)
plot(dem_rio)
len_rio <- sum(geosphere::lengthLine(red_rio_prin))/1000
num_pix <- sum(!is.na(values(dem_rio)))
prof_log_rio <- data.frame(len = seq(0, len_rio, length.out = num_pix),
                           elv = na.omit(values(dem_rio)))
ggplot(prof_log_rio, aes(x=len, y=elv))+
  geom_line(col='blue')+
  labs(x='Longitud (km)', y= 'Elevación (msnm)')+
  theme_bw()

#Curva hipsometrica
dem_cuenca <- raster('Del. Cuencas/Archivos/cuenca_dem.tif')
dem_fill <- dem_cuenca
dem_fill[dem_fill<3000] <- NA #Reducimos cantidad de pixeles
area_px <- sum(!is.na(values(dem_cuenca)))
alt_min <- min((values(dem_cuenca)), na.rm=T)
alt_max <- max((values(dem_cuenca)), na.rm=T)
alt_diff <- alt_max-alt_min
area_hps <- list(data.frame(alt_real=alt_min, area_real=area_px))
nband <- 15

for (i in 1:nband) {
  dem_sel <- dem_cuenca
  alt_f <- alt_min+(alt_diff/nband*i)
  dem_sel[dem_sel<alt_f] <- NA
  area_px_real <- sum(!is.na(values(dem_sel)))
  area_hps[[i+1]] <- data.frame(alt_real=alt_f, area_real=area_px_real)
}

area_hps_df <- do.call(rbind,areas_hps)
area_hps_df$area_tot <- area_px
area_hps_df$area_rel_pc <- area_hps_df$area_real/area_hps_df$area_tot*100

par(mfrow=c(1,2))
plot(dem_cuenca)
plot(x=area_hps_df$area_rel_pc, y=area_hps_df$alt_rel, type="l", col="blue",
     xlab='Area (%)', ylab='Altitud (msnm)')
grid()
par(mfrow=c(1,1))

#Slope vs Elevaion
slop <- raster('Del. Cuencas/Archivos/slop_porc.tif')
slop_elev <- data.frame(elev=values(dem), slp=values(slop))
slop_elev <- na.omit(slop_elev)
ggplot(slop_elev, aes(x=slp, y=elev))+
  geom_density_2d_filled()+
  labs(x='Pendiente en (%)', y= "Elevacion")+
  theme_bw()

#########################################################
#DELIMITACION DE VARIAS CUENCAS HIDROGRAFICAS
#########################################################

#Creando puntos
points_out <- data.frame(lon=c(-75.99,-75.9,-75.81), lat=c(-13.28,-13.24,-13.25))
points <- SpatialPoints(points_out, proj4string = CRS('EPSG:4326'))
shapefile(points, 'Del. Cuencas/Archivos/points.shp', overwrite=T)

wbt_jenson_snap_pour_points(pour_pts = 'Del. Cuencas/Archivos/points.shp',
                            streams = 'Del. Cuencas/Archivos/red_drenaje.tif',
                            output = 'Del. Cuencas/Archivos/ponits_out_snap.shp',
                            snap_dist = 0.005)

wbt_watershed(d8_pntr = 'Del. Cuencas/Archivos/dir_pointer.tif',
              pour_pts = 'Del. Cuencas/Archivos/ponits_out_snap.shp',
              output = 'Del. Cuencas/Archivos/cuencas_delim.tif')

cuencas_del <- raster('Del. Cuencas/Archivos/cuencas_delim.tif')
plot(cuencas_del)

#Convertir de tif a poligono
cuencas_del_pol <- rasterToPolygons(cuencas_del, dissolve = T)
plot(cuencas_del_pol, border=rainbow(3))
plot(points, ad=T, pch=20, col='red')
shapefile(cuencas_del_pol,'Del. Cuencas/Archivos/Cuencas_delim.shp', overwrite=T)

#DEM vs Slop
cuencas <- shapefile('Del. Cuencas/Archivos/Cuencas_delim.shp')
cuencas_dem <- raster::extract(dem, cuencas)
cuencas_dem_ls <- setNames(cuencas_dem, c('cuenca_1','cuenca_2','cuenca_3'))
cuenca_1 <- data.frame(cuenca='cuenca_1', val=cuencas_dem_ls$cuenca_1)
cuenca_2 <- data.frame(cuenca='cuenca_2', val=cuencas_dem_ls$cuenca_2)
cuenca_3 <- data.frame(cuenca='cuenca_3', val=cuencas_dem_ls$cuenca_3)

cuencas_df <- rbind(cuenca_1, cuenca_2, cuenca_3)
ggplot(cuencas_df, aes(x=val, fill=cuenca))+
  geom_density(alpha=0.5)+
  labs(x='Elevación (msnm)')+
  theme_bw()






