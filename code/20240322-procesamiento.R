################################################################################
# Instalo librerias
################################################################################
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("reshape", quietly = TRUE)) {
  install.packages("reshape")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

################################################################################
# Librerias
################################################################################
library(dplyr)
library(ggplot2)
library(reshape) 

################################################################################
# Lectura de datos
################################################################################
# Radiancia corregida
data_pco <- read.csv("../data/007-20240320-LABCETT/medicion_1/L1B_pco/007-20240320-LABCETT-pco.csv", header = TRUE)

################################################################################
# Funciones previas
################################################################################
# funcion radiancia
radiancia_mean <- function(id, data, kind="tg"){
  # Creo los identificadores
  sp <- paste("",id,kind, sep=".")
  # Junto los datos para poder manipularlos
  df_kind <-  melt(select(data,contains(c("wavelength",sp))), id="wavelength")
  # Calculo la media del spectralon
  mean_kind <- aggregate(x= df_kind$value, by = list(df_kind$wavelength), FUN = mean)
  wavelength <- mean_kind$Group.1
  # Calculo la media reflectancia
  rad <- mean_kind
  # Acomodo los nombres
  colnames(rad) <- c("wavelength", "x")
  return(rad)
}

# funcion radiancia
radiancia_rel <- function(id, data, kind="tg"){
  # Creo los identificadores
  sp <- paste("",id,kind, sep=".")
  # Junto los datos para poder manipularlos
  df_kind <-  melt(select(data,contains(c("wavelength",sp))), id="wavelength")
  # Calculo la media del spectralon
  mean_kind <- aggregate(x= df_kind$value, by = list(df_kind$wavelength), FUN = mean)
  sd_kind <- aggregate(x= df_kind$value, by = list(df_kind$wavelength), FUN = sd)
  wavelength <- mean_kind$Group.1
  # Calculo la media reflectancia
  rel <-sd_kind/ mean_kind
  rel$wavelength <- wavelength
  return(rel)
}

# funcion reflectancia
reflectance_mean <- function(id, data){
  # Creo los identificadores
  sp <- paste("",id,"sp", sep=".")
  tg <- paste("",id,"tg", sep=".")
  # Junto los datos para poder manipularlos
  df_sp <-  melt(select(data,contains(c("wavelength",sp))), id="wavelength")
  df_tg <-  melt(select(data,contains(c("wavelength",tg))), id="wavelength")
  # Calculo la media del spectralon
  mean_sp <- aggregate(x= df_sp$value, by = list(df_sp$wavelength), FUN = mean)
  wavelength <- mean_sp$Group.1
  # Calculo la media del blanco
  mean_tg <- aggregate(x= df_tg$value, by = list(df_tg$wavelength), FUN = mean)
  # Calculo la media reflectancia
  ref <- mean_tg/mean_sp
  ref$wavelength <- wavelength
  return(ref)
}

# funcion error relativo
reflectance_rel <- function(id, data){
  # Creo los identificadores
  sp <- paste("",id,"sp", sep=".")
  tg <- paste("",id,"tg", sep=".")
  # Junto los datos para poder manipularlos
  df_sp <-  melt(select(data,contains(c("wavelength",sp))), id="wavelength")
  df_tg <-  melt(select(data,contains(c("wavelength",tg))), id="wavelength")
  
  # Calculo la incerteza relativa
  mean_sp <- aggregate(x= df_sp$value, by = list(df_sp$wavelength), FUN = mean)
  wavelength <- mean_sp$Group.1
  mean_tg <- aggregate(x= df_tg$value, by = list(df_tg$wavelength), FUN = mean)
  std_sp <- aggregate(x= df_sp$value, by = list(df_sp$wavelength), FUN = sd)
  wavelength <- mean_sp$Group.1
  std_tg <- aggregate(x= df_tg$value, by = list(df_tg$wavelength),FUN = sd)
  ref <- mean_tg/mean_sp
  rel <- sqrt((std_tg/mean_tg)**2+(std_sp/mean_sp)**2)/sqrt(10)
  rel$wavelength <- wavelength
  rel$x <- 100*rel$x
  return(rel)
}

################################################################################
# Graficos radiancia
################################################################################
# Obtengo las radiancias promedio
arena0 <-radiancia_mean(0, data_pco, kind="tg")
arena0$superficie <-"arena-0.0g"
arena18 <-radiancia_mean(1, data_pco, kind="tg")
arena18$superficie <-"arena-1.8g"
arena28 <-radiancia_mean(2, data_pco, kind="tg")
arena28$superficie <-"arena-2.8g"

# Lo junto en un dataframe
arena_rad_mean <- rbind(arena0, arena18, arena28)

ggplot(data=arena_rad_mean) + geom_line(aes(x=wavelength, y=x,color=superficie))+
  labs(x = "λ [nm]", y = "L [W/m²/nm/sr]" ) + ggtitle("Radiancia promedio")

# Obtengo las radiancias promedio
arena0 <-radiancia_rel(0, data_pco, kind="tg")
arena0$superficie <-"arena-0.0g"
arena18 <-radiancia_rel(1, data_pco, kind="tg")
arena18$superficie <-"arena-1.8g"
arena28 <-radiancia_rel(2, data_pco, kind="tg")
arena28$superficie <-"arena-2.8g"

# Lo junto en un dataframe
arena_rad_rel <- rbind(arena0, arena18, arena28)

ggplot(data=arena_rad_rel) + geom_line(aes(x=wavelength, y=x,color=superficie))+
  labs(x = "λ [nm]", y = "ΔL/L [%]" ) + ggtitle("Errror relativo radiancia")

################################################################################
# Graficos reflectancia
################################################################################
# Obtengo las reflectancias promedio
arena0 <-reflectance_mean(0, data_pco)
arena0$superficie <-"arena-0.0g"
arena18 <-reflectance_mean(1, data_pco)
arena18$superficie <-"arena-1.8g"
arena28 <-reflectance_mean(2, data_pco)
arena28$superficie <-"arena-2.8g"

# Lo junto en un dataframe
arena_ref_mean <- rbind(arena0, arena18, arena28)

ggplot(data=arena_ref_mean) + geom_line(aes(x=wavelength, y=x,color=superficie))+
  labs(x = "λ [nm]", y = "ρ") + ggtitle("Reflectancia promedio")

# Obtengo las radiancias promedio
arena0 <-reflectance_rel(0, data_pco)
arena0$superficie <-"arena-0.0g"
arena18 <-reflectance_rel(1, data_pco)
arena18$superficie <-"arena-1.8g"
arena28 <-reflectance_rel(2, data_pco)
arena28$superficie <-"arena-2.8g"

# Lo junto en un dataframe
arena_ref_rel <- rbind(arena0, arena18, arena28)

ggplot(data=arena_ref_rel) + geom_line(aes(x=wavelength, y=x,color=superficie))+
  labs(x = "λ [nm]", y = "Δρ/ρ [%]") + ggtitle("Errror relativo reflectancia")
