#################################
# TRABAJO APLICATIVO - CLASE 2 - N1
# CURSO: PROGRAMACION CON R
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
#################################

#---------------------------------------------------------
# Para limpiar la consola:
# TeclaS :  Ctrl + L

# Para limpiar el workspace:
rm(list = ls())

###############
#  Paquetes   #
###############

library(ggplot2)

# FIJAR EL DIRECTORIO DE TRABAJO

path_wd <- readClipboard()
setwd(path_wd) 

# PREGUNTA 1
df <- read.table("DailyDelhiClimateTrain.csv", sep = ",", header = T,
                 stringsAsFactors = F)

# PREGUNTA 2
str(df)

summary(df)

hist(log(df$wind_speed))

qqnorm(df$wind_speed)

boxplot(df[,-c(1,5)])


# PREGUNTA 3

df$meantemp_f <- (df$meantemp*9/5) + 32

# PREGUNTA 4

df[df$meantemp == min(df$meantemp), ]

df[df$meantemp == max(df$meantemp), ]

# PREGUNTA 5 

df$date <- as.Date(df$date, format = "%Y-%m-%d")
df$date_m <- format(df$date, format = "%B-%Y")

str(df)

ggplot(df, aes(meantemp, humidity)) +
  geom_point()+
  facet_wrap(~date_m)

# PREGUNTA 6

ggplot(df, aes(date, humidity)) +
  geom_point()+
  facet_wrap(~date_m, nrow = 4, ncol = 3)


levels(as.factor(df$date_m))

df2 <- df[df$date_m == "Enero-2013",]
df3 <- df[df$date_m == "Febrero-2013",]


g1 <- ggplot(df2, aes(date, humidity)) +
  geom_point()+
  geom_line()

g2 <- ggplot(df3, aes(date, humidity)) +
  geom_point()+
  geom_line()

library(ggpubr)
figure <- ggarrange(g1, g2,
                    labels = c("01-2013", "02-2013"),
                    ncol = 2)
figure

# PREGUNTA 7

ggplot(df, aes(y = humidity)) +
  geom_boxplot() +
  facet_wrap(~date_m)

df$date_m <- as.factor(df$date_m)

levels(df$date_m) <- c("Enero", "Febrero", "Marzo", "Abril",
                       "Mayo", "Junio", "Julio", "Agosto",
                       "Setiembre", "Octubre", "Novimebre", "Diciembre")

ggplot(df, aes(y = humidity, color = date_m)) +
  geom_boxplot() +
  facet_wrap(~date_m)

# PREGUNTA 8

library(hydroTSM)

df$season <- time2season(df$date, out.fmt = "months")

f1 <- function(x){
  if(x[length(x)] == "DJF"){
    return("VERANO")
  }else if(x[length(x)] == "MAM"){
    return("OTONHO")
  }else if(x[length(x)] == "JJA"){
    return("INVERNO")
  } else(return("PRIMAVERA"))
  }

df[,6] <- NA

for(i in 1:nrow(df)){
  
  if(df[i,7] %in% c("Diciembre","Enero", "Febrero")){
    df[i,6] <- "VERANO"
    
  } else if(df[i,7] %in% c("Marzo","Abril", "Mayo")){
    df[i,6] <- "OTONHO"
    
  } else if(df[i,7] %in% c("Junio","Julio", "Agosto")){
    df[i,6] <- "INVIERNO"
  
  }else(df[i,6] <- "PRIMAVERA")
  
}







