#################################
# PROCESAMIENTO DE DATOS DE CALIDAD DE AIRE Y METEOROLÓGICOS
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

###############
#  Paquetes   #
###############

library(ggplot2)
library(httr)
library(DataExplorer)

# CONFIGURAR EL DIRECTORIO DE TRABAJO

path_wd <- readClipboard()
                   
setwd(path_wd)

#######################
#  iMPORTACION DE DATOS  #
#######################

# IMPORTAR BASES DE DATO

df1 <- read.table("air-data-campodm.csv", header = T, 
                  stringsAsFactors = F, sep = ",")

df2 <- read.table("data-meteo-campodm.csv", header = T, 
                  stringsAsFactors = F, sep = ",")

# MERGE A LOS DF
df <- merge(df1, df2, by = "date")

# CONVERTIR FECHAS
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")


####################################
#  ANALISIS EXPLORATORIO DE DATOS  #
####################################

# VISUALIZAR LA ESCTRUCTURA DE NUESTRA DATA
plot_str(df)

# RESUMEN DE NUESTROS DATOS
introduce(df)
plot_intro(df)

# VALORES FALTANTES O MISSING
plot_missing(df)
profile_missing(df)

# HISTOGRAMA 
plot_histogram(df)

# QQPLOT
plot_qq(df)

# TIME PLOT
timePlot(df, pollutant = "PM25",
         ref.y = list(h = 40, lty = 5),
         avg.time = "1 day")

# BOXPLOT
ggplot(df, aes(y = PM25)) +
  geom_boxplot()

# SCATTER PLOT
ggplot(df, aes(x=date, y=PM25)) + 
  geom_line() +
  geom_smooth(method = "loess")



#coef(model)
#model <- lm(mpg~disp+hp+wt, data)

library(caret)
library(klaR)

library(caret)

preproc1 <- preProcess(df, method = "range")
norm1 <- predict(preproc1, df)


smp_size <- floor(0.8 * nrow(df))

set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]


# train a naive bayes model
model <- NaiveBayes(Species~., data=data_train)
# make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)
