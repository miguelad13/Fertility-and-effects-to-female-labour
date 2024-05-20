ls()

rm(list=ls())

ls()

#Modelo por OLS__________________________________________________________________________________________________________________________________________________________
install.packages("stargazer")
library(stargazer)

modelo <- lm(`female_employment` ~ `fertility_rate` + `secondary_education` + `gini` + `WBLIS`, data=Base_de_Datos_1_)

summary(modelo)


#Estadísticas descriptivas de la base de datos_________________________________________________________________________________________________________________________________________
stats <- summary(Base_de_Datos_1_[, c("female_employment", "fertility_rate", "gini", "secondary_education", "WBLIS")])

print(stats)

#Estimación por FE, RE, FD y POLS, y sus respectivos Akaike y Schwartz________________________________________________________________________________________________________________
install.packages("plm")
library(plm)

# Estimar el modelo con Efectos Fijos -> model = "within" indica que será estimador de Efectos Fijos
modelo_fen <- plm(`female_employment` ~ `fertility_rate` + `secondary_education` + `gini` + `WBLIS` - 1, 
                  data = Base_de_Datos_1_,
                  model = "within")  # "within" indica Efectos Fijos
#Summary Efectos Fijos
summary(modelo_fen)
# Obtener el número de observaciones y parámetros estimados
n_obs <- length(residuals(modelo_fen))
n_params <- length(coefficients(modelo_fen))
# Calcular el AIC y BIC manualmente Efectos Fijos
AIC_value_fen <- n_obs * log(sum(residuals(modelo_fen)^2) / n_obs) + 2 * n_params
BIC_value_fen <- n_obs * log(sum(residuals(modelo_fen)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_fen
BIC_value_fen


# Estimar el modelo con Efectos Aleatorios
modelo_re <- plm(`female_employment` ~ `fertility_rate` + `secondary_education` + `gini` + `WBLIS`, 
                 data = Base_de_Datos_1_,
                 model = "random")  # "random" indica Efectos Aleatorios
# Mostrar un resumen del modelo de Efectos Aleatorios
summary(modelo_re)
# Calcular el AIC y BIC manualmente Random Effect
AIC_value_re <- n_obs * log(sum(residuals(modelo_re)^2) / n_obs) + 2 * n_params
BIC_value_re <- n_obs * log(sum(residuals(modelo_re)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_re
BIC_value_re


# Estimar el modelo con Diferencias Primeras (First Difference)
modelo_fd <- plm(`female_employment` ~ `fertility_rate` + `secondary_education` + `gini` + `WBLIS`, 
                 data = Base_de_Datos_1_,
                 model = "fd")  # "fd" indica First Difference
# Mostrar un resumen del modelo de Diferencias Primeras
summary(modelo_fd)
# Calcular el AIC y BIC manualmente First Difference
AIC_value_fd <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + 2 * n_params
BIC_value_fd <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_fd
BIC_value_fd

# Estimar el modelo con Pooled Ordinary Least Squares (POLS)
modelo_pols <- lm(`female_employment` ~ `fertility_rate` + `secondary_education` + `gini` + `WBLIS`, 
                  data = Base_de_Datos_1_)
# Mostrar un resumen del modelo de Pooled OLS
summary(modelo_pols)
# Calcular el AIC y BIC manualmente First Difference
AIC_value_pols <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + 2 * n_params
BIC_value_pols <- n_obs * log(sum(residuals(modelo_fd)^2) / n_obs) + n_params * log(n_obs)
# Mostrar los resultados
AIC_value_pols
BIC_value_pols

# Variance Inflator Factor______________________________________________________________________________________________________________________________________
install.packages("car")
library(car)

# Calcula el VIF para cada modelo
vif_pols <- vif(modelo_pols)
vif_fd <- vif(modelo_fd)
vif_fe <- vif(modelo_fen)
vif_re <- vif(modelo_re)

print(vif_fd)
print(vif_fe)
print(vif_pols)
print(vif_re)

#Prueba de Hausman
hausman_test <- phtest(modelo_fen, modelo_re)

print(hausman_test)



#GRÁFICAS_________________________________________________________________________________________________________________________________________
install.packages("ggplot2")
library(ggplot2)

#Gráfico de dispersión
scatter_plot <- function(data, title) {
  ggplot(data, aes(x = fertility_rate, y = female_employment)) +
    geom_point() +
    labs(x = "Fertilidad", y = "Participación Laboral Femenina", title = title) +
    theme_minimal()
}

#Aquí filtramos los datos para incluir solo las observaciones de cada país y hacer los gráficos de dispersión.
data_mexico <- subset(Base_de_Datos_1_, country_code == "MEX")
data_colombia <- subset(Base_de_Datos_1_, country_code == "COL")
data_chile <- subset(Base_de_Datos_1_, country_code == "CHL")
data_argentina <- subset(Base_de_Datos_1_, country_code == "ARG")
data_brasil <- subset(Base_de_Datos_1_, country_code == "BRA")
data_peru <- subset(Base_de_Datos_1_, country_code == "PER")
data_uruguay <- subset(Base_de_Datos_1_, country_code == "URY")
data_usa <- subset(Base_de_Datos_1_, country_code == "USA")


scatter_plot(data_mexico, "Fertilidad y Participación Laboral Femenina en México")
scatter_plot(data_colombia, "Fertilidad y Participación Laboral Femenina en Colombia")
scatter_plot(data_chile, "Fertilidad y Participación Laboral Femenina en Chile")
scatter_plot(data_argentina, "Fertilidad y Participación Laboral Femenina en Argentina")
scatter_plot(data_brasil, "Fertilidad y Participación Laboral Femenina en Brasil")
scatter_plot(data_peru, "Fertilidad y Participación Laboral Femenina en Perú")
scatter_plot(data_uruguay, "Fertilidad y Participación Laboral Femenina en Uruguay")
scatter_plot(data_usa, "Fertilidad y Participación Laboral Femenina en USA")

# Serie de tiempo
ggplot(data_mexico, aes(x = year, y = female_employment)) +
  geom_line() +
  labs(x = "Años", y = "Participación Laboral Femenina",
       title = "Serie de Tiempo de Participación Laboral de Mujeres en México (2005-2015)") +
  theme_minimal()


#Promedio__________________________
# Filtra los datos para incluir solo las observaciones de USA y para el período de 2005 a 2015
datos_usa <- subset(Base_de_Datos_1_, country_code == "USA" & year >= 2005 & year <= 2015)

# Calcula el promedio de la participación femenina
promedio_participacion <- mean(datos_usa$female_employment, na.rm = TRUE)

# Imprime el resultado
print(paste("El promedio de participación femenina en USA (2005-2015) es:", round(promedio_participacion, 2)))
