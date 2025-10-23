# Instalar paquetes necesarios (si no los tienes)
install.packages(c("readxl", "YieldCurve", "vars", "tidyverse", "ggplot2"))

#####
# Cargar librerías
install.packages("YieldCurve")
install.packages("DNS")  # si quieres Dynamic Nelson-Siegel
library(YieldCurve)
library(DNS)
install.packages("KFAS")
library(KFAS)
library(readxl)
library(YieldCurve)
library(vars)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

system("git --version")
system('git config --global user.name "abi0498"')
system('git config --global user.email "leonel.espinoza@unah.hn"')
system("git config --global --list")
system("git init")
system("git remote add origin https://github.com/abi0498/seminario-de-investigacion")

getwd()
list.files()

mi_carpeta <- "C:/Users/abies/OneDrive/Escritorio"
setwd("C:/Users/abies/OneDrive/Escritorio/GitHub/seminario-de-investigacion")
# Para archivos Excel
archivos_xlsx <- list.files(mi_carpeta, pattern = "\\.xlsx$", full.names = TRUE)
data <- read_excel("ADVANCED RESULT_2025-09-15_21_32.xlsx", sheet = "Exported data")
dataac<-read_excel("ADVANCED RESULT_2025-10-20_03_22.xlsx", sheet = "Exported data")
  



# Verificar estructura
head(yield_data)

colnames(data) <- c("Period", "M1_avg", "M1_ult", "M3_avg", "M3_ult",
                          "M6_avg", "M6_ult", "Y2_avg", "Y2_ult", "Y5_avg",
                          "Y5_ult", "Y7_avg", "Y7_ult", "Y10_avg", "Y10_ult")

datas<-data[-c(1:4,196:226), ]
datas1<-datas[,-c(2,4,6,8,10,12,14)]  

cols <- c("M1_ult", "M3_ult", "M6_ult", "Y2_ult", "Y5_ult", "Y7_ult", "Y10_ult")

datas1[cols] <- lapply(datas1[cols], as.numeric)

str(datas1)
datas1$Period

tau <- c(1/12, 3/12, 6/12, 2, 5, 7, 10)  # 1m, 3m, 6m, 2a, 5a, 7a, 10a
landa<-0.0609
f1<- matrix(1, 191, 1)
f1 <- function(tau, lambda) {
  (1 - exp(-lambda * tau)) / (lambda * tau)
}
f2 <- function(tau, lambda) {
  f1(tau, lambda) - exp(-lambda * tau)
}

f3 <- function(x) {
  # Ejemplo: función cuadrática
  return((1-exp(-landa*x))/(landa*x))
}
f4<-function(x) {
  # Ejemplo: función cuadrática
  return(((1-exp(-landa*x))/(landa*x)) -exp(-landa*x) )
}




library(lubridate)

datas1$Period_date <- ym(datas1$Period)
str(datas1)
yields_matrix <- as.matrix(datas1[ , -c(1,9)]) 
str(yields_matrix)
landa<-0.0601
L <- cbind(
  1,                          # Columna 1: todos 1
f3(tau),        # Columna 2: función1 evaluada
 f4(tau)         # Columna 3: función2 evaluada
)
str(yields_matrix)
betas<-solve(t(L)%*%L)%*%t(L)%*%t(yields_matrix)
betasts <- t(betas)

print(colSums(is.na(yields_matrix)))



y_real <- yields_matrix[c(1:5), ] 
y_estp<-Y_est[c(1:5), ]

# Calcular todas las curvas estimadas
Y_est <- t(L %*% betas)   # quedará 191 x 7 (igual que Y_obs)

###################estimar landa

# Construir la matriz de factores L para un lambda dado
make_L <- function(tau, lamda) {
  cbind(
    rep(1, length(tau)),
    f1(tau, lamda),
    f2(tau, lamda)
  )
}

# SSE perfilada: dado un lambda, estima betas_t y calcula error total
sse_lambda <- function(lamda, yields_matrix, tau) 
  {
  L <- make_L(tau, lamda)
  # Proyección OLS: betas_hat_t(lambda)
  betas_hat <- t(solve(t(L) %*% L) %*% t(L) %*% t(yields_matrix))
  # Rendimientos ajustados
  fitted <- betas_hat %*% t(L)
  # Error cuadrático total
  sum((yields_matrix - fitted)^2)
}

lambda_grid <- seq(0.01, 2, length.out = 200)
sse_vals <- sapply(lambda_grid, sse_lambda, yields_matrix = yields_matrix, tau = tau)

##este sera el landa que minimiza el error
lambda_hat_grid <- lambda_grid[which.min(sse_vals)]

plot(lambda_grid, sse_vals, type = "l", lwd = 2,
     xlab = expression(lambda), ylab = "SSE",
     main = "Error de ajuste para distintos valores de lambda")

#calcualmos ahora
landa<-lambda_hat_grid

f3 <- function(x) {
  
  return((1-exp(-landa*x))/(landa*x))
}
f4<-function(x) {
  
  return(((1-exp(-landa*x))/(landa*x)) -exp(-landa*x) )
}

L <- cbind(
  1,                          # Columna 1: todos 1
  f3(tau),        # Columna 2: función1 evaluada
  f4(tau)         # Columna 3: función2 evaluada
)

betas<-solve(t(L)%*%L)%*%t(L)%*%t(yields_matrix)
betasts <- t(betas)

Y_est <- t(L %*% betas)


##############################################grafica

datos_largos <- datas1 %>%
  pivot_longer(
    cols = c(M1_ult, M3_ult, M6_ult, Y2_ult, Y5_ult, Y7_ult, Y10_ult),
    names_to = "Plazo",
    values_to = "Tasa"
  )

ggplot(datos_largos, aes(x = Period_date, y = Tasa, color = Plazo)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Curvas de Rendimiento (1996-2011)",
    subtitle = ,
    x = "Fecha",
    y = "Tasa de Interés (%)",
    color = "Plazo"
  ) +
  scale_color_manual(
    values = c(
      "M1_ult" = "#E41A1C",    # Rojo - 1 Mes
      "M3_ult" = "#377EB8",    # Azul - 3 Meses
      "M6_ult" = "#4DAF4A",    # Verde - 6 Meses
      "Y2_ult" = "#984EA3",    # Púrpura - 2 Años
      "Y5_ult" = "#FF7F00",    # Naranja - 5 Años
      "Y7_ult" = "#FFFF33",    # Amarillo - 7 Años
      "Y10_ult" = "#A65628"    # Marrón - 10 Años
    ),
    labels = c(
      "M1_ult" = "1 Mes",
      "M3_ult" = "3 Meses", 
      "M6_ult" = "6 Meses",
      "Y2_ult" = "2 Años",
      "Y5_ult" = "5 Años",
      "Y7_ult" = "7 Años",
      "Y10_ult" = "10 Años"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA))


######## 1. Curvas de rendimiento estiamdas
df_y_est <- as.data.frame(Y_est)

# 2. Asignar nombres a las columnas (plazos)
colnames(df_y_est) <- c("M1_est", "M3_est", "M6_est", "Y2_est", "Y5_est", "Y7_est", "Y10_est")

# 3. Agregar la columna de fechas desde tu tibble original
df_y_est$Period_date <- datas1$Period_date

datos_largosest <- df_y_est %>%
  pivot_longer(
    cols = c(M1_est, M3_est, M6_est, Y2_est, Y5_est, Y7_est, Y10_est),
    names_to = "Plazo",
    values_to = "Tasa"
  )

ggplot(datos_largosest, aes(x = Period_date, y = Tasa, color = Plazo)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Curvas de Rendimiento suavizada (1996-2011)",
    subtitle = ,
    x = "Fecha",
    y = "Tasa de Interés (%)",
    color = "Plazo"
  ) +
  scale_color_manual(
    values = c(
      "M1_est" = "#E41A1C",    # Rojo - 1 Mes
      "M3_est" = "#377EB8",    # Azul - 3 Meses
      "M6_est" = "#4DAF4A",    # Verde - 6 Meses
      "Y2_est" = "#984EA3",    # Púrpura - 2 Años
      "Y5_est" = "#FF7F00",    # Naranja - 5 Años
      "Y7_est" = "#FFFF33",    # Amarillo - 7 Años
      "Y10_est" = "#A65628"    # Marrón - 10 Años
    ),
    labels = c(
      "M1_est" = "1 Mes",
      "M3_est" = "3 Meses", 
      "M6_est" = "6 Meses",
      "Y2_est" = "2 Años",
      "Y5_est" = "5 Años",
      "Y7_est" = "7 Años",
      "Y10_est" = "10 Años"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA))

######errores

errores <- data.frame(
  Period_date = datas1$Period_date,
  
  # Errores para cada plazo (Observado - Estimado)
  error_M1 = abs(datas1$M1_ult - df_y_est$M1_est),
  error_M3 = abs(datas1$M3_ult - df_y_est$M3_est),
  error_M6 = abs(datas1$M6_ult - df_y_est$M6_est),
  error_Y2 = abs(datas1$Y2_ult - df_y_est$Y2_est),
  error_Y5 = abs(datas1$Y5_ult - df_y_est$Y5_est),
  error_Y7 = abs(datas1$Y7_ult - df_y_est$Y7_est),
  error_Y10 = abs(datas1$Y10_ult - df_y_est$Y10_est)
)

datos_errores <- errores %>%
  pivot_longer(
    cols = c(error_M1, error_M3, error_M6, error_Y2, error_Y5, error_Y7, error_Y10),
    names_to = "Plazo",
    values_to = "Tasa"
  )

ggplot(datos_errores, aes(x = Period_date, y = Tasa, color = Plazo)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "errores",
    subtitle = "Tasas de Letras del Tesoro y Bonos Gubernamentales (Valores Ultimo)",
    x = "Fecha",
    y = "Tasa de Interés (%)",
    color = "Plazo"
  ) +
  scale_color_manual(
    values = c(
      "error_M1" = "#E41A1C",    # Rojo - 1 Mes
      "error_M3" = "#377EB8",    # Azul - 3 Meses
      "error_M6" = "#4DAF4A",    # Verde - 6 Meses
      "error_Y2" = "#984EA3",    # Púrpura - 2 Años
      "error_Y5" = "#FF7F00",    # Naranja - 5 Años
      "error_Y7" = "#FFFF33",    # Amarillo - 7 Años
      "error_Y10" = "#A65628"    # Marrón - 10 Años
    ),
    labels = c(
      "M1_est" = "1 Mes",
      "M3_est" = "3 Meses", 
      "M6_est" = "6 Meses",
      "Y2_est" = "2 Años",
      "Y5_est" = "5 Años",
      "Y7_est" = "7 Años",
      "Y10_est" = "10 Años"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA))









########################################################## predicciones modelo var
#################################################################################
#esta son las predicciones que estoy trabajando
#aqui utilizaremos varios enfoques
#probar con transformaciones para los betas, estacionaria, cointegrada y los betas que estimamos 
install.packages(c("vars","MASS"))
library(vars)
library(MASS)
library(tseries)
library(urca)
library(forecast)
library(vars)

frequency_val <- 12   # 
h <- 4 




beta0 <- betasts[, 1]  # Primera columna
beta1 <- betasts[, 2]  # Segunda columna  
beta2 <- betasts[, 3]  # Tercera columna

###### analisis estacionario de mis betas estimados
## primero para beta0

adf_beta0 <- adf.test(na.omit(beta0))
print(adf_beta0)

## segundo para beta1

adf_beta1 <- adf.test(na.omit(beta1))
print(adf_beta1)

## tercera para beta2

adf_beta2 <- adf.test(na.omit(beta2))
print(adf_beta2)

##otra prueba 
kpss_beta0 <- kpss.test(na.omit(beta0), null = "Level")
print(kpss_beta0)

kpss_beta1 <- kpss.test(na.omit(beta1), null = "Level")
print(kpss_beta1)

kpss_beta2 <- kpss.test(na.omit(beta2), null = "Level")
print(kpss_beta2)

#forzar la estacionaridad
beta0_diff <- diff(beta0)
beta1_diff <- diff(beta1)  
beta2_diff <- diff(beta2)

# Crear matriz VAR con series diferenciadas
longitud_minima <- min(length(beta0_diff), length(beta1_diff), length(beta2_diff))

matriz_var_final <- cbind(
  beta0_diff[1:longitud_minima],
  beta1_diff[1:longitud_minima],
  beta2_diff[1:longitud_minima]
)

colnames(matriz_var_final) <- c("beta0_diff", "beta1_diff", "beta2_diff")

# Verificar estacionariedad después de diferenciar




adf.test(beta0_diff)  
adf.test(beta1_diff)    
adf.test(beta2_diff)  

kpss.test(beta0_diff)
kpss.test(beta1_diff)
kpss.test(beta2_diff)




################### con mis betas sin aplicarla ninguna trasnfomacion
betas_ts_mat <- t(betas)
betas_ts <- ts(betas_ts_mat, frequency = frequency_val)

sel <- VARselect(betas_ts, lag.max = 12, type = "const")

sel$selection
p_choice <- as.integer(sel$selection["AIC(n)"])

var_model <- VAR(betas_ts, p = p_choice, type = "const")

var_pred <- predict(var_model, n.ahead = 80, ci = 0.95)
var_pred$fcst

beta_point <- sapply(var_pred$fcst, function(x) x[,"fcst"])
t(beta_point)
ypt <- t(L %*% t(beta_point))



########## ahora con betas estacionarios

frequency_val <- 12   # 
h <- 4 


beta0 <- betasts[, 1]  # Primera columna
beta1 <- betasts[, 2]  # Segunda columna  
beta2 <- betasts[, 3]  # Tercera columna

###### analisis estacionario de mis betas estimados
## primero para beta0

adf_beta0 <- adf.test(na.omit(beta0))
print(adf_beta0)

## segundo para beta1

adf_beta1 <- adf.test(na.omit(beta1))
print(adf_beta1)

## tercera para beta2

adf_beta2 <- adf.test(na.omit(beta2))
print(adf_beta2)

##otra prueba 
kpss_beta0 <- kpss.test(na.omit(beta0), null = "Level")
print(kpss_beta0)

kpss_beta1 <- kpss.test(na.omit(beta1), null = "Level")
print(kpss_beta1)

kpss_beta2 <- kpss.test(na.omit(beta2), null = "Level")
print(kpss_beta2)

# ESTRATEGIA DIFERENCIAR TODO


beta0_diff <- diff(beta0)
beta1_diff <- diff(beta1)  
beta2_diff <- diff(beta2)

# Crear matriz VAR con series diferenciadas
longitud_minima <- min(length(beta0_diff), length(beta1_diff), length(beta2_diff))

betasEst <- cbind(
  beta0_diff[1:longitud_minima],
  beta1_diff[1:longitud_minima],
  beta2_diff[1:longitud_minima]
)

colnames(betasEst) <- c("beta0_diff", "beta1_diff", "beta2_diff")

# Verificar estacionariedad después de diferenciar




adf.test(beta0_diff)  
adf.test(beta1_diff)    
adf.test(beta2_diff)  

kpss.test(beta0_diff)
kpss.test(beta1_diff)
kpss.test(beta2_diff)

betas_ts_mat <- cbind(beta0, beta1, beta2)
colnames(betas_ts_mat) <- c("beta0", "beta1", "beta2")

##prueba para cointegracion

johansen_test <- ca.jo(betas_ts_mat, 
                       type = "trace",     # Test de la traza
                       ecdet = "const",    # Incluir constante
                       K = 2) 

summary(johansen_test)

johansen_test <- ca.jo(betas, type = "trace", ecdet = "none", K = 2)

# 2. Ajuste del modelo VECM con r = 2
vecm_model <- cajorls(johansen_test, r = 2)
summary(vecm_model$rlm)


# Convertir el VECM a VAR
var_model <- vec2var(johansen_test, r = 2)


# Pronóstico de 8 pasos adelante
forecast <- predict(var_model, n.ahead = 13)

# Ver los valores pronosticados
forecast$fcst

beta_pointNu <- sapply(forecast$fcst, function(x) x[,"fcst"])
t(beta_pointNu)
yptNu <- t(L %*% t(beta_pointNu))

#ajsutamos el modelo var con los nuevos betas

betas_ts_mat <- t(betas)

betas_tsEst <- ts(betasEst, frequency = frequency_val)

selEst <- VARselect(betas_tsEst, lag.max = 12, type = "const")

selEst$selection

p_choiceEst <- as.integer(selEst$selection["AIC(n)"])

var_modelEst <- VAR(betas_tsEst, p = p_choiceEst, type = "const")

var_predEst <- predict(var_modelEst, n.ahead = h, ci = 0.95)
var_predEst$fcst

beta_pointEst <- sapply(var_predEst$fcst, function(x) x[,"fcst"])
t(beta_pointEst)
yptEst <- t(L %*% t(beta_pointEst))







