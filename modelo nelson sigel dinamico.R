# Instalar paquetes necesarios (si no los tienes)
install.packages(c("readxl", "YieldCurve", "vars", "tidyverse", "ggplot2"))

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



mi_carpeta <- "C:/Users/abies/Downloads"
setwd("C:/Users/abies/Downloads")
# Para archivos Excel
archivos_xlsx <- list.files(mi_carpeta, pattern = "\\.xlsx$", full.names = TRUE)
data <- read_excel("ADVANCED RESULT_2025-09-15_21_32.xlsx", sheet = "Exported data")



# Verificar estructura
head(yield_data)

colnames(data) <- c("Period", "M1_avg", "M1_ult", "M3_avg", "M3_ult",
                          "M6_avg", "M6_ult", "Y2_avg", "Y2_ult", "Y5_avg",
                          "Y5_ult", "Y7_avg", "Y7_ult", "Y10_avg", "Y10_ult")

seq(0.01, 2, length.out = 200)
datas<-data[-c(1:4,196:226), ]
datas1<-datas[,-c(2,4,6,8,10,12,14)]  
datas1[ , 2:(ncol(datas1)-1)] <- lapply(datas1[ , 2:(ncol(datas1)-1)], as.numeric)
str(datas1)
datas1$Period
e
exp(1)
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
f2(3)



library(lubridate)

datas1$Period_date <- ym(datas1$Period)

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

# Verificar rangos de tasas
print("Resumen de tasas:")
print(summary(as.vector(yields_matrix)))

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

lambda_hat_grid <- lambda_grid[which.min(sse_vals)]

plot(lambda_grid, sse_vals, type = "l", lwd = 2,
     xlab = expression(lambda), ylab = "SSE",
     main = "Error de ajuste para distintos valores de lambda")

#calcualmos ahora
landa<-lambda_hat_grid

f3 <- function(x) {
  # Ejemplo: función cuadrática
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


plot(tau, yields_matrix, type="b", pch=19, xaxt="n",
     xlab="Maturities", ylab="Yield",
     main=paste("Curva observada vs estimada (t =", t, ")"))
axis(1, at=1:7, labels=c("M1","M3","M6","Y2","Y5","Y7","Y10"))
lines(1:7, y_est, type="b", pch=17, col="blue")
legend("bottomright", legend=c("Observado","Estimado"), 
       col=c("black","blue"), pch=c(19,17), lty=1)


#############################################
#residuos


res<-yields_matrix- Y_est


########################################################## predicciones modelo var

install.packages(c("vars","MASS"))
library(vars)
library(MASS)

frequency_val <- 12   # 
h <- 4 

betas_ts_mat <- t(betas)
betas_ts <- ts(betas_ts_mat, frequency = frequency_val)

sel <- VARselect(betas_ts, lag.max = 12, type = "const")

sel$selection
p_choice <- as.integer(sel$selection["AIC(n)"])

var_model <- VAR(betas_ts, p = p_choice, type = "const")

var_pred <- predict(var_model, n.ahead = h, ci = 0.95)
var_pred$fcst

beta_point <- sapply(var_pred$fcst, function(x) x[,"fcst"])
t(beta_point)
ypt <- t(L %*% t(beta_point))







