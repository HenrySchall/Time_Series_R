####################################
### Modelo SARIMA (p,d,q)(P,D,Q) ###
####################################

# Modelo 1 -> SARIMA (0,0,1)(0,0,1)[12] 
# Modelo 2 -> SARIMA (1,0,0)(1,0,0)[12]  
# Modelo 3 -> SARIMA (1,1,0)(1,1,0)[12]  

################
### Modelo 1 ###
################

set.seed(123)

sarima_1 <- arima.sim(model = list(order = c(0,0,12),ma = c(0.5, rep(0,10), 0.4)), n = 1000, rand.gen = rnorm)
plot.ts(sarima_1, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(sarima_1, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(sarima_1, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

################
### Modelo 2 ###
################

set.seed(123)

sarima_2 <- arima.sim(model = list(order = c(12,0,0),ar = c(0.5, rep(0,10), 0.4)), n = 1000, rand.gen = rnorm)
plot.ts(sarima_2, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(sarima_2, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(sarima_2, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

################
### Modelo 3 ###
################

set.seed(123)

model <- Arima(ts(rnorm(200),freq=12), order=c(0,1,0), seasonal=c(1,0,0), fixed=c(Phi=0.6))
sarima_3 <- simulate(model, nsim=200)
plot.ts(sarima_3, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(sarima_3, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(sarima_3, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

###############################################
### Comportamento da série  com 1 diferença ###
###############################################

################
### Modelo 1 ###
################

DIF_sarima <- diff(sarima_1)
plot.ts(DIF_sarima, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 96)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

################
### Modelo 2 ###
################

DIF_sarima <- diff(sarima_2)
plot.ts(DIF_sarima, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 96)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

################
### Modelo 3 ###
################

DIF_sarima <- diff(sarima_3)
plot.ts(DIF_sarima, type = "l", main = "Modelo Simulado", xlab = "Tempo", ylab = "") 

acf_sarima <- acf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 96)
plot(acf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_sarima <- pacf(DIF_sarima, na.action = na.pass, plot = FALSE, lag.max = 48)
plot(pacf_sarima, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

#################
### Estimação ###
#################

### Modelo SARIMA (0,0,1)(0,0,1)[12] -> Modelo 1

fit <- Arima(sarima_1, order=c(0,0,1), seasonal=list(order=c(0,0,1), period=12))
coeftest(fit)

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals,type="Ljung",lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals^2,type="Ljung",lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
jarque.bera.test(na.remove(fit$residuals))

print(aic <- AIC(fit))

###  Modelo SARIMA (1,0,0)(1,0,0)[12] -> Modelo 2 

fit <- Arima(sarima_2, order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12))
coeftest(fit)

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals,type="Ljung",lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals^2,type="Ljung",lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
jarque.bera.test(na.remove(fit$residuals))

print(aic <- AIC(fit))

### Modelo SARIMA (1,1,0)(1,1,0)[12] -> Modelo 3

fit <- Arima(sarima_3, order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12))
coeftest(fit)

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals,type="Ljung",lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
Box.test(fit$residuals^2,type="Ljung",lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
jarque.bera.test(na.remove(fit$residuals))

print(aic <- AIC(fit))

#########################
### Modelo Incorretos ### 
#########################

fit <- Arima(TS_sarima, order=c(1,1,0), seasonal=list(order=c(1,0,1), period=12))

fit <- Arima(TS_sarima, order=c(0,1,1), seasonal=list(order=c(1,0,1), period=12))

fit <- Arima(TS_sarima, order=c(1,1,1), seasonal=list(order=c(1,0,0), period=12))

fit <- Arima(TS_sarima, order=c(1,1,1), seasonal=list(order=c(1,1,0), period=12))

fit <- Arima(TS_sarima, order=c(1,1,0), seasonal=list(order=c(1,1,1), period=12))

fit <- Arima(TS_sarima, order=c(1,1,1), seasonal=list(order=c(1,1,0), period=12))

fit <- Arima(TS_sarima, order=c(1,1,0), seasonal=list(order=c(1,1,1), period=12))

# Se rodarmos todos esses modelos veremos que todos terão um critério AIC/BIC, 
# inferior aos nossos três modelos originais, dando a série que nos geramos. 

###########################################
### Exemplo 1 - VENDAS PASSAGENS AÉREAS ###
###########################################

air_passengers <- ts(AirPassengers, start = c(1949, 1), frequency = 12)
plot(air_passengers, xlab = "Ano", ylab = "Quantidade", main = "Passagens Aéreas Vendidas pela Pan Am nos EUA")

# Diminuir o impacto de outliers (interpolação)
air_passengers_clean <- tsclean(air_passengers, replace.missing = TRUE)
plot(air_passengers)
lines(air_passengers_clean, col = "green")

# Examinar a existência de tendência e/ou sazonalidade
decomp <- stl(air_passengers_clean, "periodic")
colnames(decomp$time.series) = c("sazonalidade","tendência","restante")
plot(decomp)

# Outra alternativa
decomp <- decompose(air_passengers_clean)
plot(decomp)

# Efeito sazonal por ano comparando cada ano 
ggseasonplot(window(air_passengers_clean, start=c(1949), end=1960))

# Estabilizar a variância (logaritmo dos dados
log_air_passengers <- (log(air_passengers_clean))
plot(log_air_passengers, xlab = "Ano", ylab = "Log da quantidade", main = "Passagens Aéreas Vendidas pela Pan Am nos EUA")

# Como observamos no gráfico da série, há tendência nos dados e assim o teste verificará se a série se comporta como um passeio aleatório, isto é possível 
# por meio da opção type que tem as seguintes alternativas:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)

### Teste de Raiz Unitária Dickey-Fuller (ADF) ###
# - H0: raiz unitária (passeio aleatório) (p >= 0.05)
# - H1: sem raiz unitária (não é um passeio aleatório) (p <= 0.05)

unitRootnc_air_passengers <- adfTest(log_air_passengers, lags =12, type = c("nc"))
print(unitRootnc_air_passengers)

unitRootc_air_passengers <- adfTest(log_air_passengers, lags = 12, type = c("c"))
print(unitRootc_air_passengers)

unitRootct_air_passengers <- adfTest(log_air_passengers, lags = 12, type = c("ct"))
print(unitRootct_air_passengers)

# A série apresenta raiz unitária e precisamos aplicar diferenciação para torná-la estacionária. Como temos tendência e sazonalidade, o ideal é aplicar a primeira diferença para retirar 
# a tendência e após isso, a diferenciação sazonal para retirar a sazonalidade.

# Descobrindo quantas diferenças devemos tomar
ndiffs(log_air_passengers)

# Tirando a diferença
air_passengers_diff <- diff(diff(log_air_passengers, lag = 1, differences = 1), lag = 12, differences = 1)

# Calcular a FAC
acf_air_passengers_diff <- acf(air_passengers_diff, plot = FALSE, na.action = na.pass, lag.max = 24)
plot(acf_air_passengers_diff, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

# Calcular a FACP 
pacf_passengers_diff <- pacf(air_passengers_diff, plot = FALSE, na.action = na.pass, lag.max = 24)
plot(pacf_passengers_diff, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

##################################
### Escolhendo o melhor modelo ###
##################################

pars_air_passengers <- expand.grid(ar = 0:1, diff = 1, ma = 0:1, ars = 0:1, diffs = 1, mas = 0:1)
modelo_air_passengers <- list()

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML). Por default, a função arima usa a hipótese de que o termo de erro dos modelos arima segue uma distribuição Normal
for (i in 1:nrow(pars_air_passengers)) {modelo_air_passengers[[i]] <- arima(x = log_air_passengers, order = unlist(pars_air_passengers[i, 1:3]), 
seasonal = list(order = unlist(pars_air_passengers[i,4:6]), period = 12), method = "ML")}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca_air_passengers <- list()
for (i in 1:length(modelo_air_passengers)){log_verossimilhanca_air_passengers[[i]] <- modelo_air_passengers[[i]]$loglik}

# Calcular o AIC
aicsarima_air_passengers <- list()
for (i in 1:length(modelo_air_passengers)){aicsarima_air_passengers[[i]] <- AIC(modelo_air_passengers[[i]])}

# Calcular o BIC
bicsarima_air_passengers <- list()
for (i in 1:length(modelo_air_passengers)){bicsarima_air_passengers[[i]] <- BIC(modelo_air_passengers[[i]])}

# Quantidade de parâmetros estimados por modelo
quant_paramentros_air_passengers <- list()
for (i in 1:length(modelo_air_passengers)){quant_paramentros_air_passengers[[i]] <- length(modelo_air_passengers[[i]]$coef)+1}

# Montar a tabela com os resultados
especificacao_air_passengers <- paste0("SARIMA",pars_air_passengers$ar, pars_air_passengers$diff, pars_air_passengers$ma, pars_air_passengers$ars, pars_air_passengers$diffs, pars_air_passengers$mas)
tamanho_amostra_air_passengers <- rep(length(log_air_passengers), length(modelo_air_passengers))
resultado_air_passengers <- data.frame(especificacao_air_passengers, ln_verossimilhanca = unlist(log_verossimilhanca_air_passengers),quant_paramentros_air_passengers = unlist(quant_paramentros_air_passengers), 
tamanho_amostra_air_passengers, aic = unlist(aicsarima_air_passengers), bic = unlist(bicsarima_air_passengers))

# Mostrar a tabela de resultado
print(resultado_air_passengers)

# Modelo com menor AIC e/ou BIC
which.min(resultado_air_passengers$aic)
which.min(resultado_air_passengers$bic)

# Mostrando Coeficientes
print(summary(modelo_air_passengers[[11]]))
coeftest(modelo_air_passengers[[11]])

#########################
### Examinar Residuos ###
#########################

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
Box.test(modelo_air_passengers[[11]]$residuals,type="Ljung",lag=1)

acf_sarima_est <- acf(modelo_air_passengers[[11]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_sarima_est, main = "", ylab = "", xlab = "Defasagem")

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
Box.test(modelo_air_passengers[[11]]$residuals^2,type="Ljung",lag=1)

acf_sarima_square <- acf(modelo_air_passengers[[11]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_sarima_square, main = "", ylab = "", xlab = "Defasagem")

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
jarque.bera.test(na.remove(modelo_air_passengers[[11]]$residuals))

# Uma vez que os resíduos são ruído branco, obter as previsões.

################
### Previsão ###
################

forecast(object = modelo_air_passengers[[11]], h = 12, level = 0.80)

# - object: o modelo escolhido anteriormente
# - level: o intervalo de confiança (abaixo, 80%)
# - h: o horizonte de previsão

# gráfico da previsão
plot(forecast(object = modelo_air_passengers[[11]], h = 12, level = 0.80))
plot(modelo_air_passengers[[11]]$resid, col= "red")

# Gráfico Real x Previsto
fitted_air_passengers <- fitted(modelo_air_passengers[[11]])
plot(fitted_air_passengers, type = "l", lty = 4, col = "green")
lines(log_air_passengers, type = "l", lty = 1, col = "black")
legend("topleft", legend = c("Ajustado", "Real"), col = c(1,2), lty = c(1,3))

#########################################
### Exemplo 2 - PIB mensal em dólares ###
#########################################

dados <- gbcbd_get_series(id = 4385, first.date = "1990-01-01", last.date = Sys.Date())
View(dados)
dygraph(dados, main = "PIB mensal em dólares - US$ (milhões)") %>% dyRangeSelector()

# Criando série
dados_x <- dados$"value"
serie <- ts(dados_x, frequency = 12, star= c(1990))
title("PIB mensal em dólares - US$ (milhões)")
plot(serie)

# Examinar a existência de tendência e/ou sazonalidade nos dados
decomp <- stl(dados, "periodic")
colnames(decomp$time.series) = c("sazonalidade","tendência","restante")
plot(decomp)

# Diminuir o impacto de outliers (interpolação)
serie_suv <- tsclean(serie, replace.missing = TRUE)
plot(serie)
lines(serie_suv, col = "green")

# Estabilizar a variância (logaritmo dos dados)
log_serie <- (log(serie_suv))
plot(log_serie, xlab = "Ano", ylab = "Log", main = "PIB mensal em dólares - US$ (milhões)")

# Série Suavizada x Série Logaritimizada
par(mfrow=c(2,1)) 
plot(log_serie)
plot(serie2)
par(mfrow=c(1,1)) 

### Teste de Raiz Unitária Dickey-Fuller (ADF) ###
# - H0: raiz unitária (passeio aleatório) (p >= 0.05)
# - H1: sem raiz unitária (não é um passeio aleatório) (p <= 0.05)

# - nc: passeio aleatório
# - c: passeio aleatório com drift
# - ct: passeio aleatório com constante e tendência

adf_dados <- adfTest(log_serie, lags = 12, type = c("nc"))
print(adf_dados)

adf_dados <- fadfTest(log_serie, lags = 12, type = c("c"))
print(adf_dados)

adf_dados <- fadfTest(log_serie, lags = 12, type = c("ct"))
print(adf_dados)

#Não rejeito H0 -> O processo é não estacionário
ndiffs(log_serie)
dados_diff <- diff(diff(log_serie, lag = 1, differences = 1), lag = 12, differences = 1)
plot(dados_diff)

par(mfrow=c(2,1))

# FAC
acf_dados_diff <- acf(dados_diff, plot = FALSE, na.action = na.pass, lag.max = 24)
plot(acf_dados_diff, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)", adj = 0.5, line = 1)

# FACP 
pacf_dados_diff <- pacf(dados_diff, plot = FALSE, na.action = na.pass, lag.max = 24)
plot(pacf_dados_diff, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)", adj = 0.5, line = 1)

par(mfrow=c(1,1)) 

##################################
### Selecionando Melhor Modelo ###
##################################

pars_dados <- expand.grid (ar = 0, diff = 1, ma = 0, ars = 0:1, diffs = 0:1, mas = 0:1)
modelos <- list()

for (i in 1:nrow(pars_dados)) {modelos[[i]] <- arima(x = dados_diff, order = unlist(pars_dados[i, 1:3]), 
seasonal = list(order = unlist(pars_dados[i,4:6]), period = 12), method = "ML")}

log_verossimilhanca <- list()
for (i in 1:length(modelos)){log_verossimilhanca[[i]] <- modelos[[i]]$loglik}

# Calcular o AIC
aic_sarima <- list()
for (i in 1:length(modelos)) {aic_sarima[[i]] <- AIC(modelos[[i]])}

# Calcular o BIC
bic_sarima <- list()
for (i in 1:length(modelos)) {bic_sarima[[i]] <- BIC(modelos[[i]])}

#Quantidade de parâmetros estimados por modelo
quant_parametros <- list()
for (i in 1:length(modelos)) {quant_parametros[[i]] <- length(modelos[[i]]$coef)+1}

especificacao <- paste0("SARIMA",pars_dados$ar, pars_dados$diff, pars_dados$ma, pars_dados$ars,
pars_dados$diffs, pars_dados$mas)

resultado <- data.frame(especificacao,ln_verossimilhanca = unlist(log_verossimilhanca), 
quant_parametros = unlist(quant_parametros),aic = unlist(aic_sarima), bic = unlist(bic_sarima))

print(resultado)

best1 <- which.min(resultado$aic)
print(best1)
best2 <- which.min(resultado$bic)
print(best2)

coeftest(modelos[[best1]])

#########################
### Examinar Residuos ###
#########################

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados
#  - H1: resíduos são autocorrelacionados
Box.test(modelos[[best1]]$residuals,type="Ljung",lag = 24)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados
#  - H1: quadrado dos resíduos são autocorrelacionados
Box.test(modelos[[best1]]$residuals^2,type="Ljung",lag = 24)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos
#  - H1: resíduos não são normalmente distribuídos
jarque.bera.test(na.remove(modelos[[best1]]$residuals))

################
### Previsão ###
################

# Gráfico da previsão
plot(forecast(object = modelos[[best1]], h = 12, level = 0.95))

# Gráfico Real x Previsto
fitted_modelo <- fitted(modelos[[best1]])
plot(fitted_modelo, type = "l", lty = 1, col = "black")
lines(log_serie, type = "l", lty = 3, col = "white")
legend("topleft", legend = c("Ajustado", "Real"), col = c(1,2), lty = c(1,3))