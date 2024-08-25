#########################
### Passeio aleatório ###
#########################

set.seed(123)

# Configurando R para dividir a página de gráficos em duas linhas e uma coluna. Porque temos 2 gráficos
par(mfrow=c(2,1)) 

# Retornar o ambiente para ter um gráfico por página, ou seja, uma linha e uma coluna
par(mfrow=c(1,1)) 

###################################
### Passeio aleatório sem drift ###
###################################

n <- 1000
p0 <- 10
phi1 <- 1
pt <- rep(p0,n)
for (i in 1:(n-1)) {pt[i+1] <- phi1*pt[i] + rnorm(1)}

# Visualização do passeio aleatório.
plot(pt, type = "l", xlab = "", ylab = "preço", main = "Simulação Passeio Aleatório sem Drift")

# Visualização da primeira diferença do passeio aleatório, que é estacionário
plot(diff(pt), type = "l", xlab = "", ylab = "", main = "Diferença nos preços - Estacionária")

###################################
### Passeio aleatório com drift ###
###################################

n <- 1000
p0 <- 10
mi <- 0.2
phi1 <- 1
pt_drift <- rep(p0,n)
for (i in 1:(n-1)) {pt_drift[i+1] <- mi + phi1*pt_drift[i] + rnorm(1)}

# Visualização do passeio aleatório com drift
plot(pt_drift, type = "l", xlab = "", ylab = "preço", main = "Simulação Passeio Aleatório com Drift")

# Visualização da primeira diferença do passeio aleatório que é estacionário
plot(diff(pt_drift), type = "l", xlab = "", ylab = "", main = "Diferença nos preços - Estacionária")

###############################################
### Passeio aleatório com drift e tendência ###
###############################################

n <- 1000
p0 <- 10
mi <- 0.2
phi1 <- 1
trend <- 1:n
pt_drift_trend <- rep(p0,n)
for (i in 1:(n-1)) {pt_drift_trend[i+1] <- mi + trend + phi1*pt_drift_trend[i] + rnorm(1)}

# Visualização do passeio aleatório com drift
plot(pt_drift_trend, type = "l", xlab = "", ylab = "preço", main = "Simulação Passeio Aleatório com Drift")

#  Visualização da primeira diferença do passeio aleatório que é estacionário
plot(diff(pt_drift_trend), type = "l", xlab = "", ylab = "", main = "Diferença nos preços - Estacionária")

############################
### Modelo ARIMA (p,d,q) ###
############################

# Usando como exemplo a série do preço da ação da Microsoft

microsoft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE, return.class = 'xts')
View(microsoft)
plot.xts(microsoft$MSFT.Close, xlab = "", ylab = "", main = "Preço da ação da Microsoft")

##################
### Tratamento ###
##################

microsoft_clean <- tsclean(x = microsoft$MSFT.Close, replace.missing = TRUE) #diminuir o impacto de outliers (interpolação)
microsoft_tratado <- Return.calculate(microsoft_clean$MSFT.Close, method = "log") #transformar os dados para estabilizar a variância (logaritmo)
plot.xts(microsoft_tratado, xlab = "", ylab = "", main = "Retornos da ação da Microsoft") #gráfico dos retornos

################################
### Testando Estacionaridade ###
################################

# Como observamos no gráfico da série, não há tendência nos dados e assim o teste verificará se a série se comporta como um passeio aleatório, isto é possível 
# por meio da opção type que tem as seguintes alternativas:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)

### Teste de Raiz Unitária Dickey-Fuller (ADF) ###
# - H0: raiz unitária (passeio aleatório)
# - H1: sem raiz unitária (não é um passeio aleatório)

unit_root_microsoft <- adfTest(microsoft_tratado, lags = 2, type = c("nc"))
print(unit_root_microsoft)

unit_root_microsoft <- adfTest(microsoft_tratado, lags = 2, type = c("c"))
print(unit_root_microsoft)

unit_root_microsoft <- adfTest(microsoft_tratado, lags = 2, type = c("ct"))
print(unit_root_microsoft)

# Como resultado do teste temos um p-valor de 0.01 indicando que rejeitamos a hipótese nula de presença de raiz unitária ao nível de 5% de significância. Assim, continuamos com a série de
# retornos e não precisamos realizar a diferença para estacionar a série

##################################
### Escolhendo o melhor modelo ###
##################################

# Função de autocorrelação (FAC)
acf_microsoft <- acf(microsoft_tratado, plot = FALSE, na.action = na.pass, max.lag = 25) 
plot(acf_microsoft, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Retornos da Microsoft")

# Função de autocorrelação parcial (FACP)
pacf_microsoft <- pacf(microsoft_tratado, plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_microsoft, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP) dos Retornos da Microsoft")

for (i in 1:nrow(pars_microsoft)) {modelo_microsoft[[i]] <- arima(x = microsoft_tratado, order = unlist(pars_microsoft[i, 1:3]), method = "ML")}
log_verossimilhanca_microsoft <- list()
for (i in 1:length(modelo_microsoft)) {log_verossimilhanca_microsoft[[i]] <- modelo_microsoft[[i]]$loglik}

# Calcular o AIC
aicarima_microsoft <- list()
for (i in 1:length(modelo_microsoft)) {aicarima_microsoft[[i]] <- stats::AIC(modelo_microsoft[[i]])}

# Calcular o BIC
bicarima_microsoft <- list()
for (i in 1:length(modelo_microsoft)) {bicarima_microsoft[[i]] <- stats::BIC(modelo_microsoft[[i]])}

quant_paramentros_microsoft <- list()
for (i in 1:length(modelo_microsoft)) {quant_paramentros_microsoft[[i]] <- length(modelo_microsoft[[i]]$coef)+1}
especificacao_microsoft <- paste0("ARIMA",pars_microsoft$ar, pars_microsoft$diff, pars_microsoft$ma)
tamanho_amostra_microsoft <- rep(length(logmicrosoft), length(modelo_microsoft))
resultado_microsoft <- data.frame(especificacao_microsoft, ln_verossimilhanca = unlist(log_verossimilhanca_microsoft),
quant_paramentros_microsoft =  unlist(quant_paramentros_microsoft), tamanho_amostra_microsoft, aic = unlist(aicarima_microsoft), bic = unlist(bicarima_microsoft))

print(resultado_microsoft)
which.min(resultado_microsoft$aic)
which.min(resultado_microsoft$bic)

coeftest(modelo_microsoft[[5]])
coeftest(modelo_microsoft[[14]])

#########################
### Examinar Residuos ###
#########################

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
acf_arima101_est <- acf(modelo_microsoft[[5]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_arima101_est, main = "", ylab = "", xlab = "Defasagem")
Box.test(modelo_microsoft[[5]]$residuals,type="Ljung",lag=1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
acf_arima101_square <- acf(modelo_microsoft[[5]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_arima101_square, main = "", ylab = "", xlab = "Defasagem")
Box.test(modelo_microsoft[[5]]$residuals^2,type="Ljung",lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
jarque.bera.test(na.remove(modelo_microsoft[[5]]$residuals))

# Se os resíduos são ruído branco, obtem-se as previsões.

################
### Previsão ###
################

forecast(object = modelo_microsoft[[5]], level = 95, h = 10)
plot(forecast(object = modelo_microsoft[[5]], level = 95, h = 10))
modelo_microsoft[[5]]$coef

# Comparando modelo real x modelo ajustado 
fitted_microsoft <- fitted(modelo_microsoft[[5]])
plot.ts(microsoft_tratado, type = "l", lty = 1, col = 2)
lines(fitted_microsoft, type = "l", lty = 1, col = 1)
legend("topright", legend = c("Real", "Ajustado"), col = c(2,1), lty = c(1,1))
