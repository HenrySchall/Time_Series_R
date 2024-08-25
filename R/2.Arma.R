#########################
### Modelo AR (p,0,0) ###
#########################

#################
### AR(1,0,0) ###
#################

set.seed(123) #definindo mesmo ponto inicial

ar1 <- arima.sim(model = list(order = c(1,0,0), ar = c(0.5)), n = 1000, rand.gen = rnorm)

# model: uma lista com as especificações ar, diff e ma do modelo com as seguintes opções:
# order: a ordem do modelo (p,d,q)
# ar: valores para os parâmetros ar do modelo (no exemplo abaixo apeas um valor para o ar1)
# ma: valores para os parâmetros ma do modelo (no exemplo abaixo, não temos nenhum valor porque em order não especificamos ma)
# n: o tamanho da série temporal a ser simulada
# rand.gen: função para gerar os resíduos do modelo.
# rnorm torna os resíduos normalmente distribuídos. Outra alternativa, seria usar a função rt para resíduos distribuídos conforme uma t de Student.  

########################
### Gráfico da Série ###
########################

plot.ts(ar1, main = "AR(1) Simulado", xlab = "Tempo", ylab = "")
# plot.ts(ar1, main = "AR(1) Simulado")

#########################
### Função FAC e FACP ###
#########################

# Função de autocorrelação (FAC)
acf_ar1 <- acf(ar1, na.action = na.pass)
plot(acf_ar1, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

# title("Função de Autocorrelação (FAC)", adj = 0.5, line = 2) 
#   - adj define a posição do titulo
#   - line define a altura do título

# Função de autocorrelação parcial (FACP)
pacf_ar1 <- pacf(ar1, na.action = na.pass)
plot(pacf_ar1, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

# Análise dos resíduos 
checkresiduals(ar1)

################
### Previsão ###
################

previsao_ar1 <- arima(ar1, order=c(1,0,0))
previsao_ar1$coef

previsao_ar1 <- forecast(object = ar1, level = 95, h = 10)
plot(previsao_ar1)

#################
### AR(2,0,0) ###
#################

ar2 <- arima.sim(model = list(order = c(2,0,0), ar = c(-0.8,-0.6)), n = 1000, rand.gen = rnorm)
plot.ts(ar2, main = "AR(2) Simulado", xlab = "", ylab = "")

acf_ar2 <- acf(ar2)
plot(acf_ar2, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_ar2 <- pacf(ar2)
plot(pacf_ar2, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

previsao_ar2 <- arima(ar2, order=c(2,0,0))
previsao_ar2$coef

previsao_ar2 <- forecast(object = ar2, level = 95, h = 10)
plot(previsao_ar2)

#########################
### Modelo MA (0,d,0) ###
#########################

set.seed(123)

#################
### MA(0,1,0) ###
#################

ma1 <- arima.sim(model = list(order = c(0,0,1), ma = c(0.3)), n = 1000, rand.gen = rnorm)
plot.ts(ma1, main = "MA(1) Simulado", xlab = "", ylab = "")

acf_ma1 <- acf(ma1, na.action = na.pass)
plot(acf_ma1, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_ma1 <- pacf(ma1, na.action = na.pass)
plot(pacf_ma1, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

checkresiduals(ma1)

previsao_ma1 <- arima(ma1, order=c(0,0,1))
previsao_ma1$coef

previsao_ma1 <- forecast(object = ma1, level = 95, h = 10)
plot(previsao_ma1)

#################
### MA(0,2,0) ###
#################

ma2 <- arima.sim(model = list(order = c(0,0,2), ma = c(0.3,0.5)), n = 1000, rand.gen = rnorm)
plot.ts(ma2, main = "MA(2) Simulado", xlab = "", ylab = "")

acf_ma2 <- acf(ma2, na.action = na.pass)
plot(acf_ma2, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_ma2 <- pacf(ma2, na.action = na.pass)
plot(pacf_ma2, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

previsao_ma2 <- arima(ma2, order=c(0,0,2))
previsao_ma2$coef

previsao_ma2 <- forecast(object = ma2, level = 95, h = 10)
plot(previsao_ma2)

###########################
### Modelo ARMA (p,0,q) ###
###########################

set.seed(123)

#################
### ARMA(1,1) ###
#################

arma11 <- arima.sim(model = list(order = c(1,0,1),ar = c(0.4),ma = c(0.3)), n = 1000, rand.gen = rnorm)+2 #+2 faz o papel do intercepto = 2, pois estou somando 2 na parte função dos parâmetroa ar e ma
plot.ts(arma11, type = "l", main = "ARMA(1,1) Simulado", xlab = "Tempo", ylab = "") 

acf_arma11 <- acf(arma11, na.action = na.pass)
plot(acf_arma11, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)")

pacf_arma11 <- pacf(arma11, na.action = na.pass)
plot(pacf_arma11, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)")

################################################
### Escolhendo a melhor combinação usando MV ###
################################################

pars <- expand.grid(ar = 0:2, diff = 0, ma = 0:2) #todas as combinações possíveis de p=0 até p=max e q=0 até q=max
modelo <- list() #armazenando os resultados

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars)) {modelo[[i]] <- arima(x = arma11, order = unlist(pars[i, 1:3]), method = "ML")}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca <- list()
for (i in 1:length(modelo)) {log_verossimilhanca[[i]] <- modelo[[i]]$loglik}

# Calcular o AIC
aicarma <- list()
for (i in 1:length(modelo)) {aicarma[[i]] <- stats::AIC(modelo[[i]])}

# Calcular o BIC
bicarma <- list()
for (i in 1:length(modelo)) {bicarma[[i]] <- stats::BIC(modelo[[i]])}

# Quantidade de parâmetros estimados por modelo
quant_paramentros <- list()
for (i in 1:length(modelo)) {quant_paramentros[[i]] <- length(modelo[[i]]$coef)+1} #+1 porque temos a variância do termo de erro

# Montar a tabela com os resultados
especificacao <- paste0("ARMA",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(arma11), length(modelo))
resultado <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca),
quant_paramentros = unlist(quant_paramentros),tamanho_amostra, aic = unlist(aicarma), bic = unlist(bicarma))

print(resultado)

# Escolhendo o melhor modelo
which.min(resultado$aic) 
which.min(resultado$bic)

# Gerar a tabela usamos a função stargazer
stargazer(modelo[[5]], type = "text", title = "Resultado Estimação modelo ARMA(1,1)")

##########################
### Verificar Residuos ###
##########################

# Teste de autocorrelação dos resíduos
#  - H0: resíduos são não autocorrelacionados (p > 0.05)
#  - H1: resíduos são autocorrelacionados (p <= 0.05)
acf_arma11_est <- acf(modelo[[5]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_arma11_est, main = "", ylab = "", xlab = "Defasagem") #correlacionado apenas com ele mesmo 
Box.test(modelo[[5]]$residuals,type="Ljung",lag=1)

# LEMBRE-SE: Ter autocorrelação não e um problema (questão de grau), mas ter autocorrelção com os 
# resíduos sim.

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados (p > 0.05)
#  - H1: quadrado dos resíduos são autocorrelacionados (p <= 0.05)
acf_residuals = acf(modelo[[5]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
Box.test(modelo[[5]]$residuals^2,type="Ljung",lag=1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
# shapiro.test(na.remove(modelo[[5]]$residuals))
jarque.bera.test(na.remove(modelo[[5]]$residuals))

# Se os resíduos são ruído branco, obtem-se as previsões.

# Previsão do valor médio condicional esperado e respectivo desvio
# - object: o modelo escolhido anteriormente
# - level: o intervalo de confiança
# - h: o horizonte de previsão

forecast(object = modelo[[5]], level = 95, h = 10)
plot(forecast(object = modelo[[5]], level = 95, h = 10))
modelo[[5]]$coef

##################
### Observação ###
##################

# No momento que simulamos os dados do modelo optamos pela função rnorm para gerar o termo 
# de erro. Assim, estamos dizendo que a variável aleatória do termo de erro segue uma normal. 
# Caso optássemos por outra distribuição de probabilidade, estariamos assumindo que o termo 
# de erro segue tal distribuição. Se por exemplo fosse uma t-student, o teste para verificar 
# se os resíduos obtidos após o processo de estimação deve ser alterado. 