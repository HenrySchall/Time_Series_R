################################
####  MODELO ARMA-GARCH    #####
################################

#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

#####
##   PETROBRAS (PETR3.SA)
#####

# Dados da ação PETR3.SA desde 01/01/2017
price_day <- quantmod::getSymbols("PETR3.SA", src = "yahoo", from = '2015-01-01')
log_day_return <- na.omit(PerformanceAnalytics::Return.calculate(PETR3.SA$PETR3.SA.Close, method = "log"))
log_day_return_squad <- log_day_return^2

# Gráfico dos preços e retonos (observer a presença de heterocedasticidade condicional)
plot.xts(PETR3.SA$PETR3.SA.Close, main = "Preços da PETR3", xlab = "tempo", ylab = "preços")
plot.xts(log_day_return, main = "Retornos da PETR3", xlab = "tempo", ylab = "retorno")


##################################
####   ESTIMAÇÃO ARMA-ARCH    ####
##################################

####
##  1: Especificar uma equação para a média condicional 
####

# Trata-se de aplicar o processo de estimação de modelos ARMA(p,q) já estudado:
# - detalhes no arquivo simulacaoarma.R disponível na pasta 4.ARMA deste projeto

# Função de autocorrelação
acf_arma <- stats::acf(log_day_return, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação. 
plot(acf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)", adj = 0.5, line = 1)

# Função de autocorrelação parcial
pacf_arma <- stats::pacf(log_day_return, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação parcial. 
plot(pacf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)", adj = 0.5, line = 1)

# Todas as combinações possíveis de p=0 até p=max e q=0 até q=max
pars <- expand.grid(ar = 0:0, diff = 0, ma = 0:0)

# Local onde os resultados de cada modelo será armazenado
modelo <- list()

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars)) {
  modelo[[i]] <- arima(log_day_return, order = unlist(pars[i, 1:3]), method = "ML")
}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca <- list()
for (i in 1:length(modelo)) {
  log_verossimilhanca[[i]] <- modelo[[i]]$loglik
}

# Calcular o AIC
aicarma <- list()
for (i in 1:length(modelo)) {
  aicarma[[i]] <- stats::AIC(modelo[[i]])
}

# Calcular o BIC
bicarma <- list()
for (i in 1:length(modelo)) {
  bicarma[[i]] <- stats::BIC(modelo[[i]])
}

# Quantidade de parâmetros estimados por modelo
quant_parametros <- list()
for (i in 1:length(modelo)) {
  quant_parametros[[i]] <- length(modelo[[i]]$coef)+1 # +1 porque temos a variância do termo de erro 
}

# Montar a tabela com os resultados
especificacao <- paste0("arma",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(log_day_return), length(modelo))
resultado_arma <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca),
                       quant_parametros = unlist(quant_parametros),
                       tamanho_amostra, aic = unlist(aicarma), 
                       bic = unlist(bicarma), stringsAsFactors = FALSE)

# Mostrar a tabela de resultado
print(resultado_arma)

# Escolher o melhor modelo
which.min(resultado_arma$aic)
which.min(resultado_arma$bic)

# Como resultado temos que o modelo escolhido tanto pelo AIC quanto pelo BIC é o ARMA(0,0)
media_condicional <- arima(log_day_return, order = c(0,0,0), method = "ML")

# Verificar qual distribuição de probabilidade melhor se assemelha aos resíduos da média condicional
# Este é um passo importante para o restante da análise. Precisamos garantir que distribuição de 
# probabilidade usada no processo de estimação por meio de máxima verossimilhança faça uso da correta
# distribuição. Assim, comparamos graficamente os resíduos obtidos pela estimação da média condicional
# com duas distribuições de probabilidade (Normal e t-Student). A comparação feita aqui não considera
# assimetria e em função disso, caso você perceba a existência de assimetria, você deve escolher a 
# distribuição que mais se assemelha aos dados, mas optar pela sua versão com assimetria no momento
# que for estimar o modelo arma-garch conjuntamente. Como resultado, temos que a distribuição t-Student
# é a melhor escolha. 

symmetric_normal = stats::density(stats::rnorm(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                               sd = sd(media_condicional$residuals)))

symmetric_student = stats::density(fGarch::rstd(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                                sd = sd(media_condicional$residuals)))

hist(media_condicional$residuals, n = 25, probability = TRUE, border = "white", col = "steelblue",
     xlab = "Resíduos estimados pela média condicional", ylab = "Densidade", 
     main = "Comparativo da distribuição dos resíduos")
lines(symmetric_normal, lwd = 3, col = 2)
lines(symmetric_student, lwd = 2, col = 1)
legend("topleft", legend = c("Normal", "t-Student"), col = c("red", "black"), lwd = c(3,2))

# Parâmetros estimados
print(media_condicional)

####
##  2: Testar existência de efeito ARCH
####

# Opção 1: visualizar a FAC dos resíduos aos quadrado (lembre-se de que eles 
# são uma proxy para o retorno ao quadrado). Como resultado temos que há defasagens
# estatísticamente significantes (acima da linha pontilhada). O gráfico da FAC com
# a linha pontilhada é apenas uma forma visual de verificar o teste Ljung-Box que
# analisa se a autocorrelação é estatísticamente diferente de zero.
acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(0,0)", adj = 0.5, line = 1)


# Opção 2: teste LM de Engle (similar ao teste F de uma regressão linear). O resultado
# mostra o p-valor do teste assumindo que a equação do mesmo (a equação do modelo ARCH)
# pode ter tantas defasagens quantas apresentadas na coluna order. Assim, assumindo um
# modelo ARCH(4), rejeitamos a hipótese nula de que todas as defasagens são nulas, ou seja,
# há pelo menos uma diferente de zero e assim, temos heterocedasticidade condicional no erro
# da média condicional
lm_test <- aTSA::arch.test(media_condicional, output = FALSE)
lm_test

####
##  3: Especificar modelo para a volatilidade condicional 
####

# Passo 1: Identificar as ordens máximas M e N do arch e garch, respectivamente. Para tanto, 
# usamos as funções de autocorrelação parcial (FACP) e autocorrelação (FAC)

acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(0,0)", adj = 0.5, line = 1)

pacf_residuals <- stats::pacf(media_condicional$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos resíduos do ARMA(0,0)", adj = 0.5, line = 1)

# Passo 2: Modelar conjuntamente a média condicional e a variância condicional

# Todas as combinações possíveis de m=1 até m=max e n=0 até n=max
pars_arma_garch <- expand.grid(m = 1:3, n = 0:2)

# Local onde os resultados de cada modelo será armazenado
modelo_arma_garch <- list()

# Especificação arma encontrada na estimação da média condicional
arma_set <- "~arma(0,0)"

# Distribuição de probabilidade assumida para o termo de erro da média condicional 
# - norm: normal, std: t-student, snorm: normal assimétrica, sstd: t-student assimétrica
arma_residuals_dist <- "std"

# Definição se o processo estimará parâmetros de assimetria e curtose para a distribuição
include.skew = FALSE
include.shape = FALSE

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars_arma_garch)) {
  modelo_arma_garch[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                            data = log_day_return, trace = FALSE, cond.dist = arma_residuals_dist,
                                            include.skew = include.skew, include.shape = include.shape) 
}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  log_verossimilhanca_arma_garch[[i]] <- modelo_arma_garch[[i]]@fit$llh
}

# Calcular o AIC
aicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  aicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[1]
}

# Calcular o BIC
bicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  bicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[2]
}

# Quantidade de parâmetros estimados por modelo
quant_paramentros_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  quant_paramentros_arma_garch[[i]] <- length(modelo_arma_garch[[i]]@fit$coef)
}

# Montar a tabela com os resultados
especificacao <- paste0(arma_set,"-","garch",pars_arma_garch$m,pars_arma_garch$n)
tamanho_amostra <- rep(length(log_day_return), length(modelo_arma_garch))
resultado_arma_garch <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca_arma_garch),
                       quant_paramentros = unlist(quant_paramentros_arma_garch),
                       tamanho_amostra, aic = unlist(aicarma_garch), bic = unlist(bicarma_garch),
                       stringsAsFactors = FALSE)

# Escolher o modelo com menor AIC e/ou BIC
which.min(resultado_arma_garch$aic)
which.min(resultado_arma_garch$bic)

# Mostrar o resultado da tabela
print(resultado_arma_garch)

# Estimar o modelo escolhido
media_variancia_condicional <- fGarch::garchFit(~arma(0,0)+garch(1,1), data = log_day_return, trace = FALSE, 
                                                cond.dist = "std", include.skew = include.skew,
                                                include.shape = include.shape)

# Parâmetros estimados. Aqui, usamos a função stargazer do pacote stargazer para 
# mostrar os resultados em um formato textual mais amigável para interpretação.
# Mais detalhes? Use help("stargazer")
stargazer::stargazer(media_variancia_condicional, type = "text", title = "Resultado Estimação modelo ARMA(0,0)-GARCH(1,1)")

####
##  4: Avaliar o modelo estimado
####

# Verificar se todos os parâmetros são estatisticamente significantes.
# Nos resultados mostrados no código da linha 231 percebemos que todos
# os parâmetros são estatísticamente significantes. Se esse não for o 
# caso, deveríamos retirar o parâmetro não significante do processo de
# estimação e verificar novamente a significância dos demais parâmetros


# Verificando se os resíduos ao quadrado ainda continuam com heterocedasticidade condicional
# O resultado mostra que a heterocedasticidade condicional foi tratada dado que não há defasagens
# estatísticamente significantes (acima da linha pontilhada) na FAC
acf_residuals_armagarch <- acf(media_variancia_condicional@residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_armagarch, main = "", ylab = "", xlab = "Defasagem")
title("FAC dos resíduos do arma(0,0)-GARCH(1,1)", adj = 0.5, line = 1)

# Previsão do retorno esperado e variância esperada
fGarch::predict(media_variancia_condicional, n.ahead = 3)
