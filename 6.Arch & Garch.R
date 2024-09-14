##################################
####   SIMULAÇÃO ARMA-ARCH    ####
##################################


#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

#####
##   SIMULAR DADOS PARA PROCESSO ARMA(1,0)-ARCH(1)
#####

# Fixar a raiz para os dados gerados na simulação serem iguais em qualquer computador
set.seed(123)

# Especificação para as funções da média e variância condicional:
# - mu: valor do parâmetro da média incodicional da função da média condicional
# - ar: valor do parâmetro da parte autorregressiva da média condicional 
# - ma: valor do parâmetro da parte de médias móveis da média condicional
# - omega: valor do parâmetro do intercepto da variância condicional
# - alpha: valor do parâmetro da parte arch da variância condicional
# - beta: valor do parâmetro da parte garch da variância condicional
# - cond.dist: a distribuição de probabilidade assumida para at que pode ser:
# norm: normal, std: t-student, snorm: normal assimétrica, sstd: t-student assimétrica
# - skew: parâmetro da assimetria da distribuição de probabilidade assumida para 
# o termo de erro da média condicional (at)
# - shape: parâmetro da forma da distribuição de probabilidade assumida para o 
# termo de erro da média condicional (at)
arma10arch1_spec <- fGarch::garchSpec(model = list(mu = 0.02, ar = 0.5, omega = 1.5, alpha = 0.6, beta = 0), cond.dist = "std")

# Simulação da série temporal dos retornos
# - spec: a especificação criada na etapa anterior
# - n: o tamanho da série temporal de interesse
arma10arch1_sim <- fGarch::garchSim(spec = arma10arch1_spec, n = 1000)

# obter os preços do ativo a partir dos retornos
precos <- rep(0, length(arma10arch1_sim))
precos[1] <- 10

for (i in 2:length(precos)) {
  precos[i] <- exp(arma10arch1_sim[i]/100)*precos[i-1]
}

#####
##   GRÁFICO DA SÉRIE TEMPORAL DOS RETORNOS SIMULADA BEM COMO DOS PREÇOS
#####

plot.ts(precos, main = "Preços Simulados: ARMA(1,0)-ARCH(1)", xlab = "tempo", ylab = "preços")
plot.ts(arma10arch1_sim, main = "Retornos Simulados: ARMA(1,0)-ARCH(1)", xlab = "tempo", ylab = "retorno")

##################################
####   ESTIMAÇÃO ARMA-ARCH    ####
##################################

####
##  1: Especificar uma equação para a média condicional 
####

# Trata-se de aplicar o processo de estimação de modelos ARMA(p,q) já estudado:
# - detalhes no arquivo simulacaoarma.R disponível na pasta 4.ARMA deste projeto

# Função de autocorrelação
acf_arma <- stats::acf(arma10arch1_sim, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação. 
plot(acf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)", adj = 0.5, line = 1)

# Função de autocorrelação parcial
pacf_arma <- stats::pacf(arma10arch1_sim, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação parcial. 
plot(pacf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)", adj = 0.5, line = 1)

# Todas as combinações possíveis de p=0 até p=max e q=0 até q=max
pars <- expand.grid(ar = 0:1, diff = 0, ma = 0:3)

# Local onde os resultados de cada modelo será armazenado
modelo <- list()

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars)) {
  modelo[[i]] <- arima(arma10arch1_sim, order = unlist(pars[i, 1:3]), method = "ML")
}

# Obter o logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca <- list()
for (i in 1:length(modelo)) {
  log_verossimilhanca[[i]] <- modelo[[i]]$loglik
}

# Calcular o AIC
aicarma <- list()
for (i in 1:length(modelo)) {
  aicarma[[i]] <- AIC(modelo[[i]])
}

# Calcular o BIC
bicarma <- list()
for (i in 1:length(modelo)) {
  bicarma[[i]] <- BIC(modelo[[i]])
}

# Quantidade de parâmetros estimados por modelo
quant_parametros <- list()
for (i in 1:length(modelo)) {
  quant_parametros[[i]] <- length(modelo[[i]]$coef)+1 # +1 porque temos a variância do termo de erro 
}

# Montar a tabela com os resultados
especificacao <- paste0("arma",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(arma10arch1_sim), length(modelo))
resultado_arma <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca),
                            quant_parametros = unlist(quant_parametros),
                            tamanho_amostra, aic = unlist(aicarma), 
                            bic = unlist(bicarma), stringsAsFactors = FALSE)

# Escolher o melhor modelo
which.min(resultado_arma$aic)
which.min(resultado_arma$bic)

# Mostrar a tabela de resultado
print(resultado_arma)

# Como resultado temos que o modelo escolhido tanto pelo AIC quanto pelo BIC é o ARMA(1,0)
# o que está de acordo com os dados simlados
media_condicional <- arima(arma10arch1_sim, order = c(1,0,0), method = "ML")

# Verificar qual distribuição de probabilidade melhor se assemelha aos resíduos da média condicional
# Este é um passo importante para o restante da análise. Precisamos garantir que distribuição de 
# probabilidade usada no processo de estimação por meio de máxima verossimilhança faça uso da correta
# distribuição. Assim, comparamos graficamente os resíduos obtidos pela estimação da média condicional
# com duas distribuições de probabilidade (Normal e t-Student). A comparação feita aqui não considera
# assimetria e em função disso, caso você perceba a existência de assimetria, você deve escolher a 
# distribuição que mais se assemelha aos dados, mas optar pela sua versão com assimetria no momento
# que for estimar o modelo arma-garch conjuntamente. Como resultado, temos que a distribuição t-Student
# é a melhor escolha. 

symmetric_normal <- stats::density(stats::rnorm(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
                                               sd = sd(media_condicional$residuals)))

symmetric_student <- stats::density(fGarch::rstd(length(media_condicional$residuals), mean = mean(media_condicional$residuals), 
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
title("FAC do quadrado dos resíduos do ARMA(1,0)", adj = 0.5, line = 1)

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

# Passo 1: Identificar a ordem m do arch. Para tanto, usamos a função de autocorrelação parcial (FACP) 
# que confirma que a defasagem ARCH é igual a 1.
pacf_residuals <- stats::pacf(media_condicional$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos Resíduos do ARMA(1,0)", adj = 0.5, line = 1)

# Passo 2: Modelar conjuntamente a média condicional e a variância condicional
# Aqui, usamos a função garchFit do pacote fGarch que tem as seguintes opções:
# - formula: a especificação a ser estimada arma(p,q)~garch(m,n)
# - data: a série temporal dos retornos
# - cond.dist: a distribuição de probabilidade assumida para o termo de erro da
# média condicional (norm: normal, std: t-student, snorm: normal assimétrica, 
# sstd: t-student assimétrica)
# - include.mean: se a média da equação da média condicional deve ser estimada ou não
# - include.skew: se o parâmetro de assimetria da distribuição de probabilidade assumida
# para o termo de erro da média condicional deve ser estimado ou não
# - include.shape: se o parâmetro da forma da distribuição de probabilidade assumida para
# o termo de erro da média condicional deve ser estimado ou não 
# - leverage: o modelo assume que há clusters de volatilidade ou não
# - trace: mostrar o processo de estimação na tela ou não
# - outras opções: use help("garchFit") e tenha todos os parâmetros possíveis
media_variancia_condicional <- fGarch::garchFit(~arma(1,0)+garch(1,0), data = arma10arch1_sim, trace = FALSE, cond.dist = "std")

# Parâmetros estimados. Aqui, usamos a função stargazer do pacote stargazer para 
# mostrar os resultados em um formato textual mais amigável para interpretação.
# Mais detalhes? Use help("stargazer")
stargazer::stargazer(media_variancia_condicional, type = "text", title = "Resultado Estimação modelo ARMA(1,0)-ARCH(1)")

####
##  4: Avaliar o modelo estimado
####

# Verificando se os resíduos ao quadrado ainda continuam com heterocedasticidade condicional
acf_residuals_armagarch <- acf(media_variancia_condicional@residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_armagarch, main = "", ylab = "", xlab = "Defasagem")
title("FAC dos resíduos do arma(1,0)-ARCH(1)", adj = 0.5, line = 1)

####
##  5: Gráficos do modelo
####

plot(media_variancia_condicional)



##################################
####   SIMULAÇÃO ARMA-ARCH    ####
##################################

#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

#####
##   SIMULAR DADOS PARA PROCESSO ARMA(2,0)-GARCH(1,1)
#####

# Fixar a raiz para os dados gerados na simulação serem iguais em qualquer computador
set.seed(123)

# Especificação para as funções da média e variância condicional:
# - mu: valor do parâmetro da média incodicional da função da média condicional
# - ar: valor do parâmetro da parte autorregressiva da média condicional 
# - ma: valor do parâmetro da parte de médias móveis da média condicional
# - omega: valor do parâmetro do intercepto da variância condicional
# - alpha: valor do parâmetro da parte arch da variância condicional
# - beta: valor do parâmetro da parte garch da variância condicional
# - cond.dist: a distribuição de probabilidade assumida para at que pode ser:
# norm: normal, std: t-student, snorm: normal assimétrica, sstd: t-student assimétrica
# - skew: parâmetro da assimetria da distribuição de probabilidade assumida para 
# o termo de erro da média condicional (at)
# - shape: parâmetro da forma da distribuição de probabilidade assumida para o 
# termo de erro da média condicional (at)
arma20garch11_spec <- fGarch::garchSpec(model = list(mu = 0.04, ar = c(0.28,0.15), 
                                                     omega = 1.2, alpha = 0.4, beta = 0.2), 
                                        cond.dist = "snorm")

# Simulação da série temporal dos retornos
# - spec: a especificação criada na etapa anterior
# - n: o tamanho da série temporal de interesse
arma20garch11_sim <- fGarch::garchSim(spec = arma20garch11_spec, n = 1000)

# obter os preços do ativo a partir dos retornos
precos <- rep(0, length(arma20garch11_sim))
precos[1] <- 10

for (i in 2:length(precos)) {
  precos[i] <- exp(arma20garch11_sim[i]/100)*precos[i-1]
}

#####
##   GRÁFICO DA SÉRIE TEMPORAL DOS RETORNOS SIMULADA BEM COMO DOS PREÇOS
#####

plot.ts(precos, main = "Preços Simulados: ARMA(2,0)-GARCH(1,1)", xlab = "tempo", ylab = "preços")
plot.ts(arma20garch11_sim, main = "Retornos Simulados: ARMA(2,0)-GARCH(1,1)", xlab = "tempo", ylab = "retorno")

##################################
####   ESTIMAÇÃO ARMA-ARCH    ####
##################################

####
##  1: Especificar uma equação para a média condicional 
####

# Trata-se de aplicar o processo de estimação de modelos ARMA(p,q) já estudado:
# - detalhes no arquivo simulacaoarma.R disponível na pasta 4.ARMA deste projeto

# Função de autocorrelação
acf_arma <- stats::acf(arma20garch11_sim, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação. 
plot(acf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC)", adj = 0.5, line = 1)

# Função de autocorrelação parcial
pacf_arma <- stats::pacf(arma20garch11_sim, na.action = na.pass, plot = FALSE, lag.max = 15)

# Gráfico da função de autocorrelação parcial. 
plot(pacf_arma, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação Parcial (FACP)", adj = 0.5, line = 1)

# Todas as combinações possíveis de p=0 até p=max e q=0 até q=max
pars <- expand.grid(ar = 0:2, diff = 0, ma = 0:3)

# Local onde os resultados de cada modelo será armazenado
modelo <- list()

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars)) {
  modelo[[i]] <- arima(arma20garch11_sim, order = unlist(pars[i, 1:3]), method = "ML")
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
quant_paramentros <- list()
for (i in 1:length(modelo)) {
  quant_paramentros[[i]] <- length(modelo[[i]]$coef)+1 # +1 porque temos a variância do termo de erro 
}

# Montar a tabela com os resultados
especificacao <- paste0("arma",pars$ar,pars$diff,pars$ma)
tamanho_amostra <- rep(length(arma20garch11_sim), length(modelo))
resultado_arma <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca),
                            quant_paramentros = unlist(quant_paramentros),
                            tamanho_amostra, aic = unlist(aicarma), 
                            bic = unlist(bicarma), stringsAsFactors = FALSE)

# Escolher o melhor modelo
which.min(resultado_arma$aic)
which.min(resultado_arma$bic)

# Mostrar a tabela de resultado
print(resultado_arma)

# Como resultado temos que o modelo escolhido tanto pelo AIC quanto pelo BIC é o ARMA(2,0)
# o que está de acordo com os dados simlados
media_condicional <- arima(arma20garch11_sim, order = c(2,0,0), method = "ML")

# Parâmetros estimados
stargazer::stargazer(media_condicional, type = "text", title = "Resultado Estimação modelo ARMA(2,0)")

####
##  2: Testar existência de efeito ARCH
####

# Opção 1: visualizar a FAC dos resíduos aos quadrado (lembre-se de que eles 
# são uma proxy para o retorno ao quadrado). Como resultado temos que há defasagens
# estatísticamente significantes (acima da linha pontilhada). O gráfico da FAC com
# a linha pontilhada é apenas uma forma visual de verificar o teste Ljung-Box que
# analisa se a autocorrelação é estatísticamente diferente de zero.
acf_residuals <- acf(media_condicional$residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(2,0)", adj = 0.5, line = 1)


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

# Passo 1: Identificar as ordens máximas M e N do arch. Para tanto, usamos a função de autocorrelação parcial (FACP)
acf_residuals <- acf(media_condicional$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARMA(2,0)", adj = 0.5, line = 1)

pacf_residuals <- stats::pacf(media_condicional$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 25)
plot(pacf_residuals, main = "", ylab = "", xlab = "Defasagem")
title("FACP do quadrado dos resíduos do ARMA(2,0)", adj = 0.5, line = 1)

# Passo 2: Modelar conjuntamente a média condicional e a variância condicional

# Todas as combinações possíveis de m=1 até m=max e n=0 até n=max
pars_arma_garch <- expand.grid(m = 1:2, n = 0:2)

# Local onde os resultados de cada modelo será armazenado
modelo_arma_garch <- list()

# Especificação arma encontrada na estimação da média condicional
arma_set <- "~arma(2,0)"

# Estimar os parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars_arma_garch)) {
    modelo_arma_garch[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                              data = arma20garch11_sim, trace = FALSE, cond.dist = 'snorm',
                                              include.skew = FALSE, include.shape = FALSE) 
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
tamanho_amostra <- rep(length(arma20garch11_sim), length(modelo_arma_garch))
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
media_variancia_condicional <- fGarch::garchFit(~arma(2,0)+garch(1,1), data = arma20garch11_sim, trace = FALSE, 
                                                cond.dist = "snorm", include.skew = FALSE,
                                                include.shape = FALSE)

# Parâmetros estimados. Aqui, usamos a função stargazer do pacote stargazer para 
# mostrar os resultados em um formato textual mais amigável para interpretação.
# Mais detalhes? Use help("stargazer")
stargazer::stargazer(media_variancia_condicional, type = "text", title = "Resultado Estimação modelo ARMA(2,0)-GARCH(1,1)")

####
##  4: Avaliar o modelo estimado
####

# Verificando se os resíduos ao quadrado ainda continuam com heterocedasticidade condicional
acf_residuals_armagarch <- acf(media_variancia_condicional@residuals, na.action = na.pass, plot = FALSE, lag.max = 20)
plot(acf_residuals_armagarch, main = "", ylab = "", xlab = "Defasagem")
title("FAC dos resíduos do arma(2,0)-GARCH(1,1)", adj = 0.5, line = 1)

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


##################################
####   SIMULAÇÃO ARMA-ARCH    ####
##################################


#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

#####
##   GERAÇÃO DAS SÉRIES ARMA - GARCH
#####

# Fixar a raiz para os dados gerados na simulação serem iguais em qualquer computador
set.seed(123)

# Especificação para as funções da média e variância condicional:
# - mu: valor do parâmetro da média incodicional da função da média condicional
# - ar: valor do parâmetro da parte autorregressiva da média condicional 
# - ma: valor do parâmetro da parte de médias móveis da média condicional
# - omega: valor do parâmetro do intercepto da variância condicional
# - alpha: valor do parâmetro da parte arch da variância condicional
# - beta: valor do parâmetro da parte garch da variância condicional
# - cond.dist: a distribuição de probabilidade assumida para at que pode ser:
# norm: normal, std: t-student, snorm: normal assimétrica, sstd: t-student assimétrica
# - skew: parâmetro da assimetria da distribuição de probabilidade assumida para 
# o termo de erro da média condicional (at)
# - shape: parâmetro da forma da distribuição de probabilidade assumida para o 
# termo de erro da média condicional (at)

# ARCH(0)
spec = garchSpec(model = list(omega = 1, alpha = 0.0, beta = 0.0))
# ARCH(1)
spec = garchSpec(model = list(omega = 1, alpha = 0.8, beta = 0.0))
# ARCH(3)
spec = garchSpec(model = list(omega = 1, alpha = c(0.3, 0.3, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0), beta = 0.0))
# ARCH(8)
spec = garchSpec(model = list(omega = 1, alpha = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), beta = 0.0))
# AR(1)- ARCH(8)
spec = garchSpec(model = list(ar = 0.5, omega = 1, alpha = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), beta = 0.0))
# GARCH(1,1) - specify omega/alpha/beta
spec = garchSpec(model = list(omega = 1, alpha = 0.1, beta = 0.8))
# AR(1)-GARCH(1,1) - specify ar/omega/alpha/beta
spec = garchSpec(model = list(ar = 0.5, omega = 1, alpha = 0.1, beta = 0.8))
# ARMA(1,1)-GARCH(1,1) - specify ar/ma/omega/alpha/beta
spec = garchSpec(model = list(ar = 0.6, ma = 0.3, omega = 1, alpha = 0.4, beta = 0.3))
# ARMA(3,2)-GARCH(2,3) - specify ar/ma/omega/alpha/beta
spec = garchSpec(model = list(ar = c(0.05, 0.10, 0.15), ma = c(0.10, 0.20), omega = 1, alpha = c(0.05, 0.10), beta = c(0.10, 0.10, 0.20)))
# APARCH(1,1) - specify omega/alpha/beta/gamma/delta
spec = garchSpec(model = list(omega = 1, alpha = 0.2, beta = 0.20, gamma = 0.5, delta = 1.8))

#####
##   COMPORTAMENTO DA SÉRIE SIMULADA
#####
x = garchSim(spec, n = 1000)
summary(x)
plot.ts (x, col = "blue")
acfPlot (x)
pacfPlot (x) 
# Teste de autocorrelação da série
#  - H0: série não autocorrelacionada
#  - H1: série é autocorrelacionada
Box.test(x,type="Ljung",lag=12)
plot.ts (x^2, col = "blue")
acfPlot (x^2)
pacfPlot (x^2)
# Teste de heterocedasticidade condicional
#  - H0: quadrado da série não autocorrelacionada
#  - H1: quadrado da série é autocorrelacionada
Box.test(x^2,type="Ljung",lag=12)
# Teste de Normalidade da série. 
#  - H0: série normalmente distribuída
#  - H1: série não é normalmente distribuída
normalTest(x, method = "ks")
normalTest(x, method = "sw")
normalTest(x, method = "jb")

#####
##   VISULALIZAÇÃO DE DISTRIBUIÇÕES DE PROBABILIDADE
#####

#gedSlider(type = c("dist", "rand"))
#sgedSlider(type = c("dist", "rand"))
#snormSlider(type = c("dist", "rand"))
#stdSlider(type = c("dist", "rand"))
#sstdSlider(type = c("dist", "rand"))

#####
##   ESTIMAÇÃO DO MODELO ARMA - GARCH
#####

# ARCH(1)
fit 	= garchFit( ~ garch(1, 0), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# ARCH(3)
fit 	= garchFit( ~ garch(3, 0), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# ARCH(8)
fit 	= garchFit( ~ garch(8, 0), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# GARCH(1,1) - specify omega/alpha/beta
fit 	= garchFit( ~ garch(1, 1), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# AR(1)-GARCH(1,1) - specify ar/omega/alpha/beta
fit 	= garchFit( ~ arma(1, 0) + garch(1, 1), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# ARMA(1,1)-GARCH(1,1) - specify ar/ma/omega/alpha/beta
fit 	= garchFit( ~ arma(1, 1) + garch(1, 1), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# APARCH(1,1) - specify omega/alpha/beta
fit 	= garchFit( ~ aparch(1, 1), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))
# ARMA(1,1)-APARCH(1,1) - specify ar/ma/omega/alpha/beta
fit 	= garchFit( ~ arma(1, 1) + garch(1, 1), data = x, cond.dist = c("norm"), 
                 algorithm = c("nlminb+nm"))

formula(fit)
summary(fit)
show(fit)
vol_sd = volatility(fit, type = "sigma") # desvio padrão
plot.ts(vol_sd, col = "blue")
vol_var = volatility(fit, type = "h") # variância
plot.ts(vol_var, col = "blue")

res = residuals(fit, standardize = FALSE)
summary(res)
plot.ts (res, col = "blue")
acfPlot (res)
pacfPlot (res) 
plot.ts (res^2, col = "blue")
acfPlot (res^2)
pacfPlot (res^2)
normalTest(res, method = "ks")
normalTest(res, method = "sw")
normalTest(res, method = "jb")

res = residuals(fit, standardize = TRUE)
summary(res)
plot.ts (res, col = "blue")
acfPlot (res)
pacfPlot (res) 
plot.ts (res^2, col = "blue")
acfPlot (res^2)
pacfPlot (res^2)
normalTest(res, method = "ks")
normalTest(res, method = "sw")
normalTest(res, method = "jb")

plot.ts(x^2, type = "l", lty = 1, col = "blue")
lines(vol_var, type = "l", lty = 1, col = "red")
legend("topright", legend = c("Real", "Ajustado"), col = c(2,1), lty = c(1,1))

mu = fit@fit$params$params[1]
erro = (x-mu)/vol_sd 
summary(erro)
plot.ts (erro, col = "blue")
acfPlot (erro)
pacfPlot (erro) 
plot.ts (erro^2, col = "blue")
acfPlot (erro^2)
pacfPlot (erro^2)
normalTest(erro, method = "ks")
normalTest(erro, method = "sw")
normalTest(erro, method = "jb")

plot (fit) 		     # 0 - Exit
plot (fit, which = 1)  # 1 - Time SeriesPlot
plot (fit, which = 2)  # 2 - Conditional Standard Deviation Plot
plot (fit, which = 3)  # 3 - Series Plot with 2 Conditional SD Superimposed
plot (fit, which = 4)  # 4 - Autocorrelation function Plot of Observations
plot (fit, which = 5)  # 5 - Autocorrelation function Plot of Squared Observations
plot (fit, which = 7)  # 7 - Residuals Plot
plot (fit, which = 8)  # 8 - Conditional Standard Deviations Plot
plot (fit, which = 9)  # 9 - Standardized Residuals Plot
plot (fit, which = 10) # 10 - ACF Plot of Standardized Residuals 
plot (fit, which = 11) # 11 - ACF Plot of Squared Standardized Residuals
plot (fit, which = 13) # 13 - Quantile-Quantile Plot of Standardized Residuals

fGarch::predict(fit, n.ahead = 3)


#####
##   ESTIMAÇÃO DO MODELO PARA A AMBEV3 (ABEV3.SA)
#####

# Dados da ação AMBEV3.SA desde 01/01/2017
price_day <- quantmod::getSymbols("ABEV3.SA", src = "yahoo", from = '2015-01-01')
log_day_return <- na.omit(PerformanceAnalytics::Return.calculate(ABEV3.SA$ABEV3.SA.Close, method = "log"))
log_day_return_squad <- log_day_return^2

# Gráfico dos preços e retonos (observer a presença de heterocedasticidade condicional)
plot.xts(ABEV3.SA$ABEV3.SA.Close, main = "Preços da AMBEV3", xlab = "tempo", ylab = "preços")
plot.xts(log_day_return, main = "Retornos da AMBEV3", xlab = "tempo", ylab = "retorno")


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

# Estimação dos parâmetros dos modelos usando Máxima Verossimilhança (ML)
for (i in 1:nrow(pars_arma_garch)) {
  modelo_arma_garch[[i]] <- fGarch::garchFit(as.formula(paste0(arma_set,"+","garch(",pars_arma_garch[i,1],",",pars_arma_garch[i,2], ")")),
                                             data = log_day_return, trace = FALSE, cond.dist = arma_residuals_dist,
                                             include.skew = include.skew, include.shape = include.shape) 
}

# Obtenção do logaritmo da verossimilhança (valor máximo da função)
log_verossimilhanca_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  log_verossimilhanca_arma_garch[[i]] <- modelo_arma_garch[[i]]@fit$llh
}

# Calculo do AIC
aicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  aicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[1]
}

# Calcular do BIC
bicarma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  bicarma_garch[[i]] <- modelo_arma_garch[[i]]@fit$ics[2]
}

# Quantidade de parâmetros estimados por modelo
quant_paramentros_arma_garch <- list()
for (i in 1:length(modelo_arma_garch)) {
  quant_paramentros_arma_garch[[i]] <- length(modelo_arma_garch[[i]]@fit$coef)
}

# Montagem da tabela com os resultados
especificacao <- paste0(arma_set,"-","garch",pars_arma_garch$m,pars_arma_garch$n)
tamanho_amostra <- rep(length(log_day_return), length(modelo_arma_garch))
resultado_arma_garch <- data.frame(especificacao, ln_verossimilhanca = unlist(log_verossimilhanca_arma_garch),
                                   quant_paramentros = unlist(quant_paramentros_arma_garch),
                                   tamanho_amostra, aic = unlist(aicarma_garch), bic = unlist(bicarma_garch),
                                   stringsAsFactors = FALSE)

# Escolha do modelo com menor AIC e/ou BIC
which.min(resultado_arma_garch$aic)
which.min(resultado_arma_garch$bic)

# Apresentação dos resultados
print(resultado_arma_garch)

# Estimação do modelo escolhido
fit <- fGarch::garchFit(~arma(0,0)+garch(1,1), data = log_day_return, trace = FALSE, 
                                                cond.dist = "std", include.skew = include.skew,
                                                include.shape = include.shape)

# Análise dos resíduos não padronizados
res = residuals(fit, standardize = FALSE)
summary(res)
plot.ts (res, col = "blue")
acfPlot (res)
pacfPlot (res) 
Box.test(res,type="Ljung",lag=12)
plot.ts (res^2, col = "blue")
acfPlot (res^2)
pacfPlot (res^2)
Box.test(res^2,type="Ljung",lag=12)
normalTest(res, method = "jb")

# Análise dos resíduos padronizados
res = residuals(fit, standardize = TRUE)
summary(res)
plot.ts (res, col = "blue")
acfPlot (res)
pacfPlot (res) 
Box.test(res,type="Ljung",lag=12)
plot.ts (res^2, col = "blue")
acfPlot (res^2)
pacfPlot (res^2)
Box.test(res^2,type="Ljung",lag=12)
normalTest(res, method = "jb")

# Previsão do retorno esperado e variância esperada
fGarch::predict(fit, n.ahead = 3)
