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
