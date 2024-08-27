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


