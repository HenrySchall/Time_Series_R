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
