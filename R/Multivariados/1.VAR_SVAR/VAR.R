##############################################################

# Modelo VAR - Luiza

##############################################################

# Instalando e Carregando Pacotes

install.packages("fBasics")
install.packages("quantmod")
install.packages("Quandl")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("dygraphs")
install.packages("tseries")
install.packages("vars")
install.packages("readxl")
install.packages("xts")
install.packages("urca")
install.packages("fUnitRoots")
install.packages("dplyr")
install.packages("MTS")


library("fBasics")
library("quantmod")
library("Quandl")
library("ggplot2")
library("lmtest")
library("dygraphs")
library("tseries")
library("vars")
library("readxl")
library("xts")
library("urca")
library("dplyr")
library("MTS")


##############################################################

# 1º passo: Carregar a base de dados:

dados <- read_excel("C:/Users/Acer/Desktop/Luiza/Ibmec/8º período/TCC/Bases de dados/Base_Luiza_FINAL.xls")

dados <- read_excel("C:/Users/henri/OneDrive/Repositórios/Base_Luiza_FINAL.xls")

Risco <- ts(dados$Risco,frequency = 12, start = c(2002,1))
Divida <- ts(dados$Divida,frequency = 12, start = c(2002,1))
UIP <- ts(dados$UIP,frequency = 12, start = c(2002,1))
Investimentos <- ts(dados$Investimentos,frequency = 12, start = c(2002,1))

dset <- data.frame(Risco, Divida, UIP, Investimentos)

# analisando características dos dados
glimpse(dados)

# gráficos de correlação
ggplot(data = dados, aes(y = Risco, x = Divida)) + geom_point()
ggplot(data = dados, aes(y = Divida, x = Investimentos)) + geom_point()
ggplot(data = dados, aes(y = Cambio, x = Selic)) + geom_point()
ggplot(data = dados, aes(y = Cambio, x = Divida)) + geom_point()

summary(dset)

# 2º Passo: Testar se os dados são estacionários
# Se todos os componentes da série são estacionários, o modelo VAR em nível se aplica;
# Se temos componentes não estacionários, contamos com duas alternativas:
# Se eles são não estacionários e não cointegrados, deve-se ajustar o VAR em primeiras diferenças
# Se eles são não estacionários, mas cointegrados, deve-se ajustar o Vetor de Correção de Erros (VEC)

# Testando a estacionaridade:

adf_Risco <- suppressWarnings(fUnitRoots::adfTest(Risco,lags=3, type=c("nc")))
adf_Divida <- suppressWarnings(fUnitRoots::adfTest(Divida,lags=3, type=c("nc")))
adf_UIP <- suppressWarnings(fUnitRoots::adfTest(UIP,lags=3, type=c("nc")))
adf_Investimentos <- suppressWarnings(fUnitRoots::adfTest(Investimentos,lags=3, type=c("nc")))

adf_Risco <- c(adf_Risco@test$statistic, adf_Risco@test$p.value)
adf_Divida <- c(adf_Divida@test$statistic, adf_Divida@test$p.value)
adf_UIP <- c(adf_UIP@test$statistic, adf_UIP@test$p.value)
adf_Investimentos <- c(adf_Investimentos@test$statistic, adf_Investimentos@test$p.value)

resultADF <- cbind(adf_Risco, adf_Divida, adf_UIP, adf_Investimentos)
colnames(resultADF) <- c("Risco", "Divida", "UIP", "Investimentos")
rownames(resultADF) <- c("Estatística do Teste ADF", "p-valor")
print(resultADF)

# Teste Dickey-Fuller Aumentado
# Resultados: (p-valor) - p-valor < 0,05 = Rejeito a HO (Série é Estácionaria)

# Risco = 0.04177217 - rejeita HO          
# Divida = 0.6635235 - não rejeita H0           
# UIP = 0.07143591 - não rejeita H0
# Investimentos = 0.08832058 não rejeita H0

# Como apenas um componente da série é estacionários, o modelo VAR em nível não se aplica.

# Então eles podem ser:
# Não estacionários e não cointegrados -> rodar modelo "VAR em primeiras diferenças"
# Não estacionários, mas cointegrados -> rodar modelo "Vetor de Correção de Erros (VEC)""

##############################################################

# 3º Passo: Selecionar os parâmetros do modelo VAR - ordem p
data <- window(ts.intersect(Risco, Divida, UIP, Investimentos))
colnames(data) <- c('Risco', 'Divida', 'UIP', 'Investimentos')
varorder <- vars::VARselect(y = data, lag.max = 4, type = "const")
print(varorder$selection)

##############################################################

# 4º passo: Testando cointegração - teste de Johansen
# H0: Não cointegração
# H1: Cointegração

# Johanse (Trace)
teste_joh <- ca.jo(dset, type = "trace", ecdet = "none", K = 2)
summary(teste_joh)

# Resultados
# r = 0 -> 84.58 > 48.28 -> rejeitamos H0
# r <= 1 -> 23.77 < 31,52 -> não rejeitamos H0
# r <= 2 -> 4.98 < 17.95 -> não rejeitamos H0
# r <= 3 -> 1.80 < 8.18 -> não reeitamos H0

# EXISTE APENAS 1 COINTEGRAÇÂO NO MODELO 
# Rodar modelo "VAR em primeiras diferenças"

# Estimando o VAR em diferenças - Diferenciando os dados

Risco_diff <- timeSeries::diff(Risco, lag = 1, differences = 1)
Divida_diff <- timeSeries::diff(Divida, lag = 1, differences = 1)
UIP_diff <- timeSeries::diff(UIP, lag = 1, differences = 1)
Investimentos_diff <- timeSeries::diff(Investimentos, lag = 1, differences = 1)

# Verificando se as séries ficaram estacionárias

adf_Risco_diff <- suppressWarnings(fUnitRoots::adfTest(Risco_diff,lags=3, type=c("nc")))
adf_Divida_diff <- suppressWarnings(fUnitRoots::adfTest(Divida_diff,lags=3, type=c("nc")))
adf_UIP_diff <- suppressWarnings(fUnitRoots::adfTest(UIP_diff,lags=3, type=c("nc")))
adf_Investimentos_diff <- suppressWarnings(fUnitRoots::adfTest(Investimentos_diff,lags=3, type=c("nc")))

adf_Risco_diff <- c(adf_Risco_diff@test$statistic, adf_Risco_diff@test$p.value)
adf_Divida_diff <- c(adf_Divida_diff@test$statistic, adf_Divida_diff@test$p.value)
adf_UIP_diff <- c(adf_UIP_diff@test$statistic, adf_UIP_diff@test$p.value)
adf_Investimentos_diff <- c(adf_Investimentos_diff@test$statistic, adf_Investimentos_diff@test$p.value)

resultADF <- cbind(adf_Risco_diff, adf_Divida_diff, adf_UIP_diff, adf_Investimentos_diff)
colnames(resultADF) <- c("Risco", "Divida", "UIP", "Investimentos")
rownames(resultADF) <- c("Estatística do Teste ADF", "p-valor")
print(resultADF)

# teste ADF -> séries ficaram estacionárias
# Rejeito a HO (Série é Estácionaria)

##############################################################

# 5º passo: Estimar o modelo com o número de parâmetros do passo 3

dados_diff <- data.frame(Risco_diff,Divida_diff,UIP_diff,Investimentos_diff)
modelo <- vars::VAR(y = dados_diff, p = 4, type = "const")
summary(modelo)


# 6º passo: Teste de causalidade de Granger
vars::causality(x = modelo, cause = "Risco_diff")$Granger
vars::causality(x = modelo, cause = "Divida_diff")$Granger
vars::causality(x = modelo, cause = "UIP_diff")$Granger
vars::causality(x = modelo, cause = "Investimentos_diff")$Granger

##############################################################

# 7º passo: Examinar se os resíduos se comportam como ruído branco e condições de estacionariedade do modelo

# Autocorrelação 
# H0: não há autocorrelação
# H1: há autocorrelação
serial.test(modelo,lags.pt=16,type = "PT.adjusted")

# Homoscedasticidade
# H0: Homoscedasticidade 
# H1: Heterocedasticidade
arch <- arch.test(modelo, lags.multi = 12, multivariate.only = FALSE)
print(arch)

# Normalidade
# H0: Resíduos seguem normalidade 
# H1: Resíduos não seguem normalidade

normality.test(modelo,multivariate.only = FALSE)

##############################################################

# 8º passo: Função Impulso

model1.irf <- vars::irf(x = modelo, impulse = "Risco_diff", response = "Investimentos_diff", n.ahead = 12, ortho = FALSE)
model2.irf <- vars::irf(x = modelo, impulse = "UIP_diff", response = "Investimentos_diff", n.ahead = 12, ortho = FALSE)
model3.irf <- vars::irf(x = modelo, impulse = "Divida_diff", response = "Investimentos_diff", n.ahead = 12, ortho = FALSE)

print(model1.irf)
print(model2.irf)
print(model3.irf)

##############################################################

# 9º passo: Previsão
fevd.model <- vars::fevd(x = modelo, n.ahead = 12)
print(fevd.model)

