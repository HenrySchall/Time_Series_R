###########################################
###  MODELO DE CORREÇÃO DE ERROS (MCE)  ###
###########################################

#####
##   PACOTES NECESSÁRIOS
#####

# instalar pacotes (executar apenas uma vez)
#install.packages(c("tidyquant", "tidyverse", "xtable", "stargazer", "highcharter", 
#                   "tseries", "htmltools", "vars", "MTS", "urca", "seasonal",
#                   "Quandl", "dlm"))

# devtools::install_git("https://github.com/ccolonescu/PoEdata")

# para tratar séries temporais
suppressMessages(require(tidyquant))

# para manusear dados
suppressMessages(require(tidyverse))

# para gerar gráficos e tabelas
suppressMessages(require(highcharter))
suppressMessages(require(xtable))
suppressMessages(require(htmltools))
suppressMessages(require(stargazer))

# para testar resíduos
suppressMessages(require(tseries))

# para coletar dados da internet
suppressMessages(require(quantmod))
suppressMessages(require(Quandl))

# para simular e estimar modelos VAR, VEC e MEC
suppressMessages(require(vars))
suppressMessages(require(MTS))
suppressMessages(require(urca))
suppressMessages(require(seasonal))
suppressMessages(require(dynlm))

# para usar dados do livro Principles of Econometrics with  R
suppressMessages(require(PoEdata))

#####
##   PROCESSO DE ESTIMAÇÃO
#####

# 1. Visualizar os dados e identificar observações fora do padrão (outliers, sazonalidade, tendência)
# 2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
# 3. Testar se as séries temporais são cointegradas:
#     * Testar se as séries são estacionárias. 
#         * Se não estacionárias estimar a relação entre as séries (modelo de regressão linear)
#         * Se estacionárias, não faz sentido testar cointegração e deve-se estimar modelo de regressão linear tradicional
#     * Obter os resíduos de cointegração a partir da regressão linear do passo anterior (caso raiz unitária)
#     * Testar se os resíduos são estacionários
#         * Se não estacionários implica que as séries não são cointegradas 
#         * Se estacionário implica que as séries são cointegradas
# 4. Estimar o modelo de correção de erros e obter a estimativa para os termos de curto e longo prazo
# 5. Avaliar os resíduos da estimação:
#     * Verificar a autocorrelação serial por meio da FAC e FACP dos resíduos do modelo estimado. O ideal é não ter
# defasagens significativas

###########################################
######      SIMULAR DUAS SÉRIES      ######
###########################################

# Semente para fazer o exemplo produzir os mesmos resultados 
# em computadores diferentes
set.seed(123)

# Repetir zero 1000 vezes para armazenar dados simulados de x
x = rep(0, 1000)

# Criar o passeio aleatório (Xt = Xt-1 + ERROt) onde o erro 
# é um ruído branco, conforme estudado em sala. Lembre-se
# que na definição em sala temos que supor a existência de
# um valor inicial para o passeio aleatório e a partir deste
# valor inicia-se a geração dos dados (aqui, assumimos que
# este valor é 0, observe que a iteração do for inicia em 2).
# A função rnorm gerará aleatóriamente um valor para uma Normal
# com média 0 e variância 1.
for (i in 2:1000) x[i] = x[i-1] + rnorm(1)

# Criar o passeio aleatório (Yt = 3*Xt-1), assim que é função de Xt
beta = 1.5
y = beta*x + rnorm(length(x))

# Salvar os dados em uma série temporal multivada usando
# a função cbind.zoo (juntar séries em colunas)
passeios = cbind.zoo(y = as.zoo(y), x = as.zoo(x))

#####
##   1. Visualizar os dados e identificar observações fora do padrão (outliers, sazonalidade, tendência).
#####

plot(passeios$y, type = "l", lty = 2, lwd = 1.5, xlab = "Período", ylab = "")
lines(passeios$x, type = "l", lty = 1, lwd = 0.5, xlab = "Período", ylab = "")
legend("topleft", legend = c("x", "y"), lty = 1:2)

#####
##   2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
#####

# Não faz sentido aplicar qualquer alteração nos dados, pois sabemos que são dados simulados

#####
##   3. Testar se as séries são cointegradas
#####

# Primeiro, testamos se as séries são estacionárias. Como geramos dois passeios aleatórios, é de se esperar 
# que os resultados do teste mostrem que ambas as séries são I(1), ou seja, tem raiz unitária

# Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária
# nas séries avaliadas. Como observamos no gráfico da série, não há tendência
# nos dados e assim o teste verificará se a série se comporta como um passeio aleatório
# sem drift. Isto é evidênciado por meio da opção type que tem as seguintes opções:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)
# Além disso, definimos que no máximo duas defasagens da série devem ser usadas como
# variáveis explicativas da regressão do teste.

unitRootX = fUnitRoots::adfTest(passeios$x,lags = 2, type = c("nc"))
print(unitRootX)

unitRootY = fUnitRoots::adfTest(passeios$y, lags = 2, type = c("nc"))
print(unitRootY)

# Como as duas séries são I(1), ou seja, têm raíz unitária estimamos a relação entre elas

# Estimação da Regressão Linear Simples via OLS. Aqui, usamos
# a função lm do pacote stats que tem as seguintes opções:
# - formula: modelo a ser ajustato (~ faz o papel de "=")
# - data: o cojunto de dados
# - weights: os pesos para regressão ponderada
# - subset: sub-conjunto dos dados
# - na.action: especificar o que fazer no cado de NA nos dados. Como
# padrão usa a função na.omit que exclui da base casos de NA
modelo = stats::lm(formula = y ~ x, data = passeios)

# Testar se os reíduos são estacionários. Observe que não faz sentido adicionar intercepto
# ou tendência neste teste, pois os resíduos de MQO oscilam em torno de zero.
unitRootResiduals = fUnitRoots::adfTest(modelo$residuals, lags = 2, type = c("nc"))
print(unitRootResiduals)

#####
##   4. Estimar o modelo de correção de erros e obter a estimativa para os termos de curto e longo prazo
#####

# Usando o processo de estimação em dois estágios de Engle-Granger

# a) Estimar a relação de cointegração de longo prazo
reg = lm(formula = y ~ x, data = passeios)

# b) Coletar os erros da estimação anterior
resid = as.zoo(reg$resid)

# c) Estimar a relação de curto prazo e longo prazo juntas (modelo de correção de erros). O
# coeficiente para os resíduos defasados indica quão rápido o ajustamento ocorre em um período. 
# Se o parâmetro for próximo de 0, o ajustamento ocorre lentamente enquanto que próximo de -1
# o ajustamento é rápido. 
mce = dynlm::dynlm(formula = d(y, 1) ~ -1 + d(x, 1) + L(resid, 1), data = passeios)
print(summary(mce))

#####
##   5. Avaliar os resíduos da estimação
#####

# Testar se os reíduos do MCE são estacionários. Observe que não faz sentido adicionar intercepto
# ou tendência neste teste, pois os resíduos de MQO oscilam em torno de zero.
unitRootResiduals_MCE = fUnitRoots::adfTest(mce$residuals, lags = 2, type = c("nc"))
print(unitRootResiduals_MCE)

# Calcular a FAC dos resíduos do modelo
acf_residuos_mce = acf(mce$residuals, plot = FALSE, na.action = na.pass, max.lag = 25)

# Gráfico da FAC
plot(acf_residuos_mce, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Resíduos do MCE", adj = 0.5, line = 1)

# Gráfico dos Resíduos
plot(mce$residuals, main = "Resíduos de MCE", type = "l", ylab = "", xlab = "")

