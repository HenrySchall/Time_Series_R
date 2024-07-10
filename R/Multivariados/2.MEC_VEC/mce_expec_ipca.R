###########################################
###  MODELO DE CORREÇÃO DE ERROS (MCE)  ###
###########################################


#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

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
#     * Verificar se os resíduos são normalmente distribuíos
#     * Verificar a presença de heterocedasticidade condicional usando a FAC dos resíduos aos quadrado

###########################################
######            DADOS              ######
###########################################

# Aqui, usaremos dados sobre a expectativa de inflação para os próximos 12 meses
# coletada pelo Instituto Brasileiro de Economia (IBRE/FGV). Além disso, coletamos
# dados sobre o Índice de Preços ao Consumidor Amplo (IPCA/IBGE). Pretendemos estimar
# um modelo de correção de erro onde a expectativa de inflação é explicada pelo IPCA.
# Abaixo, códigos que pegam os dados das fontes.

dados <- read.csv2(paste0(getwd(),"/4.Serie_Temporal_Multivariada", "/2.VEC_SVEC/","expe_ipca_cons.csv"))
colnames(dados) <- c("data","expectativa","ipca","focus")

# Definir as duas séries temporais com as respectivas periodicidades
ipca <- ts(dados$ipca,start = c(2005,9),frequency = 12)
expectativa <- ts(dados$expectativa,start = c(2005,9),frequency = 12)


#####
##   1. Visualizar os dados e identificar observações fora do padrão (outliers, sazonalidade, tendência).
#####

plot(expectativa, type = "l", lty = 1, lwd = 0.5, xlab = "", ylab = "", ylim = c(0,10))
lines(ipca, type = "l", lty = 2, lwd = 1.5, xlab = "", ylab = "")
legend("bottomright", legend = c("Expectativa Consumidor", "IPCA"), lty = 1:2, cex = 0.7)

#####
##   2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
#####

# Não faz sentido aplicar qualquer alteração nos dados

#####
##   3. Testar se as séries são cointegradas
#####

# Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária
# nas séries avaliadas. Como observamos no gráfico da série, não há tendência
# nos dados e assim o teste verificará se a série se comporta como um passeio aleatório
# sem drift. Isto é evidênciado por meio da opção type que tem as seguintes opções:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)
# Além disso, definimos que no máximo duas defasagens da série devem ser usadas como
# variáveis explicativas da regressão do teste.

unitRoot_ipca <- suppressWarnings(fUnitRoots::adfTest(ipca,lags = 2, type = c("nc")))
print(unitRoot_ipca)

unitRoot_expectativa <- suppressWarnings(fUnitRoots::adfTest(expectativa, lags = 2, type = c("nc")))
print(unitRoot_expectativa)

# Como as duas séries são I(1), ou seja, têm raíz unitária estimamos a relação entre elas

# Estimação da Regressão Linear Simples via OLS. Aqui, usamos
# a função lm do pacote stats que tem as seguintes opções:
# - formula: modelo a ser ajustato (~ faz o papel de "=")
# - data: o cojunto de dados
# - weights: os pesos para regressão ponderada
# - subset: sub-conjunto dos dados
# - na.action: especificar o que fazer no cado de NA nos dados. Como
# padrão usa a função na.omit que exclui da base casos de NA
modelo <- stats::lm(formula = expectativa  ~ ipca, data = dados)

# Testar se os reíduos são estacionários. Observe que não faz sentido adicionar intercepto
# ou tendência neste teste, pois os resíduos de MQO oscilam em torno de zero.
unitRootResiduals <- suppressWarnings(fUnitRoots::adfTest(modelo$residuals, lags = 2, type = c("nc")))
print(unitRootResiduals)

#####
##   4. Estimar o modelo de correção de erros e obter a estimativa para os termos de curto e longo prazo
#####

# Usando o processo de estimação em dois estágios de Engle-Granger

# a) Estimar a relação de cointegração de longo prazo
reg <- lm(formula = expectativa ~ ipca, data = dados)
stargazer::stargazer(reg, type = "text")

# b) Coletar os erros da estimação anterior
resid <- as.zoo(reg$resid)

# c) Estimar a relação de curto prazo e longo prazo juntas (modelo de correção de erros). O
# coeficiente para os resíduos defasados indica quão rápido o ajustamento ocorre em um período. 
# Se o parâmetro for próximo de 0, o ajustamento ocorre lentamente enquanto que próximo de -1
# o ajustamento é rápido. 
mce <- dynlm(formula = d(expectativa, 1) ~ -1 + d(ipca, 1) + L(resid, 1), data = dados)
stargazer::stargazer(mce, type = "text")


#####
##   5. Avaliar os resíduos da estimação
#####

# Testar se os reíduos do MCE são estacionários. Observe que não faz sentido adicionar intercepto
# ou tendência neste teste, pois os resíduos de MQO oscilam em torno de zero.
unitRootResiduals_MCE <- suppressWarnings(fUnitRoots::adfTest(mce$residuals, lags = 2, type = c("nc")))
print(unitRootResiduals_MCE)

# Calcular a FAC dos resíduos do modelo
acf_residuos_mce <- acf(mce$residuals, plot = FALSE, na.action = na.pass, max.lag = 25)

# Gráfico da FAC
plot(acf_residuos_mce, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Resíduos do MCE", adj = 0.5, line = 1)

# Gráfico dos Resíduos
plot(mce$residuals, main = "Resíduos de MCE", type = "l", ylab = "", xlab = "")

# FAC do quadrado dos resíduos
acf_square_residuals <- acf(mce$residuals^2, plot = FALSE, na.action = na.pass, max.lag = 25)

# Gráfico da FAC do quadrado dos resíduos
plot(acf_square_residuals, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) do Quadrado dos Resíduos do MCE", adj = 0.5, line = 1)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos
#  - H1: resíduos não são normalmente distribuídos
shapiro_test <- stats::shapiro.test(na.remove(mce$residuals))
jarque_bera <- tseries::jarque.bera.test(na.remove(mce$residuals))
