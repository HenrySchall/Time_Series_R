###########################################
###  MODELO VETORIAL AUTORREGRESSIVO    ###
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
# 3. Avaliar a função de correlação cruzada para confirmar a possibilidade de modelagem multivariada.
# 4. Testar se os dados são estacionários ou cointegrados: 
#     * Caso não tenha raiz unitária (estacionários), estimar VAR com as séries em nível
#     * Caso tenha raiz unitária, mas sem cointegração é preciso diferenciar os dados até se tornarem estacionários e estimar VAR com as séries diferenciadas
#     * Caso tenha raiz unitária, mas com cointegração devemos estimar o VEC com as séries em nível
# 5. Definir a ordem $p$ para os dados em análise por meio de critérios de informação (escolher modelo com menor AIC, por exemplo)
# 6. Estimar o modelo escolhido no passo 4
#     * Se VAR (forma reduzida): 
#        - Verificar significância estatística do modelo estimado e, caso seja necessário, eliminar parâmetros não significantes.
#        - Analisar a causalidade de Granger (variáveis que não granger causa as demais podem ser retiradas do modelo)
#     * Se SVAR (forma estrutural):
#        - Definir a estrutura para as matrizes A e B e o modelo de interesse (A, B ou AB)
#        - Verificar significância estatística do modelo estimado e, caso seja necessário, eliminar parâmetros não significantes.
#        - Analisar a causalidade de Granger (variáveis que não granger causa as demais podem ser retiradas do modelo)
#     * Se VEC (Modelo Vetorial de Correção de Erros)
#        - Usar a quantidade de vetores de cointegração obtidos no teste de cointegração para estimar o modelo VEC
# 8. Examinar se os resíduos se comportam como ruído branco e condições de estacionariedade do modelo. Caso contrário, retornar ao passo 3 ou 4.
#     * Verificar a autocorrelação serial por meio da FAC e FACP dos resíduos de cada equação do modelo estimado. O ideal é não ter defasagens significativas.
#     * Verificar correlação cruzada por meio da FCC dos resíduos.
#     * Analisar a estabildiade do modelo estimado através dos autovalores associados ao mesmo.
#     * Verificar a distribuição de probabilidade (Normal) para os resíduos de cada equação do modelo.
#     * Analisar heterocedasticidade condicional (resíduos devem ser homocedasticos, ou seja, variância condicional constante)
# 9. Uma vez que os resíduos são ruído branco e o modelo é estável:
#     * Analisar funções de resposta ao impulso
#     * Analisar a importância das variáveis para explicar a variância do erro de previsão de cada variável
#     * Fazer previsões paras as variáveis do modelo

###########################################
######           VAR SIMULADO        ######
###########################################

# Definir a raiz para o algoritmo que gera os números aleatórios. É necessário
# para gerar os mesmos números aleatórios diversas vezes. Mais detalhes em 
# https://en.wikipedia.org/wiki/Pseudorandom_number_generator
set.seed(123)

# Número de observações da série temporal multivariada
nobs <- 1000

# Defasagem do modelo VAR
arlags <- 1

# Vetor de parâmetros phio
phio <- c(0.5, 1)

# Matriz de parâmetros Phi
phi <- matrix(data = c(0.7,0.4,0.2,0.3), ncol = 2, byrow = TRUE)

# Matriz com variâncias e covariâncias do vetor de erro (at)
sigma <- matrix(data = c(2,0,0,1), ncol = 2, byrow = TRUE)

# Gerar a série temporal multivariada usando os parâmetros anteriores. 
# Vamos usar a função VARMAsim do pacote MTS que tem as seguintes opções:
# nobs: quantidade de dados que serão simulados
# arlags: defasagens do VAR
# cnst: vetor de constantes 
# phi: matriz com os coeficientes do VAR
# sigma: matriz de covariância dos erros
vardata <- MTS::VARMAsim(nobs = nobs, arlags = arlags, 
                        cnst = phio, phi = phi, sigma = sigma)

# Transformar os dados em uma série temporal
vardata <- cbind(as.ts(vardata$series))

# Renomear as colunas 
colnames(vardata) = c("r1","r2")

#####
##   1. Visualizar os dados e identificar observações fora do padrão (outliers, sazonalidade, tendência).
#####

# Gráfico da série temporal multivariada. O gráfico mostra que não há distorções incoerentes nas duas séries. 
# Pelo contrário, elas se comportam semelhantemente durante o período. Desta forma, não há necessidade de 
# qualquer alteração nos dados (lembre-se que o VAR demanda estacionariedade e como as séries foram geradas
# assumindo um modelo VAR, não faz sentido que elas não sejam estacionárias)
par(mfrow = c(1, 1))
ts.plot(vardata, gpars=list(xlab = "", ylab = "", lty = c(1,5), col = 1:2, main = "Série Temporal Multivariada"))
legend("topleft", legend = c("r1","r2"), col = 1:2, lty = c(1,4))


#####
##   2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
#####

# Como comentado anteriormente, não faz sentido aplicar qualquer alteração nos dados

#####
##  3. Avaliar a função de correlação cruzada para confirmar a possibilidade de modelagem multivariada.
#####

# Defasagens máximas
defasagens <- 10

# Montar uma "matriz" de gráficos (kxk)
par(mfrow = c(ncol(vardata), ncol(vardata)))

# Adicionar na "matriz" de gráficos a CCF de cada série
# contra ela mesma e as demais
for (i in 1:ncol(vardata)) {
  for (j in 1:ncol(vardata)) {
    ccf(drop(vardata[,i]), drop(vardata[,j]), lag.max = defasagens, 
        main = "", ylab = "FCC", xlab = "Defasagens")
    title(paste0(colnames(vardata)[i], "-", colnames(vardata)[j]), adj = 0.5, line = 1)
  }
}

# Os gráficos mostram a dependência linear entre as duas séries temporais, conforme o esperado 

#####
##  4. Testar se os dados são estacionários. Caso tenha raiz unitária é preciso diferenciar os dados até se tornarem estacionários
#####

# Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária
# na séries temporais avaliadas. Como observamos no gráfico da série, não há tendência
# nos dados e assim o teste verificará se a série se comporta como um passeio aleatório
# sem drift. Isto é evidênciado por meio da opção type que tem as seguintes opções:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)
# Além disso, definimos que no máximo duas defasagens da série devem ser usadas como
# variáveis explicativas da regressão do teste.
unitRootr1 <- suppressWarnings(fUnitRoots::adfTest(vardata[,1], lags = 2, type = c("nc")))
unitRootr2 <- suppressWarnings(fUnitRoots::adfTest(vardata[,2], lags = 2, type = c("nc")))


# Tabela com os resultados do teste que confirmam a estacionariedade das séries
adfr1 <- c(unitRootr1@test$statistic, unitRootr1@test$p.value)
adfr2 <- c(unitRootr2@test$statistic, unitRootr2@test$p.value)
resultADF = cbind(adfr1, adfr2)
colnames(resultADF) = c("r1", "r2")
rownames(resultADF) = c("Estatística do Teste ADF", "p-valor")
print(resultADF)

#####
##  5. Definir a ordem $p$ para os dados em análise por meio de critérios de informação (escolher modelo com menor AIC, por exemplo)
#####

# Usamos a função VARselect do pacote vars que tem as seguintes opções:
# - y: dados do modelo
# - lag.max: quantidade máxima de defasagens avaliadas. Aqui, lembre-se que
# para cada defasagem um modelo VAR será estimado 
# - type: quais parâmetros determinísticos queremos incluir no modelo. Eles podem ser:
#      - "const" para uma constante nas equações
#      - "trend" uma tendência nas equações
#      - "both" para tando constante quanto tendência
#      - "none": nenhum dos dois (apenas parâmetros das defasagens)
# - season: dummies sazonais caso os dados apresentem sazonalidade
# - exogen: variáveis exógenas do modelo
varorder <- vars::VARselect(y = vardata, lag.max = 6, type = "const")
print(varorder$selection)

# Os resultados mostram que para todos os critérios de informação concluímos que trata-se de um modelo VAR(1) 
# conforme simulação (valor 1 na primeira tabela do resultado). A defasagem ótima é sempre aquela para a qual
# o critério apresenta o menor valor. Assim, podemos observar que o uso destes critérios podem ajudar na 
# especificação da ordem p do modelo VAR.

#####
##  6. Estimar o modelo escolhido no passo anterior.
#####

# Usamos a função VAR do pacote vars que tem as seguintes opções 
# além das opções já apresentadas em VARselect:
# - p: número de defasagens do modelo
modelo <- vars::VAR(y = vardata, p = 1, type = "const")


# Gerar tabela com resultados
stargazer(modelo$varresult$r1, modelo$varresult$r2, type = "text")


#####
##  7. Verificar significância estatística do modelo estimado e, caso seja necessário, eliminar parâmetros não significantes.
#####

# Em função de todos os coeficientes estimados serem estatisticamente significantes, 
# optamos por não aplicar qualquer restrição sobre as equações estimadas.

#####
##  8. Examinar se os resíduos se comportam como ruído branco e condições de estacionariedade do modelo. Caso contrário, retornar ao passo 3 ou 4.
#####

# Gráficos para avaliação do ajuste da primeira equação do modelo
par(mfrow = c(1, 1))
plot(modelo, names = "r1", lag.acf = 16, lag.pacf = 16)

# Gráficos para avaliação do ajuste da segunda equação do modelo
plot(modelo, names = "r2", lag.acf = 16, lag.pacf = 16)

# Correlação Cruzada
# Usamos a função ccf do pacote stats que tem as seguintes opções:
# - x: série temporal do resíduo da primeira equação
# - y: série temporal do resíduo da segunda equação
# - lag.max: máximo de defasagens no gráfico da FCC
# - main: título do gráfico (deixei em branco e usei a função title)
# - ylab: nome do eixo y
# - xlab: nome do eixo x
par(mfrow = c(1, 1))
stats::ccf(x = modelo$varresult$r1$residuals, y = modelo$varresult$r2$residuals, lag.max = 10, 
           main = "", ylab = "FCC", xlab = "Defasagens")
title(paste0(colnames(vardata)[1], "-", colnames(vardata)[2]), adj = 0.5, line = 1)

# Como é possível observar, para ambos os termos de erro de cada variável não temos resíduos com autocorrelação 
# (a partir da não significância estatística das defasagens das funções FAC e FACP). Já a correlação cruzada entre 
# os resíduos das duas equações pode ser verificada pela Função de Correlação Cruzada (FCC).


# Uma das hipóteses do modelo estimado é que os resíduos são homocedasticos, ou seja, que não há variância
# condicional. O teste ARCH-LM para o modelo foi computado para o modelo e não é possível rejeitar a hipótese
# nula de homocedasticidade. 
vars::arch.test(modelo)

# Outra hipótese do modelo é a ausência de autocorrelação serial nos resíduos. Para tanto, o teste de Breusch-Goldfrey
# foi realizado e não foi possível rejeitar a hipótese nula de ausência de autocorrelação serial nos resíduos
vars::serial.test(modelo)

# Por fim, a hipótese de normalidade dos resíduos de cada equação estimada é avaliada. Como é possível observar
# pelos p-valor do teste de Jarque-Bera, não é possível rejeitar a hipótese nula de normalidade dos resíduos ao
# nível de 5% de significância
vars::normality.test(modelo, multivariate.only = FALSE)


#####
##  9. Uma vez que os resíduos são ruído branco e o modelo é estável:.
#####

# Aqui, usamos a função roots do pacote vars que recebe como opção:
# - x: modelo VAR estimado
# - modulus: retorna o valor absoluto das raízes. Caso contrário, 
# retorna tanto a parte real como a complexa, caso exista.
vars::roots(x = modelo, modulus = TRUE)

###  Impulso resposta 

# Usamos a função irf do pacote vars para obter as 
# funções de impulso resposta do modelo. Tal função
# tem as seguintes opções:
# - x: modelo var que será analisado
# - impulse: o nome da variável que queremos impulsionar.
# O nome deve ser o mesmo da saída do modelo
# - response: variáveis que queremos obter a resposta. Aqui,
# podemos obter a resposta de uma única variável colocando
# seu nome ou deixando NULL que calculará a resposta em 
# todas as demais váriáveis do modelo
# - n.ahead: passos à frente que queremos visualizar
# - demais opções: veja help(irf)

# Choque em Y1
model1.irf <- vars::irf(x = modelo, impulse = "r1", n.ahead = 30)

# Choque em y2
model2.irf <- vars::irf(x = modelo, impulse = "r2", n.ahead = 30)

###  Gráficos da IR 
par(mfcol = c(2,2))

plot.ts(model1.irf$irf$r1[,1], axes = F, ylab = 'r_1')
lines(model1.irf$Lower$r1[,1], lty = 2, col = 'red')
lines(model1.irf$Upper$r1[,1], lty = 2, col = 'red')
axis(side = 2, las = 2, ylab = '')
abline(h = 0, col = 'red')
box()
mtext("Resposta do choque em r1")

plot.ts(model1.irf$irf$r1[,2], axes = F, ylab = 'r_2')
lines(model1.irf$Lower$r1[,2], lty = 2, col = 'red')
lines(model1.irf$Upper$r1[,2], lty = 2, col = 'red')
axis(side = 1, las = 1)
axis(side = 2, las = 2)
abline(h = 0, col = 'red')
box()

# Gráficos para avaliação do ajuste da segunda equação do modelo
plot.ts(model2.irf$irf$r2[,1], axes = F, ylab = '')
lines(model2.irf$Lower$r2[,1], lty = 2, col = 'red')
lines(model2.irf$Upper$r2[,1], lty = 2, col = 'red')
axis(side = 2, las = 2, ylab = '')
abline(h = 0, col = 'red')
box()
mtext("Resposta do choque em r2")

plot.ts(model2.irf$irf$r2[,2], axes = F, ylab = '')
lines(model2.irf$Lower$r2[,2], lty = 2, col = 'red')
lines(model2.irf$Upper$r2[,2], lty = 2, col = 'red')
axis(side = 1, las = 1)
axis(side = 2, las = 2)
abline(h = 0, col = 'red')
box()

# Analisar a importância das variáveis para explicar a variância do erro de previsão de cada variável
# Usamos a função fevd do pacote vars que tem as opções
# - x: modelo a ser analisado
# - n.ahead: horizonte de previsão de interesse
fevd.model <- vars::fevd(x = modelo, n.ahead = 5)

# Gráfico com os resultados
plot(fevd.model, main = "", xlab = "Horizonte de Previsão", ylab = "Percentual")
