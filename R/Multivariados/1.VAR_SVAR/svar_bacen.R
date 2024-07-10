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
######     VAR BANCO CENTRAL         ######
###########################################

# Como exemplo, vamos estimar um modelo VAR usando dados mensais desde 01/2000 usando as 
# seguintes séries temporais:
# - preços livres: preços que oscilam em função das condições de oferta e demanda dos produtos
# - preços administrados: preços que são menos sensíveis às condições de oferta e de demanda 
# porque são estabelecidos por contrato ou por órgão público (energia elétrica, planos de saúde, …).
# - câmbio: média mensal da compra de Dólar americano (taxa de câmbio livre)
# - juros reais: taxa Selic acumulada no mês anualizada deflacionada pelo IPCA
# Mais detalhes sobre o modelo no link abaixo:
# https://www.bcb.gov.br/htms/relinf/port/2010/06/ri201006b6p.pdf

#####
####  PERÍODO DE INTERSSE 
#####

# Usamos o pacote Quandl para coletar dados diretamente do sistema
# de séries temporais do BACEN. Mais detalhes sobre como usar tal 
# pacote neste link https://rpubs.com/hudsonchavs/coletardados

data.inicio <- "2000-01-01"
data.fim <- "2018-10-31"
tipo <- "xts"
periodicidade <- "monthly"
Quandl.api_key('xvSrAztygUphvYp9q1bs')

#####
####  DADOS
#####

# ipca mensal - Índice nacional de preços ao consumidor - amplo (IPCA) - em 12 meses	- %
ipca.m <- Quandl::Quandl("BCB/13522", type = tipo, collapse = periodicidade, 
                start_date = data.inicio, end_date = data.fim)  

# precos livres - Índice nacional de preços ao consumidor - Amplo (IPCA) - Itens livres - Var. % mensal
livres.m <- Quandl::Quandl("BCB/11428", type = tipo, collapse = periodicidade, 
                  start_date = data.inicio, end_date = data.fim) 

# precos administrados - Índice Nacional de Preços ao Consumidor - Amplo (IPCA) - Monitorados - Var. % mensal
administrados.m <- Quandl::Quandl("BCB/4449", type = tipo, collapse = periodicidade, 
                         start_date = data.inicio, end_date = data.fim)

# média de compra do período em R$/US$
cambio.m <- Quandl::Quandl("BCB/3697", type = tipo, collapse = periodicidade, 
                  start_date = data.inicio, end_date = data.fim) 

# taxa acumulada no mês em termos anuais
selic.m <- Quandl::Quandl("BCB/4189", type = tipo, collapse = periodicidade, 
                 start_date = data.inicio, end_date = data.fim) 

# igpdi
igpdi.m <- Quandl::Quandl("BCB/190", type = tipo, collapse = periodicidade, 
                 start_date = data.inicio, end_date = data.fim) 

#####
####  OBTER AS VARIAÇÕES DO CÂMBIO E JUROS REAIS
#####

# Juros reais
juros.reais.m <- selic.m - igpdi.m
juros.reais.change <- log(juros.reais.m) - log(lag(juros.reais.m)) 

# Cambio
cambio.m.change <- log(cambio.m) - log(lag(cambio.m))
  

######
####  CONJUNTO DE DADOS 
######

# Aqui, juntamos todas as série em um único objeto usando a função
# merge.xts do pacote xts 
databacen <- na.omit(xts::merge.xts(livres.m, administrados.m, cambio.m.change, juros.reais.change))
colnames(databacen) <- c("VarLivre","VarAdministrados","VarCambio", "VarJurosReais")

#####
##   1. Visualizar os dados e identificar observações fora do padrão (outliers, sazonalidade, tendência).
#####

# Visualizando os gráficos abaixo, percebemos que há outliers nos períodos de crises, tanto nacional
# quanto internacional. Assim, uma alternativa seria adicionar uma variável dummy para cada uma das
# crises com o objetivo de diminuir o impacto destes outliers na estimação. Além disso, percebemos
# uma tendência de queda dos preços livres logo após o início do plano real (chamamos de período de
# desinflação). Assim, criamos uma dummy de tendência para este período

par(mfrow=c(2,2))
plot(databacen$VarLivre, xlab = "", ylab = "", type = "l", main = "Variação Preços Livres - mensal", cex = 0.4)
plot(databacen$VarAdministrados, xlab = "", ylab = "", type = "l", main = "Variação Preços Administrados - mensal", cex = 0.4)
plot(databacen$VarCambio, xlab = "", ylab = "", type = "l", main = "Variação Cambial (R$/US$) - mensal", cex = 0.4)
plot(databacen$VarJurosReais, xlab = "", ylab = "", type = "l", main = "Variação Juros Reais - mensal", cex = 0.4)

# Criar uma variável dummy para as Eleições de 2002 e definir período onde recebe valor 1
# criseeleicao <- ts(0, frequency = 12, start = c(2000,2), end = c(2018,10))
# window(criseeleicao, start = c(2002,09), end = c(2002,11)) = 1

# Criar uma variável dummy para a crise Financeira de 2008 e definir período onde recebe valor 1
# crisemundial <- ts(0, frequency = 12, start = c(2000,2), end = c(2018,10))
# window(crisemundial, start = c(2008,11), end = c(2008,12)) = 1

# Definir objeto com as variáveis exógenas do modelo. Não juntamos as dummies
# com as séries do modelo porque elas não terão defasagens sendo exógenas.
# x <- cbind(criseeleicao, crisemundial)

#####
##   2. Se necessário, transformar os dados para estabilizar a variância (logaritmo ou retirar sazonalidade, por exemplo)
#####

# Uma vez que adicionamos as variáveis dummy optamos por não estabilizar a variância dado que as demais
# observações parecem bem comportadas

#####
##  3. Avaliar a função de correlação cruzada para confirmar a possibilidade de modelagem multivariada. 
#####

# Defasagens máximas
defasagens <- 10

# Em função do tamanho do gráfico criamos um arquivo pdf que vai armazenar os gráficos 
pdf("/cloud/project/4.Serie_Temporal_Multivariada/1.VAR_SVAR/plots_bacen_one.pdf", paper = "USr", width = 10, height = 10)

# Montar uma "matriz" de gráficos (kxk)
par(mfrow = c(ncol(databacen), ncol(databacen)))

# Adicionar na "matriz" de gráficos a CCF de cada série
# contra ela mesma e as demais
for (i in 1:ncol(databacen)) {
  for (j in 1:ncol(databacen)) {
    ccf(drop(databacen[,i]), drop(databacen[,j]), lag.max = defasagens, 
        main = "", ylab = "FCC", xlab = "Defasagens")
    title(paste0(colnames(databacen)[i], "-", colnames(databacen)[j]), adj = 0.4, line = 1)
  }
}

# parar de gravar no arquivo pdf criado
dev.off()

# Os gráficos mostram a dependência linear entre as séries temporais, principalmente entre os preços liveres e
# administrados (defasagens dos preços livres parecem impactar os preços administrados) e entre o câmbio e os
# preços administrados (defasagens do câmbio impactando os preços administrados). Assim, faz sentido usar um 
# modelo multivariado para avaliar a dependência entre as séries.

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
unitRootVarLivres <- suppressWarnings(fUnitRoots::adfTest(databacen[,1],lags=3, type=c("nc")))
unitRootVarAdministrados <- suppressWarnings(fUnitRoots::adfTest(databacen[,2],lags=3, type=c("nc")))
unitRootVarCambio <- suppressWarnings(fUnitRoots::adfTest(databacen[,3],lags=3, type=c("nc")))
unitRootVarJurosReais <- suppressWarnings(fUnitRoots::adfTest(databacen[,4],lags=3, type=c("nc")))

# Tabela com os resultados do teste que confirmam a estacionariedade das séries
adfVarLivres <- c(unitRootVarLivres@test$statistic, unitRootVarLivres@test$p.value)
adfVarAdministrados <- c(unitRootVarAdministrados@test$statistic, unitRootVarAdministrados@test$p.value)
adfVarCambio <- c(unitRootVarCambio@test$statistic, unitRootVarCambio@test$p.value)
adfVarJurosReais <- c(unitRootVarJurosReais@test$statistic, unitRootVarJurosReais@test$p.value)
resultADF <- cbind(adfVarLivres, adfVarAdministrados, adfVarCambio, adfVarJurosReais)
colnames(resultADF) <- c("VarLivres", "VarAdministrados", "VarCambio", "VarJurosReais")
rownames(resultADF) <- c("Estatística do Teste ADF", "p-valor")
print(resultADF)

# Os resultados mostram que todas as séries são estacionárias

#####
##  5. Definir a ordem p para os dados em análise por meio de critérios de informação (escolher modelo com menor AIC, por exemplo)
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
varorder <- vars::VARselect(y = databacen, lag.max = 6, type = "const")
print(varorder$selection)

#####
##  6. Estimar o modelo escolhido no passo anterior.
#####

# Usamos a função VAR do pacote vars que tem as seguintes opções 
# além das opções já apresentadas em VARselect:
# - p: número de defasagens do modelo
# - type: se o modelo deve inserir parâmetros para constante (const), tendência (trend),
# ambos (both) ou nenhum deles (none)
# - season: se o modelo deve inserir variáveis dummy para sazonalidade
# - exogen: conjunto de variáveis exógenas que não são defasadas no modelo
modelo <- vars::VAR(y = databacen, p = 1, type = "const")

# Matriz A
# Aqui, estamos criando uma matriz diagonal (com 1's). Essa matriz precisa 
# ser quadratica (mesmo número de linhas e colunas igual ao número de variáveis
# do modelo). Devemos colocar NA onde temos o objetivo de estimar parâmetros
# para esta matriz. Observe o primeiro modelo AB apresentado no link abaixo
# https://rpubs.com/hudsonchavs/var
Amat <- diag(4) 
Amat[1, 2] <- NA 
Amat[1, 3] <- NA
Amat[1, 4] <- NA
Amat[2, 3] <- NA
Amat[2, 4] <- NA
Amat[3, 4] <- NA

# Matriz B
# Aqui, estamos criando uma matriz diagonal (com 1's). Essa matriz precisa 
# ser quadratica (mesmo número de linhas e colunas igual ao número de variáveis
# do modelo). Devemos colocar NA onde temos o objetivo de estimar parâmetros
# para esta matriz. Observe o primeiro modelo AB apresentado no link abaixo
# https://rpubs.com/hudsonchavs/var
Bmat <- diag(4)
Bmat[1, 1] <- NA 
Bmat[2, 2] <- NA
Bmat[3, 3] <- NA 
Bmat[4, 4] <- NA 

svar <- vars::SVAR(x = modelo, estmethod = "direct", Amat = Amat, Bmat = Bmat)


#####
##  9. Uma vez que os resíduos são ruído branco e o modelo é estável:
#####

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

# Resposta do choque em VarJurosReais e a resposta em VarLivre
model1.irf <- vars::irf(x = svar, impulse = "VarJurosReais", response = "VarLivre", n.ahead = 20)

# Resposta do choque em Administrados e a resposta em VarLivre
model2.irf <- vars::irf(x = svar, impulse = "VarAdministrados", response = "VarLivre", n.ahead = 20)

# Resposta do choque em VarCambio e a resposta em VarLivre
model3.irf <- vars::irf(x = svar, impulse = "VarCambio", response = "VarLivre", n.ahead = 20)

##########################
###  GRÁFICOS DA IR    ###
##########################

par(mfcol=c(3,1))

plot.ts(model1.irf$irf$VarJurosReais[,1], axes=F, ylab='VarLivre')
lines(model1.irf$Lower$VarJurosReais[,1], lty=2, col='red')
lines(model1.irf$Upper$VarJurosReais[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Resposta em VarLivre dado choque nos VarJurosReais")

plot.ts(model2.irf$irf$VarAdministrados[,1], axes=F, ylab='VarLivre')
lines(model2.irf$Lower$VarAdministrados[,1], lty=2, col='red')
lines(model2.irf$Upper$VarAdministrados[,1], lty=2, col='red')
axis(side=2,las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Resposta em VarLivre dado choque no VarAdministrados")

plot.ts(model3.irf$irf$VarCambio[,1], axes=F, ylab='VarLivre')
lines(model3.irf$Lower$VarCambio[,1], lty=2, col='red')
lines(model3.irf$Upper$VarCambio[,1], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
mtext("Resposta em VarLivre Reais dado choque no VarCâmbio")


# Analisar a importância das variáveis para explicar a variância do erro de previsão de cada variável
# Usamos a função fevd do pacote vars que tem as opções
# - x: modelo a ser analisado
# - n.ahead: horizonte de previsão de interesse
fevd.model <- vars::fevd(x = svar, n.ahead = 5)
# Gráfico com os resultados
plot(fevd.model, main="", xlab = "Horizonte de Previsão", ylab = "Percentual")
