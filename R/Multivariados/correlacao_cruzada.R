###########################################
####   FUNÇÃO DE CORRELAÇÃO CRUZADA    ####
###########################################

#####
##   PACOTES NECESSÁRIOS
#####

source("/cloud/project/install_and_load_packages.R")

#####
##   DADOS
#####

petro_day <- quantmod::getSymbols("PETR4.SA", src = "yahoo", warnings = FALSE, auto.assign = FALSE,
                                 from = "2010-01-01", return.class = "xts")

vale_day <- quantmod::getSymbols("VALE3.SA", src = "yahoo", warnings = FALSE, auto.assign = FALSE, 
                                from = "2010-01-01", return.class = "xts")

itau_day <- quantmod::getSymbols("ITUB4.SA", src = "yahoo", warnings = FALSE, auto.assign = FALSE, 
                                from = "2010-01-01", return.class = "xts")

# Em função de circuit breaker no período em análise, há datas sem preços.
# Assim, precisamos aliminar tais dados da amostra. Para tanto, usamos 
# a função !is.na que retornará TRUE para valores que não são NA. Após isso,
# filtramos os dados de fechamento para pegar apenas aqueles que retornam
# TRUE para !is.na
petro_day_clean <- petro_day$PETR4.SA.Close[!is.na(petro_day$PETR4.SA.Close)]
vale_day_clean <- vale_day$VALE3.SA.Close[!is.na(vale_day$VALE3.SA.Close)]
itau_day_clean <- itau_day$ITUB4.SA.Close[!is.na(itau_day$ITUB4.SA.Close)]

# Juntar as três séries temporais para gerar a série temporal multivariada
stocks_merge <- merge.xts(petro_day_clean,vale_day_clean,itau_day_clean, all = c(FALSE,FALSE))
colnames(stocks_merge) <- c("PETR4","VALE3", "ITUB4.SA")

plot.xts(stocks_merge, col = c("black", "green", "red"), main = "")
addLegend("top", 
          legend.names = c("PETRO4", "VALE3", "ITUB4"), 
          lty = c(1, 1, 1),
          col = c("black", "green", "red"))

#####
##   RETORNOS
#####

# Série de retornos. Aqui, usamos a função ROC do pacote
# TTR que calcula o retorno de uma série temporal multivariada
# Retiramos a primeira linha [-1,] em função de não ser possível
# calcular o retorno para a primeira observação (não temos dados
# antes desta data)
stocks_returns <- TTR::ROC(x = stocks_merge, type = "continuous")[-1,]

#####
##   CORRELAÇÃO CRUZADA
#####

# Defasagens máximas
defasagens <- 5

# Montar uma "matriz" de gráficos (kxk)
par(mfrow = c(ncol(stocks_returns), ncol(stocks_returns)))

# Adicionar na "matriz" de gráficos a CCF de cada série
# contra ela mesma e as demais
for (i in 1:ncol(stocks_returns)) {
  for (j in 1:ncol(stocks_returns)) {
    ccf(drop(stocks_returns[,i]), drop(stocks_returns[,j]), lag.max = defasagens, 
        main = "", ylab = "FCC", xlab = "Defasagens")
    title(paste0(colnames(stocks_returns)[i], "-", colnames(stocks_returns)[j]), adj = 0.5, line = 1)
  }
}

par(mfrow = c(1, 1))
i=2
j=3
ccf(drop(stocks_returns[,i]), drop(stocks_returns[,j]), lag.max = defasagens, 
    main = "", ylab = "FCC", xlab = "Defasagens")
title(paste0(colnames(stocks_returns)[i], "-", colnames(stocks_returns)[j]), adj = 0.5, line = 1)
