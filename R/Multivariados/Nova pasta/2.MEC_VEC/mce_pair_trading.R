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

##########################
####      DADOS      #####
##########################

# Coletando dados amostrais para exemplos de correlação cruzada. Aqui
# usamos a função getSymbols do pacote quantmod.
petro_day <- quantmod::getSymbols("PETR4.SA", src = "yahoo", warnings = FALSE, auto.assign=FALSE,
                                 from = "2010-01-01", return.class = "xts")

vale_day <- quantmod::getSymbols("VALE3.SA", src = "yahoo", warnings = FALSE, auto.assign=FALSE, 
                                from = "2010-01-01", return.class = "xts")

# Em função de circuit breaker no período em análise, há datas sem preços.
# Assim, precisamos aliminar tais dados da amostra. Para tanto, usamos 
# a função !is.na que retornará TRUE para valores que não são NA. Após isso,
# filtramos os dados de fechamento para pegar apenas aqueles que retornam
# TRUE para !is.na
petro_day_clean <- petro_day$PETR4.SA.Close[!is.na(petro_day$PETR4.SA.Close)]
vale_day_clean <- vale_day$VALE3.SA.Close[!is.na(vale_day$VALE3.SA.Close)]

# Juntar as três séries temporais para gerar a série temporal multivariada
stocks_merge <- merge.xts(petro_day_clean,vale_day_clean, all = c(FALSE,FALSE))
colnames(stocks_merge) <- c("PETR4","VALE3")

# Transformar do formato xts para tbl
stocks_merge_tbl <- tbl2xts::xts_tbl(stocks_merge)

p1 <- plot.xts(stocks_merge, type = "l", lty = c(1,2), col = 1, lwd = c(1,2), main = "")
p1 <- addLegend("top", on = 1, 
               legend.names = c("PETR4","VALE3"), 
               lty = c(1,2), lwd = c(1,2),
               col = 1)
p1 

# Aqui, usamos a função adfTest do pacote fUnitRoots para testar se há raiz unitária
# nas séries avaliadas. Como observamos no gráfico da série, não há tendência
# nos dados e assim o teste verificará se a série se comporta como um passeio aleatório
# sem drift. Isto é evidênciado por meio da opção type que tem as seguintes opções:
# - nc: for a regression with no intercept (constant) nor time trend (passeio aleatório)
# - c: for a regression with an intercept (constant) but no time trend (passeio aleatório com drift)
# - ct: for a regression with an intercept (constant) and a time trend (passeio aleatório com constante e tendência)
# Além disso, definimos que no máximo duas defasagens da série devem ser usadas como
# variáveis explicativas da regressão do teste.

unitRoot_vale <- fUnitRoots::adfTest(stocks_merge_tbl$VALE3,lags = 2, type = c("nc"))

unitRoot_petr <- fUnitRoots::adfTest(stocks_merge_tbl$PETR4, lags = 2, type = c("nc"))

# Como as duas séries são I(1), ou seja, têm raíz unitária estimamos a relação entre elas

# Estimação da Regressão Linear Simples via OLS. Aqui, usamos
# a função lm do pacote stats que tem as seguintes opções:
# - formula: modelo a ser ajustato (~ faz o papel de "=")
# - data: o cojunto de dados
# - weights: os pesos para regressão ponderada
# - subset: sub-conjunto dos dados
# - na.action: especificar o que fazer no cado de NA nos dados. Como
# padrão usa a função na.omit que exclui da base casos de NA

modelo_pairs <- stats::lm(formula = VALE3  ~ PETR4, data = stocks_merge_tbl)

# Testar se os reíduos são estacionários. Observe que não faz sentido adicionar intercepto
# ou tendência neste teste, pois os resíduos de MQO oscilam em torno de zero.
unitRootResiduals_pairs <- fUnitRoots::adfTest(modelo_pairs$residuals, lags = 2, type = c("nc"))

# Usando o processo de estimação em dois estágios de Engle-Granger

# a) Estimar a relação de cointegração de longo prazo
reg_pairs <- lm(formula = VALE3 ~ PETR4, data = stocks_merge_tbl)

# b) Coletar os erros da estimação anterior
resid_pairs <- as.zoo(reg_pairs$resid)

# c) Estimar a relação de curto prazo e longo prazo juntas (modelo de correção de erros). O
# coeficiente para os resíduos defasados indica quão rápido o ajustamento ocorre em um período. 
# Se o parâmetro for próximo de 0, o ajustamento ocorre lentamente enquanto que próximo de -1
# o ajustamento é rápido. 
mce_pairs <- dynlm::dynlm(formula = d(VALE3, 1) ~ -1 + d(PETR4, 1) + L(resid_pairs, 1), data = stocks_merge_tbl)

# texreg::texreg(list(reg_pairs, mce_pairs), use.packages = FALSE, single.row = TRUE)



