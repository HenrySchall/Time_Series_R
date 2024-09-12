######################
### Série Temporal ###
######################

# Criando dados aleatórios
set.seed(10) #definir ponto inicial para gerar mesmos valores aleatórios
dados1 <- rnorm(40) #rnorm torna os resíduos normalmente distribuídos, outra alternativa, seria a função rt para resíduos distribuídos conforme uma t de Student.
print(dados1)

# Criando série (Anual)
serie1 <- ts(dados1, start = c(1980), end=c(2020), frequency=1) #frequency define o prazo da série 1(ano), 12(meses).
print(serie1)
plot(serie1)

# Criando Série (Mensal)
#set.seed(10)
#dados2 <- rnorm(70)
#serie2 <- ts(dados2,start = c(2015,1), end=c(2020,12), frequency=12)
#print(serie2)
#plot(serie2)
#shapiro.test(serie1)
#jarque.bera.test(serie1)

######################
### Autocorrelação ###
######################

set.seed(123)

# Plotar a FAC
acf(dados1, main="Função de Autocorrelação (FAC)")

# Plotar a FACP
pacf(dados1, main="Função de Autocorrelação Parcial (FACP)")

#########################
### Passeio Aleatório ###
#########################

# Criando 41 números aleatórios entre 0 e 100
dados1 <- runif(41, min=0, max=100);

# Criando Passeio Aleatório 
s1 <- ts(dados1, start = 1980, end=2020, frequency=1)
plot(s1)

# Criando 41 números aleatórios entre 0 e 100 (Com repetição)
dados2 <- sample(0:100, 41, replace=TRUE)

# Criando Passeio Aleatório 
s2 = ts(dados2, start = c(1980), end=c(2020), frequency=1)
plot(s2)

##############
### Testes ###
##############

### Testes de Normalidade ###

# - H0: resíduos normalmente distribuídos (p > 0.05)
# - H1: resíduos não são normalmente distribuídos (p <= 0.05)
shapiro.test(serie1)
jarque.bera.test(serie1)

### Testes de Estacionaridade ###

# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
# - H0 = é estacionária: teste estatístico < valor crítico
# - H1 = não é estacionária: teste estatístico >= valor crítico
ur.kpss(serie1)

# Teste pp (Philips-Perron)
#  - H0 = é estacionária: p > 0.05
#  - H1 = não é estacionária: p <= 0.05
pp <- ur.pp(serie1)
summary(pp)

# Teste Dickey Fuller
#  - H0 = é estacionária:  teste estatístico < valor crítico
#  - H1 = não é estacionária: teste estatístico > valor crítico
ur.df(serie1)

### Testes de Autocorrelação ###

# Teste de Ljung-Box 
#  - H0: não autocorrelacionados (p > 0.05)
#  - H1: são autocorrelacionados (p <= 0.05)
Box.test (serie1, type = "Ljung")

####################
### Decomposição ###
####################

library(help="datasets")
sunspots
plot(sunspots)

decomposicao <- decompose(sunspots)
plot(decomposicao)

#tsclean remove outliers identificados dessa maneira e os substitui (e quaisquer valores ausentes) por substituições interpoladas linearmente
suavizacao <- tsclean(dados)
plot(dados)

# Comparação com o original
plot(dados)
lines(suavizacao, col="red")

####################################
### Tranformação e Diferenciação ###
####################################

passageiros <- AirPassengers #Número de passageiros aéreos entre 1949 a 1960
plot(passageiros)
print (passageiros)

# Testes de Normalidade
#  - H0: resíduos normalmente distribuídos (p > 0.05)
#  - H1: resíduos não são normalmente distribuídos (p <= 0.05)
hist(passageiros)
qqnorm(passageiros)
qqline(passageiros)
shapiro.test(passageiros)

### Transformação log ###

passageiros2 <- log(passageiros)
hist(passageiros2)
shapiro.test(passageiros2) #não foi muito eficiente

### Transformação raiz cúbica (Expoencial) ###

passageiros3 <- sign(passageiros)*abs(passageiros)^(1/3)
hist(passageiros3)
shapiro.test(passageiros3) #continua não foi muito eficiente

#Comparando os modelos 

qqnorm(passageiros2) #Modelo Log
qqline(passageiros2)

qqnorm(passageiros3) #Modelo Exponencial
qqline(passageiros3)

### Realizando Diferença ###

decomposicao <- decompose(passageiros3)
plot(decomposicao)

# Teste pp (Philips-Perron)
#   - H0 = é estacionária: p > 0.05
#   - H1 = não é estacionária: p <= 0.05
pp <- ur.pp(passageiros3)
summary(pp)

ndiffs(passageiros3) #indica quantas diferenças devem ser feitas

# Primeira diferença
dif_passageiros <- diff(passageiros3)
dif_passageiros

# Teste pp (Philips-Perron)
#  - H0 = é estacionária: p > 0.05
#  - H1 = não é estacionária: p <= 0.05
pp <- ur.pp(dif_passageiros)
summary(pp)

# Segunda diferença
dif_passageiros2 <- diff(dif_passageiros)

pp <- ur.pp(dif_passageiros2)
summary(pp)

##################
### Suavização ###
##################

passageiros <- ts(AirPassengers, start = c(1949), end = c(1960), frequency = 12)
print(passageiros)
plot(passageiros, type="l", ylab="Fluxo Internacional de Passageiros de Avião ",col="blue")

### MMS ###
passageiros2 <- ma(passageiros, order = 7) 
plot(passageiros2)

passageiros3 <- ma(passageiros, order = 20, centre = TRUE) #centre = TRUE -> apenas se a order número par (teória)
plot(passageiros3)

passageiros4 <- ma(passageiros, order = 51, centre = TRUE)
plot(passageiros4)

plot(passageiros)
lines(passageiros2, col = "red")
lines(passageiros3, col = "green")
lines(passageiros4, col = "blue")