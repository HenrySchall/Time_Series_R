import string as string
import random as random
import radian as rd
import pandas as pd
import numpy as np
import scipy.stats as stats
import datetime  
import seaborn as sns
import seaborn.objects as so
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.pylab import rcParams
import plotly.express as px
import math as math

######################
### Série Temporal ###
######################

# Configuração do padrão de medidas do plot dos gráficos
rcParams['figure.figsize'] = 15, 6

# Criando dados aleatórios (Série Anual)
np.random.seed(10) # definir ponto inicial para gerar mesmos valores aleatórios
dados = np.random.normal(0,1,31) # (média,desvio padrão,quantidade de valores [1990 a 2020])
dados

# Criando dados aleatórios (Série Mensal)
# np.random.seed(6)
# dados = np.random.normal(0,1,72)
# dados = pd.DataFrame(dados)
# dados.columns = ['valores']
# dados

# Selecionando o Período 
# data = np.array('2015-01', dtype = np.datetime64())
# data = data + np.arange(72)
# data = pd.DataFrame(data)
# data.columns = ['data']
# data

# Combinando valores
# serie2 = pd.concat([data, dados], axis=1)
# serie2

# Série
# serie2 = pd.Series(serie2['valores'].values, index = serie2['data'])
# serie2
# serie2.plot()
# plt.show()

# Criando dados aleatórios (Série Diária)
# np.random.seed(12)
# ados3 = np.random.normal(1,2,731)
# dados3

# Selecionando o Período 
# periodo = pd.date_range('2000', periods = len(dados), freq = 'D')
# periodo

# Tabela 
dados = pd.DataFrame(dados)
dados.columns = ['valores']
dados
dados.shape # verificando valores

# Gráfico 
serie = pd.Series(dados)
serie.plot()
plt.show()

# Selecionando o Período 
periodo = pd.date_range('2000', periods = len(dados), freq = 'Y')
periodo

serie1 = pd.Series(dados['valores'].values, index = periodo)
serie1.plot()
plt.show()

#########################
### Passeio Aleatório ###
#########################


##############
### Testes ###
##############

### Testes de Normalidade ###

# - H0: resíduos normalmente distribuídos (p > 0.05)
# - H1: resíduos não são normalmente distribuídos (p <= 0.05)
stats.shapiro(serie1)

# Verificando se segue uma distribuição normal (graficamente)
stats.probplot(serie1, dist="norm", plot=plt)
plt.title("Normal QQ plot")
plt.show()

### Testes de Estacionaridade ###

# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
# - H0 = é estacionária: teste estatístico < valor crítico
# - H1 = não é estacionária: teste estatístico >= valor crítico


# Teste pp (Philips-Perron)
#  - H0 = é estacionária: p > 0.05
#  - H1 = não é estacionária: p <= 0.05


# Teste Dickey Fuller
#  - H0 = é estacionária:  teste estatístico < valor crítico
#  - H1 = não é estacionária: teste estatístico > valor crítico

### Testes de Autocorrelação ###

# Teste de Ljung-Box 
# - H0: não autocorrelacionados (p > 0.05)
# - H1: são autocorrelacionados (p <= 0.05)

####################
### Decomposição ###
####################

