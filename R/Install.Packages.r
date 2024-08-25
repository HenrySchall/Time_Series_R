# Carregando pacotes
library(dplyr)
library(tidyverse)
library(TeachingDemos)
library(EnvStats)
library(readr)
library(readxl)
library(xtable)
library(stargazer)
library(highcharter)
library(quantmod)
library(dygraphs)
library(tseries)
library(htmltools)
library(Quandl)
library(nycflights13)
library(magrittr)
library(discreteRV)
library(aTSA)
library(fGarch)
library(fUnitRoots)
library(forecast)
library(vars)
library(MTS)
library(seasonal)
library(dynlm)
library(tbl2xts)
library(dlm)
library(stats)
library(tcltk)
library(forecast)
library(ggplot2)
library(PerformanceAnalytics)
library(nortest)
library(scales)
library(GetBCBData) 
library(ipeadatar) 
library(sidrar) 
library(stringr)
library(fma)

# Instalando pacotes (Alternativa 1)
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(TeachingDemos)){install.packages("TeachingDemos")}
if(!require(EnvStats)){install.packages("EnvStats")}
if(!require(readr)){install.packages("readr")}
if(!require(readxl)){install.packages("readxl")}
if(!require(xtable)){install.packages("xtable")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(highcharter)){install.packages("highcharter")}
if(!require(quantmod)){install.packages("quantmod")}
if(!require(dygraphs)){install.packages("dygraphs")}
if(!require(tseries)){install.packages("tseries")}
if(!require(htmltools)){install.packages("htmltools")}
if(!require(Quandl)){install.packages("Quandl")}
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(discreteRV)){install.packages("discreteRV")}
if(!require(aTSA)){install.packages("aTSA")}
if(!require(fGarch)){install.packages("fGarch")}
if(!require(fUnitRoots)){install.packages("fUnitRoots")}
if(!require(forecast)){install.packages("forecast")}
if(!require(vars)){install.packages("vars")}
if(!require(MTS)){install.packages("MTS")}
if(!require(seasonal)){install.packages("seasonal")}
if(!require(urca)){install.packages("urca")}
if(!require(dynlm)){install.packages("dynlm")}
if(!require(tbl2xts)){install.packages("tbl2xts")}
if(!require(dlm)){install.packages("dlm")}
if(!require(stats)){install.packages("stats")}
if(!require(tcltk)){install.packages("tcltk")}
if(!require(forecast)){install.packages("forecast")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
if(!require(nortest)){install.packages("nortest")}
if(!require(scales)){install.packages("scales")}
if(!require(seasonalview)){install.packages("seasonalview")}
if(!require(GetBCBData)){install.packages("GetBCBData")}
if(!require(ipeadatar)){install.packages("ipeadatar")}
if(!require(sidrar)){install.packages("sidrar")}
if(!require(stringr)){install.packages("stringr")}
if(!require(fma)){install.packages("fma")}

# Instalando pacotes (Alternativa 2)
# install.packages = c("tidyverse", "TeachingDemos", "EnvStats","readr","readxl",  "xtable", "stargazer", "highcharter", "quantmod",
# "dygraphs","tseries", "htmltools", "Quandl", "nycflights13", "magrittr","discreteRV", "aTSA", "fGarch", "fUnitRoots", "vars", "MTS", "seasonal",
# "urca", "dynlm", "tbl2xts", "dlm", "stats", "tcltk", "tcltk2", "forecast", "Quandl", "dygraphs", "magrittr", "PerformanceAnalytics", "quantmod",
# "tseries","nortest","scales", "ggpubr","ggplot2", "dplyr")

# .inst <- .packages %in% installed.packages()

# if (length(.packages[!.inst]) > 0) {
# for (package in 1:length(.packages)) {

# print(install.packages)
# message("Pacotes carregados")

# Remover Pacote
remove.packages("argo")