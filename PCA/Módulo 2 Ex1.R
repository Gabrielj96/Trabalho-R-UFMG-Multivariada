
# =====================================
# Módulo #2. EST582 – Est. Mult. Comp.
# Ex. 1 – Ortogonalidade na PCA
# Especialização em Estatística, 2023/2
# Essa versão: 25/10/2023      
# =====================================




# ############ #
# Preliminares #
# ############ #

## limpando o workspace
rm(list = ls())


## carregando pacotes necessários
## Nota: aqui podemos usar tanto 'library' quanto 'require'
# matrizes de correlação
library(corrplot)
# gráficos diversos
library(ggplot2)
# mapas e shapefiles
library(ggspatial)
library(ggsn)
library(raster)
library(rgdal)
library(sf)
library(sp)
# manipulação de bases de dados
library(tidyverse)


## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho!
setwd("C:/Users/uriel/Desktop/Módulo 2/code")


## carregando funções auxiliares
source("_src/src.R")




# ##################### #
# Ortogonalidade na PCA #
# ##################### #

## simulando os dados e aplicando a PCA
n = 25
set.seed(42)
x1 = rnorm(n)
x2 = 0.5 + 0.1*x1 + rnorm(n)
X = cbind(x1, x2)
Z = scale(X)
PCA = princomp(Z)


## projetando as componentes em um gráfico
biplot(PCA)
# salvando em arquivo [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.2
dev.print(file = '_out/figures/figBiPlotEx1.png',
          device = png, width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')

