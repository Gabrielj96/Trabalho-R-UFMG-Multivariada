
# =====================================
# Módulo #2. EST582 – Est. Mult. Comp.
# Ex. 3 – Propriedades Básicas da PCA
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
# normal multivariada
library(mvtnorm)


## mudando diretório de trabalho
## Nota: lembre-se de sempre alterar esse caminho!
setwd("C:/Users/uriel/Desktop/Módulo 2/code")


## carregando funções auxiliares
source("_src/src.R")




# ########################### #
# Propriedades Básicas da PCA #
# ########################### #

## simulando os dados
# fixando semente aleatória
set.seed(42)
# tamanho amostral
n = 50
# vetor de médias
mu = matrix(c(1e2, 1e1, 1e-1), nrow = 3, ncol = 1)
# matriz de variâncias-covariâncias
Sigma = matrix(c(1e4, 1e2, 1e1,
                 1e2, .5e2, 2e0,
                 1e1, 2e0, 1e-1),
               nrow = 3, ncol = 3)
# simulando X
X = rmvnorm(n, mean = mu, sigma = Sigma)


## PCA na matriz de *covariâncias*
# Nota: 'princomp' é nativa da biblioteca stats
PCA = princomp(X, cor = FALSE)
summary(PCA)
# matriz de pesos
PCA$loadings[]


## PCA na matriz de *correlações*
# (compare os resultados com o PCA acima)
PCA = princomp(X, cor = TRUE)
summary(PCA)
# matriz de pesos
PCA$loadings[]
# Re-escalando X: resultados equivalentes
Z = scale(X)
summary(princomp(Z, cor = FALSE))
summary(princomp(Z, cor = TRUE))
# ortogonalidade dos pesos
W = PCA$loadings[]
print(round(t(W)%*%W), 10)
# ortogonalidade das componentes
X.hat = PCA$scores
print(round(t(X.hat)%*%X.hat, 10))
print(round(cov(X.hat), 10))
print(round(cor(X.hat), 10))


## PCA via decomposição espectral
# Nota #1: eigen(cov(Z)) = eigen(cor(X))
# Nota #2: pequenas diferenças numéricas nas soluções
# =========  podem ocorrer devido ao algoritmo usado 
# =========  para decomposição na PCA
D = eigen(cov(Z))
lambda = D$values
Lambda = diag(lambda)
Lambda
# desvios padrão
sqrt.lambda = sqrt(lambda)
sqrt.lambda
# percentual da variância total
VT = sum(diag(cov(Z)))
VT
VP = 100*lambda/VT
VP
# comparação com princomp
summary(PCA)
rbind(round(sqrt.lambda, 7),
      round(VP, 7),
      round(cumsum(VP), 7)
)

