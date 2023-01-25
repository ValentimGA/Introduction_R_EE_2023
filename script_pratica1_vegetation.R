################################################################################
#
# Script Name	: Vegetation Analysis
# Description	: Analysis of Vegetation in Tabuleiro forests and Caatinga
# Args       	: data_exportation.csv
# Last update : January 23th, 2023
# Author    	: MSc. Gabriela Alves Valentim
# Email     	: valentim.gabriela@gmail.com
#
################################################################################

# set directory
setwd("C:\\Users\\valen\\Meu Drive\\Estudos\\PhD PPGCMT UFC\\Estágio em Docência\\introduction_R_EE")

# libraries --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(vegan)

# load dataset -----------------------------------------------------------------
data_vegetation <- read_excel("data_pratica1_vegetation.xlsx") #readxl

# Anacardium occidentale
# Centrosema brasilianum
# Caesalpinia ferrea
# Cereus jamaracu
# Combretum leprosum
# Cordia oncalix
# Cenostigma pyramidale
# Erythroxylum barbatum
# Erythroxylum revolutum
# Mimosa caesalpiniifolia
# Solanum paniculatum
# Thyrsodium spruceanum
# Ziziphus joazeiro

# analysis ---------------------------------------------------------------------

# abundância

data_vegetation <- data_vegetation %>%
  mutate(abundancia = rowSums(data_vegetation[,3:15]))

# densidade

data_vegetation <- data_vegetation %>%
  mutate(density = ifelse(treatment == "2x2", abundancia/4, abundancia/9))

# riqueza
data_vegetation <- data_vegetation %>%
  mutate(riqueza = specnumber(data_vegetation[,3:15])) #specnumber() #vegan

# diversidade de Simpson

data_vegetation <- data_vegetation %>%
  mutate(Simpson = diversity(data_vegetation[,3:15], "simpson")) # diversity() #vegan

# diversidade de Shannon

data_vegetation <- data_vegetation %>%
  mutate(Shannon = diversity(data_vegetation[,3:15], "shannon")) # diversity() #vegan

# equitabilidade de Pielou

data_vegetation <- data_vegetation %>%
  mutate(Pielou = 
           diversity(data_vegetation[,3:15], "shannon")/
           log(specnumber(data_vegetation[,3:15])))

# composição da Caatinga

data_vegetation %>%
  filter(ecosystem == "caatinga") %>%
  select(contains("_")) %>%
  select(where(~ any(. != 0))) %>%
  names()

# composição da Mata de Tabuleiro

data_vegetation %>%
  filter(ecosystem == "tabuleiro") %>%
  select(contains("_")) %>%
  select(where(~ any(. != 0))) %>%
  names()
