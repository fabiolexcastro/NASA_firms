

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(
  terra, sf, fs, tidyverse, glue, scales, openxlsx, readxl, patchwork, hexbin, stringr, geodata, RColorBrewer, ggspatial
)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Datos -------------------------------------------------------------------
path <- './data/datos_loja.xlsx'
dout <- './output/eda'
dir.create(dout, recursive = T, showWarnings = F)

