

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(
  terra, sf, fs, tidyverse, readxl, glue, scales, openxlsx, readxl, patchwork, hexbin, stringr, geodata, RColorBrewer, ggspatial
)
library(readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Datos -------------------------------------------------------------------
path <- './data/datos_loja.xlsx'
dout <- './output/eda'
dir.create(dout, recursive = T, showWarnings = F)

excel_sheets(path)
fire <- readxl::read_excel(path, sheet = 'FIRMS')
powr <- readxl::read_excel(path, sheet = 'POWER')

# Theme -------------------------------------------------------------------

# Colors and theme (blue / orange = categorical slots 1-2)
clrs <- c('Terra' = '#2a78d6', 'Aqua' = '#eb6834')

