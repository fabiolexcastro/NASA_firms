

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
thme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = '#e1e0d9', linewidth = 0.3),
        axis.text = element_text(color = '#52514e'),
        plot.title = element_text(face = 'bold', color = '#0b0b0b'),
        plot.subtitle = element_text(color = '#52514e'),
        legend.position = 'top')

# Control quality ---------------------------------------------------------
dim(fire)
glimpse(fire)

## Missing values and duplicados
qlty <- tibble(
  n.rows  = nrow(fire),
  n.na    = sum(is.na(fire)),
  n.dupl  = sum(duplicated(fire[, c('LATITUDE', 'LONGITUDE', 'ACQ_DATE', 'ACQ_TIME', 'SATELLITE')])),
  dt.min  = min(fire$ACQ_DATE),
  dt.max  = max(fire$ACQ_DATE),
  lat.rng = paste(range(fire$LATITUDE), collapse = ' / '),
  lon.rng = paste(range(fire$LONGITUDE), collapse = ' / ')
)

## Categorical data
fire %>% count(SATELLITE)
fire %>% count(DAYNIGHT)
fire %>% count(TYPE)  
fire %>% count(VERSION)    
fire %>% group_by(VERSION) %>% summarise(dt.min = min(ACQ_DATE), dt.max = max(ACQ_DATE))
fire %>% count(CONFIANZA)
