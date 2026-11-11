

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

## Power values, NASA NA value: -999
powr <- powr %>% mutate(across(ALLSKY_SFC_SW_DWN:PRECTOTCORR, as.numeric))
powr
powr %>% summarise(across(ALLSKY_SFC_SW_DWN:PRECTOTCORR, ~ sum(is.na(.x) | .x == -999)))
nrow(powr) == as.numeric(max(powr$FECHA) - min(powr$FECHA)) + 1

# Clean datos -------------------------------------------------------------

fire <- fire %>%
  rename(year = matches('^A.O$'), month = MES, frp.class = starts_with('POTENCIAL')) %>%
  rename_with(~ tolower(.x)) %>%
  mutate(acq_date  = as.Date(acq_date),
         frp.class = str_squish(frp.class),
         frp.class = factor(frp.class, levels = c('<10 MW', '10-<50 MW', '50-<100 MW', '\u2265100 MW')),
         satellite = factor(satellite, levels = c('Terra', 'Aqua')))

fire |> dplyr::select(frp.class) |> pull() |> table()

## Keep vegetation fires (type 0) with nominal / high confidence (>= 30)
frcl <- fire %>% filter(type == 0, confidence >= 30)
cat('Removed: ', nrow(fire) - nrow(frcl), ' detections (type != 0 or confidence < 30)\n')

## power
powr <- powr %>%
  mutate(date = as.Date(DOY - 1, origin = paste0(YEAR, '-01-01'))) %>%
  transmute(date, year = YEAR, month = month(date),
            tmean = T2M, tmax = T2M_MAX, tmin = T2M_MIN, rhum = RH2M,
            wind = WS2M, prec = PRECTOTCORR, srad = ALLSKY_SFC_SW_DWN)
