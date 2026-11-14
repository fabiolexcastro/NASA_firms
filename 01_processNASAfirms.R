

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

# Analisis temporal -------------------------------------------------------

## Annual detections by satellite
anul <- frcl %>% count(year, satellite)
g.anl <- ggplot(anul, aes(x = year, y = n, fill = satellite)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = 'Annual MODIS detections', subtitle = 'Aqua starts in July 2002; 2000 only covers Nov-Dec',
       x = NULL, y = 'Detections', fill = NULL) +
  thme
g.anl

ggsave(plot = g.anl, filename = './graphs/eda_fire_satellite.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)

## Trend on Terra only
trra <- anul %>% filter(satellite == 'Terra', year >= 2001)
mken <- cor.test(trra$year, trra$n, method = 'kendall')
mken

# Monthly climatology -----------------------------------------------------
mndt <- frcl %>%
  filter(year >= 2001) %>%
  count(year, month) %>%
  complete(year, month = 1:12, fill = list(n = 0))

g.mnt <- ggplot(mndt, aes(x = factor(month), y = n)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, fill = '#cde2fb', color = '#52514e') +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = '#2a78d6') +
  scale_x_discrete(labels = month.abb) +
  labs(title = 'Monthly distribution of detections', subtitle = 'One point per year, 2001-2025',
       x = NULL, y = 'Detections / month') +
  thme

g.mnt
ggsave(plot = g.mnt, filename = './graphs/dist_monthly_fires.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)

## Year x month heatmap
hmap <- frcl %>%
  count(year, month) %>%
  complete(year = 2000:2025, month = 1:12, fill = list(n = 0))

g.hmp <- hmap %>%
  mutate(n = na_if(n, 0)) %>%
  ggplot(aes(x = factor(month), y = factor(year), fill = n)) +
  geom_tile(color = '#fcfcfb', linewidth = 0.4) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1, trans = 'sqrt', na.value = '#e1e0d9') +
  scale_x_discrete(labels = month.abb) +
  labs(title = 'Detections by year and month', subtitle = 'Grey = no detections',
       x = NULL, y = NULL, fill = 'Detections') +
  thme + theme(panel.grid = element_blank(), legend.position = 'right')
g.hmp
ggsave(plot = g.hmp, filename = './graphs/heatmap_detecciones_fires.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)

## Monthly anomalies
anml <- mndt %>%
  group_by(month) %>%
  mutate(clim = mean(n),
         anom = n - clim) %>%
  ungroup()
anml

g.anm <- ggplot(anml, aes(x = factor(month), y = factor(year), fill = anom)) +
  geom_tile(color = '#fcfcfb', linewidth = 0.4) +
  scale_fill_gradient2(low = '#2a78d6', mid = '#f0efec', high = '#eb6834', midpoint = 0) +
  scale_x_discrete(labels = month.abb) +
  labs(title = 'Monthly anomalies of detections', subtitle = 'Difference from the 2001-2025 monthly mean',
       x = NULL, y = NULL, fill = 'Anomaly') +
  thme + theme(panel.grid = element_blank(), legend.position = 'right', legend.key.height = unit(2.3, 'line'))
g.anm
ggsave(plot = g.anm, filename = './graphs/anomalies_detections.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)

# Fire intensity ----------------------------------------------------------
summary(frcl$frp)
quantile(frcl$frp, c(0.5, 0.9, 0.99))
frcl %>% count(frp.class) %>% mutate(prc = n / sum(n) * 100)




