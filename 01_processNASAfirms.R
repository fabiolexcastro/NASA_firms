

# Libraries ----------------------------------------------------------------
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

## Share of total radiative power from the top 5% of detections
frcl %>%
  arrange(desc(frp)) %>%
  mutate(cums = cumsum(frp) / sum(frp), rank = row_number() / n()) %>%
  filter(rank <= 0.05) %>%
  summarise(frp.share.top5 = max(cums))

g.frp <- ggplot(frcl, aes(x = frp, fill = daynight)) +
  geom_histogram(bins = 40, color = '#fcfcfb', linewidth = 0.2) +
  scale_x_log10(labels = label_number()) +
  scale_fill_manual(values = c('D' = '#2a78d6', 'N' = '#eb6834'), labels = c('Day', 'Night')) +
  labs(title = 'FRP distribution', subtitle = 'Log scale - strongly right-skewed',
       x = 'FRP (MW)', y = 'Detections', fill = NULL) +
  thme
g.frp

ggsave(plot = g.frp, filename = './graphs/frp_distributions.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)

# Spatial pattern ---------------------------------------------------------

##
p_load(MetBrewer)

##
## Boundaries (geoBoundaries, simplified)
root.bnd <- 'https://media.githubusercontent.com/media/wmgeolab/geoBoundaries/main/releaseData/gbOpen'
read.bnd <- function(iso3, levl){
  # iso3 <- 'ECU'; levl <- 'ADM1'
  url <- glue::glue('{root.bnd}/{iso3}/{levl}/geoBoundaries-{iso3}-{levl}_simplified.geojson')
  bnd <- st_read(url, quiet = T) %>% st_make_valid()
  return(bnd)
}

ecu1 <- read.bnd('ECU', 'ADM1')
ecu2 <- read.bnd('ECU', 'ADM2')

loja <- ecu1 %>% filter(shapeName == 'Loja')
cntn <- ecu2 %>% filter(st_intersects(st_point_on_surface(geometry), loja, sparse = F)[, 1])

g.map <- ggplot() +
  geom_hex(data = frcl, aes(x = longitude, y = latitude), bins = 45, color = NA) +
  geom_sf(data = cntn, fill = NA, color = 'grey55', linewidth = 0.2) +
  geom_sf(data = loja, fill = NA, color = 'grey20', linewidth = 0.4) +
  scale_fill_met_c('Tam') +
  coord_sf() +
  labs(title = 'Detection density',
       subtitle = 'Fire activity concentrates in western Loja (Pindal, Celica, Zapotillo)',
       caption = 'MODIS active fires 2000-2025, hexagonal bins (~5 km)',
       x = NULL, y = NULL, fill = 'Detections') +
  thme + theme(legend.position = 'right', axis.text = element_blank(), legend.key.height = unit(2, 'line'))
g.map
ggsave(plot = g.map, filename = './graphs/spatial-hex-pattern_distributions.png', units = 'in', width = 8, height = 6, dpi = 300, create.dir = T)


# Weather on fire days vs non-fire days -----------------------------------
daly <- frcl %>%
  group_by(date = acq_date) %>%
  summarise(n.fire = n(), frp.sum = sum(frp), .groups = 'drop')

dywt <- powr %>%
  left_join(daly, by = 'date') %>%
  mutate(n.fire  = replace_na(n.fire, 0),
         frp.sum = replace_na(frp.sum, 0),
         fire.day = if_else(n.fire > 0, 'Fire day', 'No fire'))

## Compare within the fire season only (Aug-Dec) to avoid a pure seasonal contrast
ssn <- dywt %>% filter(month %in% 8:12)

cmpr <- ssn %>%
  select(fire.day, tmax, rhum, wind, prec, srad) %>%
  pivot_longer(-fire.day, names_to = 'var', values_to = 'value') %>%
  group_by(var, fire.day) %>%
  summarise(median = median(value), .groups = 'drop') %>%
  pivot_wider(names_from = fire.day, values_from = median)
cmpr

# Weather on fire vs non-fire days - figure -------------------------------
clfr <- c('Fire day' = '#de4f33', 'No fire' = '#8b8b99')

## Median and Wilcoxon p-value per variable, used for the labels
stts <- ssn %>%
  select(fire.day, tmax, rhum, wind, srad) %>%
  pivot_longer(-fire.day, names_to = 'var', values_to = 'value') %>%
  group_by(var) %>%
  summarise(md.fire = median(value[fire.day == 'Fire day']),
            md.none = median(value[fire.day == 'No fire']),
            p.value = wilcox.test(value ~ fire.day)$p.value,
            .groups = 'drop')

make.dens <- function(vrbl, ttle, unit){
  # vrbl <- 'tmax'; ttle <- 'Maximum temperature'; unit <- '\u00b0C'
  cat('Processing: ', vrbl, '\n')
  dta <- ssn %>% select(fire.day, value = all_of(vrbl))
  stt <- stts %>% filter(var == vrbl)
  dff <- stt$md.fire - stt$md.none
  sgn <- if_else(stt$p.value < 0.001, 'p < 0.001', paste0('p = ', round(stt$p.value, 2)))
  sbt <- paste0('Median difference: ', if_else(dff > 0, '+', ''), round(dff, if_else(abs(dff) < 0.1, 2, 1)), ' ', unit, ' (', sgn, ')')
  gpl <- ggplot(dta, aes(x = value, fill = fire.day, color = fire.day)) +
    geom_density(alpha = 0.30, linewidth = 0.6, adjust = 1.2) +
    geom_vline(data = tibble(fire.day = c('Fire day', 'No fire'), md = c(stt$md.fire, stt$md.none)),
               aes(xintercept = md, color = fire.day), linetype = 'dashed', linewidth = 0.5) +
    scale_fill_manual(values = clfr) +
    scale_color_manual(values = clfr) +
    labs(title = ttle, subtitle = sbt, x = unit, y = NULL, fill = NULL, color = NULL) +
    thme +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 11), plot.subtitle = element_text(size = 8.5))
  return(gpl)
}

## To apply the fucntion
g.tmx <- make.dens(vrbl = 'tmax', ttle = 'Maximum temperature', unit = '\u00b0C')
g.rhm <- make.dens(vrbl = 'rhum', ttle = 'Relative humidity', unit = '%')
g.srd <- make.dens(vrbl = 'srad', ttle = 'Solar radiation', unit = 'MJ/m\u00b2/day')
g.wnd <- make.dens(vrbl = 'wind', ttle = 'Wind speed', unit = 'm/s')

## Join the density graphs into only one
g.dns <- (g.tmx + g.rhm) / (g.srd + g.wnd) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = 'Weather on fire vs non-fire days',
                  subtitle = 'Loja, fire season (Aug-Dec) 2000-2025 \u00b7 NASA POWER daily data \u00b7 dashed lines = medians',
                  theme = theme(plot.title = element_text(face = 'bold', size = 15),
                                plot.subtitle = element_text(color = '#52514e', size = 10))) &
  theme(legend.position = 'bottom')

ggsave(plot = g.dns, filename = file.path('./graphs', 'weather_density_4panels.png'),
       units = 'in', width = 9, height = 8, dpi = 300, bg = 'white')

## Precipitation: skewed with many dry days -> share of days by rain class
prcl <- ssn %>%
  mutate(rain = cut(prec, breaks = c(-Inf, 0.1, 1, 5, Inf), labels = c('Dry (< 0.1 mm)', '0.1 - 1 mm', '1 - 5 mm', '> 5 mm'))) %>%
  count(fire.day, rain) %>%
  group_by(fire.day) %>%
  mutate(prc = n / sum(n)) %>%
  ungroup() %>%
  mutate(rain = fct_rev(rain))

## Wilcoxon test per variable
wlcx <- ssn %>%
  select(fire.day, tmax, rhum, wind, prec, srad) %>%
  pivot_longer(-fire.day, names_to = 'var', values_to = 'value') %>%
  group_by(var) %>%
  summarise(p.value = wilcox.test(value ~ fire.day)$p.value, .groups = 'drop')

g.prc <- ggplot(prcl, aes(x = prc, y = fire.day, fill = rain)) +
  geom_col(width = 0.6, color = '#fcfcfb', linewidth = 0.6) +
  geom_text(aes(label = if_else(prc >= 0.04, percent(prc, accuracy = 1), ''), group = rain,
                color = if_else(rain %in% c('Dry (< 0.1 mm)', '0.1 - 1 mm'), '#0b0b0b', '#ffffff')),
            position = position_stack(vjust = 0.5), size = 3, fontface = 'bold') +
  scale_color_identity() +
  scale_fill_manual(values = c('Dry (< 0.1 mm)' = '#f2c88f', '0.1 - 1 mm' = '#92b4d4', '1 - 5 mm' = '#4f7cac', '> 5 mm' = '#1f3f66'),
                    breaks = c('Dry (< 0.1 mm)', '0.1 - 1 mm', '1 - 5 mm', '> 5 mm')) +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  labs(title = 'Precipitation', subtitle = 'Share of days by daily rainfall class', x = NULL, y = NULL, fill = NULL) +
  thme +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 8.5), legend.position = 'bottom', legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 1))

ggsave(plot = g.prc, filename = './graphs/prec_firedays.jpg', units = 'in', width = 8, height = 5, dpi = 300, create.dir = T)


##

g.wth <- (g.tmx + g.rhm) / (g.srd + g.wnd) / g.prc +
  plot_layout(guides = 'collect', heights = c(1, 1, 0.75)) +
  plot_annotation(title = 'Weather on fire vs non-fire days',
                  subtitle = 'Loja, fire season (Aug-Dec) 2000-2025 \u00b7 NASA POWER daily data \u00b7 dashed lines = medians',
                  theme = theme(plot.title = element_text(face = 'bold', size = 15), plot.subtitle = element_text(color = '#52514e', size = 10))) &
  theme(legend.position = 'bottom')

ggsave(plot = g.wth, filename = file.path('./graphs', 'weather_fire_vs_nofire.png'), units = 'in', width = 9, height = 10, dpi = 300, bg = 'white')
