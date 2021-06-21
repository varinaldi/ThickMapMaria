library(sf)
library(sp)
library(rgdal)
library(leaflet)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtern)
library(tricolore)
library(shiny)
library(grid)
library(png)
library(stringr)
library(Ternary)
library(compositions)
library(biscale)
library(cowplot)
library(mapview)


source('functions.R')
#source('apitest.R')

## get geography
f= file.path('geography',list.files('geography', pattern = '.rds'))
# geo = readRDS(f %>% str_subset('municipio'))

# SenseMaker Data

# version 3
v3 = read.csv('ERIC_CRISPv3_Standard.csv') %>% wrangle()


# version 4
#v4 = ERIC_CRISP %>%
#  rename(X1.3.Story.relates.to_before.the.event..hurricane.and.or.earthquakes. ='X1.3.Story.relates.to_before.the.event..Maria.Ponce.earthqukes.' )



# the ones without coordinate
#v4_nocoord = v4 %>% filter(str_starts(X7.3.Zip, '18', negate = T)) %>%
  filter(str_starts(X7.3.Zip, '17', negate = T))



# ones with coordinates
#options(digits = 14)
#v4_coord = v4 %>% filter(str_starts(X7.3.Zip, '18' )) %>%
  bind_rows(filter(v4, str_starts(X7.3.Zip, '17' )) ) %>%
  mutate(X7.3.Zip = X7.3.Zip %>% str_sub(1,37)) %>% wrangle()



# see difference in colnames
#names(v4_coord) %>% setdiff(., names(v3))


sm = v3
#%>% mutate(v = 'Maria') %>%
#  bind_rows(v4_coord %>% mutate(v = 'Earthquake 2020')) %>% getPoints()
