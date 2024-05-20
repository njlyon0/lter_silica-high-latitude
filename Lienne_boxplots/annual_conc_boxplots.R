## ------------------------------------------------------- ##
# Annual WRTDS Data - Creating summary boxplots
## ------------------------------------------------------- ##
# Written by: Lienne Sethna

# PURPOSE:
## Make boxplot visualizations for *annual* WRTDS data

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, patchwork)

# Make a folder for exporting graphs
getwd()
setwd("C:/Users/lsethna_smm/Documents/GitHub/lter_silica-high-latitude/Lienne_boxplots")
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
# Data Prep ----
## ----------------------------------------- ##
annual <- read.csv("Full_Results_WRTDS_annual.csv")
glimpse(annual)
length(unique(annual$Stream_Name))
length(unique(annual$LTER))

#filter to only high latitude sites
high_lat_lter <- c("Canada","Finnish Environmental Institute","GRO","Krycklan","MCM","NIVA","Swedish Government")
high_lat_annual <- annual %>% filter(LTER %in% high_lat_lter)
unique(high_lat_annual$LTER) #where is Swedish Government data?
unique(high_lat_annual$chemical)

#add column for fills for consistency between plots
high_lat_annual <-
high_lat_annual %>% mutate(LTER_fill = case_when(LTER=="Canada"~"#003f5c",
                                                 LTER=="Finnish Environmental Institute"~"#374c80",
                                                 LTER=="GRO"~"#7a5195",
                                                 LTER=="Krycklan"~"#bc5090",
                                                 LTER=="MCM"~"#ef5675",
                                                 LTER=="NIVA"~"#ff764a",
                                                 LTER=="Swedish Government"~"#ffa600"))

## ---------------------------------------- ##
## Boxplots!
## ---------------------------------------- ##
high_lat_annual %>% filter(chemical=="DSi") %>%
  filter(LTER=="Canada") %>%
  ggplot(aes(x=Stream_Name,y=Conc_uM,fill=LTER_fill))+
  geom_boxplot()+
  scale_fill_manual(values=high_lat_annual$LTER_fill)+
  theme(axis.text.x=element_text(angle=90),
        legend.position="none")+
  facet_grid(~LTER)
