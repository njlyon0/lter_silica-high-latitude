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

#clean site names
high_lat_annual <- high_lat_annual %>%
  #change to lower case
  mutate(plot_site_name = tolower(Stream_Name)) %>%
  #only keep text before "stream" or "river"
  mutate(plot_site_name = gsub("(stream|river|creek).*","",plot_site_name)) %>%
  #change to sentence case
  mutate(plot_site_name = str_to_title(plot_site_name))

#create df of fills for consistency between plots
high_lat_annual_colors <- high_lat_annual %>%
  mutate(LTER_fill = case_when(LTER=="Canada"~"#003f5c",
                               LTER=="Finnish Environmental Institute"~"#374c80",
                               LTER=="GRO"~"#7a5195",
                               LTER=="Krycklan"~"#bc5090",
                               LTER=="MCM"~"#ef5675",
                               LTER=="NIVA"~"#ff764a",
                               LTER=="Swedish Government"~"#ffa600")) %>%
  select(plot_site_name,LTER_fill) %>%
  distinct()

## ---------------------------------------- ##
## Boxplots!
## ---------------------------------------- ##
dsi <- 
high_lat_annual %>% filter(chemical=="DSi") %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),width=1)+
  scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,300),name="DSi concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+ #switch="x"; will move facet labels under x-axis
  theme_classic(base_size=14)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=10),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.placement="outside",
        strip.background=element_blank())

din <- 
  high_lat_annual %>% filter(chemical=="DIN") %>%
  #add placeholder data for missing Canada sites?
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),width=1)+
  scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,250),name="DIN concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+
  theme_classic(base_size=14)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=10),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.placement="outside",
        strip.background=element_blank())

dip <- 
  high_lat_annual %>% filter(chemical=="P") %>%
  #add placeholder data for missing Canada sites?
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),width=1)+
  scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,2.5),name="DIP concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+
  theme_classic(base_size=14)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=10),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.placement="outside",
        strip.background=element_blank())

#patchwork together
dsi/din/dip
