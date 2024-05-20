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
librarian::shelf(tidyverse, googledrive, cowplot)

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
high_lat_lter <- c("Canada","Finnish Environmental Institute","GRO","Krycklan","MCM","NIVA","Swedish Goverment")
high_lat_annual <- annual %>% filter(LTER %in% high_lat_lter) %>%  
  dplyr::filter(!Stream_Name %in% c("Site 69038", "Kymijoki Ahvenkoski 001", "Kymijoki Kokonkoski 014",
                                    "BEAVER RIVER ABOVE HIGHWAY 1 IN GLACIER NATIONAL PARK",
                                    "KICKING HORSE RIVER AT FIELD IN YOHO NATIONAL PARK", 
                                    "SKEENA RIVER AT USK","KOOTENAY RIVER ABOVE HIGHWAY 93 IN KOOTENAY NATIONAL PARK",
                                    "Helgean Hammarsjon", "Ronnean Klippan", "Morrumsan Morrum", "Lyckebyan Lyckeby",
                                    "Lagan Laholm", "Nissan Halmstad", "Atran Falkenberg", "Alsteran Getebro", "Eman Emsfors",
                                    "Viskan Asbro", "Gota Alv Trollhattan"))
unique(high_lat_annual$LTER)
unique(high_lat_annual$chemical)
#fix spelling of "Swedish Goverment"
high_lat_annual <- high_lat_annual %>% mutate(LTER = case_when(LTER=="Swedish Goverment"~"Swedish Government",
                                                               T~LTER))

#clean site names
high_lat_annual <- high_lat_annual %>%
  #change to lower case
  mutate(plot_site_name = tolower(Stream_Name)) %>%
  #only keep text before "stream" or "river"
  mutate(plot_site_name = gsub("(stream|river|creek).*","",plot_site_name)) %>%
  #change to sentence case
  mutate(plot_site_name = str_to_title(plot_site_name))

#create df of fills for consistency between plots
#we really don't end up using this...
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

#create blank Canada data to align patchwork plots
glimpse(high_lat_annual)
canada_blank_DIN <- high_lat_annual %>%
  filter(LTER=="Canada") %>% select(LTER,Stream_Name,plot_site_name) %>% distinct() %>% #get only canada sites
  mutate(chemical="DIN",
         Conc_uM=0)
canada_blank_DIP <- high_lat_annual %>%
  filter(LTER=="Canada") %>% select(LTER,Stream_Name,plot_site_name) %>% distinct() %>% #get only canada sites
  mutate(chemical="P",
         Conc_uM=0)
canada_blank = rbind(canada_blank_DIN,canada_blank_DIP)

## ---------------------------------------- ##
## Boxplots!
## ---------------------------------------- ##
dsi <- 
high_lat_annual %>% filter(chemical=="DSi") %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER))+
  scale_fill_manual(values=c("#006999","#586db4","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,300),name="DSi concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+ #switch="x"; will move facet labels under x-axis
  theme_classic(base_size=12)+
  theme(axis.text.x=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.placement="outside",
        strip.background=element_blank()) #panel.border=element_rect(fill=NA); if we want to box in each LTER

din <- 
  high_lat_annual %>% 
  #add placeholder data for missing Canada sites?
  full_join(canada_blank) %>% #Kootenay is here - maybe will go away with updated WRTDS results?
  filter(chemical=="DIN") %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER))+
  scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,250),name="DIN concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+
  theme_classic(base_size=12)+
  theme(axis.text.x=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.text=element_blank())

dip <- 
  high_lat_annual %>% 
  #add placeholder data for missing Canada sites?
  full_join(canada_blank) %>%
  filter(chemical=="P") %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER))+
  scale_fill_manual(values=c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a","#ffa600"))+
  scale_y_continuous(limits=c(0,2.5),name="DIP concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+
  theme_classic(base_size=12)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=5),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.text=element_blank())

#cowplot together
cowplot::plot_grid(dsi,din,dip, nrow=3, align="hv")
