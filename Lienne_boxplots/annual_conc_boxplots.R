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
#fix spelling of "Swedish Goverment"; shorten Canada for plotting
high_lat_annual <- high_lat_annual %>% mutate(LTER = case_when(LTER=="Swedish Goverment"~"Swedish Government",
                                                               LTER=="Canada"~"CAN",
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

#create blank data to align patchwork plots
#get list of all sites
site_list <- high_lat_annual %>% distinct(LTER,Stream_Name,plot_site_name)
## ---------------------------------------- ##
## Boxplots!
## ---------------------------------------- ##
dsi <- 
high_lat_annual %>% filter(chemical=="DSi") %>%
  full_join(site_list) %>%
  mutate(Conc_uM = case_when(is.na(chemical)~0, #concentration needs to be 0 to plot with facet_grid... we'll need to remove these with illustrator
                             T~Conc_uM)) %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),linewidth=0.05,outlier.size=0.3)+
  scale_fill_manual(values=c("#0099e0","#7c92ef","#c883e4","#ff72c1","#ff6e8d","#ff8452","#ffa600"))+
  scale_y_continuous(limits=c(0,300),name="DSi concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+ #switch="x"; will move facet labels under x-axis
  theme_classic(base_size=12)+
  theme(axis.text.x=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.placement="outside",
        strip.background=element_blank()) #panel.border=element_rect(fill=NA); if we want to box in each LTER

din <- 
  high_lat_annual %>% filter(chemical=="DIN") %>%
  full_join(site_list) %>% 
  mutate(Conc_uM = case_when(is.na(chemical)~0, #concentration needs to be 0 to plot with facet_grid... we'll need to remove these with illustrator
                             T~Conc_uM)) %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),linewidth=0.05,outlier.size=0.3)+
  scale_fill_manual(values=c("#0099e0","#7c92ef","#c883e4","#ff72c1","#ff6e8d","#ff8452","#ffa600"))+
  scale_y_continuous(limits=c(0,250),name="DIN concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free")+
  theme_classic(base_size=12)+
  theme(axis.text.x=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.text=element_blank())

dip <- 
  high_lat_annual %>% filter(chemical=="P") %>%
  full_join(site_list) %>%
   mutate(Conc_uM = case_when(is.na(chemical)~0, #concentration needs to be 0 to plot with facet_grid... we'll need to remove these with illustrator
                              T~Conc_uM)) %>%
  ggplot()+
  geom_boxplot(aes(x=plot_site_name,y=Conc_uM,fill=LTER),linewidth=0.05,outlier.size=0.3)+
  scale_fill_manual(values=c("#0099e0","#7c92ef","#c883e4","#ff72c1","#ff6e8d","#ff8452","#ffa600"))+
  scale_x_discrete(drop=F)+
  scale_y_continuous(limits=c(0,2.5),name="DIP concentration (uM)")+
  facet_grid(~LTER,scales="free_x",space="free",drop=F)+
  theme_classic(base_size=12)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=5),
        legend.position="none",
        axis.title.x=element_blank(),
        strip.text=element_blank())

#cowplot together
cowplot::plot_grid(dsi,din,dip, nrow=3, rel_heights=c(1,1,1.2))

#which sites are missing for each variable?
dsi_sites <- high_lat_annual %>% filter(chemical=="DSi") %>% distinct(LTER,Stream_Name)
din_sites <- high_lat_annual %>% filter(chemical=="DIN") %>% distinct(LTER,Stream_Name)
dip_sites <- high_lat_annual %>% filter(chemical=="P") %>% distinct(LTER,Stream_Name)

supportR::diff_check(old=high_lat_annual$Stream_Name,
                     new=dip_sites$Stream_Name)

high_lat_annual %>% filter(!Stream_Name %in% din_sites$Stream_Name) %>% distinct(LTER,plot_site_name)
