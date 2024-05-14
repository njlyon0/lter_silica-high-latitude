##examining annual WRTDS model output results for Si polar paper

setwd("~/LNO_Si_Synthesis/HighLat_2024/lter_silica-high-latitude/Jo_explore")
#Data<-readr::read_csv('Full_Results_WRTDS_monthly.csv')
Data<-readr::read_csv('Full_Results_WRTDS_annual.csv')
names(Data)[names(Data) == "Stream_Name"] <- "stream"


PolarSites <- Data %>%
  # Keep only cryosphere LTERs
  dplyr::filter(LTER %in% c("MCM", "GRO", "NIVA", "Krycklan",
                            "Finnish Environmental Institute", "Canada", "Swedish Goverment")) %>%
  # But drop problem sites that are otherwise retained
  dplyr::filter(!stream %in% c("Site 69038", "Kymijoki Ahvenkoski 001",
                               "Kymijoki Kokonkoski 014",
                               "BEAVER RIVER ABOVE HIGHWAY 1 IN GLACIER NATIONAL PARK",
                               "KICKING HORSE RIVER AT FIELD IN YOHO NATIONAL PARK", 
                               "SKEENA RIVER AT USK",
                               "KOOTENAY RIVER ABOVE HIGHWAY 93 IN KOOTENAY NATIONAL PARK",
                               "Helgean Hammarsjon", "Ronnean Klippan", "Morrumsan Morrum", "Lyckebyan Lyckeby",
                               "Lagan Laholm", "Nissan Halmstad", "Atran Falkenberg", "Alsteran Getebro", "Eman Emsfors",
                               "Viskan Asbro", "Gota Alv Trollhattan")) %>%
  # Calculate number of years
  dplyr::group_by(LTER, stream) %>%
  dplyr::mutate(num_years = length(unique(Year)), .after = Year) %>%
  dplyr::ungroup() %>%
  # Filter to only more than some threshold years
  dplyr::filter(num_years >= 12) %>%
  # Drop that column now that we've used it
  dplyr::select(-num_years) %>%
  # Convert 10^-6 xx to just xx
  dplyr::mutate(Flux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                    yes = (Flux_10_6kg_yr * 10^6),
                                    no = NA),
                FNFlux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (FNFlux_10_6kg_yr * 10^6),
                                      no = NA),
                Flux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (Flux_10_6kmol_yr * 10^6),
                                      no = NA),
                FNFlux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                        yes = (FNFlux_10_6kmol_yr * 10^6),
                                        no = NA),
                Yield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                           yes = (Yield_10_6kmol_yr_km2 * 10^6),
                                           no = NA),
                FNYield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                             yes = (FNYield_10_6kmol_yr_km2 * 10^6),
                                             no = NA)) %>%
  # Tweak chemical names to exclude `:` in ratio
  dplyr::mutate(chemical = dplyr::case_when(
    chemical == "Si:DIN" ~ "Si_DIN",
    chemical == "Si:P" ~ "Si_P",
    TRUE ~ chemical))

names(PolarSites)
####=================================================
#this new column allows one to have boxplots sorted by lter, then stream
PolarSites$LTERStream<-paste0(stringr::str_sub(PolarSites$LTER,1,3),"_",PolarSites$stream)
PolarSites$LTERStreamshort<-paste0(stringr::str_sub(PolarSites$LTER,1,3),"_",
                                   paste0(stringr::str_sub(PolarSites$stream,1,3)))

#this new column allows one to have boxplots with shorter stream end
PolarSites$streamshort<-paste0(stringr::str_sub(PolarSites$stream,1,13))

#chemicals and time frames for each LTER
#let's group by site and then summarise
names(Si_Polar)
Si_Polar<-subset(PolarSites, PolarSites$chemical=="DSi")
N_Polar<-subset(PolarSites, PolarSites$chemical=="DIN")
P_Polar<-subset(PolarSites, PolarSites$chemical=="P")
unique(Si_Polar$stream)
Si_Polar$Qnorm<-Si_Polar$Discharge_cms/Si_Polar$drainSqKm

SiSumStats_by_LTER<-Si_Polar %>% 
  group_by(LTER) %>%
  summarise(streams=length(unique(stream)),
    minYear=min(Year),
          maxYear=max(Year),
          duration_yrs=length(unique(Year)),
          minDA_km2=min(drainSqKm),
          maxDA=max(drainSqKm),
          meanQ_cms=mean(Discharge_cms),
          sdDQ=sd(Discharge_cms),
          meanQNorm_cms_km2=mean(Qnorm),
          sdDQNorm=sd(Qnorm),
          minDSiConc=min(Conc_uM),
          maxDSiConc=max(Conc_uM),
          meanDSiConc_uM=mean(Conc_uM),
          sdDSiConc=sd(Conc_uM),
          minDSiYield=min(Yield_kmol_yr_km2),
          maxDSiYield=max(Yield_kmol_yr_km2),
          meanDSiYield_kmol_yr_km2=mean(Yield_kmol_yr_km2),
          sdDSiYield=sd(Yield_kmol_yr_km2))

SiSumStats_by_Stream<-Si_Polar %>% 
  group_by(LTER, stream) %>%
  summarise(minYear=min(Year),
            maxYear=max(Year),
            duration_yrs=length(unique(Year)),
            DA_km2=min(drainSqKm),
            meanQ_cms=mean(Discharge_cms),
            sdDQ=sd(Discharge_cms),
            meanQNorm_cms_km2=mean(Qnorm),
            sdDQNorm=sd(Qnorm),
            minDSiConc=min(Conc_uM),
            maxDSiConc=max(Conc_uM),
            meanDSiConc_uM=mean(Conc_uM),
            sdDSiConc=sd(Conc_uM),
            minDSiYield=min(Yield_kmol_yr_km2),
            maxDSiYield=max(Yield_kmol_yr_km2),
            meanDSiYield_kmol_yr_km2=mean(Yield_kmol_yr_km2),
            sdDSiYield=sd(Yield_kmol_yr_km2))

#summary stats for N and P by LTER and stream
NSumStats_by_Stream<-N_Polar %>% 
  group_by(LTER, stream) %>%
  summarise(minYear=min(Year),
            maxYear=max(Year),
            duration=length(unique(Year)),
            meanConc_uM=mean(Conc_uM),
            sdConc=sd(Conc_uM),
            meanYield_kmol_yr_km2=mean(Yield_kmol_yr_km2),
            sdYield=sd(Yield_kmol_yr_km2))

PSumStats_by_Stream<-P_Polar %>% 
  group_by(LTER, stream) %>%
  summarise(minYear=min(Year),
            maxYear=max(Year),
            duration=length(unique(Year)),
            meanConc_uM=mean(Conc_uM),
            sdConc=sd(Conc_uM),
            meanYield_kmol_yr_km2=mean(Yield_kmol_yr_km2),
            sdYield=sd(Yield_kmol_yr_km2))
  
#Exporting to excel
write.csv(SiSumStats_by_LTER, file="Si_LTERSummary_WRTDSOuput_1.7.23.csv", row.names=FALSE)
write.csv(SiSumStats_by_Stream, file="Si_StreamSummary_WRTDSOuput_11.8.23_old.csv", row.names=FALSE)
write.csv(PSumStats_by_Stream, file="PSummary_WRTDSOuput_allStreams_11.8.23_old.csv", row.names=FALSE)
write.csv(NSumStats_by_Stream, file="NSummary_WRTDSOuput_allStreams_11.8.23_old.csv", row.names=FALSE)

names(Si_Polar)

##=====================================
##=====================================

#boxplots Si conc across each stream
#updated 5.14.24

#Si
# Count numbers of streams at each LTER
core_vlines <- Si_Polar %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(Si_Polar, aes(x=LTERStreamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DSi Conc (uM) each stream") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Stream name") +
  ylab("DSi concentration (uM)")+
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5)) 

ggsave(paste0("Boxplots Modelled DSi Conc all rivers",Sys.Date(),".png"), width=10, height=5)



#DIN
core_vlines <- N_Polar %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(N_Polar, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIN Conc (uM) each stream") +
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5)) 

#P
core_vlines <- P_Polar %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(P_Polar, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIP Conc (uM) each stream") +
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5)) 

####=================================================
#exploratory plots - DSi trends through time and boxplots for various consitutents
#removed outliers in some cases so re-import after this section

#remanming Polar sites to Cryosites so i don't have to re-write code
CryoSites<-PolarSites

#creating column for area-normalized discharge
CryoSites$Q_norm<-CryoSites$Discharge_cms/CryoSites$drainSqKm

#this new column allows one to have boxplots with shorter stream end
CryoSites$streamshort<-paste0(stringr::str_sub(CryoSites$stream,1, 10))

#this new column allows one to have boxplots sorted by lter, then stream
CryoSites$LTERStream<-paste0(stringr::str_sub(CryoSites$LTER,1,3),"_",CryoSites$streamshort)



#if want all boxplots together, need to separate out MCM and Kry for DIN:Si ratios
CryoSites2<-subset(CryoSites, CryoSites$LTER!="MCM")
#CryoSites3<-subset(CryoSites, CryoSites$LTER!="MCM" & CryoSites$LTER!="Krycklan")
MCM<-subset(CryoSites, CryoSites$LTER=="MCM")
#MCM_Kry<-subset(CryoSites, CryoSites$LTER=="MCM" | CryoSites$LTER=="Krycklan")


#subset for various chemicals
unique(CryoSites$chemical)
Si_CryoSites<-subset(CryoSites, CryoSites$chemical=="DSi") #all data incluing MCM
Si_CryoSites2<-subset(CryoSites2, CryoSites2$chemical=="DSi")#all data excluing MCM
Si_MCM<-subset(MCM, MCM$chemical=="DSi")#just MCM

NOx_CryoSites<-subset(CryoSites, CryoSites$chemical=="NOx")
NH4_CryoSites<-subset(CryoSites, CryoSites$chemical=="NH4")

DIN_CryoSites<-subset(CryoSites, CryoSites$chemical=="DIN")
DIN_CryoSites2<-subset(CryoSites2, CryoSites2$chemical=="DIN")#all data excluing MCM
DIN_MCM<-subset(MCM, MCM$chemical=="DIN")#just MCM

DIP_CryoSites<-subset(CryoSites, CryoSites$chemical=="P")
unique(DIP_CryoSites$stream) #not seeing N data Onyx at Lake Vanda Weir, and missing some P values for some Finnish Rv
DIP_CryoSites2<-subset(CryoSites2, CryoSites2$chemical=="P")#all data excluing MCM
DIP_MCM<-subset(MCM, MCM$chemical=="P")#just MCM

unique(CryoSites2$chemical)
#for Si:DIN ratios have to separate out Krycklan and MCM b/c yields so high
Si_DIN_CryoSites<-subset(CryoSites, CryoSites$chemical=="Si_DIN") #all data (yields very diff)
Si_DIN_CryoSites2<-subset(CryoSites2, CryoSites2$chemical=="Si_DIN") #all data excluding MCM 
#Si_DIN_CryoSites3<-subset(CryoSites3, CryoSites3$chemical=="Si_DIN") #all data excluding MCM & Krycklan
Si_DIN_MCM<-subset(MCM, MCM$chemical=="Si_DIN") #just MCM

Si_DIP_CryoSites<-subset(CryoSites, CryoSites$chemical=="Si_P")
Si_DIP_CryoSites2<-subset(CryoSites2, CryoSites2$chemical=="Si_P") #all data excluding MCM 
Si_DIP_MCM<-subset(MCM, MCM$chemical=="Si_P") #just MCM


#Si boxplots12.22####
Si_theme<-theme(legend.position="none", 
                axis.text.x=element_text(angle=90, hjust=1, vjust=0.3))

#boxplots except MCM b/c Q norm and Yield off chart
ggplot(Si_CryoSites2, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DSi Conc (uM) each LTER")+
  geom_vline(xintercept=24.5, linetype="longdash")+
  geom_vline(xintercept=2.5, linetype="longdash")+
  geom_vline(xintercept=30.5, linetype="longdash")+
  geom_vline(xintercept=40.5, linetype="longdash")+
  geom_vline(xintercept=48.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots Modelled DSi Conc uM by LTER and stream_noMCM_annual",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_DIP_CryoSites, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si:DIP Conc (uM) each stream")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=44.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=38.5, linetype="longdash")+
  geom_vline(xintercept=52.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots annual Si_DIP conc",Sys.Date(),".png"), width=12, height=5)


ggplot(Si_CryoSites2, aes(x=LTERStream, y=Q_norm, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Q norm each LTER")+
  #geom_vline(xintercept=1.5, linetype="longdash")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots Modelled Q norm by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_CryoSites2, aes(x=LTERStream, y=Yield_kmol_yr_km2, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si Yield each LTER")+
  #geom_vline(xintercept=1.5, linetype="longdash")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  #geom_vline(xintercept=38.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots Modelled DSi Yield by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)


#boxplots for MCM 
ggplot(MCM, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled DSi Conc uM MCM",Sys.Date(),".png"), width=2, height=5)

#what is going on w/ MCM - boxplots look messed up - only messed up on monthly data. annual looks fine
names(MCM)
ggplot(MCM, aes(Conc_uM))+geom_histogram()+ggtitle("Histograms - Modelled DSi Conc (uM) MCM")+
  facet_wrap(~stream, scales="free") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave(paste0("Histograms_MCM Si conc annual output", Sys.Date(),".png"), width=8, height=5)

ggplot(MCM, aes(Year, Conc_uM))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Si Conc over time - MCM")
ggsave(paste0("Si Conc_bySite_Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)


ggplot(MCM3, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled DSi Conc uM MCM_Oynx",Sys.Date(),".png"), width=2, height=5)

ggplot(MCM, aes(x=LTERStream, y=Q_norm, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled Q norm MCM ",Sys.Date(),".png"), width=2, height=5)

ggplot(MCM, aes(x=LTERStream, y=Yield_kmol_yr_km2, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled DSi Yield MCM ",Sys.Date(),".png"), width=2, height=5)


###boxplots for DIN & DIP conc ####
ggplot(DIN_CryoSites2, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIN Concentration each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIN Conc uM by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(DIP_CryoSites2, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIP Concentration each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIP Conc uM by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(DIN_MCM, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIN Conc MCM ",Sys.Date(),".png"), width=2, height=5)

ggplot(DIP_MCM, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIP Conc MCM ",Sys.Date(),".png"), width=2, height=5)



####boxplots for ratios of concentrations####
ggplot(Si_DIN_CryoSites2, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si:DIN of conc each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme+
  ylab("") # need to remove y axis label for ratios b/c unitless
ggsave(paste0("Boxplots Modelled Si_DIN ratios of conc by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_DIP_CryoSites2, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si:DIP of conc each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme+
  ylab("") # need to remove y axis label for ratios b/c unitless
ggsave(paste0("Boxplots Modelled Si_DIP ratios of conc by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_DIN_MCM, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme +
  ylab("")
ggsave(paste0("Boxplots Modelled Si_DIN conc ratio MCM ",Sys.Date(),".png"), width=2, height=5)

ggplot(Si_DIP_MCM, aes(x=LTERStream, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme +
  ylab("")
ggsave(paste0("Boxplots Modelled Si_DIP conc ratio MCM ",Sys.Date(),".png"), width=2, height=5)



####boxplots for ratios of fluxes####
#need to plot ratio file for fluxes b/c yield ratios are divided by watershed area so incorrect
ggplot(Si_DIN_CryoSites2, aes(x=LTERStream, y=Flux_10_6kmol_yr, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si:DIN of fluxes each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme+
  ylab("") # need to remove y axis label for ratios b/c unitless
ggsave(paste0("Boxplots Modelled Si_DIN ratios of yields by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_DIP_CryoSites2, aes(x=LTERStream, y=Flux_10_6kmol_yr, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled Si:DIP of fluxes each LTER")+
  geom_vline(xintercept=22.5, linetype="longdash")+
  geom_vline(xintercept=28.5, linetype="longdash")+
  geom_vline(xintercept=29.5, linetype="longdash")+
  Si_theme+
  ylab("") # need to remove y axis label for ratios b/c unitless
ggsave(paste0("Boxplots Modelled Si_DIP ratios of yields by LTER and stream_noMCM_noARC",Sys.Date(),".png"), width=12, height=5)

ggplot(Si_DIN_MCM, aes(x=LTERStream, y=Flux_10_6kmol_yr, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme +
  ylab("")
ggsave(paste0("Boxplots Modelled Si_DIN Flux ratio MCM ",Sys.Date(),".png"), width=2, height=5)

ggplot(Si_DIP_MCM, aes(x=LTERStream, y=Flux_10_6kmol_yr, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("")+
  Si_theme +
  ylab("")
ggsave(paste0("Boxplots Modelled Si_DIP Flux ratio MCM ",Sys.Date(),".png"), width=2, height=5)





#need to use facet wrap for DIN and DIN:Si displays because the concentrations vary so widely - too hard to get them on same boxplot like Si
#DIN
ggplot(DIN_CryoSites, aes(x=streamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled DIN Conc (uM) each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIN Conc all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)

ggplot(DIN_CryoSites, aes(x=streamshort, y=Yield_kmol_yr_km2, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled DIN Yield each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIN Yield all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)


#DIP
ggplot(DIP_CryoSites, aes(x=streamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled DIP Conc (uM) each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIP Conc all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)

ggplot(DIP_CryoSites, aes(x=streamshort, y=Yield_kmol_yr_km2, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled DIP Yield each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled DIP Yield all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)


#Si:DIN
ggplot(Si_DIN_CryoSites, aes(x=streamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled Si:DIN Conc (uM) each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled Si_DIN Conc all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)

ggplot(Si_DIN_CryoSites, aes(x=streamshort, y=Flux_10_6kmol_yr, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+
  ggtitle("Annual Modelled Si:DIN Yield each site by LTER")+
  facet_wrap(~LTER, scales="free")+
  Si_theme
ggsave(paste0("Boxplots Modelled Si_DIN Yield all sites by LTER Facetted ",Sys.Date(),".png"), width=6, height=5)









#looking at distribution of data
ggplot(Si_CryoSites, aes(Conc_uM))+geom_histogram()+ggtitle("Histograms - Modelled DSi Conc (uM) each LTER")+
  facet_wrap(~LTER, scales="free") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Histograms Modelled DSi Conc by LTER 10.29.22.png", width=8, height=5)

ggplot(Si_CryoSites, aes(Yield))+geom_histogram()+ggtitle("Histograms - Modelled DSi Yield (10^6 kg yr-1 km-2) each LTER")+
  facet_wrap(~LTER, scales="free") +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Histograms Modelled DSi Yield by LTER 10.29.22.png", width=8, height=5)

#creating column for area-normalized discharge
Si_CryoSites$Q_norm<-Si_CryoSites$Discharge_cms/Si_CryoSites$drainSqKm

#removing outliers
Si_CryoSites<-subset(Si_CryoSites, Si_CryoSites$Q_norm<.1) #removed 80 values b/c MCM Q norm so high

unique(Si_CryoSites$LTER)

#all sites for each LTER on same plot
ggplot(Si_CryoSites, aes(Year, Conc_uM))+geom_point(aes(color = stream)) +  
  facet_wrap(~LTER, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  theme(legend.position="none")+
  ggtitle("Modelled Si Concentration (uM) - annual average - over time")
ggsave("Modelled Annual Si Conc_byLTER_10.31.22.png", width=8, height=5)


#boxplot of each constituent at each LTER
ggplot(Si_CryoSites, aes(LTER, Conc_uM))+geom_boxplot()+ggtitle("Modelled DSi Conc (uM) each LTER")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Modelled DSi Conc uM by LTER 12.21.22.png", width=8, height=5)

ggplot(Si_CryoSites, aes(LTER, Q_norm))+geom_boxplot()+ggtitle("Modelled Q normalized each LTER with 100 values from MCM removed")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Annual Q norm WRDS output by LTER 12.21.22.png", width=8, height=5)

ggplot(Si_CryoSites, aes(Year, Yield))+geom_point(aes(color = stream)) +  
  facet_wrap(~LTER, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  theme(legend.position="none")+
  ggtitle("Yield over time")
ggsave("Yield_byLTER_10.28.22.png", width=8, height=5)

names(Si_DIN_CryoSites)
ggplot(Si_DIN_CryoSites, aes(LTER, Conc_uM))+geom_boxplot()+ggtitle("Modelled Si:DIN molar ratios each LTER")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Modelled Si_DIN ratios Conc uM by LTER 11.1.22.png", width=8, height=5)

#boxplot of DSi at each stream
ggplot(Si_CryoSites, aes(stream, Conc_uM))+geom_boxplot()+ggtitle("Modelled DSi Conc (uM) each stream")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Modelled DSi Conc uM by LTER 12.21.22.png", width=8, height=5)

ggplot(Si_CryoSites, aes(LTER, Yield))+geom_boxplot()+ggtitle("Modelled DSi Yield (10^6 kg y-1 km-2) each LTER - top values removed from MCM")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Modelled DSi Yield by LTER 12.21.22.png", width=8, height=5)

ggplot(Si_CryoSites, aes(Year, Discharge_cms))+geom_point(aes(color = stream)) +  
  facet_wrap(~LTER, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  theme(legend.position="none")+
  ggtitle("Discharge over time")
ggsave("Discharge_byLTER_10.28.22.png", width=8, height=5)



ggplot(Si_CryoSites, aes(LTER, Q_norm))+geom_boxplot()+ggtitle("Modelled DSi Area-norm Discharge (cms/km2) - top 80 values removed, most from Finn sites, 4 from MCM")+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))
ggsave("Boxplots Modelled Area-norm discharge by LTER 10.29.22.png", width=8, height=5)



##===============================================
##===============================================
#plots change over time - all sites same figure except Finland####
#lets break down Finn from rest to make legible panel plots
Finn_Si<-subset(Si_Polar, Si_Polar$LTER=="Finnish Environmental Institute")
Si_Polar2<-subset(Si_Polar, Si_Polar$LTER!="Finnish Environmental Institute")

Finn_Si_DIN<-subset(Si_DIN_CryoSites, Si_DIN_CryoSites$LTER=="Finnish Environmental Institute")
Si_DIN_CryoSites<-subset(Si_DIN_CryoSites, Si_DIN_CryoSites$LTER!="Finnish Environmental Institute")

Finn_Si_DIP<-subset(Si_DIP_CryoSites, Si_DIP_CryoSites$LTER=="Finnish Environmental Institute")
Si_DIP_CryoSites<-subset(Si_DIP_CryoSites, Si_DIP_CryoSites$LTER!="Finnish Environmental Institute")

Finn_P<-subset(DIP_CryoSites, DIP_CryoSites$LTER=="Finnish Environmental Institute")
DIP_CryoSites<-subset(DIP_CryoSites, DIP_CryoSites$LTER!="Finnish Environmental Institute")

Finn_N<-subset(DIN_CryoSites, DIN_CryoSites$LTER=="Finnish Environmental Institute")
DIN_CryoSites<-subset(DIN_CryoSites, DIN_CryoSites$LTER!="Finnish Environmental Institute")
names(DIN_CryoSites)

#all sites on their own plot
#Si conc
ggplot(Finn_Si, aes(Year, Conc_uM))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Si Conc over time - Finland")
ggsave(paste0("Si Conc_bySite_Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)

ggplot(Si_Polar2, aes(Year, Conc_uM))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Si Conc over time - All sites except Finland")
ggsave(paste0("Si Conc_bySite_all sites except Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)

#Si Yield
ggplot(Si_Polar2, aes(Year, Yield_kmol_yr_km2))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Si Yield over time - except Finland")
ggsave(paste0("Si Yield_bySite_all sites except Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)

ggplot(Finn_Si, aes(Year, Yield_kmol_yr_km2))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Si Yield over time - Finland")
ggsave(paste0("Si Yield_bySite_all sites- Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)

#Dishcarge
ggplot(Si_Polar2, aes(Year, Discharge_cms))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Discharge over time - no Finland")
ggsave(paste0("Q_bySite_all sites- no Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)


ggplot(Finn_Si, aes(Year, Discharge_cms))+geom_point() +  
  facet_wrap(~stream, scales="free_y") + theme(panel.grid.minor = element_blank())+ 
  #geom_smooth(method="lm") + 
  ggtitle("WRTDS Model Results Discharge over time - Finland")
ggsave(paste0("Q_bySite_all sites- Finland_noSmoother", Sys.Date(),".png"), width=8, height=5)


##===================================
##===================================
#Getting handle on which chemical values available for each site
#Si first
Si_Polar<-Si_CryoSites

Si_SumStats_by_site<-Si_Polar %>% 
  group_by(stream) %>%
  summarise(minSiConc=min(Conc_mgL),
            medianSiConc=median(Conc_mgL),
            meanSiConc = mean(Conc_mgL),
            sdSiConc = sd(Conc_mgL),
            maxSiConc = max(Conc_mgL))

Si_DateRange_by_site<-Si_Polar %>% 
  group_by(stream) %>%
  summarise(minDate=min(Year),
            maxDate=max(Year))

Si_counts<-Si_Polar %>% 
  group_by(stream) %>%
  count(chemical)

Si_SumStats_by_Site2<-merge(Si_SumStats_by_site, Si_counts, by="stream")
Si_SumStats_by_Site3<-merge(Si_SumStats_by_Site2, Si_DateRange_by_site, by="stream")


#NOx
NOx_Polar<-subset(PolarSites, PolarSites$chemical=="NOx")
NH4_Polar<-subset(PolarSites, PolarSites$chemical=="NH4")
P_Polar<-subset(PolarSites, PolarSites$chemical=="P")

NOx_SumStats_by_site<-NOx_Polar %>% 
  group_by(stream) %>%
  summarise(minNOxConc=min(Conc_mgL),
            medianNOxConc=median(Conc_mgL),
            meanNOxConc = mean(Conc_mgL),
            sdNOxConc = sd(Conc_mgL),
            maxNOxConc = max(Conc_mgL))

NOx_DateRange_by_site<-NOx_Polar %>% 
  group_by(stream) %>%
  summarise(minDate=min(Year),
            maxDate=max(Year))

NOx_counts<-NOx_Polar %>% 
  group_by(stream) %>%
  count(chemical)

NOx_SumStats_by_Site2<-merge(NOx_SumStats_by_site, NOx_counts, by="stream")
NOx_SumStats_by_Site3<-merge(NOx_SumStats_by_Site2, NOx_DateRange_by_site, by="stream")

##NH4 now
NH4_SumStats_by_site<-NH4_Polar %>% 
  group_by(stream) %>%
  summarise(minNH4Conc=min(Conc_mgL),
            medianNH4Conc=median(Conc_mgL),
            meanNH4Conc = mean(Conc_mgL),
            sdNH4Conc = sd(Conc_mgL),
            maxNH4Conc = max(Conc_mgL))

NH4_DateRange_by_site<-NH4_Polar %>% 
  group_by(stream) %>%
  summarise(minDate=min(Year),
            maxDate=max(Year))

NH4_counts<-NH4_Polar %>% 
  group_by(stream) %>%
  count(chemical)

NH4_SumStats_by_Site2<-merge(NH4_SumStats_by_site, NH4_counts, by="stream")
NH4_SumStats_by_Site3<-merge(NH4_SumStats_by_Site2, NH4_DateRange_by_site, by="stream")


##P now
P_SumStats_by_site<-P_Polar %>% 
  group_by(stream) %>%
  summarise(minPConc=min(Conc_mgL),
            medianPConc=median(Conc_mgL),
            meanPConc = mean(Conc_mgL),
            sdPConc = sd(Conc_mgL),
            maxPConc = max(Conc_mgL))

P_DateRange_by_site<-P_Polar %>% 
  group_by(stream) %>%
  summarise(minDate=min(Year),
            maxDate=max(Year))

P_counts<-P_Polar %>% 
  group_by(stream) %>%
  count(chemical)

P_SumStats_by_Site2<-merge(P_SumStats_by_site, P_counts, by="stream")
P_SumStats_by_Site3<-merge(P_SumStats_by_Site2, P_DateRange_by_site, by="stream")

#Exporting to excel
write.csv(Si_SumStats_by_Site3, file="AnnualModel_DSi_sumstats_10.28.22.csv", row.names=FALSE)
write.csv(P_SumStats_by_Site3, file="AnnualModel_PO4_sumstats_10.28.22.csv", row.names=FALSE)
write.csv(NOx_SumStats_by_Site3, file="AnnualModel_NOx_sumstats_10.28.22.csv", row.names=FALSE)
write.csv(NH4_SumStats_by_Site3, file="AnnualModel_NHx_sumstats_10.28.22.csv", row.names=FALSE)


###==================================
#OLD CODE
#remove unneeded columns
NOx_NH4_Data <-subset(NOx_NH4_Data, select =-c(Discharge_cms.x, drainSqKm.x, 
                                               Discharge_cms.y, drainSqKm.y ))

#rename column names to make more sense
colnames(NOx_NH4_Data)<-c("LTER", "site", "Year", "NOxConc", "NOxFNConc", "NOxFlux", "NOxFNFlux",
                          "NOxYield", "NOxFNYield", "NH4Conc", "NH4FNConc", "NH4Flux", "NH4FNFlux",
                          "NH4Yield", "NH4FNYield", "DINConc", "DINFNConc", "DINFlux", "DINFNFlux",
                          "DINYield", "DINFNYield")
names(NOx_NH4_Data)

#make wide data long for ease of plotting
long_NData<- NOx_NH4_Data %>% gather (Variable, Value, NOxConc, NOxFNConc, NOxFlux, NOxFNFlux,
                                      NOxYield, NOxFNYield, NH4Conc, NH4FNConc, NH4Flux, NH4FNFlux,
                                      NH4Yield, NH4FNYield, DINConc, DINFNConc, DINFlux, DINFNFlux,
                                      DINYield, DINFNYield)

names(long_NData)
head(long_NData)


#converting mg/L to molar for stoich analysis

#re-loading annual Si data for ease:
Si_Data<-readr::read_csv('WRTDS_GFN_AnnualResults_AllSites_062822.csv')

names(Si_Data)
unique(N_Data$site)
Si_Data$FNConc_uM<-Si_Data$FNConc_mgL/28*1000
Si_Data$Conc_uM<-Si_Data$Conc_mgL/28*1000
Si_Data$FNFlux_103_Km_y<-Si_Data$FNFlux_106_kg_y/28*1000
Si_Data$Flux_103_Km_y<-Si_Data$Flux_106_kg_y/28*1000
Si_Data$FNYield_Km_y_km<-Si_Data$FNYield/28*1000000
Si_Data$Yield_Km_y_km<-Si_Data$Yield/28*1000000

#renaming new N file that includes DIN data to match below
N_Data<-NOx_NH4_Data
names(NOx_NH4_Data)
N_Data$DIN_FNConc_uM<-N_Data$DINFNConc/14*1000
N_Data$DIN_Conc_uM<-N_Data$DINConc/14*1000
N_Data$DIN_FNFlux_103_Km_y<-N_Data$DINFNFlux/14*1000
N_Data$DIN_Flux_103_Km_y<-N_Data$DINFlux/14*1000
N_Data$DIN_FNYield_Km_y_km<-N_Data$DINFNYield/14*1000000
N_Data$DIN_Yield_Km_y_km<-N_Data$DINYield/14*1000000



