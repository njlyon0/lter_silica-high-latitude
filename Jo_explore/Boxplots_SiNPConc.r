##creating boxplots for high lat paper to show data
##using annual or monthly WRTDS model output 

#load packages
librarian::shelf(tidyverse, supportR)

#read in data
#Data<-readr::read_csv('Full_Results_WRTDS_monthly.csv')
Data<-readr::read_csv('Full_Results_WRTDS_annual.csv')

#new WRTDS output has different names - fix to match script
names(Data)[names(Data) == "Stream_Name"] <- "stream"

#creating new file with only sites we need
PolarSites <- Data %>%
  # Keep only cryosphere LTERs
  dplyr::filter(LTER %in% c("MCM", "GRO", "NIVA", "Krycklan",
                            "Finnish Environmental Institute", "Canada", "Swedish Goverment")) %>%
  # But drop problem sites that are otherwise retained
  dplyr::filter(!stream %in% c("Site 69038", "Kymijoki Ahvenkoski 001", "Kymijoki Kokonkoski 014",
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
unique(PolarSites$stream)

####=================================================
#this new column allows one to have boxplots sorted by lter, then stream
PolarSites$LTERStream<-paste0(stringr::str_sub(PolarSites$LTER,1,3),"_",PolarSites$stream)
PolarSites$LTERStreamshort<-paste0(stringr::str_sub(PolarSites$LTER,1,3),"_",
                                   paste0(stringr::str_sub(PolarSites$stream,1,20)))


#Subset by chemical
Si_Polar<-subset(PolarSites, PolarSites$chemical=="DSi")
N_Polar<-subset(PolarSites, PolarSites$chemical=="DIN")
P_Polar<-subset(PolarSites, PolarSites$chemical=="P")
Si_Polar$Qnorm<-Si_Polar$Discharge_cms/Si_Polar$drainSqKm

names(Si_Polar)

##=====================================
##=====================================

#boxplots Si conc across each stream
#updated 5.14.24

#Si
# Count numbers of streams at each LTER
core_vlines <- Si_Polar %>%
  dplyr::select(LTER, LTERStreamshort) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(Si_Polar, aes(x=LTERStreamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DSi Conc (uM) each stream") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Stream name") +
  ylab("DSi concentration (uM)") +
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5)) 

ggsave(paste0("Boxplots Modelled DSi Conc all rivers",Sys.Date(),".png"), width=10, height=5)



#DIN
core_vlines <- N_Polar %>%
  dplyr::select(LTER, LTERStreamshort) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(N_Polar, aes(x=LTERStreamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIN Conc (uM) each stream") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Stream name") +
  ylab("DIN concentration (uM)") +
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5))

ggsave(paste0("Boxplots Modelled DIN Conc all rivers",Sys.Date(),".png"), width=10, height=5)


#P
core_vlines <- P_Polar %>%
  dplyr::select(LTER, LTERStreamshort) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

ggplot(P_Polar, aes(x=LTERStreamshort, y=Conc_uM, fill=LTER))+
  geom_boxplot(outlier.shape=21, outlier.size=2)+ggtitle("Annual Modelled DIP Conc (uM) each stream") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("Stream name") +
  ylab("DIP concentration (uM)") +
  geom_vline(xintercept = (core_vlines$stream_cumulative + 0.5))

ggsave(paste0("Boxplots Modelled DIP Conc all rivers",Sys.Date(),".png"), width=10, height=5)







