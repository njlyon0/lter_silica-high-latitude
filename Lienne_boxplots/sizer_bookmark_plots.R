## ------------------------------------------------------- ##
# Bookmark plots for high latitude SiZer outputs
## ------------------------------------------------------- ##
# Written by: Lienne Sethna

# PURPOSE:
## Create "bookmark" visualizations for sizer slopes over time for high-latitude plots

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

#set working directory
getwd()
setwd("C:/Users/lsethna_smm/Documents/GitHub/lter_silica-high-latitude/Lienne_boxplots/bookmark plot stat outs v2")
#clear environment
rm(list=ls())
#load packages
librarian::shelf(readxl,tidyverse,janitor)

## ----------------------------------------- ##
# Load and prep data ----
## ----------------------------------------- ##

#load data
listcsv <- dir(pattern = "*.csv",recursive = F)
#view files
listcsv
#find and read site data
df_outs <- list()

for (i in 1:length(listcsv)) {
  sizer_out = read.csv(listcsv[i])
  
  # Make a data object with only the columns that we'll want
  sizer_out <- sizer_out %>%
    # Arrange by LTER and site
    dplyr::arrange(LTER, Stream_Name) %>%
    # Pare down to only needed columns
    dplyr::select(sizer_groups, LTER, Stream_Name, chemical:section_duration, 
                  F_statistic:line_fit, slope_estimate:slope_std_error,
                  dplyr::starts_with("dir_")) %>%
    # Drop non-unique rows
    dplyr::distinct()
  
  df_outs[[i]]=sizer_out
}

all_sizer_outs <- list_rbind(df_outs)
glimpse(all_sizer_outs)

length(unique(all_sizer_outs$stream))
unique(all_sizer_outs$chemical)
#create nice stream names to match summary boxplots
all_sizer_outs <- all_sizer_outs %>%
  #change to lower case
  mutate(plot_site_name = tolower(Stream_Name)) %>%
  #only keep text before "stream" or "river"
  mutate(plot_site_name = gsub("(stream|river|creek).*","",plot_site_name)) %>%
  #change to sentence case
  mutate(plot_site_name = str_to_title(plot_site_name))

# Filter the simplified data object to only significant rivers with a good fit
sig_only <- core_df %>%
  # Keep only significant slopes
  dplyr::filter(significance %in% c("sig", "marg")) %>%
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Drop non-unique rows
  dplyr::distinct()

## ----------------------------------------- ##
# Plotting Prep ----
## ----------------------------------------- ##

# Pick a missing and non significant color
na_col <- "#e5e5e5"
nonsig_col <- "#6c757d"

# Define color palettes
p_palt <- c("NA" = na_col, "sig" = "#132a13",  "marg" = "#006400",  "NS" = nonsig_col)
r2_palt <- c("NA" = na_col,  "bad" = "#b5e48c",  "fine" = "#76c893", 
             "good" = "#1a759f",  "great" = "#184e77")
dir_p_palt <- c("NA" = na_col, "NS" = nonsig_col,
                "pos-sig" = "#ff5400", "pos-marg" = "#ff9e00", 
                "neg-sig" = "#03045e", "neg-marg" = "#00b4d8")
dir_fit_palt <- c("NA" = na_col, "NS" = nonsig_col,
                  "pos-bad" = "#ffe863", "pos-fine" = "#ffe150", 
                  "pos-good" = "#facb2e", "pos-great" = "#f5bd1f",
                  "neg-bad" = "#e4afff", "neg-fine" = "#c86bfa", 
                  "neg-good" = "#722e9a", "neg-great" = "#47297b")

#organize stream names by LTER
stream_order <- all_sizer_outs %>% select(LTER,plot_site_name) %>% distinct() %>% arrange(LTER, plot_site_name)
all_sizer_outs <- all_sizer_outs %>% mutate(plot_site_name = factor(plot_site_name,levels=stream_order$plot_site_name))
glimpse(all_sizer_outs)

# Count numbers of streams at each LTER
core_hlines <- all_sizer_outs %>%
  dplyr::select(LTER, plot_site_name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))
glimpse(core_hlines)

#how many years in each site
core_years <- all_sizer_outs %>% group_by(LTER,plot_site_name,chemical) %>%
  summarize(year_ct=dplyr::n()) %>% filter(year_ct <= 5)

## ----------------------------------------- ##
# 'Bookmark Graphs' - Full Data ----
## ----------------------------------------- ##
unique(all_sizer_outs$chemical)
# Make a graph showing the slope direction and significance for all streams
all_sizer_outs %>% filter(chemical=="DSi-Q") %>%
  full_join(stream_order) %>%
ggplot(aes(x = Year, y = plot_site_name, color = dir_sig)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  geom_hline(yintercept = 78.5-c(2,24,30,40,51,59)) +
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(1995,2025),breaks=seq(1995,2025,5))+
  ggtitle("Significant changes in [DSi]-Q slope")+
  # Customize theme / formatting elements
  #facet_grid(~chemical)+
  theme_classic() +
  theme(legend.title=element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_blank(),
        strip.text=element_blank(),
        strip.background=element_blank(),
        panel.spacing=unit(0.5,"cm"))

## ----------------------------------------- ##
# 'Bookmark Graphs' - Sig. Only ----
## ----------------------------------------- ##

# Make a graph showing the slope direction and significance for all streams
sig_only %>% arrange(LTER,plot_site_name) %>% 
ggplot(aes(x = Year, y = plot_site_name, color = dir_sig)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  #geom_hline(yintercept = (sig_hlines$stream_cumulative + 0.5)) +
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.spacing=unit(0.5,"cm"),
        axis.text=element_text(size=8))

        