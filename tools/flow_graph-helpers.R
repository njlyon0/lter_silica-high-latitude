## ------------------------------------------------------- ##
                        # Graph Helpers
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

## ----------------------------------------- ##
# Script Explanation ----
## ----------------------------------------- ##

# Purpose:
## Contains objects that are useful for streamlining data viz

# Rationale for treatment as a separate script:
## Data visualization happens in many separate scripts (exploratory and publication-quality)
## We want some innovations (e.g., `ggplot2` custom themes, etc.), to be available *to all graphing scripts in the project)
## And to be update-able in a single centralized location

## ----------------------------------------- ##
# Factor Levels ----
## ----------------------------------------- ##

# Slope direction & signficance levels
dir_sig_levels <- c("pos-sig", "pos-marg", "neg-marg", "neg-sig", "NA", "NS")

# Slope direction & strength of fit (R^2) levels
dir_fit_levels <- c("pos-great", "pos-good", "pos-fine", "pos-bad",
                    "neg-bad", "neg-fine", "neg-good", "neg-great", 
                    "NA", "NS")

## ----------------------------------------- ##
# Colors ----
## ----------------------------------------- ##

# Define the missing and non significant colors
na_col <- "#fff"
nonsig_col <- "#6c757d"

# Define color palettes
## P value palette
p_palt <- c("sig" = "#132a13",  "marg" = "#006400",  "NS" = nonsig_col, "NA" = na_col)
## R2 palette
r2_palt <- c("great" = "#184e77", "good" = "#1a759f", "fine" = "#76c893", 
             "bad" = "#b5e48c", "NA" = na_col)
## Slope direction palette
dir_palt <- c("pos" = "#ff5400", "neg" = "#03045e", "NS" = nonsig_col, "NA" = na_col)
## Direction + P value palette
dir_p_palt <- c("pos-sig" = "#ff5400", "pos-marg" = "#ff9e00", 
                "neg-sig" = "#03045e", "neg-marg" = "#00b4d8",
                "NS" = nonsig_col, "NA" = na_col)
## Direction + R2 palette
dir_fit_palt <- c("pos-great" = "#f5bd1f", "pos-good" = "#facb2e", 
                  "pos-fine" = "#ffe150", "pos-bad" = "#ffe863", 
                  "neg-great" = "#47297b", "neg-good" = "#722e9a", 
                  "neg-fine" = "#c86bfa", "neg-bad" = "#e4afff",
                  "NS" = nonsig_col, "NA" = na_col)
## LTER palette
lter_palt <- c("Canada" = "#390099", 
               "Finnish Environmental Institute" = "#374c80", "Finland" = "#374c80",
               "GRO" = "#7a5195", "Krycklan" = "#bc5090", "MCM" = "#ef5675",
               "NIVA" = "#ff764a", "Norway" = "#ff764a", 
               "Swedish Goverment" = "#ffa600", "Sweden" = "#ffa600")

## LTER shapes
lter_shps <- c("Canada" = 21, 
               "Finnish Environmental Institute" = 22, "Finland" = 22,
               "GRO" = 23, "Krycklan" = 24, "MCM" = 21,
               "NIVA" = 25, "Norway" = 25, 
               "Swedish Goverment" = 22, "Sweden" = 22)

## ----------------------------------------- ##
# `ggplot2` Theme
## ----------------------------------------- ##

# ggplot2 theme tweaks
theme_high_lat <- theme_bw() +
  theme(legend.title = element_blank())

# End ----
