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
na_col <- "#e5e5e5"
nonsig_col <- "#6c757d"

# Define color palettes
## P value palette
p_palt <- c("NA" = na_col, "sig" = "#132a13",  "marg" = "#006400",  "NS" = nonsig_col)
## R2 palette
r2_palt <- c("NA" = na_col,  "bad" = "#b5e48c",  "fine" = "#76c893", 
             "good" = "#1a759f",  "great" = "#184e77")
## Direction + P value palette
dir_p_palt <- c("NA" = na_col, "NS" = nonsig_col,
                "pos-sig" = "#ff5400", "pos-marg" = "#ff9e00", 
                "neg-sig" = "#03045e", "neg-marg" = "#00b4d8")
## Direction + R2 palette
dir_fit_palt <- c("NA" = na_col, "NS" = nonsig_col,
                  "pos-bad" = "#ffe863", "pos-fine" = "#ffe150", 
                  "pos-good" = "#facb2e", "pos-great" = "#f5bd1f",
                  "neg-bad" = "#e4afff", "neg-fine" = "#c86bfa", 
                  "neg-good" = "#722e9a", "neg-great" = "#47297b")

## ----------------------------------------- ##
# `ggplot2` Theme
## ----------------------------------------- ##

# ggplot2 theme tweaks
theme_high_lat <- theme_bw() +
  theme(legend.title = element_blank())



# End ----
