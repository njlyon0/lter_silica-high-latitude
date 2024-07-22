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




# End ----
