# dplyr::filter(p_value < 0.05) %>%
#   # And only R squareds that are 'pretty good'
#   dplyr::filter(r_squared >= 0.30) %>%
#   # Arrange by LTER and site
#   arrange(LTER, site)


## ----------------------------------------- ##
# Exploratory Plotting ----
## ----------------------------------------- ##

# Tweak the combination object in preparation for an exploratory plot
combo_v4 <- combo_v3 %>%
  # Abbreviate LTER name if needed
  dplyr::mutate(LTER_abbrev = ifelse(nchar(LTER) > 4,
                                     yes = stringr::str_sub(string = LTER, start = 1, end = 4),
                                     no = LTER), .after = LTER) %>%
  # Make a combination LTER + site information column
  dplyr::mutate(LTER_site = paste0(LTER_abbrev, "_", site), .before = LTER)

# Check structure
sort(unique(combo_v4$LTER_abbrev))
dplyr::glimpse(combo_v4)

# Break off the first bit of the response variable (i.e., drop units)
## Ugly code but it works!
(response_simp <- tidyr::separate_wider_delim(data = data.frame(response_var = response_var), 
                                              cols = response_var, delim = "_", 
                                              too_few = "align_start",
                                              names = c("want", paste0(rep("junk", times = 10), 
                                                                       1:10))) %>%
    dplyr::pull(want))

# Make the exploratory graph
ggplot(combo_v4, aes(x = estimate, y = LTER_site, fill = duration)) +
  geom_col() +
  geom_errorbar(aes(xmax = estimate + std_error, xmin = estimate - std_error), 
                width = 0.2, linewidth = 0.75, color = "gray66") +
  labs(title = paste("Significant Changes in", element, response_simp),
       x = "Estimate", y = "LTER Abbreviation") +
  theme_bw()

# Export this graph
ggsave(filename = file.path(export_folder, 
                            paste0("_SEASONAL_sig-sizer-barplot_", Sys.Date(), ".png")),
       width = 6, height = 8, units = "in")


# End ----
