# ---------------------------------------------------------------------------- #
#### 1. Functions needed in the process                                     ####
# ---------------------------------------------------------------------------- #

## Function to process tidy results and add a model identifier
process_results <- function(tidy_results, model_name) {
  tidy_results %>%
    filter(!grepl("^factor\\(", term)) %>%  # Exclude factor variables
    mutate(
      lower = estimate - 2.576 * std.error,  # 99% confidence interval
      upper = estimate + 2.576 * std.error,
      Model = model_name  # Add model identifier
    )
}

# ---------------------------------------------------------------------------- #
#### 3. Plots of Coefficients                                               ####
# ---------------------------------------------------------------------------- #

# Load NUTS1-3 for both filtering options and Schmitt, then plot together in ONE plot

for (file.name in c("FADN")) {
  
  # CROP MAP FILTER
  nuts1_30_cf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS1_crop_map_filter_10_30dt.RDS"))
  nuts1_30_cf_tidy <- process_results(nuts1_30_cf, "NUTS1")
  nuts1_30_cf_tidy$SpecKey <- "NUTS1_30"
  nuts1_30_cf_tidy$Filter <- "Crop filter"
  
  nuts2_30_cf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS2_crop_map_filter_10_30dt.RDS"))
  nuts2_30_cf_tidy <- process_results(nuts2_30_cf, "NUTS2")
  nuts2_30_cf_tidy$SpecKey <- "NUTS2_30"
  nuts2_30_cf_tidy$Filter <- "Crop filter"
  
  nuts3_30_cf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS3_crop_map_filter_10_30dt.RDS"))
  nuts3_30_cf_tidy <- process_results(nuts3_30_cf, "NUTS3")
  nuts3_30_cf_tidy$SpecKey <- "NUTS3_30"
  nuts3_30_cf_tidy$Filter <- "Crop filter"
  
  # NO FILTER
  nuts1_30_nf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS1_no_filter_10_30dt.RDS"))
  nuts1_30_nf_tidy <- process_results(nuts1_30_nf, "NUTS1")
  nuts1_30_nf_tidy$SpecKey <- "NUTS1_30"
  nuts1_30_nf_tidy$Filter <- "No filter"
  
  nuts2_30_nf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS2_no_filter_10_30dt.RDS"))
  nuts2_30_nf_tidy <- process_results(nuts2_30_nf, "NUTS2")
  nuts2_30_nf_tidy$SpecKey <- "NUTS2_30"
  nuts2_30_nf_tidy$Filter <- "No filter"
  
  nuts3_30_nf <- readRDS(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/", 
                                file.name, "_winter.wheat_NUTS3_no_filter_10_30dt.RDS"))
  nuts3_30_nf_tidy <- process_results(nuts3_30_nf, "NUTS3")
  nuts3_30_nf_tidy$SpecKey <- "NUTS3_30"
  nuts3_30_nf_tidy$Filter <- "No filter"
  
  # Schmitt reference (only once)
  schmitt_results <- readRDS("02_Data/07_Schmitt_et_al_Estimates/FADN_winter.wheat_Schmitt.RDS")
  schmitt_tidy <- process_results(schmitt_results, "Municipality (Schmitt et al.)")
  schmitt_tidy$SpecKey <- "SCHMITT"
  schmitt_tidy$Filter <- "Reference"
  
  ## COMBINE ALL DATASETS
  comp_all <- dplyr::bind_rows(nuts1_30_cf_tidy, nuts2_30_cf_tidy, nuts3_30_cf_tidy,
                               nuts1_30_nf_tidy, nuts2_30_nf_tidy, nuts3_30_nf_tidy,
                               schmitt_tidy)
  
  ## PREPARE DATA FOR PLOTTING
  comp_all$lower <- comp_all$estimate - 1.96 * comp_all$std.error
  comp_all$upper <- comp_all$estimate + 1.96 * comp_all$std.error
  
  # Create combined SpecFilter for legend with proper ordering
  comp_all$SpecFilter <- paste0(comp_all$SpecKey, "_", comp_all$Filter)
  
  spec_labels <- c(
    "NUTS1_30_No filter" = "NUTS-1 • No filter",
    "NUTS1_30_Crop filter" = "NUTS-1 • Crop map filter",
    "NUTS2_30_No filter" = "NUTS-2 • No filter",
    "NUTS2_30_Crop filter" = "NUTS-2 • Crop map filter",
    "NUTS3_30_No filter" = "probabilistic NUTS-3 • No filter",
    "NUTS3_30_Crop filter" = "probabilistic NUTS-3 • Crop map filter",
    "SCHMITT_Reference" = "Municipality (Schmitt et al.)"
  )
  
  # Colors for NUTS levels (same color for both filters of same NUTS level)
  spec_colors <- c(
    "NUTS1_30_No filter" = "#F28C45",
    "NUTS1_30_Crop filter" = "#f4a261",
    "NUTS2_30_No filter" = "#7570b3",
    "NUTS2_30_Crop filter" = "#b2abd2",
    "NUTS3_30_No filter" = "#1b9e77",
    "NUTS3_30_Crop filter" = "#52c4a0",
    "SCHMITT_Reference" = "#e7298a"
  )
  
  # Shapes: open for No filter, filled for Crop filter
  spec_shapes <- c(
    "NUTS1_30_No filter" = 0,    # open square
    "NUTS1_30_Crop filter" = 15,  # filled square
    "NUTS2_30_No filter" = 1,    # open circle
    "NUTS2_30_Crop filter" = 16,  # filled circle
    "NUTS3_30_No filter" = 2,    # open triangle
    "NUTS3_30_Crop filter" = 17,  # filled triangle
    "SCHMITT_Reference" = 18     # filled diamond
  )
  
  ## CUSTOM LABELS FOR EFFECTS
  custom_names <- c(
    "Black_Frost"         = "Black\nFrost",
    "Heat"                = "Heat",
    "Spring_Drought"      = "Spring\nDrought",
    "Summer_Drought"      = "Summer\nDrought",
    "Spring_Waterlogging" = "Spring\nWaterlogging",
    "Summer_Waterlogging" = "Summer\nWaterlogging"
  )
  comp_all$term <- dplyr::recode(comp_all$term, !!!custom_names)
  
  comp_all$term <- factor(comp_all$term,
                          levels = rev(c("Black\nFrost", "Heat", "Spring\nDrought",
                                         "Summer\nDrought", "Spring\nWaterlogging",
                                         "Summer\nWaterlogging")))
  
  # Order SpecFilter for legend (No filter first, then Crop filter for each NUTS level)
  comp_all$SpecFilter <- factor(comp_all$SpecFilter, 
                                levels = rev(c("NUTS1_30_No filter", "NUTS1_30_Crop filter",
                                               "NUTS2_30_No filter", "NUTS2_30_Crop filter",
                                               "NUTS3_30_No filter", "NUTS3_30_Crop filter",
                                               "SCHMITT_Reference")))
  
  x_pad   <- 0.002
  x_lower <- min(comp_all$lower, na.rm = TRUE) - x_pad
  x_upper <- max(comp_all$upper, na.rm = TRUE) + x_pad
  x_lower <- -0.11  # Set the minimum x-axis value
  x_upper <- 0.03  # Set the maximum x-axis value
  
  ## CREATE FOREST PLOT WITH BOTH FILTERS
  x_breaks <- seq(-0.100, 0.025, by = 0.025)
  
  all_specs_plot <- ggplot(comp_all,
                           aes(x = estimate, y = term, color = SpecFilter, shape = SpecFilter)) +
    geom_hline(yintercept = seq(1.5, 5.5, by = 1), color = "grey90", linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   position = position_dodge(width = 0.8), height = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = scales::label_number(accuracy = 0.001),
      minor_breaks = NULL
    ) +
    scale_color_manual(
      values = spec_colors, 
      labels = spec_labels, 
      name = "Spatial resolution"
    ) +
    scale_shape_manual(
      values = spec_shapes, 
      labels = spec_labels, 
      name = "Spatial resolution"
    ) +
    coord_cartesian(xlim = c(-0.105, 0.027)) +
    labs(x = "", y = "", title = NULL) +
    guides(
      color = guide_legend(ncol = 4, reverse = TRUE),
      shape = guide_legend(ncol = 4, reverse = TRUE)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid       = element_blank(),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
      legend.text      = element_text(size = 15),
      legend.title     = element_text(size = 15, hjust = 0),
      legend.position  = "bottom",
      legend.margin    = margin(t = 10, unit = "pt"),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.key.size  = unit(0.9, "lines"),
      axis.text        = element_text(size = 16),
    )
  
  all_specs_plot
  
  ggsave(paste0("03_Output/01_Extreme_Events/robustness_re_estimation/",
                file.name, "_all_NUTS_both_filters_seperate_thresholds.png"),
         all_specs_plot, dpi = set.dpi, width = 16, height = 10)
}
