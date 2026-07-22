
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

## READ + PREP DATA
read_nuts3 <- function(file.name, filtering, yield_bound, key) {
  x <- readRDS(paste0("03_Output/02_Estimation/objects/",
                      file.name, "_winter.wheat_NUTS3_", filtering, "_", yield_bound, ".RDS"))
  x %>%
    dplyr::filter(!grepl("^factor\\(", term)) %>%
    dplyr::mutate(
      SpecKey = key,
      lower   = estimate - 2.576 * std.error,   # 99% CI
      upper   = estimate + 2.576 * std.error
    )
}

read_schmitt <- function() {
  x <- readRDS("02_Data/07_Schmitt_et_al_Estimates/FADN_winter.wheat_Schmitt.RDS")
  x %>%
    dplyr::filter(!grepl("^factor\\(", term)) %>%
    dplyr::mutate(
      SpecKey = "SCHMITT",
      lower   = estimate - 2.576 * std.error,
      upper   = estimate + 2.576 * std.error
    )
}

norm_term <- function(x) {
  x %>%
    tolower() %>%
    # convert factor(YEAR) terms to NA so they drop out
    {ifelse(str_detect(., "^factor\\("), NA_character_, .)} %>%
    str_replace_all(" ", "_")
}

## Function to compute adjustments
compute_adjustments <- function(results_tbl, schmitt_df, level_label = "NUTS1") {
  ## keep only the extremes you care about and align names
  model_coefs <- results_tbl %>%
    mutate(term_clean = norm_term(term)) %>%
    filter(!is.na(term_clean)) %>%
    # keep just the extremes that exist in Schmitt
    semi_join(schmitt_df %>% select(Extreme), by = c("term_clean" = "Extreme")) %>%
    transmute(Extreme = term_clean, Coef_model = round(estimate,4))
  
  ## join and compute factors
  out <- schmitt_df %>%
    left_join(model_coefs, by = "Extreme") %>%
    mutate(
      Adjust = abs(round(Coef_model,4)) / abs(round(Coef,4)),
      Ha_losses_adj = Ha_losses * Adjust,
      Total_losses_adj = Total_losses * Adjust,
      Level = level_label
    ) %>%
    select(Level, Extreme, Coef_model, Coef_schmitt = Coef, Adjust,
           Ha_losses_schmitt = Ha_losses, Ha_losses_adj,
           Total_losses_schmitt = Total_losses, Total_losses_adj)
  
  out
}

## one plotting function
plot_term <- function(df_term) {
  term_name <- unique(df_term$term)
  
  ggplot(df_term, aes(x = Model, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
    geom_point(size = 2.5) +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    labs(
      title = paste0(term_name),
      x = "Threshold",
      y = "Estimate (99% CI)"
    ) +
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(size = 16))
}

# ---------------------------------------------------------------------------- #
#### 2. Descriptives of Extremes                                            ####
# ---------------------------------------------------------------------------- #

for(filtering in c("crop_map_filter", "no_filter")){
    
  extremes_NUTS1 <- read.csv(paste0("03_Output/01_Extreme_Events/NUTS1_winter.wheat_", filtering, ".csv"))
  colnames(extremes_NUTS1)[1:2] <- c("YEAR", "NUTS")
  extremes_NUTS2 <- read.csv(paste0("03_Output/01_Extreme_Events/NUTS2_winter.wheat_", filtering, ".csv"))
  colnames(extremes_NUTS2)[1:2] <- c("YEAR", "NUTS")
  extremes_NUTS3 <- read.csv(paste0("03_Output/01_Extreme_Events/NUTS3_winter.wheat_", filtering, ".csv"))
  colnames(extremes_NUTS3)[1:2] <- c("YEAR", "NUTS")
  
  extremes_NUTS1$resolution <- "NUTS1"
  extremes_NUTS2$resolution <- "NUTS2"
  extremes_NUTS3$resolution <- "NUTS3 (probabilistic)"
  
  extremes_all <- bind_rows(extremes_NUTS1, extremes_NUTS2, extremes_NUTS3)
  
  long_data <- extremes_all %>%
    pivot_longer(
      # Pivot everything except YEAR, NUTS_ID, and resolution
      cols = -c(YEAR, NUTS, resolution), 
      names_to = "Events", 
      values_to = "Values"
    ) %>%
    # Filter out any events you don’t want to plot
    filter(Events != "Spring_Chill")  
  
  long_data <- long_data %>%
    mutate(Events = gsub("_", "\n", Events))
  
  ## Convert 'resolution_label' into a factor so boxplots align in 
  ## the correct order. We want the levels: NUTS1, NUTS2, NUTS3, and "".
  long_data <- long_data %>%
    mutate(
      resolution = factor(
        resolution,
        levels = c("NUTS1", "NUTS2", "NUTS3 (probabilistic)")
      )
    )
  
  ## Build the ggplot using resolution on the x-axis
  extreme_event_count <- ggplot(
    long_data,
    aes(x = resolution, y = Values, fill = resolution)
  ) +
    geom_boxplot(
      outlier.size = 0.5,
      outlier.shape = 16,
      width = 0.7
    ) +
    facet_grid(Events ~ YEAR, scales = "free_y", switch = "y") +
    scale_fill_manual(
      values = c("NUTS1" = "#d95f02",
                 "NUTS2" = "#7570b3",
                 "NUTS3 (probabilistic)" = "#1b9e77"),
      name = "Spatial resolution"
    ) +
    labs(
      x = NULL,
      y = "Degree days or days",
      fill = "Spatial resolution"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      panel.spacing.y = unit(1, "lines"),
      axis.text.x = element_blank(),
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5),
      legend.margin = margin(t = 10, unit = "pt"),
      
      ## NEW: put facet labels left + make text horizontal
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, hjust = 1)   # angle=0 = horizontal
    )

    ggsave(paste0("03_Output/02_Estimation/graphs/extreme_event_count_", filtering, ".png"),
           extreme_event_count, 
           dpi = set.dpi, width = 14, height = 13)
}

# ---------------------------------------------------------------------------- #
#### 3. Plots of Coefficients                                               ####
# ---------------------------------------------------------------------------- #

for(filtering in c("crop_map_filter", "no_filter")){
  for (yield_bound in c("30dt", "1dt")) {

  file.names <- c("FADN")
  
    for (file.name in file.names) {
      NUTS1.results <- readRDS(paste0("03_Output/02_Estimation/objects/", file.name, "_winter.wheat_NUTS1_", filtering, "_", yield_bound,".RDS"))
      NUTS2.results <- readRDS(paste0("03_Output/02_Estimation/objects/", file.name, "_winter.wheat_NUTS2_", filtering, "_", yield_bound,".RDS"))
      NUTS3.results <- readRDS(paste0("03_Output/02_Estimation/objects/", file.name, "_winter.wheat_NUTS3_", filtering, "_", yield_bound,".RDS"))
      Schmitt.results <- readRDS("02_Data/07_Schmitt_et_al_Estimates/FADN_winter.wheat_Schmitt.RDS")
      
      ## Process and combine all models
      tidy_NUTS1 <- process_results(NUTS1.results, "NUTS1")
      tidy_NUTS2 <- process_results(NUTS2.results, "NUTS2")
      tidy_NUTS3 <- process_results(NUTS3.results, "NUTS3 (probabilistic)")
      tidy_Schmitt <- process_results(Schmitt.results, "Municipality (Schmitt et al.)")
      
      ## Combine datasets
      all_results <- bind_rows(tidy_NUTS1, tidy_NUTS2, tidy_NUTS3, tidy_Schmitt)
      
      ## Custom names for display
      custom_names <- c(
        "Black_Frost" = "Black\nFrost",
        "Heat" = "Heat",
        "Spring_Drought" = "Spring\nDrought",
        "Summer_Drought" = "Summer\nDrought",
        "Spring_Waterlogging" = "Spring\nWaterlogging",
        "Summer_Waterlogging" = "Summer\nWaterlogging"
      )
      
      ## Replace term names
      all_results$term <- recode(all_results$term, !!!custom_names)
      
      ## Ensure NUTS levels are ordered correctly (NUTS1 first, NUTS3 last)
      all_results$Model <- factor(all_results$Model, levels = c("Municipality (Schmitt et al.)", "NUTS3 (probabilistic)", "NUTS2", "NUTS1"))
      
      ## Generate and save table
      source("01_Code/01_Functions/Regression_Table_generator.R")
      
      ## save to file
      cat(latex_code, file = paste0("03_Output/02_Estimation/tables/regression_results_", filtering, ".tex"))
      
      ## Define reasonable x-axis limits
      x_lower <- -0.11  # Set the minimum x-axis value
      x_upper <- 0.02  # Set the maximum x-axis value
      
      ## Generate ggplot with models side-by-side and x-axis cut-off
      coef_plot <- ggplot(all_results, aes(x = estimate, y = term, color = Model, shape = Model)) +
        geom_point(position = position_dodge(width = 0.7), size = 3) +
        geom_errorbarh(aes(xmin = lower, xmax = upper),
                       position = position_dodge(width = 0.7), height = 0.3) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
        scale_y_discrete(limits = rev(unique(all_results$term))) +
        scale_color_manual(
          values = c("NUTS1" = "#d95f02",
                     "NUTS2" = "#7570b3",
                     "NUTS3 (probabilistic)" = "#1b9e77",
                     "Municipality (Schmitt et al.)" = "#d95f70"),
          name = "Spatial resolution"
        ) +
        scale_shape_manual(
          values = c("NUTS1" = 15,   # square
                     "NUTS2" = 16,   # circle
                     "NUTS3 (probabilistic)" = 17, # triangle
                     "Municipality (Schmitt et al.)" = 18), # diamond
          name = "Spatial resolution"
        ) +
        coord_cartesian(xlim = c(x_lower, x_upper)) +
        labs(x = "", y = "", title = NULL, color = "Spatial resolution", shape = "Spatial resolution") +
        guides(
          color = guide_legend(reverse = TRUE, nrow = 1),
          shape = guide_legend(reverse = TRUE, nrow = 1)
        ) +
        theme_minimal(base_size = 20) +
        theme(
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.title = element_text(size = 16, face = "bold", hjust = 0),
          legend.margin = margin(t = 0, unit = "pt"),
          # legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
          legend.key = element_rect(fill = "white", color = NA),
          legend.key.size = unit(1, 'lines')
        )
      
      ## Save the plot
      ggsave(paste0("03_Output/02_Estimation/graphs/", file.name, "_coef_plot_multi_", filtering, "_", yield_bound, ".png"),
             coef_plot, dpi = set.dpi, width = 14, height = 10)
      
      
      if (yield_bound == "30dt" && filtering == "crop_map_filter") {
        
        ## SETTINGS
        ## 1) Legend/display names
        spec_labels <- c(
          CF30    = "Crop filter • 30 dt/ha",
          CF1     = "Crop filter • 1 dt/ha",
          NF30    = "No filter • 30 dt/ha",
          NF1     = "No filter • 1 dt/ha",
          SCHMITT = "Municipality (Schmitt et al.)"
        )
        
        ## 2) Optional manual x-limits
        x_limits_manual <- NULL   # e.g., c(-0.06, 0.01)
        
        ## 3) Padding (absolute) to add around auto range
        x_pad <- 0.002
        
        nuts3_cf_30 <- read_nuts3(file.name, "crop_map_filter", "30dt", "CF30")
        nuts3_cf_1  <- read_nuts3(file.name, "crop_map_filter", "1dt",  "CF1")
        nuts3_nf_30 <- read_nuts3(file.name, "no_filter",      "30dt", "NF30")
        nuts3_nf_1  <- read_nuts3(file.name, "no_filter",      "1dt",  "NF1")
        schmitt_all <- read_schmitt()
        
        comp_all <- dplyr::bind_rows(nuts3_cf_30, nuts3_cf_1, nuts3_nf_30, nuts3_nf_1, schmitt_all)
        
        ## labels for effects
        custom_names <- c(
          "Black_Frost"         = "Black\nFrost",
          "Heat"                = "Heat",
          "Spring_Drought"      = "Spring\nDrought",
          "Summer_Drought"      = "Summer\nDrought",
          "Spring_Waterlogging" = "Spring\nWaterlogging",
          "Summer_Waterlogging" = "Summer\nWaterlogging"
        )
        comp_all$term <- dplyr::recode(comp_all$term, !!!custom_names)
        
        comp_all$term <- factor(comp_all$term, levels = rev(unique(comp_all$term)))
        spec_order <- c("CF30","CF1","NF30","NF1","SCHMITT")
        comp_all$SpecKey <- factor(comp_all$SpecKey, levels = spec_order)
        comp_all$Spec    <- factor(spec_labels[as.character(comp_all$SpecKey)],
                                   levels = spec_labels[spec_order])
        
        ## AUTO-ZOOM
        if (is.null(x_limits_manual)) {
          x_lower <- min(comp_all$lower, na.rm = TRUE) - x_pad
          x_upper <- max(comp_all$upper, na.rm = TRUE) + x_pad
        } else {
          x_lower <- x_limits_manual[1]
          x_upper <- x_limits_manual[2]
        }
        
        ## FOREST PLOT
        nuts3_schmitt_comp <- ggplot(comp_all,
                                     aes(x = estimate, y = term, color = Spec, shape = Spec)) +
          geom_point(position = position_dodge(width = 0.7), size = 3) +
          geom_errorbarh(aes(xmin = lower, xmax = upper),
                         position = position_dodge(width = 0.7), height = 0.3) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
          coord_cartesian(xlim = c(x_lower, x_upper)) +
          labs(x = "", y = "", color = "Specification", shape = "Specification", title = NULL) +
          guides(color = guide_legend(reverse = TRUE, nrow = 2),
                 shape = guide_legend(reverse = TRUE, nrow = 2)) +
          theme_minimal(base_size = 20) +
          theme(
            legend.text   = element_text(size = 14),
            legend.title  = element_text(size = 16, face = "bold", hjust = 0),
            legend.position = "bottom",
            legend.margin   = margin(t = 0, unit = "pt"),
            legend.key      = element_rect(fill = "white", color = NA),
            legend.key.size = unit(1, "lines")
          )
        
        ggsave(paste0("03_Output/02_Estimation/graphs/",
                      file.name, "_NUTS3_vs_Schmitt_forest_allEffects.png"),
               nuts3_schmitt_comp, dpi = set.dpi, width = 14, height = 10)
        
        ## FACETED VARIANT
        nuts3_schmitt_facets <- ggplot(comp_all,
                                       aes(x = estimate, y = Spec, color = Spec, shape = Spec)) +
          geom_point(size = 2.6) +
          geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
          facet_wrap(~ term, ncol = 3, scales = "fixed") +
          coord_cartesian(xlim = c(x_lower, x_upper)) +
          labs(x = "", y = "", color = "Specification", shape = "Specification", title = NULL) +
          theme_minimal(base_size = 18) +
          theme(
            strip.text = element_text(size = 16, face = "bold"),
            legend.position = "bottom",
            legend.text  = element_text(size = 12),
            legend.title = element_text(size = 14, face = "bold", hjust = 0)
          )
        
        ggsave(paste0("03_Output/02_Estimation/graphs/",
                      file.name, "_NUTS3_vs_Schmitt_forest_facets.png"),
               nuts3_schmitt_facets, dpi = set.dpi, width = 14, height = 10)
      }
    }
  }
}

# ---------------------------------------------------------------------------- #
#### 4. Estimated losses                                                    ####
# ---------------------------------------------------------------------------- #

NUTS1.results <- readRDS("03_Output/02_Estimation/objects/FADN_winter.wheat_NUTS1_crop_map_filter_30dt.RDS")
NUTS2.results <- readRDS("03_Output/02_Estimation/objects/FADN_winter.wheat_NUTS2_crop_map_filter_30dt.RDS")
NUTS3.results <- readRDS("03_Output/02_Estimation/objects/FADN_winter.wheat_NUTS3_crop_map_filter_30dt.RDS")

## Input data
Schmitt_coeffs_and_losses <- data.frame(
  Extreme = c("summer_drought", "spring_waterlogging"),
  Coef = c(-0.0036, -0.0028),
  Ha_losses = c(7.5, 1.5),
  Total_losses = c(23.2, 4.6),
  stringsAsFactors = FALSE
)

## Run for NUTS1-3
nuts1_adjust <- compute_adjustments(NUTS1.results, Schmitt_coeffs_and_losses, "NUTS1")
nuts2_adjust <- compute_adjustments(NUTS2.results, Schmitt_coeffs_and_losses, "NUTS2")
nuts3_adjust <- compute_adjustments(NUTS3.results, Schmitt_coeffs_and_losses, "NUTS3")

## Combine into one table
all_adjust <- bind_rows(nuts1_adjust, nuts2_adjust, nuts3_adjust)

## Round for readability
all_adjust_round <- all_adjust %>%
  mutate(
    across(c(Coef_model, Coef_schmitt, Adjust, Ha_losses_adj, Total_losses_adj), 
           ~round(., 3))
  )

write.table(all_adjust_round,
            file = "03_Output/02_Estimation/tables/adjusted_losses_30dt.txt",
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

# ---------------------------------------------------------------------------- #
#### 5. Sensitivity check on crop map threshold                             ####
# ---------------------------------------------------------------------------- #

for(NUTS in c("NUTS1", "NUTS2", "NUTS3")){

  ## Threshold labels + files
  model_files <- tibble::tibble(
    Model = paste0(thresholds, " %"),
    file  = paste0("03_Output/02_Estimation/objects/FADN_winter.wheat_", NUTS,
                   "_crop_map_filter_", thresholds, "_30dt.RDS"))
  
  ## Load + process + combine
  all_results <- model_files %>%
    mutate(raw = map(file, readRDS),
           tidy = map2(raw, Model, process_results)) %>%
    select(tidy) %>%
    unnest(tidy) %>%
    mutate(Model = factor(Model, levels = paste0(thresholds, " %")))
  
  all_results$term[all_results$term == "Black_Frost"] <- "Black Frost"
  all_results$term[all_results$term == "Heat"] <- "Heat"
  all_results$term[all_results$term == "Spring_Drought"] <- "Spring Drought"
  all_results$term[all_results$term == "Summer_Drought"] <- "Summer Drought"
  all_results$term[all_results$term == "Spring_Waterlogging"] <- "Spring Waterlogging"
  all_results$term[all_results$term == "Summer_Waterlogging"] <- "Summer Waterlogging"
  
  ## build plots
  plots_by_term <- all_results %>%
    group_split(term) %>%
    setNames(map_chr(., ~ unique(.x$term))) %>%
    map(plot_term)
  
  ## view one:
  plots_by_term$`Summer Waterlogging`
  
  ## Save
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Black_Frost_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Black Frost`, width = 6, height = 4, dpi = 600)
  
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Heat_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Heat`, width = 6, height = 4, dpi = 600)
  
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Spring_Drought_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Spring Drought`, width = 6, height = 4, dpi = 600)
  
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Spring_Waterlogging_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Spring Waterlogging`, width = 6, height = 4, dpi = 600)
  
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Summer_Drought_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Summer Drought`, width = 6, height = 4, dpi = 600)
  
  ggsave(filename = paste0("03_Output/02_Estimation/graphs/Summer_Waterlogging_threshold_comparison_", NUTS, ".png"),
         plot = plots_by_term$`Summer Waterlogging`, width = 6, height = 4, dpi = 600)
}

# ---------------------------------------------------------------------------- #
#### 7. produce summary statistics for municipality extreme events          ####
# ---------------------------------------------------------------------------- #

municipality_extremes <- read.csv("03_Output/01_Extreme_Events/LAU_winter.wheat_no_filter_10.csv")

nrow(municipality_extremes)
summary(municipality_extremes)
sd(municipality_extremes$Black_Frost)
sd(municipality_extremes$Heat)
sd(municipality_extremes$Spring_Drought)
sd(municipality_extremes$Summer_Drought)
sd(municipality_extremes$Spring_Waterlogging)
sd(municipality_extremes$Summer_Waterlogging)

# ---------------------------------------------------------------------------- #
#### 8. More plotting                                                       ####
# ---------------------------------------------------------------------------- #
source("01_Code/01_Functions/plotting.R")
