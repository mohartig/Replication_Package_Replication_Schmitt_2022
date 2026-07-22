
#------------------------------------------------------------------------------
# Run loops
#------------------------------------------------------------------------------

for(filtering in c("crop_map_filter", "no_filter")){
    
  for(crop.filter.threshold in thresholds){
    
    if(filtering == "no_filter" & crop.filter.threshold > thresholds[1]){next}
    
    for (yield_bound in c("1dt", "30dt")) {
      
      for (NUTS in NUTS.OPTIONS) {
    
        for (crop in crops) {
        
        #------------------------------------------------------------------------------
        # Load and merge: FADN & BRK_JKI & EXTREMES
        #------------------------------------------------------------------------------
        
        ## LOAD JKI Bodenraumklima
        BRK <- readRDS("02_Data/04_BRK/area_matching.RDS")
        
        ## LOAD FADN
        FADN <- readRDS(paste0(FADN.path, "/02_Merged_by_Countries/FADN.rda"))
        FADN.NUTS.3 <- readRDS(paste0(FADN.path, "04_Others/FADN_PROB_NUTS3.Rds"))
        FADN <- FADN[FADN$COUNTRY == "DEU",]
        FADN$NUTS3 <- FADN.NUTS.3$NUTS3
        
        FADN <- FADN %>%
          left_join(BRK, by = "NUTS3")
        
        if(NUTS == "NUTS1"){FADN$NUTS1 <- substr(FADN$NUTS3, 1, nchar(FADN$NUTS3) - 2)}
    
        if(NUTS == "NUTS2"){FADN$NUTS2 <- substr(FADN$NUTS3, 1, nchar(FADN$NUTS3) - 1)}
        
        if(crop == "winter.wheat"){
          ## generate dt/ha
          FADN$CROP.DT.HA <- FADN$WHEAT.dt.N / FADN$WHEAT.ha.D
          
          ## delete NAs
          FADN <- FADN[!is.na(FADN$CROP.DT.HA), ]
          
          ## delete yield > 130dt/ha
          FADN <- FADN[FADN$CROP.DT.HA <= 130, ]
          
          ## apply yield filter
          if(yield_bound == "30dt"){FADN <- FADN[FADN$CROP.DT.HA > 30, ]}
          if(yield_bound == "1dt"){FADN <- FADN[FADN$CROP.DT.HA > 1, ]}
          
          FADN <- FADN[,c("ID", as.character(NUTS), "YEAR", "CROP.DT.HA", "REGION_JKI")]
        }
        
        ## LOAD EXTREMES
        extremes <- read.csv(paste0("03_Output/01_Extreme_Events/", NUTS, "_", crop, "_", filtering, "_", crop.filter.threshold, ".csv"))
        colnames(extremes)[1:2] <- c("YEAR", NUTS)
        
        ## Merge FADN & EXTREMES
        FADN <- merge(FADN, extremes, by = c("YEAR", as.character(NUTS)))
        
        FADN.NORTH <- FADN[FADN$REGION_JKI == "north",]
        FADN.SOUTH <- FADN[FADN$REGION_JKI == "south",]
        FADN.EAST <- FADN[FADN$REGION_JKI == "east",]
        FADN.WEST <- FADN[FADN$REGION_JKI == "west",]
        
        #------------------------------------------------------------------------------
        # Creation of new variable "region" for the regional sub-samples as Robustness Check
        #------------------------------------------------------------------------------
        
        FADN.NORTH <- FADN[FADN$REGION_JKI == "north",]
        FADN.SOUTH <- FADN[FADN$REGION_JKI == "south",]
        FADN.EAST <- FADN[FADN$REGION_JKI == "east",]
        FADN.WEST <- FADN[FADN$REGION_JKI == "west",]
        
        #------------------------------------------------------------------------------
        # Creation of temporal sub-samples as Robustness Check
        #------------------------------------------------------------------------------
        
        FADN.2004 <- FADN[which(FADN$YEAR < 2012),] 
        FADN.2011 <- FADN[which(FADN$YEAR > 2011),] 
        
        FADN.NORTH.2004 <- FADN.NORTH[which(FADN.NORTH$YEAR < 2012),]
        FADN.NORTH.2011 <- FADN.NORTH[which(FADN.NORTH$YEAR > 2011),]
    
        FADN.EAST.2004 <- FADN.EAST[which(FADN.EAST$YEAR < 2012),]
        FADN.EAST.2011 <- FADN.EAST[which(FADN.EAST$YEAR > 2011),]
    
        FADN.WEST.2004 <- FADN.WEST[which(FADN.WEST$YEAR < 2012),]
        FADN.WEST.2011 <- FADN.WEST[which(FADN.WEST$YEAR > 2011),]
    
        FADN.SOUTH.2004 <- FADN.SOUTH[which(FADN.SOUTH$YEAR < 2012),]
        FADN.SOUTH.2011 <- FADN.SOUTH[which(FADN.SOUTH$YEAR > 2011),]
        
        ## Define different datasets
        crop_data_list <- list("FADN" = FADN,
                               "FADN.2004" = FADN.2004,
                               "FADN.2011" = FADN.2011,
                               "FADN.NORTH" = FADN.NORTH,
                               "FADN.SOUTH" = FADN.SOUTH,
                               "FADN.EAST" = FADN.EAST,
                               "FADN.WEST" = FADN.WEST,
                               "FADN.NORTH.2004" = FADN.NORTH.2004,
                               "FADN.NORTH.2011" = FADN.NORTH.2011,
                               "FADN.EAST.2004" = FADN.EAST.2004,
                               "FADN.EAST.2011" = FADN.EAST.2011,
                               "FADN.WEST.2004" = FADN.WEST.2004,
                               "FADN.WEST.2011" = FADN.WEST.2011,
                               "FADN.SOUTH.2004" = FADN.SOUTH.2004,
                               "FADN.SOUTH.2011" = FADN.SOUTH.2011
                               )
        
        ## Define fixed effects
        all_FEs <- c("ID", "YEAR")
        
        ## Loop through each dataset
          for (DF in names(crop_data_list)) {
            
            crop_reg <- crop_data_list[[DF]]  # Select the dataset
            
            #------------------------------------------------------------------------------
            # 1) Fixed Effect Model Estimation for Whole Germany (All Weather Events)
            #------------------------------------------------------------------------------
            
            crop_GER_joint <- list()
            for(i in 0:2){
              crop_GER_joint[[i+1]] <- fixest::feols(
                log(CROP.DT.HA)  ~ Black_Frost  + Heat + Spring_Drought + Summer_Drought + 
                  Spring_Waterlogging + Summer_Waterlogging,
                crop_reg, fixef = all_FEs[1:i], cluster = ~ ID + YEAR
              )
            }
            
            #------------------------------------------------------------------------------
            # 2) Fixed Effect Model Estimation for Black Frost Only
            #------------------------------------------------------------------------------
            
            crop_GER_BF <- list()
            for(i in 0:2){
              crop_GER_BF[[i+1]] <- fixest::feols(
                log(CROP.DT.HA)  ~ Black_Frost, 
                crop_reg, fixef = all_FEs[1:i], cluster = ~ ID + YEAR
              )
            }
            
            #------------------------------------------------------------------------------
            # 3) Panel Model for Revenue Losses
            #------------------------------------------------------------------------------
            
            crop_results <- plm::plm(
              data = crop_reg,
              formula = log(CROP.DT.HA)  ~ Black_Frost  + Heat + Spring_Drought + 
                Summer_Drought + Spring_Waterlogging + 
                Summer_Waterlogging + factor(YEAR),
              effect = "individual", model = "within", index = c("ID", "YEAR")
            )
            
            tidy_crop_results <- broom::tidy(crop_results)
            saveRDS(tidy_crop_results, file = paste0("03_Output/02_Estimation/objects/",
                                              DF, "_", crop, "_", NUTS, "_", filtering, "_", crop.filter.threshold, "_", yield_bound, ".RDS"))
            
            #------------------------------------------------------------------------------
            # Save Output as tables, graphs, and objects
            #------------------------------------------------------------------------------
            
            tidy_crop_results <- readRDS(paste0("03_Output/02_Estimation/objects/",
                                                "FADN", "_", crop, "_", NUTS, "_", filtering, "_", crop.filter.threshold, "_", yield_bound, ".RDS"))
            
            
            ## GRAPHS
            plot_coefficients <- function(tidy_results, output_file) {
              
              ## Filter out factor variables (e.g., "factor(YEAR)")
              tidy_results_filtered <- tidy_results %>%
                filter(!grepl("^factor\\(", term)) %>% 
                mutate(
                  lower = estimate - 2.576 * std.error, # 99% confidence interval
                  upper = estimate + 2.576 * std.error#,
                  # term = recode(term, c("Black frost"))  # Replace terms with custom names
                )
              
              # Generate ggplot with reversed y-axis order
              coef_plot <- ggplot(tidy_results_filtered, aes(x = estimate, y = term)) +
                geom_point(size = 3, color = "black") +  # Points for estimates
                geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "black") +  # 99% CI
                geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Vertical line at 0
                scale_y_discrete(limits = rev(tidy_results_filtered$term)) +  # Reverse order of terms
                labs(
                  x = "",
                  y = "",
                  title = ""
                ) +
                theme_minimal()
              
              # Save the plot
              ggsave(output_file, coef_plot, dpi = 300, width = 8, height = 6)
              
              print(paste("Plot saved to:", output_file))
            }
            
            ## Example usage:
            plot_coefficients(tidy_crop_results, paste0("03_Output/02_Estimation/graphs/",
                                                        DF, "_", crop, "_", NUTS, "_", filtering, "_", crop.filter.threshold, "_", yield_bound, ".png"))
            
            ## OBJECTS
            saveRDS(tidy_crop_results, file = paste0("03_Output/02_Estimation/tables/",
                                                     DF, "_", crop, "_", NUTS, "_", filtering, "_", crop.filter.threshold, "_", yield_bound, ".tex"))
            
            ## TABLES
            ## Define filename with dataset name, crop, and NUTS
            latex_file <- file.path("03_Output/02_Estimation/tables",
                                      paste0(DF, "_", crop, "_", NUTS, "_", filtering, "_", crop.filter.threshold, "_", yield_bound, ".tex"))
              
            ## Save LaTeX table
            writeLines(
              knitr::kable(
                tidy_crop_results,
                format = "latex",
                booktabs = TRUE,
                digits = 4,
                caption = paste0("Regression Results for ", crop, " DF: ", DF),
                align = "lrrrr"
              ) %>%
                kable_styling(latex_options = "hold_position"),
              latex_file
            )
              
              print(paste("Saved results for:", DF, crop, NUTS, filtering, yield_bound, "threshold:", crop.filter.threshold))
          }
        }    
      }
    }
  }
}

