


Year_to_Filter_Data_by <- 2019

Export_END_Year <- 2021

options(scipen = 999)

{ # Library   ----
  
  # get libraries
  if (!require(librarian)){
    install.packages("librarian")
    library(librarian)
  }
  librarian::shelf(
    tidyverse,
    lubridate, 
    here, 
    janitor,
    glue,
    MASS,
    vegan,
    labdsv,
    lme4,
    car,
    randomForest,
    pdp, 
    scales,
    arrow,
    modelr,
    tidymodels,
    # Cairo,
    # ggpubr,
    # zoo,
    # indicspecies,
    quiet = TRUE)
}

{ # Species and Trophic Levels   ----
  
  species_info <- 
    readr::read_csv(
      here::here(
        "data", "meta", "Species_Complete.csv"
      )
    ) %>% clean_names()
  
  mixed_data_xref_biomass <- 
    readr::read_csv(
      here::here(
        "data", "meta", "Mixed_Data_xref_Fish_Biomass.csv"
      )
    ) %>% clean_names()
  
  mixed_data_xref_density <- 
    readr::read_csv(
      here::here(
        "data", "meta", "Mixed_Data_xref_Fish_Density.csv"
      )
    ) %>% clean_names()
  
  benthic_biomass_species <- c(
    "Macrocystis pyrifera", "giant kelp",
    "Crassedoma giganteum", "rock scallop",
    "Haliotis rufescens", "red abalone",
    "Haliotis corrugata", "pink abalone",
    "Haliotis fulgens", "green abalone",
    "Kelletia kelletii", "Kellet's whelk",           
    "Lithopoma gibberosa", "red turban snail", 
    "Lytechinus anamesus", "white sea urchin", 
    "Megastraea undosa", "wavy turban snail",             
    "Megathura crenulata", "giant keyhole limpet", 
    "Patiria miniata", "bat star",
    
    "Cypraea spadicea", "chestnut cowrie", 
    
    "Pisaster ochraceus", "ochre sea star",
    
    "Parastichopus parvimensis", "warty sea cucumber",
    
    
    
    "Pisaster giganteus", "giant-spined sea star",            
    "Pycnopodia helianthoides", "sunflower star",
    "Strongylocentrotus franciscanus", "red sea urchin", 
    "Strongylocentrotus purpuratus", "purple sea urchin",
    "Tethya aurantia", "orange puffball sponge", 
    "Tegula regina", "queen tegula", 
    "Muricea californica", "California golden gorgonian",
    "Muricea fruticosa", "brown gorgonian",
    "Lophogorgia chilensis", "red gorgonian")
  
  Potential_Biomass_Additions <- c(
    "Astrangia lajollaensis", "Corynactis californicus",
    "Phragmatopoma californica", "Serpulorbis squamiger",
    "Diaperoecia californica")
  
  fish_biomass_species <- c(
    "Caulolatilus princeps", "ocean whitefish",
    "Chromis punctipinnis", "blacksmith",
    "Embiotoca jacksoni", "black surfperch",
    "Embiotoca lateralis", "striped surfperch",
    "Girella nigricans", "opaleye",
    "Halichoeres semicinctus", "rock wrasse, female", "rock wrasse, male",     
    "Hypsypops rubicundus", "garibaldi",
    "Medialuna californiensis", "halfmoon",
    "Ophiodon elongatus", "lingcod",
    "Oxyjulis californica", "senorita",
    "Paralabrax clathratus", "kelp bass",
    "Rhacochilus toxotes", "rubberlip surfperch",
    "Rhacochilus vacca", "pile perch",
    "Scorpaena guttata", "California scorpionfish",
    "Scorpaenichthys marmoratus", "cabezon",
    "Sebastes atrovirens", "kelp rockfish",
    "Sebastes chrysomelas", "black and yellow rockfish",
    "Sebastes mystinus", "blue rockfish",  
    "Sebastes serranoides", "olive rockfish",
    "Sebastes serriceps", "treefish",
    "Semicossyphus pulcher", "California sheephead, male", 
    "California sheephead, female")
  
  juvenile_rf_cn <- c(
    "rockfish spp.",
    "rockfish spp., juvenile", 
    "olive/yellowtail rockfish, juvenile",                
    "kelp/gopher/copper/black and yellow rockfish, juvenile",
    "gopher rockfish, juvenile",                             
    "vermillion rockfish, juvenile",                         
    "black and yellow/gopher rockfish, juvenile",            
    "copper rockfish, juvenile",                             
    "canary rockfish, juvenile",                             
    "rosy rockfish, juvenile",                               
    "stripetail rockfish, juvenile",                         
    "halfbanded rockfish, juvenile",                         
    "brown rockfish, juvenile",                              
    "squarespot rockfish, juvenile",                         
    "calico rockfish, juvenile",                             
    "splitnose rockfish, juvenile"
  )
  juvenile_rf_sn <- c(
    "Sebastes",
    "Sebastes serranoides/flavidus",                    
    "Sebastes atrovirens/carnatus/caurinus/chrysomelas",
    "Sebastes carnatus",                                
    "Sebastes miniatus",                                
    "Sebastes chrysomelas/carnatus",                    
    "Sebastes caurinus",                                
    "Sebastes pinniger",                                
    "Sebastes rosaceus",                                
    "Sebastes saxicola",                                
    "Sebastes semicinctus",                             
    "Sebastes auriculatus",                             
    "Sebastes hopkinsi",                                
    "Sebastes dallii",                                  
    "Sebastes diploproa" 
  )
  
  vft_species <- c(
    "blacksmith", 
    "black_surfperch", 
    "striped_surfperch", 
    "opaleye",                     
    "garibaldi", 
    "senorita",
    "kelp_bass", 
    "pile_perch",                 
    "kelp_rockfish", 
    "blue_rockfish", 
    "olive_rockfish", 
    "California_sheephead_female",
    "California_sheephead_male", 
    "rock_wrasse_female", 
    "rock_wrasse_male")
  
  target_shapes <- c(
    "Targeted" = 10, 
    "Non-targeted" = 5, 
    'Mixed' = 9)
  
  biomass_all_groups <- c(
    "total benthic biomass", 
    "total fish biomass", 
    "Targeted", 
    "Non-targeted",
    "Detritivore", 
    "Herbivore", 
    "Planktivore", 
    "Producer",
    "Carnivore",
    "Piscivore")
  
  biomass_trophic_groups <- c(
    "Detritivore",
    "Producer",
    "Piscivore")
  
}

{ # Sites, Islands, & MPAs   -----
  
  site_info <- 
    readr::read_csv(
      here::here(
        "data", "meta", "Site_Info.csv"
      )
    ) %>% clean_names()
 
  island_code_levels <- c("SR", "SC", "AN", 'SB')
  
  island_levels_short <- 
    c("San Miguel", 
      "Santa Rosa", 
      "Santa Cruz", 
      "Anacapa", 
      "Santa Barbara")
  
  island_levels_long <- 
    c("San Miguel Island", 
      "Santa Rosa Island", 
      "Santa Cruz Island", 
      "Anacapa Island",  
      "Santa Barbara Island")
  
  mpa_levels_short <- 
    c("Santa Rosa", 
      "Santa Cruz", 
      "Anacapa",  
      "Santa Barbara")
  
  mpa_levels_long <- 
    c("Santa Rosa Island",
      "Santa Cruz Island", 
      "Anacapa Island",  
      "Santa Barbara Island")
  
}

{ # Water Temperature Anomaly Indices (ONI and PDO)   ----
  
  { # Oceanic Nino Index  ----
    oni <- read.table( 
      base::paste0(
        "https://origin.cpc.ncep.noaa.gov/",
        "products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt"
      ), header = T) %>%
      dplyr::mutate(date = make_date(YR, MON, 1),
                    date_start = date,
                    date_end = ceiling_date(date_start, "month")) %>%
      dplyr::rename(oni_anom = ANOM,
                    month = MON,
                    survey_year = YR) %>% 
      dplyr::select(survey_year, month, date, date_start, date_end, oni_anom) 
    
  }
  
  { # PDO  ----
    pdo <- read.table(
      paste0(
        "https://www.cpc.ncep.noaa.gov/",
        "products/GODAS/PDO/pdo_h300_pac_current.txt"
      ), header = T) %>%
      dplyr::mutate(date = make_date(Year, Month, 1),
                    date_start = date,
                    date_end = ceiling_date(date_start, "month")) %>%
      dplyr::rename(pdo_anom = PDO,
                    month = Month,
                    survey_year = Year) %>% 
      dplyr::select(survey_year, month, date, date_start, date_end, pdo_anom) 
    
  }
  
  { # Full Index  ----
    sst_anomaly_index <- dplyr::left_join(oni, pdo) %>% 
      readr::write_csv(
        here::here(
          "data", "tidy", "SST_Anomaly_Index.csv"
        )
      )
  }
  
  { # Mean ONI and PDO with lags ----
    sst_anomaly_index_mean <- sst_anomaly_index %>%
      dplyr::group_by(survey_year)  %>% 
      dplyr::summarise(
        mean_oni_anom = mean(oni_anom), 
        mean_pdo_anom = mean(pdo_anom)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        oni_lag_1 = dplyr::lag(mean_oni_anom, n = 1),
        oni_lag_2 = dplyr::lag(mean_oni_anom, n = 2),
        oni_lag_3 = dplyr::lag(mean_oni_anom, n = 3),
        oni_lag_4 = dplyr::lag(mean_oni_anom, n = 4),
        oni_lag_5 = dplyr::lag(mean_oni_anom, n = 5),
        
        pdo_lag_1 = dplyr::lag(mean_pdo_anom, n = 1),
        pdo_lag_2 = dplyr::lag(mean_pdo_anom, n = 2),
        pdo_lag_3 = dplyr::lag(mean_pdo_anom, n = 3),
        pdo_lag_4 = dplyr::lag(mean_pdo_anom, n = 4),
        pdo_lag_5 = dplyr::lag(mean_pdo_anom, n = 5)) %>%
      dplyr::filter(survey_year > 2004, survey_year < Export_END_Year + 1) %>% 
      readr::write_csv(
        here::here(
          "data", "tidy", "SST_Anomaly_Mean.csv"
        )
      ) 
    
  }
  
}

{ # Plot Themes   ----
  
  map_bubble_theme <- function() {
    ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right", 
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5))
  }
  
  all_sites_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.major = element_line(),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        legend.spacing.y = unit(.01, 'cm'),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  
  timeseries_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color = "black"),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 10, color="black"),
        axis.text.y = element_text(size = 10, color="black"),
        axis.text.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  timeseries_bottom_theme <- function (){
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color="black"),
        plot.caption = element_text(size = 13),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color="black"),
        axis.line.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  Original_16_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color = "black"),
        legend.key.width = unit(.75, "cm"),
        legend.justification = c(0, 0.5),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.y = element_text(size = 10, color="black"),
        axis.text.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  Original_16_bottom_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color="black"),
        legend.position = "right",
        legend.justification = c(0,0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_line(),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color="black"),
        axis.text.y = element_text(size = 10, color="black"),
        axis.line.x = element_blank(),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  
  nMDS_theme <- function () {
    ggplot2::theme_minimal() + 
      ggplot2::theme(
        legend.position = "bottom", 
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.caption = element_text(size=9, hjust = 0),
        aspect.ratio=1) 
  }
  
  PDP_theme <- function() {
    theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            axis.line = element_line())
  }
  
  ISA_theme <- function () {
    ggplot2::theme_bw() + 
      ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                     axis.text.x = element_text(size = 10.5, color = "black", angle = 45, hjust = 1, vjust = 1),
                     axis.text.y = element_text(size = 11, color = "black", face = "italic"),
                     axis.title = element_text(size = 14),
                     legend.position = "right",
                     legend.text = element_text(size = 10, color = "black"),
                     legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),  
                     panel.grid.minor = element_blank())
  }
  
  Ratio_theme <- function () {
    theme_classic() +
      theme(panel.grid.major = element_line(),
            legend.position = "bottom",
            strip.background = element_blank())
  }
  
  Biomass_Summary_theme <- function () {
    ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(fill = FALSE),
        legend.position = "right",
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        panel.spacing.x = unit(.45, "cm"),
        strip.text = element_text(size = 10, colour = "black"))
  }
  
  Boxplot_theme <- function() {
    theme_classic() +
      theme(plot.title = element_text(size = 16, face = "italic"),
            plot.subtitle = element_text(size = 14),
            axis.title = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
            strip.text = element_text(size = 12, angle = 90),
            legend.position = "bottom",
            legend.background = element_rect(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            plot.caption = element_text(size = 10, hjust = 0),
            axis.line.x = element_blank())
  }
  
}

{ # Plot Templates  ----
  
  { # Defaults   ---- 
    
    p_1 <- ggplot2::ggplot() +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
      ggplot2::scale_color_viridis_d(end = .8) +
      ggplot2::labs(x = NULL, y = NULL, linetype = "Reserve Status", color = "Reserve Status", tag = "A") +
      timeseries_top_theme()
    
    p_2 <- ggplot2::ggplot() +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::labs(x = NULL, y = NULL, color = "Island", tag = "B") +
      timeseries_top_theme()
    
    p_3 <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = sst_anomaly_index_mean, 
                         aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = 0, fill = oni_anom)) +
      ggplot2::scale_fill_viridis_c(
        option = "plasma",
        guide = guide_colorbar(direction = "horizontal", title.position = "top",
                               order = 3, barheight = unit(.2, "cm"))) +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0.01)),
                                  limits = c(0, NA), oob = squish) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::geom_hline(aes(yintercept = 0)) +
      ggplot2::guides(color = guide_legend(order = 1), 
                      linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
      ggplot2::labs(x = "Survey Year", y = NULL, color = "Island", tag = "C",
                    fill = "Oceanic Ni\u00f1o Index", linetype = "Reserve Status") +
      timeseries_bottom_theme()
    
    p_1_16 <- ggplot2::ggplot() + 
      ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
      ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                              hjust = 1, label = "MPAs Created (2003)"), color = "black") +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                  limits = c(0, NA), oob = squish) +
      ggplot2::scale_color_viridis_d(end = .8) +
      ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                    color = "Reserve Year", linetype = "Reserve Year",
                    x = NULL, y = NULL) +
      Original_16_top_theme()
  }
  
  { # Diversity Plots   ----
    Diversity_Plot <- function(Sci_Name) {
      DF <- Diversity |> 
        dplyr::filter(Index == Sci_Name)
      
      Ind <- if(Sci_Name == "shannon_2005") {"Shannon's"} else {"Simpson's"}
      
      p1 <- ggplot2::ggplot() +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = ReserveStatus), size = 1.25, 
                             method = 'loess', formula = 'y~x') +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(NA, NA), expand = c(0, 0), oob = squish) +
        ggplot2::scale_color_viridis_d(end = .8) +
        ggplot2::labs(x = NULL, y = NULL, linetype = "Reserve Status", color = "Reserve Status", tag = "A") +
        timeseries_top_theme() 
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x') +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(NA, NA), expand = c(0, 0), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(x = NULL, y = NULL, tag = "B",
                      color = "Island") +
        timeseries_top_theme()
      
      p3 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = sst_anomaly_index_mean,
                           aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = .4, fill = oni_anom)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, 
                             aes(x = Date, y = Value, color = IslandName, linetype = ReserveStatus),
                             method = 'loess', formula = 'y~x', se = F) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(.4, NA), oob = squish) +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(x = "Survey Year", y = NULL,
                      color = "Island", tag = "C",
                      fill = "Oceanic Ni\u00f1o Index",
                      linetype = "Reserve Status") +
        timeseries_bottom_theme() 
      Div_Plot <-ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      Div_annotated <- ggpubr::annotate_figure(
        Div_Plot,
        left = text_grob(paste(Ind, " Index (Diversity)", sep = ""), 
                         family ="Cambria", color = "black", rot = 90, size = 13))
      print(Div_annotated)
    }
  }
  
  { # Diversity Original 16 Plot  ----
    Diversity_16_Plot <- function(Sci_Name) {
      
      DF <- Diversity_Orginal_16 |> 
        dplyr::filter(Index == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      Ind <- if(Sci_Name == "shannon_all"){"Shannon's"}else{"Simpson's"}
      
      p1 <- ggplot2::ggplot() + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x') +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
        ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                                hjust = 1, label = "MPAs Created (2003)"), color = "black") +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                    limits = c(NA, NA), oob = squish) +
        ggplot2::scale_color_viridis_d(end = .8) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                      color = "Reserve Year", linetype = "Reserve Year",
                      x = NULL, y = NULL) +
        Original_16_top_theme()
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = SST, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = .4, fill = oni_anom)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                             aes(x = Date, y = Value, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(.4, NA), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme()
      
      Orig16_Diversity_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Diversity_Annotated <- ggpubr::annotate_figure(
        Orig16_Diversity_Plot,
        left = text_grob(paste(Ind, " Index (Diversity)", sep = ""), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Diversity_Annotated)
    }
  }
  
  { # Mean Density Plot   ----
    Mean_Density_Plot <- function(Sci_Name) {
      DF <- Density |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = IslandName),
                            size = 1.25, method = 'loess', formula = 'y~x') 
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = IslandName, linetype = ReserveStatus),
                             method = 'loess', formula = 'y~x', size = 1.25, se = FALSE)
      
      Density_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      Density_annotated <- 
        ggpubr::annotate_figure(
          Density_Plot,
          left = text_grob(paste(Sci_Name, " density (#/\U33A1)"), color = "black", rot = 90, size = 12))
      
      print(Density_annotated)
    }
  }
  
  { # Mean Density Original 16 Plot  ----
    Mean_Density_16_Plot <- function(Sci_Name, Com_Name) {
      
      DF <- Density_Orginal_16 |> 
        dplyr::filter(ScientificName == Sci_Name,
                      CommonName %in% Com_Name) |> 
        dplyr::group_by(ScientificName, SurveyYear, SiteNumber) |> 
        dplyr::mutate(Mean_Density = sum(Count)/2000)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      if(Sci_Name == "Parastichopus parvimensis"){
        p1 <- p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x') +
          ggplot2::geom_vline(aes(xintercept = as.Date("1993-01-01")), size = 1) +
          ggplot2::geom_label(aes(x = as.Date("1993-01-01"), y = Inf, vjust = 1,
                                  hjust = 1, label = "Dive Fishery Begins"), color = "black")
        
        p2 <- ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = SST, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = 0, fill = oni_anom)) +
          ggplot2::scale_fill_viridis_c(
            option = "plasma",
            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                   order = 3, barheight = unit(.2, "cm"))) +
          ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                               aes(x = Date, y = Mean_Density, color = IslandName)) +
          ggplot2::geom_vline(aes(xintercept = as.Date("1993-01-01")), size = 1) +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
          ggplot2::geom_hline(aes(yintercept = 0)) +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d() +
          ggplot2::guides(color = guide_legend(order = 1), 
                          linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                        color = "Island",
                        x = "Survey Year", y = NULL,
                        fill = "Oceanic Ni\u00f1o Index") +
          Original_16_bottom_theme()
      } 
      else {
        p1 <- ggplot2::ggplot(data = DF, aes(x = Date, y = Mean_Density, color = ReserveYear)) + 
          ggplot2::geom_smooth(size = 1.25, method = 'loess', formula = 'y~x') +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
          ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                                  hjust = 1, label = "MPAs Created (2003)"), color = "black") +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d(end = .8) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                        color = "Reserve Year", linetype = "Reserve Year",
                        x = NULL, y = NULL) +
          Original_16_top_theme()
        
        p2 <- ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = SST, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = 0, fill = oni_anom)) +
          ggplot2::scale_fill_viridis_c(
            option = "plasma",
            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                   order = 3, barheight = unit(.2, "cm"))) +
          ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                               aes(x = Date, y = Mean_Density, color = IslandName)) +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
          ggplot2::geom_hline(aes(yintercept = 0)) +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d() +
          ggplot2::guides(color = guide_legend(order = 1), 
                          linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                        color = "Island",
                        x = "Survey Year", y = NULL,
                        fill = "Oceanic Ni\u00f1o Index") +
          Original_16_bottom_theme()
      }
      
      Orig16_Density_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Density_Annotated <- ggpubr::annotate_figure(
        Orig16_Density_Plot,
        left = text_grob(paste(DF$ScientificName, " density (#/\U33A1)"), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Density_Annotated)
    }
  }
  
  { # Mean Biomass Plot   ----
    Mean_Biomass_Plot <- function(Sci_Name, Com_Name) {
      DF <- Biomass |> 
        dplyr::filter(ScientificName == Sci_Name,
                      CommonName == Com_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x') 
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, method = 'loess', formula = 'y~x', size = 1.25,
                             aes(x = Date, y = Mean_Biomass, color = IslandName, 
                                 linetype = ReserveStatus), se = FALSE) 
      
      Biomass_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      Biomass_annotated <- 
        ggpubr::annotate_figure(
          Biomass_Plot,
          left = text_grob(paste(Sci_Name, " biomass (g/\U33A1)"), color = "black", rot = 90, size = 12))
      
      print(Biomass_annotated)
    }
  }
  
  { # Mean Biomass Original 16 Plot  ----
    Mean_Biomass_16_Plot <- function(Sci_Name) {
      
      DF <- dplyr::filter(Biomass_Orginal_16, ScientificName == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      p1 <- p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = SST,
                           aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = 0, fill = oni_anom)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                             aes(x = Date, y = Mean_Biomass, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(0, NA), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme()
      
      Orig16_Biomass_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Biomass_Annotated <- ggpubr::annotate_figure(
        Orig16_Biomass_Plot,
        left = text_grob(paste(DF$ScientificName, "biomass (g/\U33A1)"), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Biomass_Annotated)
    }
  }
  
  { # Percent Cover Plot  ----
    Percent_Cover_Plot <- function(Sci_Name) {
      DF <- RPC |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x') 
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, method = 'loess', formula = 'y~x', size = 1.25,
                             aes(x = Date, y = Percent_Cover, color = IslandName, 
                                 linetype = ReserveStatus), se = FALSE) 
      
      cover_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      cover_annotated <- 
        ggpubr::annotate_figure(
          cover_Plot,
          left = text_grob(paste(Sci_Name, " percent cover", sep = ""), color = "black", rot = 90, size = 12))
      
      print(cover_annotated)
    }
  }
  
  { # Percent Cover Original 16 Plot  ----
    Percent_Cover_Original_16_Plot <- function(Sci_Name) {
      DF <- RPC_Original_16 |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      p1 <-  p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = SST,
                           aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = 0, fill = oni_anom)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x', 
                             aes(x = Date, y = Percent_Cover, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(0, NA), oob = squish) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme() 
      
      Cover_Orig16_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Cover_Orig16_annotated <- ggpubr::annotate_figure(
        Cover_Orig16_Plot,
        left = text_grob(paste(Sci_Name, " percent cover", sep = ""),
                         color = "black", rot = 90, size = 12))
      print(Cover_Orig16_annotated)
    }
  }
  
}




