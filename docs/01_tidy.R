#
#
#
#
#     Use for Manipulating data frames 
#
#
#     Use ALT + O to see outline
#
#

source(here::here("docs", "00_global.R"))

{ # Density   ----
  
  { # 1 m Density     ----
    one_meter <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_1mQuadrat_Summary_1982-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      tidyr::separate(survey_date, c('date','time'),' ') %>%
      dplyr::left_join(site_info) %>% 
      dplyr::mutate(
        date = lubridate::mdy(date),
        survey_type = "1 m quads") %>% 
      dplyr::filter(
        island_code != "CL",
        !common_name %in% c(
          "giant kelp stipes > 1m",
          'giant kelp, all',
          "California sea palm, all",
          "oar weed, all",
          "Southern sea palm, all"),
        !scientific_name %in% c(
          "Undaria pinnatifida",
          "Dictyoneuropsis reticulata/Agarum fimbriatum",
          "Haliotis rufescens",
          "Crassedoma giganteum",
          "Kelletia kelletii",
          "Oxylebius pictus",
          "Pycnopodia helianthoides",
          "Lytechinus anamesus",
          "Sargassum horneri"),
        scientific_name != "Pisaster giganteus" | survey_year < 1996,
        common_name != "giant kelp, adult (>1m)" | survey_year < 1996) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date,
        scientific_name, common_name, 
        total_count, mean_density_sqm, total_area_surveyed_sqm, 
        mean_depth, survey_type, reserve_status, reference) 
  } 
  
  { # 5 m Density    ----
  
    five_meter <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_5mQuadrat_Summary_1996-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      tidyr::separate(survey_date, c('date','time'),' ') %>%
      dplyr::left_join(site_info) %>%
      dplyr::mutate(
        date = lubridate::mdy(date),
        survey_type = "5 m quads",
        common_name = fct_inorder(common_name),
        common_name = forcats::fct_collapse(
          common_name, 
          "giant kelp, adult (>1m)" = c(
            "giant kelp, adult (>1m and haptera above the primary dichotomy)",
            "giant kelp, subadult (>1m and no haptera above the primary dichotomy)"))) %>% 
      dplyr::group_by(site_number, survey_year) %>% 
      dplyr::filter(date == max(date)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date,
        scientific_name, common_name, 
        total_area_surveyed_sqm, 
        mean_depth, survey_type, reserve_status, reference) %>% 
      dplyr::summarise(total_count = sum(total_count),
                       mean_density_sqm = total_count / 200) %>%
      dplyr::ungroup() %>%  
      dplyr::relocate(c(total_count, mean_density_sqm), .after = common_name) %>% 
      dplyr::filter(
        island_code != "CL",
        scientific_name != "Pisaster giganteus" | survey_year < 2014,
        scientific_name != "Pisaster ochraceus" | survey_year < 2014,
        scientific_name != "Undaria pinnatifida")
    
  }
  
  { # Bands Density    ----
    bands <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_BandTransect_Summary_1982-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      tidyr::separate(survey_date, c('date','time'),' ') %>%
      dplyr::left_join(site_info) %>% 
      dplyr::mutate(
        date = lubridate::mdy(date),
        survey_type = "bands transects") %>% 
      dplyr::filter(
        island_code != "CL",
        scientific_name != "Sargassum horneri") %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date,
        scientific_name, common_name, 
        total_count, mean_density_sqm, total_area_surveyed_sqm, 
        mean_depth, survey_type, reserve_status, reference)  
    
  }
  
  { # Benthic Density   ----
    benthic_density <- base::rbind(one_meter, five_meter, bands)
  }
  
  { # RDFC    ----
    
    rdfc <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_RovingDiverFishCount_Summary_1982-", Export_END_Year, ".txt")
        ), locale = locale(encoding = "ISO-8859-1")) %>% 
      janitor::clean_names() %>% 
      tidyr::separate(survey_date, c('date','time'),' ') %>%
      dplyr::left_join(site_info) %>% 
      dplyr::rename(total_count = avg_count) %>%
      dplyr::mutate(
        date = lubridate::mdy(date),
        survey_type = "rdfc",
        scientific_name = dplyr::case_when(
          common_name %in% juvenile_rf ~ "rockfish YOY", T ~ scientific_name),
        common_name = dplyr::case_when(
          common_name %in% juvenile_rf ~ "rockfish YOY",
          common_name == 'garibaldi, subadult' ~ 'garibaldi, juvenile',
          T ~ common_name)) %>%
      dplyr::group_by(site_code, survey_year) %>%
      dplyr::filter(
        island_code != "CL",
        survey_year > 2003,
        !common_name %in% c(
          "blue-banded goby", "blackeye goby", "island kelpfish",
          "copper rockfish, all",
          "black surfperch, all", "blacksmith, all", 
          "blue rockfish, all", "kelp bass, all", 
          "kelp rockfish, all", "olive rockfish, all",  
          "opaleye, all", "pile perch, all", 
          "señorita, all", "striped surfperch, all"),
        date == base::max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date,
        scientific_name, common_name, 
        mean_depth, survey_type, reserve_status, reference) %>% 
      dplyr::summarise(
        total_area_surveyed_sqm = 2000,
        total_count = sum(total_count),
        mean_density_sqm = total_count / total_area_surveyed_sqm
      )
    
  }
  
  { # VFT   ----
    vft <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_VisualFishTransect_NormalizedDataForTransect_1985-", 
                 Export_END_Year, ".txt")
        ), locale = locale(encoding = "ISO-8859-1")) %>% 
      janitor::clean_names() %>% 
      tidyr::separate(survey_date, c('date','time'),' ') %>%
      dplyr::left_join(site_info) %>% 
      dplyr::rename(total_count = count_a) %>%
      dplyr::mutate(
        date = lubridate::mdy(date),
        survey_type = "vft",
        total_area_surveyed_sqm = 400, 
        mean_density_sqm = total_count / total_area_surveyed_sqm) %>%
      dplyr::group_by(site_number, survey_year) %>%
      dplyr::filter(
        date == max(date),
        island_code != "CL") %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date,
        scientific_name, common_name, 
        total_count, mean_density_sqm, total_area_surveyed_sqm, 
        mean_depth, survey_type, reserve_status, reference) 
    
  }
  
  { # Fish Density    ----
    
    fish_density <- rbind(rdfc, vft) 
    
  }
  
  { # Density Combined  ----
    density <- rbind(benthic_density, fish_density) %>%  
      dplyr::left_join(
        site_info %>%
          dplyr::select(site_name, mean_depth, reserve_status, reference,
                        reserve_year, latitude, longitude)) %>%
      dplyr::left_join(
        species_info %>%
          dplyr::select(common_name, classification)) %>%
      dplyr::mutate(
        site_count = mean_density_sqm * 2000,
        common_name = gsub('ñ', 'n', common_name),
        reserve_status = case_when(
          survey_year < 2003 & site_code == "LC" ~ "Inside",
          survey_year < 2003 & site_code == "CC" ~ "Inside",
          survey_year < 2003 ~ "Outside",
          TRUE ~ reserve_status)) %>% 
      dplyr::relocate(site_count, .after = common_name) %>% 
      readr::write_csv(here::here("data", "tidy", "density.csv"))
  }
  
}

{ # RPC % Cover   ----
  
  rpc <- 
    readr::read_csv(here::here(
      "data", "summary", 
      paste0(
        "KFM_RandomPointContact_Summary_1982-", Export_END_Year, ".txt"))) %>% 
    janitor::clean_names() %>% 
    tidyr::separate(survey_date, c('date','time'),' ') %>%
    dplyr::left_join(site_info) %>% 
    dplyr::mutate(
      date = lubridate::mdy(date),
      survey_type = "rpc",
      scientific_name = dplyr::case_when(
        common_name == "encrusting coralline algae" ~ "encrusting coralline algae",
        common_name == "articulated coralline algae" ~ "articulated coralline algae",
        common_name %in% c(
          "Miscellaneous Invertebrates excluding Ophiothrix spiculata",
          "Miscellaneous Invertebrates including Ophiothrix spiculata",
          "spiny brittle star") ~ "Misc Inverts", TRUE ~ scientific_name),
      common_name = dplyr::case_when(
        scientific_name %in% c(
          "Misc Inverts") ~ "Misc Inverts", TRUE ~ common_name)) %>%
    dplyr::group_by(site_number, survey_year) %>% 
    dplyr::filter(
      date == base::max(date),
      island_code != "CL",
      !scientific_name %in% c(
        "Macrocystis, Pterygophora, and Eisenia combined", 
        "Leucetta losangelensis", "Hydrozoa", "Balanus",
        "Sargassum muticum", "Polymastia pachymastia",
        "Spirobranchus spinosus")) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(
      site_number, island_code, island_name, site_code, site_name, 
      survey_year, date,
      scientific_name, common_name, 
      total_area_surveyed_sqm, 
      mean_depth, survey_type, reserve_status, reference, 
      reserve_year, latitude, longitude) %>%
    dplyr::summarise(
      mean_density_sqm = sum(mean_density_sqm, na.rm = T),
      total_count = mean_density_sqm * total_area_surveyed_sqm) %>%
    dplyr::distinct(site_number, survey_year, mean_density_sqm, .keep_all = T) %>% 
    dplyr::ungroup() %>% 
    dplyr::relocate(c(total_count, mean_density_sqm), .after = common_name) %>%
    dplyr::mutate(
      reserve_status = case_when(
        survey_year < 2003 & site_code == "LC" ~ "Inside",
        survey_year < 2003 & site_code == "CC" ~ "Inside",
        survey_year < 2003 ~ "Outside",
        TRUE ~ reserve_status)) %>% 
    readr::write_csv(here::here("data", "tidy", "rpc.csv"))
  
}

{ # Diversity   ----
  
  { # Shannon's Index   ---- 
    
    diversity_shannon <- readr::read_csv(
      here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(
        # survey_year > 2004, reference == TRUE,
        survey_type != "vft") %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, reserve_status, mean_depth, reference, reserve_year) %>% 
      dplyr::summarise(shannon_index = vegan::diversity(
        total_count, index = "shannon")) %>% 
      dplyr::relocate(shannon_index, .after = survey_year) 
  
  }
  
  { # Simpson's Index  ----
    
    diversity_simpson <- readr::read_csv(
      here::here("data", "tidy", "rpc.csv")) %>% 
      dplyr::filter(
        # survey_year > 2004, reference == TRUE,
        !scientific_name %in% c("Rock", "Sand", "Cobble", "Bare Substrate")) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, reserve_status, mean_depth, reference, 
        reserve_year, latitude, longitude) %>% 
      dplyr::summarise(simpson_index = vegan::diversity(
        total_count, index = "simpson")) %>% 
      dplyr::relocate(simpson_index, .after = survey_year)
      
  }
  
  { # Diversity Complete   ----
    
    diversity <- left_join(diversity_shannon, diversity_simpson) %>%  
      dplyr::mutate(date = lubridate::make_date(survey_year, 7, 1)) %>%
      readr::write_csv(here::here("data", "tidy", "diversity.csv"))
    
  }
  
}

{ # nMDS Calculations    ----
  
  density <- readr::read_csv(
    here::here("data", "tidy", "density.csv")) %>% 
    dplyr::filter(
      survey_type != "vft", survey_year > 2004, reference == TRUE) %>% 
    dplyr::select(
      site_number, island_code, island_name, site_code, site_name, 
      survey_year, common_name, total_count, reserve_status) %>%  
    tidyr::pivot_wider(
      names_from = common_name, values_from = total_count, values_fill = 0) %>% 
    dplyr::mutate(island_name = factor(island_name, levels = island_levels_long))
  
  
  anosim_table <- data.frame(survey_year = integer(), P_val = double(), R_statistic = double())
  
  df_ellipse <- data.frame(survey_year = integer(), island_name = character())
  
  nMDS_2D_2005 <- data.frame(
    NMDS1 = double(), NMDS2 = double(), site_code = character(), site_name = character(),
    island_name = character(), reserve_status = character(), survey_year = integer())
  
  for (year in unique(density$survey_year)) {
    nMDS_Table <- density %>%
      filter(survey_year %in% year) %>%
      arrange(island_name) %>% 
      droplevels()
    
    nMDS <- nMDS_Table %>%
      dplyr::select(-site_number, -island_code, - island_name, -site_code,
                    -site_name, - survey_year, - reserve_status) %>%
      metaMDS(k = 2, trymax = 100)
    
    data_scores <- as.data.frame(scores(nMDS))
    data_scores$site_code <- nMDS_Table$site_code
    data_scores$site_name <- nMDS_Table$site_name
    data_scores$island_name <- nMDS_Table$island_name
    data_scores$reserve_status <- nMDS_Table$reserve_status
    data_scores$survey_year <- year
    
    nMDS_2D_2005 <- rbind(data_scores, nMDS_2D_2005)
    
    plot.new() # this is here because ordiellipse pops an error that plot.new() hasn't been opened yet
    ellipses <- ordiellipse(
      ord = nMDS, groups = nMDS_Table$island_name, 
      display = "sites", kind = "sd", conf = 0.95, label = T)
    
    
    veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) {
      theta <- (0:npoints) * 2 * pi / npoints
      Circle <- cbind(cos(theta), sin(theta))
      t(center + scale * t(Circle %*% chol(cov)))
    }
    
    for(island in unique(nMDS_Table$island_name)){
      df_ellipse <- rbind(
        df_ellipse, 
        cbind(as.data.frame(with(
          nMDS_Table[nMDS_Table$island_name == island,],
          veganCovEllipse(ellipses[[island]]$cov,
                          ellipses[[island]]$center, 
                          ellipses[[island]]$scale))),
          island_name = island,
          survey_year = year))
    }
    
    anosim_output <- nMDS_Table %>%
      dplyr::select(-island_code, -island_name, -site_code, -site_name, -survey_year, -reserve_status) %>%
      anosim(nMDS_Table$island_name)
    
    anosim_table <- anosim_table %>%
      add_row(survey_year = year, P_val = anosim_output$signif, R_statistic= anosim_output$statistic)
    
  }
  
  nMDS_2D_2005 %>%
    dplyr::left_join(site_info) %>% 
    dplyr::select(site_number, island_code, island_name, site_code, site_name, survey_year, 
                  NMDS1, NMDS2, reserve_status, reference) %>%
    readr::write_csv(here::here("data", "tidy", "nMDS.csv"))
  
  df_ellipse %>%
    dplyr::left_join(site_info) %>% 
    dplyr::select(site_number, island_code, island_name, site_code, site_name, survey_year, 
                  NMDS1, NMDS2, reserve_status, reference) %>%
    readr::write_csv(here::here("data", "tidy", "ellipses.csv"))
  
  readr::write_csv(anosim_table, here::here("data", "tidy", "ANOSIM.csv"))
  
}

{ # Benthic Biomass   ---- 
  
  { # Conversion Coefficients  ----
    
    biomass_conversions <- 
      readr::read_csv(
        here::here(
          "data", "meta", "biomass_conversions.csv"
        )) %>% 
      janitor::clean_names() 
  }

  { # Kelp    -----
    
    kelp_stipes <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_Macrocystis_Summary_1984-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(island_code != "CL",
                    dimensions_measured == "Stipe_Count") %>%  
      dplyr::rename(mean_size = mean_dimensions) %>% 
      dplyr::mutate(biomass = mean_size * 8.477333) 
    
    kelp_biomass <- readr::read_csv(
      here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(common_name == "giant kelp, adult (>1m)") %>% 
      dplyr::select(-total_count) %>%
      dplyr::left_join(site_info) %>% 
      dplyr::left_join(kelp_stipes)  %>%  
      dplyr::mutate(
        mean_density_sqm = dplyr::case_when(
          !is.na(biomass) & mean_density_sqm == 0 ~ total_count / 2000,
          T ~ mean_density_sqm),
        mean_biomass = dplyr::case_when(
          is.na(biomass) & mean_density_sqm == 0 ~ 0,
          T ~ biomass * mean_density_sqm),
        common_name = str_replace(common_name,  ", adult \\([^\\)]+\\)", "")) %>%  
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date, scientific_name, common_name, mean_biomass,
        mean_density_sqm, reserve_status, reference, reserve_year)
    # ggplot(data = kelp_biomass, aes(x = log(mean_density_sqm), y = log(mean_biomass))) +
    #   geom_point() +
    #   geom_smooth(method = lm)
    
  }
  
  { # Gorgonian     ----
    
    gorg_sizes <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_Gorgonians_RawData_1984-2014_ NHSF_ 2015-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(island_code != "CL") %>%  
      dplyr::rename(size = width_cm) %>%
      tidyr::uncount(weights = count) %>% 
      dplyr::mutate(
        biomass = dplyr::case_when(
          scientific_name ==  "Lophogorgia chilensis" ~ (.018 * 10 ^ 1.529) * size ^ 1.529,
          T ~ (.002 * 10 ^ 1.529) * size ^ 2.001)) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, scientific_name, common_name) %>% 
      dplyr::summarise(
        total_count = n(),
        mean_size = mean(size),
        biomass = mean(biomass)) %>% 
      dplyr::ungroup()
    
    gorg_biomass <- readr::read_csv(
      here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(scientific_name %in% c(
        "Lophogorgia chilensis", "Muricea californica", "Muricea fruticosa")) %>% 
      dplyr::select(-total_count) %>%
      dplyr::left_join(site_info) %>% 
      dplyr::left_join(gorg_sizes) %>%  
      dplyr::mutate(
        mean_density_sqm = dplyr::case_when(
          !is.na(biomass) & mean_density_sqm == 0 ~ total_count / 2000,
          T ~ mean_density_sqm),
        mean_biomass = dplyr::case_when(
          is.na(biomass) & mean_density_sqm == 0 ~ 0,
          T ~ biomass * mean_density_sqm)) %>% 
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date, scientific_name, common_name, mean_biomass,
        mean_density_sqm, reserve_status, reference, reserve_year)

  }
  
  { # Invertebrate    ----
    
    invert_sizes <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_NHSF_Summary_1985-", Export_END_Year, ".txt")
        )) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(island_code != "CL", site_number < 38) %>% 
      dplyr::left_join(biomass_conversions) %>% 
      tidyr::drop_na(a) %>%  
      dplyr::mutate(
        biomass = case_when(
          measurement ==  "body diameter" ~  a * (mean_size * 2) ^ b,
          TRUE ~ a * mean_size ^ b)) %>% 
      dplyr::select(-classification)
      
    invert_biomass <- readr::read_csv(
      here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(scientific_name %in% unique(invert_sizes$scientific_name)) %>% 
      dplyr::select(-total_count) %>%
      dplyr::left_join(invert_sizes) %>%  
      dplyr::left_join(site_info) %>%
      dplyr::mutate(
        mean_density_sqm = dplyr::case_when(
          !is.na(biomass) & mean_density_sqm == 0 ~ total_count / 2000,
          T ~ mean_density_sqm),
        mean_biomass = dplyr::case_when(
          is.na(biomass) & mean_density_sqm == 0 ~ 0,
          biomass == 0 & mean_density_sqm > 0 ~ as.double(NA),
          T ~ biomass * mean_density_sqm)) %>% 
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date, scientific_name, common_name, mean_biomass,
        mean_density_sqm, reserve_status, reference, reserve_year)

  }
  
  { # Model  ----
    
    benthic_biomass <- base::rbind(kelp_biomass, gorg_biomass, invert_biomass) 
    
    sp_model <- function(df){lm(formula(mean_biomass ~ mean_density_sqm + 0), data = df)}
    
    benthic_biomass_model <- benthic_biomass %>% 
      dplyr::group_by(common_name, reserve_status) %>% 
      tidyr::nest() %>% 
      dplyr::mutate(
        model = map(data, sp_model),
        tidy = map(model, broom::tidy),
        predictions = map2(data, model, modelr::add_predictions),
        residuals = map2(data, model, modelr::add_residuals)) 
    
    benthic_biomass <- benthic_biomass_model %>%
      tidyr::unnest(predictions) %>%
      dplyr::mutate(
        mean_biomass = dplyr::case_when(
          is.na(mean_biomass) ~ pred,
          T ~ mean_biomass)) %>% 
      dplyr::select(
        -c(data, model, tidy, residuals, date,
           mean_density_sqm, pred, reserve_year)) %>%
    dplyr::left_join(
      species_info %>% 
        dplyr::distinct(scientific_name, trophic_broad, targeted_broad)) %>% 
      dplyr::ungroup() %>% 
      dplyr::relocate(common_name, .after = scientific_name) %>% 
      dplyr::relocate(reserve_status, .before = reference)
    
  }
  
  { # Group: Total  -----
    benthic_biomass_total <- benthic_biomass %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference) %>% 
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>% 
      dplyr::mutate(
        scientific_name = "Total benthic", 
        common_name = "Total benthic", 
        trophic_broad = 'Mixed', 
        targeted_broad = 'Mixed') %>% 
      dplyr::ungroup()
    
  }
  
  { # Group: Fishery Status     ----- 
    
    benthic_biomass_target <- benthic_biomass %>%
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference, targeted_broad) %>%
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>%
      dplyr::mutate(
        scientific_name = targeted_broad, 
        common_name = targeted_broad,
        trophic_broad = 'Mixed') %>%
      dplyr::ungroup()
    
  }
  
  { # Group: Trophic Level    ----
    
    benthic_biomass_trophic <- benthic_biomass %>%
      dplyr::filter(!trophic_broad %in% c('Detritivore', 'Producer')) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference, trophic_broad) %>%
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>%
      dplyr::mutate(
        scientific_name = trophic_broad, 
        common_name = trophic_broad,
        targeted_broad = 'Mixed') %>%
      dplyr::ungroup() 

  }
  
  { # Complete   -----
    
    benthic_biomass_complete <- dplyr::bind_rows(
      benthic_biomass, benthic_biomass_total, 
      benthic_biomass_target, benthic_biomass_trophic) %>%
      dplyr::left_join(
        species_info %>% 
          dplyr::filter(classification %in% c('Invertebrates', 'Algae')) %>% 
          dplyr::distinct(scientific_name, classification)) %>% 
      dplyr::mutate(
        common_name = dplyr::case_when(
          common_name == 'Non-targeted' ~ 'Non-targeted invert',
          common_name == 'Targeted' ~ 'Targeted invert',
          common_name == 'Carnivore' ~ 'Carnivorous invert',
          common_name == 'Herbivore' ~ 'Herbivorous invert',
          common_name == 'Planktivore' ~ 'Planktivorous invert',
          T ~ common_name),
        scientific_name = dplyr::case_when(
          scientific_name == 'Non-targeted' ~ 'Non-targeted invert',
          scientific_name == 'Targeted' ~ 'Targeted invert',
          scientific_name == 'Carnivore' ~ 'Carnivorous invert',
          scientific_name == 'Herbivore' ~ 'Herbivorous invert',
          scientific_name == 'Planktivore' ~ 'Planktivorous invert',
          T ~ scientific_name))
  }
  
}

{ # Fish Biomass   ----
  
  { # Fish sizes   ----
    
    fish_sizes <- 
      readr::read_csv(
        here::here(
          "data", "summary", 
          paste0("KFM_FishSizeFrequency_Summary_2007-", Export_END_Year, ".txt")
        ), locale = locale(encoding = "ISO-8859-1")) %>% 
      janitor::clean_names() %>% 
      dplyr::filter(island_code != "CL", site_number < 38) %>% 
      dplyr::mutate(
        common_name = gsub('ñ', 'n', common_name),
        common_name = gsub(", all", "", common_name),
        common_name = base::gsub(
          "California sheephead, juvenile", 
          "California sheephead, female", common_name),
        common_name = base::gsub(
          "rock wrasse, male", "rock wrasse", common_name),
        common_name = base::gsub(
          "rock wrasse, female", "rock wrasse", common_name),
        mean_size = ifelse(total_count == 0, 0, mean_size)) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, scientific_name, common_name) %>% 
      dplyr::summarise(
        mean_size = mean(mean_size, na.rm = T),
        total_count = sum(total_count, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(dplyr::select(biomass_conversions, -common_name)) %>% 
      tidyr::drop_na(a)  %>%
      dplyr::mutate(
        biomass = case_when(
          mean_size == 0 & total_count == 0 ~ 0,
          scientific_name == "Embiotoca jacksoni" ~ a * (0.799 * mean_size - 0.407) ^ b,
          scientific_name == "Girella nigricans" ~ a * (0.851 * mean_size) ^ b,
          scientific_name == "Hypsypops rubicundus" ~ a * (0.79 * mean_size + 0.42) ^ b,
          scientific_name == "Medialuna californiensis" ~ a * (0.92 * mean_size) ^ b,
          scientific_name == "Scorpaenichthys marmoratus" ~ a * mean_size ^ b * 1000,
          scientific_name == "Ophiodon elongatus" ~ a * mean_size ^ b * 1000,
          TRUE ~ a * mean_size ^ b)) %>% 
      dplyr::select(-classification)
    
  }
  
  { # Conversion   ----
    
    fish_biomass <- readr::read_csv(
      here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(
        survey_type == "rdfc",
        scientific_name %in% fish_biomass_species) %>% 
      dplyr::select(-total_count) %>%
      dplyr::left_join(site_info) %>%
      dplyr::mutate(
        common_name = base::gsub(
          "California sheephead, juvenile", "California sheephead, female", common_name),
        common_name = base::gsub(
          "rock wrasse, male", "rock wrasse", common_name),
        common_name = base::gsub(
          "rock wrasse, female", "rock wrasse", common_name),
        common_name = base::gsub(", adult", "", common_name),
        common_name = base::gsub(", subadult", "", common_name),
        common_name = base::gsub(", juvenile", "", common_name)) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name, 
        survey_year, date, scientific_name, common_name, 
        mean_depth, reserve_status, reference) %>% 
      dplyr::summarise(mean_density_sqm = sum(mean_density_sqm, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(fish_sizes) %>%  
      dplyr::mutate(
        mean_density_sqm = dplyr::case_when(
          !is.na(biomass) & mean_density_sqm == 0 ~ total_count / 2000,
          T ~ mean_density_sqm),
        mean_biomass = dplyr::case_when(
          is.na(biomass) & mean_density_sqm == 0 ~ 0,
          biomass == 0 & mean_density_sqm > 0 ~ as.double(NA),
          T ~ biomass * mean_density_sqm)) %>% 
      dplyr::relocate(c(mean_biomass, mean_density_sqm), .after = common_name)
    
  }
  
  { # Model   ----
    
    sp_model <- function(df){lm(formula(mean_biomass ~ mean_density_sqm + 0), data = df)}
    
    fish_biomass_model <- fish_biomass %>% 
      dplyr::group_by(common_name, reserve_status) %>% 
      tidyr::nest() %>% 
      dplyr::mutate(
        model = map(data, sp_model),
        tidy = map(model, broom::tidy),
        predictions = map2(data, model, modelr::add_predictions),
        residuals = map2(data, model, modelr::add_residuals)) 
    
    fish_biomass <- fish_biomass_model %>%
      tidyr::unnest(predictions) %>%
      dplyr::mutate(
        mean_biomass = dplyr::case_when(
          is.na(mean_biomass) ~ pred,
          T ~ mean_biomass)) %>% 
      dplyr::select(
        -c(data, model, tidy, residuals, pred, mean_density_sqm, date,
           mean_depth, mean_size, total_count, measurement, a, b, biomass)) %>%
      dplyr::left_join(
        species_info %>%
          dplyr::distinct(
            scientific_name, common_name, trophic_broad, targeted_broad)) %>% 
      dplyr::ungroup() %>% 
      dplyr::relocate(common_name, .after = scientific_name) %>% 
      dplyr::relocate(reserve_status, .before = reference)
    
  }

  { # Group: Total  ----
    
    fish_biomass_total <- fish_biomass %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference) %>% 
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>% 
      dplyr::mutate(
        scientific_name = "Total fish", 
        common_name = "Total fish",
        trophic_broad = 'Mixed', 
        targeted_broad = 'Mixed') %>% 
      dplyr::ungroup()
    
  }
  
  { # Group: Fishery Status  ----
    
    fish_biomass_target <- fish_biomass %>%
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference, targeted_broad) %>%
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>%
      dplyr::mutate(
        scientific_name = targeted_broad, 
        common_name = targeted_broad,
        trophic_broad = 'Mixed') %>%
      dplyr::ungroup()
    
  }
  
  { # Group: Trophic Level  ----
    
    fish_biomass_trophic <- fish_biomass %>%
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference, trophic_broad) %>%
      dplyr::summarise(mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>%
      dplyr::mutate(
        scientific_name = trophic_broad,
        common_name = trophic_broad,
       targeted_broad = 'Mixed') %>%
      dplyr::ungroup()
    
  }
  
  { # Complete  ----
    
    fish_biomass_complete <- dplyr::bind_rows(
      fish_biomass, fish_biomass_total, 
      fish_biomass_target, fish_biomass_trophic) %>%
      dplyr::left_join(
        species_info %>% 
          dplyr::filter(classification %in% c('Fish')) %>% 
          dplyr::distinct(scientific_name, classification)) %>% 
      dplyr::mutate(
        common_name = dplyr::case_when(
          common_name == 'Non-targeted' ~ 'Non-targeted fish',
          common_name == 'Targeted' ~ 'Targeted fish',
          common_name == 'Carnivore' ~ 'Carnivorous fish',
          common_name == 'Herbivore' ~ 'Herbivorous fish',
          common_name == 'Planktivore' ~ 'Planktivorous fish',
          common_name == 'Piscivore' ~ 'Piscivorous fish',
          T ~ common_name),
        scientific_name = dplyr::case_when(
          scientific_name == 'Non-targeted' ~ 'Non-targeted fish',
          scientific_name == 'Targeted' ~ 'Targeted fish',
          scientific_name == 'Carnivore' ~ 'Carnivorous fish',
          scientific_name == 'Herbivore' ~ 'Herbivorous fish',
          scientific_name == 'Planktivore' ~ 'Planktivorous fish',
          scientific_name == 'Piscivore' ~ 'Piscivorous fish',
          T ~ scientific_name))
  }
  
  { # Fish Biomass Wide   ----
    
    fish_biomass_Wide <- fish_biomass_complete %>% 
      # dplyr::filter(survey_year > 2004, reference == T) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, common_name, mean_biomass, reserve_status, reference) %>%
      tidyr::pivot_wider(
        names_from = common_name, 
        values_from = mean_biomass, values_fill = 0) %>%
      dplyr::rename_with(~ base::gsub(",", "", .)) %>%
      dplyr::rename_with(~ base::gsub(" ", "_", .))
    
  } 
  
}

{ # Total Grouped Biomass     -----
  
  { # Group: Total   ----
    
    total_biomass <- dplyr::bind_rows(benthic_biomass, fish_biomass) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, reserve_status, reference) %>% 
      dplyr::summarise(
        mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>% 
      dplyr::mutate(
        scientific_name = "Total mixed", 
        common_name = "Total mixed",
        trophic_broad = 'Mixed', 
        targeted_broad = 'Mixed',
        classification = 'Mixed') %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(survey_year > 2004) 
    
  }
  
  { # Group: Fishery Status    ----
    
    total_biomass_target <- dplyr::bind_rows(benthic_biomass, fish_biomass) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, targeted_broad, reserve_status, reference) %>% 
      dplyr::summarise(
        mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>% 
      dplyr::mutate(
        scientific_name = targeted_broad, 
        common_name = targeted_broad,
        trophic_broad = 'Mixed',
        targeted_broad = 'Mixed',
        classification = 'Mixed') %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(survey_year > 2004) 
    
  }
  
  { # Group: Trophic Level    ----
    
    total_biomass_trophic <- dplyr::bind_rows(benthic_biomass, fish_biomass) %>% 
      dplyr::filter(!trophic_broad %in% biomass_trophic_groups) %>% 
      dplyr::group_by(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, trophic_broad, reserve_status, reference) %>% 
      dplyr::summarise(
        mean_biomass = sum(mean_biomass, na.rm = TRUE)) %>% 
      dplyr::mutate(
        scientific_name = trophic_broad, 
        common_name = trophic_broad,
        trophic_broad = 'Mixed',
        targeted_broad = 'Mixed',
        classification = 'Mixed') %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(survey_year > 2004)
    
  }
  
  { # Complete    ----
    
    total_biomass_complete <- dplyr::bind_rows(
      total_biomass, total_biomass_target, total_biomass_trophic) %>% 
      dplyr::relocate(c(scientific_name, common_name), .after = survey_year) %>% 
      dplyr::relocate(mean_biomass, .after = common_name) %>% 
      dplyr::mutate(
        common_name = dplyr::case_when(
          common_name == 'Non-targeted' ~ 'Non-targeted mixed',
          common_name == 'Targeted' ~ 'Targeted mixed',
          common_name == 'Carnivore' ~ 'Carnivorous mixed',
          common_name == 'Herbivore' ~ 'Herbivorous mixed',
          common_name == 'Planktivore' ~ 'Planktivorous mixed',
          T ~ common_name),
        scientific_name = dplyr::case_when(
          scientific_name == 'Non-targeted' ~ 'Non-targeted mixed',
          scientific_name == 'Targeted' ~ 'Targeted mixed',
          scientific_name == 'Carnivore' ~ 'Carnivorous mixed',
          scientific_name == 'Herbivore' ~ 'Herbivorous mixed',
          scientific_name == 'Planktivore' ~ 'Planktivorous mixed',
          T ~ scientific_name))
    
  }

}

{ # All Biomass     ----
  
  biomass <- dplyr::bind_rows(
    benthic_biomass_complete, fish_biomass_complete, total_biomass_complete) %>% 
    dplyr::left_join(
      dplyr::select(site_info, site_name, reserve_year, latitude, longitude)) %>% 
    readr::write_csv(here::here("data", "tidy", "biomass.csv"))
  
}

  ############### COUNT ?????????
  
  ############### KEEP LONG FIRST THEN WIDEN ?????????
  
  ############### Finish sci names in xref table ?????????
  

{ # Mixed Data (% Cover, Count, Biomass)    ----
  
  { # Density     ----
    
    density <- readr::read_csv(here::here("data", "tidy", "density.csv")) %>% 
      dplyr::filter(
        # survey_year > 2004, reference == T,
        !scientific_name %in% c(
          fish_biomass_species, benthic_biomass_species)) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, common_name, mean_density_sqm, reserve_status, reference) %>% 
      tidyr::pivot_wider(
        names_from = common_name, 
        values_from = mean_density_sqm, values_fill = 0) 
  }
  
  { # Diversity    ----
      
    diversity <- readr::read_csv(here::here("data", "tidy", "diversity.csv")) %>% 
      # dplyr::filter(survey_year > 2004, reference == T) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, shannon_index, simpson_index, reserve_status, reference)
    
  }
  
  { # Biomass     ----
    
    biomass <- readr::read_csv(here::here("data", "tidy", "biomass.csv")) %>%
      # dplyr::filter(survey_year < 2004, reference == T) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, common_name, mean_biomass, reserve_status, reference) %>% 
      tidyr::pivot_wider(
        names_from = common_name, 
        values_from = mean_biomass, values_fill = 0) 
  }
  
  { # RPC     ----
    rpc <- readr::read_csv(here::here("data", "tidy", "rpc.csv")) %>% 
      dplyr::mutate(
        common_name = dplyr::case_when(
          scientific_name == "Macrocystis pyrifera" ~ "giant kelp cover",
          scientific_name == "Pterygophora californica" ~ "California sea palm cover",
          scientific_name == "Eisenia arborea" ~ "Southern sea palm cover",
          scientific_name == "Laminaria farlowii" ~ "oar weed cover",
          T ~ common_name)) %>% 
      dplyr::filter(
        # reference == TRUE, survey_year > 2004,
        !scientific_name %in% c(
          # "Macrocystis pyrifera", "Eisenia arborea",
          # "Pterygophora californica", "Laminaria farlowii",
          # "Bare Substrate",
          "Sargassum horneri", "Rock", "Cobble", "Sand")) %>%
      dplyr::select(
        site_number, island_code, island_name, site_code, site_name,
        survey_year, common_name, mean_density_sqm, reserve_status, reference) %>% 
      tidyr::pivot_wider(
        names_from = common_name, 
        values_from = mean_density_sqm, values_fill = 0)

  }
  
  { # Mixed Compiled   ---- 
    
    mixed_data <- biomass %>% 
      dplyr::left_join(density) %>% 
      dplyr::left_join(diversity) %>% 
      dplyr::left_join(rpc) %>% 
      dplyr::filter(survey_year > 2004, reference == T) %>%
      dplyr::rename_with(~ base::gsub(",", "", .)) %>%
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      dplyr::rename_with(~ base::gsub("-", "_", .)) %>%
      dplyr::rename_with(~ base::gsub("'", "", .)) %>%
      dplyr::rename_with(~ base::gsub("\\.", "", .)) %>%
      dplyr::rename_with(~ base::gsub("/", "_", .)) %>%
      dplyr::rename_with(~ str_replace(., "_\\([^\\)]+\\)", "")) %>% 
      readr::write_csv(here::here("data", "tidy", "mixed_data.csv"))
    
  }
  
}

{ # Random Forest   ----
  
  { # Data   ----
    
    mixed_data <- readr::read_csv(
      here::here("data", "tidy", "mixed_data.csv")) %>%
      dplyr::mutate(survey_year = factor(survey_year),
                    island_code = factor(island_code, levels = island_code_levels),
                    reserve_status = factor(reserve_status)) %>%
      dplyr::select(-site_number, -site_name, -island_name, -site_code)
    
  }
  
  { # Model   ----
    
    set.seed(42)
    
    rf_model <- randomForest::randomForest(
      data = mixed_data,
      reserve_status  ~ ., ntree = 3000, mtry = 8,
      importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    
  }
  
  { # Variable Importance   -----
    
    rf_importance <- randomForest::varImpPlot(rf_model) %>% 
      base::as.data.frame() %>%
      janitor::clean_names() %>% 
      tibble::rownames_to_column("variable") %>%
      dplyr::left_join(mixed_data_xref_biomass) %>% 
      dplyr::select(variable, common_name, scientific_name, 
                    mean_decrease_accuracy, mean_decrease_gini,             
                    data_type, classification, targeted) %>% 
      dplyr::arrange(desc(mean_decrease_accuracy)) %>%  
      dplyr::mutate(
        common_name = paste(common_name, " (", data_type, ")", sep = ""),
        common_name = gsub("Biomass", "B", common_name),
        common_name = gsub("Count", "C", common_name),
        common_name = gsub("Percent Cover", "P", common_name),
        common_name = gsub("Index Value", "I", common_name),
        common_name = gsub("Categorical", "Cg", common_name)) 
    
    # rf_na <- rf_importance %>% 
    #   filter(common_name == "NA (NA)") %>% 
    #   dplyr::select(-mean_decrease_accuracy, -mean_decrease_gini)
    # 
    # readr::write_csv(rf_na, here::here("data", "meta", "rf_na.csv"))
  }
  
}

{ # Partial Dependence       ----
  top_30_mda <- rf_importance %>%
    dplyr::arrange(desc(mean_decrease_accuracy)) %>%  
    head(30) %>% 
    droplevels()
  
  pdp <- lapply(top_30_mda$variable[1:30], function(i){
    partial(rf_model, pred.var = i) %>%
      data.frame() %>%
      dplyr::mutate(variable = i) %>%
      dplyr::rename(x_var = i)
  })
  
  partial_df <- bind_rows(pdp) %>%
    dplyr::left_join(mixed_data_xref_biomass) %>% 
    dplyr::mutate(common_name = paste(common_name, " (", data_type, ")", sep = ""),
                  common_name = gsub("Biomass", "B", common_name),
                  common_name = gsub("Count", "C", common_name),
                  common_name = gsub("Percent Cover", "P", common_name),
                  common_name = gsub("Index Value", "I", common_name),
                  common_name = gsub("Categorical", "Cg", common_name),
                  common_name = fct_inorder(common_name)) %>%
    dplyr::mutate(Rank = as.integer(common_name))
}
  












{ # RATIOS - BEWARE... THESE TAKE A LOOOOONG TIME  -----------------
  
  { # Ratio Functions   -----
    
    biomass_boot_ratio <- function (data, indices) {
      sample = data[indices, ]
      ratio = mean(sample$mean_biomass[sample$reserve_status == "Inside"])/
        mean(sample$mean_biomass[sample$reserve_status == "Outside"])
      return(ratio) 
    }
    
    density_boot_ratio <- function (data, indices) {
      sample = data[indices, ]
      ratio = mean(sample$mean_density_sqm[sample$reserve_status == "Inside"])/
        mean(sample$mean_density_sqm[sample$reserve_status == "Outside"])
      return(ratio) 
    }
    
    diversity_boot_ratio <- function (data, indices) {
      sample = data[indices, ]
      ratio = mean(sample$Value[sample$reserve_status == "Inside"])/
        mean(sample$Value[sample$reserve_status == "Outside"])
      return(ratio) 
    }
    
  }
  
  { # Diversity Ratios   ----
    
    ######### Insignificant... Don't Include
    
    # Diversity <- arrow::read_feather("Tidy_Data/Diversity_2005.feather")
    # 
    # Diversity_Ratios <- tibble(
    #   common_name = character(), survey_year = integer(), classification = character(),
    #   Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
    # 
    # for (yr in unique(Diversity$survey_year)){
    #   for (sp in unique(Diversity$Index)){
    #     
    #     DF <- Diversity %>% 
    #       dplyr::filter(survey_year == yr, Index == sp) 
    #     
    #     output <- boot::boot(data = DF, statistic = diversity_boot_ratio, R = 1000)
    #     
    #     ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
    #     
    #     Diversity_Ratios <- Diversity_Ratios %>% 
    #       tibble::add_row(
    #         common_name = sp, survey_year = yr, classification = "Index",
    #         Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
    #   }
    # }
    
    
  }
  
  { # Biomass Ratios   ----
    
    { # Data  ----
      Biomass_Data <- arrow::read_feather("Tidy_Data/Biomass.feather") %>%
        dplyr::filter(
          reference == TRUE, survey_year > 2004,
          !scientific_name %in% c(
            "total benthic biomass", "total fish biomass", "total biomass", "Targeted", "Non-targeted",
            "Detritivore", "Herbivore", "Planktivore", "Producer", "Carnivore", "Piscivore"),
          scientific_name != 'Muricea californica' | survey_year > 1990,
          scientific_name != 'Lithopoma gibberosa' | survey_year > 2002) %>% 
        dplyr::group_by(site_number, common_name, survey_year) %>% 
        dplyr::mutate(mean_biomass = mean_biomass + runif(1, min = .9, max = 1.1),
                      # mean_biomass = ifelse(classification == "Fish", mean_biomass * 2000, mean_biomass),
                      common_name = factor(common_name),
                      targeted_broad = factor(targeted_broad),
                      trophic_broad = factor(trophic_broad)) %>% 
        dplyr::ungroup()
    }
    
    { # species Level  ----
      Biomass_Species_Ratios <- tibble(
        common_name = character(), survey_year = integer(), classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$classification)) {
        class_filtered <- Biomass_Data %>% 
          dplyr::filter(classification == class)
        for (yr in unique(class_filtered$survey_year)){
          dropped_levels <- class_filtered %>% 
            dplyr::filter(survey_year == yr) %>% 
            droplevels()
          for (sp in levels(dropped_levels$common_name)){
            d <- dropped_levels %>%
              filter(common_name == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Species_Ratios <- Biomass_Species_Ratios %>% 
              tibble::add_row(
                common_name = sp, survey_year = yr, classification = class,
                Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Total Class Level  ----
      Biomass_Total_Ratios <- tibble(
        common_name = character(), survey_year = integer(), classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$classification)) {
        class_filtered <- Biomass_Data %>% 
          dplyr::filter(classification == class)
        for(yr in unique(class_filtered$survey_year)){
          dropped_levels <- class_filtered %>% 
            dplyr::filter(survey_year == yr) %>% 
            droplevels() 
          output <- boot::boot(data = dropped_levels, statistic = biomass_boot_ratio, R = 5000)
          ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
          Biomass_Total_Ratios <- Biomass_Total_Ratios %>% 
            tibble::add_row(
              common_name = paste("Total ", class, " Biomass", sep = ""), survey_year = yr, classification = class, 
              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          
        }
      }
    }
    
    { # All Total Level  ----
      Biomass_All_Total_Ratios <- tibble(
        common_name = character(), survey_year = integer(), classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for(yr in unique(Biomass_Data$survey_year)){
        dropped_levels <- Biomass_Data %>% 
          dplyr::filter(survey_year == yr) %>% 
          droplevels() 
        output <- boot::boot(data = dropped_levels, statistic = biomass_boot_ratio, R = 5000)
        ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
        Biomass_All_Total_Ratios <- Biomass_All_Total_Ratios %>% 
          tibble::add_row(common_name = "total biomass", survey_year = yr, classification = "Mixed", 
                          Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
        
        
      }
    }
    
    { # Targeted Level  ----
      Biomass_Target_Ratios <- tibble(
        common_name = character(), survey_year = integer(), classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$classification)) {
        class_filtered <- Biomass_Data %>% 
          dplyr::filter(classification == class)
        for(yr in unique(class_filtered$survey_year)){
          dropped_levels <- class_filtered %>% 
            dplyr::filter(survey_year == yr) %>% 
            droplevels()
          for(sp in levels(dropped_levels$targeted_broad)){
            d <- dropped_levels %>%
              filter(targeted_broad == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 5000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Target_Ratios <- Biomass_Target_Ratios %>% 
              tibble::add_row(common_name = sp, survey_year = yr, classification = class, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Trophic level ----
      Biomass_Trophic_Ratios <- tibble(
        common_name = character(), survey_year = integer(), classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$classification)) {
        class_filtered <- Biomass_Data %>% 
          dplyr::filter(classification == class)
        for(yr in unique(class_filtered$survey_year)){
          dropped_levels <- class_filtered %>% 
            dplyr::filter(survey_year == yr) %>% 
            droplevels()
          for(sp in levels(dropped_levels$trophic_broad)){
            d <- dropped_levels %>%
              filter(trophic_broad == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 5000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Trophic_Ratios <- Biomass_Trophic_Ratios %>% 
              tibble::add_row(common_name = sp, survey_year = yr, classification = class, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Data Output  ----
      
      Biomass_Ratios <- base::rbind(
        Biomass_Species_Ratios, 
        Biomass_Total_Ratios,
        Biomass_All_Total_Ratios,
        Biomass_Target_Ratios,
        Biomass_Trophic_Ratios) %>% 
        dplyr::mutate(
          date = lubridate::mdy(glue::glue('7-1-{survey_year}')),
          survey_type = 'Mixed',
          Metric = 'biomass_ratio') %>%
        dplyr::left_join(
          species_info %>% 
            dplyr::distinct(
              scientific_name, common_name, classification, trophic_broad, 
              targeted_broad, recreational_fishery, commercial_fishery)) %>% 
        dplyr::filter(common_name != "Targeted" | classification != "Algae",
                      common_name != "Producer" | classification != "Algae",
                      common_name != "Total Algae Biomass" | classification != "Algae")
    }
    
  }
  
  { # Density Ratios   ----
    
    { # Benthic Density Ratios    -----
      
      { # Data    ----
        Density_Boot <- arrow::read_feather("Tidy_Data/Density.feather") %>%
          dplyr::filter(
            reference == TRUE, survey_year > 2004, classification != "Fish",
            scientific_name != 'Muricea californica' | survey_year > 1990,
            scientific_name != 'Cypraea spadicea' | survey_year > 1983,
            scientific_name != 'Undaria pinnatifida',
            scientific_name != 'Haliotis assimilis',
            scientific_name != 'Haliotis sorenseni',
            scientific_name != 'Cryptochiton stelleri',
            scientific_name != 'Pisaster ochraceus') %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::distinct(scientific_name, trophic_broad, targeted_broad, 
                              recreational_fishery, commercial_fishery)) %>%
          dplyr::group_by(site_number, common_name, survey_year) %>% 
          dplyr::mutate(mean_density_sqm = mean_density_sqm + runif(1, min = .9, max = 1.1),
                        common_name = factor(common_name),
                        targeted_broad = factor(targeted_broad),
                        trophic_broad = factor(trophic_broad)) %>% 
          dplyr::ungroup()
      }
      
      { # species Level  ----
        Density_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(Density_Boot$survey_year)){
          dropped <- Density_Boot %>% 
            dplyr::filter(survey_year == y) %>% 
            droplevels()
          for(s in levels(dropped$common_name)){
            d <- dropped %>%
              filter(common_name == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Species_Ratio <- Density_Species_Ratio %>% 
              tibble::add_row(common_name = s, survey_year = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        Density_Target_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(Density_Boot$survey_year)){
          drop <- Density_Boot %>% 
            dplyr::filter(survey_year == yr,
                          targeted_broad != 'Mixed') %>% 
            droplevels()
          for(c in levels(drop$targeted_broad)){
            d <- drop %>%
              filter(targeted_broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Target_Species_Ratio <- Density_Target_Species_Ratio %>% 
              tibble::add_row(common_name = c, survey_year = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        Density_Trophic_Level_Ratios <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(Density_Boot$survey_year)){
          dropT <- Density_Boot %>% 
            dplyr::filter(survey_year == year,
                          trophic_broad != 'Mixed Trophic Levels') %>% 
            droplevels()
          for(t in levels(dropT$trophic_broad)){
            d <- dropT %>%
              filter(trophic_broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Trophic_Level_Ratios <- Density_Trophic_Level_Ratios %>% 
              tibble::add_row(common_name = t, survey_year = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        Density_Ratios <- base::rbind(
          Density_Species_Ratio, 
          Density_Target_Species_Ratio, 
          Density_Trophic_Level_Ratios) %>%
          dplyr::mutate(date = lubridate::mdy(glue::glue('7-1-{survey_year}')),
                        survey_type = 'Mixed',
                        Metric = 'density_ratio') %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::filter(
                classification != "Fish" |
                  common_name %in% c('island kelpfish', 'blackeye goby', 'blue-banded goby')) %>%
              dplyr::distinct(
                scientific_name, common_name, classification, trophic_broad, 
                targeted_broad, recreational_fishery, commercial_fishery))
        
      }
      
    }
    
    { # RDFC Density Ratios    -----
      
      { # Data    ----
        RDFC_Density_Boot <- arrow::read_feather("Tidy_Data/Density.feather") %>%
          dplyr::filter(
            reference == TRUE, survey_year > 2004, classification == "Fish",
            survey_type == "RDFC",
            !scientific_name %in% 
              c('Coryphopterus nicholsi', 
                'Lythrypnus dalli', 
                'Alloclinus holderi')) %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::distinct(scientific_name, trophic_broad, targeted_broad, 
                              recreational_fishery, commercial_fishery)) %>%
          dplyr::group_by(site_number, common_name, survey_year) %>% 
          dplyr::mutate(
            mean_density_sqm = mean_density_sqm + runif(1, min = .9, max = 1.1),
            common_name = factor(common_name),
            targeted_broad = factor(targeted_broad),
            trophic_broad = factor(trophic_broad)) %>% 
          dplyr::ungroup()
      }
      
      { # species Level  ----
        RDFC_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(RDFC_Density_Boot$survey_year)){
          dropped <- RDFC_Density_Boot %>% 
            dplyr::filter(survey_year == y) %>% 
            droplevels()
          for(s in levels(dropped$common_name)){
            d <- dropped %>%
              filter(common_name == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Species_Ratio <- RDFC_Species_Ratio %>% 
              tibble::add_row(common_name = s, survey_year = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        RDFC_Target_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(RDFC_Density_Boot$survey_year)){
          drop <- RDFC_Density_Boot %>% 
            dplyr::filter(survey_year == yr,
                          targeted_broad != 'Mixed') %>% 
            droplevels()
          for(c in levels(drop$targeted_broad)){
            d <- drop %>%
              filter(targeted_broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Target_Species_Ratio <- RDFC_Target_Species_Ratio %>% 
              tibble::add_row(common_name = c, survey_year = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        RDFC_Trophic_Level_Ratios <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(RDFC_Density_Boot$survey_year)){
          dropT <- RDFC_Density_Boot %>% 
            dplyr::filter(survey_year == year,
                          trophic_broad != 'Mixed Trophic Levels') %>% 
            droplevels()
          for(t in levels(dropT$trophic_broad)){
            d <- dropT %>%
              filter(trophic_broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Trophic_Level_Ratios <- RDFC_Trophic_Level_Ratios %>% 
              tibble::add_row(common_name = t, survey_year = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        RDFC_Density_Ratios <- base::rbind(
          RDFC_Species_Ratio,
          RDFC_Target_Species_Ratio, 
          RDFC_Trophic_Level_Ratios) %>%
          dplyr::mutate(
            date = lubridate::mdy(glue::glue('7-1-{survey_year}')),
            survey_type = 'RDFC',
            Metric = 'density_ratio') %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::filter(classification == 'Fish') %>% 
              dplyr::distinct(
                scientific_name, common_name, classification, trophic_broad, 
                targeted_broad, recreational_fishery, commercial_fishery))
        
      }
      
    }
    
    { # VFT Density Ratios    -----
      
      { # Data    ----
        
        VFT_Density_Boot <- arrow::read_feather("Tidy_Data/Density.feather") %>%
          dplyr::filter(
            reference == TRUE, survey_year > 2004, classification == "Fish",
            survey_type == "VFT") %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::distinct(scientific_name, trophic_broad, targeted_broad, 
                              recreational_fishery, commercial_fishery)) %>%
          dplyr::group_by(site_number, common_name, survey_year) %>% 
          dplyr::mutate(mean_density_sqm = mean_density_sqm + runif(1, min = .9, max = 1.1),
                        common_name = factor(common_name),
                        targeted_broad = factor(targeted_broad),
                        trophic_broad = factor(trophic_broad)) %>% 
          dplyr::ungroup()
      }
      
      { # species Level  ----
        VFT_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(VFT_Density_Boot$survey_year)){
          dropped <- VFT_Density_Boot %>% 
            dplyr::filter(survey_year == y) %>% 
            droplevels()
          for(s in levels(dropped$common_name)){
            d <- dropped %>%
              filter(common_name == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Species_Ratio <- VFT_Species_Ratio %>% 
              tibble::add_row(common_name = s, survey_year = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        VFT_Target_Species_Ratio <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(VFT_Density_Boot$survey_year)){
          drop <- VFT_Density_Boot %>% 
            dplyr::filter(survey_year == yr,
                          targeted_broad != 'Mixed') %>% 
            droplevels()
          for(c in levels(drop$targeted_broad)){
            d <- drop %>%
              filter(targeted_broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Target_Species_Ratio <- VFT_Target_Species_Ratio %>% 
              tibble::add_row(common_name = c, survey_year = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        VFT_Trophic_Level_Ratios <- tibble(
          common_name = character(), survey_year = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(VFT_Density_Boot$survey_year)){
          dropT <- VFT_Density_Boot %>% 
            dplyr::filter(survey_year == year,
                          trophic_broad != 'Mixed Trophic Levels') %>% 
            droplevels()
          for(t in levels(dropT$trophic_broad)){
            d <- dropT %>%
              filter(trophic_broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Trophic_Level_Ratios <- VFT_Trophic_Level_Ratios %>% 
              tibble::add_row(common_name = t, survey_year = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        VFT_Density_Ratios <- base::rbind(
          VFT_Species_Ratio,
          VFT_Target_Species_Ratio, 
          VFT_Trophic_Level_Ratios) %>%
          dplyr::mutate(
            date = lubridate::mdy(glue::glue('7-1-{survey_year}')),
            survey_type = 'VFT',
            Metric = 'density_ratio') %>%
          dplyr::left_join(
            species_info %>% 
              dplyr::filter(classification == 'Fish') %>% 
              dplyr::distinct(
                scientific_name, common_name, classification, trophic_broad, 
                targeted_broad, recreational_fishery, commercial_fishery))
        
      }
      
    }
    
  }
  
  { # All Ratios   -----
    All_Ratios <- 
      base::rbind(
        Biomass_Ratios,  
        Density_Ratios, 
        RDFC_Density_Ratios, 
        VFT_Density_Ratios) %>% 
      arrow::write_feather("Tidy_Data/Ratios.feather")
  }
  
}






