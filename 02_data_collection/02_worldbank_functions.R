get_country_climate_data <- function(country_iso, start_year, end_year) {
  # Working indicators only (based on your test results)
  climate_indicators <- c(
    "EN.CLC.HEAT.XD",        # Heat index exposure ✓ Working
    "AG.LND.PRCP.MM",        # Average precipitation ✓ Working  
    "SP.URB.TOTL.IN.ZS",     # Urban population ✓ Working
    "EG.ELC.ACCS.ZS",        # Access to electricity ✓ Working
    "ER.LND.PTLD.ZS",        # Terrestrial protected areas ✓ Working
    # Added alternative CO2/emissions indicators
    "EN.ATM.CO2E.KT",        # CO2 emissions (kt) - alternative to per capita
    "EN.ATM.METH.KT.CE",     # Methane emissions (kt of CO2 equivalent)
    "EN.ATM.NOXE.KT.CE"      # Nitrous oxide emissions (kt of CO2 equivalent)
  )
  
  tryCatch({
    wb_data(
      indicator = climate_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    message(paste("Error in World Bank batch retrieval, trying individually..."))
    
    # Try indicators one by one
    working_data <- data.frame()
    
    for (ind in climate_indicators) {
      ind_data <- tryCatch({
        cat("Trying indicator:", ind, "\n")
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        cat("  Failed:", ind, "\n")
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        cat("  Success:", ind, "- got", nrow(ind_data), "rows\n")
        working_data <- bind_rows(working_data, ind_data)
      }
    }
    
    return(working_data)
  })
}

get_economic_and_gender_data <- function(country_iso, start_year, end_year) {
  # Core economic indicators (verified to work)
  economic_indicators <- c(
    "NY.GDP.MKTP.CD",         # GDP (current US$)
    "NY.GDP.PCAP.CD",         # GDP per capita (current US$)
    "NY.GDP.PCAP.PP.CD",      # GDP per capita, PPP
    "NV.IND.MANF.ZS",         # Manufacturing, value added (% of GDP)
    "NV.SRV.TOTL.ZS",         # Services, value added (% of GDP)
    "NV.AGR.TOTL.ZS"          # Agriculture, value added (% of GDP)
  )
  
  # Simplified gender indicators (most reliable ones)
  gender_employment <- c(
    "SL.TLF.CACT.FE.ZS",      # Labor force participation rate, female
    "SL.TLF.CACT.FM.ZS",      # Ratio of female to male labor force participation
    "SL.UEM.TOTL.FE.ZS"       # Unemployment, female
  )
  
  gender_education <- c(
    "SE.PRM.CMPT.FE.ZS",      # Primary completion rate, female
    "SE.ADT.LITR.FE.ZS",      # Literacy rate, adult female
    "SE.ENR.TERT.FM.ZS"       # Ratio of female to male tertiary enrollment
  )
  
  gender_health <- c(
    "SP.DYN.LE00.FE.IN",      # Life expectancy at birth, female
    "SH.STA.MMRT",            # Maternal mortality ratio
    "SP.ADO.TFRT"             # Adolescent fertility rate
  )
  
  gender_rights <- c(
    "SG.GEN.PARL.ZS"          # Proportion of seats held by women in parliament
    # Removed problematic indicators: SG.DMK.SRCR.FN, etc.
  )
  
  # Combine indicators
  all_indicators <- c(economic_indicators, gender_employment, gender_education, gender_health, gender_rights)
  
  # Try to get data with individual fallback
  combined_data <- tryCatch({
    wb_data(
      indicator = all_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    cat("Batch retrieval failed, trying indicators individually...\n")
    
    all_data <- data.frame()
    
    for (ind in all_indicators) {
      ind_data <- tryCatch({
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        all_data <- bind_rows(all_data, ind_data)
      }
    }
    
    return(all_data)
  })
  
  # Add categories
  if (!is.null(combined_data) && nrow(combined_data) > 0) {
    id_col <- intersect(c("indicator_id", "indicatorID", "id"), colnames(combined_data))[1]
    
    if (!is.na(id_col)) {
      combined_data <- combined_data %>%
        mutate(indicator_category = case_when(
          .data[[id_col]] %in% economic_indicators ~ "Economic",
          .data[[id_col]] %in% gender_employment ~ "Employment",
          .data[[id_col]] %in% gender_education ~ "Education",
          .data[[id_col]] %in% gender_health ~ "Health",
          .data[[id_col]] %in% gender_rights ~ "Rights",
          TRUE ~ "Other"
        ))
    }
  }
  
  return(combined_data)
}

get_global_social_vulnerability <- function(country_iso, start_year, end_year) {
  social_indicators <- c(
    "SI.POV.GINI",
    "SP.POP.65UP.TO.ZS",
    "SP.URB.TOTL.IN.ZS",
    "SP.POP.TOTL",
    "SP.DYN.LE00.IN",
    "SH.STA.BASS.ZS",
    "SH.H2O.BASW.ZS",
    "SE.ADT.LITR.ZS"
  )
  
  social_data <- tryCatch({
    wb_data(
      indicator = social_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    print(paste("Error in social data retrieval:", e$message))
    
    all_data <- data.frame()
    for (ind in social_indicators) {
      ind_data <- tryCatch({
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        all_data <- bind_rows(all_data, ind_data)
      }
    }
    
    if (nrow(all_data) > 0) {
      return(all_data)
    } else {
      return(NULL)
    }
  })
  
  return(social_data)
}

# Export functions for use in other scripts
cat("✅ World Bank functions loaded\n")