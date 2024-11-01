

create_tau_significance_table <- function(data_to_analyse) { 

  probe_types        <- unique(data_to_analyse$Probe_Type)
  significance_table <- data.frame()
  

  for (probe_type in probe_types) {
    results            <- calculate_significance_record(data_to_analyse, 'Tau')
    significance_table <- rbind(significance_table, results)
  }
  return (significance_table)
}


create_density_significance_table <- function(data_to_analyse) { 
  
  probe_types        <- unique(data_to_analyse$Probe_Type)
  significance_table <- data.frame()

  for (probe_type in probe_types) {
    results            <- calculate_significance_record(data_to_analyse, 'Density')
    significance_table <- rbind(significance_table, results)
  }
  return (significance_table)
}


calculate_significance_record <- function(data_to_analyse, parameter) { 
  
  if (parameter == 'Tau') { 
    aov2_result <- aov(formula = Tau ~ Probe, data = data_to_analyse)
  } else if (parameter == 'Density'){
    aov2_result <- aov(formula = Density ~ Probe, data = data_to_analyse)  
  }
  else {
     abort("Valid options are either 'Tau' or 'Density'.")
  }
  
  tukey_results <- TukeyHSD(aov2_result, which='Probe')
  tukey_results <- tukey_results$Probe
  results       <- as.data.frame(tukey_results)

  # Get the probes separated
  results$Probes <- rownames(results)
  results[c('Probe1', 'Probe2')] <- str_split_fixed(results$Probes, '-', 2)
  results <- results %>% select(-Probes)
  
  # Determine significance
  results$Significance <- ' '
  results$Significance[results$'p adj' <= 0.05]   <-  '*'
  results$Significance[results$'p adj' <= 0.01]   <-  '**'
  results$Significance[results$'p adj' <= 0.001]  <-  '***'
  results$Significance[results$'p adj' <= 0.0001] <-  '****'
  
  results$p_adj  <- format(results$'p adj', format='f', digits=3)
  results        <- results[c( 'Probe1', 'Probe2', 'p_adj', 'Significance')]
  
  return (results)
}


get_significance <- function(significance_table, probe1, probe2) {
  
  if (sum(significance_table$Probe1 == probe1 & 
          significance_table$Probe2 == probe2) == 1) {
    p <- significance_table[significance_table$Probe1 == probe1 & 
                            significance_table$Probe2 == probe2, 'p_adj']
  } else if (sum(significance_table$Probe1 == probe2 & 
                 significance_table$Probe2 == probe1) == 1) {
    p <- significance_table[significance_table$Probe1 == probe2 & 
                            significance_table$Probe2 == probe1, 'p_adj']
  } else {
    
    # Not been able to retrieve an unique combibation
    
    sum1 <- sum(significance_table$Probe1 == probe1 & 
                significance_table$Probe2 == probe2)
    
    sum2 <- sum(significance_table$Probe1 == probe2 & 
                significance_table$Probe2 == probe1)
    
    if (sum1 + sum2 == 0) {
      # There is no entry for the combination (can happen)
      p <- -1
    }
    else {
      # There are multiple entries (which would be a logical error)
      p <- -2
    }
  }
  return (p)
}

