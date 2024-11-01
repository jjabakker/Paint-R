

library(readxl)


read_data <<- function(root_directory) {
  
  n_directory      <<- 'New Experiments/Output/'
  t_directory      <<- 'Thesis/Output/'
  
  squares_file     <<- 'All Squares.xlsx'
  batches_file     <<- 'All Batches.xlsx'
  
  squares_master_n <- read_excel(paste0(root_directory, n_directory, squares_file))
  batches_master_n <- read_excel(paste0(root_directory, n_directory, batches_file))
  
  squares_master_t <- read_excel(paste0(root_directory, t_directory, squares_file))
  batches_master_t <- read_excel(paste0(root_directory, t_directory, batches_file))
  
  squares_master   <<- rbind(squares_master_t, squares_master_n)
  batches_master   <<- rbind(batches_master_t, batches_master_n)
  
  rm(squares_master_n)
  rm(squares_master_t)
  rm(batches_master_n)
  rm(batches_master_t)
  
  # Remove spaces from column names
  names(squares_master) <<- str_replace_all(names(squares_master), c(" " = "_" ))
  names(batches_master) <<- str_replace_all(names(batches_master), c(" " = "_" ))
  
  # Set Structure to Control when Probe is Control
  squares_master$Structure[squares_master$Probe == 'Control'] <<- 'Control'
  squares_master$Valency[squares_master$Probe == 'Control']   <<- 'Control'
  
  # Shorten Probe Type names
  squares_master$Probe_Type[squares_master$Probe_Type == 'Epitope'] <<- 'E'
  squares_master$Probe_Type[squares_master$Probe_Type == 'Simple']  <<- 'S'
  
  batches_master$Probe_Type[batches_master$Probe_Type == 'Epitope'] <<- 'E'
  batches_master$Probe_Type[batches_master$Probe_Type == 'Simple']  <<- 'S'
  
  # Shorten Cell_Type names
  squares_master$Cell_Type[squares_master$Cell_Type == 'CHO-MR']  <<- 'CHOMR'
  squares_master$Cell_Type[squares_master$Cell_Type == 'MR -/-']  <<- 'MR-'
  
  batches_master$Cell_Type[batches_master$Cell_Type == 'CHO-MR']  <<- 'CHOMR'
  batches_master$Cell_Type[batches_master$Cell_Type == 'MR -/-']  <<- 'MR-'
  
  # Remove the threshold part of the image name and store in column Recording_Name
  squares_master <<- squares_master %>% separate_wider_regex(Recording_Name, c(Recording_Name = ".*", "-threshold-\\d+"))
  
  # Change Adjuvant name from No to None 
  squares_master$Adjuvant[squares_master$Adjuvant == 'No']       <<- 'None'
  
  batches_master$Adjuvant[batches_master$Adjuvant == 'No']       <<- 'None'
  
  # Correct concentrations (irrelevant accuracy)
  squares_master$Concentration[squares_master$Concentration == 4.9]     <<- 5
  squares_master$Concentration[squares_master$Concentration == 14.6]    <<- 15
  
  batches_master$Concentration[batches_master$Concentration == 4.9]     <<- 5
  batches_master$Concentration[batches_master$Concentration == 14.6]    <<- 15
  
  # Remove suspect Concentration  
  squares_master <<- squares_master %>%
    filter(squares_master$Concentration != 0.1)
  
  batches_master <<- batches_master %>%
    filter(batches_master$Concentration != 0.1)
  
  # Make Concentration integer
  # squares_master$Concentration <<- as.numeric(squares_master$Concentration)
  
  # Drop unusual thresholds 
  # squares_master <<- squares_master[squares_master$Threshold != '7',] 
  # squares_master <<- squares_master[squares_master$Threshold != '8',] 
  
  # Merge Adjuvants
  squares_master$Adjuvant[squares_master$Adjuvant == 'MPLA']     <<- 'Adj'
  squares_master$Adjuvant[squares_master$Adjuvant == 'LPS']      <<- 'Adj'
  squares_master$Adjuvant[squares_master$Adjuvant == 'LPS+CytD'] <<- 'Adj+CytD'
  
  batches_master$Adjuvant[batches_master$Adjuvant == 'MPLA']     <<- 'Adj'
  batches_master$Adjuvant[batches_master$Adjuvant == 'LPS']      <<- 'Adj'
  batches_master$Adjuvant[batches_master$Adjuvant == 'LPS+CytD'] <<- 'Adj+CytD'
  
  # Make columns factor where necessary
  squares_master$Probe      <<- factor(squares_master$Probe,       levels = c('1 Mono', '2 Mono', '6 Mono', '1 Bi', '2 Bi', '6 Bi', '1 Tri', '2 Tri', '6 Tri', 'Control'))
  squares_master$Probe_Type <<- factor(squares_master$Probe_Type,  levels = c('S', 'E'))
  squares_master$Cell_Type  <<- factor(squares_master$Cell_Type,   levels = c('CHOMR', 'BMDC', "MR-", "iCD103", "spDC" ))
  squares_master$Adjuvant   <<- factor(squares_master$Adjuvant,    levels = c('None', 'CytD', 'Adj', 'Adj+CytD'))
  squares_master$Valency    <<- factor(squares_master$Valency,     levels = c('1', '2', '6', 'Control'))
  squares_master$Structure  <<- factor(squares_master$Structure,   levels = c('Mono', 'Bi', 'Tri', 'Control'))
  
  # Make columns factor where necessary
  batches_master$Probe      <<- factor(batches_master$Probe,       levels = c('1 Mono', '2 Mono', '6 Mono', '1 Bi', '2 Bi', '6 Bi', '1 Tri', '2 Tri', '6 Tri', 'Control'))
  batches_master$Probe_Type <<- factor(batches_master$Probe_Type,  levels = c('S', 'E'))
  batches_master$Cell_Type  <<- factor(batches_master$Cell_Type,   levels = c('CHOMR', 'BMDC', "MR-", "iCD103", "spDC" ))
  batches_master$Adjuvant   <<- factor(batches_master$Adjuvant,    levels = c('None', 'CytD', 'Adj', 'Adj+CytD'))
  
  # Only consider images that have the correct length
  squares_master <<- squares_master %>%
    filter(Recording_Size >= 1058000896 * 0.95 | Recording_Size <= 1058000896 * 1.20)
  
  # Only consider squares that do have a Valid Tau and are Visible
  squares_master <<- squares_master %>%
    filter(Valid_Tau == TRUE) %>%
    filter(Visible == TRUE)  
  
  # Remove columns that will never be used
  squares_master <<- squares_master %>%
    select(-c(X0, X1, Y0, Y1, Visible, Valid_Tau, Recording_Size))
  
  
  # Check
  # if (sum(is.na(squares_master)) > 0 ) {
  #   which(is.na(squares_master), arr.ind=TRUE)
  #   stop('Na in squares master')
  # }
  # 
  # if (sum(is.na(batches_master)) > 0 ) {
  #   which(is.na(batches_master), arr.ind=TRUE)
  #   stop('Na in batches_master')
  #   
  # }
  
  write_csv(squares_master, '~/Downloads/squares_master.csv')
  write_csv(batches_master, '~/Downloads/batches_master.csv')
  
}

