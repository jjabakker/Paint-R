library(readxl)
source('../Utility Code/Eliminate.R')


read_data <- function(root_directory, output, manually_exclude = FALSE, duration_exclude = FALSE, exclude_file='Excluded Images.csv') {
  
  
  squares_file        <- 'All Squares.csv'
  images_file         <- 'All Images.csv'
  valid_squares_file  <- 'Valid Squares.csv'
  valid_images_file   <- 'Valid Images.csv'
  tracks_file         <- 'All Tracks.csv'

  squares_master <- read_csv(paste0(root_directory, '/', output, '/', squares_file), show_col_types = FALSE)
  images_master  <- read_csv(paste0(root_directory, '/', output, '/', images_file), show_col_types = FALSE)
  tracks_master <- read_csv(paste0(root_directory, '/', output, '/', tracks_file), show_col_types = FALSE)
  
  # Remove spaces from column names
  names(squares_master) <- str_replace_all(names(squares_master), c(" " = "_" ))
  names(images_master)  <- str_replace_all(names(images_master), c(" " = "_" ))
  names(tracks_master)  <- str_replace_all(names(images_master), c(" " = "_" ))
  
  # Eliminate excluded images
  data <- eliminate_excluded(root_directory, squares_master, images_master, manually_exclude, duration_exclude, exclude_file)
  squares_master <- data$squares_master
  images_master  <- data$images_master
  
  write_csv(squares_master, paste0(root_directory, '/', output, '/', valid_squares_file))
  write_csv(images_master, paste0(root_directory, '/', output, '/', valid_images_file))
  
  # Check if Valency and Structure exist and create if needed
  if (!('Valency' %in% names(squares_master))) {
    
    # Suppressing both messages and warnings within the code block
    squares_master <- suppressMessages(suppressWarnings(
      squares_master %>%
        separate(Probe, into = c('Valency', 'Structure'), sep = ' ', remove = FALSE) %>%
        mutate(Structure = replace_na(Structure, 'Not specified')) %>%
        mutate(Valency = replace_na(Valency, 0))
    ))
  }
  
  # Set Structure to Control when Probe is Control
  squares_master$Structure[squares_master$Probe == 'Control'] <- 'Control'
  squares_master$Valency[squares_master$Probe == 'Control']   <- 'Control'

  # Shorten Cell_Type names
  squares_master$Cell_Type[squares_master$Cell_Type == 'CHO-MR']  <- 'CHOMR'
  squares_master$Cell_Type[squares_master$Cell_Type == 'MR -/-']  <- 'MR-'
  
  images_master$Cell_Type[images_master$Cell_Type == 'CHO-MR']  <- 'CHOMR'
  images_master$Cell_Type[images_master$Cell_Type == 'MR -/-']  <- 'MR-'
  
  # Remove the threshold part of the image name and store in column Recording_Name
  squares_master <- squares_master %>%
    mutate(Ori_Recording_Name = Recording_Name) %>% 
    separate_wider_regex(Recording_Name, c(Recording_Name = ".*", "-threshold-\\d+"))
  
  # Change Adjuvant name from No to None 
  squares_master$Adjuvant[squares_master$Adjuvant == 'No']       <- 'None'
  images_master$Adjuvant[images_master$Adjuvant == 'No']         <- 'None'
  
  # Correct concentrations (irrelevant accuracy)
  squares_master$Concentration[squares_master$Concentration == 4.9]     <- 5
  squares_master$Concentration[squares_master$Concentration == 14.6]    <- 15
  
  images_master$Concentration[images_master$Concentration == 4.9]     <- 5
  images_master$Concentration[images_master$Concentration == 14.6]    <- 15
  
  # Remove suspect Concentration  
  squares_master <- squares_master %>%
    filter(squares_master$Concentration != 0.1)
  
  images_master <- images_master %>%
    filter(images_master$Concentration != 0.1)
  
  # Make Concentration integer
  # squares_master$Concentration <- as.numeric(squares_master$Concentration)
  
  # Drop unusual thresholds 
  # squares_master <- squares_master[squares_master$Threshold != '7',] 
  # squares_master <- squares_master[squares_master$Threshold != '8',] 
  
  # Merge Adjuvants
  squares_master$Adjuvant[squares_master$Adjuvant == 'MPLA']     <- 'Adj'
  squares_master$Adjuvant[squares_master$Adjuvant == 'LPS']      <- 'Adj'
  squares_master$Adjuvant[squares_master$Adjuvant == 'LPS+CytD'] <- 'Adj+CytD'
  
  images_master$Adjuvant[images_master$Adjuvant == 'MPLA']     <- 'Adj'
  images_master$Adjuvant[images_master$Adjuvant == 'LPS']      <- 'Adj'
  images_master$Adjuvant[images_master$Adjuvant == 'LPS+CytD'] <- 'Adj+CytD'
  
  # Make columns factor where necessary
  squares_master$Probe      <- factor(squares_master$Probe,       levels = c('1 Mono', '2 Mono', '6 Mono', '1 Bi', '2 Bi', '6 Bi', '1 Tri', '2 Tri', '6 Tri', 'Control'))
  squares_master$Probe_Type <- factor(squares_master$Probe_Type,  levels = c('Simple', 'Epitope'))
  squares_master$Cell_Type  <- factor(squares_master$Cell_Type,   levels = c('CHOMR', 'BMDC', "MR-", "iCD103", "spDC" ))
  squares_master$Adjuvant   <- factor(squares_master$Adjuvant,    levels = c('None', 'CytD', 'Adj', 'Adj+CytD'))
  squares_master$Valency    <- factor(squares_master$Valency,     levels = c('1', '2', '6', 'Control'))
  squares_master$Structure  <- factor(squares_master$Structure,   levels = c('Mono', 'Bi', 'Tri', 'Control'))
  
  # Make columns factor where necessary
  images_master$Probe      <- factor(images_master$Probe,       levels = c('1 Mono', '2 Mono', '6 Mono', '1 Bi', '2 Bi', '6 Bi', '1 Tri', '2 Tri', '6 Tri', 'Control'))
  images_master$Probe_Type <- factor(images_master$Probe_Type,  levels = c('Simple', 'Epitope'))
  images_master$Cell_Type  <- factor(images_master$Cell_Type,   levels = c('CHOMR', 'BMDC', "MR-", "iCD103", "spDC" ))
  images_master$Adjuvant   <- factor(images_master$Adjuvant,    levels = c('None', 'CytD', 'Adj', 'Adj+CytD'))
  
  # Only consider squares that do have a Valid Tau and are Visible
  squares_master <- squares_master %>%
    filter(Valid_Tau == TRUE) %>%
    filter(Visible == TRUE)  
  
  # Remove columns that will never be used
  squares_master <- squares_master %>%
    select(-c(X0, X1, Y0, Y1, Visible, Valid_Tau))
  
  # Check for NA
  if (FALSE) {
    if (sum(is.na(squares_master)) > 0 ) {
      which(is.na(squares_master), arr.ind=TRUE)
      stop('Na in squares master')
    }
  
    if (sum(is.na(images_master)) > 0 ) {
      which(is.na(images_master), arr.ind=TRUE)
      stop('Na in images_master')
  
    }
  }
  
  return (list(squares_master=squares_master, images_master=images_master, tracks_master=tracks_master))
}


