library(readxl)
source('../Utility Code/Eliminate.R')

read_data <- function(root_directory, output, manually_exclude = FALSE, duration_exclude = FALSE, exclude_file='Excluded Images.csv') {
  
  
  squares_file        <- 'All Squares.csv'
  images_file         <- 'All Images.csv'
  tracks_file         <- 'All Tracks.csv'
  
  exclude_file        <- 'Excluded Images.csv'
  valid_squares_file  <- 'Valid Squares.csv'
  valid_images_file   <- 'Valid Images.csv'

  squares_master      <- read_csv(paste0(root_directory, '/', output, '/', squares_file), show_col_types = FALSE)
  images_master       <- read_csv(paste0(root_directory, '/', output, '/', images_file), show_col_types = FALSE)
  tracks_master       <- read_csv(paste0(root_directory, '/', output, '/', tracks_file), show_col_types = FALSE)

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
  
  # Remove the threshold part of the image name and store in column Recording_Name
  squares_master <- squares_master %>% separate_wider_regex(Recording_Name, c(Recording_Name = ".*", "-threshold-\\d+"))
  
  # Change Adjuvant name from No to None 
  squares_master$Adjuvant[squares_master$Adjuvant == 'No']       <- 'None'
  images_master$Adjuvant[images_master$Adjuvant == 'No']         <- 'None'

  probes_list      <- c('M5N2', '6E-643', '2,3SL', '2,6SL', 'sLeX', '2LeX', '(2,6SL)3', 'Mal')
  cell_types_list  <- c('BMDM', 'BMDC')
  adjuvants_list   <- c('None', 'LPS', 'IL-4')
  probe_types_list <- c('Simple')
  
  setdiff(squares_master$Probe, probes_list)
  
  # Make columns factor where necessary
  squares_master$Probe      <- factor(squares_master$Probe,       levels = probes_list)
  squares_master$Probe_Type <- factor(squares_master$Probe_Type,  levels = probe_types_list)
  squares_master$Cell_Type  <- factor(squares_master$Cell_Type,   levels = cell_types_list)
  squares_master$Adjuvant   <- factor(squares_master$Adjuvant,    levels = adjuvants_list)
  
  # Make columns factor where necessary
  images_master$Probe      <- factor(images_master$Probe,       levels = probes_list)
  images_master$Probe_Type <- factor(images_master$Probe_Type,  levels = probe_types_list)
  images_master$Cell_Type  <- factor(images_master$Cell_Type,   levels = cell_types_list)
  images_master$Adjuvant   <- factor(images_master$Adjuvant,    levels = adjuvants_list)
  
  # Only consider squares that are Visible
  squares_master <- squares_master %>%
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
  
  # # Remove records from images_master that have no squares or no valid Tau
  # images_master <- images_master %>% 
  #   filter(Nr_Of_Squares_per_Row > 0) %>% 
  #   filter(Tau > 0)
  
  write_csv(squares_master, paste0(root_directory, '/', output, '/squares_master.csv'))
  write_csv(images_master, paste0(root_directory, '/', output, '/images_master.csv'))
  
  return (list(squares_master=squares_master, images_master=images_master, tracks_master=tracks_master))
}


