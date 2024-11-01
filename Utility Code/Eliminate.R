

diagnosis = TRUE


# manually_exclude = TRUE
# duration_exclude = TRUE
# squares_file        <- 'All Squares.csv'
# images_file         <- 'All Images.csv'
# output='output'
# root_directory   = '../Data/Paint Regular Probes - Traditional - 20 squares - 2 DR'
# squares_master <- read_csv(paste0(root_directory, '/', output, '/', squares_file), show_col_types = FALSE)
# images_master  <- read_csv(paste0(root_directory, '/', output, '/', images_file), show_col_types = FALSE)
# names(squares_master) <- str_replace_all(names(squares_master), c(" " = "_" ))
# names(images_master)  <- str_replace_all(names(images_master), c(" " = "_" ))


eliminate_excluded <- function(root_directory, squares_master, images_master, manually_exclude, duration_exclude, exclude_file='Excluded Images.csv') {
  
  # Create a directory for diagnosis if needed
  if (diagnosis) {
    dir_path <- paste0(root_directory, '/Diagnostics/')
    print(dir_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }
  
  # Identify images that have a suspect length and eliminate
  if (duration_exclude) {
    
    images_master_d <- images_master
    squares_master_d <- squares_master
    
    images_master_d <- images_master %>%
      filter(Recording_Size >= 1058000896 * 0.95 & Recording_Size <=  1058000896 * 1.20) 
    
    squares_master_d <- squares_master %>%
      filter(Recording_Name %in% images_master_d$Ext_Recording_Name)
    
    if (diagnosis) {
      write_csv(squares_master_d, paste0(root_directory, '/Diagnostics/', 'squares_master_after_duration_elimination.csv'))
      write_csv(images_master_d, paste0(root_directory, '/Diagnostics/', 'images_master_after_duration_elimination.csv'))
    }
  } 
  
  # Identify images that have been manually excluded and eliminate
  if (manually_exclude) {
    
    images_master_m <- images_master
    squares_master_m <- squares_master
    
    excluded_images <- read.csv(paste0(root_directory, '/../Reference Info/', exclude_file)) %>% 
      filter(Process == 'Yes') %>% 
      select('Ext.Image.Name') %>% 
      rename(Ext_Recording_Name = Ext.Image.Name)
      
    images_master_m   <- anti_join(images_master , excluded_images, by = c('Ext_Recording_Name' = 'Ext_Recording_Name'))
    squares_master_m  <- anti_join(squares_master, excluded_images, by = c('Recording_Name' = 'Ext_Recording_Name'))
    
    if (diagnosis) {
      write_csv(squares_master_m, paste0(root_directory, '/Diagnostics/', 'squares_master_after_manual_elimination.csv'))
      write_csv(images_master_m, paste0(root_directory, '/Diagnostics/', 'images_master_after_manual_elimination.csv'))
    }
  }
  
  
  if (duration_exclude & manually_exclude) {
    images_master <- images_master %>% 
      filter(Ext_Recording_Name %in% images_master_d$Ext_Recording_Name,
             Ext_Recording_Name %in% images_master_m$Ext_Recording_Name)
    
    squares_master <- squares_master %>% 
      filter(Recording_Name %in% squares_master_d$Recording_Name,
             Recording_Name %in% squares_master_m$Recording_Name)
  } else if (duration_exclude) {
    images_master <- images_master_d
    squares_master <- squares_master_d
  } else if (manually_exclude) {
    images_master <- images_master_m
    squares_maste <- squares_master_m
  }
  
  if (diagnosis) {
    write_csv(squares_master, paste0(root_directory, '/Diagnostics/', 'squares_master_after_combined_elimination.csv'))
    write_csv(images_master, paste0(root_directory, '/Diagnostics/', 'images_master_after_combined_elimination.csv'))
  }
  
  return (list(squares_master=squares_master, images_master=images_master))
}
