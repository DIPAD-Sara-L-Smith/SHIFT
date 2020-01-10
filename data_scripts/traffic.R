# All traffic
# https://www.gov.uk/government/statistical-data-sets/tra25-quarterly-estimates
# Road traffic (vehicle miles) by vehicle type in Great Britain

library(tidyverse)


data_location = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/852702/tra2501.ods"

# read_ods can't read ODS directly from the web so download to a temp location
temp_file = tempfile()
download.file(data_location, 
              temp_file, 
              quiet = TRUE,
              mode="wb")

df <- readODS::read_ods(temp_file,
                        skip = 6, 
                        sheet = 'TRA2501b')

# Remove the file from temp directory
file.remove(temp_file)

df <- df %>%
  select(-3) %>%
  drop_na(Quarter) %>% 
  fill(Year) %>%
  extract(Quarter, into = "Quarter", regex = "([1234])") 

names(df) <- make.names(names(df))