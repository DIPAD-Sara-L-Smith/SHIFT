# New vehicle registrations
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853464/veh0150.ods
# Vehicles registered for the first time by body type, monthly: Great Britain and United Kingdom 


library('tidyverse')

data_location = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853464/veh0150.ods"

# read_ods can't read ODS directly from the web so download to a temp location
temp_file = tempfile()
download.file(data_location, 
              temp_file, 
              quiet = TRUE,
              mode="wb")

df <- readODS::read_ods(temp_file,
                        skip = 8, 
                        sheet = 'VEH0150', 
                        col_types = cols( 
                          Date = col_character(), 
                          .default = col_double()))

# Remove the file from temp directory
file.remove(temp_file)

# Filter to grab only the Quarterly data and drop unused columns.
df <- df %>% 
  filter(grepl('Q', Date)) %>% 
  separate(Date, into=c("Year", "Quarter"), sep =" Q", convert = TRUE) %>%
  select(Year:Total) 




