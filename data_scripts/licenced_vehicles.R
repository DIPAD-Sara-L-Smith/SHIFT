# Licences Vehicles
# https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# Licensed vehicles by body type (quarterly): Great Britain and United Kingdom
library('tidyverse')

data_location = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853452/veh0101.ods"

# read_ods can't read ODS directly from the web so download to a temp location
temp_file = tempfile()
download.file(data_location, 
              temp_file, 
              quiet = TRUE,
              mode="wb")

df <- readODS::read_ods(temp_file,
                        skip = 8, 
                        sheet = 'VEH0101')


# Remove the file from temp directory
file.remove(temp_file)


# Fix some headings
df[10] <- NULL
names(df)[6] <- "Total Goods Vehicles"

# The ODS has two tables in it, keep the top table by locating
# the first empty row and 
df <- df %>% slice(1:which(is.na(df$Quarter))[1]-1)

df <- df %>%
  separate(Quarter, into=c("Year", "Quarter"), sep =" Q", convert = TRUE) %>%
  mutate_if(is.character, list(~suppressWarnings(as.numeric(.)))) %>%
  fill(Cars:Total, .direction="up")


