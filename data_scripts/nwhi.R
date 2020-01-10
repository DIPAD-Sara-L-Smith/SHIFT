# Nationwide Housing Index 

# We have previously used the Seasonally Adjusted pricing index
# this is a particulary messy one.

library(tidyverse)

data_location = "https://www.nationwide.co.uk/-/media/MainSite/documents/about/house-price-index/downloads/seasonal_regional.xls"


# Download the excel file to a temporary file location
temp_file = tempfile()
download.file(data_location, 
              temp_file, 
              quiet = TRUE,
              mode="wb")

nwhi <- readxl::read_xls(path = temp_file,
                         sheet = "Seasonal",
                         skip = 2,
                         .name_repair = "universal")

# Clean up temp
file.remove(temp_file)


# Split Quarter-Year Column into seperate columns, then drop the regional indices.
nwhi <- nwhi %>% 
  separate(1, into=c("Quarter", "Year"), sep =" Q", convert = TRUE) %>%
  select(Year, Quarter, National = "UK...15") %>%
  mutate(Projection = FALSE) 


# TODO Add projections as per current method
