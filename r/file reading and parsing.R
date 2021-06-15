# Load
file_data <- readRDS("./data/file_meta.RDS")

# Set up list for csv
raw_csv_data <- vector(mode='list',length=nrow(file_data))
  
  file_data %>%
  mutate(raw_content=ifelse(
    file_type=="csv" & !is.na(month) & !is.na(year),
    read_csv(file_list,col_names=TRUE,col_types="c"),
    list()
    ))
