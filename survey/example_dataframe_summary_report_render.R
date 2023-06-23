# example dataframe summary report rendering

# may need to create reports folder in project root - currently ignored by git

data_frame_file <- "U:/Born In Bradford - Confidential/Data/BiB/processing/AoW/survey/data/aow_survey_module4_linked.rds"
data_frame_name <- "Survey module 4 linked data frame"

  
rmarkdown::render("tools/describe_data_frame.Rmd", 
                  params = list(df_file = data_frame_file, df_name = data_frame_name),
                  output_file=paste0("../reports/", gsub(" ", "_", tolower(data_frame_name)), ".html"))
