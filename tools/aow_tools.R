
# output data frame summary report in html
aow_df_summary <- function(data_frame_file, data_frame_name) {
  
  rmarkdown::render("tools/describe_data_frame.Rmd", 
                    params = list(df_file = data_frame_file, df_name = data_frame_name),
                    output_file=paste0("../reports/df_summaries/", gsub(" ", "_", tolower(data_frame_name)), ".html"))
  
  
}

