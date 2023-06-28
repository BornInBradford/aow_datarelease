
# output data frame summary report in html
aow_df_summary <- function(data_frame_file, data_frame_name) {
  
  rmarkdown::render("tools/describe_data_frame.Rmd", 
                    params = list(df_file = data_frame_file, df_name = data_frame_name),
                    output_file=paste0("../reports/df_summaries/", gsub(" ", "_", tolower(data_frame_name)), ".html"))
  
  
}


# load salt and output digest only if string is non-null and has length>0
aow_pseudo <- function(input, salt = getOption("aow_salt")) {
  
  # load the config file that sets the salt option if it wasn't already
  if(is.null(salt)) {
  
    source("config/set_aow_salt.R")
    salt <- getOption("aow_salt")
  
  }
  
  # make sure empty string input gives empty string output
  if(!is.na(input) && nchar(input) > 0) {
    
    output <- digest::digest(paste0(input, salt), algo = "sha1", serialize = FALSE)
  
  } else {
    
    output <- ""
    
  }
  
  return(output)
  
}
