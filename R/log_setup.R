

# get name of script
#' This function gets the name of the script being used in order to log that information
#' @return A string with the name of the script
#' @export
getname <- function(){
  name <-rstudioapi::getSourceEditorContext()$path
  name <- strsplit(name, "/")[[1]]
  name <- name[length(name)]
  return(name)
}


#' Start Logging
#' 
#' This function creates a log file that records messages and outputs from your script or markdown, the time it started running and the time it finished, and records the session info.
#' Note: Store this variable so that we can end it using the end_log function. I suggest naming it log or log_info.
#' @param log_directory Path to the folder for the log file
#' @param script_name Name of the script that is being logged
#' @export
start_log <- function(log_directory, script_name = getname()) {
  
  # Ensure proper formatting of the directory path
  log_directory <- file.path(log_directory)
  
  # Remove trailing slashes if they exist
  log_directory <- gsub("/+$", "", log_directory)
  
  # Check if the directory exists
  if (!file.exists(log_directory)) {
    stop("Error: The specified directory does not exist.")
  }
  
  
  # Define the log file path including the chunk name
  log_file_name <- paste0("log-", format(Sys.time(), "%Y-%m-%d.%H%M"), ".txt")
  log_path <- file.path(log_directory, log_file_name)
  
  # Open the log file for writing
  log_file <- file(log_path, open = "wt")
  
  # Redirect R output to the log file
  sink(log_file, type = "output", split = TRUE)
  sink(log_file, type = "message")
  
  
  # Log the start of the chunk
  cat("--------------------------------------------------\n", file = log_file)
  cat("Script:", script_name, "\n", file = log_file)
  cat("--------------------------------------------------\n", file = log_file)
  cat("Start Time:", format(Sys.time(), "%a %b %d %Y %X"), "\n", file = log_file)
  cat("--------------------------------------------------\n", file = log_file)
  
  
  # Return the log file path and connection
  return(list(path = log_path, file = log_file, script = script_name))
}

# Logging end function
#' End Log
#' This function ends the log and closes the connection to the log file.
#' @param log_info This is the variable that stores the start_log call.
#' @export
end_log <- function(log_info) {
  # Log end time and close the log file
  cat("--------------------------------------------------\n", file = log_info$file)
  cat("End Time:", format(Sys.time(), "%a %b %d %Y %X"), "\n", file = log_info$file)
  cat("--------------------------------------------------\n", file = log_info$file)
  session.info <- capture.output(sessionInfo())
  write(session.info, log_info$file, append=TRUE)
  unlink(log_info$file)
  closeAllConnections()

}
