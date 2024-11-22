library(readxl)

#---------- Download data
#===============================================================================
# Function to open a URL in the default web browser
open_url <- function(url) {
  sysname <- Sys.info()[["sysname"]]
  
  if (sysname == "Windows") {
    shell.exec(url)
  } else if (sysname == "Linux") {
    system(paste("xdg-open", shQuote(url)), wait = FALSE)
  } else if (sysname == "Darwin") {
    system(paste("open", shQuote(url)), wait = FALSE)
  } else {
    stop("Unsupported OS")
  }
}

# Function to read URLs from the file one by one and open each
read_and_open_urls <- function(file_path) {
  con <- file(file_path, "r")
  on.exit(close(con))  # Ensure that the file connection is closed when done
  
  while (TRUE) {
    url <- readLines(con, n = 1)
    if (length(url) == 0) {
      break
    }
    open_url(url)
    Sys.sleep(1)  # pause for a second between opening URLs to avoid crash
  }
}

# File with links
file_path <- "DHS_API.txt"  

# Call the function to read and open URLs
read_and_open_urls(file_path)


#---------- Create folders
#===============================================================================
ct <- read_excel("DHS_Data.xlsx")

for (i in 1:nrow(ct)) {
  dir.create(paste0("Raw data/", ct$Country[i]), showWarnings = TRUE)
}














