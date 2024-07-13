#' Load dataset from various formats
#'
#' This function loads datasets from .csv, .xls, .xlsx, and .txt files
#'
#' @param file_path The path to the data file.
#' @return A data frame containing the loaded data
#' @export
#' @examples
#' data <- load_dataset("data.csv")
 load_dataset <- function(file_path) {
   extension <- tools::file_ext(file_path)
   if(extension == "csv") {
     data <- read.csv(file_path)
   } else if (extension %in% c("xls", "xlsx")) {
     data <- readxl::read_excel(file_path)
   } else if (extension == "txt") {
     data <- read.table(file_path, header = TRUE)
   } else {
     stop("Unsupported file type")
   }
   return(data)
 }
