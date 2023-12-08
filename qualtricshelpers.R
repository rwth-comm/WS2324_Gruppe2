require(lubridate)
require(tidyverse)

#' Loads Qualtrics generated csv files that contain 3 line headers and removes the first and the second line of meta-information.
#'
#' @param filename Character of the file location.
#'
#' @return raw data with a single header
#' @export
#'
#' @examples
load_qualtrics_csv <- function(filename) {
  suppressWarnings(
    header <- read_csv(filename, n_max = 2))
  suppressWarnings(
    raw <- read_csv(filename, skip = 2)
  )
  names(raw) <- names(header)
  
  remove_nbsp <- function(text){
    gsub("\u00A0", " ", text, fixed = TRUE)
  }
  raw <- raw %>% mutate_if(is.character,remove_nbsp)
  
  raw
}

#' 
#' @param df shortened Dataframe.
#' @param filename Path to unshortened Data with matching names to df. 
#' @param path Path where codebook should be generated.
#'
#' @return raw data with a single header
#' @export
#'
#' @examples
generate_codebook <- function(df, filename, path) {
  suppressWarnings(
    header <- read_csv(filename, n_max = 2))
  raw <- select(header, one_of(names(df))) 
  variable <- names(raw)
  variable_old <- names(raw)
  text <- as.character(as.vector(raw[1,]))
  tmp_text <- strsplit(as.character(raw[1,]), " - ")
  item <- NULL
  for (elem in tmp_text) {
    item <- append(item, elem[2])
  }
  info <- as.character(as.vector(raw[2,]))
  codebook <- tibble(variable, variable_old, item, text, info)
  suppressMessages(
    write_delim(codebook, path, delim = ";"))
  
}


read_codebook <- function(filename){
  read_delim(filename, delim = ";")
}

vars4psych <- function(x, pat){
  names <- names(x)
  all <- str_subset(names, pattern = paste0("^", pat))
  input_text <- str_subset(names, pattern = "text")
  items <- all[!all %in% input_text]
  neg <- str_subset(items, pattern = paste0("n$"))
  if(!is_empty(neg)){
    res <- c(items[!items %in% neg], paste0("-",neg))
  }else{
    res <- items[!items %in% neg]
  }
  if(is_empty(res)){
    NA
  }else{
    res
  }
}

