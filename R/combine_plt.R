#' @title Combine plot files
#' @description Combine multiple plot files in a single file
#' @param files Path plot files
#' @param out File output
#' @param type Type plot file 
#' @return A vector of characters
#' @export combine_plt
#' @examples
#' file_dat <- system.file("extdata", "rank.dat", package="Rcalmet")
#' file_out <- tempfile()
#' plt <- combine_plt(c(file_dat, file_dat), type="long")
#' # writeLines(plt, file_out)
#'

combine_plt <- function(files, out="int.dat", type="short"){
  header <- readLines(files[1], n=6)
  n = length(files)
  body <- lapply(files, function(fl){
    df <- utils::read.table(fl, skip=6)
  })
  df <- body[[1]]
  i = 2
  while(i <= n){
    df$V3 <- df$V3 + body[[i]]$V3
    i = i + 1
  }
  # Format Body
  df$V1 <- sprintf("%12.3f", df$V1)
  df$V2 <- sprintf("%9.3f", df$V2)
  df$V3 <- sprintf("%14.4E", df$V3)
  if(type=="short"){
    df$V4 <- sprintf("%14.6f", df$V4)
    df$V5 <- sprintf("%11.6f", df$V5)
    bd <- paste(df$V1, df$V2, df$V3, df$V4, df$V5)
  } else if(type=="long"){
    bd <- paste(df$V1, df$V2, df$V3)
  }
  return(c(header, bd))
}
