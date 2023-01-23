#' @title Import data of UA file, output UAMAKE
#' @description Combine multiple plot files 
#' @param ua Path UAMAKE output file
#' @param lvs Levels
#' @return A list
#' @export combine_plt
#' @examples
#' file_dat <- system.file("extdata", "uamake.dat", package="Rcalmet")
#' importUA(file_dat, 22)
#'

importUA <- function(ua, lvs){
  cnt_lev <- ceiling(lvs/4) + 1
  # dt <- readr::read_lines(ua, skip=9)
  dt <- readLines(ua)
  dt <- dt[-1:-9]
  len <- length(dt)/cnt_lev
  dt <- split(dt, rep(1:len, each=cnt_lev))
  hd <- lapply(dt, function(x){
    nm <- strsplit(x[1], " +")[[1]]
    nm1 <- sprintf("%2s", nm[5:7])
    nm1 <- gsub(" ", "0", nm1)
    paste0(nm[4], "-", nm1[1], "-", nm1[2], " ", nm1[3])
  })
  bd <- lapply(dt, function(x){
    dt <- paste(x[-1], collapse = "")
    dt <- gsub(" ", "", dt)
    dt <- strsplit(dt, ",")[[1]]
    dt <- split(dt, rep(1:5, 22))
    df <- data.frame(PRES=as.numeric(dt[[1]]),
                     Z=as.numeric(dt[[2]]),
                     TEMPK=as.numeric(dt[[3]]),
                     WD=as.numeric(dt[[4]]),
                     WS=as.numeric(dt[[5]]))
  })
  names(bd) <- unlist(hd)
  return(bd)
}
