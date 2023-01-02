#' @title Import data of TSF file, output METSERIES
#' @description Import data to TSF file generaded for METSERIES as a data.frame.
#' @param tsf Path TSF file
#' @param type Type output TSF file. "m3d" or "calmet"
#' @return A data.frame
#' @export importarTSF
#' @examples
#' file_tsf <- system.file("extdata", "pasco.tsf", package="calmet")
#' importTSF(file_tsf, "m3d")
#'

importTSF <- function(tsf, type){
  if(type == "m3d"){
    nl <- 27
  } else if(type == "calmet"){
    nl <- 31
  }
  df <- utils::read.table(tsf, skip = nl)
  df$V1 <- paste(df$V1, df$V2, df$V3, df$V4)
  df$V1 <- as.POSIXct(strptime(df$V1, "%Y %m %d %H"))
  if(type == "m3d"){
    df <- df[, c(1,11:19)]
    names(df) <- c("date", "WDIR", "WSPEED", "TEMP", "SHUMID", "SLVPRESS",
                   "PRECIP_RATE", "SHORT_WAVE", "SST_GROUNDT", "RH_HUMIDITY")
  } else if(type == "calmet"){
    df <- df[, c(1,11:23)]
    names(df) <- c("date", "WDIR", "WSPEED", "TEMP", "SHUMID", "MIXHGT",
                   "PRECIP_RATE", "USTAR", "MONIN_OB", "CONV_VEL_S", "SHORT_WAVE",
                   "STAB_CLASS", "RH_HUMIDITY", "SFC_PRESS")
  }
  return(df)
}
