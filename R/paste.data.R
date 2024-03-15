#' paste.data
#'
#' This function is used to read data from moodle into R
#' @param sep symbol used for separation
#' @param header does data have a header?
#' @param is.table is data a table? Needed if all data is character.
#' @return the data in the clipboard
#' @export

paste.data = function(sep="", header=TRUE, is.table=FALSE) {
  .gmd.win = function(sep="", header=TRUE, is.table=FALSE) {
    if(is.table) {
       df = utils::read.table("clipboard", header=header)
    }
    else {
      moodledata = scan("clipboard", what="character", sep=sep, quiet=TRUE)
      if(any(is.na(as.numeric(moodledata))) & any(!is.na(as.numeric(moodledata)))) {
         df = utils::read.table("clipboard", header=header)
      }
      else {
        if(all(!is.na(as.numeric(moodledata)))) {
          df = as.numeric(moodledata)    
        }  
        else {
          z = table(moodledata)
          z = z[z==1]
          if(length(z)>1) 
            df = utils::read.table("clipboard", header=header)
          else  {
            df = moodledata
          }
        }
      }
    }
    df
  }

  .gmd.other = function(sep="", header=TRUE, is.table=FALSE) {
    if(is.table) {
      df = utils::read.table(pipe("pbpaste"), header=header)
    }
    else {
      moodledata = scan(pipe("pbpaste"), what="character", sep=sep, quiet=TRUE)
      if(any(is.na(as.numeric(moodledata))) & any(!is.na(as.numeric(moodledata)))) {
        df = utils::read.table(pipe("pbpaste"), header=header)
      }
      else {
        if(all(!is.na(as.numeric(moodledata)))) {
             df = as.numeric(moodledata)    
        }  
        else {
          z = table(moodledata)
          z = z[z==1]
          if(length(z)>1) 
            df = utils::read.table(pipe("pbpaste"), header=header)
          else  {
            df = moodledata
          }
        }
      }
    }
    df
  }

  if(.Platform$`OS.type`=="windows")
      df = suppressWarnings(.gmd.win(sep=sep, header=header, is.table=is.table))
  else
      df = suppressWarnings(.gmd.other(sep=sep, header=header, is.table=is.table))
  message("Data begins with:\n")
  if(is.null(dim(df))) {
    dftext = paste(utils::head(df), " ")
    message(dftext)
  }  
  else {
    message(as.character(paste(colnames(df)," ")))
    for(i in 1:min(6, nrow(df)))
      message(as.character(paste(df[i, ]," ")))
  }  
  df
}



