################################################################################
# This software is released under the MIT License see LICENSE.txt
# Filename : WorkingSet1.R
# Overview : Make a graph of a windows performance log of specified process's
#            Working Set
# HowToUse : rscript WorkingSet1.R WindowsPerformanceLog.csv ProcessName
# 
#-------------------------------------------------------------------------------
# Author: Isaac Factory (sir.isaac.factory@icloud.com)
# Repository: https://github.com/SirIsaacFactory/RTools
# Date: 2021/04/30
# Code version: v1.00
################################################################################

################################################################################
# Import Libraries
################################################################################
library(dplyr)

################################################################################
# MAIN
################################################################################
main <- function(infile, procname) {
  #-----------------------------------------------------------------------------
  # Define variables
  #-----------------------------------------------------------------------------
  normal_end    <- 0
  error_end     <- 1
  infilebase    <- sub("\\.[^\\.]*", "", infile, ignore.case = FALSE)
  resultcsv     <- paste(infilebase,"_", procname, "_WorkingSet1.csv", sep="")
  resultpdf     <- paste(infilebase,"_", procname, "_WorkingSet1.pdf", sep="")
  graphtitle    <- basename(infile)
  resource_name <- "Working Set"
  resource      <- "\\.Working\\.Set"

  print(paste("input file:", infile))
  print(paste("result csv:", resultcsv))
  print(paste("result pdf:", resultpdf))


  #-----------------------------------------------------------------------------
  # Summarise
  #-----------------------------------------------------------------------------
  # read csv file
  df <- read.csv(infile, row.names=1)
  # replace space with 0
  df[is.na(df)] <- 0
  # change data scale from bytes to megabytes
  df <- df/(1024*1024)
  # select data
  df <- select(df, matches(paste("\\.Process\\.", procname, "\\..*", resource, "$", sep="")))

  #-----------------------------------------------------------------------------
  # Check input data
  #-----------------------------------------------------------------------------
  if(length(df) == 0) {
    print(paste("There is no data of", procname, "process Working Set"))
    return(error_end)
  }

  #-----------------------------------------------------------------------------
  # Additional summarise
  #-----------------------------------------------------------------------------
  # get minimum and maximum memory
  minmem <- 0
  maxmem <- max(df)
  print(paste("minmem=", minmem, ", maxmem=", maxmem))
  # get the number of columns
  datanum <- ncol(df)

  #-----------------------------------------------------------------------------
  # Display graph
  #-----------------------------------------------------------------------------
  # set x-axis
  x <- 1:nrow(df)

  # get dataframe row name(windows counter log time columns)
  time    <- rownames(df)
  # get datafrmae's row number
  datacnt <- length(time)
  # set max x-axis label
  axiscnt <- 20

  # get ready for write pdf file
  pdf(resultpdf)
  # show graph
  for(i in 1:datanum) {
    plot(
      x,
      df[, i],
      col=i,
      type="l",
      xlab="",
      ylab="",
      ylim=c(minmem, maxmem),
      axes=FALSE,
      xaxt="n"
    )
    par(new=T)
  }

  # graph title
  mtext(paste(procname, " ", resource_name, "(", graphtitle, ")", sep=""), side = 3, line = 1)

  # left y
  axis(2)
  # left y label
  mtext("Megabytes", side = 2, line = 3)

  # show x-axis labels
  # Windows counter logs' output format is as below
  # 03/05/2021 00:02:05.262
  # find first space and get 5 characters from poisition
  # and make it as x-axis label
  i       <- 1 # loop count
  atpoint <- 1 # the point that shows x-axis label
  step    <- floor(datacnt/axiscnt)
  while(i<datacnt) {
    atpoint <- i
    # find a space
    base <- regexpr("\\ ", time[atpoint])
    # get hours and minutes
    timelab <- substr(time[atpoint], base+1, base+5)
    # show x-axis label(HH:MM)
    axis(side=1, at=atpoint, labels=timelab, las=2)

    # count up for the next loop
    i <- step + i
  }

  box()

  procs <- c()
  for(i in 1:datanum) {
    procs[i] <- paste(procname, "_", i, sep="")
  }

  legend("bottomleft",
        legend=procs,
        lty=c(1,1),
        col=c(1:datanum),
        bg="transparent"
  )

  # output csv file
  write.csv(df, resultcsv)
  # output pdf file
  dev.off()
}

################################################################################
# CALL MAIN
################################################################################
args <- commandArgs(trailingOnly = T)
if( length(args) != 2) {
  print("add the performance log as a command line argment.")
} else {
  main(args[1], args[2])
}
