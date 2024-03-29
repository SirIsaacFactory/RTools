################################################################################
# This software is released under the MIT License; see LICENSE.txt
# Filename : AvailableMegabytes.R
# Overview : Make a graph of a Windows performance log of Available MBytes
# HowToUse : rscript AvailableMegabytes.R WindowsPerformanceLog.csv
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
main <- function(infile) {
  #-----------------------------------------------------------------------------
  # Define variables
  #-----------------------------------------------------------------------------
  normal_end <- 0
  error_end  <- 1
  infilebase <- sub("\\.[^\\.]*", "", infile, ignore.case = FALSE)
  resultcsv  <- paste(infilebase,"_AvailableMegabytes.csv", sep="")
  resultpdf  <- paste(infilebase,"_AvailableMegabytes.pdf", sep="")
  infilename <- basename(infile)
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
  # select Available Megabytes column
  df <- select(df, matches(".*Memory\\.Available\\.MBytes$"))

  #-----------------------------------------------------------------------------
  # Check input data
  #-----------------------------------------------------------------------------
  if(length(df) == 0) {
    print("There is no data of Available MBytes")
    return(error_end)
  }

  #-----------------------------------------------------------------------------
  # Graph
  #-----------------------------------------------------------------------------
  # set x-axis
  x <- 1:nrow(df)

  # get dataframe row name(windows counter log time columns)
  time    <- rownames(df)
  # get datafrmae's row number
  datacnt <- length(time)
  # set max x-axis label
  axiscnt <- 20

  # get ready to write pdf file
  pdf(resultpdf)
  # show graph
  maxmem <- max(df)
  minmem <- 0
  plot(
    x,
    df[,1],
    col="blue",
    type="l",
    xaxt="n",
    xlab="Time",
    ylab="Available Megabytes",
    ylim=c(minmem, maxmem),
    main=paste("Available Megabytes(", infilename, ")", sep="")
  )

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

  # output csv file
  write.csv(df, resultcsv)
  # output pdf file
  dev.off()
}

################################################################################
# CALL MAIN
################################################################################
args <- commandArgs(trailingOnly = T)
if( length(args) != 1) {
  print("add the performance log as a command line argment.")
} else {
  main(args[1])
}
