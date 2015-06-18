best <- function(state,outcome) {
#read data frame into R
  
  df <- read.csv("outcome-of-care-measures.csv",colClasses= "character",na.strings="Not Available")

#declare variables
  goodOutcome <- c("heart attack","heart failure","pneumonia")
  goodState <- unique(df[,7])
  df2 <- df[which(df[,7] == state),]
     
#check if the state and outcomes are valid
  if (!outcome %in% goodOutcome) {stop("invalid outcome")}
  if (!state %in% goodState) {stop("invalid state")}
  
#get the column with the correct subset of outcome and state
  if (outcome == "heart attack") {colOutcome <- 11}
  else if (outcome == "heart failure") {colOutcome <- 17}
  else {colOutcome <- 23}
  
#remove na values from the specified outcome
  completedf <- df2[complete.cases(df2[,colOutcome]),]

#find the rows with the minimum outcome value
  collook <- as.numeric(completedf[,colOutcome])
  minrow <- which(collook == min(collook))
#find hospital
  hospital <-completedf[minrow,2]

#return the hospital
  hospital
}
