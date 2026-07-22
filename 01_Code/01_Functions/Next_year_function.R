## Function that takes a year and returns the next year in the given vector
getNextYear <- function(inputYear, yearVector, smaller = TRUE) {
  
  if(smaller == TRUE){
    ## Convert input year and yearVector to integers for comparison
    inputYear <- as.integer(inputYear)
    yearVector <- as.integer(yearVector)
    
    if(inputYear %in% yearVector){
      smallerYears <- inputYear
    } else if (inputYear < min(yearVector)){
      smallerYears <- min(yearVector)
    } else {
      ## Find the next smaller year in the year vector
      smallerYears <- yearVector[yearVector < inputYear]
      smallerYears <- max(smallerYears)
    }
  }
  
  if(smaller == FALSE){
    ## Convert input year and yearVector to integers for comparison
    inputYear <- as.integer(inputYear)
    yearVector <- as.integer(yearVector)
    
    if(inputYear %in% yearVector){
      smallerYears <- inputYear
    }  else {
      ## Find the next year in the year vector
      closestYears <- which(abs(as.numeric(yearVector) - inputYear) == min(abs(as.numeric(yearVector) - inputYear)))
      smallerYears <- yearVector[closestYears[1]]
    }
  }
  
  return(smallerYears)
}