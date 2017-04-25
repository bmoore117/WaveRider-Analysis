localMinima <- function(x) {
  y <- diff(c(Inf, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

localMaxima <- function(x) {
  y <- diff(c(-Inf, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

analysis <- function(x) {
  min = localMinima(x)
  max = localMaxima(x)
  
  temp = c(min, max)
  
  inflectionPts = temp[order(temp)]
  
  trends = data.frame()
  
  prevPoint = -1
  
  for(point in inflectionPts) {
    if(prevPoint != -1) {
      
      price = x[point]
      prevPrice = x[prevPoint]
      
      delta = price - prevPrice
      pctDelta = signif(delta/prevPrice, 2)
      trendDuration = point - prevPoint
      
      trends <- rbind(trends, data.frame('startIdx' = prevPoint, 'endIdx' = point, 'trendPct' = pctDelta, 'trendDuration' = trendDuration))
    }
    prevPoint = point
  }
  
  return(trends)
}

writeLabels <- function(prices, trendData) {
  
  out = data.frame('idx' = integer(0), 'trend1' = integer(0), 'trend2' = integer(0), 'trend3' = integer(0), 'trend4plus' = integer(0), 'pct25' = integer(0), 'pct50' = )
  
  #task: given a point & its features, tell how far up and for how long we'll go
  
  #arch: label all points with how far up this trend will continue as a % value change. 
  #Also say for how many days the trend will continue
  
  for(i in 1:length(x)) {
    prevPoint = 0
    for(j in 1:nrow(trendData)) {
      point = trendData$startIdx[j]
      if(i) {}
    }
    
    isUnder1 = as.integer(abs(trend) <= stddev)
    isUnder2 = as.integer(abs(trend) <= 2*stddev)
    isUnder3 = as.integer(abs(trend) <= 3*stddev)
    isUnder4plus = as.integer(abs(trend) <= 4*stddev)
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

init <- function() {
  library(readr)
  F5 <- read_csv("C:/Users/Ben/Documents/DataSets/F5.csv", 
                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  
  F5 <- F5[order(as.Date(F5$Date)), ]
  return(F5)
}