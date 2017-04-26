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
  
  out = data.frame()
  
  #task: given a point & its features, tell how far up and for how long we'll go
  
  #arch: label all points with how far up this trend will continue as a % value change. 
  #Also say for how many days the trend will continue
  
  j = 1
  for(i in 1:length(prices)) {
    
    trendStart = trendData$startIdx[j]
    trendEnd = trendData$endIdx[j]
    
    while((i < trendStart || i >= trendEnd) && j < nrow(trendData)) {
      j = j + 1
      trendStart = trendData$startIdx[j]
      trendEnd = trendData$endIdx[j]
    }
    
    if(j < nrow(trendData)) {
      price = prices[i]
      
      valueChangeFromHere = (prices[trendData$endIdx[j]] - price) / prices[trendData$endIdx[j]] 
      trendDurationFromHere = trendEnd - i
      
      out <- rbind(out, data.frame('pricePctChgByTrendEnd' = valueChangeFromHere, 'trendRemainingDuration' = trendDurationFromHere))
    }
  }
  
  write_csv(out, "labeling.csv")
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