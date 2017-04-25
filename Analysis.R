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
  
  trends = data.frame(startIdx = integer(0), endIdx = integer(0), trendPct = double(0))
  
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
  
  #need: df with trend start idx, end idx, and strength. Then later on in labeling can say 
  #how far along in the trend point is, and how strong a trend that happens to be
  
  #end result of analysis and labeling will be a csv with all points labeled as above
  #so that when we feed into the trader, we'll have a set of predictions about this point, e.g.:
  
  #25% chance it is in a uptrend of 5% or greater, and is in the first 25% of that trend
  
  #sample file format:
  
  #idx, trend1Dev, trend2Dev, trend3Dev, trend4+, percentile25, percentile50, percentile75+
  
  return(trends)
}

WriteLabels <- function(x) {
  stddev = sd(x$trendPct)
  avg = mean(x$trendPct)
  
  out = data.frame('idx' = integer(0), 'trend1' = integer(0), 'trend2' = integer(0), 'trend3' = integer(0), 'trend4plus' = integer(0), 'pct25' = integer(0), 'pct50' = )
  
  for(trend in x) {
    #1 is up, 0 is down
    trendDir = 0
    temp = sign(trend)
    if(temp == -1) {
      temp = 0
    }
    
    isUnder1 = as.integer(abs(trend) <= stddev)
    isUnder2 = as.integer(tabs(trend) <= 2*stddev)
    isUnder3 = as.integer(abs(trend) <= 3*stddev)
    isUnder4plus = as.integer(abs(trend) <= 4*stddev)
    
    
    
  }
}

init <- function() {
  library(readr)
  F5 <- read_csv("C:/Users/Ben/OneDrive/Code/Trading/DataSets/F5.csv", 
                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  
  F5 <- F5[order(as.Date(F5$Date)), ]
  return(F5)
}