# Copied frokm https://raw.githubusercontent.com/topepo/caret/master/pkg/caret/R/nearZeroVar.R
# because caret build was failing on travis. This will be removed once the build problem is solved

nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE)
{
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data)
  {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0);
    }
    w <- which.max(t);
    return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics)
  {
    out <- data.frame(freqRatio = freqRatio,
                      percentUnique = percentUnique,
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
    if(names){
      out <- colnames(x)[out]
    }
  }
  out
}
