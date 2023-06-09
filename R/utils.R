
ts2time <- function(timestamp) {
  timestamp <- substr(as.character(timestamp), 1, 10)
  as.POSIXct(as.integer(timestamp), origin = "1970-01-01 00:00:00", tz = "Asia/Shanghai")
}


time2ts <- function(time, type = c("s", "ms")) {
  if (!inherits(time, "POSIXt")) {
    stop("parameter time must be of POSIXt type")
  } else {
    type <- match.arg(type)
    time <- as.integer(time)
    if (type == "ms") {
      time <- time * 1000
    }
    time
  }
}


str2period <- function(str) {
  time_vec <- c(
    "1m" = 60, "3m" = 3*60, "5m" = 5*60, "15m" = 15*60, "30m" = 30*60,
    "1H" = 1*60*60, "2H" = 2*60*60, "4H" = 4*60*60,
    "6H" = 6*60*60, "12H" = 12*60*60,
    "1D" = 1*24*60*60, "2D" = 2*24*60*60, "3D" = 3*24*60*60
  )
  time_vec[str]
}


numeric_fields <- function() {
  c(
    "accFillSz", "activePx", "actualPx", "actualSz", "adl", "availPos", "avgPx",
    "baseBal", "baseBorrowed", "baseInterest", "bizRefType",
    "callbackRatio", "callbackSpread", "closeFraction",
    "deltaBS", "deltaPA",
    "failCode", "fee", "fillPx", "fillSz",
    "gammaBS", "gammaPA",
    "imr", "interest",
    "last", "lever", "liab", "liqPx",
    "margin", "markPx", "mgnRatio", "mmr", "moveTriggerPx",
    "notionalUsd",
    "optVal", "ordPx",
    "pendingCloseOrdLiabVal", "pnl", "pos", "px", "pxLimit", "pxSpread", "pxVar",
    "quoteBal", "quoteBorrowed", "quoteInterest",
    "rebate",
    "slOrdPx", "slTriggerPx", "slTriggerPxType", "source", "spotInUseAmt", "state", "sz", "szLimit",
    "tdMode", "thetaBS", "thetaPA", "timeInterval", "tpOrdPx", "tpTriggerPx", "triggerPx", "triggerTime",
    "upl", "uplLastPx", "uplRatio", "uplRatioLastPx", "usdPx",
    "vegaBS", "vegaPA"
  )
}


time_fields <- function() {
  c(
    "cTime", "uTime", "fillTime"
  )
}

