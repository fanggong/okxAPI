#' @name get_functions
#'
#' @title Wrapper for some frequently used APIs to get data easily
#'
#' @description The main purpose is to handle APIs that have limitations on the number of results returned per single request.
#' @seealso
#' \code{\link{get_positions_history}}
#' \code{\link{get_history_candles}}
NULL


#' @title Retrieve the position data
#'
#' @description Wrapper for API [Get positions history](https://www.okx.com/docs-v5/en/#rest-api-account-get-positions-history).
#'
#' @param api_key Okx API key.
#' @param secret_key Okx API secret key.
#' @param passphrase Okx API passphrase.
#' @param before POSIXct type. Return records newer than \code{before}.
#' @param after POSIXct type. Return records earlier than \code{after}.
#' @param period Due to the 'Number of results per request' limitation of the API,
#' the \code{period} parameter must be specified to ensure that the number of position data entries within each period(unit: second) does not exceed 100.
#' @param ... Other request parameters to be passed, See
#' [Get positions history](https://www.okx.com/docs-v5/en/#rest-api-account-get-positions-history) for more information.
#'
#' @return Position data
#'
#' @examples
#' \dontrun{
#' positions <- get_positions_history(
#'   api_key, secret_key, passphrase, count = 90, period = 10,
#'   instType = "SWAP", mgnMode = "isolated"
#' )
#' }
#'
#' @import data.table
#' @export
get_positions_history <- function(
    api_key, secret_key, passphrase, before, after, period, ...
) {
  account <- restAPIaccount$new(api_key, secret_key, passphrase)
  before <- time2ts(before, type = "s")
  after <- time2ts(after, type = "s")

  start <- before
  end <- start + period

  dat <- list()
  while (start < after) {
    retry <- 0
    result <- NULL
    while (is.null(result) && retry < 5) {
      tryCatch({
        result <- account$positions_history(
          before = as.character(1000*start),
          after = ifelse(end < after, as.character(1000*end + 1), as.character(1000*after + 1)),
          ...
        )
      }, error = function(e) {
        retry <- retry + 1
        message(paste0("Error occurred, retrying (", retry, ")...\n"))
        Sys.sleep(10)
      })
    }

    if (result$code == "0") {
      dat <- c(dat, result$data)
      message("From ", ts2time(start), " to ", ts2time(ifelse(end < after, end, after)), " complete")
      start <- end + 0.001
      end <- start + period
      Sys.sleep(10)
    }
  }
  dat <- lapply(dat, data.table::as.data.table)
  dat <- data.table::rbindlist(dat)
  if (nrow(dat) > 0) {
    to_time <- c("cTime", "uTime")
    dat[, (to_time) := lapply(.SD, ts2time), .SDcols = to_time]
    to_numeric <- c("closeAvgPx", "closeTotalPos", "lever", "openAvgPx", "openMaxPos", "pnl", "pnlRatio")
    dat[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
  }
  dat
}


#' @title Retrieve the candlestick charts
#'
#' @description Wrapper for API [Get candlesticks](https://www.okx.com/docs-v5/en/#rest-api-market-data-get-candlesticks)
#' and [Get candlesticks history](https://www.okx.com/docs-v5/en/#rest-api-market-data-get-candlesticks-history).
#'
#' @param api_key Okx API key.
#' @param secret_key Okx API secret key.
#' @param passphrase Okx API passphrase.
#' @param before POSIXct type. Return records newer than \code{before}.
#' @param after POSIXct type. Return records earlier than \code{after}.
#' @param instId Instrument ID, e.g. BTC-USDT-SWAP.
#' @param bar Bar size, the default is 1m, e.g. 1m/3m/5m/15m/30m/1H/2H/4H, Hong Kong time opening price k-line: 6H/12H/1D/2D/3D.
#' @param ... Other request parameters to be passed, See [Get candlesticks history](https://www.okx.com/docs-v5/en/#rest-api-market-data-get-candlesticks-history) for more information.
#'
#' @return Candlestick charts data
#'
#' @examples
#' \dontrun{
#' candles <- get_history_candles(
#'   api_key, secret_key, passphrase, bar = "1m",
#'   count = 24*60, instId = "CFX-USDT-SWAP"
#' )
#' }
#'
#' @import data.table
#' @export
get_history_candles <- function(
    api_key, secret_key, passphrase, instId, before, after = Sys.time(),
    bar = c("1m", "3m", "5m", "15m", "30m", "1H", "4H", "6H", "12H", "1D", "2D", "3D"), ...
) {
  bar <- match.arg(bar)

  market <- restAPImarket$new(api_key, secret_key, passphrase)

  before <- time2ts(before)
  after <- time2ts(after)
  period <- str2period(bar)

  count <- (after - before + 1) / period

  end <- after
  start <- end - ifelse(count >= 100, 100, count) * period + 0.001

  dat <- list()
  for (i in 1:ceiling(count / 100)) {
    retry <- 0
    result <- NULL
    while (is.null(result) && retry < 5) {
      tryCatch({
        if (i == 1) {
          result <- market$candles(
            instId = instId, before = as.character(1000*start - 1),
            after = as.character(1000*end + 1), bar = bar, ...
          )
        } else {
          result <- market$history_candles(
            instId = instId, before = as.character(1000*start - 1),
            after = as.character(1000*end + 1), bar = bar, ...
          )
        }
      }, error = function(e) {
        retry <- retry + 1
        message(paste0("Error occurred, retrying (", retry, ")...\n"))
        Sys.sleep(10)
      })
    }

    if (result$code == "0") {
      dat <- c(dat, result$data)
      message("From ", ts2time(start), " to ", ts2time(end), " complete")
      count <- count - 100
      end <- start - 0.001
      start <- end - ifelse(count >= 100, 100, count) * period + 0.001
      # Sys.sleep(1/20)
    }
  }
  col_names <- c("ts", "open", "high", "low", "close", "vol", "volCcy", "volCcyQuote", "confirm")
  dat <- lapply(dat, as.data.frame, col.names = c(col_names))
  dat <- data.table::rbindlist(dat)
  dat$ts <- ts2time(dat$ts)
  to_numeric <- c("open", "high", "low", "close", "vol", "volCcy", "volCcyQuote")
  dat[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
  dat
}
