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
#' @param count Retrieve position data for a specified number of past days, with a maximum of 90(days)
#' @param period Due to the 'Number of results per request' limitation of the API,
#' the \code{period} parameter must be specified to ensure that the number of position data entries within each period does not exceed 100.
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
  api_key, secret_key, passphrase, count = 90, period = 10, ...
) {
  account <- restAPIaccount$new(api_key, secret_key, passphrase)
  now <- time2ts(Sys.time())
  start <- now - count*24*60*60
  end <- start + period*24*60*60 - 0.001
  dat <- list()
  while (start < now) {
    result <- account$positions_history(
      before = as.character(1000*start - 1), after = as.character(1000*end + 1), ...
    )
    if (result$code == "0") {
      dat <- c(dat, result$data)
      message("From ", ts2time(start), " to ", ts2time(end), " complete")
      start <- end + 0.001
      end <- start + period*24*60*60 - 0.001
      Sys.sleep(15)
    }
  }
  dat <- lapply(dat, data.table::as.data.table)
  dat <- data.table::rbindlist(dat)
  to_time <- c("cTime", "uTime")
  dat[, (to_time) := lapply(.SD, ts2time), .SDcols = to_time]
  to_numeric <- c("closeAvgPx", "closeTotalPos", "lever", "openAvgPx", "openMaxPos", "pnl", "pnlRatio")
  dat[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
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
#' @param bar Bar size, the default is 1m, e.g. 1m/3m/5m/15m/30m/1H/2H/4H, Hong Kong time opening price k-line: 6H/12H/1D/2D/3D.
#' @param count Number of Bars.
#' @param instId Instrument ID, e.g. BTC-USDT-SWAP.
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
  api_key, secret_key, passphrase,
  bar = c("1m", "3m", "5m", "15m", "30m", "1H", "4H", "6H", "12H", "1D", "2D", "3D"),
  count, instId, ...
) {
  bar <- match.arg(bar)
  period <- str2period(bar)

  market <- restAPImarket$new(api_key, secret_key, passphrase)

  now <- time2ts(Sys.time())
  end <- now
  start <- end - ifelse(count >= 100, 100, count) * period + 0.001
  dat <- list()
  for (i in 1:ceiling(count / 100)) {
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
