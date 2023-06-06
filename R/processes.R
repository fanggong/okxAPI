
#' @export
process_market_books <- function(response) {
  bids <- response$data[[1]]$bids
  bids <- lapply(bids, data.table::as.data.table)
  bids <- data.table::rbindlist(bids)
  names(bids) <- c("price", "sz", "nonuse", "order_number")
  bids[, `:=`(direction = "bid")]

  asks <- response$data[[1]]$asks
  asks <- lapply(asks, data.table::as.data.table)
  asks <- data.table::rbindlist(asks)
  names(asks) <- c("price", "sz", "nonuse", "order_number")
  asks[, `:=`(direction = "ask")]

  res <- data.table::rbindlist(list(bids, asks))
  res <- res[, `:=`(ts = ts2time(response$data[[1]]$ts))]

  to_numeric <- c("sz", "price", "order_number")
  res[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]

  res[order(price, decreasing = TRUE), .(ts, price, sz, order_number, direction)]
}


#' @export
process_standard <- function(response) {
  res <- response$data
  res <- lapply(res, data.table::as.data.table)
  res <- data.table::rbindlist(res)

  to_numeric <- numeric_fields()[numeric_fields() %in% names(res)]
  if (length(to_numeric) > 0) {
    res[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
  }

  to_time <- time_fields()[time_fields() %in% names(res)]
  if (length(to_time) > 0) {
    res[, (to_time) := lapply(.SD, ts2time), .SDcols = to_time]
  }
}
