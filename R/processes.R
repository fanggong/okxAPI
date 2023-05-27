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
