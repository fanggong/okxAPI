#' @export
get_positions_history <- function(
  api_key, secret_key, passphrase, forward = 90, period = 10, ...
) {
  account <- restAPIaccount$new(api_key, secret_key, passphrase)
  now <- as.integer(Sys.time())
  start <- now - forward*24*60*60
  end <- start + period*24*60*60 - 1
  dat <- list()
  while (start < now) {
    result <- account$positions_history(
      before = as.character(1000*start), after = as.character(1000*end), ...
    )
    if (result$code == "0") {
      dat <- c(dat, result$data)
      start_time <- as.POSIXct(start, origin = "1970-01-01 00:00:00", tz = "Asia/Shanghai")
      end_time <- as.POSIXct(end, origin = "1970-01-01 00:00:00", tz = "Asia/Shanghai")
      message("From ", start_time, " to ", end_time, " complete")
      start <- end + 1
      end <- start + period*24*60*60 - 1
      Sys.sleep(15)
    }
  }
  dat <- lapply(dat, data.table::as.data.table)
  dat <- data.table::rbindlist(dat)
  dat$cTime <- as.POSIXct(as.numeric(dat$cTime)/1000, origin = "1970-01-01 00:00:00", tz = "Asia/Shanghai")
  dat$uTime <- as.POSIXct(as.numeric(dat$uTime)/1000, origin = "1970-01-01 00:00:00", tz = "Asia/Shanghai")
  to_numeric <- c("closeAvgPx", "closeTotalPos", "lever", "openAvgPx", "openMaxPos",
                  "pnl", "pnlRatio")
  dat[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
  dat
}


