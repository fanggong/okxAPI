restAPI <- R6::R6Class(
  "restAPI",
  public = list(
    url = "https://www.okx.com",
    api_key = NA,
    secret_key = NA,
    passphrase = NA,
    simulate = NA,
    initialize = function(api_key, secret_key, passphrase, simulate=FALSE) {
      self$api_key <- api_key
      self$secret_key <- secret_key
      self$passphrase <- passphrase
      self$simulate <- simulate
    },
    get_timestamp = function() {
      timestamp <- as.numeric(Sys.time()) * 1000
      timestamp <- as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")
      timestamp <- format(timestamp, format = "%Y-%m-%dT%H:%M:%OS3Z", usetz = FALSE)
      timestamp
    },
    get_request_path = function(api, ...) {
      tmp <- list(...)
      tmp <- tmp[!is.na(tmp)]
      request_path <- paste0(api, "?")
      for (i in 1:length(tmp)) {
        tmp[[i]] <- paste0(names(tmp)[i], "=", paste(tmp[[i]], collapse = ","))
      }
      tmp <- paste(unlist(tmp), collapse = "&")
      request_path <- paste0(api, "?", tmp)
      request_path
    },
    get_message = function(timestamp, request_path) {
      paste0(timestamp, "GET", request_path)
    },
    get_signature = function(secret_key, msg) {
      base64enc::base64encode(digest::hmac(
        secret_key, msg, algo = "sha256", raw = TRUE
      ))
    },
    get_header = function(timestamp, msg) {
      signature <- self$get_signature(self$secret_key, msg)
      headers <- c(
        "OK-ACCESS-KEY" = self$api_key,
        "OK-ACCESS-SIGN" = signature,
        "OK-ACCESS-TIMESTAMP" = timestamp,
        "OK-ACCESS-PASSPHRASE" = self$passphrase,
        "Content-Type" = "application/json"
      )
      if (self$simulate) {
        headers <- c(headers, "x-simulated-trading" = "1")
      }
      headers
    },
    get_result = function(response, process = "identity") {
      result <- httr::content(response)
      if (httr::http_error(response)) {
        message("Error code ", result$code, ": ", result$msg)
      } else {
        return(do.call(process, list(result$data)))
      }
    }
  )
)


#' @export
restAPIaccount <- R6::R6Class(
  "restAPIaccount",
  inherit = restAPI,
  public = list(
    balance = function(ccy, process = "identity") {
      api <- "/api/v5/account/balance"
      timestamp <- self$get_timestamp()
      request_path <- self$get_request_path(api = api, ccy = ccy)
      msg <- self$get_message(timestamp, request_path)
      response <- httr::GET(
        paste0(self$url, request_path),
        httr::add_headers(.headers = self$get_header(timestamp, msg))
      )
      self$get_result(response, process = process)
    },
    positions = function(
      inst_type = c("MARGIN", "SWAP", "FUTURES", "OPTION"),
      inst_id = NA, pos_id = NA, process = "identity"
    ) {
      inst_type <- match.arg(inst_type)
      api <- "/api/v5/account/positions"
      timestamp <- self$get_timestamp()
      request_path <- self$get_request_path(
        api, instType = inst_type, instId = inst_id, posId = pos_id
      )
      msg <- self$get_message(timestamp, request_path)
      response <- httr::GET(
        paste0(self$url, request_path),
        httr::add_headers(.headers = self$get_header(timestamp, msg))
      )
      self$get_result(response, process = process)
    }
  )
)

#' @export
restAPImarket <- R6::R6Class(
  "restAPImarket",
  inherit = restAPI,
  public = list(
    books = function(inst_id, sz = NA, process = "identity") {
      api <- "/api/v5/market/books"
      timestamp <- self$get_timestamp()
      request_path <- self$get_request_path(
        api, instId = inst_id, sz = sz
      )
      msg <- self$get_message(timestamp, request_path)
      response <- httr::GET(
        paste0(self$url, request_path),
        httr::add_headers(.headers = self$get_header(timestamp, msg))
      )
      self$get_result(response, process = process)
    }
  )
)
