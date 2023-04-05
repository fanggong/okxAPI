restAPI <- R6::R6Class(
  "restAPI",
  public = list(
    url = "https://www.okx.com",
    api_key = NA,
    secret_key = NA,
    passphrase = NA,
    simulate = NA,
    initialize = function(api_key, secret_key, passphrase, simulate = FALSE) {
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
    get_request_path = function(api, method = c("GET", "POST"), ...) {
      method <- match.arg(method)
      if (method == "GET") {
        tmp <- list(...)
        if (length(tmp) == 0) {
          request_path <- api
        } else {
          tmp <- tmp[!is.na(tmp)]
          request_path <- paste0(api, "?")
          for (i in 1:length(tmp)) {
            tmp[[i]] <- paste0(names(tmp)[i], "=", paste(tmp[[i]], collapse = ","))
          }
          tmp <- paste(unlist(tmp), collapse = "&")
          request_path <- paste0(api, "?", tmp)
        }
      } else {
        request_path <- api
      }
      request_path
    },
    get_body = function(method = c("GET", "POST"), ...) {
      method <- match.arg(method)
      if (method == "GET") {
        body <- ""
      } else {
        tmp <- list(...)
        tmp <- tmp[!is.na(tmp)]
        body <- jsonlite::toJSON(tmp, auto_unbox = TRUE)
      }
      body
    },
    get_message = function(timestamp, request_path, body, method = c("GET", "POST")) {
      method <- match.arg(method)
      paste0(timestamp, method, request_path, body)
    },
    get_signature = function(msg, secret_key = self$secret_key) {
      base64enc::base64encode(digest::hmac(secret_key, msg, algo = "sha256", raw = TRUE))
    },
    get_header = function(timestamp, msg) {
      headers <- c(
        "OK-ACCESS-KEY" = self$api_key,
        "OK-ACCESS-SIGN" = self$get_signature(msg),
        "OK-ACCESS-TIMESTAMP" = timestamp,
        "OK-ACCESS-PASSPHRASE" = self$passphrase,
        "Content-Type" = "application/json"
      )
      if (self$simulate) {
        headers <- c(headers, "x-simulated-trading" = "1")
      }
      headers
    },
    get_result = function(api, method = c("GET", "POST"), process, ...) {
      timestamp <- self$get_timestamp()
      request_path <- self$get_request_path(api = api, method = method, ...)
      body <- self$get_body(method = method, ...)
      msg <- self$get_message(timestamp, request_path, body, method)
      if (method == "GET") {
        response <- httr::GET(
          paste0(self$url, request_path),
          httr::add_headers(.headers = self$get_header(timestamp, msg))
        )
      } else {
        response <- httr::POST(
          paste0(self$url, request_path),
          httr::add_headers(.headers = self$get_header(timestamp, msg)),
          body = body
        )
      }
      result <- httr::content(response)
      if (httr::http_error(response)) {
        message("Error code ", result$code, ": ", result$msg)
      } else if (result$code != "0") {
        message("Error code ", result$data[[1]]$sCode, ": ", result$data[[1]]$sMsg)
      } else {
        return(do.call(process, list(result)))
      }
    }
  )
)

#' @export
restAPItrade <- R6::R6Class(
  "restAPItrade",
  inherit = restAPI,
  public = list(
    order  = function(
      instId, tdMode = c("isolated", "cross", "cash"), side = c("buy", "sell"), sz,
      ordType = c("market", "limit", "post_only", "fok", "ioc", "optimal_limit_ioc"),
      process = "identity", ...
    ) {
      tdMode <- match.arg(tdMode)
      side <- match.arg(side)
      ordType <- match.arg(ordType)

      self$get_result(
        api = "/api/v5/trade/order", method = "POST", process = process,
        instId = instId, tdMode = tdMode, side = side, sz = sz, ordType = ordType, ...
      )
    },
    # 撤单返回sCode等于0不能严格认为该订单已经被撤销
    # 只表示您的撤单请求被系统服务器所接受
    # 撤单结果以订单频道推送的状态或者查询订单状态为准
    cancel_order = function(instId, ordId, clOrdId, process = "identity", ...) {
      if (missing(ordId)) ordId <- NA
      if (missing(clOrdId)) clOrdId <- NA
      self$get_result(
        api = "/api/v5/trade/cancel-order", method = "POST", process = process,
        instId = instId, ordId = ordId, clOrdId = clOrdId, ...
      )
    }
  )
)


#' @export
restAPIaccount <- R6::R6Class(
  "restAPIaccount",
  inherit = restAPI,
  public = list(
    balance = function(ccy, process = "identity") {
      self$get_result(
        api = "/api/v5/account/balance", method = "GET", process = process,
        ccy = ccy
      )
    },
    positions = function(process = "identity", ...) {
      self$get_result(
        api = "/api/v5/account/positions", method = "GET", process = process,
        ...
      )
    },
    positions_history = function(process = "identity", ...) {
      self$get_result(
        api = "/api/v5/account/positions-history", method = "GET", process = process,
        ...
      )
    }
  )
)


#' @export
restAPImarket <- R6::R6Class(
  "restAPImarket",
  inherit = restAPI,
  public = list(
    books = function(instId, process = "identity", ...) {
      self$get_result(
        api = "/api/v5/market/books", method = "GET", process = process,
        instId = instId, ...
      )
    }
  )
)

