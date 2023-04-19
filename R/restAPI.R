#' @title restAPI Class
#'
#' @description Base class for [Okx exchange v5 API](https://www.okx.com/docs-v5/en/).
#'
#' @details You can implement all REST API requests in Okx exchange by inheriting the \code{restAPI} class.
#' Please refer to the example provided at the end of the document.
#'
#' For commonly used interfaces, they have been implemented in this package.
#' The naming convention is as follows: for "/api/v5/AAA/BBB", a new class named \code{restAPIAAA},
#' which inherits from \code{restAPI}, has been defined in this package.
#' The \code{BBB} method in this new class is used to call the API.
#'
#' @examples
#' \dontrun{
#' # for [Get currencies](https://www.okx.com/docs-v5/en/#rest-api-funding-get-currencies)
#' # you can define the class like this
#' myRestAPI <- R6::R6Class(
#'   inherit = restAPI,
#'   public = list(
#'     get_currencies = function(ccy, process = "identity") {
#'       self$get_result(
#'         api = "/api/v5/asset/currencies", method = "GET", process = process,
#'         ccy = ccy
#'       )
#'     }
#'   )
#' )
#' # And call it like this
#' tmp <- myRestAPI$new(api_key, secret_key, passphrase)
#' tmp$get_currencies("BTC")
#' }
#'
#' @seealso
#' \code{\link{restAPItrade}}
#' \code{\link{restAPIaccount}}
#' \code{\link{restAPImarket}}
#'
#' @import R6 httr jsonlite base64enc base64enc
#' @export
restAPI <- R6::R6Class(
  "restAPI",
  public = list(
    #' @field url Okx REST API url, which is https://www.okx.com.
    url = "https://www.okx.com",
    #' @field api_key Okx API key.
    api_key = NA,
    #' @field secret_key Okx API secret key.
    secret_key = NA,
    #' @field passphrase Okx API passphrase.
    passphrase = NA,
    #' @field simulate Whether to use demo trading service.
    simulate = NA,
    #' @description Create a new REST API object.
    #' @param api_key Okx API key.
    #' @param secret_key Okx API secret key.
    #' @param passphrase Okx API passphrase.
    #' @param simulate Whether to use demo trading service.
    initialize = function(api_key, secret_key, passphrase, simulate = FALSE) {
      self$api_key <- api_key
      self$secret_key <- secret_key
      self$passphrase <- passphrase
      self$simulate <- simulate
    },
    #' @description Get UTC timestamp.
    get_timestamp = function() {
      timestamp <- as.numeric(Sys.time()) * 1000
      timestamp <- as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")
      timestamp <- format(timestamp, format = "%Y-%m-%dT%H:%M:%OS3Z", usetz = FALSE)
      timestamp
    },
    #' @description Get request path.
    #' @param api Request api e.g. /api/v5/account/positions-history.
    #' @param method Request method, \code{GET} or \code{POST}.
    #' @param ... Other request parameters.
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
    #' @description Get request body.
    #' @param method Request method, \code{GET} or \code{POST}.
    #' @param ... Other request parameters.
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
    #' @description Get the signing messages.
    #' @param timestamp Retrieve through method \code{get_timestamp}.
    #' @param request_path Retrieve through method \code{get_request_path}.
    #' @param body Retrieve through method \code{get_body}.
    #' @param method Request method, \code{GET} or \code{POST}.
    get_message = function(timestamp, request_path, body, method = c("GET", "POST")) {
      method <- match.arg(method)
      paste0(timestamp, method, request_path, body)
    },
    #' @description Get the signature.
    #' @param msg Retrieve through method \code{get_message}.
    #' @param secret_key Okx API secret key.
    get_signature = function(msg, secret_key = self$secret_key) {
      base64enc::base64encode(base64enc::hmac(secret_key, msg, algo = "sha256", raw = TRUE))
    },
    #' @description Get request headers.
    #' @param timestamp Retrieve through method \code{get_timestamp}.
    #' @param msg Retrieve through method \code{get_message}.
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
    #' @description Retrieve data from api.
    #' @param api Request api e.g. /api/v5/account/positions-history.
    #' @param method Request method, \code{GET} or \code{POST}.
    #' @param process A function to process the data received from the API.
    #' @param ... Other request parameters.
    get_result = function(api, method = c("GET", "POST"), process, ...) {
      method <- match.arg(method)
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


#' @title restAPItrade Class
#'
#' @description Wrapper for [REST API TRADE](https://www.okx.com/docs-v5/en/#rest-api-trade).
#'
#' @import R6
#' @export
restAPItrade <- R6::R6Class(
  "restAPItrade",
  inherit = restAPI,
  public = list(
    #' @description See [Place order](https://www.okx.com/docs-v5/en/#rest-api-trade-place-order) for more information.
    #' @param instId Instrument ID, e.g. BTC-USD-190927-5000-C.
    #' @param tdMode Trade mode. Margin mode: \code{cross} or \code{isolated.} Non-Margin mode: \code{cash}.
    #' @param side Order side, \code{buy} or \code{sell}.
    #' @param sz Quantity to buy or sell.
    #' @param ordType Order type. \code{market}: Market order, \code{limit}: Limit order, \code{post_only}: Post-only order,
    #' \code{fok}: Fill-or-kill order, \code{ioc}: Immediate-or-cancel order,
    #' \code{optimal_limit_ioc}: Market order with immediate-or-cancel order (applicable only to Futures and Perpetual swap).
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
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
    #' @description See [Cancel order](https://www.okx.com/docs-v5/en/#rest-api-trade-cancel-order) for more information.
    #' @param instId Instrument ID, e.g. BTC-USD-190927.
    #' @param ordId Order ID, Either \code{ordId} or \code{clOrdId} is required. If both are passed, \code{ordId} will be used.
    #' @param clOrdId Client Order ID as assigned by the client.
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
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

#' @title restAPIaccount Class
#'
#' @description Wrapper for [REST API ACCOUNT](https://www.okx.com/docs-v5/en/#rest-api-account).
#'
#' @import R6
#' @export
restAPIaccount <- R6::R6Class(
  "restAPIaccount",
  inherit = restAPI,
  public = list(
    #' @description See [Get balance](https://www.okx.com/docs-v5/en/#rest-api-account-get-balance) for more information.
    #' @param ccy Single currency or a vector composed of multiple currencies. (no more than 20).
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    balance = function(ccy, process = "identity") {
      self$get_result(
        api = "/api/v5/account/balance", method = "GET", process = process,
        ccy = ccy
      )
    },
    #' @description See [Get positions](https://www.okx.com/docs-v5/en/#rest-api-account-get-positions) for more information.
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
    positions = function(process = "identity", ...) {
      self$get_result(
        api = "/api/v5/account/positions", method = "GET", process = process,
        ...
      )
    },
    #' @description See [Get positions history](https://www.okx.com/docs-v5/en/#rest-api-account-get-positions-history) for more information.
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
    positions_history = function(process = "identity", ...) {
      self$get_result(
        api = "/api/v5/account/positions-history", method = "GET", process = process,
        ...
      )
    }
  )
)


#' @title restAPImarket Class
#'
#' @description Wrapper for [REST API MARKET](https://www.okx.com/docs-v5/en/#rest-api-market-data).
#'
#' @import R6
#' @export
restAPImarket <- R6::R6Class(
  "restAPImarket",
  inherit = restAPI,
  public = list(
    #' @description See [Get candlesticks](https://www.okx.com/docs-v5/en/#rest-api-market-data-get-candlesticks) for more information.
    #' @param instId Instrument ID, e.g. BTC-USD-190927-5000-C.
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
    candles = function(instId, process = "identity", ...) {
      self$get_result(
        api = "/api/v5/market/candles", method = "GET", process = process,
        instId = instId, ...
      )
    },
    #' @description See [Get candlesticks history](https://www.okx.com/docs-v5/en/#rest-api-market-data-get-candlesticks-history) for more information.
    #' @param instId Instrument ID, e.g. BTC-USD-190927-5000-C.
    #' @param process A function to process the data received from the API. Default to \code{identity}.
    #' @param ... Other request parameters.
    history_candles = function(instId, process = "identity", ...) {
      self$get_result(
        api = "/api/v5/market/history-candles", method = "GET", process = process,
        instId = instId, ...
      )
    }
  )
)
