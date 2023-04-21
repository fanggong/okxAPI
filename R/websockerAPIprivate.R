#' @title websocketAPIprivate Class
#'
#' @description Private channel of WebSocket API for [Okx exchange v5 API](https://www.okx.com/docs-v5/en/).
#' See [Private Channel](https://www.okx.com/docs-v5/en/#websocket-api-private-channel) for more information.
#'
#' @examples
#' \dontrun{
#' tmp <- websocketAPIprivate$new(api_key, secret_key, passphrase)
#' tmp$connect()
#' Sys.sleep(1)
#' tmp$login()
#'
#' # subscribe account information
#' msg <- list(
#'   op = "subscribe",
#'   args = list(
#'     list(channel = "account", ccy = "USDT")
#'   )
#' )
#' msg <- jsonlite::toJSON(msg, auto_unbox = TRUE, pretty = TRUE)
#' tmp$send(msg)
#'
#' # pass your own callback function
#' tmp$on_message(function(event) {
#'   if (event$data == "pong") {
#'     cat("Bingo!!\n")
#'   }
#' })
#' tmp$send("ping")
#'
#' tmp$close()
#' }
#'
#' @import R6 websocket base64enc digest
#' @export
websocketAPIprivate <- R6::R6Class(
  "websockerAPIprivate",
  public = list(
    #' @field channel Private WebSocket url.
    channel = "wss://ws.okx.com:8443/ws/v5/private",
    #' @field api_key Okx API key.
    api_key = NA,
    #' @field secret_key Okx API secret key.
    secret_key = NA,
    #' @field passphrase Okx API passphrase.
    passphrase = NA,
    #' @field simulate Whether to use demo trading service.
    simulate = NA,
    #' @field ws A websocket::WebSocket object to establish a connection to the server.
    ws = NA,
    #' @description Craeate a new websocketAPIprivate object.
    #' @param api_key Okx API key.
    #' @param secret_key Okx API secret key.
    #' @param passphrase Okx API passphrase.
    #' @param simulate Whether to use demo trading service.
    initialize = function(api_key, secret_key, passphrase, simulate = FALSE) {
      self$api_key <- api_key
      self$secret_key <- secret_key
      self$passphrase <- passphrase
      self$simulate <- simulate
      if (simulate) self$channel <- "wss://wspap.okx.com:8443/ws/v5/private?brokerId=9999"
      self$ws <- websocket::WebSocket$new(self$channel, autoConnect = FALSE)
    },
    #' @description Get UTC timestamp.
    get_timestamp = function() {
      as.integer(Sys.time())
    },
    #' @description Get the signing messages.
    #' @param timestamp Retrieve through method \code{get_timestamp}.
    get_message = function(timestamp) {
      paste0(timestamp, "GET", "/users/self/verify")
    },
    #' @description Get the signature.
    #' @param secret_key Okx API secret key.
    #' @param msg Retrieve through method \code{get_message}.
    get_signature = function(secret_key, msg) {
      base64enc::base64encode(digest::hmac(
        secret_key, msg, algo = "sha256", raw = TRUE
      ))
    },
    #' @description Initiate the connection to the server.
    connect = function() {
      self$ws$onOpen(function(event) {
        cat("Connection opened\n")
      })

      self$ws$onMessage(function(event) {
        cat("Client got msg: ", event$data, "\n")
      })

      self$ws$onClose(function(event) {
        cat("Client disconnected with code ", event$code, ": ", event$reason, "\n")
      })

      self$ws$onError(function(event) {
        cat("Client failed to connect: ", event$message, "\n")
      })

      self$ws$connect()
    },
    #' @description Log in.
    login = function() {
      status <- attr(self$ws$readyState(), "description") == "Open"
      if (status) {
        timestamp <- self$get_timestamp()
        msg <- self$get_message(timestamp)
        signature <- self$get_signature(self$secret_key, msg)

        login_msg <- list(
          "op" = "login",
          "args" = list(
            list(
              "apiKey" = self$api_key,
              "passphrase" = self$passphrase,
              "timestamp" = timestamp,
              "sign" = signature
            )
          )
        )
        login_msg <- jsonlite::toJSON(login_msg, auto_unbox = TRUE)
        self$ws$send(login_msg)
      } else {
        stop("Connection is not open")
      }
    },
    #' @description Called when the connection is established.
    #' @param func A Callback function.
    on_open = function(func) {
      self$ws$onOpen(func)
    },
    #' @description Called when a previously-opened connection is closed.
    #' The event will have 'code' (integer) and 'reason' (one-element character)
    #' elements that describe the remote's reason for closing.
    #' @param func A Callback function.
    on_close = function(func) {
      self$ws$onClose(func)
    },
    #' @description Called each time a message is received from the server.
    #' The event will have a 'data' element, which is the message content.
    #' @param func A Callback function.
    on_message = function(func) {
      self$ws$onMessage(func)
    },
    #' @description Called when the connection fails to be established.
    #' The event will have an 'message' element, a character vector of length 1
    #' describing the reason for the error.
    #' @param func A Callback function.
    on_error = function(func) {
      self$ws$onError(func)
    },
    #' @description Send a message to the server.
    #' @param msg Messages.
    send = function(msg) {
      self$ws$send(msg)
    },
    #' @description Close the connection.
    close = function() {
      self$ws$close()
    }
  )
)
