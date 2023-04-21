#' @title websocketAPIpublic Class
#'
#' @description Public channel of WebSocket API for [Okx exchange v5 API](https://www.okx.com/docs-v5/en/).
#' See [Public Channel](https://www.okx.com/docs-v5/en/#websocket-api-public-channel) for more information.
#'
#' @examples
#' \dontrun{
#' tmp <- websocketAPIpublic$new()
#' tmp$connect()
#'
#' # subscribe BTC-USDT-SWAP 5m candlesticks data
#' msg <- list(
#'   op = "unsubscribe",
#'   args = list(
#'     list(channel = "candle5m", instId = "BTC-USDT-SWAP")
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
#' @import R6 websocket
#' @export
websocketAPIpublic <- R6::R6Class(
  "websocketAPIpublic",
  public = list(
    #' @field channel Public WebSocket url.
    channel = "wss://ws.okx.com:8443/ws/v5/public",
    #' @field simulate Whether to use demo trading service.
    simulate = NA,
    #' @field ws A websocket::WebSocket object to establish a connection to the server.
    ws = NA,
    #' @description Create a new websocketAPIpublic object.
    #' @param simulate Whether to use demo trading service.
    initialize = function(simulate = FALSE) {
      self$simulate <- simulate
      if (simulate) self$channel <- "wss://wspap.okx.com:8443/ws/v5/public?brokerId=9999"
      self$ws <- websocket::WebSocket$new(self$channel, autoConnect = FALSE)
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

