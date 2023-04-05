#' @export
websocketAPIpublic <- R6::R6Class(
  "websocketAPIpublic",
  public = list(
    channel = "wss://ws.okx.com:8443/ws/v5/public",
    simulate = NA,
    ws = NA,
    initialize = function(simulate = FALSE) {
      self$simulate <- simulate
      if (simulate) self$channel <- "wss://wspap.okx.com:8443/ws/v5/public?brokerId=9999"
      self$ws <- websocket::WebSocket$new(self$channel, autoConnect = FALSE)
    },
    connect = function() {
      self$ws$onOpen(function(event) {
        message("Connection opened")
      })

      self$ws$onMessage(function(event) {
        message("Client got msg: ", event$data)
      })

      self$ws$onClose(function(event) {
        message("Client disconnected with code ", event$code, ": ", event$reason)
      })

      self$ws$onError(function(event) {
        cat("Client failed to connect: ", event$message,)
      })

      self$ws$connect()
    },
    on_open = function(...) {
      self$ws$onOpen(...)
    },
    on_close = function(...) {
      self$ws$onClose(...)
    },
    on_message = function(...) {
      self$ws$onMessage(...)
    },
    on_error = function(...) {
      self$ws$onError(...)
    },
    send = function(msg) {
      self$ws$send(msg)
    },
    close = function() {
      self$ws$close()
    }
  )
)

