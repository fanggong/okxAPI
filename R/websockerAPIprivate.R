#' @export
websocketAPIprivate <- R6::R6Class(
  "websockerAPIprivate",
  public = list(
    channel = "wss://ws.okx.com:8443/ws/v5/private",
    api_key = NA,
    secret_key = NA,
    passphrase = NA,
    simulate = NA,
    ws = NA,
    initialize = function(api_key, secret_key, passphrase, simulate = FALSE) {
      self$api_key <- api_key
      self$secret_key <- secret_key
      self$passphrase <- passphrase
      self$simulate <- simulate
      if (simulate) self$channel <- "wss://wspap.okx.com:8443/ws/v5/private?brokerId=9999"
      self$ws <- websocket::WebSocket$new(self$channel, autoConnect = FALSE)
    },
    get_timestamp = function() {
      as.integer(Sys.time())
    },
    get_message = function(timestamp) {
      paste0(timestamp, "GET", "/users/self/verify")
    },
    get_signature = function(secret_key, msg) {
      base64enc::base64encode(digest::hmac(
        secret_key, msg, algo = "sha256", raw = TRUE
      ))
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
        stop("Connect is not open")
      }
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
