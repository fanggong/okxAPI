# okxAPI

An unofficial wrapper for [Okx exchange v5 API](https://www.okx.com/docs-v5/en/)

## Installation

You can install the released version of okxAPI from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("okxAPI")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fanggong/okxAPI")
```

## Requirements

Please refer to [okx my api page](https://www.okx.com/account/my-api) regarding V5 API Key creation.

## Example

### REST API

You can implement all REST API requests in Okx exchange by inheriting the `restAPI` class. For example, to implement [Get currencies](https://www.okx.com/docs-v5/en/#rest-api-funding-get-currencies), you can define the class and call the method as follows:

``` r
myRestAPI <- R6::R6Class(
  inherit = restAPI,
  public = list(
    get_currencies = function(ccy, process = "identity") {
      self$get_result(
        api = "/api/v5/asset/currencies", method = "GET", process = process,
        ccy = ccy
      )
    }
  )
)
tmp <- myRestAPI$new(api_key, secret_key, passphrase)
tmp$get_currencies("BTC")
```

For commonly used interfaces, they have been implemented in this package. The naming convention is as follows: for "/api/v5/AAA/BBB", a new class named `restAPIAAA`, which inherits from `restAPI`, has been defined in this package. The `BBB` method in this new class is used to call the API.

### WebSocket API Public Channel

A `websocketAPIpublic` class based on [websocket package](https://github.com/rstudio/websocket) is used here.

Create new `websocketAPIpublic` object and initiate the connection to the server.

``` r
tmp <- websocketAPIpublic$new()
tmp$connect()
```

Subscribe BTC-USDT-SWAP 5m candlesticks data.

``` r
msg <- list(
  op = "subscribe",
  args = list(
    list(channel = "candle5m", instId = "BTC-USDT-SWAP")
  )
)
msg <- jsonlite::toJSON(msg, auto_unbox = TRUE, pretty = TRUE)
tmp$send(msg)
```

Pass your own callback function

``` r
tmp$on_message(function(event) {
  if (event$data == "pong") {
    cat("Bingo!!")
  }
})
tmp$send("ping")
```

Close the connection.

``` r
tmp$close()
```

### WebSocket API Private Channel

A `websocketAPIprivate` class based on [websocket package](https://github.com/rstudio/websocket) is used here.

Create new `websocketAPIprivate` object and initiate the connection to the server.

``` r
tmp <- websocketAPIprivate$new(api_key, secret_key, passphrase)
tmp$connect()
Sys.sleep(1)  # Waiting for connection success
tmp$login()
```

Subscribe account information.

``` r
msg <- list(
  op = "subscribe",
  args = list(
    list(channel = "account", ccy = "USDT")
  )
)
msg <- jsonlite::toJSON(msg, auto_unbox = TRUE, pretty = TRUE)
tmp$send(msg)
```

Pass your own callback function.

``` r
tmp$on_message(function(event) {
  if (event$data == "pong") {
    cat("Bingo!!")
  }
})
tmp$send("ping")
```

Close the connection.

``` r
tmp$close()
```
