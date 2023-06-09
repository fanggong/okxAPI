% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restAPI.R
\name{restAPItrade}
\alias{restAPItrade}
\title{restAPItrade Class}
\description{
Wrapper for \href{https://www.okx.com/docs-v5/en/#rest-api-trade}{REST API TRADE}.
}
\section{Super class}{
\code{\link[okxAPI:restAPI]{okxAPI::restAPI}} -> \code{restAPItrade}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-restAPItrade-order}{\code{restAPItrade$order()}}
\item \href{#method-restAPItrade-cancel_order}{\code{restAPItrade$cancel_order()}}
\item \href{#method-restAPItrade-orders}{\code{restAPItrade$orders()}}
\item \href{#method-restAPItrade-orders_pending}{\code{restAPItrade$orders_pending()}}
\item \href{#method-restAPItrade-orders_algo_pending}{\code{restAPItrade$orders_algo_pending()}}
\item \href{#method-restAPItrade-clone}{\code{restAPItrade$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_body"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_body'><code>okxAPI::restAPI$get_body()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_header"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_header'><code>okxAPI::restAPI$get_header()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_message"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_message'><code>okxAPI::restAPI$get_message()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_request_path"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_request_path'><code>okxAPI::restAPI$get_request_path()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_result"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_result'><code>okxAPI::restAPI$get_result()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_signature"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_signature'><code>okxAPI::restAPI$get_signature()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="get_timestamp"><a href='../../okxAPI/html/restAPI.html#method-restAPI-get_timestamp'><code>okxAPI::restAPI$get_timestamp()</code></a></span></li>
<li><span class="pkg-link" data-pkg="okxAPI" data-topic="restAPI" data-id="initialize"><a href='../../okxAPI/html/restAPI.html#method-restAPI-initialize'><code>okxAPI::restAPI$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-order"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-order}{}}}
\subsection{Method \code{order()}}{
See \href{https://www.okx.com/docs-v5/en/#rest-api-trade-place-order}{Place order} for more information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$order(
  instId,
  tdMode = c("isolated", "cross", "cash"),
  side = c("buy", "sell"),
  sz,
  ordType = c("market", "limit", "post_only", "fok", "ioc", "optimal_limit_ioc"),
  process = "identity",
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{instId}}{Instrument ID, e.g. BTC-USD-190927-5000-C.}

\item{\code{tdMode}}{Trade mode. Margin mode: \code{cross} or \code{isolated.} Non-Margin mode: \code{cash}.}

\item{\code{side}}{Order side, \code{buy} or \code{sell}.}

\item{\code{sz}}{Quantity to buy or sell.}

\item{\code{ordType}}{Order type. \code{market}: Market order, \code{limit}: Limit order, \code{post_only}: Post-only order,
\code{fok}: Fill-or-kill order, \code{ioc}: Immediate-or-cancel order,
\code{optimal_limit_ioc}: Market order with immediate-or-cancel order (applicable only to Futures and Perpetual swap).}

\item{\code{process}}{A function to process the data received from the API. Default to \code{identity}.}

\item{\code{...}}{Other request parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-cancel_order"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-cancel_order}{}}}
\subsection{Method \code{cancel_order()}}{
See \href{https://www.okx.com/docs-v5/en/#rest-api-trade-cancel-order}{Cancel order} for more information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$cancel_order(instId, ordId, clOrdId, process = "identity", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{instId}}{Instrument ID, e.g. BTC-USD-190927.}

\item{\code{ordId}}{Order ID, Either \code{ordId} or \code{clOrdId} is required. If both are passed, \code{ordId} will be used.}

\item{\code{clOrdId}}{Client Order ID as assigned by the client.}

\item{\code{process}}{A function to process the data received from the API. Default to \code{identity}.}

\item{\code{...}}{Other request parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-orders"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-orders}{}}}
\subsection{Method \code{orders()}}{
See \href{https://www.okx.com/docs-v5/en/#rest-api-trade-get-order-details}{Get order details} for more information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$orders(process = "identity", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{process}}{A function to process the data received from the API. Default to \code{identity}.}

\item{\code{...}}{Other request parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-orders_pending"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-orders_pending}{}}}
\subsection{Method \code{orders_pending()}}{
See \href{https://www.okx.com/docs-v5/en/#rest-api-trade-get-order-list}{Get order List} for more information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$orders_pending(process = "identity", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{process}}{A function to process the data received from the API. Default to \code{identity}.}

\item{\code{...}}{Other request parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-orders_algo_pending"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-orders_algo_pending}{}}}
\subsection{Method \code{orders_algo_pending()}}{
See \href{https://www.okx.com/docs-v5/en/#rest-api-trade-get-algo-order-list}{Get algo order list} for more information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$orders_algo_pending(process = "identity", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{process}}{A function to process the data received from the API. Default to \code{identity}.}

\item{\code{...}}{Other request parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restAPItrade-clone"></a>}}
\if{latex}{\out{\hypertarget{method-restAPItrade-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{restAPItrade$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
