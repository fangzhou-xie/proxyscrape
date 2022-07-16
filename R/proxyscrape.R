# main functions

#' scrape urls with proxy (Tor as default)
#'
#' @param urls, character vector of urls
#' @param proxy, character, use default Tor proxy
#' @param port, int (default to 9050), use default Tor port 9050
#' @param max_retry, int (default 5), how many times to retry on failure
#' @param useragent, character (default NULL), overwrite useragent if supplied
#' @param random_ua, bool (default TRUE), random user agent by Randomuseragent::random_useragent()
#' @export
scrape <- function(urls, proxy = "socks5://127.0.0.1", port = 9050, max_retry = 3, useragent = NULL, random_ua = TRUE) {

  # async processing by default

  # init
  try <- 1
  urls_remain <- urls

  if (is.null(useragent)) {
    if (random_ua) {
      ua <- Randomuseragent::random_useragent()
    } else {
      ua <- NULL
    }
  } else {
    ua <- useragent
  }
  
  while (try <= max_retry & length(urls_remain) > 0)  {
    
    if (try == 1) {
      # first try
      fail_lgl <- rep(TRUE, length(urls))
      
      # urls to reqs
      # alternative: purrr::map(urls, function(x) url2req(x, proxy, port, ua))
      reqs <- unname(Map(function(x) url2req(x, proxy, port, ua), urls_remain))
      resps <- httr2::multi_req_perform(reqs)
      
      # get bool vector of success/fail responses
      fail_lgl <- unlist(Map(function(x) inherits(x, "error"), resps), FALSE, FALSE)
      
    } else {
      # urls to reqs
      reqs1 <- unname(Map(function(x) url2req(x, proxy, port, ua), urls_remain))
      resps1 <- httr2::multi_req_perform(reqs1)
      
      fail_ids <- which(fail_lgl)
      Map(function(i) {
        resps[[fail_ids[i]]] <- resps1[[i]]
      }, 1:length(fail_ids))  
      
      # get bool vector of success/fail responses
      fail1 <- unlist(Map(function(x) inherits(x, "error"), resps), FALSE, FALSE)
      fail_lgl <- as.logical(fail_lgl * fail1)
    }
    
    urls_remain <- urls[fail_lgl]

    if (length(urls_remain) == 0) break
    
    try <- try + 1
  }

  resps
}

url2req <- function(url, proxy, port, ua) {
  # url, proxy and port (default Tor), ua (useragent string)

  req <- httr2::req_proxy(httr2::request(url), proxy, port)
  req <- httr2::req_headers(req, `Cache-Control` = "no-store, no-cache, max-age=10, must-revalidate, proxy-revalidate")
  req$options$maxage_conn <- 10

  if (!is.null(ua)) {
    req <- httr2::req_user_agent(req, ua)
  }
  req
}
