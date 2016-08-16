#' Function to fix the rate limit problem
#'

fix_rate_limit <- function(){
  limit1 <- getCurRateLimitInfo()[44, 3]
  limit2 <- getCurRateLimitInfo()[39, 3]
  limit3 <- getCurRateLimitInfo()[11, 3]
  limit4 <- getCurRateLimitInfo()[53, 3]
  limit5 <- getCurRateLimitInfo()[56, 3]

  while (limit1 == "0"|
         as.numeric(limit2) < 30|
         limit3 == "0"|
         limit4 == "0"|
         limit5 == "0") {
    Sys.sleep(60)
    limit1 <- getCurRateLimitInfo()[44, 3]
    limit2 <- getCurRateLimitInfo()[39, 3]
    limit3 <- getCurRateLimitInfo()[11, 3]
    limit4 <- getCurRateLimitInfo()[53, 3]
    limit5 <- getCurRateLimitInfo()[56, 3]
  }
}
