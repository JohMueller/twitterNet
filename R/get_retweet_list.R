#' @title  Get retweet-lists for mutliple users
#'
#' @description This function scrapes all retweets in a user's timeline for a list of twitter users.
#'
#' Note that this may take a while due to the Twitter API rate limits...
#'
#' @param user_list  chr-vector or -list containing multiple Twitter user names
#'
#' @return Returns a list of character vectors which contain the retweeted accounts for all users from the user_list
#' @export
#'
#' @import twitteR
#' @import stringr
#'
#' @examples
#' # Setup a retweet matrix
#'  user_list <- c("hadleywickham", "joh_jo_mueller")
#'  get_retweet_list(user_list)



###Step 3: Functions to  scrape the retweet list for a list of user

get_retweet_list <- function(user_list,
                             limit = 2400,
                             date_begin = "2006-01-01",
                             date_end = Sys.Date()){

  retweet_list <- list()
  iter <- 0
  total_tweets <- 0

  for (user in user_list){
    timeline <- userTimeline(user, n=limit, includeRts=T)
    user1 <- do.call("rbind", lapply(timeline, as.data.frame))
    user1 <- user1[which(user1$isRetweet ==T), ]

    if (!is.null(user1)){
      user1 <- user1[which(as.Date(user1$created) >= as.Date(date_begin)&
                             as.Date(user1$created) <= as.Date(date_end)), ] ### Created after 1.Feburary
    }

    user1$retweets <- str_extract(user1$text, "((@)\\w+){1}") ### extract only the mentions
    user_retweets <- paste(user1$retweets, collapse = ' ')

    retweet_list <- c(retweet_list, user_retweets)

    fix_rate_limit()

    total_tweets <- total_tweets + length(user1$text)
    iter <- iter + 1
    message('Processing user ', iter, ' of ', length(user_list), ' at ',
            Sys.time(), '; Rate Limit Info (Remaining): ', getCurRateLimitInfo()[39, 3])
  }

  print(paste0("The total number of tweets processed is ", total_tweets))
  retweet_list
}
