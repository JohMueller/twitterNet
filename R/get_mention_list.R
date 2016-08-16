#' @title  Get mention-lists for mutliple users
#'
#' @description This function scrapes all mentions in a user's timeline for a list of twitter users.
#'
#' Note that this may take a while due to the Twitter API rate limits.
#'
#' @param user_list  chr-vector or -list containing multiple Twitter user names
#'
#' @return Returns a list of character vectors which contain the "accounts followed" for all users from the user_list
#' @export
#'
#' @import twitteR
#' @import stringr
#'
#' @examples
#' # Setup a retweet matrix
#'  user_list <- c("hadleywickham", "joh_jo_mueller")
#'  get_following_list(user_list)


###Step 3: Functions to  (1) scrape list of all mentions of one user for a list of users

get_mention_list <- function(user_list,
                             limit = 2400,
                             date_begin = "2006-01-01",
                             date_end = Sys.Date()
){

  mention_list <- list()
  iter <- 0
  total_tweets <- 0

  for (user in user_list){
    timeline <- userTimeline(user, n= limit, includeRts=F)
    user1 <- do.call("rbind", lapply(timeline, as.data.frame))

    if (!is.null(user1)){
      user1 <- user1[which(as.Date(user1$created) >= as.Date(date_begin)&
                             as.Date(user1$created) <= as.Date(date_end) ), ] ### Created after 1.Feburary
    }

    user1$mentions <- str_extract_all(user1$text, "((@)\\w+)") ### extract only the mentions
    user_mentions <- paste(user1$mentions, collapse = ' ')

    mention_list <- c(mention_list, user_mentions)

    fix_rate_limit()

    total_tweets <- total_tweets + length(user1$text)
    iter <- iter + 1
    message('Processing user ', iter, ' of ', length(user_list), ' at ',
            Sys.time(), '; Rate Limit Info (Remaining): ', getCurRateLimitInfo()[39, 3])
  }

  print(paste0("The total number of tweets processed is ", total_tweets))
  mention_list
}
