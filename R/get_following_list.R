#' @title  Get a following-list for mutliple users
#'
#' @description This function scrapes all accounts a user follows for a list of twitter users.
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


###Step 4: Function to set up the adj. Matrix using two arguments:
###             A list of twitter users and a list of all mentions by a user


### Get follower-list for every user from a list

get_following_list <- function(user_list){

    following_list <- list()
    iter <- 0

    for (user in user_list){
      user1 <- getUser(user)
      following <- user1$getFriends
      following <- as.character(lapply(following, function(x) x$getName()))
      user_following <- paste(following, collapse = ' ')

      following_list <- c(following_list, user_following)

      fix_rate_limit()

      iter <- iter + 1
      message('Processing user ', iter, ' of ', length(user_list), ' at ',
              Sys.time(), '; Rate Limit Info (Remaining): ', getCurRateLimitInfo()[39, 3])
    }

    following_list
}
