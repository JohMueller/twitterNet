#' @title  Building Twitter Networks
#'
#' @description Set up adjacency matrix for a follower-, mention- or retweet-network given a Twitter user list
#'
#' Note that this may take a while due to the Twitter API rate limits...
#'
#' @param user_list  chr-vector or -list containing multiple Twitter user names
#' @param type type of interaction matrix: c("mention", "retweet", "following"), default = "mention"
#' @param update_list Boolean value if he user list should be updated (strongly recommended as the function will fail if there are inactive users in the list)
#' @param limit Optional: Limit of Tweets per person
#' @param date_begin Start date for scraping tweets
#' @param date_end End date for scraping tweets
#'
#' @return Returns an adjacency Matrix
#' @export
#'
#' @import twitteR
#' @import stringr
#'
#' @examples
#' # Setup a retweet matrix
#'  user_list <- c("hadleywickham", "joh_jo_mueller")
#'  get_retweet_list(user_list)



#### Set up adjacency matrix for a follower-, mention- or retweet-network given a
#### Twitter_user_list

setup_adj_matrix <- function(user_list,
                    type = "mention",
                    update_list = TRUE,
                    limit = 2400,
                    date_begin = "2006-01-01",
                    date_end = Sys.Date()){

  ### Step 1: Update user_list
  if (update_list== TRUE){
    user_list <- update_user_list(user_list)
  }

  ### Step 2: Get mention/follower/retweet-lists for every user from the list
  if(type == "mention"){
    mention_list <- get_mention_list(user_list, limit, date_begin, date_end)
  } else if (type == "retweet"){
    mention_list <- get_retweet_list(user_list, limit, date_begin, date_end)
  } else{
    mention_list <- get_following_list(user_list)
  }

  ### Step 3: Get adj. Matrix
  adj_matrix <- get_adj_matrix(user_list, mention_list)
  adj_matrix
}
