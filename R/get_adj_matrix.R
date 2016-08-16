#' @title  Set up an adjadency matrix
#'
#' @description This function sets up an adjacency matrix from a
#' list of interactions between Twitter users. This function can be used to set up
#' a mention-, following- or retweet-matrix.
#'
#' @param user_list  chr-vector or -list containing multiple Twitter user names
#' @param mention_list a list of all interactions - e.g. following, retweet, mention - of one users with other users
#'
#' @return Returns an matrix containing the number of directed interactions from a user -cols- to another user -rows-
#' @export
#'
#' @import twitteR
#' @import stringr
#'
#' @examples
#' # Setup a retweet matrix
#'  get_adj_matrix(user_list, retweet_list)


###Step 4: Function to set up the adj. Matrix using two arguments:
###             A list of twitter users and a list of all mentions by a user

get_adj_matrix <- function(user_list, mention_list){

  adj_matrix <- matrix(nrow = length(user_list), ncol = length(user_list))
  colnames(adj_matrix) <- user_list
  rownames(adj_matrix) <- user_list

  for (i in 1:length(user_list)){
    for (j in 1:length(user_list)){
      adj_matrix[j,i] <- str_count(mention_list[i], pattern=paste(user_list[j]))
    }
  }

  adj_matrix
}
