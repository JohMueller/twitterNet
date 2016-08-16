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
