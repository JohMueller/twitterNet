#' @title  Update a twitter user list
#'
#' @description Fix for an errorness list of Twitter users. Also prints names of inactive accounts.
#'
#' @param user_list  chr-vector or -list containing multiple Twitter user names
#' @return Returns an updated user list without the inactive Twitter accounts
#' @export
#'
#' @import twitteR
#'
#' @examples
#' update_user_list(c("joh_jo_mueller", "hadleywickham"))


update_user_list <- function(user_list){
  user_ok <- lookupUsers(user_list, includeNA=T)
  user_ok <- vapply(user_ok, function(x){!is.null(x)}, logical(1))
  user_list <- as.list(user_list)

  for (i in 1:length(user_list)){
    if (user_ok[[i]] == F){
      message(user_list[[i]], " is not on Twitter anymore")
      user_list[[i]]<-NULL
    }
  }

  user_list <- unlist(user_list)
  user_list
}
