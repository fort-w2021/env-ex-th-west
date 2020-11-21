#' Function to check if element is found in any environment
#' @param name name of the element to search for (must be a character vector
#' of length 1)
#' @param env the environment to start the search
#'
#' @return  a list of environments in which the element was found
anywhere <- function(name, env = parent.frame()) {
  checkmate::assert_character(name, len = 1)
  checkmate::assert_environment(env)
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  env_list <- list()

  while (!identical(env, emptyenv())) {
    if (exists(name, env, inherits = FALSE)) {
      env_list <- c(env_list, list(env))
    }
    env <- parent.env(env)
  }
  return(env_list)
}
