#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}


#' Make a Zoom Token
#'
#' This function allows you to make a Zoom API token from your app.
#' @param client_id Client ID associated with your Zoom app.
#' @param client_secret Client secret associated with your Zoom app.
#' @param redirect_uri Redirect URI associated with your Zoom app.
#' @param app_name Optional app name, for organizational purposes.
#' @param save Save the token in the home directory?  Defaults to TRUE.
#' @keywords Auth
#' @export
#' @examples
#' makeToken()

makeToken <- function(client_id, client_secret, redirect_uri, app_name = "", save = TRUE){
  zoom_endpoint <- httr::oauth_endpoint(authorize = "https://zoom.us/oauth/authorize", access = "https://zoom.us/oauth/token")
  zoom_app <- httr::oauth_app(appname = app_name, key = client_id, secret = client_secret, redirect_uri = redirect_uri)
  message("Opening a browser window to authenticate.  After authentication, copy code from redirect URL below to complete token creation...")
  zoom_token <- httr::oauth2.0_token(zoom_endpoint, zoom_app, use_basic_auth = TRUE, query_authorize_extra=list(prompt="none"), cache=FALSE, use_oob = TRUE, oob_value = redirect_uri)
  if(save){saveRDS(zoom_token, file = "~/.ZOOMR_TOKEN.rds")}
  return(zoom_token)
}

#' Load Saved Zoom Token
#'
#' This function allows you to load a Zoom API token previously saved in the home directory.
#' @keywords Auth
#' @export
#' @examples
#' loadToken()

loadToken <- function() {
  if(file.exists("~/.ZOOMR_TOKEN.rds"))
  {return(readRDS("~/.ZOOMR_TOKEN.rds"))}
  else
  {stop("No Zoom token found.  Please run zoomR::makeToken()")}
}
