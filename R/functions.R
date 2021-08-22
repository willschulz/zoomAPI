
#' Make a Zoom Token
#'
#' This function allows you to make a Zoom API token from your app.
#' @param client_id Client ID associated with your Zoom app.
#' @param client_secret Client secret associated with your Zoom app.
#' @param redirect_uri Redirect URI associated with your Zoom app.
#' @param app_name Optional app name, for organizational purposes.
#' @param save Save token object to home directory?
#' @keywords auth
#' @export
#' @examples
#' makeToken()

makeToken <- function(client_id, client_secret, redirect_uri, app_name = "", save = TRUE){
  zoom_endpoint <- httr::oauth_endpoint(authorize = "https://zoom.us/oauth/authorize", access = "https://zoom.us/oauth/token")
  zoom_app <- httr::oauth_app(appname = app_name, key = client_id, secret = client_secret, redirect_uri = redirect_uri)
  message("Opening a browser window to authenticate.  After authentication, copy code from redirect URL below to complete token creation...")
  zoom_token <- httr::oauth2.0_token(zoom_endpoint, zoom_app, use_basic_auth = TRUE, query_authorize_extra=list(prompt="none"), cache=FALSE, use_oob = TRUE, oob_value = redirect_uri)
  if(save){
    expanded_path <- path.expand("~/.ZOOM_TOKEN.rds")
    Sys.setenv(ZOOM_TOKEN = expanded_path)
    saveRDS(zoom_token, file = expanded_path)
    }
  return(zoom_token)
}

#' Load Saved Zoom Token
#'
#' This function allows you to load a Zoom API token previously saved in the home directory.
#' @keywords auth
#' @export
#' @examples
#' loadToken()

loadToken <- function() {
  token_location <- Sys.getenv('ZOOM_TOKEN')
     if (identical(token_location, "")) {
       stop("No saved Zoom token found.  Please run zoomAPI::makeToken()",
         call. = FALSE)}
  return(readRDS(token_location))
}


#' Get User
#'
#' Query the /users/ endpoint.
#' @param user_id Specify ID of user to lookup.  Defaults to "me".
#' @keywords users
#' @export
#' @examples
#' loadToken()

getUser <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/me"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  return(content)
}


#' Get User Meetings
#'
#' Query the /users/{user_id}/meetings endpoint.
#' @param user_id Specify ID of user whose meetings to lookup.  Defaults to "me".
#' @keywords users
#' @export
#' @examples
#' getMyMeetings()

getMyMeetings <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/me/meetings"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}
