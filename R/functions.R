
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
    if (file.exists("~/.Renviron")){
      renviron <- readLines("~/.Renviron")
      renviron <- c(renviron, paste0("ZOOM_TOKEN = ", expanded_path))
    } else (renviron <- paste0("ZOOM_TOKEN = ", expanded_path))
    writeLines(renviron, "~/.Renviron")
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
#' user()

listUser <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/me"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  content
  return(content)
}


#' Get User Meetings
#'
#' Query the /users/{user_id}/meetings endpoint.
#' @param user_id Specify ID of user whose meetings to lookup.  Defaults to "me".
#' @keywords users
#' @export
#' @examples
#' userMeetings()

listUserMeetings <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/",user_id,"/meetings"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' Get Meeting Info
#'
#' Query the /meetings/{meeting_id} endpoint.
#' @param meeting_id Specify ID of meeting to lookup.
#' @keywords meetings
#' @export
#' @examples
#' meetingInfo()

meetingInfo <- function(meeting_id, token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "/meetings/", meeting_id),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' List Cloud Recordings
#'
#' Query the /users/{user_id}/recordings endpoint.
#' @param meeting_id Specify user ID whose recordings to lookup.
#' @keywords recordings
#' @export
#' @examples
#' listRecordings()

listRecordings <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/",user_id,"/recordings"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' List Meeting Polls
#'
#' Query the /users/{meeting_id}/polls endpoint.
#' @param meeting_id Specify meeting ID whose polls to lookup.
#' @keywords polls
#' @export
#' @examples
#' listMeetingPolls()

listMeetingPolls <- function(meeting_id, token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "/meetings/",meeting_id,"/polls"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' Get Meeting Poll Results
#'
#' Query the /past_meetings/{meeting_id}/polls endpoint.
#' @param meeting_id Specify meeting ID whose recordings to lookup.
#' @keywords polls
#' @export
#' @examples
#' getMeetingPollsResults()

getMeetingPollsResults <- function(meeting_id, token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "/past_meetings/",meeting_id,"/polls"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' List User Meeting Templates
#'
#' Query the /users/{user_id}/meeting_templates endpoint.
#' @param meeting_id Specify user ID whose recordings to lookup.  Defaults to "me".
#' @keywords templates
#' @export
#' @examples
#' listTemplates()

listTemplates <- function(user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "users/me/meeting_templates"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' Create a new meeting.
#'
#' Post to the /users/{user_id}/meetings endpoint.
#' @param topic Topic/title of meeting.
#' @param start_time Starting time of meeting.  For example, "2021-08-19T17:10:00".
#' @param duration Duration of meeting in minutes.  Defaults to 60.
#' @param type Type of meeting.  Defaults to 2.
#' @param pre_schedule Pre-schedule meeting? Defaults to FALSE.
#' @param timezone Time zone to schedule meeting.  Defaults to "America/New_York".
#' @param template_id Template ID for creating meeting.  Defaults to "".
#' @param user_id Specify user ID to create a meeting for.  Defaults to "me".
#' @keywords meetings
#' @export
#' @examples
#' createMeeting()

createMeeting <- function(topic, start_time, duration = 60, type=2, pre_schedule = FALSE, timezone = "America/New_York", template_id = "", user_id = "me", token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::POST(url =  paste0(request_base_url, "users/me/meetings"),
                               config = httr::config(token = token),
                               body = list(topic = topic,
                                           type = type,
                                           pre_schedule = pre_schedule,
                                           start_time = start_time,
                                           duration = duration,
                                           timezone = timezone,
                                           template_id = template_id),
                               encode = "json")
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' List Meeting Registrants
#'
#' Query the /meetings/{meeting_id}/registrants endpoint.
#' @param meeting_id Specify user ID whose recordings to lookup.  Defaults to "me".
#' @keywords registration
#' @export
#' @examples
#' listMeetingRegistrants()

listMeetingRegistrants <- function(meeting_id, token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::GET(url = paste0(request_base_url, "meetings/",meeting_id,"/registrants"),
                              config = httr::config(token = token))
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}


#' Register Participants for a Meeting
#'
#' Post to the /meetings/{meeting_id}/registrants endpoint.
#' @param meeting_id ID of the meeting to register the participant for.
#' @param email Registrant email.
#' @param first_name Registrant first name.
#' @keywords registration
#' @export
#' @examples
#' addRegistrant()

addRegistrant <- function(meeting_id, email, first_name, token = NULL){
  request_base_url = "https://api.zoom.us/v2/"
  if (is.null(token)) {token <- zoomAPI::loadToken()}
  request_result <- httr::POST(url =  paste0(request_base_url, "users/me/meetings"),
                               config = httr::config(token = token),
                               body = list(email = email,
                                           first_name = first_name),
                               encode = "json")
  content <- httr::content(request_result)
  #content <- do.call(dplyr::bind_rows, content$meetings)
  return(content)
}
