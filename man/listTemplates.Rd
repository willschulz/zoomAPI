% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{listTemplates}
\alias{listTemplates}
\title{List User Meeting Templates}
\usage{
listTemplates(user_id = "me", token_cache = "~/.zoom_token_cache")
}
\arguments{
\item{token_cache}{Path to token cache}

\item{meeting_id}{Specify user ID whose recordings to lookup.  Defaults to "me".}
}
\description{
Query the /users/{user_id}/meeting_templates endpoint.
}
\examples{
listTemplates()
}
\keyword{templates}
