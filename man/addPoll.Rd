% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{addPoll}
\alias{addPoll}
\title{Create a Poll for a Meeting}
\usage{
addPoll(meeting_id, title, questions, token_cache = "~/.zoom_token_cache")
}
\arguments{
\item{meeting_id}{ID of the meeting to register the participant for.}

\item{title}{Title of poll.}

\item{questions}{Questions, formatted appropriately.}

\item{token_cache}{Path to token cache}
}
\description{
Post to the /meetings/{meeting_id}/polls endpoint.
}
\examples{
addPoll()
}
\keyword{polls}
