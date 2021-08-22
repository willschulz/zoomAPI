
# zoomAPI

An R wrapper for the Zoom API

## Getting Started

``` r
devtools::install_github("willschulz/zoomAPI")
library(zoomAPI)
```

To get started using the zoom API, you must create an OAuth app
accessible to your Zoom account, which must be Pro or higher.

``` r
zoomAPI::makeToken(client_id = "XXXXXXX", client_secret = "YYYYYYY", redirect_uri = "https://www.ZZZZZZZ.com", app_name = "", save = TRUE)

zoomAPI::loadToken()
```

## Get User Info

Try an API call by fetching your own user information:

``` r
zoomAPI::listUser()
```

## List User’s Meetings

``` r
zoomAPI::listUserMeetings()
```

## Look Up Specific Meeting Info

``` r
zoomAPI::meetingInfo(meeting_id = "98354593291")
```

``` r
zoomAPI::listMeetingPolls(meeting_id = "98354593291")
```

``` r
zoomAPI::getMeetingPollsResults(meeting_id = "98354593291")
```

``` r
zoomAPI::listTemplates()
```

``` r

zoomAPI::createMeeting(topic = "test_topic", start_time = "2021-08-22T17:35:00", duration = 10)
```

``` r

zoomAPI::listMeetingRegistrants(meeting_id = "97864966163")
```

``` r

zoomAPI::addRegistrant(meeting_id = "92581841447", email = "wschulz@princeton.edu", first_name = "Will")
```

``` r
zoomAPI::listRecordings()
```
