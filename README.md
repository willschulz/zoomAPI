
# zoomAPI

An R wrapper for the Zoom API. See API Reference here:
<https://marketplace.zoom.us/docs/api-reference/introduction>.

## Getting Started

``` r
devtools::install_github("willschulz/zoomAPI")
library(zoomAPI)
```

To get started using the zoom API, you must create an OAuth app
accessible to your Zoom account, which must be Pro or higher (this
includes Education accounts).

``` r
makeToken(client_id = "XXXXXXX", client_secret = "YYYYYYY", redirect_uri = "https://www.ZZZZZZZ.com", app_name = "")
```

## User Info

``` r
listUser()
```

This should return your own user information. If it does not, there has
been an error.

## Meeting Info

``` r
my_meetings <- listUserMeetings()
my_meetings

meeting_info <- meetingInfo(meeting_id = my_meetings$id[1])
meeting_info

meeting_polls <- listMeetingPolls(meeting_id = my_meetings$id[1])
meeting_polls

meeting_poll_results <- getMeetingPollsResults(meeting_id = my_meetings$id[1])
meeting_poll_results
```

## Templates

``` r
listTemplates()
```

## Create Meetings and Register Participants

``` r
#createMeeting(topic = "test_topic", start_time = "2021-08-22T17:35:00", duration = 10)

#create a meeting starting in 5 minutes:
new_meeting <- createMeeting(topic = "Zoom API Test Meeting", start_time = as.POSIXct(Sys.time() + 5*60), duration = 10, settings = list(approval_type = 0))

listUserMeetings()
```

Now we can register participants for this meeting.

``` r
addRegistrant(meeting_id = new_meeting$id, email = "person@email.com", first_name = "Test Registrant")

listMeetingRegistrants(meeting_id = new_meeting$id)
```

## Recordings

``` r
listRecordings()
```
