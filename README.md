# ElmReddit

## What is it

To be Reddit susbcriptions manager. Ugly, to be better.

Following actions supported:

* Viewing
  * All subreddits
  * Subreddits from specific multireddit
  * Subreddits not present in any multireddit
  * All subreddits where user is subscriber
  * All subreddits present in any multireddit where user is not a subscriber
* Editing
  * Add subreddit to multireddit
  * Remove subreddit from multireddit
  * Subscribe to subreddit
  * Unsubscribe from subreddit

Not (yet?) implemented:

* Create multireddit
* Remove multireddit
* Subscribe to subreddit which is not listed in any of supported views
* Add subreddit to multireddit if it's not listed in any of supported views

## Installation

Just clone the repo.

## Running

Go to cloned repo and run `elm-reactor` (I am assuming you have `elm` installed),
then navigate to `http://localhost:8000/index.html` in your browser

## Caveats

It's work in progress, if it's not going to change then it's not going to work as intended.
