# ElmReddit

## What is it

To be Reddit susbcriptions manager. UI done with [MDB](https://mdbootstrap.com/).

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

0. I am assuming you have [elm 0.19](https://www.npmjs.com/package/elm) installed
1. Go to the cloned repo and run `elm make src/Main.elm --output elm.js`
2. Run `elm reactor` from the same directory
3. Navigate to `http://localhost:8000/index.html` in your browser

## Tinkering

1. [Get](https://www.reddit.com/prefs/apps) reddit API key
2. Put it into `src/Config.elm`

Or you can create `./Config.elm.secret` and run `./build.sh` script to build `elm.js`.
Look into `build.sh`, it's straightforward

Now you can run it on your domain, or do other fun stuff, probably.
