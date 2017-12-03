tidysportsfeed - Make Tibbles from the MySportsFeeds.com API
================

## Getting started

1.  Go to <https://www.mysportsfeeds.com/> and register an account.

2.  Sign in.

3.  Browse to <https://www.mysportsfeeds.com/data-feeds/api-docs>.

4.  Use the selectors on the left to pick your sport, season and feed.
    I’ll use NBA, 2016-2017 regular and Daily DFS for the example.

5.  Now you’re on the documentation page for the sport, season and feed.
    Click on the “CSV” sample.
    
    If you get an authentication request, enter your user name and
    password. You can then download the file, or, if you have a
    spreadsheet program installed, open the file in it.

## What’s in this package?

As you’ve seen above, MySportsFeeds.com allows you to directly download
files in spreadsheet-friendly CSV format. You can also set it up to
email you the data. As long as you’re doing non-commercial analysis on
historical data, everything is free.

But if you want to build up a database of historical data, or want to
track the current season, you’ll want to automate things. Enter
`tidysportsfeeds`. I’m doing daily fantasy sports analytics, so I want
to build up databases and build projection / optimization software.
`tidysportsfeeds` is a collection of the front-end part of this - the
workflow from the raw API to tibbles.

## What now?

That depends on what you want to do with the data. I recommend exploring
the data on MySportsFeeds.com with a browser and a spreadsheet as
described above. Once you know what’s on the site, you can focus on a
workflow.

I’m building this package as a general tool to get data into tibbles.
But the examples will nearly all be relevant to NBA daily fantasy
sports.

## Patreon links

  - [M. Edward (Ed) Borasky](https://www.patreon.com/znmeb)
  - [MySportsFeeds](https://www.patreon.com/mysportsfeeds/posts)
