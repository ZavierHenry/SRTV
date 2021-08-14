# SRTV

SRTV (Screen Reader Tweet Visualizer) is a [Twitter bot](https://twitter.com/srtvtweeter) that tweets the screen reader output of a tweet either as audio, an image, or text.


## Motivation

Accessibility is an important consideration for any website.


Take for example this tweet from the Vanderbilt University Medical Center:

![Vanderbilt children's statement](assets/statement_tweet_black.png)

In this tweet, the Medical Center issued a statement stating that the Adult Hospital and Medical Center is at capacity and will be limiting elective procedures and denying transfer requests from other hospitals.
However, when using a screen reader, the information received is different.

![Vanderbilt children's statement screen reader text](assets/statement_screen_reader_tweet.png)

The statement in the tweet is replaced with "image", indicating that the image did not have any alt text. People who are using screen readers lose the valuable information within the statement because of a lack of alt text.

## How It Works

To use SRTV with a tweet, reply or quote tweet to it mentioning @srtvtweeter with the word "render".

### Render Commands

SRTV has several options in rendering the screen reader output of tweets. These commands are detailed below:

#### Render Video (default)

This option renders the specified tweet into a video. The video contains the screen reader audio of the rendered tweet with captions. 

#### Render Image

This option renders the specified tweet into an image. The user also has the option of specifing either a light theme, dim theme, or a dark theme by specifing either "light", "dim", or "dark" before "image". The bot defaults to picking the dim theme.
The image also has alt text with the contents of the screen reader output, or reply tweets if the length of the alt text is loo large.


#### Render Text

This option renders the specified tweet into a text tweet. Mutliple tweets will be sent if the length of the text cannot fit in a single tweet.


### Private Tweets

This bot is intentionally designed so that you cannot render a tweet sent from a private account, even if the bot has access to the tweet. However, a private account may still want SRTV to render one of their own tweets.
SRTV allows this, but the process is a bit different:


## Screen Reader Output Caveat


## Rendering Tweet Issues


## Contributions

Contributions are always welcome. Read [the contributions file](CONTRIBUTING.md) for guidelines on contributing to SRTV.

