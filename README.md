# SRTV

SRTV (Screen Reader Tweet Visualizer) is a [Twitter bot](https://twitter.com/srtvtweeter) that tweets the screen reader output of a tweet either as audio, an image, or text.

## How It Works

### Render Commands

SRTV has several options in rendering the screen reader output of tweets. These commands are shown down below

#### Render Video (default)


#### Render image

This option renders the specified tweet into an image. The user also has the option of specifing either a light theme, dim theme, or a dark theme. The bot defaults to picking the dim theme.
The image also has alt text with the contents of the screen reader output, or a below tweet if the length of the alt text is loo large.


#### Render Text


### Private Tweets

This bot is intentionally designed so that you cannot render a tweet coming from a private account, even if the bot has access to the tweet.

## Contributions

Contributions are always welcome. Read [the contributions file](CONTRIBUTING.md) for guidelines on contributing to SRTV.


## Screen Readers Tested


## Rendering Tweet Issues

