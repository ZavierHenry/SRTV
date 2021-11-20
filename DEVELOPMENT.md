# SRTV Development

## Building the SRTV bot

Build this application like any other .NET application with Visual Studio or dotnet. However, for this application to work properly, follow the below steps. 

### Twitter Development Account

In order to use this application, you must have access to a [Twitter Developer account](https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api). 
Once you have access, set up a project that will give you API keys. Be sure that the app has both read and write permissions. Direct message permissions are unnecessary.

### Getting Dependency Executables

Along with a Twitter development account, this application requires the following for all of the features to work properly:

- [FFMPEG](https://ffmpeg.org/download.html)
- [Coqui TTS](https://github.com/coqui-ai/TTS/)
- [Google Chrome](https://www.google.com/chrome/)

Each has installation instructions which can be found by clicking on the item. Afterwards, the path to each executable should be put in environmental variables which are explained below.

### Setting the environment variables

In order for the application to work, the following environment variables must be set:

- TWITTER_ACCESS_TOKEN: Twitter access token key
- TWITTER_ACCESS_TOKEN_SECRET: Twitter access token secret key
- TWITTER_CONSUMER_KEY: Twitter API key
- TWITTER_CONSUMER_SECRET: Twitter API secret
- TWITTER_USER_ID: User ID of the authenticated Twitter account

These environment variables must also be set, but 
- CHROME_EXECUTABLE: Path to Chrome executable. This is used when rendering a tweet as an image
- TTS_EXECUTABLE: Path to Coqui TTS executable. This is used when rendering a tweet to a video
- FFMPEG_EXECUTABLE: Path to FFMPEG exectuable. This is also used when rendering a tweet to a video

- NETCORE_ENVIRONMENT: This is an optional variable that sets whether the NET core environment should be set to development or production. 
  
  Keep in mind that if the variable is not set, the project is considered to be in development mode. This should not change anything for running the project but it will try to read variables from [the secrets.json file](https://docs.microsoft.com/en-us/aspnet/core/security/app-secrets?view=aspnetcore-6.0&tabs=linux#how-the-secret-manager-tool-works).

### Docker image

The easiest way to make sure all the dependencies and environment variables are in the right place is to build the Docker image in the home directory. See [the Docker documentation](https://docs.docker.com/) for guides on how to build a Docker project. Note that if you run the Docker image, you will still need to set the Twitter environment variables in your environment.

## Demo Options

Several options are available in order to do demos or debugging, which are detailed below:

- mentions: Read appsettings.json and render requests in the authenticated account's mentions

- synthesize [-f] --tweet_id <tweet_id> [--outfile \<outfile\>]: fetches <tweet_id> and renders into a video. Uses outfile to save to local file. If no outfile is specified, the result will be tweeted out. Use the -f switch to control whether to render the full version of a tweet.

- synthesize \<text\> [outfile]: renders \<text\> to a local file. Can specify the filename with [outfile]

- speak \<text\> [outfile]: renders \<text\> to a local audio file. Can specify filename with [outfile]

- image \<theme\> [-f] --tweet_id \<tweet_id\> [--outfile \<outfile\>]: fetches \<tweet_id\> and renders into an image with theme \<theme\> (dark, light, or dim). Other parameters function the same as in the synthesize option
 
- sendTweet \<text\>: sends tweet as text

- getTweet \<tweet_id\>: fetches tweet and prints the ID, text, and image alt text

- text [-f] --tweet_id \<tweet_id\> [--outfile \<outfile\>]: renders \<tweet_id\> as text. Other parameters function the same as in the synthesize option

- help: Shows this help screen