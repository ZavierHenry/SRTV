# Demo Options

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