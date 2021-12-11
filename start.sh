#!/bin/bash

if [[ $DYNO == worker* ]]; then
     dotnet sr_tweet_vis.dll mentions
elif [[ $DYNO == clock* ]]; then
     python clock.py 
fi