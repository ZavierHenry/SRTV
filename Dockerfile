# Copy local submodule
FROM mcr.microsoft.com/dotnet/sdk:5.0 AS submodule
COPY LinqToTwitter/src/LinqToTwitter6 /LinqToTwitter/src/LinqToTwitter6

# Restore dependencies
FROM submodule AS build
WORKDIR /app
COPY sr_tweet_vis/sr_tweet_vis.fsproj .
RUN dotnet restore

# Publish app
FROM build AS publish
COPY sr_tweet_vis/ .
RUN dotnet publish -c Release -o publish

# Extract needed TTS files and directories
FROM synesthesiam/coqui-tts:latest AS tts
RUN mkdir -p /TTS
RUN cp -r -t /TTS /app /usr /etc /lib

# Extract needed FFMPEG files and directories
FROM jrottenberg/ffmpeg AS ffmpeg
RUN mkdir -p /FFMPEG/usr/local/bin
RUN mkdir -p /FFMPEG/usr/local/lib
RUN cp -r /usr/local/bin/ffmpeg /FFMPEG/usr/local/bin/ffmpeg
RUN cp -r /usr/local/lib /FFMPEG/usr/local/
RUN cp -r -t /FFMPEG /lib /etc

# Run program
FROM mcr.microsoft.com/dotnet/runtime:5.0
COPY --from=publish /app/publish .
COPY --from=publish /app/assets/ /app/assets/
COPY --from=ffmpeg /FFMPEG .
COPY --from=tts /TTS .
ENV FFMPEG_EXECUTABLE="/usr/local/bin/ffmpeg"
ENV TTS_EXECUTABLE="/app/bin/tts"
ENV LD_LIBRARY_PATH="/usr/local/lib"
# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "synthesize", "This is the chosen spoken text to test the docker version of the text to speech. This speech also has a longer line than I would use to test this in order the see if detecting silence needs to be refined to preserve synchronization"]
