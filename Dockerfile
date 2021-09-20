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

FROM synesthesiam/coqui-tts:latest AS tts
RUN rm -r TTS-*/
RUN rm TTS-*.tar.gz


# Run program
FROM mcr.microsoft.com/dotnet/runtime:5.0
COPY --from=publish /app/publish .
COPY --from=publish /app/assets/ /app/assets
COPY --from=jrottenberg/ffmpeg / /
COPY --from=tts / /
ENV FFMPEG_EXECUTABLE="/usr/local/bin/ffmpeg"
ENV TTS_EXECUTABLE="/app/bin/tts"
ENV LD_LIBRARY_PATH="/usr/local/lib"
# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "synthesize", "This is the chosen spoken text to test the docker version of the text to speech. This speech also has a longer line than I would use to test this in order the see if detecting silence needs to be refined to preserve synchronization"]
