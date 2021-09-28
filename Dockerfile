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

# Extract needed FFMPEG files and directories
FROM jrottenberg/ffmpeg:scratch AS ffmpeg

# Extract needed TTS files and directories
FROM synesthesiam/coqui-tts:latest AS tts
RUN	mkdir -p /TTS/app/bin && \
	mkdir -p /TTS/usr/local && \
	mkdir -p /TTS/app/lib/python3.7 && \
	mkdir -p /TTS/usr/lib && \
	cp /app/bin/tts /TTS/app/bin && \
	cp /app/bin/python3 /TTS/app/bin/python3 && \
	cp -r /usr/local/lib /TTS/usr/local && \
	cp -r /usr/lib/x86_64-linux-gnu /TTS/usr/lib && \
	cp -r /etc /TTS && \
	cp -r /lib /TTS && \
	cp -r /app/lib/python3.7/site-packages /TTS/app/lib/python3.7

WORKDIR /TTS/app/lib/python3.7/site-packages
RUN rm -r pip wheel setuptools tests werkzeug *.dist-info Cython \
	     matplotlib/mpl-data/images unidic_lite/dicdir/unidic-mecab.pdf && \
    cp -r /app/lib/python3.7/site-packages/gdown-*.dist-info .

# Run program
FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
COPY --from=publish /app/publish .
COPY --from=publish /app/assets/ /app/assets/
COPY --from=ffmpeg / /
COPY --from=tts /TTS .
ENV FFMPEG_EXECUTABLE="/bin/ffmpeg"
ENV TTS_EXECUTABLE="/app/bin/tts"
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PYTHONPATH="/app/lib/python3.7/site-packages"
# ENTRYPOINT [ "/app/bin/tts", "-h" ]
# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "synthesize", "This is the chosen spoken text to test the docker version of the text to speech. This speech also has a longer line than I would use to test this in order the see if detecting silence needs to be refined to preserve synchronization"]
