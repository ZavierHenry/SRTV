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
#RUN mkdir -p /FFMPEG/usr/local/bin
#RUN mkdir -p /FFMPEG/usr/local/lib
#RUN cp -r /usr/local/bin/ffmpeg /FFMPEG/usr/local/bin/ffmpeg
#RUN cp -r /usr/local/lib /FFMPEG/usr/local
# RUN cp -r -t /FFMPEG /lib /etc

# Extract needed TTS files and directories
FROM synesthesiam/coqui-tts:latest AS tts
#RUN mkdir -p /TTS/usr/local/bin
#RUN cp -r /usr/local/bin/python3 /TTS/usr/local/bin/python3
#RUN cp -r -t /TTS /app /usr /etc /lib
#RUN rm -r /TTS/usr/include
RUN	mkdir -p /TTS/app/bin && \
	mkdir -p /TTS/usr/local && \
	mkdir -p /TTS/app/lib/python3.7/ && \
	mkdir -p /TTS/usr/lib && \
	cp /app/bin/tts /TTS/app/bin && \
	cp /app/bin/python3 /TTS/app/bin/python3 && \
	cp -r /usr/local/lib /TTS/usr/local && \
	cp -r /app/lib/python3.7/site-packages \
		 /TTS/app/lib/python3.7/ && \
	cp -r /usr/lib/x86_64-linux-gnu/ /TTS/usr/lib && \
	cp -r /etc /TTS/etc && \
	cp -r /lib /TTS
	


# Run program
FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
COPY --from=publish /app/publish .
COPY --from=publish /app/assets/ /app/assets/
# COPY --from=ffmpeg /FFMPEG .
COPY --from=ffmpeg / /
COPY --from=tts /TTS .
ENV FFMPEG_EXECUTABLE="/bin/ffmpeg"
ENV TTS_EXECUTABLE="/app/bin/tts"
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PYTHONPATH="/app/lib/python3.7/site-packages"
# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "synthesize", "This is the chosen spoken text to test the docker version of the text to speech. This speech also has a longer line than I would use to test this in order the see if detecting silence needs to be refined to preserve synchronization"]
