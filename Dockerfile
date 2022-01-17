# Copy local submodule
FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base

# Restore dependencies
FROM mcr.microsoft.com/dotnet/sdk:5.0 AS restore
COPY LinqToTwitter/src/LinqToTwitter6 /LinqToTwitter/src/LinqToTwitter6
WORKDIR /app
COPY sr_tweet_vis/sr_tweet_vis.fsproj .
RUN dotnet restore

# Extract needed FFMPEG files and directories
FROM jrottenberg/ffmpeg:4.1-scratch AS ffmpeg
#FROM base
#COPY --from=ffmpeg / /

# Publish app
FROM restore AS publish
COPY sr_tweet_vis/ .
RUN dotnet publish -c Release -o publish

# Copy published app to base
#FROM base
#COPY --from=publish /app/publish .
#COPY --from=publish /app/assets/ /app/assets/

FROM base AS puppeteer
RUN apt-get update && apt-get -f install && apt-get -y install wget gnupg2 apt-utils
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
    && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' \
    && apt-get update \
    && apt-get install -y google-chrome-unstable fonts-ipafont-gothic fonts-wqy-zenhei fonts-thai-tlwg fonts-kacst \
      --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*



FROM python:3.7-buster AS tts
WORKDIR /app
COPY /requirements.txt .
RUN	pip install --no-cache-dir -r requirements.txt && \
	apt-get update -y && apt-get -y install --no-install-recommends libsndfile1 && \
	rm -rf /var/lib/apt/lists/*

# Run program
FROM base
COPY --from=publish /app/publish .
COPY --from=publish /app/assets/ /app/assets/
COPY --from=ffmpeg / /
COPY --from=tts / /
COPY /tts.py /
COPY --from=puppeteer / /

# Add Procfile to project
COPY clock.py clock.py

# Update cache to have soundfile
RUN /sbin/ldconfig

# ENV PATH="/venv/bin:$PATH"
ENV FFMPEG_EXECUTABLE="/bin/ffmpeg"
# ENV TTS_EXECUTABLE="/venv/bin/tts"
ENV TTS_PATH="/tts.py"
#ENV LD_LIBRARY_PATH="/usr/local/lib"
#ENV PYTHONPATH="/app/lib/python3.7/site-packages"
ENV CHROME_EXECUTABLE="/usr/bin/google-chrome"

ENTRYPOINT [ "python", "clock.py" ]


# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "mentions"]
# ENTRYPOINT ["dotnet", "sr_tweet_vis.dll", "synthesize", "This is the chosen spoken text to test the docker version of the text to speech. This speech also has a longer line than I would use to test this in order the see if detecting silence needs to be refined to preserve synchronization"]
