FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
COPY LinqToTwitter/ /LinqToTwitter
WORKDIR app
COPY sr_tweet_vis/sr_tweet_vis.fsproj .
RUN dotnet restore

FROM build AS publish
COPY sr_tweet_vis/ .
RUN dotnet publish -c Release -o publish

FROM synesthesiam/coqui-tts AS base
RUN wget https://packages.microsoft.com/config/debian/10/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
RUN dpkg -i packages-microsoft-prod.deb
RUN rm packages-microsoft-prod.deb
RUN apt-get update; \
  apt-get install -y apt-transport-https && \
  apt-get update && \
  apt-get install -y dotnet-runtime-5.0

FROM base
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "sr_tweet_vis.dll"]
