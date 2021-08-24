FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR app

COPY sr_tweet_vis/sr_tweet_vis.fsproj .
COPY LinqToTwitter/src/LinqToTwitter6/LinqToTwitter/LinqToTwitter.csproj .
RUN dotnet restore

FROM build AS publish
COPY . .
RUN dotnet publish "sr_tweet_vis.fsproj" -c Release -o /sr_tweet_vis/publish


FROM mcr.microsoft.com/dotnet/runtime:5.0
WORKDIR app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet" "sr_tweet_vis.dll"]