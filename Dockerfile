FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
COPY LinqToTwitter/ /LinqToTwitter
WORKDIR app
COPY sr_tweet_vis/sr_tweet_vis.fsproj .
RUN dotnet restore

FROM build AS publish
COPY sr_tweet_vis/ .
RUN dotnet publish -c Release -o publish

FROM mcr.microsoft.com/dotnet/runtime:5.0
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "sr_tweet_vis.dll"]