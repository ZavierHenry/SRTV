from apscheduler.schedulers.blocking import BlockingScheduler
import subprocess

scheduler = BlockingScheduler()
minutes = 4

@scheduler.scheduled_job('interval', minutes=minutes)
def schedule_mentions_run():
    print("Running SRTV Twitter bot...")
    result = subprocess.run(['dotnet', 'sr_tweet_vis.dll', 'mentions'])
    
    if result.returncode == 0:
        print("Done running SRTV Twitter mentions bot...")
        print(result.stdout)
    else:
        print("An error occurred running SRTV Twitter mentions bot...")
        print(result.stderr)

scheduler.start()