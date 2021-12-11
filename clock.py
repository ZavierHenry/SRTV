from apscheduler.schedulers.blocking import BlockingScheduler
import requests
import os

scheduler = BlockingScheduler()
minutes = 5

@scheduler.scheduled_job('interval', minutes=minutes)
def schedule_mentions_run():
    app_name = os.environ.get("APP_NAME")
    auth_token = os.environ.get("AUTH_TOKEN")
    
    url = f'https://api.heroku.com/apps/{app_name}/dynos'

    dyno = {
        "command": "./start.sh",
        "size": 'free',
        "type": "worker",
        'time_to_live': 60 * 4
    }

    headers = {
        "Accept": "application/vnd.heroku_json; version=3",
        "Authorization": f"Bearer {auth_token}",
        "Content-Type": "application/json"
    }

    resp = requests.post(url, data=dyno, headers=headers)
    try:
        resp.raise_for_status()
    except requests.HTTPError as ex:
        print("HTTP error when trying to run worker dyno:", ex)

scheduler.start()