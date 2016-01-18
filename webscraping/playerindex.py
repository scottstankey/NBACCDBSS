import requests
import json
import pandas as pd 

url = 'http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2015-16'

#Create Dict based on JSON response
response = requests.get(url)
shots = response.json()['resultSets'][0]['rowSet']
data = json.loads(response.text)

#Create df from data 
headers = data['resultSets'][0]['headers']
player_data = data['resultSets'][0]['rowSet']
df = pd.DataFrame(player_data,columns=headers)

#output a csv of the DataFrame
df.to_csv(path_or_buf="playerindex.csv")