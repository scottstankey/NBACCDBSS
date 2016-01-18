import requests
import json
import pandas as pd 


for i in range(10, 15):

	#season
	season = '20' + str(i) + '-' + str(i+1)

	#NBA Stats API using selected player ID
	url = 'http://stats.nba.com/stats/leaguegamelog?Counter=1000&Direction=DESC&LeagueID=00&PlayerOrTeam=P&Season=' + season + '&SeasonType=Regular+Season&Sorter=PTS'

	#Create Dict based on JSON response
	response = requests.get(url)
	shots = response.json()['resultSets'][0]['rowSet']
	data = json.loads(response.text)

	#Create df from data and find averages 
	headers = data['resultSets'][0]['headers']
	shot_data = data['resultSets'][0]['rowSet']
	df = pd.DataFrame(shot_data,columns=headers)

	#output a csv of the DataFrame
	df.to_csv(path_or_buf= season + '.csv')

