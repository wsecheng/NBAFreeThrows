from bs4 import BeautifulSoup
import urllib 
import re
import csv

############################ Get Game Metadata ############################
def getGameMetadata():	
	ids = []
	#date = []
	scores = []
	team = []
	homeOrAway = []

	teams = ['BOS', 'BKN', 'NYK', 'PHI', 'TOR', 'GS', 'LAC', 'LAL', 'PHX', 'SAC', 
		 'CHI', 'CLE', 'DET', 'IND', 'MIL', 'DAL', 'HOU', 'MEM', 'NO', 'SA',
		 'ATL', 'CHA', 'MIA', 'ORL', 'WSH', 'DEN', 'MIN', 'OKC', 'POR', 'UTAH']


	for tm in teams:
		url = "http://www.espn.com/nba/team/schedule/_/name/{0}/seasontype/2".format(tm)

		r = urllib.urlopen(url)
		soup = BeautifulSoup(r, "lxml")

		oddRows = soup.find_all('tr', class_ = 'oddrow')
		for odd in oddRows:
			print 'odd'
			if not 'Postponed' in odd.text:
				#dte = odd.find("td")
				links = odd.find("a", href=re.compile("recap"))
				homeAway = odd.find('li', class_ = 'game-status')
				#homeAway = [e for e in soup.find('li', class_='game-status') if len(e['class'])==1]
				
				#date.append(str(dte.text))
				m = re.search(r'\d{9}', str(links))
				team.append(tm.upper())
				ids.append(m.group(0))			
				scores.append(str(links.text))
				homeOrAway.append(str(homeAway.text))

		evenRows = soup.find_all('tr', class_ = 'evenrow')
		for even in evenRows:
			print ' even'
			if not 'Postponed' in even.text:
				links = even.find("a", href=re.compile("recap"))
				homeAway = even.find('li', class_ = 'game-status')
				#homeAway = [e for e in soup.find('li', class_='game-status') if len(e['class'])==1]
				m = re.search(r'\d{9}', str(links))
				team.append(tm.upper())
				ids.append(m.group(0))			
				scores.append(str(links.text))
				homeOrAway.append(str(homeAway.text))

	metadata = list(zip(team, ids, homeOrAway, scores))
	return metadata
	

############################ Get Play by Play Data ############################

def getPlays(quarterTable, scrapedTeam, gameID,  homeOrAway, quarterNumber):
		plays = []
		for play in quarterTable.find_all('tr')[1:]:
			row = [scrapedTeam, gameID, homeOrAway, quarterNumber]
			m = [play.find_all('td')[i] for i in [0,2,3]]
			for attr in m:
				row.append(str(attr.text))
			plays.append(row)
		with open("test.csv", "ab") as f:
			writer = csv.writer(f)
			writer.writerows(plays)
		print plays

##############################################################################

metadata = getGameMetadata()

for m in metadata:
	end = 4
	team = m[0]
	game_id = m[1]
	home_or_away = m[2]
	if " OT" in m[2]:
		end = 5
	if " 2OT" in m[2]:
		end = 6
	game_url = 'http://www.espn.com/nba/playbyplay?gameId={0}'.format(game_id)
	r = urllib.urlopen(game_url)
	soup = BeautifulSoup(r, "lxml")

	for i in range(1, end+1):
		q = soup.find_all('li', class_ = 'accordion-item')[i].find('table')
		getPlays(q, team, game_id, home_or_away, 'Q'+str(i))









