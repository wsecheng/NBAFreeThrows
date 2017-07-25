from bs4 import BeautifulSoup
import urllib 
import re
import csv

##############################################
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

