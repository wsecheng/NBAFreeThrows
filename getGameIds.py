from bs4 import BeautifulSoup
import urllib 
import re
import time
#import MySQLdb


ids = []
scores = []
teams = ['cle']

for tm in teams:
	url = "http://www.espn.com/nba/team/schedule/_/name/{0}/seasontype/2".format(tm)

	r = urllib.urlopen(url)
	soup = BeautifulSoup(r, "lxml")

	links = soup.find_all("a", href=re.compile("recap"))
	for l in links:
		m = re.search(r'\d{9}', str(l))
		ids.append(m.group())
		scores.append(str(l.text))

hi = list(zip(ids,scores))
