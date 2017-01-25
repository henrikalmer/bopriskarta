import json
import operator
from pprint import pprint

with open('data/areas.json') as f:
    areas = json.load(f)

def area2link(a):
	link = u"<a href=\"https://www.booli.se/{}/\">{}</a>".format(a['id'], a['name'])
	return link

links = []
for alist in areas:
	sorted_list = sorted(alist, key=operator.itemgetter('size'))
	a_html = u"<br>".join([area2link(a) for a in sorted_list if a['type'] in ('Kommun', 'undefined',)])
	links.append(a_html)

with open('data/arealinks.json', 'w') as f:
	f.write(json.dumps(links))
