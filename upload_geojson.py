import json
from mapbox import Datasets

with open('data/geojson/apts-sqmprice-polygons.final.geojson') as f:
	geojson = json.loads(f.read())

datasets = Datasets()
dataset_id = 'civi1pnjp002o2op4ghvhvcma'
for feat in geojson['features']:
	feat['id'] = str(feat['id'])
	resp = datasets.update_feature(dataset_id, feat['id'], feat)
	if resp.status_code != 200:
		print resp.status_code
		print resp._content
		break
	else:
		print "Added feature {} of {}".format(feat['id'], len(geojson['features']))
