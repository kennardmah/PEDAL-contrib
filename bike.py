import json
import urllib.request
url = 'https://data.boston.gov/api/3/action/datastore_search?resource_id=e4bfe397-6bfc-49c5-9367-c879fac7401d&limit=5&q=title:jones'  
fileobj = urllib.request.urlopen(url)
response_dict = json.loads(fileobj.read())
print(response_dict)