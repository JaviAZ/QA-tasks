from pymongo import MongoClient
client = MongoClient(host = "localhost", port = 27017)
my_db = client["zipsdb"]
my_colleciton = my_db["zips"]

zips_data = my_colleciton.find()

for a in zips_data:
	print(a)

client.close()