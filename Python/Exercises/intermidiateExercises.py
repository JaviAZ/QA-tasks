import csv
import math
def blackJack(x, y):
	if(x>=y and x<=21 or x<y and y>21 and x<=21):
		return x
	elif(x<y and y<=21 or x>y and x>21 and y<=21):
		return y
	else: 
		return 0
#print(blackJack(2,13))
def uniqueSum(x,y,z):
	total = 0
	if (x!=y and x!=z):
		total+=x
	if (y!=x and y!=z):
		total+=y
	if (z!=x and z!=y):
		total+=z
	return total
#print(uniqueSum(10,1,10))
def tooHot(temp, isSummer):
	if(temp>=60 and temp<=90 or isSummer and temp>=60 and temp<=100):
		return True
	else:
		return False
#print(tooHot(100,True))
def leapYear(year):
	if(year%4==0):
		if(year%100==0):
			if(year%400==0):
				return True
			else:
				return False
		else:
			return True
	else:
		return False
#print(leapYear(2012))
def paintWizard(roomSize):
	optionsDic = [
	{"name":"CheapoMax","tinSize":10,"price":19.99},
	{"name":"AverageJoes","tinSize":11,"price":17.99},
	{"name":"DuluxourusPaints","tinSize":20,"price":25.00}]
	options = []
	if(roomSize%optionsDic[0]["tinSize"]==0):
		options.append(roomSize//optionsDic[0]["tinSize"]*optionsDic[0]["price"])
	else:
		options.append((roomSize//optionsDic[0]["tinSize"]+1)*optionsDic[0]["price"])
	if(roomSize%optionsDic[1]["tinSize"]==0):
		options.append(roomSize//optionsDic[1]["tinSize"]*optionsDic[1]["price"])
	else:
		options.append((roomSize//optionsDic[1]["tinSize"]+1)*optionsDic[1]["price"])
	if(roomSize%optionsDic[2]["tinSize"]==0):
		options.append(roomSize//optionsDic[2]["tinSize"]*optionsDic[2]["price"])
	else:
		options.append((roomSize//optionsDic[2]["tinSize"]+1)*optionsDic[2]["price"])
	print(optionsDic[options.index(min(options))]["name"])
#paintWizard(15)
def workingWithFiles():
	class person():
		def __init__(self,name,occupation,age):
			self.name = name
			self.occupation = occupation
			self.age = age
		def print(self):
			return (self.name,self.occupation,str(self.age))
	people = [person("Imran", "Trainee", 30), person("Javier", "Trainee", 20), person("Adrian", "Trainee", 21), person("Tom","Trainer", 30), person("Chris","Trainer",27)]

	with open('peopleTest.csv', mode = 'w', newline = '') as peopleTest:
		peopleTestWriter = csv.writer(peopleTest, delimiter=',')
		for p in people:
			peopleTestWriter.writerow([p.name,p.occupation,str(p.age)])
	people2 = []
	with open('peopleTest.csv', mode = 'r') as peopleTest:
		peopleTestReader = csv.reader(peopleTest, delimiter=',')
		for row in peopleTestReader:
			if(len(row)!=0):
				people2.append(person(row[0],row[1],int(row[2])))

	for p in people2:
		print(p.print())
#workingWithFiles()
def library():
	pass
	#cba rn
#library()
def primeNumbers1():
	A = []
	n=200000000
	primecount = 0

	for i in range(0,n):
		A.append(True)
	for i in range(2,int(math.sqrt(n))):
		if(A[i]):
			j=i**2
			while(j<n):
				A[j] = False
				j+=i
	for i in A:
		if(i):
			primecount+=1
	print(primecount)
#primeNumbers1()
def strings1(s1,s2):
	maxStr = ""
	for i in range(0,len(s1)):
		if(s1[i] in s2):
			idx = s2.index(s1[i])+1
			tempStr = s1[i]
			increase = 1
			for j in range(idx,len(s2)):
				if(i+increase<len(s1)):
					if(s2[j] == s1[i+increase]):
						tempStr +=s2[j]
					else:
						break
				increase += 1
			if(len(tempStr) > len(maxStr)):
				maxStr = tempStr
		else:
			pass
		
	print(maxStr)
#strings1("ldfhklawebhfhulawebwklawebwr", "afvadfgawebwrldfkfhulabhvhwesw")
def strings2(s1,s2):
	commonChar = ""
	start = 0
	for c1 in range(0,len(s1)):
		for c2 in range (start,len(s2)):
			if (s1[c1]==s2[c2]):
				commonChar+=s1[c1]
				start = c2
				break
	if(len(s1)>=len(s2)):
		print(len(s1)-len(commonChar))
	else:
		print(len(s2)-len(commonChar))	
#strings2("hello","wewhuirejknmlsddal")
def battleships():
	pass
#battleships()
