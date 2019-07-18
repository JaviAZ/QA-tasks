import math
def primecounter(n):
	primes = [2,3,5]
	for i in range(7,n,2):
		for j in primes:
			if(i%j==0):
				break
			if(j>math.sqrt(i)):
				primes.append(i)
				break
	print(len(primes))
primecounter(3000000)