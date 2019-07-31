def primeNumbers1():
	primeNumbers = []
	for i in range(2,3000000):
		isPrime = True
		for n in primeNumbers:
			if(n>math.sqrt(i)):
				break
			if(i%n==0):
				isPrime = False
				break
		if(isPrime):
			primeNumbers.append(i)
	print(len(primeNumbers))
primeNumbers1()