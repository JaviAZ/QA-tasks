import math
import time

A = []
n=3000000
primecount = 0

for i in range(0,n):
	A.append(True)
start_time = time.time()
for i in range(2,int(math.sqrt(n))):
	if(A[i]):
		j=i**2
		while(j<n):
			A[j] = False
			j+=i
end_time = time.time()
for i in A:
	if(i):
		primecount+=1
print(primecount)
print(str(end_time-start_time))