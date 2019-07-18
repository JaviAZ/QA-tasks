board = [["-","-","-","*"],
["-","*","*","-"],
["*","-","-","-"],
["*","-","-","-"]]

for row in range(0,len(board)):
	col = [i for i, x in enumerate(board[row]) if x == "*"]
	print(col)
	if(len(col) > 1):
		if(col[0]-1 >= 0):
			if(board[row][col-1] == "-"):
				print("Shoot left")
				break
			elif(board[row][col-1] == "*" and col[0]-2 >= 0):
				print("Shoot 2 left")
				break
		if(col[len(col)-1]+1 < len(board[0])):
			if(board[row][col+1] == "-"):
				print("Shoot right")
				break
			elif(board[row][col+1] == "*" and col[len(col)-1]+2 < len(board[0])):
				print("Shoot 2 right")
				break
		
