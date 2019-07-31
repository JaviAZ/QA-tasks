import random
X_SIZE = 6 #should be 12
Y_SIZE = 6 #should be 12

class ship:
	'''	Constructor for ship object with arguements id and size, also sets x,y start and end 
		values to -1 and creates a list for hitmarkers of the size of the ship containing its id'''
	def __init__(self, id, size):
		self.id = str(id)
		self.size = size
		self.xstart = -1
		self.ystart = -1
		self.xend = -1
		self.yend = -1
		self.hitmarkers = []
		for i in range(0,size):
			self.hitmarkers.append(str(id))
	
	'''	getHit with arguements x and y is executed when the ship object gets shot at x,y position
		it sets the position in hitmarker where the ship got hit to * and returns -1, or all 
		hitmarkers to x if the whole ship has been shot down and return ship id'''
	def getHit(self, x, y):
		if(self.xstart == self.xend):
			if(self.ystart < self.yend):
				self.hitmarkers[y-self.ystart] = "*"
			else:
				self.hitmarkers[y-self.yend] = "*"
		else:
			if(self.xstart < self.xend):
				self.hitmarkers[x-self.xstart] = "*"
			else:
				self.hitmarkers[x-self.xend] = "*"
		if(self.id not in self.hitmarkers):
			print("Ship down!")
			for i in range(0,len(self.hitmarkers)):
				self.hitmarkers[i] = "x"
			return int(self.id)
		else:
			return -1
	
	'''	setPos with arguements for x and y, start and end points sets the ships points to those
		from the arguement'''
	def setPos(self, xstart, ystart, xend, yend):
		self.xstart = xstart
		self.ystart = ystart
		self.xend = xend
		self.yend = yend

class board:
	'''	Constructor for board with arguement xsize and ysize creates a 2D list of x columns and y
		rows filled with - setting up an empty board for the game'''
	def __init__(self, xsize, ysize):
		self.board = []
		for y in range(0,xsize):
			self.board.append([])
			for x in range(0,ysize):
				self.board[y].append("-")
	
	'''	emptyBoard resets all the cells in the board to its default -, used by the AI when adding
		ships, if it gets stucked because of the ships not being properly aligned in a small space'''
	def emptyBoard(self):
		for y in range(0,X_SIZE):
			for x in range(0,Y_SIZE):
				self.board[y][x] = "-"
	
	'''	addShip with argument shipiterates through the ship's x and y values and writes its id in 
		that position in the board'''
	def addShip(self,ship):
		for y in range(ship.ystart, ship.yend+1):
			for x in range(ship.xstart, ship.xend+1):
				self.board[y][x] = ship.id
	
	'''	printBoard will iterate through the rows in the board printing their content (could improve
		for nicer looking UI)'''
	def printBoard(self):
		for row in self.board:
			print(row)
	
	'''	get with arguements x and y returns the content of cell at row y, column x'''
	def get(self, x, y):
		return self.board[y][x]

	'''	set with arguements x, y and newval sets the cell at row y and column x to newval'''
	def set(self, x, y, newval):
		self.board[y][x] = newval

class player:
	'''	Constructor for player object with arguements name, board and oponent board also sets a
		number for ships'''
	def __init__(self, name, board, opboard):
		self.name = name
		self.board = board
		self.ships = []
		self.shipsLeft = 0
		self.oponentBoard = opboard
	
	'''	addShip with arguement ship adds a ship to the board and to the list of ships for the
		player, as well as increasing the ships left counter'''
	def addShip(self, ship):
		self.board.addShip(ship)
		self.ships.append(ship)
		self.shipsLeft += 1
	
	'''	sentShot with arguements x, y and val set the cell in row y and column x to val if it is 
		of length 1 (would be in the case of *) or iterates through val (list of points of a ship)
		marking each corresponding point in the oponent board with an x'''
	def sentShot(self, x, y, val):
		if(len(val) > 1):
			for coord in val:
				self.oponentBoard.set(coord[0],coord[1],"x")
		else:
			self.oponentBoard.set(x,y,val)
	
	'''	receiveShot with arguements x and y checks the content of cell in row y and column x and 
		updates it if necesary in the case of a missed shot or a hit on a ship. Returns * if just
		hit, returns list of x,y points if ship sinks'''
	def receiveShot(self, x, y):
		currVal = self.board.get(x,y)
		if(currVal == "-"):
			print("You missed me!")
			self.board.set(x, y, "o")
			return "o"
		elif(currVal == "*" or currVal == "o"):
			print("You already shot here!")
		else:
			print("You hit me!")
			shipid = self.ships[int(currVal)-1].getHit(x,y)-1
			currShip = self.ships[shipid]
			if(shipid > -1): #shipid is bigger than -1 if a ship with id:shipid is sunk
				self.shipsLeft -= 1
				hitPos = []
				#The following nested for loops iterate through the x,y values of the ship and marks the board with x
				for ysub in range (currShip.ystart,currShip.yend+1):
					for xsub in range (currShip.xstart,currShip.xend+1):
						self.board.set(xsub,ysub,"x")
						hitPos.append([xsub,ysub])
				return hitPos
			else:
				self.board.set(x, y, "*")
				return "*"

class ai(player):
	'''	Constructor with arguements name, board and oponent board for ai that extendes from player'''
	def __init__(self, name, board, opboard):
		super().__init__(name,board,opboard)
	
	'''	aoAddShips adds ships to the board randomly without them overlaping or going out of the range'''
	def aiAddShips(self,ships):
		shipn = 0
		counter = 0
		#Iterates through all the ships in the list
		while (shipn < len(ships)):
			#If after 100000 iterations with the same ship an correct place has not been found, reset board
			if(counter > 100000):
				for i in range(shipn,-1, -1):
					ships[shipn].setPos(-1,-1,-1,-1)
				shipn = 0
				counter = 0
				self.board.emptyBoard()
			#Randomly choose a point in the board
			xstart = random.randint(0,X_SIZE-1)
			ystart = random.randint(0,Y_SIZE-1)
			#Check if start point is available
			if(self.board.get(xstart,ystart) == "-"): 
				placed = False
				triedCoords = [1,2,3,4]
				#Check for movement north, east, south and west
				while (len(triedCoords) > 0):
					xend = xstart
					yend = ystart
					#Choose direction randomly
					direction = random.choice(triedCoords) 
					 #Move in that direction as long as the ship size
					for i in range(1,ships[shipn].size):
						if(direction == 1):
							yend = yend-1
						elif(direction == 2):
							xend = xend+1
						elif(direction == 3):
							yend = yend+1
						else:
							xend = xend-1
						#Catching index out of range exception in case it goes out of the map
						try: 
							#If the point is not available remove direction and try again
							if(self.board.get(xend,yend) != '-'):
								xend = -1
								triedCoords.remove(direction)
								break
						#If out of range remove direction and try again
						except Exception as e: 
							xend = -1
							triedCoords.remove(direction)
							break
					#If the for loop finished and is within range add ship
					if(yend > 0 and xend > 0 and yend < Y_SIZE and xend < X_SIZE): 
						if(xstart>xend):
							xstart,xend = xend,xstart
						if(ystart>yend):
							ystart,yend = yend,ystart
						ships[shipn].setPos(xstart,ystart,xend,yend)
						self.addShip(ships[shipn])
						shipn += 1
						counter = -1
						triedCoords.remove(direction)
						break
					#If not in range remove direction and try again
					elif(direction in triedCoords): 
						triedCoords.remove(direction)
			counter += 1

	'''	AIattacks will find a random point in the map and attack if it hasn't been attacked previously'''
	def AIattacks(self):
		xcoord, ycoord = -1, -1
		while(True):
			for row in range(0,len(self.board)):
				col = row.index("*")
			xcoord = random.randint(0,X_SIZE-1)
			ycoord = random.randint(0,Y_SIZE-1)
			if(self.oponentBoard.get(xcoord,ycoord) not in ["o","*","x"]):
				break
		return xcoord, ycoord



#Create list of ships for each player
p1Ships = [ship(1,2), ship(2,2), ship(3,3)]#, ship(4,3), ship(5,3), ship(6,4), ship(7,5)]
p2Ships = [ship(1,2), ship(2,2), ship(3,3)]#, ship(4,3), ship(5,3), ship(6,4), ship(7,5)]

#Create object for each player
p1 = ai("Player 1", board(X_SIZE,Y_SIZE), board(X_SIZE,Y_SIZE)) 
p2 = ai("AI", 		board(X_SIZE,Y_SIZE), board(X_SIZE,Y_SIZE)) 

#Ask for user input to place ships (needs validation and verification)
for ship in p1Ships:
	print(p1.name,"Where would you like to place -",ship.id,"of size:",ship.size)
	xstart, ystart = input("Start position: ").split(',')
	xend, yend = input("End position: ").split(',')
	ship.setPos(int(xstart),int(ystart),int(xend),int(yend))
	p1.addShip(ship)

###Predefined locations for testing
#p1Ships[0].setPos(0,0,1,0)
#p1Ships[1].setPos(3,3,3,4)
#p1Ships[2].setPos(2,5,4,5)
#for ship in p1Ships:
#	p1.addShip(ship)
###Predefined locations for testing

#Add ships to AI
#p1.aiAddShips(p1Ships)
p2.aiAddShips(p2Ships)

#Loop through game until there is a winner
i = 1
while(p1.shipsLeft>0 and p2.shipsLeft>0):
	print(p1.name,"board")
	p1.board.printBoard()
	print("Oponents board")
	p1.oponentBoard.printBoard()
	#Player 1 turn
	if(i % 2 != 0):
		print(p1.name, "Where would you like to shoot?")
		x, y = input("x, y: ").split(",")
		val = p2.receiveShot(int(x), int(y))
		p1.sentShot(int(x), int(y), val)
	#Player 2 turn
	else:
		input("AIs turn, please press enter.")
		x, y = p2.AIattacks()
		print("AI2 shoots at -",x,y)
		val = p1.receiveShot(x,y)
		p2.sentShot(x,y,val)
	i += 1

#Declare winner
if (p1.shipsLeft==0):
	print(p2.name,"is the winner!")
else:
	print(p1.name,"is the winner!")

#Show both boards after the game
print(p1.name,"board")
p1.board.printBoard()
print(p2.name,"board")
p2.board.printBoard()