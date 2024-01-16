def read_board_from_file(file):
    with open (file, 'r') as bfile:
        board = dict()
        row = 0
        for line in bfile:
            for col, x in zip(range(9), line):
                if x in "123456789":
                    board[ (row, col) ] = int(x)
            row += 1
        return board

# read the board from standard input
# represent the board by a dictionary
def read_board():
    board = dict()
    for row in range(9):
        line = input()
        for col, x in zip(range(9), line):
            if x in "123456789":
                board[ (row, col) ] = int(x)
    return board

def print_board(board):
    for row in range(9):
        line = ""
        for col in range(9):
            line += str(board.get ((row, col), '-'))
        print (line)

def can_set(board, row, col, x):
    # which block
    brow = row // 3
    bcol = col // 3
    # check whether x already occurs in row or col
    for (r,c) , v in board.items():
        if v == x:
            if r == row: return False
            if c == col: return False
            if r // 3 == brow and c // 3 == bcol: return False    
    return True

def advance (row, col):
    col = (col + 1) % 9
    if col == 0:
        row = (row + 1) % 9
        if row == 0:
            return None
    return (row, col)

def find_solution(board, nrnc = (0,0)):
    if nrnc is None:
        # print_board( board) # .copy()
        yield board.copy()
    else:
        row, col = nrnc
        # print ("Trying " + str (nrnc)) # DEBUG
        nrnc = advance (row, col)
        if (row, col) in board:
            yield from find_solution (board, nrnc)
        else:
            for x in range(1,10):
                if can_set (board, row, col, x):
                    board[(row, col)] = x
                    yield from find_solution (board, nrnc)
                    del board[(row,col)]


def find_solution2(board, nrnc = (0,0)):
    while nrnc is not None and nrnc in board:
        nrnc = advance (*nrnc)
    if nrnc is None:
        # print_board( board) # .copy()
        yield board.copy()
    else:
        row, col = nrnc
        # print ("Trying " + str (nrnc)) # DEBUG
        nrnc = advance (row, col)
        for x in range(1,10):
            if can_set (board, row, col, x):
                board[(row, col)] = x
                yield from find_solution (board, nrnc)
                del board[(row,col)]

