

def g1 x = x + 1i32
def g2 x = g1 x + 2i32
def g3 x = g2 x + 3i32
def g4 x = g3 x + 4i32
def g5 x = g4 x + 5i32

entry f x = g5 x + 6i32