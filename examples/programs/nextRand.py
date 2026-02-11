seed = 12345678910

def nextRand():
    global seed
    a = 1664525
    b = 1013904223
    c = 2147483647
    seed = (a * seed + b) % c
    if seed < 0:
        seed = -seed
    return seed

for iter in range(10):
    print(nextRand() % 100)
