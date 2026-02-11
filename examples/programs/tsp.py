NUM_CITIES = 8
POP_SIZE = 100
MAX_GEN = 50

distance = [[0] * NUM_CITIES for _ in range(NUM_CITIES)]

population = [[0] * NUM_CITIES for _ in range(POP_SIZE)]
new_population = [[0] * NUM_CITIES for _ in range(POP_SIZE)]

seed = 12345678
def nextRand():
    global seed
    a = 1664525
    b = 1013904223
    c = 2147483647
    seed = (a * seed + b) % c
    if seed < 0:
        seed = -seed
    return seed

def fitness(chromosome):
    total = 0
    for i in range(NUM_CITIES - 1):
        total += distance[chromosome[i]][chromosome[i + 1]]
    total += distance[chromosome[NUM_CITIES - 1]][chromosome[0]]
    return total

def select_parent():
    a = nextRand() % POP_SIZE
    b = nextRand() % POP_SIZE
    if fitness(population[a]) < fitness(population[b]):
        return b
    else:
        return a

def crossover(p1, p2):
    child = [-1] * NUM_CITIES
    start = nextRand() % NUM_CITIES
    final_idx = nextRand() % NUM_CITIES
    if start > final_idx:
        start, final_idx = final_idx, start

    for i in range(start, final_idx + 1):
        child[i] = p1[i]

    idx = (final_idx + 1) % NUM_CITIES
    for city in p2:
        if city not in child[start:final_idx + 1]:
            child[idx] = city
            idx = (idx + 1) % NUM_CITIES
    return child

def mutate(chromosome, chance_per_gene):
    for i in range(NUM_CITIES):
        r = nextRand() % 100
        if r < chance_per_gene:
            j = nextRand() % NUM_CITIES
            chromosome[i], chromosome[j] = chromosome[j], chromosome[i]

def init_distance():
    for i in range(NUM_CITIES):
        for j in range(NUM_CITIES):
            if i == j:
                distance[i][j] = 0
            else:
                distance[i][j] = (nextRand() % 20) + 1

def init_population():
    for i in range(POP_SIZE):
        population[i] = list(range(NUM_CITIES))
        for k in range(NUM_CITIES - 1, 0, -1):
            rand_idx = nextRand() % (k + 1)
            population[i][k], population[i][rand_idx] = (
                population[i][rand_idx],
                population[i][k],
            )

def print_path(path):
    print(", ".join(map(str, path)))

def assign_1d(src, target):
    for i in range(len(src)):
        target[i] = src[i]

def assign_2d(src, target):
    for i in range(len(src)):
        for j in range(len(src[i])):
            target[i][j] = src[i][j]

init_distance()
init_population()

best_path = [0] * NUM_CITIES
best_distance = float("inf")

for gen in range(MAX_GEN):
    for i in range(POP_SIZE):
        d = fitness(population[i])
        if d < best_distance:
            best_distance = d
            assign_1d(population[i], best_path)
    print(f"Generation {gen} best distance = {best_distance} with best path: ", end="")
    print_path(best_path)

    for k in range(POP_SIZE // 2):
        p1 = select_parent()
        p2 = select_parent()
        child1 = crossover(population[p1], population[p2])
        child2 = crossover(population[p2], population[p1])
        mutate(child1, 5)
        mutate(child2, 5)
        assign_1d(child1, new_population[2 * k])
        assign_1d(child2, new_population[2 * k + 1])

    assign_1d(best_path, new_population[0])
    assign_2d(new_population, population)

print("Best distance:", best_distance)
print("Best path:", end=" ")
print_path(best_path)
