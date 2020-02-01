import csv
import sys
import random

filename = sys.argv[1]
nodes = int(sys.argv[2])
minEdges = int(sys.argv[3])
maxEdges = int(sys.argv[4]) if len(sys.argv) == 5 else minEdges

class RandomGraph:

    def __init__(self, nodes, minEdges, maxEdges):
        self.nodes = range(1, nodes + 1)
        self.edges = [[] for _ in range(0, nodes)]
        for i in self.nodes:
            edges = random.randint(minEdges, maxEdges)
            for idx in range(0, edges):
                v2 = i
                while i == v2:
                    v2 = random.choice(self.nodes)
                time = random.randint(946684800, 1577836800)
                length = random.randint(3600, 43200)
                self.edges[i - 1].append((v2, time, length))
        
    def to_csv(self, filename):
        with open(filename, 'w') as csvfile:
            for i in self.nodes:
                for e in self.edges[i - 1]:
                    wr = csv.writer(csvfile, delimiter=',',
                                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
                    wr.writerow([i, e[0], e[1], e[2]])

rg = RandomGraph(nodes, minEdges, maxEdges)
rg.to_csv(filename)