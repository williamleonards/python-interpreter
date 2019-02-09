# https://leetcode.com/problems/cheapest-flights-within-k-stops/description/
# Bellman ford
def findCheapestPrice(n, flights, src, dst, K):
    INF = 10000
    mn = [INF]*n
    mn[src] = 0
    
    for k in range(K+1):
        newmn = mn[:]
        for flight in flights:
            a = flight[0]
            b = flight[1]
            cost = flight[2]

            newmn[b] = min(newmn[b],mn[a]+cost)
        mn=newmn
    if mn[dst] != INF:
        return mn[dst]
    return -1

n = 3 
edges = [[0,1,100],[1,2,100],[0,2,500]]
src = 0 
dst = 2 
k = 0
assert(findCheapestPrice(n, edges, src, dst, k) == 500)