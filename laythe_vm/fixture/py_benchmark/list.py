import time

start = time.time()

for i in range(10000):
  collection = []
  for j in range(1000):
    collection.append(None)
    collection[j] = j

  sum = 0
  for j in range(1000):
    sum = sum + collection[1000 - j - 1]
    collection.pop()

print(time.time() - start)
