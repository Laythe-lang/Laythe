let start = clock();

for (let i in 100000.times()) {
  let collection = [];
  for (let j in 1000.times()) {
    collection.push(nil);
    collection[j] = j;
  }

  let sum = 0;
  for (let j in 1000.times()) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print(clock() - start);
