let start = clock();

for (let i = 0; i < 10000; i = i + 1) {
  let collection = [];
  for (let j = 0; j < 1000; j = j + 1) {
    collection.push(nil);
    collection[j] = j;
  }

  let sum = 0;
  for (let j = 0; j < 1000; j = j + 1) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print clock() - start;
