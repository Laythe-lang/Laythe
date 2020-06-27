let large = [];
let small = [];

for (let i = 0; i < 10000; i = i + 1) {
  large.push(i);
}

for (let i = 0; i < 1000; i = i + 1) {
  small.push(i);
}

let start = clock();

for (let i in large) {
  let collection = [];
  for (let j in small) {
    collection.push(nil);
    collection[j] = j;
  }

  let sum = 0;
  for (let j in small) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print clock() - start;
