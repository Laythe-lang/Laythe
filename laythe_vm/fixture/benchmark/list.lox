var start = clock();

for (var i = 0; i < 10000; i = i + 1) {
  var collection = [];
  for (var j = 0; j < 1000; j = j + 1) {
    collection.push(nil);
    collection[j] = j;
  }

  var sum = 0;
  for (var j = 0; j < 1000; j = j + 1) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print clock() - start;
