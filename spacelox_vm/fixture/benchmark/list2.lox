var start = clock();

for (var i in 100000.times()) {
  var collection = [];
  for (var j in 1000.times()) {
    collection.push(nil);
    collection[j] = j;
  }

  var sum = 0;
  for (var j in 1000.times()) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print clock() - start;
