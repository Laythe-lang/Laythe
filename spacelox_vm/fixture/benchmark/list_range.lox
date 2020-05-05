var large = [];
var small = [];

for (var i = 0; i < 10000; i = i + 1) {
  large.push(i);
}

for (var i = 0; i < 1000; i = i + 1) {
  small.push(i);
}

var start = clock();

for (var i in large) {
  var collection = [];
  for (var j in small) {
    collection.push(nil);
    collection[j] = j;
  }

  var sum = 0;
  for (var j in small) {
    sum = sum + collection[1000 - j - 1];
    collection.pop();
  }
}

print clock() - start;
