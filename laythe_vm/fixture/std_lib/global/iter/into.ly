fn makeIter() {
  return 10.times();
}

fn sum(iter) {
  let sum = 0;
  for (let i in iter) {
    sum = sum + i;
  }

  return sum;
}

let intoList = makeIter().into(List.collect);
let sum = makeIter().into(sum);

assertEq(intoList[4], 4);
assertEq(sum, 45);