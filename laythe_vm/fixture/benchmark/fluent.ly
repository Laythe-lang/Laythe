import Math from 'std/math';
let rem = Math.rem;

let start = clock();

100000000.times()
  .filter(|x| rem(x, 3))
  .map(|x| (x * 4) - 2)
  .reduce(0, |acc, curr| acc + curr);

print clock() - start;
