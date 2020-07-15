import Math from 'std/math';

for (let _ in 10.times()) {
  let rand = Math.rand();
  assert(rand >= 0 and rand < 1);
}