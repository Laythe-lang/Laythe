import Math from 'std/math';

fun almost(actual, expect) {
  assert(Math.abs((actual - expect)) < 0.0000001);
}

almost(Math.cos(0), 1.0);
almost(Math.cos(Math.pi), -1.0);
almost(Math.cos(Math.pi * 2), 1.0);

almost(Math.cos(Math.pi / 2), 0.0);
almost(Math.cos(Math.pi * 3 / 2), 0.0);