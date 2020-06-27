import Math from 'std/math';

fn almost(actual, expect) {
  assert(Math.abs((actual - expect)) < 0.0000001);
}

almost(Math.sin(0), 0.0);
almost(Math.sin(Math.pi), 0.0);
almost(Math.sin(Math.pi * 2), 0.0);

almost(Math.sin(Math.pi / 2), 1.0);
almost(Math.sin(Math.pi * 3 / 2), -1.0);