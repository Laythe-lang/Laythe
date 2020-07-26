let someString = 'someString';
let expected = [
  's', 'o', 'm', 'e',
  'S', 't', 'r', 'i', 'n', 'g'
];

for (let pair in someString.iter().zip(expected.iter())) {
  assertEq(pair[0], pair[1]);
}