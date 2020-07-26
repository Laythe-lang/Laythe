let x = {
  "10": [1, 2, 3],
  false: {
    5: false,
    3: nil
  },
  true: "sup",
};

let str = x.str();

assert(str.has("true: 'sup'"));
assert(str.has("'10': [1, 2, 3]"));
assert(str.has("3: nil"));
assert(str.has("5: false"));

assertEq({}.str(), "{}");