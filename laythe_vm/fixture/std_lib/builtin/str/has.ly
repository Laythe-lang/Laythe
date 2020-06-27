let example = "four score and seven years ago";

assert(example.has("four"));
assert(example.has("n years"));
assert(!example.has("Score"));
assert(!example.has("basketball"));