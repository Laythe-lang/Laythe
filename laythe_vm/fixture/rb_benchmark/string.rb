# frozen_string_literal: true
concatStart = Time.new

10000000.times() do |i|
  a = "cat"
  b = "dog"
  c = "parrot"
  d = "rabbit"
  e = "deer"
  f = "bear"
  g = "coyote"
  h = "squirrl"
  i = "fish"
  j = "monkey"

  k = a + b + c + d + e + f + g + h + i + j;
end

concatTime = Time.new - concatStart;

interopStart = Time.new;

10000000.times() do |i|
  a = "cat"
  b = "dog"
  c = "parrot"
  d = "rabbit"
  e = "deer"
  f = "bear"
  g = "coyote"
  h = "squirrl"
  i = "fish"
  j = "monkey"

  k = "#{a}#{b}#{c}#{d}#{e}#{f}#{g}#{h}#{i}#{j}"
end

interopTime = Time.new - interopStart;
puts "concat"
puts concatTime
puts "interpolation"
puts interopTime