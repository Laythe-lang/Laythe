# frozen_string_literal: true

loopStart = Time.new

10000000.times do |_|
  1; 1; 1; 2; 1; nil; 1; "str"; 1; true;
  nil; nil; nil; 1; nil; "str"; nil; true;
  true; true; true; 1; true; false; true; "str"; true; nil;
  "str"; "str"; "str"; "stru"; "str"; 1; "str"; nil; "str"; true;
end

loopTime = Time.new - loopStart

start = Time.new

10000000.times do |_|
  1 == 1; 1 == 2; 1 == nil; 1 == "str"; 1 == true;
  nil == nil; nil == 1; nil == "str"; nil == true;
  true == true; true == 1; true == false; true == "str"; true == nil;
  "str" == "str"; "str" == "stru"; "str" == 1; "str" == nil; "str" == true;
end

elapsed = Time.new - start;
puts "loop"
puts loopTime
puts "elapsed"
puts elapsed
puts "equals"
puts elapsed - loopTime
