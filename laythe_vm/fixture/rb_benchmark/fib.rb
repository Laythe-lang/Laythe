def fib(n)
  return n if n < 2
  fib(n - 2) + fib(n - 1)
end 

start = Time.new
puts fib(35) == 9227465
puts Time.new - start