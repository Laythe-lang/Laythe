def fib(n)
  return n if n < 2
  fib(n - 2) + fib(n - 1)
end 

start = Time.new
puts fib(30) == 832040
puts Time.new - start