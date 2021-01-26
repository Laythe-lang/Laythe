import time

concatStart = time.time()
for _ in range(10000000):
    a = 'cat'
    b = 'dog'
    c = 'parrot'
    d = 'rabbit'
    e = 'deer'
    f = 'bear'
    g = 'coyote'
    h = 'squirrl'
    i = 'fish'
    j = 'monkey'

    k = a + b + c + d + e + f + g + h + i + j

concatTime = time.time() - concatStart

interopStart = time.time()

for _ in range(10000000):
    a = 'cat'
    b = 'dog'
    c = 'parrot'
    d = 'rabbit'
    e = 'deer'
    f = 'bear'
    g = 'coyote'
    h = 'squirrl'
    i = 'fish'
    j = 'monkey'

    k = f'{a}{b}{c}{d}{e}{f}{g}{h}{i}{j}'

interopTime = time.time() - interopStart
print("concat")
print(concatTime)
print("interpolation")
print(interopTime)