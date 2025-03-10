# basic R
# for use in IAEA fellowship training, Feb 2025
# R. Picha, TINT RD

# the lines with # in front are comment. you can write anything here.
# it is useful to use this to describe each part of the script


# define variables
a <- 10
b <- 5
c <- 2

print(paste0('a=', a, ', b=', b, ', c=', c))


# addition
sum <- a + b
print(sum)
print(paste0('Sum: ', sum))

# subtraction
diff <- a - b
print(paste0('Difference: ', diff))
print(paste0("difference between a and b: ", diff))

# multiplication
prod <- a * b * c
print(paste0('Product: ', prod))

# division
quot <- a / b
print(paste0('Quotient: ', quot))

# exponentiation
exp <- b^c
print(paste0('Exponentiation: ', exp))

# square root
sqrt_a <- sqrt(a)
print(paste0('Square root of a: ', sqrt_a))

# modulus (remainder)
modu <- b %% c
print(paste0('Modulus: ', modu))

# integer division
int_div <- b %/% c
print(paste0('Integer division: ', int_div))

# more complex operation
print(a^c - 10*b - c)


print('Vectors:')
print('a vector can contain any type of variables, but must be of same type.')
print('c = concatenate function')

# vector of numbers
v1 <- c(1, 2, 4, 6, 9)
print(v1)

v2 <- c(3, 5, 2, 1, 8)

# adding two vectors
print(v1 + v2)


# vector of strings
v3 <- c('Laos', 'Fiji', 'Myanmar', 'Cambodia', 'Thailand')
print(v3)

# print each element
print(v3[3])


# boolean data (true or false)
# logic
# greater than >
# less than <
# equal ==

v4 <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
print(v4)

print(2 > 1)
print(5 == 6)
print(10 + 5 > 20)


# changing variable data type
v5 <- 5
print(v5)

v5 <- 'my apples'
print(v5)


# basic plots
x <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
y <- c(95, 87, 75, 65, 52, 43, 31, 25, 17)

plot(x, y)

hist(y)
