def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
}

def getHead(as: Array[Int]) = {
    as(0)
}

def plusTen(x: Int) = {
    x + 10
}

val as = Array(1,2,3)

val getHeadAndPlusTen = compose(plusTen, getHead)

println(getHeadAndPlusTen(as))