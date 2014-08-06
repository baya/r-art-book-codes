# s3 employee 类
j <- list(name="Joe", salary=55000, union=T)
class(j) <- "employee"

print.employee <- function(wrkr){
  cat(wrkr$name, "\n")
  cat("salary", wrkr$salary, "\n")
  cat("union member", wrkr$union, "\n")
}

# 类的实例方法，范型函数
f.employee <- function(wrkr){
  cat("hello ", wrkr$name, "\n")
}

f <- function(x,...){
  UseMethod("f")
}

# 继承
k <- list(name="Kate", salary=68000, union=F, hrsthismonth=2)
class(k) <- c("hrlyemployee", "employee")