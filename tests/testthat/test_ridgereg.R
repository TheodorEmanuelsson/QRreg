context("ridgereg")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda = 1))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda = 1))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = "1"))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, use_QR = 50))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("print() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, use_QR = FALSE)
  
  expect_output(print(ridgereg_mod),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 2\\)")
})

test_that("predict() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  ridgereg_mod2 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, use_QR = TRUE)
  expect_equal(round(unname(predict(ridgereg_mod)[c(1,5,7)]),2), c(1.82, 1.51, 1.07))    
  expect_equal(round(unname(predict(ridgereg_mod2)[c(1,5,7)]),2), c(1.82, 1.51, 1.08))
  #expect_equal(round(unname(predict(ridgereg_mod, newdata = iris[c(2,1)])[c(1,5,7)]),2), c(1.82, 1.51, 1.07))
})


test_that("coef() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  ridgereg_mod2 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2, use_QR = TRUE)
  iris.scaled <- iris
  iris.scaled$Sepal.Width <- scale(iris.scaled$Sepal.Width)
  iris.scaled$Sepal.Length <- scale(iris.scaled$Sepal.Length)
  ridgereg_MASS <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris.scaled, lambda = 2)
  
  expect_true(all(ceiling(coef(ridgereg_mod)) == ceiling(coef(ridgereg_MASS)))) # Similar to lm.ridge
  expect_true(all(ceiling(coef(ridgereg_mod2)) == ceiling(coef(ridgereg_MASS))))# Similar to lm.ridge
  expect_true(all(round(coef(ridgereg_mod),2) == round(coef(ridgereg_mod2), 2))) # Same coefficients
})

