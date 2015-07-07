context("iMath Operations")

# Binary morphological operations
test_that("iMath MD", {
  img<-antsImageRead( getANTsRData("rsboldmask") )
  img<-iMath(img, "MD", 1)
  expect_true(sum(img) == 45544)
})

test_that("iMath ME", {
  img<-antsImageRead( getANTsRData("rsboldmask") )
  img<-iMath(img, "ME", 1)
  expect_true(sum(img) > 0)
})

test_that("iMath MO", {
  img<-antsImageRead( getANTsRData("rsboldmask") )
  img<-iMath(img, "MO", 1)
  expect_true(sum(img) == 37385)
})

test_that("iMath MC", {
  img<-antsImageRead( getANTsRData("rsboldmask") )
  img<-iMath(img, "MC", 1)
  expect_true(sum(img) == 37834)
})

# Grayscale morphological operations
test_that("iMath GD", {
  img<-antsImageRead( getANTsRData("r16") )
  img<-iMath(img, "GD", 10)
  expect_true(sum(img) == 5328430)
})

test_that("iMath GE", {
  img<-antsImageRead( getANTsRData("r16") )
  img<-iMath(img, "GE", 10)
  expect_true(sum(img) == 1094198)
})

test_that("iMath GO", {
  img<-antsImageRead( getANTsRData("r16") )
  img<-iMath(img, "GO", 10)
  expect_true(sum(img) == 2412733)
})

test_that("iMath GC", {
  img<-antsImageRead( getANTsRData("r16") )
  img<-iMath(img, "GC", 10)
  expect_true(sum(img) == 3913893)
})
