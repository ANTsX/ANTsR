library(ANTsRCore)
context("antsImage Basic Operations")

# Use pixeltype=="double" for tests, if using the default pixeltype=="float"
# then all test will fail due loss of precision
set.seed(20170525)

varvalues = rnorm(100)
varimg = makeImage(c(10, 10), varvalues, pixeltype = "double")  # this get changed
values = rnorm(100)
img = makeImage(c(10, 10), values, pixeltype = "double")  # this get changed
valuesm = c( -49:50 )
img1 = makeImage(c(10, 10), valuesm, pixeltype = "double") # this is constant

arr = array( rnorm(prod(dim(img1))), dim = dim(img1))

values2 = rnorm(100)
img2 = makeImage(c(10, 10), values2, pixeltype = "double")
values3 = abs(values2)
img3 = makeImage(c(10, 10), values3, pixeltype = "double")

test_that("makeImage creates antsImage", {
  expect_true(is.antsImage(img))
})


test_that("Comparisons give back antsImages", {
  expect_true(is.antsImage(img1 > 5))
  expect_true(is.antsImage(img1 >= 5))
  expect_true(is.antsImage(img1 <= 5))
  expect_true(is.antsImage(img1 < 5))
  expect_true(is.antsImage(img1 < 5 & img1 > 2))
  expect_true(is.antsImage( -img1))
})

test_that("Array operations give back antsImages", {
  expect_true(is.antsImage( img1 + arr))
  expect_true(is.antsImage( arr + img ))
  expect_true(is.antsImage( img1 == arr))
})

test_that("Masks are in summary measures", {
  expect_equal( mean(img1), 0.5, tolerance = 1.e-6)
  expect_equal( mean(img1, mask = img1 > 0), 25.5, tolerance = 0.01)
  expect_silent( sum( img1 ) )
  expect_silent( sum( img1, mask = img1 > 4 ) )

  expect_warning(all(img1))
  expect_false(all(img1 > max(img1)))
  expect_silent(all(coerce_mask(img1 > max(img1))))

  expect_silent(prod(img1))
  expect_silent(prod(img1, mask = img1 > 1))

  expect_silent(range(img1))
  expect_silent(range(img1, mask = img1 > 1))

  #nothign greater than max of img1
  expect_warning(range(img1, mask = img1 > max(img1)))
})

test_that("dim of antsImage", {
  expect_true(all(dim(img) == c(10, 10)))
})

test_that("dim of as.array", {
  expect_true(all(dim(as.array(img)) == c(10, 10)))
})

test_that("mean of antsImage", {
  expect_true(mean(img) == mean(values))
})

test_that("max of antsImage", {
  expect_true(max(img) == max(values))
})

test_that("min of antsImage", {
  expect_true(min(img) == min(values))
})

test_that("var of antsImage", {
  expect_s4_class(varimg, "antsImage")  
  expect_is(varvalues, "numeric")
  # print(search())
  # print(var)  
  expect_equal( ANTsRCore::var(varimg), var(varvalues), tolerance = 1.e-7 )
  expect_equal( var(as.numeric(varimg)), var(varvalues), tolerance = 1.e-7 )
  # expect_equal( var(varimg), var(varvalues), tolerance = 1.e-7 )
})

test_that("sd of antsImage", {
  expect_true(sd(img) == sd(values))
})

test_that("[] returns correct value", {
  expect_true(values[2] == img[2, 1])
})

test_that("[] sets correct value", {
  val = rnorm(1)
  img[3, 1] = val
  expect_true(img[3, 1] == val)
})

test_that("antsSetSpacing works", {
  expect_true(antsSetSpacing(img, c(2, 3)) == 0)
})

test_that("antsSetOrigin works", {
  expect_true(antsSetOrigin(img, c(4, 5)) == 0)
})

test_that("antsSetDirection works", {
  expect_true(antsSetDirection(img,-diag(2)) == 0)
})

test_that("antsGetSpacing works", {
  expect_true(all(antsGetSpacing(img) == c(2, 3)))
})

test_that("antsGetOrigin works", {
  expect_true(all(antsGetOrigin(img) == c(4, 5)) )
})

test_that("antsGetDirection works", {
  expect_true(all(antsGetDirection(img) == -diag(2)) )
})

test_that("antsImage + scalar", {
  expect_true(is.antsImage(img1 + 2))
  expect_true(all(sum(img1 + 2) == sum(valuesm + 2)))
})
test_that("antsImage + scalar, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img +
                                                  2))
})

test_that("antsImage + antsImage", {
  expect_true(is.antsImage(img1 + img2))
  expect_true(sum(img1 + img2) == sum(values2 + valuesm))
})
test_that("antsImage + antsImage, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE,
                                                img, img + img))
})

test_that("antsImage * scalar", {
  expect_true(is.antsImage(img1 * 2))

  expect_true(sum(img1 * 2) == sum(valuesm * 2))
})
test_that("antsImage * scalar, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img *
                                                  2))
})

test_that("antsImage * antsImage", {
  expect_true(sum(img1 * img2) == sum(valuesm * values2))
})
test_that("antsImage * antsImage, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img *
                                                  img))
})

test_that("antsImage / scalar", {
  expect_true(sum(img1 / 2) == sum(valuesm / 2))
})
test_that("antsImage / scalar, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img /
                                                  2))
})

test_that("antsImage / antsImage", {
  expect_true(sum(img1 / img3) == sum(valuesm / values3))
})
test_that("antsImage / antsImage, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img1, img1 /
                                                  img3))
})

test_that("antsImage ^ scalar", {
  expect_true(sum(img1 ^ 2) == sum(valuesm ^ 2))
})
test_that("antsImage ^ scalar, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img ^
                                                  2))
})

test_that("antsImage ^ antsImage", {
  expect_true(sum(img3 ^ img3) == sum(values3 ^ values3))
})
test_that("antsImage ^ antsImage, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img3, img3 ^
                                                  img3))
})

test_that("antsImage ^ scalar", {
  expect_true(sum(img1 %% 2) == sum(valuesm %% 2))
})
test_that("antsImage %% scalar, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img %%
                                                  2))
})

test_that("antsImage %% antsImage", {
  expect_true(sum(img1 %% img2) == sum(valuesm %% values2))
})
test_that("antsImage %% antsImage, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, img %%
                                                  img))
})

test_that("antsImage log", {
  expect_true(sum(log(img3)) == sum(log(values3)))
})
test_that("antsImage log, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img3, log(img3)))
})

test_that("antsImage exp", {
  expect_true(sum(exp(img1)) == sum(exp(valuesm)))
})
test_that("antsImage exp, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, exp(img)))
})

test_that("antsImage abs", {
  expect_true(sum(abs(img1)) == sum(abs(valuesm)))
})
test_that("antsImage exp, preserves header", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, img, abs(img)))
})


#
# Multi channel images
#

mvalues = c(valuesm, values2)
mvalues3 = c(values3, values3)
mimg = mergeChannels(list(img1, img2))
mimg1 = mergeChannels(list(img1, img2))
mimg2 = mergeChannels(list(img1, img2))
mimg3 = mergeChannels(list(img3, img3))

test_that("mergeChannels creates antsImage", {
  expect_true(is.antsImage(mimg))
})

test_that("dim of multichannel antsImage", {
  expect_true(sum(dim(mimg) == c(10, 10)) == 2)
})

test_that("dim of as.array on multichannel", {
  expect_true(all(dim(as.array(mimg)) == c(2, 10, 10)))
})

test_that("mean of multichannel antsImage", {
  expect_equal(mean(mimg), mean(mvalues), tolerance = 1.e-7)
})

test_that("max of multichannel antsImage", {
  expect_true(max(mimg) == max(mvalues))
})

test_that("min of multichannel antsImage", {
  expect_true(min(mimg) == min(mvalues))
})

test_that("var of multichannel antsImage", {
  expect_equal(ANTsRCore::var(mimg), var(mvalues), tolerance = 1.e-7)
})

test_that("sd of multichannel antsImage", {
  expect_equal(sd(mimg), sd(mvalues), tolerance = 1.e-7)
})

test_that("[] returns correct value", {
  expect_true(sum(c(valuesm[2], values2[2]) == mimg[2, 1]) == 2)
})

test_that("antsSetSpacing works on multichannel", {
  expect_true(antsSetSpacing(mimg, c(2, 3)) == 0)
})

test_that("antsSetOrigin works on multichannel", {
  expect_true(antsSetOrigin(mimg, c(4, 5)) == 0)
})

test_that("antsSetDirection works on multichannel", {
  expect_true(antsSetDirection(mimg,-diag(2)) == 0)
})

test_that("antsGetSpacing works on multichannel", {
  expect_true(sum(antsGetSpacing(mimg) == c(2, 3)) == 2)
})

test_that("antsGetOrigin works on multichannel", {
  expect_true(sum(antsGetOrigin(mimg) == c(4, 5)) == 2)
})

test_that("antsGetDirection works on multichannel", {
  expect_true(sum(antsGetDirection(mimg) == -diag(2)) == 4)
})

test_that("antsImage + scalar on multichannel", {
  expect_true(sum(mimg + 2) == sum(mvalues + 2))
})
test_that("antsImage + scalar, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg1, mimg1 + 2))
})

test_that("antsImage + antsImage on multichannel", {
  expect_true(sum(mimg1 + mimg2) == sum(mvalues + mvalues))
})
test_that("antsImage /+ antsImage, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg1, mimg1 +
                                                  mimg3))
})

test_that("antsImage * scalar on multichannel", {
  expect_true(sum(mimg1 * 2) == sum(mvalues * 2))
})
test_that("antsImage * scalar, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg1, mimg1 * 2))
})

test_that("antsImage * antsImage on multichannel", {
  expect_true(sum(mimg1 * mimg2) == sum(mvalues * mvalues))
})
test_that("antsImage * antsImage, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg1, mimg1 *
                                                  mimg3))
})

test_that("antsImage / scalar on multichannel", {
  expect_true(sum(mimg1 / 2) == sum(mvalues / 2))
})
test_that("antsImage / scalar, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg, mimg / 2))
})

test_that("antsImage / antsImage on multichannel", {
  expect_true(sum(mimg1 / mimg3) == sum(mvalues / mvalues3))
})
test_that("antsImage / antsImage, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg1, mimg1 /
                                                  mimg3))
})

test_that("antsImage ^ scalar on multichannel", {
  expect_true(sum(mimg3 ^ 2) == sum(mvalues3 ^ 2))
})
test_that("antsImage ^ scalar, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, mimg3 ^
                                                  2))
})

test_that("antsImage ^ antsImage on multichannel", {
  expect_true(sum(mimg3 ^ mimg3) == sum(mvalues3 ^ mvalues3))
})
test_that("antsImage ^ antsImage, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, mimg3 ^
                                                  mimg3))
})

test_that("antsImage ^ scalar on multichannel", {
  expect_true(sum(mimg3 %% 2) == sum(mvalues3 %% 2))
})
test_that("antsImage %% scalar, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, mimg3 %%
                                                  2))
})

test_that("antsImage %% antsImage on multichannel", {
  expect_true(sum(mimg3 %% mimg3) == sum(mvalues3 %% mvalues3))
})
test_that("antsImage %% antsImage, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, mimg3 %%
                                                  mimg3))
})

test_that("antsImage log on multichannel", {
  expect_true(sum(log(mimg3)) == sum(log(mvalues3)))
})
test_that("antsImage log, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, log(mimg3)))
})

test_that("antsImage exp on multichannel", {
  expect_true(sum(exp(mimg3)) == sum(exp(mvalues3)))
})
test_that("antsImage exp, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg3, exp(mimg3)))
})

test_that("antsImage abs on multichannel", {
  expect_true(sum(abs(mimg1)) == sum(abs(mvalues)))
})
test_that("antsImage exp, preserves header on multichannel", {
  expect_true(antsImagePhysicalSpaceConsistency(data.type = TRUE, mimg, abs(mimg)))
})
