# Plots made while no graphics device is open would open the default device,
# which writes Rplots.pdf into the working directory (tests/testthat when run
# with devtools/testthat, or wherever the tests are started). Send them to
# tempdir() instead.
options(device = function(...) {
  grDevices::pdf(file = file.path(tempdir(), "Rplots.pdf"), ...)
})
