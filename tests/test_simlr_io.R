library(ANTsR)

#' Test roundtrip of SiMLR write/read
#'
#' Writes a SiMLR object to disk, reads it back, and compares the original and
#' reconstructed objects for equality in structure, type, and values. Skips NULL
#' components and prints only the first mismatch with an explicit reason for failure.
#'
#' @param simlr_object A named list containing SiMLR outputs.
#' @param file_prefix A character string for the output directory prefix.
#'
#' @return Logical: TRUE if roundtrip is successful, FALSE otherwise.
test_simlr_roundtrip <- function(simlr_object, file_prefix) {
  # Write and read
  write_simlr(simlr_object, file_prefix)
  reconstructed <- read_simlr(paste0(file_prefix, "_simlr"))
  
  # Recursive comparison function
  compare_objects <- function(orig, recon, path = "root") {
    # Skip NULL components
    if (is.null(orig) && is.null(recon)) {
      cat(sprintf("Skipping NULL component at %s\n", path))
      return(TRUE)
    }
    if (is.null(orig) != is.null(recon)) {
      cat(sprintf("Mismatch at %s: one object is NULL (original=%s, reconstructed=%s)\n",
                  path, ifelse(is.null(orig), "NULL", "non-NULL"), ifelse(is.null(recon), "NULL", "non-NULL")))
      cat(sprintf("Reason for failure: NULL mismatch\n"))
      return(FALSE)
    }
    
    # Normalize tbl_df to data.frame for comparison
    orig_class <- class(orig)
    recon_class <- class(recon)
    if ("tbl_df" %in% orig_class && "data.frame" %in% recon_class) {
      orig_class <- c("data.frame", setdiff(orig_class, c("tbl_df", "tbl")))
      recon_class <- c("data.frame", setdiff(recon_class, "data.frame"))
    }
    
    # Check type mismatch
    if (!identical(orig_class, recon_class)) {
      cat(sprintf("Mismatch at %s: original type=%s, reconstructed type=%s\n",
                  path, paste(orig_class, collapse=", "), paste(recon_class, collapse=", ")))
      cat(sprintf("Reason for failure: Type mismatch\n"))
      return(FALSE)
    }
    
    # Handle lists (excluding data.frames)
    if (is.list(orig) && !is.data.frame(orig)) {
      orig_names <- names(orig) %||% character(0)
      recon_names <- names(recon) %||% character(0)
      if (!identical(orig_names, recon_names)) {
        cat(sprintf("Mismatch at %s: original names=%s, reconstructed names=%s\n",
                    path, paste(orig_names, collapse=", "), paste(recon_names, collapse=", ")))
        cat(sprintf("Reason for failure: Name mismatch\n"))
        return(FALSE)
      }
      result <- TRUE
      for (n in orig_names) {
        subpath <- paste0(path, "$", n)
        # Skip NULL components in lists
        if (is.null(orig[[n]]) && is.null(recon[[n]])) {
          cat(sprintf("Skipping NULL component at %s\n", subpath))
          next
        }
        if (!compare_objects(orig[[n]], recon[[n]], subpath)) {
          result <- FALSE
          break  # Stop at first mismatch
        }
      }
      return(result)
    }
    
    # Handle data.frames and matrices
    if (is.data.frame(orig) || is.matrix(orig)) {
      if (!identical(dim(orig), dim(recon))) {
        cat(sprintf("Mismatch at %s: original dims=%s, reconstructed dims=%s\n",
                    path, paste(dim(orig), collapse="x"), paste(dim(recon), collapse="x")))
        cat(sprintf("Reason for failure: Dimension mismatch\n"))
        return(FALSE)
      }
      orig_rownames <- rownames(orig) %||% character(0)
      recon_rownames <- rownames(recon) %||% character(0)
      if (!identical(orig_rownames, recon_rownames)) {
        cat(sprintf("Mismatch at %s: original row names=%s, reconstructed row names=%s\n",
                    path, paste(orig_rownames, collapse=", "), paste(recon_rownames, collapse=", ")))
        cat(sprintf("Reason for failure: Row names mismatch\n"))
        return(FALSE)
      }
      if (is.data.frame(orig)) {
        orig_colnames <- colnames(orig) %||% character(0)
        recon_colnames <- colnames(recon) %||% character(0)
        if (!identical(orig_colnames, recon_colnames)) {
          cat(sprintf("Mismatch at %s: original column names=%s, reconstructed column names=%s\n",
                      path, paste(orig_colnames, collapse=", "), paste(recon_colnames, collapse=", ")))
          cat(sprintf("Reason for failure: Column names mismatch\n"))
          return(FALSE)
        }
      }
      if (!all.equal(orig, recon, check.attributes = FALSE, tolerance = 1e-6)) {
        diff_idx <- which(orig != recon, arr.ind = TRUE)
        for (i in seq_len(min(nrow(diff_idx), 5))) {  # Limit to first 5 differences
          r <- if (is.data.frame(orig)) diff_idx[i, "row"] else orig_rownames[diff_idx[i, 1]]
          c <- if (is.data.frame(orig)) colnames(orig)[diff_idx[i, "col"]] else diff_idx[i, 2]
          cat(sprintf("Mismatch at %s[%s,%s]: original=%s, reconstructed=%s\n",
                      path, r, c, orig[diff_idx[i, 1], diff_idx[i, 2]], recon[diff_idx[i, 1], diff_idx[i, 2]]))
        }
        if (nrow(diff_idx) > 5) {
          cat(sprintf("... and %d more value mismatches\n", nrow(diff_idx) - 5))
        }
        cat(sprintf("Reason for failure: Value mismatch\n"))
        return(FALSE)
      }
      return(TRUE)
    }
    
    # Handle other types
    if (!all.equal(orig, recon, tolerance = 1e-6)) {
      cat(sprintf("Mismatch at %s: original=%s, reconstructed=%s\n",
                  path, deparse(orig), deparse(recon)))
      cat(sprintf("Reason for failure: Value mismatch\n"))
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Log root-level names for debugging
  cat(sprintf("Comparing root names: original=%s, reconstructed=%s\n",
              paste(names(simlr_object) %||% character(0), collapse=", "),
              paste(names(reconstructed) %||% character(0), collapse=", ")))
  
  result <- compare_objects(simlr_object, reconstructed)
  if (result) {
    cat("Roundtrip test passed: original and reconstructed objects are identical\n")
  }
  return(result)
}

set.seed(1500)
nsub <- 25
npix <- c(100, 200, 133)
nk <- 5
outcome <- matrix(rnorm(nsub * nk), ncol = nk)
outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
outcome3 <- matrix(rnorm(nsub * nk), ncol = nk)
view1tx <- matrix(rnorm(npix[1] * nk), nrow = nk)
view2tx <- matrix(rnorm(npix[2] * nk), nrow = nk)
view3tx <- matrix(rnorm(npix[3] * nk), nrow = nk)
mat1 <- (outcome %*% t(outcome1) %*% (outcome1)) %*% view1tx
mat2 <- (outcome %*% t(outcome2) %*% (outcome2)) %*% view2tx
mat3 <- (outcome %*% t(outcome3) %*% (outcome3)) %*% view3tx
matlist <- list(vox = mat1, vox2 = mat2, vox3 = mat3)
result <- simlr(matlist)

fn="/tmp/DANKZ"
test_simlr_roundtrip(result, fn )
rr=read_simlr( paste0(fn,"_simlr") )
# rr=read_simlr("~/Downloads/ASS_simlr")