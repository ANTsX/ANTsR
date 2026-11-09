Development scripts, benchmarks and comparisons (for example R vs. Python
nsa_flow). They are excluded from the package build via `.Rbuildignore`.

These used to live in `tests/` (where `R CMD check` ran them and failed without
Python `torch` and several Suggests packages) and in `R/` (where `R CMD check`
flags non-R files).
