
# Test that the output vector has the correct values
test_that("sieve_primes works", {
  if (requireNamespace("primes")) {
    o <- options(zulutils.zerodeps=FALSE)
    expect_equal(sieve_primes(100), primes::generate_primes(max=100))
    options(o)
  }
})


test_that("nth_prime works", {
  if (requireNamespace("primes")) {
    actual <- sapply(c(1:10,25), nth_prime)
    expected <- sapply(c(1:10,25), \(n){primes::generate_n_primes(n)[n]})
    expect_equal(actual, expected)
  }
})
