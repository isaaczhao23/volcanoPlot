# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)

# fmt_dcimals <- function(decimals = 0) {
#   function(x) format(x, nsmall = decimals, scientific = FALSE)
# }


reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf))
}
