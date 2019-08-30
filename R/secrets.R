# as advised by https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html
# source this file to aid giving encrypted files for travis testing
# 
# # secret_pw_name("gargle") --> "GARGLE_PASSWORD"
# secret_pw_name <- function(package) {
#   paste0(toupper(package), "_PASSWORD")
# }
# 
# # secret_pw_gen() --> "9AkKLa50wf1zHNCnHiQWeFLDoch9MYJHmPNnIVYZgSUt0Emwgi"
# secret_pw_gen <- function() {
#   x <- sample(c(letters, LETTERS, 0:9), 50, replace = TRUE)
#   paste0(x, collapse = "")
# }
# 
# # secret_pw_exists("gargle") --> TRUE or FALSE
# secret_pw_exists <- function(package) {
#   pw_name <- secret_pw_name(package)
#   pw <- Sys.getenv(pw_name, "")
#   !identical(pw, "")
# }
# 
# # secret_pw_get("gargle") --> error or key-ified PASSWORD =
# #                             hash of charToRaw(PASSWORD)
# secret_pw_get <- function(package) {
#   pw_name <- secret_pw_name(package)
#   pw <- Sys.getenv(pw_name, "")
#   if (identical(pw, "")) {
#     stop_secret(
#       message = sprintf("Envvar %s is not defined", pw_name),
#       package = package
#     )
#   }
#   
#   sodium::sha256(charToRaw(pw))
# }
# 
# # Store and retrieve encrypted data -------------------------------------------
# 
# secret_can_decrypt <- function(package) {
#   requireNamespace("sodium", quietly = TRUE) && secret_pw_exists(package)
# }
# 
# # input should either be a filepath or a raw vector
# secret_write <- function(package, name, input) {
#   if (is.character(input)) {
#     input <- readBin(input, "raw", file.size(input))
#   } else if (!is.raw(input)) {
#     stop("Expected raw or character, got ", class(input), call. = FALSE)
#   }
#   
#   destdir <- fs::path("inst", "secret")
#   fs::dir_create(destdir)
#   destpath <- fs::path(destdir, name)
#   
#   enc <- sodium::data_encrypt(
#     msg = input,
#     key = secret_pw_get(package),
#     nonce = secret_nonce()
#   )
#   attr(enc, "nonce") <- NULL
#   writeBin(enc, destpath)
#   
#   invisible(destpath)
# }
# 
# # Generated with sodium::bin2hex(sodium::random(24)). AFAICT nonces are
# # primarily used to prevent replay attacks, which shouldn't be a concern here
# secret_nonce <- function() {
#   sodium::hex2bin("cb36bab652dec6ae9b1827c684a7b6d21d2ea31cd9f766ac")
# }
# 
# secret_path <- function(package, name) {
#   fs::path_package(package, "secret", name)
# }
# 
# # Returns a raw vector
# secret_read <- function(package, name) {
#   if (!secret_can_decrypt(package)) {
#     stop_secret(message = "Decryption not available", package = package)
#   }
#   
#   path <- secret_path(package, name)
#   raw <- readBin(path, "raw", file.size(path))
#   
#   sodium::data_decrypt(
#     bin = raw,
#     key = secret_pw_get(package),
#     nonce = secret_nonce()
#   )
# }
# 
# stop_secret <- function(message, package) {
#   rlang::abort(
#     "gargle_error_secret",
#     message = message,
#     package = package
#   )
# }