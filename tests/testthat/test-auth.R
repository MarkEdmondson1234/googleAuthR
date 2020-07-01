test_that("vectorized condiction are correctly managed", {

  withr::local_envvar(c(`_R_CHECK_LENGTH_1_LOGIC2_` = "true"))
  withr::local_options(
    list(googleAuthR.scopes.selected = c("foo", "bar"))
  )

  # The original error to avoid was 
  # "'length(x) = 2 > 1' in coercion to 'logical(1)'"
  expect_error(
    googleAuthR::gar_auth(),
    "Non-interactive session and no authentication email selected"
  )
})
