library(googleCloudRunner)

cr_deploy_pkgdown(
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAuthR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml"
)

