library(googleCloudRunner)

cr_deploy_pkgdown(
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAuthR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml"
)

# revdep
make_dep_build <- function(
  trg = cr_buildtrigger_repo("MarkEdmondson1234/googleAuthR",
                             branch = "master")){
  
  pkg <- trg$repo$name
  
  r_cmd <- c("revdepcheck::revdep_check(quiet = FALSE)")
             
  bs <- cr_buildstep_r(r_cmd, 
                       name = "gcr.io/gcer-public/packagetools")
  yml <- cr_build_yaml(bs)
  build <- cr_build_make(yml)
  
  cr_buildtrigger(build, 
                  name = "revdepcheck-googleAuthR", 
                  description = Sys.time(),
                  trigger = trg, 
                  trigger_tags = "dependency_check",
                  overwrite = TRUE)
  
}

make_dep_build()


