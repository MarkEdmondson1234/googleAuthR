library(googleCloudRunner)

cr_deploy_pkgdown(
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAuthR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml"
)

# revdep

library(revdepcheck)

pkgs <- revdep_todo()$package

a_build <- function(pkg){
  
  r_cmd <- c("install.packages('revdepcheck')",
             sprintf("revdep_check('%s', quiet = FALSE)", pkg))
             
  bs <- cr_buildstep_r(r_cmd)
  yml <- cr_build_yaml(bs)
  build <- cr_build_make(yml)
  
}

builds <- lapply(pkgs, a_build)
builds <- setNames(builds, pkgs)

# make build triggers for all the revdeps
a_buildtrigger <- function(
  pkg, 
  builds, 
  trigger = cr_buildtrigger_repo("MarkEdmondson1234/googleCloudRunner",
                                 branch = "master")){
  build <- builds[[pkg]]
  cr_buildtrigger(build, 
                  name = pkg, 
                  description = "dependency check googleAuthR",
                  trigger = trigger, 
                  trigger_tags = "dependency_check",
                  overwrite = TRUE)
}

lapply(pkgs, a_buildtrigger, builds = builds)

