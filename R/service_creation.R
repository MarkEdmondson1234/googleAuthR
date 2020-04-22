#' Work with service accounts via the API
#' 
#' These functions let you create a service JSON key from an OAuth2 login.  You can then assign it roles and do a one time download of a service account key to use for authentication in other Google APIs
#' 
#' @param projectId The projectId containing the service account
#' @param accountId The service accountId  
#' @param serviceName Name of service account
#' @param serviceDescription Description of service account
#' 
#' @export
gar_service_create <- function(
  accountId,
  projectId,
  serviceName = "googleAuthR::gar_service_create",
  serviceDescription = "A service account created via googleAuthR"
){
  
  the_url <- sprintf(
    "https://iam.googleapis.com/v1/projects/%s/serviceAccounts",
    projectId
  )
  
  body <- list(
    accountId = accountId,
    serviceAccount = list(
      description = serviceDescription,
      displayName = serviceName
    )
  )
  
  api_call <- gar_api_generator(the_url, "POST", 
                                data_parse_function = function(x) x)
  
  api_call(the_body = body)
  
}

#' Grant IAM roles to accountIds
#' 
#' @param accountIds A vector of accountIds in the form \code{serviceAccount:accountId@projectid.iam.gserviceaccount.com}
#' @param role The role to give the accountIds e.g. \code{roles/editor} - see list of roles here \url{https://cloud.google.com/iam/docs/understanding-roles#predefined_roles}
#' 
#' @export
#' @rdname gar_service_create
gar_service_grant_roles <- function(accountIds,
                                    role,
                                    projectId,
                                    type = c("serviceAccount", "user")){
  
  type <- match.arg(type)
  
  the_url <- sprintf(
    "https://cloudresourcemanager.googleapis.com/v1/projects/%s:setIamPolicy",
    projectId
  )
  
  body <- list(
    policy = list(
      bindings = list(
        list(
          role = role,
          members = list(
              paste0(type, ":", accountIds)
              )
          )
        )
      )
    )
  
  api_call <- gar_api_generator(the_url, "POST", 
                                data_parse_function = function(x) x)
  
  api_call(the_body = body)
  
}

#' Create a service account key
#' 
#' 
#' @seealso https://cloud.google.com/iam/docs/reference/rest/v1/projects.serviceAccounts.keys/create
#' 
#' @param file The file to download the private JSON key to
#' 
#' @export
#' @examples 
#' \dontrun{
#'  library(googleAuthR)
#'  gar_set_client(scopes = "https://www.googleapis.com/auth/cloud-platform")
#'  gar_auth()
#'  gar_service_create("test12345678", "my-project")
#'  
#'  gar_service_grant_roles("test12345678@my-project.iam.gserviceaccount.com",
#'                          role = "roles/editor",
#'                          projectId = "my-project")
#'  
#'  gar_service_key("test12345678", "my-project", "my-auth.json")
#'  
#'  gar_service_key_list("test12345678", "my-project")
#' }
#' @rdname gar_service_create
gar_service_key <- function(accountId, 
                            projectId, 
                            file = paste0(accountId,"-auth-key.json")){
  
  the_url <- sprintf(
    "https://iam.googleapis.com/v1/projects/%s/serviceAccounts/%s@%s.iam.gserviceaccount.com/keys",
    projectId, accountId, projectId)
  
  api_call <- gar_api_generator(
    the_url, "POST", data_parse_function = decode_key)
  
  o <- api_call()
  
  myMessage("Writing secret auth JSON key to ", file, level = 3)
  write(o, file = file)

}

decode_key <- function(x){
  rawToChar(jsonlite::base64_dec(x$privateKeyData))
}

#' @rdname gar_service_create
#' @export
gar_service_key_list <- function(accountId,
                                 projectId){
  
  the_url <- sprintf(
    "https://iam.googleapis.com/v1/projects/%s/serviceAccounts/%s@%s.iam.gserviceaccount.com/keys", projectId, accountId, projectId
  )
  
  api_call <- gar_api_generator(the_url, "GET", 
                                data_parse_function = function(x) x$keys)
  
  api_call()
}
