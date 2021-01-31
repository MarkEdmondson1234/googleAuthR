#' Provision a service account
#' 
#' This uses all the \link{gar_service_create} functions to enable creating service account roles more easily
#' 
#' @inheritParams gar_service_create
#' @inheritParams gar_set_client
#' @inheritParams gar_auth
#' @details 
#' 
#' You will need the OAuth2.0 Client ID JSON from your GCP project via 
#' \code{menu icon > APIs & Services > Credentials > Create Credentials > OAuth client ID}
#' 
#' You need to authenticate with a user with permission \code{iam.serviceAccounts.create} for the project.  Most often the user is an Owner/Editor
#' 
#' @seealso \url{https://cloud.google.com/iam/docs/creating-managing-service-accounts#iam-service-accounts-create-rest}
#' 
#' @export
#' @family IAM functions
#' @examples 
#' \dontrun{
#' 
#' gar_service_provision("my-service-account", 
#'                       c("roles/viewer", "roles/bigquery.jobUser"))
#' }
gar_service_provision <- function(accountId, 
                                  roles,
                                  json = Sys.getenv("GAR_CLIENT_JSON"),
                                  file = paste0(accountId,"-auth-key.json"),
                                  email = Sys.getenv("GARGLE_EMAIL")){
  projectId <- gar_set_client(json, 
                  scopes = "https://www.googleapis.com/auth/cloud-platform")
  if(email == ""){
    email <- NULL
  }
  gar_auth(email = email, cache = FALSE)
  created <- gar_service_create(accountId, projectId = projectId)
    
  gar_service_grant_roles(created$email,
                          roles = roles,
                          projectId = projectId)

  gar_service_key(accountId, projectId = projectId, file = file)
  
}

#' Work with service accounts via the API
#' 
#' These functions let you create a service JSON key from an OAuth2 login.  You can then assign it roles and do a one time download of a service account key to use for authentication in other Google APIs
#' 
#' @param projectId The projectId containing the service account
#' @param accountId The service accountId  
#' @param serviceName Name of service account
#' @param serviceDescription Description of service account
#' 
#' @return If it already exists, returns it via \link{gar_service_get}, else creates the service key
#' 
#' @seealso Combine these functions to provision emails in one step with \link{gar_service_provision}
#' 
#' @export
#' @family IAM functions
gar_service_create <- function(
  accountId,
  projectId,
  serviceName = "googleAuthR::gar_service_create",
  serviceDescription = "A service account created via googleAuthR"
){
  
  candidate <- sprintf("%s@%s.iam.gserviceaccount.com",
                       accountId, projectId)

  o <- tryCatch(
    gar_service_get(candidate, projectId = projectId), 
    error = function(err){

         # watch out if they update the error message #197
         need_one <- grepl(paste("Not found"), err$message)
         if(need_one){
           myMessage("Creating new service account: ", candidate, level = 3)
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
           
           myMessage("Creating service accountId - ", accountId, level = 3)
           api_call <- gar_api_generator(the_url, "POST", 
                                         data_parse_function = function(x) x)
           
           api_call(the_body = body)
         } else {
           stop(err$message)
         }
      })
  o
  
}

#' Grant IAM roles to accountIds
#' 
#' @param accountIds A vector of accountIds in the form \code{accountId@projectid.iam.gserviceaccount.com}
#' @param roles A character vector of roles to give the accountIds e.g. \code{roles/editor} - see list of roles here \url{https://cloud.google.com/iam/docs/understanding-roles#predefined_roles} or in your GCP console \code{https://console.cloud.google.com/iam-admin/roles/details/roles}
#' @param type The type of accountId to add role for - e.g. \code{user:mark@me.com} or \code{serviceAccount:accountId@projectid.iam.gserviceaccount.com}
#' 
#' @details 
#' 
#' It will download the existing roles, and append the role you add to it here.
#' 
#' @export
#' @rdname gar_service_create
#' @seealso \url{https://cloud.google.com/resource-manager/reference/rest/v1/projects/setIamPolicy}
gar_service_grant_roles <- function(accountIds,
                                    roles,
                                    projectId,
                                    type = c("serviceAccount", "user", "group")){
  
  type <- match.arg(type)
  
  existing <- extract_existing(gar_service_get_roles(projectId))
  
  the_url <- sprintf(
    "https://cloudresourcemanager.googleapis.com/v1/projects/%s:setIamPolicy",
    projectId
  )
  
  the_roles <- lapply(roles, function(x){
    list(role = x, members = list(paste0(type,":",accountIds)))
  })
  
  body <- list(
    policy = list(
      bindings = unname(c(existing, the_roles))
      )
    )
  
  myMessage("Granting roles: ", paste(roles, collapse = " "), 
            "to accountIds: ", paste(accountIds, collapse = " "),
            level = 3)
  api_call <- gar_api_generator(the_url, "POST", 
                                data_parse_function = function(x) x$bindings)
  
  api_call(the_body = body)
  
}

extract_existing <- function(bs){
  lapply(bs$role, function(x){
    list(role = x, 
         members = list(bs$members[[which(bs$role == x)]]))
    })
}

#' Get current IAM roles
#' 
#' @details 
#' 
#' If you supply an accountId to \code{gar_service_get_roles} then it will return only those roles that accountId has.
#' 
#' @export
#' @rdname gar_service_create
#' @seealso \url{https://cloud.google.com/resource-manager/reference/rest/v1/projects/setIamPolicy}
#' @examples 
#' 
#' \dontrun{
#' 
#' # all roles
#' projectId <- gar_set_client(
#'                 json = Sys.getenv("GAR_CLIENT_JSON"), 
#'                 scopes = "https://www.googleapis.com/auth/cloud-platform")
#' gar_service_get_roles(projectId)
#' 
#' # roles for one accountId
#' gar_service_get_roles(
#'     projectId, 
#'     accountId = "1080525199262@cloudbuild.gserviceaccount.com")
#' 
#' }
gar_service_get_roles <- function(
  projectId, 
  accountId = NULL, 
  type = c("serviceAccount", "user", "group")){
  
  type <- match.arg(type)
  
  the_url <- sprintf(
    "https://cloudresourcemanager.googleapis.com/v1/projects/%s:getIamPolicy",
    projectId
  )
  
  myMessage("Checking existing roles", level = 3)
  api_call <- gar_api_generator(the_url, "POST", 
                                data_parse_function = function(x) x$bindings)
  
  existing_roles <- api_call()
  
  if(!is.null(accountId)){
    # return roles only for this accountId
    check_email <- paste0(type, ":", accountId)
    present <- unlist(lapply(existing_roles$members,
                             function(x) check_email %in% x))
    present_roles <- existing_roles[present, "role"]
    existing_roles <- data.frame(roles = present_roles, 
                                 members = check_email,
                                 stringsAsFactors = FALSE)
  }
  
  existing_roles
  
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
#'  gar_service_get("test12345678@my-project.iam.gserviceaccount.com", 
#'                  projectId = "my-project")
#'  
#'  gar_service_grant_roles("test12345678@my-project.iam.gserviceaccount.com",
#'                          role = "roles/editor",
#'                          projectId = "my-project")
#'  
#'  gar_service_key("test12345678", "my-project", "my-auth.json")
#'  
#'  gar_service_list("my-project")
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
  
  myMessage("Creating secret auth key for service account", accountId,
            " for project ", projectId, level = 3)
  api_call <- gar_api_generator(
    the_url, "POST", data_parse_function = decode_key)
  
  o <- api_call()
  
  myMessage("Writing secret auth JSON key to ", file, 
            " and adding to .gitignore", level = 3)
  add_line(file, ".gitignore")
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

#' @rdname gar_service_create
#' @export
gar_service_list <- function(projectId){
  the_url <- sprintf(
    "https://iam.googleapis.com/v1/projects/%s/serviceAccounts", projectId
  )
  
  api_call <- gar_api_generator(the_url, "GET", 
                                data_parse_function = function(x) x$accounts)
  
  api_call()
}

#' @rdname gar_service_create
#' @export
gar_service_get <- function(accountId, projectId){
  the_url <- sprintf(
    "https://iam.googleapis.com/v1/projects/%s/serviceAccounts/%s", 
    projectId, accountId
  )
  
  api_call <- gar_api_generator(the_url, "GET", 
                                data_parse_function = function(x) x)
  
  api_call()
}
