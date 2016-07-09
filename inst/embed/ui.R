library(shiny)
library(googleAuthR)

## ui.R
ui <- fluidPage(
  singleton(HTML(
    "<script src='https://apis.google.com/js/client.js'></script>"
  )),
  div(id="auth-button"),
  singleton(HTML("
                 <script>
      function auth() {
                 var config = {
                    'client_id': '201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com',
                    'scope': 'https://www.googleapis.com/auth/analytics.readonly'
                 };
                 gapi.auth.authorize(config, function() {
                    token = gapi.auth.getToken();
                    console.log('login complete');
                    console.log(token);
                    Shiny.onInputChange('js_auth_access_token', token.access_token);
                    Shiny.onInputChange('js_auth_token_type', token.token_type);
                    Shiny.onInputChange('js_auth_expires_in', token.expires_in);
                 });
                 }
                 </script>
                 "
                 )
            ),
  tags$button(onclick="auth();", "Authorize"),
  textOutput("js_auth"),
  googleAuthUI("loginButton"),
  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)