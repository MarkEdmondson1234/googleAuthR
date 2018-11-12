var authorizeButton = document.getElementById('%s');
var signoutButton = document.getElementById('%s');
signoutButton.style.display = 'none';
function auth() {
  var config = {
    'client_id': '%s',
    'scope': '%s' %s
  };

  gapi.auth.authorize(config, function() {
    token = gapi.auth.getToken();
    console.log('login complete');
    Shiny.onInputChange('%s', token.access_token);
    Shiny.onInputChange('%s', token.token_type);
    Shiny.onInputChange('%s', token.expires_in);
    authorizeButton.style.display = 'none';
    signoutButton.style.display = 'block';
  });
}

function out(){
  gapi.auth.signOut();
  location.reload();
}
