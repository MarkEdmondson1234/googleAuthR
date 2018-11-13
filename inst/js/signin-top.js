var authorizeButton = document.getElementById('%s');
    var signoutButton = document.getElementById('%s');
    signoutButton.style.display = 'none';
    function signOut() {
      var auth2 = gapi.auth2.getAuthInstance();
      auth2.signOut().then(function () {
        console.log('User signed out.');
        authorizeButton.style.display = 'block';
        signoutButton.style.display = 'none';
      });
    Shiny.onInputChange('%s', null);
    Shiny.onInputChange('%s', null);
    Shiny.onInputChange('%s', null);
    Shiny.onInputChange('%s', null);
    }