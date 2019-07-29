function onSignIn(googleUser) {
      var profile = googleUser.getBasicProfile();
      var is_signed_in = googleUser.isSignedIn();
      
      console.log('ID: ' + profile.getId()); 
      console.log('Name: ' + profile.getName());
      console.log('Image URL: ' + profile.getImageUrl());
      console.log('Email: ' + profile.getEmail());
      console.log('Logged in: ' + is_signed_in);
      
      Shiny.onInputChange('%s', profile.getId());
      Shiny.onInputChange('%s', profile.getName());
      Shiny.onInputChange('%s', profile.getImageUrl());
      Shiny.onInputChange('%s', profile.getEmail());
      Shiny.onInputChange('%s', is_signed_in);
      authorizeButton.style.display = 'none';
      signoutButton.style.display = 'block';
}
if (typeof gapi == 'undefined') {
  alert('Failed to load Google API. Check your ad blocker. You will not be able to authenticate.');
}