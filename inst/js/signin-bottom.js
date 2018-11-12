function onSignIn(googleUser) {
      var profile = googleUser.getBasicProfile();
      console.log('ID: ' + profile.getId()); 
      console.log('Name: ' + profile.getName());
      console.log('Image URL: ' + profile.getImageUrl());
      console.log('Email: ' + profile.getEmail());
      Shiny.onInputChange('%s', profile.getId());
      Shiny.onInputChange('%s', profile.getName());
      Shiny.onInputChange('%s', profile.getImageUrl());
      Shiny.onInputChange('%s', profile.getEmail());
      authorizeButton.style.display = 'none';
      signoutButton.style.display = 'block';
}
if (typeof gapi == 'undefined') {
  alert('Failed to load Google API. Check your ad blocker. You will not be able to authenticate.');
}