This is an example of creating a Google login before your main app loads. 

Wrap the call to your Shiny ui with `gar_shiny_ui(ui)` and then use `gar_shiny_auth(session)` at the top of your server function.  The API calls within Shiny can then be authenticated as you would offline, since each Shiny session will carry the logged in user's credentials before the app loads.