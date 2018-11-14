This is an example of using googleAuthR multi-user authentication within a Shiny app.

Use the Shiny Module functions `googleAuthUI` and `googleAuth` to make the login flow, and then wrap your created functions made with `gar_api_generator` with `with_shiny` to ensure the authentication token is passed in the correct manner. 