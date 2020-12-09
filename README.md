# servant-auth-elm

An implementation of the servant-auth [example](https://github.com/haskell-servant/servant-auth) using Elm and the [XSRF-protect](https://package.elm-lang.org/packages/3kyro/xsrf-protection/latest/Http-XSRF) package.

The haskell part of the implementation tries to stay as close as poosible to the literal haskell Readme [example](https://github.com/haskell-servant/servant-auth/blob/master/servant-auth-server/README.lhs). The JWT option has been removed as its not relevant for the XSRF cookie example.

To run the example, first build the Elm app inside the frontend folder

````bash
elm make src/App.elm --output app.js
````

You can then run the backend server using stack

````bash
stack run
````

and open `index.html` at [localhost:4000](http://localhost:4000/index.html)

## Limitations

I tried to stay as close as possible to the servant-auth example. Therefore, there is no logout option avaliable. In order to test login / logout behaviour, you can use your browser's development tools to manually clear the session cookies.
