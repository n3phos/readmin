require('./index.html')
require('./styles.css')

var Elm = require('./Main.elm'),
    app = Elm.Main.embed(document.getElementById('main'))
