// pull in desired CSS/SASS files
require( './styles/main.scss' );

var zxcvbn =  require('zxcvbn');

// inject bundled Elm app into div#main
var Elm = require( './Main' );

var storedState = localStorage.getItem('globals');
var startingState = storedState ? JSON.parse(storedState) : {apiToken : "", username: ""};

var app = Elm.Main.embed( document.getElementById( 'main' ),  startingState);
app.ports.checkPassword.subscribe(function(password){
  var result = zxcvbn(password);
  app.ports.passwordScore.send(result.score);
});

app.ports.saveToLocalstorage.subscribe(function(model) {
    localStorage.setItem('globals', JSON.stringify(model));
});
