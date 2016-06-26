// pull in desired CSS/SASS files
require( './styles/main.scss' );

var zxcvbn =  require('zxcvbn');

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );
app.ports.checkPassword.subscribe(function(password){
  console.log(password);
  var result = zxcvbn(password);
  app.ports.passwordScore.send(result.score);
});
