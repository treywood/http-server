import { Elm } from './elm/Main.elm';

console.dir(Elm);

Elm.Main.init({
  flags: true,
  node: document.getElementById('app')
});