global.window = global;
let app = Elm.worker(Elm.Main);
app.ports.display.subscribe(console.log);
