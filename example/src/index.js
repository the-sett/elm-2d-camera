import Main from "./elm/Main.elm";
import Pointer from "./pointer.js";

const app = Main.init({
  node: document.getElementById("application")
});

new Pointer(app);
