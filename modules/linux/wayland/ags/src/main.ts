import Gdk from "gi://Gdk";
import Bar from "./bar";

const range = (length, start = 1) => Array.from({ length }, (_, i) => i + start);
function forMonitors(widget) {
  const n = Gdk.Display.get_default()?.get_n_monitors() || 1;
  return range(n, 0).map(widget).flat(1);
}

const cssOut = `${App.configDir}/style.css`;

App.config({
  gtkTheme: "adw-gtk3-dark",
  style: cssOut,
  windows: forMonitors(Bar),
});

Utils.monitorFile(cssOut, () => {
  App.resetCss();
  App.applyCss(cssOut);
});
