import { App } from "ags-ts";
import { monitorFile } from "resource:///com/github/Aylur/ags/utils.js";

import { Bar } from "./bar";

const cssOut = `${App.configDir}/style.css`;

App.config({
  gtkTheme: "adw-gtk3-dark",
  style: cssOut,
  windows: [Bar(0), Bar(1)],
  // windows: [Bar(0)],
});

monitorFile(cssOut, () => {
  App.resetCss();
  App.applyCss(cssOut);
});
