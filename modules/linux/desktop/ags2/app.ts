import { App, Gdk, Gtk } from "astal/gtk3";
import Bar from "./widget/Bar";
import { bind } from "astal";
import { cssOut, watchCss } from "./build";

let bars: Gtk.Widget[] = [];

const startBars = (monitors: Gdk.Monitor[]) => {
  bars = [];
  App.get_windows().forEach((w) => App.remove_window(w));
  bars = monitors.map((monitor) => {
    const idx = monitors.indexOf(monitor);
    return Bar({ monitorId: idx, monitor });
  });
};

const tryStartApp = () => {
  if (bars.length === 0) {
    try {
      App.start({
        css: cssOut,
        main() {
          startBars(App.get_monitors());
          bind(App, "monitors").subscribe((monitors) => {
            startBars(monitors);
          });
        },
      });
      console.log("App started");
      watchCss();
    } catch (e) {
      bars = [];
      console.warn(e);
      console.log("Failed to start app, retrying...");
      setTimeout(() => tryStartApp(), 3000);
    }
  }
};

tryStartApp();
