import GLib from "gi://GLib";
import { exec } from "astal/process";
import { Gio, monitorFile } from "astal/file";
import { App } from "astal/gtk3";

const home = GLib.getenv("HOME");
const configPath = `${home}/.config/ags2`;

const buildCssScript = `${configPath}/build-css`;
const cssIn = `${configPath}/style.css`;
const cssOut = `${configPath}/style.out.css`;

const tryBuildCss = (_file: string, event: Gio.FileMonitorEvent) => {
  // console.log("got event on", _file, ":", event);
  if (_file !== cssIn) return;
  if (event !== Gio.FileMonitorEvent.CHANGED) return; // skip duplicate CHANGED events
  try {
    // console.log("Building CSS...");
    exec(`${buildCssScript}`);
    console.log(`Wrote ${cssOut}`);
  } catch (e) {
    console.warn("Build CSS failed:", e);
  }
};

const tryApplyCss = (_file: string, event: Gio.FileMonitorEvent) => {
  // console.log("got event on", _file, ":", event);
  if (_file !== cssOut) return;
  if (event !== Gio.FileMonitorEvent.CHANGED) return; // skip duplicate CHANGED events
  try {
    const timeStart = GLib.get_monotonic_time();
    App.apply_css(cssOut); // don't wipe out CSS until we know the new one is valid
    App.apply_css(cssOut, true);
    const timeElapsed = GLib.get_monotonic_time() - timeStart;
    const nn = Gio.Notification.new("ags: CSS updated");
    App.send_notification("css-updated", nn);
    console.log("CSS updated in", timeElapsed / 1000, "ms");
  } catch (e) {
    console.warn(e);
    console.warn("Failed to apply css, waiting for changes");
    const nn = Gio.Notification.new("ags: Error loading CSS");
    App.send_notification("css-error", nn);
  }
};

let cssInMon: Gio.FileMonitor | null = null;
let cssOutMon: Gio.FileMonitor | null = null;

const watchCss = () => {
  if (cssInMon) cssInMon.cancel();
  if (cssOutMon) cssOutMon.cancel();
  cssInMon = monitorFile(cssIn, tryBuildCss);
  console.log(`Watching ${cssIn}`);
  cssOutMon = monitorFile(cssOut, tryApplyCss);
  console.log(`Watching ${cssOut}`);
};

export { watchCss, cssOut, home, configPath };
