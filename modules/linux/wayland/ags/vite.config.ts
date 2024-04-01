/* vite.config.ts */
import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
  build: {
    target: "node20",
    lib: {
      entry: resolve(__dirname, "src/main.ts"),
      name: "ags-config",
      fileName: "main",
    },
    rollupOptions: {
      external: [/resource:/, /gi:/],
    },
  },
});
