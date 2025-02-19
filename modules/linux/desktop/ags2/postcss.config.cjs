module.exports = {
  plugins: {
    "postcss-import": {},
    "postcss-color-alpha": {}, // for white() black()
    "postcss-apply": {},
    "postcss-nested": {},
    "postcss-calc": {},
    "postcss-preset-env": {
      stage: false,
      features: {
        "color-function": true,
        "color-functional-notation": true,
        "color-mix": true,
        "custom-properties": { preserve: false },
        "oklab-function": {
          preserve: false,
          subFeatures: {
            displayP3: false,
          },
        },
        "relative-color-syntax": true,
      },
    },
    "postcss-discard": {
      rule: [":root"],
    },
  },
};
