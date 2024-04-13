module.exports = {
  plugins: {
    "postcss-import": {},
    "postcss-color-alpha": {},
    "postcss-color-functional-notation": {},
    "@csstools/postcss-color-mix-function": {},
    "@csstools/postcss-relative-color-syntax": {},
    "postcss-custom-properties": {
      preserve: false,
    },
    "postcss-nested": {},
    "postcss-apply": {},
    "@csstools/postcss-hwb-function": {},
    "postcss-discard": {
      rule: ":root",
    },
  },
};
