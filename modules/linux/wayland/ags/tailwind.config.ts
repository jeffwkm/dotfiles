import type { Config } from 'tailwindcss';
import colors from 'tailwindcss/colors';

export default {
  darkMode: 'class',
  content: [
    './src/**/*.ts',
    './src/**/*.css',
  ],
  theme: {
    fontFamily: {
      sans: `Inter, system-ui, sans-serif`,
      serif: `Inter, system-ui, sans-serif`,
      mono: `'JetBrains Mono', monospace`,
    },
    colors: {
      transparent: 'transparent',
      current: 'currentColor',
      black: colors.black,
      white: colors.white,
      gray: colors.neutral,
      blue: colors.sky,
      green: colors.lime,
      yellow: colors.amber,
      purple: colors.violet,
      red: colors.red,
    },
    extend: {
      opacity: {
        '15': '.15',
        '35': '.35',
        '45': '.45',
        '55': '.55',
        '65': '.65',
        '85': '.85',
      },
    },
  },
} satisfies Config;
