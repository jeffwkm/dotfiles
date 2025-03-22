# AGS2 Desktop Shell Guide

## Build and Development Commands
- Build CSS: `./build-css`
- Development mode: `./dev`
- Apply CSS: CSS auto-updates via file watchers
- Project uses Bun for package management: `bun install --no-summary`

## Code Style Guidelines
- TypeScript with strict typing and React-JSX syntax
- JSX imports from `astal/gtk3` (jsxImportSource in tsconfig.json)
- Component files use PascalCase naming with .tsx extension
- Component props use TypeScript interfaces with descriptive names
- Prefer functional components with props destructuring
- Use explicit typing for variables and function parameters
- Use `bind()` for reactive state with astal framework
- Handle errors explicitly with try/catch blocks
- Small focused components with single responsibility pattern

## Development Patterns
- Modular widget components for the desktop bar
- Use Astal bindings for GTK3/Gdk integration
- File monitoring for CSS hot-reloading
- JSX components return GTK widget hierarchies
- Components use reactive binding for state updates