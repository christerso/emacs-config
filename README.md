# Emacs Configuration for C11 Development

A clean, literate Emacs configuration optimized for C11 development with LSP support.

## Features

- **Literate Configuration**: Written in org-mode for beautiful documentation
- **LSP Support**: Automatic clangd integration with Eglot
- **Build/Run/Debug**: F3 build, F4 run, Shift+F4 debug with RemedyBG
- **Clean Environment**: No backup/auto-save clutter files
- **C11 Optimized**: Proper syntax highlighting and indentation
- **Voidlight Theme**: Beautiful minimal dark theme for comfortable coding
- **Nerd Font**: JetBrains Mono Nerd Font with programming ligatures and icons
- **Helm Navigation**: Ultra-fast fuzzy file finding and command completion
- **Project Management**: Projectile + Helm for instant file navigation
- **Everything Search**: Windows system-wide file search integration
- **Git Integration**: Magit for powerful Git workflows
- **Smart Completion**: Company mode with LSP integration
- **Multiple Cursors**: Efficient multi-line editing
- **Discoverable Keys**: Which-key for learning keybindings

## Installation

1. Clone this repository to your Emacs directory:
   ```bash
   git clone <repo-url> ~/.emacs.d
   ```

2. Install JetBrains Mono Nerd Font (recommended):
   - Download from: https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/JetBrainsMono.zip
   - Extract and install the font files
   - Alternative: Cascadia Code or Fira Code work as fallbacks

3. Ensure clangd is installed and in your PATH:
   - Windows: Install LLVM/Clang with clangd
   - Path should include `C:\Program Files\LLVM\bin`

4. Start Emacs - packages and theme will auto-install on first run
   - The voidlight theme will be downloaded automatically from GitHub
   - No manual theme installation required

## Key Bindings

### Build/Run/Debug
- `F3` - Build Rift project (cmake --build build --config Release --target rift)
- `F4` - Run Rift project executable
- `Shift+F4` - Debug Rift project in RemedyBG

### LSP Navigation
- `M-.` - Go to definition (LSP)
- `M-,` - Go back from definition
- `M-x eglot` - Manually start LSP (auto-starts for C files)

### File Finding & Navigation (Helm-powered)
- `C-x C-f` - Find files (Helm fuzzy matching)
- `C-x b` - Switch buffers (with recent files)
- `C-x C-r` - Recent files
- `M-x` - Enhanced command palette
- `C-c p f` - Find file in project (fuzzy)
- `C-c p p` - Switch between projects
- `C-c e` - Everything search (system-wide on Windows)

### Text Search
- `C-c s` - Search text with ripgrep (helm-rg)
- `C-c S` - Search text in project root (helm-projectile-rg)
- `C-c p s g` - Search text in project (Helm + ripgrep)

### Git Integration
- `C-x g` - Magit status (staging, commits, push/pull)

### Multiple Cursors
- `C-Shift-c C-Shift-c` - Edit multiple lines
- `C->` - Mark next like this
- `C-<` - Mark previous like this
- `C-c C-<` - Mark all like this

## Configuration Structure

- `init.el` - Minimal loader that loads the org configuration
- `config.org` - Main literate configuration with documentation
- `.gitignore` - Excludes generated files and packages

## LSP Features

When editing C files, you automatically get:
- Real-time syntax checking
- Code completion
- Go to definition/declaration
- Find references
- Hover documentation
- Automatic formatting

## Build Integration

The configuration includes a custom build function for CMake projects:
- Sets working directory to project root
- Runs CMake build with Release configuration
- Shows output in compilation buffer
- Clickable error navigation

## Requirements

- Emacs 27+ (for built-in Eglot support)
- clangd (from LLVM/Clang installation)
- Git (for version control)
- CMake (for build integration)

## Project Structure

```
.emacs.d/
├── init.el          # Configuration loader
├── config.org       # Main literate configuration
├── README.md        # This file
├── .gitignore       # Git ignore rules
└── elpa/           # Auto-installed packages (ignored)
```

## Customization

Edit `config.org` to modify the configuration. The file is organized into logical sections:

1. **Basic Settings** - UI and startup preferences
2. **Package Management** - Repository configuration
3. **Development Environment** - PATH and tool setup
4. **LSP Configuration** - Eglot and clangd setup
5. **Build System Integration** - Custom build commands

Changes to `config.org` are automatically loaded when Emacs restarts.