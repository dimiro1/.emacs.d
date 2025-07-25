# CAFN Emacs Configuration

A modern, modular Emacs configuration focused on developer productivity and clean code organization.

## Features

- **Package Management**: Uses `package.el` with MELPA/GNU ELPA
- **Modular Architecture**: Clean separation with `cafn-*` prefixed modules
- **Performance Optimized**: Fast startup with lazy loading
- **LSP Support**: Native `eglot` for Go, Rust, TypeScript/JavaScript
- **Git Integration**: Comprehensive Magit setup
- **Themes**: Modus, Doom, and Standard themes
- **Evil Mode**: Optional Vim emulation
- **Smart Completion**: Flex matching with Vertico/Marginalia

## Installation

### Prerequisites
- Emacs 29.1+ (for tree-sitter support)
- Language servers:
  ```bash
  # Go
  go install golang.org/x/tools/gopls@latest
  
  # Rust  
  rustup component add rust-analyzer
  
  # TypeScript
  npm install -g typescript-language-server typescript
  ```

### Quick Start
1. Backup existing config: `mv ~/.emacs.d ~/.emacs.d.backup`
2. Clone: `git clone https://github.com/dimiro1/.emacs.d.git ~/.emacs.d`
3. Launch Emacs - packages will install automatically

## File Structure

```
~/.emacs.d/
├── init.el                    # Main entry point
├── early-init.el             # Performance settings
├── custom.el                 # Auto-generated customization
└── lisp/                     # Configuration modules
    ├── cafn-packages.el      # Package management
    ├── cafn-colorscheme.el   # Themes
    ├── cafn-editor.el        # Core editing
    ├── cafn-languages.el     # Programming languages
    ├── cafn-magit.el         # Git integration
    ├── cafn-evil.el          # Vim emulation
    └── ...                   # Additional modules
```

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-x g` | `magit-status` | Git status |
| `F5` | `modus-themes-toggle` | Toggle theme |
| `C-c P r` | Refresh packages | Update package list |
| `C-c P u` | Update packages | Update all packages |
| `C-M-←/→/↑/↓` | Window resize | Resize windows |

## Customization

### Change Theme
Edit `lisp/cafn-colorscheme.el`:
```elisp
(load-theme 'modus-operandi-tritanopia :no-confirm)
```

### Disable Evil Mode
Comment out in `init.el`:
```elisp
;; (require 'cafn-evil)
```

### Add Custom Module
Create `lisp/cafn-mymodule.el`:
```elisp
;;; cafn-mymodule.el --- Description  -*- lexical-binding: t; -*-
;; Your configuration
(provide 'cafn-mymodule)
;;; cafn-mymodule.el ends here
```

Add to `init.el`: `(require 'cafn-mymodule)`

## Troubleshooting

- **Startup time**: `M-x emacs-init-time`
- **Package issues**: `C-c P r` then `C-c P u`
- **LSP problems**: Check `*eglot-events*` buffer

---

**Author**: Claudemiro Alves Feitosa Neto (<dimiro1@gmail.com>)  
**Repository**: [https://github.com/dimiro1/.emacs.d](https://github.com/dimiro1/.emacs.d)