# D1 Emacs Configuration

My personal Emacs configuration with a modular architecture.

## Features

- Modular configuration with `d1-*` prefixed modules
- LSP support via eglot (Go, Rust, TypeScript)
- Git integration with Magit
- Modern completion with fido-vertical-mode and orderless

## Structure

```
~/.emacs.d/
├── init.el					# Entry point
├── early-init.el			# Performance settings
└── lisp/					# Configuration modules
    ├── d1-core.el			# Core settings
    ├── d1-ui.el			# UI and themes
    ├── d1-system.el		# System integration
    ├── d1-completion.el	# Completion framework
    ├── d1-languages.el		# Programming languages
    ├── d1-navigation.el	# File navigation
    ├── d1-git.el			# Git tools
    ├── d1-keybindings.el	# Custom keybindings
    └── d1-npm.el			# NPM integration
```

## Key Bindings

- `C-x g`		 - Magitn
- `F8`			 - NeoTree toggle
- `C-c p n`		 - Run NPM scripts
- `C-=` / `C--`	 - Expand/contract region
- `C-c f r`		 - Recent files

---

**Author**: Claudemiro Alves Feitosa Neto (<dimiro1@gmail.com>)
