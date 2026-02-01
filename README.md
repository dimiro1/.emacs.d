# D1 Emacs Configuration

My personal Emacs configuration with a modular architecture.

Requires **Emacs 31+** to leverage the `user-lisp` auto-scrape feature for automatic `load-path`, byte-compilation, and autoload cookie scraping.

## Features

- Modular configuration with `d1-*` prefixed modules
- LSP support via eglot (Go, Rust, TypeScript)
- Git integration with Magit
- Modern completion with Vertico, Corfu, and Orderless

## Structure

```
~/.emacs.d/
├── init.el              # Entry point
├── early-init.el        # Performance settings
└── user-lisp/           # Configuration modules (auto-scraped by Emacs 31)
    ├── d1-core.el       # Core settings
    ├── d1-ui.el         # UI and themes
    ├── d1-system.el     # System integration
    ├── d1-completion.el # Completion framework
    ├── d1-languages.el  # Programming languages
    ├── d1-navigation.el # File navigation
    ├── d1-git.el        # Git tools
    ├── d1-keybindings.el# Custom keybindings
    ├── d1-npm.el        # NPM integration
    └── test/            # ERT tests
        ├── d1-git-permalink-test.el
        └── d1-homebrew-test.el
```

## Key Bindings

- `C-x g`       - Magit
- `F8`           - NeoTree toggle
- `C-c p n`     - Run NPM scripts
- `C-=` / `C--` - Expand/contract region
- `C-c f r`     - Recent files

## Running Tests

```sh
make test
```

---

**Author**: Claudemiro Alves Feitosa Neto (<dimiro1@gmail.com>)
