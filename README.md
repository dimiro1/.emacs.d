# My Emacs Configuration

This repository contains my personal Emacs configuration, designed for modularity, efficiency, and ease of customization. Each feature or package is organized into separate files for better maintainability.

---

## **Features**

- **Package Management**: Uses `straight.el` for robust and reproducible package management.
- **Completion Framework**: Configured with `company-mode` for in-buffer, context-aware completion.
- **Git Integration**: Seamless Git workflows with `magit`.
- **File Navigation**: Easy file navigation using `projectile`.
- **Programming Support**:
  - Language Server Protocol (LSP) integration with `eglot`.
  - Syntax-aware editing via Tree-sitter.
- **Themes**: Configured with a selection of color schemes, including `gruvbox` and `catppuccin`.
- **Additional Enhancements**:
  - Keybinding assistance with `which-key`.
  - Paredit for structured editing of Lisp code.
  - GitHub Copilot integration.

---

## **Installation**

1. Clone this repository into your `.emacs.d` directory:
   ```bash
   git clone https://github.com/yourusername/emacs-config.git ~/.emacs.d

	2.	Install Emacs (version 29+ is recommended).
	3.	Launch Emacs. On the first run, the configuration will:
	•	Install straight.el automatically.
	•	Download and configure all required packages.

Usage

Key Features:
	•	Open Init File: Quickly edit your init.el file with C-c i.
	•	Git Commands: Launch Magit with C-x g.
	•	Project Navigation: Use C-c p for projectile commands.
	•	Language-Specific Features:
	•	company-mode for code completion.
	•	eglot for LSP support.

File Structure

The configuration is split into multiple files for modularity:

.
├── init.el             # Entry point for the configuration
├── lisp/
│   ├── my-straight.el  # Package management setup
│   ├── my-common.el    # Common configurations
│   ├── my-magit.el     # Magit setup
│   ├── my-lsp.el       # Language server integration
│   ├── my-company.el   # Company-mode configuration
│   ├── my-paredit.el   # Paredit for Lisp editing
│   └── ...             # Additional modules
