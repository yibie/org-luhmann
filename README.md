# org-luhmann

An Emacs package that implements Luhmann's numbering system for Org mode.

![org-luhmann demo](https://github.com/yibie/org-luhmann/blob/main/assets/figure1.gif)

## Overview

org-luhmann provides a systematic way to organize your notes using Luhmann's numbering system in Org mode. This system enables:

- Hierarchical numbering (e.g., 1, 1.1, 1.2)
- Branch sequences with letters (e.g., 1a, 1b, 1c)
- Infinite insertion between existing notes
- Automatic number generation based on context

## How It Works

org-luhmann is designed to work incrementally, helping you build your note structure one node at a time. Unlike automatic outline numbering, it:

- Does not automatically number all headings at once
- Generates numbers based on existing context (siblings and parents)
- Lets you choose the relationship of new nodes to existing ones
- Preserves the semantic meaning of your note structure

When you add a new number, org-luhmann analyzes:
1. Current heading's level
2. Previous sibling's number (if any)
3. Parent's number (if any)
4. Existing branch sequences

Then offers appropriate numbering options like:
- Next main sequence number (1, 2, 3, ...)
- Next branch letter (1a, 1b, 1c, ...)
- Next sub-number (1.1, 1.2, ...)
- Next parent branch (2.1, 2.2, ...)

This approach ensures that your note structure grows organically and maintains meaningful relationships between notes.


### Adding Numbers

When adding a number, you'll be prompted with these options:

1. New main number (e.g., next available top-level number)
2. Continue sequence (add letter suffix)
3. Branch from current (create sub-number)
4. Branch from parent (create sibling number)

## Workbench Feature

For more details, please refer to [WORKBENCH.md](WORKBENCH.md).

The workbench provides a digital card system for managing your Luhmann notes:

- **Add subtree**: Use `C-c l w` on any Luhmann-numbered heading to add the entire subtree to your current workbench
- **Add heading**: Use `C-c l h` on any Luhmann-numbered heading to add only the heading (without subtree) to your current workbench
- **Select workbench**: Use `C-c l W` to select, create, or manage workbenches
- **Move subtree**: Use `C-c l m` to move subtrees with intelligent renumbering
- **Organize cards**: Move cards up/down (`M-↑`/`M-↓`) to arrange them in your preferred order
- **Navigate cards**: Use `n`/`p` or `C-n`/`C-p` to move between cards
- **Remove cards**: Use `C-c C-k` to remove cards from workbench
- **Clear workbench**: Use `C-c C-c` to clear all cards from current workbench
- **Multi-workbench support**: Create separate workbenches for different projects or topics
- **Persistent storage**: All workbench states are automatically saved and restored across sessions
- **Visual interface**: Clean org-mode outline with hidden stars, supporting folding and efficient navigation

The workbench is perfect for:
- Creating temporary collections of related notes
- Planning writing projects
- Organizing research materials
- Building argument structures
- Managing multiple projects with separate workbenches
- Keeping different topics organized independently

## Installation

### With use-package and straight.el

```elisp
(use-package org-luhmann
  :straight (:host github :repo "yibie/org-luhmann")
  :after org
  :config
  (org-luhmann-setup))
```

### Manual Installation

1. Download `org-luhmann.el` to your load-path
2. Add to your init file:

```elisp
(require 'org-luhmann)
(org-luhmann-setup)
```

## Usage

### Basic Commands

- `M-x org-luhmann-add-number` - Add a Luhmann number to current heading
- `M-x org-luhmann-add-node` - Create a new heading with a Luhmann number
- `M-x org-luhmann-export-region-as-links` - Export headings in region as org-mode links
- `M-x org-luhmann-next-unnumbered-heading` - Navigate to next unnumbered heading
- `M-x org-luhmann-previous-unnumbered-heading` - Navigate to previous unnumbered heading

### Workbench Commands

- `C-c l w` - Add entire subtree to workbench (recommended)
- `C-c l h` - Add only current heading to workbench (without subtree)
- `C-c l W` - Select or manage workbenches

### Numbering Examples

```org
* 1 Main topic
** 1.1 First subtopic
** 1.2 Second subtopic
** 1.2a Branch of 1.2
** 1.2b Another branch
** 1.2.1 Sub-subtopic
* 2 Second main topic
```

### Subtree Movement Examples

The intelligent subtree movement feature allows you to reorganize your knowledge structure:

**Before moving:**
```org
* 1 AI Research
* 2 Market Analysis
* 3 Technical Details
  * 3.1 Implementation
  * 3.2 Architecture
* 4 Future Plans
```

**After moving "3 Technical Details" under "1 AI Research":**
```org
* 1 AI Research
  * 1.1 Technical Details
    * 1.1.1 Implementation
    * 1.1.2 Architecture
* 2 Market Analysis
* 4 Future Plans  ← Number gap preserved (Strategy A)
```

### Exporting Links

The `org-luhmann-export-region-as-links` command allows you to export headings as org-mode links:

1. Select a region containing headings, or position cursor on a heading
2. Use `C-c l e` or `M-x org-luhmann-export-region-as-links`
3. Choose a target file (existing files will be appended to, new files will be created)
4. The command automatically creates unique IDs for each heading and generates org-mode links

This is useful for creating:
- Table of contents for specific sections
- Reference lists for topics
- Link collections for projects

## Customization

```elisp
;; Customize separator between number and title
(setq org-luhmann-title-separator " ")

;; Optional: Use numbers instead of stars for display
(setq org-luhmann-display-style 'number)
(org-luhmann-display-mode 1)
```

The package provides two display styles for headlines:
- `star`: Traditional org-mode stars (default)
- `number`: Replace stars with Luhmann numbers

![org-luhmann display mode](https://github.com/yibie/org-luhmann/blob/main/assets/figure2.gif)

You can toggle the display enhancement with `M-x org-luhmann-display-mode`.

## Version History

### 0.4.0 (2025-08-01)  
**Features**  
- **Digital Workbench Integration**: Full card management system with persistent state across sessions, including:  
  - `C-c l w` (add subtree), `C-c l h` (add heading), `C-c l W` (select workbench) commands  
  - Card organization via move up/down functionality and jump-to-original-location links  
  - Use org-mode as a digital workbench, with all the features of org-mode, including folding/navigation, auto-save, etc.

**Multi-Workbench Support**: Manage multiple card collections with:  
  - Workbench selection interface (create/manage options)  
  - Rename/delete/create workbenches  
  - Persistent storage across sessions  
  - Clear header display for current workbench context  

**Smart Subtree Movement**: Complete implementation of:  
  - Automatic renumbering during moves  
  - Conflict resolution with letter suffixes (e.g., "3.1" → "3.1a")  
  - Maintains number gaps to reflect Luhmann's practice  

### 0.3.0 (2025-07-31)
- Added smart navigation for unnumbered headings
- New export functionality for creating org-mode link collections
- Integrated savehist-mode for persistent export history
- Streamlined keyboard shortcuts and removed redundant features
- Improved file selection interface with better user experience


## License

This project is licensed under the MIT License.

## Author

Yibie (yibie@outlook.com)

