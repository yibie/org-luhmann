# Org-Luhmann Workbench User Guide

## Overview

The Workbench is a core feature of the org-luhmann package, providing a digital card system to manage your Luhmann notes. This feature simulates a traditional physical card workbench, allowing you to organize and rearrange your notes in a digital environment.

## Basic Concepts

### Card
- Any heading with a Luhmann number can become a card
- A card contains the title, content, and original location information
- Card content is truncated to a configurable length (default: 300 characters)

### Workbench
- A special buffer that displays all cards in the current workbench using org-mode structure
- Cards are arranged in the order they were added and can be reordered
- Supports multiple independent workbenches, each with its own set of cards
- All workbench states are automatically saved to a file and persist across sessions
- Leverages org-mode’s native features, supporting folding, unfolding, and efficient navigation

### Multiple Workbench Support
- You can create independent workbenches for different projects or topics
- Each workbench has its own set of cards
- Easily switch and manage multiple workbenches

## Usage

### Adding Cards

#### Add Entire Subtree (Recommended)
1. Place the cursor on any heading with a Luhmann number
2. Press `C-c l w`
3. All headings with Luhmann numbers in the subtree will be extracted as individual cards
4. All cards in the workbench are at the same level, making it easy to move and reorganize them

#### Add Only the Current Heading
1. Place the cursor on any heading with a Luhmann number
2. Press `C-c l h`
3. Only the current heading is added, excluding its subheadings and content
4. Suitable for scenarios where only the heading reference is needed

### Selecting/Managing Workbenches
- Press `C-c l W` to open the workbench selection interface
- You can select an existing workbench or create a new one
- Supports managing workbenches (rename, delete)
- The interface displays the number of cards in each workbench

### Operations in the Workbench

#### Move Cards
- `M-↑` or `M-p`: Move the current card up (uses org-mode native functionality)
- `M-↓` or `M-n`: Move the current card down (uses org-mode native functionality)

#### Navigate Cards
- `n` or `C-n`: Move to the next card
- `p` or `C-p`: Move to the previous card
- `TAB`: Fold/unfold the current card content

#### Delete Card
- `C-c C-k`: Remove the current card from the workbench
- `C-c C-s`: Manually save the current card order

#### Clear Workbench
- `C-c C-c`: Clear all cards from the current workbench (confirmation required)

#### Refresh Display
- `g`: Refresh the workbench display

## Configuration Options

### Save File Location
```elisp
(setq org-luhmann-workbench-save-file 
      (expand-file-name "my-workbench.el" user-emacs-directory))
```

### Card Content Length
```elisp
(setq org-luhmann-workbench-card-content-length 500)
```

## Usage Scenarios

### 1. Research Project Organization
- Add related research notes to the workbench
- Arrange cards in logical order
- Quickly jump to the original notes for editing

### 2. Writing Project Planning
- Collect parts of the writing outline
- Rearrange chapter order
- Quickly access reference materials during writing

### 3. Argument Structure Building
- Add arguments and evidence as cards
- Experiment with different argument orders
- Build a logically clear argument structure

### 4. Temporary Note Collection
- Create a temporary collection for specific topics
- Quickly switch between different projects
- Keep the workspace clean

## Technical Details

### Data Storage
- All workbench states are saved in the file specified by `org-luhmann-workbench-save-file`
- Data is stored in Emacs Lisp format, including all workbenches and current workbench information
- All workbenches are automatically loaded when `org-luhmann-setup` is executed

### Card Information
Each card contains the following information:
- `:number`: Luhmann number
- `:title`: Full title
- `:content`: Truncated content (for display)
- `:level`: Level of the original title
- `:file`: Original file path

### Display Format
The workbench uses org-mode structure to display cards, breaking the original level structure to make it easier to move and reorganize cards:

#### Display when adding subtree (recommended)
```
Workbench: default (5 cards)
════════════════════════════════════════════════════════════

1 Test Card 1
This is the content of the first test card.
Contains some text content for testing workbench functions.

1a Test Branch Card
This is the content of the branch card.

1a.1 Subheading 1
Content of subheading 1.

1a.2 Subheading 2
Content of subheading 2.

1a.2.1 Deeper Subheading
Content of deeper subheading.

2 Test Card 2
This is the content of the second test card.
```

#### Display when adding heading only
```
Workbench: default (2 cards)
════════════════════════════════════════════════════════════

1 Test Card 1
This is the content of the first test card.

2 Test Card 2
This is the content of the second test card.
```

Note: Stars are completely hidden, but all org-mode features are preserved. All cards are at the same level, making it easy to move and reorganize them.

This is a branch card, used to test the handling of branch numbers.

2 Test Card 2
This is the content of the second test card.
Used to verify that the workbench can correctly extract and display card information.
```

Note: Stars are completely hidden, but all org-mode features are preserved.

## Troubleshooting

### Card Cannot Be Added
- Ensure the current heading has a Luhmann number
- Check if it is already in the workbench (duplicate additions will be ignored)

### Jump Does Not Work
- Ensure the original file still exists
- Check if the file path is correct

### Workbench Does Not Display
- Use `C-c l W` to redisplay
- Check if any cards have been added
 