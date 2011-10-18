Example for SynEdit's auto-completion features.

SynEdit (the editor used by the IDE) provides 2 different auto-completion features:

1) TSynCompletion (The word completion feature used by the IDE)
Displays a drop-down providing a list of completions (or replacements) for the current word. The completions, must be provided, and updated by the application.
If the user typed "Sel", then a list of words beginning with "Sel" may be displayed.


2) TSynAutoComplete
Replaces the current word/token, with a fixed pre-defined substitution.
If the user typed "IB", the replacement may be "if foo then begin"
