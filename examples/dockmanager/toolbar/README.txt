The test1 project should do:

Pressing "Add button" should add an autosized button to the right of the toolbar.

All buttons should be grouped, so that only one button can be down at the same
time.

The toolbar should wrap or give some navigation aid, when the buttons no more
fit into a single row.


What it does:

- The added buttons are NOT autosized.
- New buttons are added to the LEFT of the bar.
- The toolbar does NOT properly wrap added buttons.

A dummy button was added to the bar, and seemed to cure some misbehaviour. It
finally should be removed, so that the toolbar only contains programmatically
created buttons.
When the project was published from the IDE menu, the button size was not
retained, nor did the button autosize any more.

Option CheckBoxes:

"Init button size" sets the Width of the new button to some predefined value.
"First button visible" immediately toggles the visibility of the dummy button.

It's recommended to check "Init button size", so that the buttons can be
distinguished by their Captions, as long as AutoSize doesn't work.

