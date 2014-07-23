unit editortoolbar_str;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

resourcestring
  SErrCouldNotFind = 'Could not find <%s>';
  rsConfigureToo = 'Configure Toolbar';
  rsJumpTo = 'Jump To';
  rsEditorToolbarConfigForm = 'Editor Toolbar Configuration';
  rsOK                      = 'OK';
  rsCancel                  = 'Cancel';
  rsToolbar                 = 'Toolbar Items';
  rsMenuTree                = 'Available Menu Items';
  rsAddDivider              = 'Add Divider';
  rsAddSelected             = 'Add selected item to toolbar';
  rsRemoveSelected          = 'Remove selected item from toolbar';
  rsMoveSelectedUp          = 'Move selected toolbar item up';
  rsMoveSelectedDown        = 'Move selected toolbar item down';
  rsPosition                = 'Position';
  rsTop                     = 'Top';
  rsBottom                  = 'Bottom';
  rsLeft                    = 'Left';
  rsRight                   = 'Right';
  rsVisible                 = 'Visible';
  rsShowHide                = 'Show/Hide Toolbar';
  rsWarning                 = 'You''ve chosen to hide the toolbar. ' +
    'You may access the toolbar configuration via "%s" -> "%s" main menu.';
  rsEditorToolbar           = 'Editor ToolBar';
  rsMenuView                = 'View';

implementation

end.

