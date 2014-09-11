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
  rsClearSelection          = 'Clear selection';
  rsHint                    = 'You may add here your favorite commands';
  rsPosition                = 'Position';
  rsTop                     = 'Top';
  rsBottom                  = 'Bottom';
  rsLeft                    = 'Left';
  rsRight                   = 'Right';
  rsEditorToolbar           = 'Editor ToolBar';
  rsMenuView                = 'View';
  rsProfile                 = 'Profile';
  rsAll                     = 'All';
  rsDesign                  = 'Design';
  rsDebug                   = 'Debug';
  rsHTML                    = 'HTML';
  rsCustom                  = 'Custom';
  rsHelp1                   = 'The "Add" button inserts a Menu Item before the '+
    'selected Item of the Toolbar.';
  rsHelp2                   = 'If none is selected, the Item will be appended at the end '+
    'of the Toolbar.';
  rsHelp3                   = 'You may rearrage Toolbar Items with "Move Up" and '+
    '"Move Down" buttons.';
  rsHelp4                   = 'Hint: to append an Item at the end, use "Clear selection"';
  // Root Node Captions
  rsIDEMainMenu             = 'IDE Main Menu';
  rsSourceTab               = 'Source Tab';
  rsSourceEditor            = 'Source Editor';
  rsMessages                = 'Messages';
  rsCodeExplorer            = 'Code Explorer';
  rsCodeTemplates           = 'Code Templates';
  rsDesigner                = 'Designer';
  rsPackageEditor           = 'Package Editor';
  rsPackageEditorFiles      = 'Package Editor Files';

implementation

end.

