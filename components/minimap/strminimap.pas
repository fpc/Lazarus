unit StrMiniMap;

{$mode objfpc}{$H+}

interface

const
  SConfigFile = 'minimap.xml';

  KeyEnabled = 'Enabled';
  KeyAlignLeft = 'AlignLeft';
  KeyWidth = 'Width';
  KeyViewWindowColor = 'ViewWindowColor';
  KeyViewWindowTextColor = 'ViewWindowTextColor';
  KeyInitialFontSize = 'InitialFontSize';

resourcestring
  SMinimapConfigTitle = 'Minimap';
  SShowMinimap = 'Show minimap';
  SPutMapLeftOfEditorRe = 'Put minimap left of editor (requires IDE restart for '
    +'existing tabs)';
  SMapWidth = 'Minimap width';
  SInitialFontSize = 'Initial font size';
  SViewWindowColor = 'View window color';
  SViewWindowTextColor = 'View window text color';


implementation

end.

