{
 **********************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 **********************************************************************
}

unit IconFinderCommon;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ICONFINDER_CONFIG_FILENAME = 'iconfindercfg.xml';
  IMAGE_DIR = 'images/';
  DEFAULT_IMAGE_FOLDERS: array[0..18] of String = (
    'images/general_purpose/',  // only this one will be active by default
    'images/actions/',
    'images/codecompletion/',
    'images/codeexplorer/',
    'images/codetoolsdefines/',
    'images/components/',
    'images/componenttreeview/',
    'images/debugger/',
    //'images/designer/',
    'images/items/',
    'images/lazdoc/',
    'images/menu/',
    'images/packages/',
    //'images/sourceeditor/',
    'images/states/',
    'components/chmhelp/lhelp/images/',
    'components/datetimectrls/pictures/',
    'components/iconfinder/images/',
    'components/lazcontrols/images/',
    'components/onlinepackagemanager/images/Tree_Buttons/',
    'components/projectgroups/images/'
  );





implementation

end.

