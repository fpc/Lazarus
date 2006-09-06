{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Common IDE dialogs.
}
unit IDEDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TIDESelectDirectory = function(const Title, InitialDir: string): string of object;
  TInitIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;
  TStoreIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;
  TIDEMessageDialog = function(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                                const HelpKeyword: string = ''): Integer of object;
  TIDEQuestionDialog = function(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: array of const;
                                const HelpKeyword: string = ''): Integer of object;
function LazSelectDirectory(const Title: string; const InitialDir: string = ''
  ): string;

var
  LazIDESelectDirectory: TIDESelectDirectory = nil;// set by the IDE
  InitIDEFileDialog: TInitIDEFileDialog = nil;
  StoreIDEFileDialog: TStoreIDEFileDialog = nil  ;
  IDEMessageDialog: TIDEMessageDialog = nil;
  IDEQuestionDialog: TIDEQuestionDialog = nil;


implementation

function LazSelectDirectory(const Title: string; const InitialDir: string
  ): string;
begin
  Result:=LazIDESelectDirectory(Title,InitialDir);
end;

end.

