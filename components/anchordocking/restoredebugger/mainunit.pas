{ Main form Anchordocking Restore Debugger

  Copyright (C) 2012 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, SynEdit, SynHighlighterXML,
  AnchorDocking, AnchorDockStorage, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons, StdCtrls, XMLPropStorage;

type

  { TADRestDbg }

  TADRestDbg = class(TForm)
    OriginalFileLabel: TLabel;
    OriginalLayoutPanel: TPanel;
    RestoredFileLabel: TLabel;
    RestoredLayoutPanel: TPanel;
    RestoredLayoutToolBar: TToolBar;
    SplitterXMLLayout: TSplitter;
    SplitterBetweenXML: TSplitter;
    OriginalSynEdit: TSynEdit;
    RestoredSynEdit: TSynEdit;
    SplitterBetweenLayouts: TSplitter;
    SynXMLSyn1: TSynXMLSyn;
    MainToolBar: TToolBar;
    OpenToolButton: TToolButton;
    OpenRecentToolButton: TToolButton;
    OriginalLayoutToolBar: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRestoredLayout: TAnchorDockLayoutTree;
    FOriginalFilename: string;
    FOriginalLayout: TAnchorDockLayoutTree;
    FSettings: TAnchorDockSettings;
  public
    procedure OpenLayout(Filename: string);
    procedure LoadSettingsFromOriginalSynedit;
    property Settings: TAnchorDockSettings read FSettings;
    property OriginalLayout: TAnchorDockLayoutTree read FOriginalLayout;
    property OriginalFilename: string read FOriginalFilename;
    property RestoredLayout: TAnchorDockLayoutTree read FRestoredLayout;
  end;

var
  ADRestDbg: TADRestDbg;

implementation

{$R *.lfm}

{ TADRestDbg }

procedure TADRestDbg.FormCreate(Sender: TObject);
begin
  Caption:='Anchordocking Restore Debugger';
  FSettings:=TAnchorDockSettings.Create;
  FOriginalLayout:=TAnchorDockLayoutTree.Create;
  FRestoredLayout:=TAnchorDockLayoutTree.Create;

  OriginalFileLabel.Caption:='Original: '+OriginalFilename;
  RestoredFileLabel.Caption:='Restored XML:';
  OpenToolButton.Caption:='Open Config File';
  OpenRecentToolButton.Caption:='Open Recent';

  if Paramcount>0 then
    OpenLayout(ParamStrUTF8(1));
end;

procedure TADRestDbg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
  FreeAndNil(FOriginalLayout);
  FreeAndNil(FRestoredLayout);
end;

procedure TADRestDbg.OpenLayout(Filename: string);
begin
  if Filename='' then exit;
  Filename:=TrimAndExpandFilename(Filename);
  if (not FileExistsUTF8(Filename)) or (DirPathExists(Filename)) then begin
    MessageDlg('Unable to load','File does not exist: "'+Filename+'"',mtError,
      [mbCancel],0);
    exit;
  end;
  FOriginalFilename:=Filename;
  try
    OriginalSynEdit.Lines.LoadFromFile(Filename);
  except
    on E: Exception do begin
      MessageDlg('Unable to load','File: "'+Filename+'"'#13+E.Message,mtError,
        [mbCancel],0);
      exit;
    end;
  end;

  LoadSettingsFromOriginalSynedit;
end;

procedure TADRestDbg.LoadSettingsFromOriginalSynedit;
var
  ms: TMemoryStream;
  Config: TXMLConfigStorage;
begin
  ms:=TMemoryStream.Create;
  Config:=nil;
  try
    OriginalSynEdit.Lines.SaveToStream(ms);
    ms.Position:=0;
    Config:=TXMLConfigStorage.Create(ms);
    FSettings.LoadFromConfig(Config);
    Config.AppendBasePath('MainConfig/');
    FOriginalLayout.LoadFromConfig(Config);
    Config.UndoAppendBasePath;
  finally
    Config.Free;
    ms.Free;
  end;
end;

end.

