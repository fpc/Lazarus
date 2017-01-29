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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, LazFileUtils, LazLogger, SynEdit,
  SynHighlighterXML, AnchorDocking, AnchorDockStorage, ADLayoutViewer, Forms,
  Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls,
  XMLPropStorage;

type

  { TADRestDbg }

  TADRestDbg = class(TForm)
    OriginalFileLabel: TLabel;
    OriginalLayoutPanel: TPanel;
    OriginalLayoutToolBar: TPanel;
    OriginalZoomTrackBar: TTrackBar;
    RestoredFileLabel: TLabel;
    RestoredLayoutPanel: TPanel;
    RestoredLayoutToolBar: TPanel;
    RestoredZoomLabel: TLabel;
    RestoredZoomTrackBar: TTrackBar;
    SplitterXMLLayout: TSplitter;
    SplitterBetweenXML: TSplitter;
    OriginalSynEdit: TSynEdit;
    RestoredSynEdit: TSynEdit;
    SplitterBetweenLayouts: TSplitter;
    SynXMLSyn1: TSynXMLSyn;
    MainToolBar: TToolBar;
    OpenToolButton: TToolButton;
    OpenRecentToolButton: TToolButton;
    OriginalZoomLabel: TLabel;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenRecentToolButtonClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure OriginalZoomTrackBarChange(Sender: TObject);
    procedure RestoredZoomTrackBarChange(Sender: TObject);
  private
    FConfigFilename: string;
    FOriginalFilename: string;
    FSettings: TAnchorDockSettings;
    function GetOriginalLayout: TAnchorDockLayoutTree;
    function GetRestoredLayout: TAnchorDockLayoutTree;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    OriginalView: TADLayoutTreeView;
    RestoredView: TADLayoutTreeView;
    procedure OpenLayout(Filename: string);
    procedure LoadSettingsFromOriginalSynedit;
    procedure ComputeRestoredLayout;
    procedure LoadRestoredXML;
    property Settings: TAnchorDockSettings read FSettings;
    property OriginalLayout: TAnchorDockLayoutTree read GetOriginalLayout;
    property OriginalFilename: string read FOriginalFilename;
    property RestoredLayout: TAnchorDockLayoutTree read GetRestoredLayout;
    property ConfigFilename: string read FConfigFilename write FConfigFilename;
  end;

var
  ADRestDbg: TADRestDbg;

implementation

{$R *.lfm}

{ TADRestDbg }

procedure TADRestDbg.FormCreate(Sender: TObject);
begin
  FConfigFilename:=GetAppConfigFileUTF8(false);
  DebugLn(['TADRestDbg.FormCreate ',FConfigFilename]);

  Caption:='Anchordocking Restore Debugger';
  FSettings:=TAnchorDockSettings.Create;

  OriginalFileLabel.Caption:='Original: '+OriginalFilename;
  RestoredFileLabel.Caption:='Restored as XML:';
  OpenToolButton.Caption:='Open Config File';
  OpenRecentToolButton.Caption:='Open Recent';

  OriginalView:=TADLayoutTreeView.Create(Self);
  with OriginalView do begin
    Name:='OriginalView';
    Parent:=OriginalLayoutPanel;
    Align:=alClient;
    ZoomTrackbar:=OriginalZoomTrackBar;
  end;

  RestoredView:=TADLayoutTreeView.Create(Self);
  with RestoredView do begin
    Name:='RestoredView';
    Parent:=RestoredLayoutPanel;
    Align:=alClient;
    ZoomTrackbar:=RestoredZoomTrackBar;
  end;

  LoadConfig;

  if Paramcount>0 then
    OpenLayout(ParamStrUTF8(1));
end;

procedure TADRestDbg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure TADRestDbg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
end;

procedure TADRestDbg.OpenRecentToolButtonClick(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TADRestDbg.OpenToolButtonClick(Sender: TObject);
begin
  ShowMessage('not implemented yet');
end;

procedure TADRestDbg.OriginalZoomTrackBarChange(Sender: TObject);
begin
  OriginalZoomLabel.Caption:=FloatToStrF(OriginalView.Scale,ffGeneral,6,3);
end;

procedure TADRestDbg.RestoredZoomTrackBarChange(Sender: TObject);
begin
  RestoredZoomLabel.Caption:=FloatToStrF(RestoredView.Scale,ffGeneral,6,3);
end;

function TADRestDbg.GetOriginalLayout: TAnchorDockLayoutTree;
begin
  Result:=OriginalView.Layout;
end;

function TADRestDbg.GetRestoredLayout: TAnchorDockLayoutTree;
begin
  Result:=RestoredView.Layout;
end;

procedure TADRestDbg.LoadConfig;
var
  Cfg: TXMLConfigStorage;
  NewBounds: TRect;
begin
  try
    Cfg:=TXMLConfigStorage.Create(ConfigFilename,true);
    try
      Cfg.GetValue('Bounds',NewBounds,Rect(0,0,0,0));
      if (NewBounds.Right>NewBounds.Left)
      and (NewBounds.Bottom>NewBounds.Top) then
        BoundsRect:=NewBounds;
      SplitterXMLLayout.Left:=Max(1,Cfg.GetValue('Splitter/BetweenXMLAndLayout/Left',SplitterXMLLayout.Left));
      SplitterBetweenXML.Top:=Max(1,Cfg.GetValue('Splitter/BetweenXML/Top',SplitterBetweenXML.Top));
      SplitterBetweenLayouts.Top:=Max(1,Cfg.GetValue('Splitter/BetweenLayout/Top',SplitterBetweenLayouts.Top));
      OriginalView.Scale:=Cfg.GetValue('Original/Layout/Scale',250)/1000;
      RestoredView.Scale:=Cfg.GetValue('Restored/Layout/Scale',250)/1000;
    finally
      Cfg.Free;
    end;
  except
    on E:Exception do begin
      debugln(['TADRestDbg.LoadConfig ',E.Message]);
    end;
  end;
end;

procedure TADRestDbg.SaveConfig;
var
  Cfg: TXMLConfigStorage;
begin
  if not ForceDirectoriesUTF8(ExtractFilePath(ConfigFilename)) then begin
    debugln(['WARNING: TADRestDbg.SaveConfig: can not create directory  ',ExtractFilePath(ConfigFilename)]);
  end;
  try
    Cfg:=TXMLConfigStorage.Create(ConfigFilename,false);
    try
      Cfg.SetDeleteValue('Bounds',BoundsRect,Rect(0,0,0,0));
      Cfg.SetDeleteValue('Splitter/BetweenXMLAndLayout/Left',SplitterXMLLayout.Left,-1);
      Cfg.SetDeleteValue('Splitter/BetweenXML/Top',SplitterBetweenXML.Top,-1);
      Cfg.SetDeleteValue('Splitter/BetweenLayout/Top',SplitterBetweenLayouts.Top,-1);
      Cfg.SetDeleteValue('Original/Layout/Scale',round(OriginalView.Scale*1000),250);
      Cfg.SetDeleteValue('Restored/Layout/Scale',round(RestoredView.Scale*1000),250);
    finally
      Cfg.Free;
    end;
  except
    on E:Exception do begin
      debugln(['TADRestDbg.SaveConfig ',E.Message]);
    end;
  end;
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
  OriginalFileLabel.Caption:='Original: '+Filename;
  LoadSettingsFromOriginalSynedit;
  ComputeRestoredLayout;
  LoadRestoredXML;
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
    OriginalLayout.LoadFromConfig(Config);
    Config.UndoAppendBasePath;
  finally
    Config.Free;
    ms.Free;
    OriginalView.LayoutChanged;
  end;
end;

procedure TADRestDbg.ComputeRestoredLayout;
begin
  RestoredLayout.Assign(OriginalLayout);
  // ToDo
  RestoredView.LayoutChanged;
end;

procedure TADRestDbg.LoadRestoredXML;
var
  Config: TXMLConfigStorage;
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  Config:=TXMLConfigStorage.Create('dummy.xml',false);
  try
    RestoredLayout.SaveToConfig(Config);
    Config.SaveToStream(ms);
    ms.Position:=0;
    RestoredSynEdit.Lines.LoadFromStream(ms);
  finally
    Config.Free;
    ms.Free;
  end;
end;

end.

