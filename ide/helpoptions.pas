{ /***************************************************************************
                     helpoptions.pas  -  Lazarus IDE unit
                     ------------------------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    - THelpOptions and THelpOptsDlg
}
unit HelpOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, HelpIntf, Laz_XMLCfg, ObjectInspector,
  LazConf, LazarusIDEStrConsts, IDEOptionDefs, StdCtrls;

type
  { THelpOptions }

  THelpOptions = class
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure Save;
    procedure SetDefaultFilename;
    procedure Assign(HelpOpts: THelpOptions);
    function IsEqual(HelpOpts: THelpOptions): boolean;
    function CreateCopy: THelpOptions;
  public
    property Filename: string read FFilename write SetFilename;
  end;
  
  
  { THelpOptionsDialog }

  THelpOptionsDialog = class(TForm)
    ViewerPropsGroupBox: TGroupBox;
    ViewersLabel: TLabel;
    ViewersListBox: TListBox;
    MainNotebook: TNotebook;
    OkButton: TButton;
    CancelButton: TButton;
    ViewersPage: TPage;
    procedure CancelButtonClick(Sender: TObject);
    procedure HelpOptionsDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure HelpOptionsDialogCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ViewersListBoxSelectionChange(Sender: TObject; User: boolean);
  private
  public
    ViewersPropertiesGrid: TCustomPropertiesGrid;
    procedure FillViewersList;
    procedure FillViewerPropGrid;
  end;
  
var
  HelpOpts: THelpOptions; // set by the IDE

const
  HelpOptionsVersion = 1;
  DefaultHelpOptsFile = 'helpoptions.xml';

function ShowHelpOptionsDialog: TModalResult;


implementation

function ShowHelpOptionsDialog: TModalResult;
var
  HelpOptionsDialog: THelpOptionsDialog;
begin
  HelpOptionsDialog:=THelpOptionsDialog.Create(nil);
  try
    Result:=HelpOptionsDialog.ShowModal;
  finally
    HelpOptionsDialog.Free;
  end;
end;

{ THelpOptionsDialog }

procedure THelpOptionsDialog.HelpOptionsDialogCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,500,300);

  Caption:=lisHlpOptsHelpOptions;
  OkButton.Caption:=lisLazBuildOk;
  CancelButton.Caption:=dlgCancel;
  ViewersPage.Caption:=lisHlpOptsViewers;
  ViewerPropsGroupBox.Caption:=lisHlpOptsProperties;
  ViewersLabel.Caption:=lisHlpOptsViewers;
  
  ViewersPropertiesGrid:=TCustomPropertiesGrid.Create(Self);
  with ViewersPropertiesGrid do begin
    Name:='ViewersPropertiesGrid';
    Parent:=ViewerPropsGroupBox;
    Align:=alClient;
  end;
  
  FillViewersList;
  FillViewerPropGrid;
end;

procedure THelpOptionsDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure THelpOptionsDialog.ViewersListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  FillViewerPropGrid;
end;

procedure THelpOptionsDialog.FillViewersList;
var
  i: Integer;
  Viewer: THelpViewer;
begin
  if (HelpViewers=nil) then begin
    ViewersListBox.Items.Clear;
    exit;
  end;
  ViewersListBox.Items.BeginUpdate;
  for i:=0 to HelpViewers.Count-1 do begin
    Viewer:=HelpViewers[i];
    if ViewersListBox.Items.Count>i then
      ViewersListBox.Items[i]:=Viewer.GetLocalizedName
    else
      ViewersListBox.Items.Add(Viewer.GetLocalizedName);
  end;
  while ViewersListBox.Items.Count>HelpViewers.Count do
    ViewersListBox.Items.Delete(ViewersListBox.Items.Count-1);
  if (ViewersListBox.ItemIndex<0) and (ViewersListBox.Items.Count>0) then
    ViewersListBox.ItemIndex:=0;
  ViewersListBox.Items.EndUpdate;
end;

procedure THelpOptionsDialog.FillViewerPropGrid;
var
  i: LongInt;
begin
  i:=ViewersListBox.ItemIndex;
  if (HelpViewers=nil) or (i<0) or (i>=HelpViewers.Count) then begin
    ViewersPropertiesGrid.TIObject:=nil;
  end else begin
    ViewersPropertiesGrid.TIObject:=HelpViewers[i];
  end;
end;

procedure THelpOptionsDialog.HelpOptionsDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure THelpOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  // ToDo: restore backup
  ModalResult:=mrCancel;
end;

{ THelpOptions }

procedure THelpOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

constructor THelpOptions.Create;
begin
  Clear;
end;

destructor THelpOptions.Destroy;
begin
  inherited Destroy;
end;

procedure THelpOptions.Clear;
begin

end;

procedure THelpOptions.Load;
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;
  Storage: TXMLOptionsStorage;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    Storage:=nil;
    try
      FileVersion:=XMLConfig.GetValue('HelpOptions/Version/Value',0);
      if (FileVersion<>0) and (FileVersion<HelpOptionsVersion) then
        writeln('Note: Loading old Help options file', FFileName);
        
      if HelpViewers<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Viewers');
        HelpViewers.Load(Storage);
      end;

    finally
      XMLConfig.Free;
      Storage.Free;
    end;
  except
    on E: Exception do begin
      writeln('[THelpOptions.Load]  error reading "',FFilename,'": ',E.Message);
    end;
  end;
end;

procedure THelpOptions.Save;
var
  XMLConfig: TXMLConfig;
  Storage: TXMLOptionsStorage;
begin
  try
    XMLConfig:=TXMLConfig.CreateClean(FFileName);
    Storage:=nil;
    try
      XMLConfig.SetValue('HelpOptions/Version/Value',HelpOptionsVersion);

      if HelpViewers<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Viewers');
        HelpViewers.Save(Storage);
      end;

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
      Storage.Free;
    end;
  except
    on E: Exception do begin
      writeln('[THelpOptions.Save]  error writing "',FFilename,'": ',E.Message);
    end;
  end;
end;

procedure THelpOptions.SetDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(
                             GetPrimaryConfigPath+'/'+DefaultHelpOptsFile);
  CopySecondaryConfigFile(DefaultHelpOptsFile);
  if (not FileExists(ConfFileName)) then begin
    writeln('NOTE: help options config file not found - using defaults');
  end;
  FFilename:=ConfFilename;
end;

procedure THelpOptions.Assign(HelpOpts: THelpOptions);
begin

end;

function THelpOptions.IsEqual(HelpOpts: THelpOptions): boolean;
begin
  Result:=true;
end;

function THelpOptions.CreateCopy: THelpOptions;
begin
  Result:=THelpOptions.Create;
  Result.Assign(Self);
end;

initialization
  {$I helpoptions.lrs}

end.

