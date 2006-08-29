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
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  ObjectInspector, LazHelpIntf, IDEWindowIntf, IDEDialogs, Laz_XMLCfg,
  LazConf, LazarusIDEStrConsts, IDEProcs, IDEOptionDefs;

type
  { THelpOptions }

  THelpOptions = class
  private
    FFilename: string;
    FFPCDocsHTMLDirectory: string;
    procedure SetFPCDocsHTMLDirectory(const AValue: string);
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
  published
    property FPCDocsHTMLDirectory: string read FFPCDocsHTMLDirectory
                                          write SetFPCDocsHTMLDirectory;
  end;
  
  
  { THelpOptionsDialog }

  THelpOptionsDialog = class(TForm)
    FPCDocHTMLBrowseButton: TButton;
    FPCDocHTMLEdit: TEdit;
    FPCDocHTMLLabel: TLabel;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    DataBasePage: TPage;
    DatabasesLabel: TLabel;
    DataBasesPropsGroupBox: TGroupBox;
    DatabasesListBox: TListBox;
    GeneralPage: TPage;
    ViewerPropsGroupBox: TGroupBox;
    ViewersLabel: TLabel;
    ViewersListBox: TListBox;
    MainNotebook: TNotebook;
    ViewersPage: TPage;
    procedure CancelButtonClick(Sender: TObject);
    procedure DatabasesListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FPCDocHTMLBrowseButtonClick(Sender: TObject);
    procedure HelpOptionsDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure HelpOptionsDialogCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ViewersListBoxSelectionChange(Sender: TObject; User: boolean);
  private
  public
    ViewersPropertiesGrid: TCustomPropertiesGrid;
    DatabasesPropertiesGrid: TCustomPropertiesGrid;
    procedure FillGeneralPage;
    procedure FillViewersList;
    procedure FillViewerPropGrid;
    procedure FillDatabasesList;
    procedure FillDatabasesPropGrid;
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
  GeneralPage.Caption:=lisMenuInsertGeneral;
  FPCDocHTMLLabel.Caption:=lisHOFPCDocHTMLPath;
  ViewersPage.Caption:=lisHlpOptsViewers;
  ViewerPropsGroupBox.Caption:=lisHlpOptsProperties;
  ViewersLabel.Caption:=lisHlpOptsViewers;
  DataBasePage.Caption:=lisHlpOptsDatabases;
  DataBasesPropsGroupBox.Caption:=lisHlpOptsProperties;
  DatabasesLabel.Caption:=lisHlpOptsDatabases;

  ViewersPropertiesGrid:=TCustomPropertiesGrid.Create(Self);
  with ViewersPropertiesGrid do begin
    Name:='ViewersPropertiesGrid';
    Parent:=ViewerPropsGroupBox;
    Align:=alClient;
  end;
  
  DatabasesPropertiesGrid:=TCustomPropertiesGrid.Create(Self);
  with DatabasesPropertiesGrid do begin
    Name:='DatabasesPropertiesGrid';
    Parent:=DataBasesPropsGroupBox;
    Align:=alClient;
  end;

  FillGeneralPage;
  FillViewersList;
  FillViewerPropGrid;
  FillDatabasesList;
  FillDatabasesPropGrid;
end;

procedure THelpOptionsDialog.OkButtonClick(Sender: TObject);
begin
  HelpOpts.FPCDocsHTMLDirectory:=FPCDocHTMLEdit.Text;
  ModalResult:=mrOk;
end;

procedure THelpOptionsDialog.ViewersListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  FillViewerPropGrid;
end;

procedure THelpOptionsDialog.FillGeneralPage;
begin
  FPCDocHTMLEdit.Text:=HelpOpts.FPCDocsHTMLDirectory;
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

procedure THelpOptionsDialog.FillDatabasesList;
var
  i: Integer;
  HelpDB: THelpDatabase;
begin
  if (HelpDatabases=nil) then begin
    DatabasesListBox.Items.Clear;
    exit;
  end;
  DatabasesListBox.Items.BeginUpdate;
  for i:=0 to HelpDatabases.Count-1 do begin
    HelpDB:=HelpDatabases[i];
    if DatabasesListBox.Items.Count>i then
      DatabasesListBox.Items[i]:=HelpDB.GetLocalizedName
    else
      DatabasesListBox.Items.Add(HelpDB.GetLocalizedName);
  end;
  while DatabasesListBox.Items.Count>HelpDatabases.Count do
    DatabasesListBox.Items.Delete(DatabasesListBox.Items.Count-1);
  if (DatabasesListBox.ItemIndex<0) and (DatabasesListBox.Items.Count>0) then
    DatabasesListBox.ItemIndex:=0;
  DatabasesListBox.Items.EndUpdate;
end;

procedure THelpOptionsDialog.FillDatabasesPropGrid;
var
  i: LongInt;
begin
  i:=DatabasesListBox.ItemIndex;
  if (HelpDatabases=nil) or (i<0) or (i>=HelpDatabases.Count) then begin
    DatabasesPropertiesGrid.TIObject:=nil;
  end else begin
    DatabasesPropertiesGrid.TIObject:=HelpDatabases[i];
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

procedure THelpOptionsDialog.DatabasesListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  FillDatabasesPropGrid;
end;

procedure THelpOptionsDialog.FPCDocHTMLBrowseButtonClick(Sender: TObject);
var
  NewFilename: String;
begin
  NewFilename:=LazSelectDirectory('FPC Doc HTML directory','');
  if NewFilename='' then exit;
  FPCDocHTMLEdit.Text:=NewFilename;
end;

{ THelpOptions }

procedure THelpOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure THelpOptions.SetFPCDocsHTMLDirectory(const AValue: string);
begin
  if FFPCDocsHTMLDirectory=AValue then exit;
  FFPCDocsHTMLDirectory:=AValue;
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
  FFPCDocsHTMLDirectory:='';
end;

procedure THelpOptions.Load;
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;
  Storage: TXMLOptionsStorage;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    try
      FileVersion:=XMLConfig.GetValue('HelpOptions/Version/Value',0);
      if (FileVersion<>0) and (FileVersion<HelpOptionsVersion) then
        DebugLn('Note: Loading old Help options file', FFileName);
      FPCDocsHTMLDirectory:=
                    XMLConfig.GetValue('HelpOptions/FPCDocs/HTML/Directory','');
        
      if HelpViewers<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Viewers');
        try
          HelpViewers.Load(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      if HelpDatabases<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Databases');
        try
          HelpDatabases.Load(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[THelpOptions.Load]  error reading "',FFilename,'": ',E.Message);
    end;
  end;
end;

procedure THelpOptions.Save;
var
  XMLConfig: TXMLConfig;
  Storage: TXMLOptionsStorage;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FFileName);
    try
      XMLConfig.SetValue('HelpOptions/Version/Value',HelpOptionsVersion);
      XMLConfig.SetDeleteValue('HelpOptions/FPCDocs/HTML/Directory',
                               FPCDocsHTMLDirectory,'');

      if HelpViewers<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Viewers');
        try
          HelpViewers.Save(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      if HelpDatabases<>nil then begin
        Storage:=TXMLOptionsStorage.Create(XMLConfig,'Databases');
        try
          HelpDatabases.Save(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[THelpOptions.Save]  error writing "',FFilename,'": ',E.Message);
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
    DebugLn('NOTE: help options config file not found - using defaults');
  end;
  FFilename:=ConfFilename;
end;

procedure THelpOptions.Assign(HelpOpts: THelpOptions);
begin
  FPCDocsHTMLDirectory:=HelpOpts.FPCDocsHTMLDirectory;
end;

function THelpOptions.IsEqual(HelpOpts: THelpOptions): boolean;
begin
  Result:=FPCDocsHTMLDirectory=HelpOpts.FPCDocsHTMLDirectory;
end;

function THelpOptions.CreateCopy: THelpOptions;
begin
  Result:=THelpOptions.Create;
  Result.Assign(Self);
end;

initialization
  {$I helpoptions.lrs}

end.

