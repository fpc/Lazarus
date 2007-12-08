{
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
}
unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdActns, ComCtrls, dicteditor, fpdatadict, IniPropStorage,
  conneditor,
  inifiles, inicol, RTTICtrls, ExtCtrls, StdCtrls, propertystorage, ddfiles;

type
  TEngineMenuItem = Class(TMenuItem)
  public
    FEngineName : String;
  end;
  
  TNewConnectionMenuItem = Class(TEngineMenuItem);
  TImportMenuItem = Class(TEngineMenuItem);
  
  TRecentImportMenuItem = Class(TMenuItem)
  Public
    FRecentConnection : TRecentConnection;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    AClose: TAction;
    ACloseAll: TAction;
    ACopyConnection: TAction;
    ADeleteConnection: TAction;
    ANewConnection: TAction;
    ASaveAs: TAction;
    AGenerateSQL: TAction;
    ADeleteField: TAction;
    ADeleteTable: TAction;
    ANewField: TAction;
    ANewTable: TAction;
    AOpen: TAction;
    AExit: TAction;
    ANew: TAction;
    ASave: TAction;
    ALMain: TActionList;
    ACopy: TEditCopy;
    ACut: TEditCut;
    APaste: TEditPaste;
    ILMain: TImageList;
    LVConnections: TListView;
    LVDicts: TListView;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MINewConnection: TMenuItem;
    MIConnection: TMenuItem;
    MISaveAs: TMenuItem;
    MIGenerateSQL: TMenuItem;
    MIDDSep3: TMenuItem;
    MIDeleteField: TMenuItem;
    MIDeleteTable: TMenuItem;
    MIDDSep2: TMenuItem;
    MINewField: TMenuItem;
    MIDDSep: TMenuItem;
    MINewTable: TMenuItem;
    MICloseAll: TMenuItem;
    MIClose: TMenuItem;
    MICloseSep: TMenuItem;
    MIPaste: TMenuItem;
    MICut: TMenuItem;
    MIImport: TMenuItem;
    MIDataDict: TMenuItem;
    PStatus: TPanel;
    PStatusText: TPanel;
    PBSTatus: TProgressBar;
    PSMain: TIniPropStorage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MIExit: TMenuItem;
    MISep: TMenuItem;
    MISave: TMenuItem;
    MIOpen: TMenuItem;
    MFIle: TMenuItem;
    ODDD: TOpenDialog;
    PCDD: TPageControl;
    SDDD: TSaveDialog;
    TSConnections: TTabSheet;
    ToolButton1: TToolButton;
    TBNewTable: TToolButton;
    TBNewField: TToolButton;
    ToolButton2: TToolButton;
    TBDeleteTable: TToolButton;
    TBDeleteField: TToolButton;
    ToolButton3: TToolButton;
    TBGenerateSQL: TToolButton;
    TSRecent: TTabSheet;
    TBMain: TToolBar;
    TBSave: TToolButton;
    TBOPen: TToolButton;
    TBNew: TToolButton;
    procedure ACloseAllExecute(Sender: TObject);
    procedure ACloseExecute(Sender: TObject);
    procedure ADeleteFieldExecute(Sender: TObject);
    procedure ADeleteFieldUpdate(Sender: TObject);
    procedure ADeleteTableExecute(Sender: TObject);
    procedure ADeleteTableUpdate(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AGenerateSQLExecute(Sender: TObject);
    procedure ANewConnectionExecute(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ANewFieldExecute(Sender: TObject);
    procedure ANewFieldUpdate(Sender: TObject);
    procedure ANewTableExecute(Sender: TObject);
    procedure ANewTableUpdate(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HaveConnection(Sender: TObject);
    procedure HaveDDEditor(Sender: TObject);
    procedure HaveTabs(Sender: TObject);
    procedure HaveTab(Sender: TObject);
    procedure HaveTables(Sender: TObject);
    procedure LVConnectionsDblClick(Sender: TObject);
    procedure LVDictsDblClick(Sender: TObject);
    procedure LVDictsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure PCDDChange(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
  private
    FRecentDicts : TRecentDataDicts;
    FRecentConnections : TRecentConnections;
    procedure AddRecentConnection(RC: TRecentConnection; AssumeNew: Boolean);
    procedure CheckParams;
    function CloseCurrentConnection: TModalResult;
    function CloseCurrentTab(AddCancelClose: Boolean = False): TModalResult;
    function GetConnectionName(var AName: String): Boolean;
    function GetCurrentConnection: TConnectionEditor;
    function GetCurrentEditor: TDataDictEditor;
    procedure NewConnection(EngineName : String);
    procedure NewConnection;
    procedure OpenConnection(RC: TRecentConnection);
    procedure RegisterDDEngines;
    Function SelectEngineType(Var EngineName : String) : Boolean;
    procedure ShowImportRecentconnections;
    procedure ShowNewConnectionTypes;
    procedure ShowRecentConnections;
    Procedure ShowRecentDictionaries;
    Procedure ShowDDImports;
    Procedure StartStatus;
    Procedure StopStatus;
    procedure ShowStatus(Const Msg : String);
    procedure RegisterConnectionCallBacks;
    procedure GetDBFDir(Sender : TObject; Var ADir : String);
    procedure GetSQLConnectionDlg(Sender : TObject; Var AConnection : String);
    Procedure AddRecentDict(DF : TRecentDataDict; AssumeNew : Boolean);
    Function  FindLi(LV : TListView;RI : TRecentItem) : TListItem;
    procedure ImportClick(Sender : TObject);
    procedure RecentImportClick(Sender : TObject);
    procedure NewConnectionClick(Sender : TObject);
    procedure DoImport(Const EngineName : String);
    procedure DoImport(Const EngineName,ConnectionString : String);
    Procedure DoDDEProgress(Sender : TObject; Const Msg : String);
    { private declarations }
  public
    { public declarations }
    procedure OpenDataDict(DDF : TRecentDataDict);
    procedure OpenFile(AFileName : String);
    procedure RaiseEditor(DDE: TDataDictEditor);
    Procedure SaveCurrentEditor;
    Procedure SaveCurrentEditorAs;
    Function CloseAllEditors : Boolean;
    Function CloseCurrentEditor (AddCancelClose : Boolean = False) : TModalResult;
    Function NewDataDict : TFPDataDictionary;
    Function NewDataEditor : TDataDictEditor;
    function NewConnectionEditor(AName : String): TConnectionEditor;
    procedure DeleteCurrentTable;
    procedure DeleteCurrentField;
    Procedure DoNewTable;
    Procedure DoNewField;
    procedure ShowGenerateSQL;
    Property CurrentEditor : TDataDictEditor Read GetCurrentEditor;
    Property CurrentConnection : TConnectionEditor Read GetCurrentConnection;
  end; 


var
  MainForm: TMainForm;

implementation

uses
  // Data dictionary support for
  fpdddbf,     // DBF
  fpddfb,      // Firebird
  fpddmysql40, // MySQL 4.0
  fpddmysql41, // MySQL 4.1
  fpddmysql50, // MySQL 5.0
  fpddoracle,  // Oracle
  fpddpq,      // PostgreSQL
  fpddsqlite3, // SQLite 3
  fpddodbc,    // Any ODBC supported
  frmimportdd,frmgeneratesql,fpddsqldb,frmSQLConnect,fpstdexports;

ResourceString
  SSaveData     = 'Save changes';
  SDontSave     = 'Discard changes';
  SDontClose    = 'Do not close editor';
  SConfirmClose = 'Confirm close';
  SDDModified   = 'Data dictionary "%s" has changed.'#13#10+
                  'What do you want to do with the changes?';
  SImportDictInto = 'Import datadictionary';
  SWhichCurrentDictToUse = 'A data dictionary is active.'+
                           'Would you like to import into this data dictionary ?';
  SUseCurrentDict = 'Yes, use the active dictionary';
  SUseNewDict     = 'No, import in a new dictionary';
  SNewTable       = 'Create new table';
  SNewTableName   = 'Enter a name for the new table:';
  SNewField       = 'Create new field in table %s';
  SNewFieldName   = 'Enter a name for the new field:';
  SSelectDBFDir   = 'Select a directory with DBF files';
  SNewConnection  = 'New connection';
  SConnectionDescription = 'Enter a descriptive name for the connection';
  SConnectionNameExists = 'There is already a connection named "%s"'#13#10+
                          'Would you like to override the connection data ?';
  SUnknownDictionary = 'Unknown data dictionary: %s';
  SUnknownConnection = 'Unknown connection: %s';

  
{ ---------------------------------------------------------------------
  TMainform events
  ---------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);

begin
  FRecentDicts.Save;
  FRecentConnections.Save;
  CloseAction:=caFree;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

begin
  CanClose:=True;
  If PCDD.PageCount>2 then
    begin
    PCDD.ActivePageIndex:=PCDD.PageCount-1;
    While Canclose and (CurrentEditor<>Nil) do
      CanClose:=CloseCurrentEditor(True)<>mrCancel;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

Var
  FN : String;

begin
  // Register DD engines.
  RegisterDDEngines;
  // Register standard export formats.
  RegisterStdFormats;
  FRecentDicts:=TRecentDataDicts.Create(TRecentDataDict);
  FRecentConnections:=TRecentConnections.Create(TRecentConnection);
  FN:=GetAppConfigDir(False);
  ForceDirectories(FN);
  FN:=GetAppConfigFile(False);
  FRecentDicts.LoadFromFile(FN,'RecentDicts');
  FRecentConnections.LoadFromFile(FN,'RecentConnections');
  ShowRecentDictionaries;
  ShowRecentConnections;
  ShowDDImports;
  ShowNewConnectionTypes;
  PSMain.IniFileName:=ChangeFileExt(FN,'.ini');
  LVDicts.Columns[0].Width:=150;
  LVDicts.Columns[1].Width:=150;
  LVDicts.Columns[2].Width:=150;
  RegisterConnectionCallBacks;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRecentDicts);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CheckParams;
end;

procedure TMainForm.CheckParams;

Var
  S : String;
  D : TRecentDatadict;
  C : TRecentConnection;
  
begin
  With Application do
  If HasOption('d','dictionary') then
    begin
    S:=GetOptionValue('d','dictionary');
    D:=FRecentDicts.FindFromName(S);
    If (D<>Nil) then
      OpenDataDict(D)
    else
      ShowMessage(Format(SUnknownDictionary,[S]));
    end
  else If HasOption('f','filename') then
    begin
    S:=GetOptionValue('f','filename');
    If FileExists(S) then
      OpenFile(S);
    end
  else If HasOption('c','connection') then
    begin
    S:=GetOptionValue('c','connection');
    C:=FRecentConnections.FindFromName(S);
    If (C<>Nil) then
      OpenConnection(C)
    else
      ShowMessage(Format(SUnknownConnection,[S]));
    end;

end;


procedure TMainForm.HaveConnection(Sender: TObject);
begin
  (Sender as TAction).Enabled:=True
end;


{ ---------------------------------------------------------------------
  Main form auxiliary methods
  ---------------------------------------------------------------------}
  
procedure TMainForm.RegisterDDEngines;

begin
  RegisterFBDDEngine;
  RegisterMySQL40DDEngine;
  RegisterMySQL41DDEngine;
  RegisterMySQL50DDEngine;
  RegisterOracleDDEngine;
  RegisterPostgreSQLDDengine;
  RegisterSQLite3DDEngine;
  RegisterODBCDDengine;
end;

procedure TMainForm.RegisterConnectionCallBacks;

Var
  L : TStringList;
  
  Procedure MaybeRegisterConnectionStringCallback(RC : String;CallBack : TGetConnectionEvent);
  
  begin
    If L.IndexOf(Rc)<>-1 then
      RegisterConnectionStringCallback(RC,Callback);
  end;

begin
  L:=TStringList.Create;
  try
    GetDictionaryEngineList(L);
    MaybeRegisterConnectionStringCallback('TDBFDDEngine',@GetDBFDir);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql40DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql41DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql5DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBODBCDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBPOSTGRESQLDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBIBDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBSQLite3DDEngine',@GetSQLConnectionDlg);
  finally
    L.free;
  end;

end;


procedure TMainForm.RaiseEditor(DDE: TDataDictEditor);

begin
  PCDD.ActivePage:=DDE;
end;

function TMainForm.GetCurrentEditor: TDataDictEditor;

Var
  TS : TTabSheet;

begin
  TS:=PCDD.ActivePage;
  if TS is TDataDictEditor then
    Result:=TDataDictEditor(TS)
  else
    Result:=Nil;
end;

function TMainForm.GetCurrentConnection: TConnectionEditor;

Var
  TS : TTabSheet;

begin
  TS:=PCDD.ActivePage;
  if TS is TConnectionEditor then
    Result:=TConnectionEditor(TS)
  else
    Result:=Nil;
end;

procedure TMainForm.ShowRecentDictionaries;

Var
  DF : TRecentDataDict;
  I : Integer;

begin
  LVDicts.Items.Clear;
  For I:=0 to FRecentDicts.Count-1 do
    begin
    DF:=FRecentDicts[i];
    AddRecentDict(DF,True);
    end;
end;

procedure TMainForm.ShowRecentConnections;

Var
  RC : TRecentConnection;
  I : Integer;

begin
  LVConnections.Items.Clear;
  For I:=0 to FRecentConnections.Count-1 do
    begin
    RC:=FRecentConnections[i];
    AddRecentConnection(RC,True);
    end;
end;

procedure TMainForm.ShowDDImports;

Var
  MI : TImportMenuItem;
  L : TStringList;
  dd,dt : string;
  i : integer;
  cap : TFPDDEngineCapabilities;

begin
  MIImport.Clear;
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    GetDictionaryEngineList(L);
    For i:=0 to L.Count-1 do
      begin
      GetDictionaryEngineInfo(L[i],dd,dt,cap);
      if ecImport in cap then
        begin
        MI:=TImportMenuItem.Create(Self);
        MI.Caption:=dt;
        MI.FEngineName:=L[i];
        MI.OnClick:=@ImportClick;
        MIImport.Add(MI);
        // make sure it gets loaded
        PSMain.StoredValue[L[i]]:='';
        end;
      end;
    ShowImportRecentconnections;
  Finally
    L.Free;
  end;
  MIImport.Enabled:=(MIImport.Count>0);
end;

procedure TMainForm.ShowImportRecentconnections;

Var
  MR : TMenuItem;
  MI : TRecentImportMenuItem;
  L : TStringList;
  dd,dt : string;
  i : integer;
  cap : TFPDDEngineCapabilities;
  RC: TRecentConnection;

begin
  If (FRecentConnections.Count>0) then
    begin
    MR:=TMenuItem.Create(Self);
    MR.Caption:='From connection';
    MIimport.Insert(0,MR);
    L:=TStringList.Create;
    Try
      For I:=0 to FRecentConnections.Count-1 do
        L.AddObject(FRecentConnections[i].Name,FRecentConnections[i]);
      L.Sort;
      For I:=0 to L.Count-1 do
        begin
        RC:=L.Objects[i] as TRecentConnection;
        GetDictionaryEngineInfo(RC.EngineName,dd,dt,cap);
        if (ecImport in cap) then
          begin
          MI:=TRecentImportMenuItem.Create(Self);
          MI.Caption:=L[i];
          MI.FRecentConnection:=RC;
          MI.OnClick:=@RecentImportClick;
          MR.Add(MI);
          end;
        end;
    Finally
      L.Free;
    end;
    end;
  MIImport.Enabled:=(MIImport.Count>0);
end;


procedure TMainForm.ShowNewConnectionTypes;

Var
  MI : TNewConnectionMenuItem;
  L : TStringList;
  dd,dt : string;
  i : integer;
  cap : TFPDDEngineCapabilities;

begin
  MINewConnection.Clear;
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    GetDictionaryEngineList(L);
    For i:=0 to L.Count-1 do
      begin
      GetDictionaryEngineInfo(L[i],dd,dt,cap);
      MI:=TNewConnectionMenuItem.Create(Self);
      MI.Caption:=dt;
      MI.FEngineName:=L[i];
      MI.OnClick:=@NewConnectionClick;
      MINewConnection.Add(MI);
      // make sure it gets loaded
      PSMain.StoredValue[L[i]]:='';
      end;
  Finally
    L.Free;
  end;
  MIImport.Enabled:=(MIImport.Count>0);
end;

procedure TMainForm.StartStatus;
begin
  PBStatus.Position:=0;
  PSTatusText.Caption:='';
  PStatus.Visible:=True;
  Application.ProcessMessages;
end;

procedure TMainForm.StopStatus;
begin
  PStatus.Visible:=False;
  PBStatus.Position:=0;
  PSTatusText.Caption:='';
  Application.ProcessMessages;
end;

procedure TMainForm.ShowStatus(Const Msg : String);

begin
  PStatusText.Caption:=Msg;
  With PBStatus do
    begin
    If Position>=Max then
      Position:=0;
    PBStatus.StepIt;
    end;
  Application.ProcessMessages;
end;

Procedure TMainForm.DoDDEProgress(Sender : TObject; Const Msg : String);

begin
  ShowStatus(Msg);
end;

procedure TMainForm.ImportClick(Sender : TObject);

begin
  DoImport((Sender as TEngineMenuItem).FEngineName);
end;

procedure TMainForm.RecentImportClick(Sender : TObject);

Var
  RC : TRecentConnection;

begin
  RC:=(Sender as TRecentImportMenuItem).FRecentConnection;
  DoImport(RC.EngineName,RC.ConnectionString);
end;

procedure TMainForm.NewConnectionClick(Sender : TObject);

begin
  NewConnection((Sender as TEngineMenuItem).FEngineName);
end;


procedure TMainForm.GetSQLConnectionDlg(Sender : TObject; Var AConnection : String);

Var
  Last : String;
  
begin
  Last:=PSmain.StoredValue[Sender.ClassName];
  With (Sender as TSQLDBDDEngine) do
    AConnection:=GetSQLDBConnectString(HostSupported,Last);
  If (AConnection<>'') then
    PSmain.StoredValue[Sender.ClassName]:=AConnection;
end;

procedure TMainForm.GetDBFDir(Sender : TObject; Var ADir : String);

Var
  IDir : String;

begin
  IDir:=PSmain.StoredValue[Sender.ClassName];
  if (IDir='') then
    IDir:=ExtractFilePath(ParamStr(0));
  if SelectDirectory(SSelectDBFDir,IDir,ADir) then
    PSMain.StoredValue[Sender.ClassName]:=ADir
end;

{ ---------------------------------------------------------------------
  Action Execute/Update handlers
  ---------------------------------------------------------------------}

procedure TMainForm.AExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AGenerateSQLExecute(Sender: TObject);
begin
  ShowGenerateSQL;
end;

procedure TMainForm.ANewConnectionExecute(Sender: TObject);
begin
  NewConnection;
end;


procedure TMainForm.ACloseExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

procedure TMainForm.ADeleteFieldExecute(Sender: TObject);
begin
  DeleteCurrentField;
end;

procedure TMainForm.ADeleteFieldUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentEditor) and
                               Assigned(CurrentEditor.CurrentField);
end;

procedure TMainForm.ADeleteTableExecute(Sender: TObject);
begin
  DeleteCurrentTable;
end;

procedure TMainForm.ADeleteTableUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentEditor) and
                               Assigned(CurrentEditor.CurrentTable);
end;

procedure TMainForm.ACloseAllExecute(Sender: TObject);
begin
  CloseAllEditors;
end;

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  NewDataDict;
end;

procedure TMainForm.ANewFieldExecute(Sender: TObject);
begin
  DoNewField;
end;

procedure TMainForm.ANewFieldUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>nil)
                                and (CurrentEditor.CurrentTable<>Nil);
end;

procedure TMainForm.ANewTableExecute(Sender: TObject);
begin
  DoNewTable;
end;

procedure TMainForm.ANewTableUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil);
end;

procedure TMainForm.AOpenExecute(Sender: TObject);

begin
  With ODDD do
    If Execute then
     OpenFile(FileName);
end;

procedure TMainForm.ASaveExecute(Sender: TObject);

begin
  SaveCurrentEditor;
end;


procedure TMainForm.HaveDDEditor(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil);
end;

procedure TMainForm.HaveTabs(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil) or (CurrentConnection<>Nil);
end;

procedure TMainForm.HaveTab(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(PCDD.PageCount>2);
end;

procedure TMainForm.HaveTables(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentEditor)
                               and (CurrentEditor.DataDictionary.Tables.Count>0);
end;

procedure TMainForm.LVConnectionsDblClick(Sender: TObject);
begin
  If (LVConnections.Selected<>Nil)
     and (LVConnections.Selected.Data<>Nil) then
   OpenConnection(TRecentConnection(LVConnections.Selected.Data));
end;

procedure TMainForm.SaveAsExecute(Sender: TObject);
begin
  SaveCurrentEditorAs;
end;


{ ---------------------------------------------------------------------
  File menu commands
  ---------------------------------------------------------------------}
  
procedure TMainForm.OpenDataDict(DDF : TRecentDataDict);

begin
  If (DDF<>Nil) then
    If (DDF.UserData<>Nil) then
      RaiseEditor(DDF.UserData as TDataDictEditor)
    else
      OpenFile(DDF.FileName);
end;

procedure TMainForm.OpenFile(AFileName: String);

Var
  DDE : TDataDictEditor;
  DDF : TRecentDataDict;
  B : Boolean;

begin
  DDE:=NewDataEditor;
  StartStatus;
  Try
    DDE.LoadFromFile(AFileName);
  Finally
    StopStatus;
  end;
  DDF:=FRecentDicts.FindFromFileName(AFileName);
  B:=(DDF=Nil);
  If B then
    begin
    DDF:=FRecentDicts.AddDataDict(AFileName);
    DDF.Name:=DDE.DataDictionary.Name;
    end;
  DDF.Use;
  DDF.UserData:=DDE;
  AddRecentDict(DDF,B);
end;

procedure TMainForm.SaveCurrentEditor;
begin
  If Assigned(CurrentEditor) then
    With CurrentEditor.DataDictionary do
      if (FileName='') Then
        SaveCurrentEditorAs
      else
        begin
        StartStatus;
        Try
          CurrentEditor.SaveToFile(FileName);
        Finally
          StopStatus;
        end;
        end;
end;

procedure TMainForm.SaveCurrentEditorAs;

Var
  DDF : TRecentDataDict;

begin
  If Assigned(CurrentEditor) then
    With CurrentEditor,SDDD do
      begin
      If DataDictionary.FileName<>'' then
        FileName:=DataDictionary.FileName;
      If Execute then
        begin
        StartStatus;
        Try
          SaveToFile(FileName);
        Finally
          StopStatus;
        end;
        DDF:=FRecentDicts.FindFromUserData(CurrentEditor);
        If (DDF<>Nil) then
          DDF.UserData:=Nil;
        DDF:=FRecentDicts.AddDataDict(FileName);
        DDF.Name:=CurrentEditor.DataDictionary.Name;
        DDF.UserData:=CurrentEditor;
        DDF.Use;
        AddRecentDict(DDF,True);
        end;
      end;
end;

Function TMainForm.CloseAllEditors : Boolean;

begin
  Result:=True;
  PCDD.ActivePageIndex:=PCDD.PageCount-1;
  While Result and ((CurrentEditor<>Nil) or (CurrentConnection<>Nil)) do
    Result:=CloseCurrentTab(True)<>mrCancel;
end;

Function TMainForm.CloseCurrentTab(AddCancelClose : Boolean = False) : TModalResult;

begin
  If (CurrentEditor<>Nil) then
    Result:=CloseCurrentEditor(AddCancelClose)
  else if (CurrentConnection<>Nil) then
    begin
    CloseCurrentConnection;
    Result:=mrOK;
    end;
end;

Function TMainForm.CloseCurrentConnection : TModalResult;

Var
  CE : TConnectionEditor;

begin
  CE:=CurrentConnection;
  CE.DisConnect;
  Application.ReleaseComponent(ce);
  Result:=mrOK;
end;

Function TMainForm.CloseCurrentEditor(AddCancelClose : Boolean) : TModalResult;

Var
  DD : TDataDictEditor;
  Msg : String;
  DDF : TRecentDataDict;

begin
  Result:=mrYes;
  DD:=CurrentEditor;
  If (DD=Nil) then
    Exit;
  If DD.Modified then
    begin
    Msg:=Format(SDDModified,[DD.DataDictionary.Name]);
    if AddCancelClose then
      Result:=QuestionDLg(SConfirmClose,Msg,mtConfirmation,
                [mrYes,SSaveData,mrNo,SDontSave,mrCancel,SDontClose],0)
    else
      Result:=QuestionDLg(SConfirmClose,Msg,mtConfirmation,
                [mrYes,SSaveData,mrNo,SDontSave],0);
    If Result=mrYes then
      SaveCurrentEditor;
    end;
  if (Result<>mrCancel) then
    begin
    DDF:=FRecentDicts.FindFromUserData(DD);
    If (DDF<>Nil) then
      DDF.UserData:=Nil;
    DD.Free;
    end;
end;

{ ---------------------------------------------------------------------
  Data dictionary Editor Auxiliary routines
  ---------------------------------------------------------------------}

Function TMainForm.NewDataDict : TFPDataDictionary;

Var
  DD : TDataDictEditor;

begin
  DD:=NewDataEditor;
  Result:=DD.DataDictionary;
end;

function TMainForm.NewDataEditor: TDataDictEditor;
begin
  Result:=TDataDictEditor.Create(Self);
  Result.PageControl:=PCDD;
  Result.Parent:=PCDD;
  PCDD.ActivePage:=Result;
  Result.DataDictionary.OnProgress:=@DoDDEprogress;
end;

{ ---------------------------------------------------------------------
  Data dictionary commands
  ---------------------------------------------------------------------}


procedure TMainForm.ShowGenerateSQL;

begin
  With TGenerateSQLFOrm.Create(Self) do
    try
      TableDefs:=CurrentEditor.DataDictionary.Tables;
      If CurrentEditor.CurrentTable<>Nil then
        TableName:=CurrentEditor.CurrentTable.TableName;
      ShowModal;
    Finally
      Free;
    end;
end;

procedure TMainForm.DeleteCurrentTable;

begin
  if Assigned(CurrentEditor) then
    With CurrentEditor do
      If Assigned(CurrentTable) then
        DeleteTable(CurrentTable);
end;

procedure TMainForm.DeleteCurrentField;

begin
  if Assigned(CurrentEditor) then
    With CurrentEditor do
      If Assigned(CurrentField) then
        DeleteField(CurrentField);
end;

procedure TMainForm.DoNewField;

Var
  TD : TDDTableDef;
  AFieldName : String;

begin
  If Assigned(CurrentEditor) then
    begin
    TD:=CurrentEditor.CurrentTable;
    If Assigned(TD) then
      begin
      If InputQuery(Format(SNewField,[TD.TableName]),SNEwFieldName,AFieldName) then
        If (AFieldName<>'') then
          CurrentEditor.NewField(AFieldName,TD);
      end;
    end;
end;

procedure TMainForm.DoNewTable;

Var
  ATableName : String;

begin
  If Assigned(CurrentEditor) then
    If InputQuery(SNewTable,SNEwTableName,ATableName) then
      If (ATableName<>'') then
        CurrentEditor.NewTable(ATableName);
end;




procedure TMainForm.DoImport(Const EngineName : String);

begin
  DoImport(EngineName,'');
end;

procedure TMainForm.DoImport(Const EngineName, Connectionstring : String);

  Function UseNewDataDict : Boolean;
  
  begin
    Result:=(mrNo=QuestionDLG(SImportDictInto,SWhichCurrentDictToUse,mtInformation,[mrYes,SUseCurrentDict,mrNo,SUseNewDict],0))
  end;

Var
  DDE : TFPDDengine;
  CDE : TDatadictEditor;
  CS  : String;
  L : TStringList;
  B : Boolean;

begin
  DDE:=CreateDictionaryEngine(EngineName,self);
  Try
    CS:=ConnectionString;
    If (CS='') then
      CS:=DDE.GetConnectString;
    If (CS='') then
      exit;
    DDE.Connect(CS);
    try
      L:=TStringlist.Create;
      try
        L.Sorted:=True;
        if GetTableList(DDE,L,B) then
          begin
          CDE:=CurrentEditor;
          If (CDE=Nil) or UseNewDataDict then
            CDE:=NewDataEditor;
          Try
            StartStatus;
            try
              DDE.OnProgress:=@DoDDEProgress;
              If DDE.ImportTables(CDE.DataDictionary.Tables,L,B)>0 then
                CDE.Modified:=True;
            finally
              StopStatus;
            end;
          Finally
            CDE.ShowDictionary;
          end;
          end;
      finally
        L.Free;
      end;
    finally
      DDE.Disconnect;
    end;
  Finally
    DDE.Free
  end;
end;

{ ---------------------------------------------------------------------
  Recent dictionaries tab/handling
  ---------------------------------------------------------------------}

procedure TMainForm.LVDictsDblClick(Sender: TObject);

begin
  If (LVDicts.Selected<>Nil)
     and (LVDicts.Selected.Data<>Nil) then
   OpenDataDict(TRecentDataDict(LVDicts.Selected.Data));
end;

procedure TMainForm.LVDictsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TMainForm.PCDDChange(Sender: TObject);
begin

end;


Function TMainForm.FindLi(LV : TListView;RI : TRecentItem) : TListItem;

Var
  LI : TListItem;
  I : Integer;

begin
  I:=LV.Items.Count-1;
  Result:=Nil;
  While (Result=Nil) and (I>=0) do
    begin
    LI:=LV.Items[i];
    If (LI.Data=Pointer(RI)) then
      Result:=Li;
    Dec(I);
    end;
end;


Procedure TMainForm.AddRecentDict(DF : TRecentDataDict; AssumeNew : Boolean);

Var
  LI : TListItem;

begin
  If AssumeNew then
    LI:=LVDicts.Items.Add
  else
    begin
    LI:=FindLi(LVDicts,DF);
    If (Li=Nil) then
      LI:=LVDicts.Items.Add
    else
      LI.SubItems.Clear
    end;
  LI.Caption:=DF.Name;
  LI.SubItems.Add(DF.FileName);
  LI.SubItems.Add(DateTimeToStr(DF.LastUse));
  LI.Data:=DF;
end;

{ ---------------------------------------------------------------------
  Connection handling
  ---------------------------------------------------------------------}

Procedure TMainForm.AddRecentConnection(RC : TRecentConnection; AssumeNew : Boolean);

Var
  LI : TListItem;

begin
  If AssumeNew then
    LI:=LVConnections.Items.Add
  else
    begin
    LI:=FindLI(LVConnections,RC);
    If (Li=Nil) then
      LI:=LVConnections.Items.Add
    else
      LI.SubItems.Clear
    end;
  LI.Caption:=RC.Name;
  LI.SubItems.Add(RC.EngineName);
  LI.SubItems.Add(DateTimeToStr(RC.LastUse));
  LI.SubItems.Add(RC.ConnectionString);
  LI.Data:=RC;
end;

Function TMainForm.GetConnectionName(Var AName : String) : Boolean;

Var
  OK : Boolean;
  RC : TRecentConnection;
  
begin
  Result:=False;
  RC:=Nil;
  Repeat
    AName:='';
    InputQuery(SNewConnection,SConnectionDescription,AName);
    RC:=FRecentConnections.FindFromName(AName);
    If (RC<>Nil) then
      case MessageDlg(Format(SConnectionNameExists,[AName]),mtInformation,[mbYes,mbNo,mbCancel],0) of
        mrYes : OK:=true;
        mrNo :  OK:=False;
        mrCancel : Exit;
      else
        OK:=False;
      end
    else
      OK:=True;
  Until OK;
  If (RC=Nil) then
    RC:=FRecentConnections.AddConnection(AName);
  Result:=True;
end;

procedure TMainForm.OpenConnection(RC : TRecentConnection);

Var
  DDE : TFPDDengine;
  CDE : TConnectionEditor;

begin
  RC.Use;
  DDE:=CreateDictionaryEngine(RC.EngineName,Self);
  CDE:=NewConnectionEditor(RC.Name);
  CDE.Engine:=DDE;
  CDE.Connect(RC.ConnectionString);
end;

procedure TMainForm.NewConnection(EngineName : String);


Var
  DDE : TFPDDengine;
  CDE : TConnectionEditor;
  CS,AName  : String;
  RC : TRecentConnection;

begin
  DDE:=CreateDictionaryEngine(EngineName,Self);
  CS:=DDE.GetConnectString;
  If (CS='') then
    exit;
  If not GetConnectionName(AName) then
    Exit;
  RC:=FRecentConnections.FindFromName(AName);
  RC.ConnectionString:=CS;
  RC.EngineName:=EngineName;
  RC.Use;
  CDE:=NewConnectionEditor(Aname);
  CDE.Engine:=DDE;
  CDE.Connect(CS);
end;

Function TMainForm.NewConnectionEditor(AName : String) : TConnectionEditor;

begin
  Result:=TConnectioneditor.Create(Self);
  Result.PageControl:=PCDD;
  Result.Parent:=PCDD;
  Result.Description:=AName;
  PCDD.ActivePage:=Result;
end;

procedure TMainForm.NewConnection;

Var
  ET : String;

begin
  If SelectEngineType(ET) then
    NewConnection(ET);
end;

Function TMainForm.SelectEngineType(Var EngineName : String) : Boolean;

begin
  EngineName:='SQLDB';
end;


initialization
  {$I frmmain.lrs}

end.

