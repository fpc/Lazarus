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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}


unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdatadict,
  // LCL
  Forms, Controls, Dialogs, Menus, ActnList, StdActns, ComCtrls, ExtCtrls,
  IniPropStorage, LCLType, LCLProc,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, Translations,
  // LazDataDesktop
  dicteditor, fraconnection, ddfiles, lazdatadeskstr;

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
    ACreateCode: TAction;
    AAddSequence: TAction;
    AAddForeignKey: TAction;
    AAddDomain: TAction;
    ADeleteRecentDataDict: TAction;
    AOpenRecentDatadict: TAction;
    AOpenConnection: TAction;
    ANewIndex: TAction;
    ADeleteConnection: TAction;
    ANewConnection: TAction;
    ASaveAs: TAction;
    AGenerateSQL: TAction;
    ADeleteObject: TAction;
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
    MenuItem10: TMenuItem;
    PMIDeleteDataDictA: TMenuItem;
    PMINewDataDictA: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PMIOpenDataDictA: TMenuItem;
    MIListView: TMenuItem;
    MView: TMenuItem;
    MICreateCode: TMenuItem;
    PCItems: TPageControl;
    PMIDeleteConnectionA: TMenuItem;
    PMINewConnection: TMenuItem;
    PMINewConnectionA: TMenuItem;
    PMINewDataDict: TMenuItem;
    PMIOpenConnectionA: TMenuItem;
    PMIOpenDataDict: TMenuItem;
    MIDeleteRecentConnection: TMenuItem;
    MIOpenRecentConnection: TMenuItem;
    PMIDeleteConnection: TMenuItem;
    PMIOpenConnection: TMenuItem;
    MINewConnection: TMenuItem;
    MIConnection: TMenuItem;
    MISaveAs: TMenuItem;
    MIGenerateSQL: TMenuItem;
    MIDDSep3: TMenuItem;
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
    PMRecentConnections: TPopupMenu;
    PMDataDict: TPopupMenu;
    PMAll: TPopupMenu;
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
    PCRecent: TPageControl;
    SDDD: TSaveDialog;
    SRecent: TSplitter;
    TSAll: TTabSheet;
    TBAddIndex: TToolButton;
    TBCreateCode: TToolButton;
    TBAddSequence: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    TVAll: TTreeView;
    TSConnections: TTabSheet;
    ToolButton1: TToolButton;
    TBNewTable: TToolButton;
    TBNewField: TToolButton;
    ToolButton2: TToolButton;
    TBDeleteTable: TToolButton;
    ToolButton3: TToolButton;
    TBGenerateSQL: TToolButton;
    TSRecent: TTabSheet;
    TBMain: TToolBar;
    TBSave: TToolButton;
    TBOPen: TToolButton;
    TBNew: TToolButton;
    procedure AAddDomainExecute(Sender: TObject);
    procedure AAddDomainUpdate(Sender: TObject);
    procedure AAddForeignKeyExecute(Sender: TObject);
    procedure AAddForeignKeyUpdate(Sender: TObject);
    procedure AAddSequenceExecute(Sender: TObject);
    procedure AAddSequenceUpdate(Sender: TObject);
    procedure ACloseAllExecute(Sender: TObject);
    procedure ACloseExecute(Sender: TObject);
    procedure ACreateCodeExecute(Sender: TObject);
    procedure ACreateCodeUpdate(Sender: TObject);
    procedure ADeleteConnectionExecute(Sender: TObject);
    procedure ADeleteObjectExecute(Sender: TObject);
    procedure ADeleteObjectUpdate(Sender: TObject);
    procedure ADeleteRecentDataDictExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AGenerateSQLExecute(Sender: TObject);
    procedure AllowSQL(Sender: TObject);
    procedure ANewConnectionExecute(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ANewFieldExecute(Sender: TObject);
    procedure ANewFieldUpdate(Sender: TObject);
    procedure ANewIndexExecute(Sender: TObject);
    procedure ANewIndexUpdate(Sender: TObject);
    procedure ANewTableExecute(Sender: TObject);
    procedure HaveDataDict(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HaveConnection(Sender: TObject);
    procedure HaveDDEditor(Sender: TObject);
    procedure HaveRecentConnection(Sender: TObject);
    procedure HaveRecentDataDict(Sender: TObject);
    procedure HaveTabs(Sender: TObject);
    procedure HaveTab(Sender: TObject);
    procedure HaveTables(Sender: TObject);
    procedure MIListViewClick(Sender: TObject);
    procedure OpenRecentConnection(Sender: TObject);
    procedure LVConnectionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OpenRecentDatadict(Sender: TObject);
    procedure LVDictsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MIDataDictClick(Sender: TObject);
    procedure DoCloseTabClick(Sender: TObject);
    procedure PMAllPopup(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure TVAllDblClick(Sender: TObject);
  private
    FTreeIntf : Boolean;
    FNRecentConnections : TTreeNode;
    FNRecentDictionaries : TTreeNode;
    FRecentDicts : TRecentDataDicts;
    FRecentConnections : TRecentConnections;
    PCDD : TPageControl;
    procedure AddRecentConnectionList(RC: TRecentConnection; AssumeNew: Boolean);
    procedure AddRecentConnectionTree(RC: TRecentConnection; AssumeNew: Boolean);
    procedure CheckParams;
    function CloseConnectionEditor(aTab: TConnectionEditor): TModalResult;
    function CloseCurrentConnection: TModalResult;
    function CloseCurrentTab(AddCancelClose: Boolean = False): TModalResult;
    function CloseDataEditor(DD: TDataDictEditor; AddCancelClose: Boolean): TModalResult;
    function CloseTab(aTab : TTabsheet; AddCancelClose: Boolean = False): TModalResult;
    procedure DeleteRecentConnection;
    procedure DeleteRecentDataDict;
    procedure DoShowNewConnectionTypes(ParentMenu: TMenuItem);
    procedure DoTestConnection(Sender: TObject; const ADriver: String; Params: TStrings);
    function GetConnectionName(out AName: String): Boolean;
    function GetCurrentConnection: TConnectionEditor;
    function GetCurrentEditor: TDataDictEditor;
    function GetSelectedRecentConnection: TRecentConnection;
    function GetSelectedRecentDataDict: TRecentDataDict;
    procedure NewConnection(EngineName : String);
    procedure NewConnection;
    procedure OpenConnection(RC: TRecentConnection);
    procedure RegisterDDEngines;
    Function SelectEngineType(out EngineName : String) : Boolean;
    procedure SetupIntf;
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
    Procedure AddRecentDictList(DF : TRecentDataDict; AssumeNew : Boolean);
    Procedure AddRecentDictTree(DF : TRecentDataDict; AssumeNew : Boolean);
    Function  FindLi(LV : TListView;RI : TRecentItem) : TListItem;
    function FindTN(AParent: TTreeNode; RI: TRecentItem): TTreeNode;
    procedure ImportClick(Sender : TObject);
    procedure RecentImportClick(Sender : TObject);
    procedure NewConnectionClick(Sender : TObject);
    procedure DoImport(Const EngineName : String);
    procedure DoImport(Const EngineName,ConnectionString : String);
    Procedure DoDDEProgress(Sender : TObject; Const Msg : String);
  public
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
    procedure DeleteCurrentObject;
    procedure DoNewGlobalObject(AObjectType: TEditObjectType);
    procedure DoNewTableObject(AObjectType: TEditObjectType);
    procedure ShowGenerateSQL;
    Property CurrentEditor : TDataDictEditor Read GetCurrentEditor;
    Property CurrentConnection : TConnectionEditor Read GetCurrentConnection;
    Property SelectedRecentConnection : TRecentConnection Read GetSelectedRecentConnection;
    Property SelectedRecentDataDict : TRecentDataDict Read GetSelectedRecentDataDict;
  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ $DEFINE HAVEMSSQLCONN}

uses
  frmselectconnectiontype, reglddfeatures,
  frmimportdd,frmgeneratesql,fpddsqldb,frmSQLConnect;

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
  //
  Caption := sld_Lazarusdatabasedesktop;
  //
  MFile.Caption:= sld_Menufile;
  MenuItem2.Caption:= sld_Menuedit;
  MIDataDict.Caption:= sld_Menudictionary;
  MIConnection.Caption:= sld_Menuconnections;
  MView.Caption:= sld_View;
  MIImport.Caption:= sld_Menudictionaryimport;
  MICloseSep.Caption:= sld_Separator;
  MISep.Caption:= sld_Separator;
  MIDDSep.Caption:= sld_Separator;
  MIDDSep2.Caption:= sld_Separator;
  MIDDSep3.Caption:= sld_Separator;
  MINewConnection.Caption:= sld_Actionnewconnection;
  PMINewConnection.Caption:= sld_Actionnewconnection;
  //
  ASave.Caption:= sld_Actionsave;
  ASave.Hint:= sld_ActionsaveH;
  ANew.Caption:= sld_Actionnew;
  ANew.Hint:= sld_ActionnewH;
  AExit.Caption:= sld_Actionexit;
  AExit.Hint:= sld_ActionexitH;
  AOpen.Caption:= sld_Actionopen;
  AOpen.Hint:= sld_ActionopenH;
  AClose.Caption:= sld_Actionclose;
  AClose.Hint:= sld_ActioncloseH;
  ACloseAll.Caption:= sld_Actioncloseall;
  ACloseAll.Hint:= sld_ActioncloseallH;
  ASaveAs.Caption:= sld_Actionsaveas;
  ASaveAs.Hint:= sld_ActionsaveasH;
  AOpenRecentDatadict.Caption:= sld_Actionopenrecentdatadict;
  AOpenRecentDatadict.Hint:= sld_ActionopenrecentdatadictH;
  ADeleteRecentDataDict.Caption:= sld_Actiondeleterecentdatadict;
  //
  ACut.Caption:= sld_Actioncut;
  ACut.Hint:= sld_ActioncutH;
  ACopy.Caption:= sld_Actioncopy;
  ACopy.Hint:= sld_ActioncopyH;
  APaste.Caption:= sld_Actionpaste;
  APaste.Hint:= sld_ActionpasteH;
  //
  ANewTable.Caption:= sld_Actionnewtable;
  ANewTable.Hint:= sld_ActionnewtableH;
  ANewField.Caption:= sld_Actionnewfield;
  ANewField.Hint:= sld_ActionnewfieldH;
  ADeleteObject.Caption:= sld_Actiondeleteobject;
  ADeleteObject.Hint:= sld_ActiondeleteobjectH;
  AGenerateSQL.Caption:= sld_Actiongeneratesql;
  AGenerateSQL.Hint:= sld_ActiongeneratesqlH;
  ANewIndex.Caption:= sld_Actionnewindex;
  ANewIndex.Hint:= sld_ActionnewindexH;
  ACreateCode.Caption:= sld_Actioncreatecode;
  ACreateCode.Hint:= sld_ActioncreatecodeH;
  AAddForeignKey.Caption:= sld_Actionaddforeignkey;
  AAddForeignKey.Hint:= sld_ActionaddforeignkeyH;
  AAddDomain.Caption:= sld_Actionadddomain;
  AAddDomain.Hint:= sld_ActionadddomainH;
  AAddSequence.Caption:= sld_Actionaddsequence;
  AAddSequence.Hint:= sld_ActionaddsequenceH;
  //
  ANewConnection.Caption:= sld_Actionnewconnection;
  ADeleteConnection.Caption:= sld_Actiondeleteconnection;
  ACopyConnection.Caption:= sld_Actioncopyconnection;
  AOpenConnection.Caption:= sld_Actionopenconnection;
  AOpenConnection.Hint:= sld_ActionopenconnectionH;
  //
  //
  TSRecent.Caption:= sld_Dictionaries;
  TSConnections.Caption:= sld_Connections;
  TSAll.Caption:= sld_ConnectionsDictionaries;
  LVDicts.Column[0].Caption:= sld_Recentlv1;
  LVDicts.Column[1].Caption:= sld_Recentlv2;
  LVDicts.Column[2].Caption:= sld_Recentlv3;
  LVConnections.Column[0].Caption:= sld_Connectionlv1;
  LVConnections.Column[1].Caption:= sld_Connectionlv2;
  LVConnections.Column[2].Caption:= sld_Connectionlv3;
  LVConnections.Column[3].Caption:= sld_Connectionlv4;
  //
  ODDD.Title:= sld_opendatadictionarytitle;
  ODDD.Filter:= sld_opendatadictionaryfilter;
  SDDD.Title:= sld_savefileastitle;
  SDDD.Filter:= sld_savefileasfilter;
  //
  MIListView.Caption:=sld_LegacyView;
  //
  //
  // Register DD engines.
  RegisterDDEngines;
  RegisterExportFormats;

  // Register standard export formats.
  FRecentDicts:=TRecentDataDicts.Create(TRecentDataDict);
  FRecentConnections:=TRecentConnections.Create(TRecentConnection);
  FN:=SysToUTF8(GetAppConfigDir(False));
  ForceDirectoriesUTF8(FN);
  FN:=SysToUTF8(GetAppConfigFile(False));
  PSMain.IniFileName:=ChangeFileExt(FN,'.ini');
  FTreeIntf:=PSMain.ReadBoolean('TreeInterface',True);
  if FTreeIntf then
    PCRecent.Width:=PSMain.ReadInteger('TreeWidth',PCRecent.Width);
// We need these 2 in all cases
  FNRecentConnections:=TVAll.Items.AddChild(Nil,sld_Connections);
  FNRecentConnections.ImageIndex:=16;
  FNRecentDictionaries:=TVAll.Items.AddChild(Nil,sld_Dictionaries);
  FNRecentDictionaries.ImageIndex:=19;
  SetupIntf;
  FRecentDicts.LoadFromFile(UTF8ToSys(FN),'RecentDicts');
  FRecentConnections.LoadFromFile(UTF8ToSys(FN),'RecentConnections');
  ShowRecentDictionaries;
  ShowRecentConnections;
  ShowDDImports;
  ShowNewConnectionTypes;
  LVDicts.Columns[0].Width:=120;
  LVDicts.Columns[1].Width:=380;
  LVDicts.Columns[2].Width:=150;
  LVConnections.Column[0].Width:=120;
  LVConnections.Column[1].Width:=120;
  LVConnections.Column[2].Width:=150;
  LVConnections.Column[3].Width:=260;
  RegisterConnectionCallBacks;
end;

procedure TMainForm.SetupIntf;

begin
  TSAll.TabVisible:=FTreeIntf;
  TSRecent.TabVisible:=Not FTreeIntf;
  TSConnections.TabVisible:=Not FTreeIntf;
  if FTreeIntf then
    begin
    PCDD:=PCItems;
    PCRecent.Align:=alLeft;
    //PCRecent.Width:=300;
    SRecent.Align:=alLeft;
    SRecent.Visible:=True;
    PCItems.Visible:=True;
    PCItems.Align:=alClient;
    end
  else
    begin
    PCItems.Align:=alRight;
    PCItems.Visible:=False;
    SRecent.Align:=alRight;
    SRecent.Visible:=False;
    PCRecent.Align:=alClient;
    PCDD:=PCRecent;
    end;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRecentConnections);
  FreeAndNil(FRecentDicts);
  PSMain.WriteBoolean('TreeInterface',FTreeIntf);
  if FTreeIntf then
    PSMain.WriteInteger('TreeWidth',PCRecent.Width);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CheckParams;
  if (FRecentConnections.Count=0) and (FRecentDicts.Count=0) then
    case QuestionDlg(sld_FirstStart,sql_NoConnectionsFound,mtInformation,[
         mrOK,sld_startnewdict,
         mrYes,sld_startnewconnection,
         mrCancel,sld_startempty
       ],0) of
       mrYes : NewConnection;
       mrOK  : NewDataDict;
    end
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
    If FileExistsUTF8(S) then
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
  RegisterEngines;
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
    MaybeRegisterConnectionStringCallback('TSQLDBMySql51DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql55DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql56DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMySql57DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBODBCDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBPOSTGRESQLDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBFBDDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBSQLite3DDEngine',@GetSQLConnectionDlg);
    MaybeRegisterConnectionStringCallback('TSQLDBMSSQLDDEngine',@GetSQLConnectionDlg);

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

function TMainForm.GetSelectedRecentConnection: TRecentConnection;

Var
  TN : TTreeNode;
  LI : TListItem;

begin
  Result:=Nil;
  if FTreeIntf then
     begin
     TN:=TVAll.Selected;
     if Assigned(TN) AND Assigned(TN.Data) AND TObject(TN.Data).InheritsFrom(TRecentConnection) then
       Result:=TRecentConnection(TN.Data);
     end
  else
    begin
    LI:=LVConnections.Selected;
    if (LI<>Nil) and (LI.Data<>Nil) then
      Result:=TRecentConnection(LI.Data)
    end;
end;

function TMainForm.GetSelectedRecentDataDict: TRecentDataDict;
Var
  TN : TTreeNode;
  LI : TListItem;

begin
  Result:=Nil;
  if FTreeIntf then
     begin
     TN:=TVAll.Selected;
     if Assigned(TN) AND Assigned(TN.Data) AND TObject(TN.Data).InheritsFrom(TRecentDataDict) then
       Result:=TRecentDataDict(TN.Data);
     end
  else
    begin
    LI:=LVDicts.Selected;
    if (LI<>Nil) AND (LI.Data<>Nil) then
      Result:=TRecentDataDict(LI.Data);
    end;
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
    AddRecentDictList(DF,True);
    AddRecentDictTree(DF,True);
    end;
  FNRecentDictionaries.Expand(False);
end;

procedure TMainForm.ShowRecentConnections;

Var
  RC : TRecentConnection;
  I : Integer;

begin
  LVConnections.Items.Clear;
  FNRecentConnections.DeleteChildren;
  For I:=0 to FRecentConnections.Count-1 do
    begin
    RC:=FRecentConnections[i];
    AddRecentConnectionList(RC,True);
    AddRecentConnectionTree(RC,True);
    end;
  FNRecentConnections.Expand(False);
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
    MR.Caption:=sld_Fromconnection;
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

begin
  DoShowNewConnectionTypes(MINewConnection);
  DoShowNewConnectionTypes(PMINewConnection);
end;

procedure TMainForm.DoShowNewConnectionTypes(ParentMenu : TMenuItem);
Var
  MI : TNewConnectionMenuItem;
  L : TStringList;
  dd,dt : string;
  i : integer;
  cap : TFPDDEngineCapabilities;

begin
  ParentMenu.Clear;
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
      ParentMenu.Add(MI);
      // make sure it gets loaded
      PSMain.StoredValue[L[i]]:='';
      end;
  Finally
    L.Free;
  end;
  MIImport.Enabled:=(MIImport.Count>0);
end;

procedure TMainForm.DoTestConnection(Sender: TObject;Const ADriver : String; Params: TStrings);

Var
  DDE : TFPDDEngine;


begin
  DDE:=CreateDictionaryEngine(ADriver,Self);
  try
    DDE.Connect(Params.CommaText);
  finally
    DDE.Free;
  end;
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

procedure TMainForm.ShowStatus(const Msg: String);

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

procedure TMainForm.DoDDEProgress(Sender: TObject; const Msg: String);

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


procedure TMainForm.GetSQLConnectionDlg(Sender: TObject; var AConnection: String
  );

Var
  Last : String;
  
begin
  Last:=PSmain.StoredValue[Sender.ClassName];
  With (Sender as TSQLDBDDEngine) do
    AConnection:=GetSQLDBConnectString(HostSupported,Last,ClassName,@DoTestConnection);
  If (AConnection<>'') then
    PSmain.StoredValue[Sender.ClassName]:=AConnection;
end;


procedure TMainForm.GetDBFDir(Sender: TObject; var ADir: String);

Var
  IDir : String;

begin
  IDir:=PSmain.StoredValue[Sender.ClassName];
  if (IDir='') then
    IDir:=ExtractFilePath(ParamStrUTF8(0));
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

procedure TMainForm.AllowSQL(Sender: TObject);

Var
  B : Boolean;

begin
  B:=Assigned(CurrentEditor) and (CurrentEditor.DataDictionary.Tables.Count>0);
  If not B then
    B:=Assigned(CurrentConnection) and CurrentConnection.Frame.CanCreateSQL;
  (Sender as TAction).Enabled:=B;
end;

procedure TMainForm.ANewConnectionExecute(Sender: TObject);
begin
  NewConnection;
end;


procedure TMainForm.ACloseExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

procedure TMainForm.ACreateCodeExecute(Sender: TObject);
begin
  If Assigned(CurrentEditor) then
    begin
    If Assigned(CurrentEditor.CurrentTable) then
      CurrentEditor.CreateCode
    end
  else if Assigned(CurrentConnection) then
    begin
    CurrentConnection.Frame.CreateCode;
    end;
end;


procedure TMainForm.ACreateCodeUpdate(Sender: TObject);

Var
  B : Boolean;

begin
  B:=Assigned(CurrentEditor);
  If B then
    B:=Assigned(CurrentEditor.CurrentTable)
  else
    begin
    B:=Assigned(CurrentConnection);
    If B then
      B:=CurrentConnection.Frame.CanCreateCode;
    end;
  (Sender as TAction).Enabled:=B;
end;

procedure TMainForm.ADeleteConnectionExecute(Sender: TObject);
begin
  DeleteRecentConnection;
end;

procedure TMainForm.DeleteRecentConnection;

Var
  R : TRecentConnection;

begin
  R:=SelectedRecentConnection;
  If Assigned(R) then
    begin
    FRecentConnections.Delete(R.Index);
    FindLI(LVConnections,R).Free;
    FindTN(FNRecentConnections,R).Free;
    end;
end;

procedure TMainForm.ADeleteObjectExecute(Sender: TObject);
begin
  DeleteCurrentObject;
end;

procedure TMainForm.ADeleteObjectUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentEditor) and
                               Assigned(CurrentEditor.CurrentObject);
end;

procedure TMainForm.ADeleteRecentDataDictExecute(Sender: TObject);
begin
  DeleteRecentDataDict;
end;

procedure TMainForm.ACloseAllExecute(Sender: TObject);
begin
  CloseAllEditors;
end;

procedure TMainForm.AAddSequenceUpdate(Sender: TObject);
begin
{$ifdef onlyoldobjects}
  (Sender as TAction).Enabled:=False;
{$else}
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil);
{$endif}
end;

procedure TMainForm.AAddDomainUpdate(Sender: TObject);
begin
{$ifdef onlyoldobjects}
  (Sender as TAction).Enabled:=False;
{$else}
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil);
{$endif}
end;

procedure TMainForm.AAddForeignKeyUpdate(Sender: TObject);
begin
{$ifdef onlyoldobjects}
  (Sender as TAction).Enabled:=False
{$else}
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil) and (CurrentEditor.CurrentTable<>Nil);
{$endif}
end;

procedure TMainForm.AAddDomainExecute(Sender: TObject);
begin
  DoNewGlobalObject(eotDomain);
end;

procedure TMainForm.AAddForeignKeyExecute(Sender: TObject);
begin
  DoNewTableObject(eotForeignKey);
end;

procedure TMainForm.AAddSequenceExecute(Sender: TObject);
begin
  DoNewGlobalObject(eotSequence)
end;

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  NewDataDict;
end;

procedure TMainForm.ANewFieldExecute(Sender: TObject);
begin
  DoNewTableObject(eotField);
end;

procedure TMainForm.ANewFieldUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>nil)
                                and (CurrentEditor.CurrentTable<>Nil);
end;

procedure TMainForm.ANewIndexExecute(Sender: TObject);
begin
  DoNewTableObject(eotIndex);
end;

procedure TMainForm.ANewIndexUpdate(Sender: TObject);
begin
{$ifdef onlyoldobjects}
  (Sender as TAction).Enabled:=False
{$else}
  (Sender as TAction).Enabled:=(CurrentEditor<>nil)
                                and (CurrentEditor.CurrentTable<>Nil);
{$endif onlyoldobjects}
end;

procedure TMainForm.ANewTableExecute(Sender: TObject);
begin
  DoNewGlobalObject(eotTable)
end;

procedure TMainForm.HaveDataDict(Sender: TObject);
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

procedure TMainForm.HaveRecentConnection(Sender: TObject);

begin
  (Sender as Taction).Enabled:=Assigned(SelectedRecentConnection);
end;

procedure TMainForm.HaveRecentDataDict(Sender: TObject);

begin
  (Sender as Taction).Enabled:=Assigned(SelectedRecentDataDict);
end;

procedure TMainForm.HaveTabs(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil) or (CurrentConnection<>Nil);
end;

procedure TMainForm.HaveTab(Sender: TObject);
begin
  if FTreeIntf then
    (Sender as TAction).Enabled:=(PCDD.PageCount>0)
  else
    (Sender as TAction).Enabled:=(PCDD.PageCount>2);
end;

procedure TMainForm.HaveTables(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentEditor)
                               and (CurrentEditor.DataDictionary.Tables.Count>0);
end;

procedure TMainForm.MIListViewClick(Sender: TObject);
begin
  FTreeIntf:=Not MIListView.Checked;
  SetupIntf;
end;

procedure TMainForm.OpenRecentConnection(Sender: TObject);
begin
  If (LVConnections.Selected<>Nil)
     and (LVConnections.Selected.Data<>Nil) then
   OpenConnection(TRecentConnection(LVConnections.Selected.Data));
end;

procedure TMainForm.LVConnectionsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_DELETE then
    DeleteRecentConnection;
end;

procedure TMainForm.SaveAsExecute(Sender: TObject);
begin
  SaveCurrentEditorAs;
end;

procedure TMainForm.TVAllDblClick(Sender: TObject);

Var
  TN : TTreeNode;

begin
  TN:=TVAll.Selected;
  if Assigned(TN) and Assigned(Tn) then
    if TObject(TN.Data).InheritsFrom(TRecentConnection) then
      OpenConnection(TRecentConnection(TN.Data))
    else if TObject(TN.Data).InheritsFrom(TRecentDatadict) then
      OpenDataDict(TRecentDatadict(TN.Data))
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
  AddRecentDictList(DDF,B);
  AddRecentDictTree(DDF,B);
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
        AddRecentDictList(DDF,True);
        AddRecentDictTree(DDF,True);
        end;
      end;
end;

function TMainForm.CloseAllEditors: Boolean;

begin
  Result:=True;
  PCDD.ActivePageIndex:=PCDD.PageCount-1;
  While Result and ((CurrentEditor<>Nil) or (CurrentConnection<>Nil)) do
    Result:=CloseCurrentTab(True)<>mrCancel;
end;


function TMainForm.CloseCurrentTab(AddCancelClose: Boolean): TModalResult;

begin
  Result:=CloseTab(PCItems.ActivePage,AddCancelClose);
end;

function TMainForm.CloseTab(aTab : TTabsheet; AddCancelClose: Boolean = False): TModalResult;

begin
  If (aTab is TDataDictEditor) then
    Result:=CloseDataEditor(aTab as TDataDictEditor,AddCancelClose)
  else if (aTab is TConnectionEditor) then
    Result:=CloseConnectionEditor(aTab as TConnectionEditor)
  else
    Result:=mrOK;
end;

function TMainForm.CloseConnectionEditor(aTab : TConnectionEditor): TModalResult;

begin
  aTab.Frame.DisConnect;
  Application.ReleaseComponent(aTab);
  aTab.Free;
  Result:=mrOK;
end;

function TMainForm.CloseCurrentConnection: TModalResult;

begin
  if CurrentConnection<>Nil then
    Result:=CloseConnectionEditor(CurrentConnection)
  else
    Result:=mrOK;
end;


function TMainForm.CloseCurrentEditor(AddCancelClose: Boolean): TModalResult;

begin
  Result:=CloseDataEditor(CurrentEditor,AddCancelClose);
end;

function TMainForm.CloseDataEditor(DD : TDataDictEditor;AddCancelClose: Boolean): TModalResult;

Var
  Msg : String;
  DDF : TRecentDataDict;

begin
  Result:=mrYes;
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

function TMainForm.NewDataDict: TFPDataDictionary;

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
  Result.ImageIndex:=15;
  PCDD.ActivePage:=Result;
  Result.DataDictionary.OnProgress:=@DoDDEprogress;
end;

{ ---------------------------------------------------------------------
  Data dictionary commands
  ---------------------------------------------------------------------}


procedure TMainForm.ShowGenerateSQL;

Var
  TN : String;

begin
  if Assigned(CurrentConnection) then
    CurrentConnection.Frame.CreateSQL
  else
    begin
    If CurrentEditor.CurrentTable<>Nil then
      TN:=CurrentEditor.CurrentTable.TableName;
    TGenerateSQLForm.GenerateSQLDialog(CurrentEditor.DataDictionary.Tables,TN,True);
    end
end;

procedure TMainForm.DeleteCurrentObject;

Var
  DD : TDataDictEditor;

begin
  DD:=CurrentEditor;
  If Assigned(DD) then
    DD.DeleteCurrentObject;
end;


procedure TMainForm.DoNewGlobalObject(AObjectType : TEditObjectType);

Var
  ACaption,ALabel, AObjectName : String;

begin
  AObjectName:='';
  Case AObjectType of
    eotTable :
      begin
      ACaption:=SNewTable;
      ALabel:=SNewTableName
      end;
    eotSequence:
      begin
      ACaption:=SNewSequence;
      ALabel:=SNewSequenceName
      end;
    eotDomain :
      begin
      ACaption:=SNewDomain;
      ALabel:=SNewDomainName
      end
  end;
  If InputQuery(ACaption,ALabel,AObjectName) then
    If (AObjectName<>'') then
      CurrentEditor.NewGlobalObject(AObjectName,AObjectType);
end;

procedure TMainForm.DoNewTableObject(AObjectType : TEditObjectType);

Var
  ACaption,ALabel, AObjectName : String;
  TD : TDDTableDef;

begin
  TD:=CurrentEditor.CurrentTable;
  If (TD=Nil) then
    Exit;
  AObjectName:='';
  Case AObjectType of
    eotField :
      begin
      ACaption:=SNewField;
      ALabel:=SNewFieldName
      end;
    eotIndex:
      begin
      ACaption:=SNewIndex;
      ALabel:=SNewIndexName
      end;
    eotForeignKey :
      begin
      ACaption:=SNewForeignKey;
      ALabel:=SNewForeignKeyName
      end
  end;
  If InputQuery(Format(ACaption,[TD.TableName]),ALabel,AObjectName) then
    If (AObjectName<>'') then
      CurrentEditor.NewTableObject(AObjectName,TD,AObjectType);
end;

procedure TMainForm.DoImport(const EngineName: String);

begin
  DoImport(EngineName,'');
end;

procedure TMainForm.DoImport(const EngineName, ConnectionString: String);

  Function UseNewDataDict : Boolean;
  
  begin
    Result:=(mrNo=QuestionDLG(SImportDictInto,SWhichCurrentDictToUse,mtInformation,[mrYes,SUseCurrentDict,mrNo,SUseNewDict],0))
  end;

Var
  DDE : TFPDDengine;
  CDE : TDatadictEditor;
  CS,CN  : String;
  L : TStringList;
  B : Boolean;

begin
  DDE:=CreateDictionaryEngine(EngineName,self);
  Try
    CS:=ConnectionString;
    If (CS='') then
      begin
      CS:=DDE.GetConnectString;
      If (CS<>'') then
        if MessageDLg(SCreateConnection,mtConfirmation,[mbYes,mbNo],0)=mrYes then
          GetConnectionName(CN);
      end;
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

procedure TMainForm.OpenRecentDatadict(Sender: TObject);

begin
  If (LVDicts.Selected<>Nil)
     and (LVDicts.Selected.Data<>Nil) then
   OpenDataDict(TRecentDataDict(LVDicts.Selected.Data));
end;

procedure TMainForm.LVDictsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_DELETE then
    DeleteRecentDataDict;
end;

procedure TMainForm.DeleteRecentDataDict;

Var
  D: TRecentDatadict;

begin
  D:=SelectedRecentDataDict;
  If Assigned(D) then
   begin
   FRecentDicts.Delete(D.Index);
   FindLI(LVDicts,D).Free;
   FindTN(FNRecentDictionaries,D).Free;
   end;
end;

procedure TMainForm.MIDataDictClick(Sender: TObject);
begin
  MIDataDict.Items[0].Clear;
  ShowImportRecentconnections;
  ShowDDImports;
end;

procedure TMainForm.DoCloseTabClick(Sender: TObject);

begin
  CloseTab(Sender as TTabSheet,True);
end;

procedure TMainForm.PMAllPopup(Sender: TObject);

Type
  TOption = (oConnection,oDataDict);
  TOptions = Set of TOption;

Var
  TN : TTreeNode;
  S : TOptions;

begin
  TN:=TVAll.Selected;
  S:=[];
  if Assigned(TN) and Assigned(TN.Data) then
    if TObject(TN.Data).InheritsFrom(TRecentConnection) then
      S:=[oConnection]
    else if TObject(TN.Data).InheritsFrom(TRecentDatadict) then
      S:=[oDataDict];
   PMINewConnectionA.Visible:=True;
   PMIOpenConnectionA.Visible:=oConnection in S;
   PMIDeleteConnectionA.Visible:=oConnection in S;
   PMINewDataDictA.Visible:=True;
   PMIOpenDataDictA.Visible:=oDataDict in S;
   PMIDeleteDataDictA.Visible:=oDataDict in S;
end;


function TMainForm.FindLi(LV: TListView; RI: TRecentItem): TListItem;

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

function TMainForm.FindTN(AParent: TTreeNode; RI: TRecentItem): TTreeNode;


begin
  Result:=APArent.GetFirstChild;
  While (Result<>Nil) and (Result.Data<>Pointer(RI)) do
    Result:=Result.GetNextSibling;
end;


procedure TMainForm.AddRecentDictList(DF: TRecentDataDict; AssumeNew: Boolean);

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

procedure TMainForm.AddRecentDictTree(DF: TRecentDataDict; AssumeNew: Boolean);

Var
  TN : TTreeNode;

begin
  TN:=Nil;
  If Not AssumeNew then
    TN:=FindTN(FNRecentDictionaries,DF);
  If (TN=Nil) then
    TN:=TVAll.Items.AddChild(FNRecentDictionaries,DF.Name);
  TN.DeleteChildren;
  TVAll.Items.AddChild(TN,sld_Recentlv2+': '+DF.Filename);
  TVAll.Items.AddChild(TN,sld_Recentlv3+': '+DateTimeToStr(DF.LastUse));
  TN.Data:=DF;
  TN.ImageIndex:=15;
  TN.SelectedIndex:=15;
end;


{ ---------------------------------------------------------------------
  Connection handling
  ---------------------------------------------------------------------}

procedure TMainForm.AddRecentConnectionList(RC: TRecentConnection;
  AssumeNew: Boolean);

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

procedure TMainForm.AddRecentConnectionTree(RC: TRecentConnection; AssumeNew: Boolean);

Var
  TN : TTreeNode;

begin
  If Not AssumeNew then
    TN:=FindTN(FNRecentConnections,RC)
  else
    TN:=Nil;
  if TN=Nil then
    TN:=TVAll.Items.AddChild(FNRecentConnections,RC.Name);
  TN.DeleteChildren;
  TVAll.Items.AddChild(TN,sld_Connectionlv2+': '+RC.EngineName);
  TVAll.Items.AddChild(TN,sld_Connectionlv3+': '+DateTimeToStr(RC.LastUse));
  TVAll.Items.AddChild(TN,sld_Connectionlv2+': '+RC.ConnectionString);
  TN.Data:=RC;
  TN.ImageIndex:=18;
  TN.SelectedIndex:=18;
end;

function TMainForm.GetConnectionName(out AName: String): Boolean;

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
  CDE.Frame.Engine:=DDE;
  CDE.Frame.Connect(RC.ConnectionString);
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
  CDE.Frame.Engine:=DDE;
  CDE.Frame.Connect(CS);
  ShowRecentConnections;
end;

function TMainForm.NewConnectionEditor(AName: String): TConnectionEditor;

begin
  Result:=TConnectioneditor.Create(Self);
  Result.PageControl:=PCDD;
  Result.Parent:=PCDD;
  Result.Frame.Description:=AName;
  Result.ImageIndex:=18;
  Result.Caption:=aName;
  PCDD.ActivePage:=Result;
end;

procedure TMainForm.NewConnection;

Var
  ET : String;

begin
  If SelectEngineType(ET) then
    NewConnection(ET);
end;

function TMainForm.SelectEngineType(out EngineName: String): Boolean;

begin
  With TSelectConnectionTypeForm.Create(Self) do
    try
      Result:=(ShowModal=mrOK);
      If Result then
        EngineName:=SelectedConnection;
    finally
      Free;
    end;
end;

procedure TranslateStrs;
const
  ext      = '.%s.po';
var
  LangID1, LangID2, basedir, olddir: String;
begin
  olddir := GetCurrentDir;
  //
  SetCurrentDir(ExtractFilePath(Application.Exename));
  basedir := AppendPathDelim('..') + AppendPathDelim('..');
  //
  LangID1 := Application.GetOptionValue('language');
  LangID2 := '';
  if Trim(LangId1) = '' then
    LazGetLanguageIDs(LangID1,LangID2);
  TranslateUnitResourceStrings('sdb_consts',basedir+
               'components/dbexport/languages/sdb_consts'+ext, LangID1,LangID2);
  TranslateUnitResourceStrings('ldd_consts',basedir+
               'components/datadict/languages/ldd_consts'+ext, LangID1,LangID2);
  TranslateUnitResourceStrings('lclstrconsts',basedir+
               'lcl/languages/lclstrconsts'+ext, LangID1,LangID2);
  TranslateUnitResourceStrings('lazdatadeskstr',basedir+
               'tools/lazdatadesktop/languages/lazdatadesktop'+ext, LangID1,LangID2);
  //
  SetCurrentDir(olddir);
end;

initialization
  TranslateStrs;

end.

