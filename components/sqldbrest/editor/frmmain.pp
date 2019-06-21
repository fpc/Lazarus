unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  ActnList, Menus, IniPropStorage, frasqldbfullrestschemaaditor, mrumanager;

type

  { TMainForm }

  TMainForm = class(TForm)
    aQuit: TAction;
    ASchemaNew: TAction;
    ASaveSchemaAs: TAction;
    ASaveSchema: TAction;
    ALoadSchema: TAction;
    AFileWriteConnections: TAction;
    AFileReadConnections: TAction;
    alMain: TActionList;
    ILMain: TImageList;
    IPSMain: TIniPropStorage;
    MIRecent: TMenuItem;
    MFile: TMenuItem;
    MIReadConnections: TMenuItem;
    MRUSchema: TMRUMenuManager;
    MWriteConnections: TMenuItem;
    MISchemaLoad: TMenuItem;
    MISchemaSave: TMenuItem;
    MISaveSchemaAs: TMenuItem;
    MISchemaNew: TMenuItem;
    MISep2: TMenuItem;
    MIQuit: TMenuItem;
    N1: TMenuItem;
    MMain: TMainMenu;
    fraEditor: TSchemaEditorFrame;
    odConnection: TOpenDialog;
    ODSchema: TOpenDialog;
    SDSchema: TSaveDialog;
    sdConnection: TSaveDialog;
    TreeView1: TTreeView;
    procedure AFileReadConnectionsExecute(Sender: TObject);
    procedure AFileWriteConnectionsExecute(Sender: TObject);
    procedure ALoadSchemaExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure ASaveSchemaAsExecute(Sender: TObject);
    procedure ASaveSchemaExecute(Sender: TObject);
    procedure ASchemaNewExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure IPSMainRestoreProperties(Sender: TObject);
    procedure IPSMainSaveProperties(Sender: TObject);
    procedure MRUSchemaRecentFile(Sender: TObject; const AFileName: String);
  private
    FBaseCaption,
    FFileName : String;
    procedure DoSchemaChanged(Sender: TObject);
    function SaveSchema : Boolean;
    function SaveSchemaAs : Boolean;
    function LoadSchema : Boolean;
    Function CheckSave : Boolean;
    Procedure LoadSchemaFile(const aFileName : String);
    Procedure SaveSchemaFile(const aFileName : String);
    Procedure SetCaption;
  public

  end;

var
  MainForm: TMainForm;

implementation


{$R *.lfm}

uses sqldbrestbridge, sqldbrestini;

resourcestring
  SSchemaChanged = 'Schema changed';
  SSchemaChangedSave = 'The schema has changed. %sDo you wish to save your changes?';
  SSaveSchema = 'Save schema';
  SDoNotSaveSchema = 'Do not save schema';
  SCancel = 'Cancel';
  SNewFile = 'New schema';

{ TMainForm }

procedure TMainForm.AFileReadConnectionsExecute(Sender: TObject);

begin
  With ODConnection do
    if Execute then
      fraEditor.LoadConnections(FileName);
end;


procedure TMainForm.AFileWriteConnectionsExecute(Sender: TObject);
begin
  With SDConnection do
    if Execute then
      fraEditor.SaveConnections(FileName);
end;

procedure TMainForm.ALoadSchemaExecute(Sender: TObject);
begin
  if CheckSave then
    LoadSchema;
end;

procedure TMainForm.aQuitExecute(Sender: TObject);
begin
  Close
end;

procedure TMainForm.ASaveSchemaAsExecute(Sender: TObject);
begin
  SaveSchemaAs;
end;

procedure TMainForm.ASaveSchemaExecute(Sender: TObject);
begin
  SaveSchema
end;

function TMainForm.SaveSchema : Boolean;

begin
  if FFileName='' then
    Result:=SaveSchemaAs
  else
    begin
    SaveSchemaFile(FFileName);
    Result:=True;
    end;
end;

procedure TMainForm.DoSchemaChanged(Sender: TObject);
begin
  SetCaption;
end;

function TMainForm.SaveSchemaAs : Boolean;

begin
  with SDSchema do
    begin
    FileName:=Self.FFileName;
    Result:=Execute;
    if Result then
      SaveSchemaFile(FileName);
    end;
end;

function TMainForm.LoadSchema: Boolean;
begin
  Result:= CheckSave;
  if not Result then
    exit;
  with ODSchema do
    begin
    Result:=Execute;
    if Result then
      LoadSchemaFile(FileName);
    end;
end;

function TMainForm.CheckSave: Boolean;

begin
  Result:=Not fraEditor.SchemaModified;
  if Result then
    exit;
  case QuestionDlg(SSchemaChanged, Format(SSchemaChangedSave, [LineEnding]), mtWarning, [mrYes, SSaveSchema, mrNo,
    SDoNotSaveSchema, mrCancel, SCancel], 0) of
    mrYes: Result:=SaveSchema;
    mrNo: Result:=True;
    mrCancel: Result:=False;
  end;
end;

procedure TMainForm.LoadSchemaFile(const aFileName: String);
begin
  fraEditor.LoadSchema(aFileName);
  FFileName:=aFileName;
  MRUSchema.AddToRecent(aFileName);
  SetCaption;
end;

procedure TMainForm.SaveSchemaFile(const aFileName: String);
begin
  FraEditor.SaveSchema(aFileName);
  FFileName:=aFileName;
  MRUSchema.AddToRecent(aFileName);
  SetCaption;
end;

procedure TMainForm.SetCaption;

Var
  S : String;

begin
  S:=FFileName;
  If (S='') then
    S:=SNewFile;
  if fraEditor.SchemaModified then
    S:=S+'*';
  Caption:=FBaseCaption+' ['+S+']';
end;

procedure TMainForm.ASchemaNewExecute(Sender: TObject);
begin
  if CheckSave then
    begin
    fraEditor.ClearSchema;
    FFileName:='';
    SetCaption;
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckSave;
end;

procedure TMainForm.FormCreate(Sender: TObject);

Var
  FN : String;

begin
  FBaseCaption:=Caption;
  FN:=GetAppConfigFile(False,False);
  IPSMain.IniFileName:=FN;
  IPSMain.Active:=True;
  IPSMain.Restore;
  MRUSchema.ShowRecentFiles;
  fraEditor.OnSchemaChanged:=@DoSchemaChanged;
  if Application.HasOption('c','connections') then
    fraEditor.LoadConnections(Application.GetOptionValue('c','connections'));
  if Application.HasOption('s','schema') then
    LoadSchemaFile(Application.GetOptionValue('s','schema'))
  else
    SetCaption;
end;

procedure TMainForm.IPSMainRestoreProperties(Sender: TObject);
begin
  fraEditor.LoadSession(IPSMain);
end;

procedure TMainForm.IPSMainSaveProperties(Sender: TObject);
begin
  fraEditor.SaveSession(IPSMain);
end;

procedure TMainForm.MRUSchemaRecentFile(Sender: TObject; const AFileName: String);
begin
  LoadSchemaFile(aFileName);
end;

end.

