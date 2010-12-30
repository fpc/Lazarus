unit SQLStringsPropertyEditorDlg;

{$IFDEF VER2_5_1}
{$DEFINE HASSQLPARSER}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  SynEdit, ButtonPanel, SynHighlighterSQL, ComCtrls, SQLDb, db, DBGrids, Menus,
  SrcEditorIntf,clipbrd;

type

  { TSQLStringsPropertyEditorDlg }

  TSQLStringsPropertyEditorDlg = class(TForm)
    ButtonsPanel: TButtonPanel;
    ImageList: TImageList;
    MICheck: TMenuItem;
    MICreateConstant: TMenuItem;
    MICleanup: TMenuItem;
    PMSQL: TPopupMenu;
    ResultDBGrid: TDBGrid;
    SQLDataSource: TDatasource;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    SaveDialog: TSaveDialog;
    SQLEditor: TSynEdit;
    SQLHighlighter: TSynSQLSyn;
    EditorTabSheet: TTabSheet;
    ResultTabSheet: TTabSheet;
    SQLQuery: TSQLQuery;
    ToolBar: TToolBar;
    OpenToolButton: TToolButton;
    SaveToolButton: TToolButton;
    DividerToolButton: TToolButton;
    ExecuteToolButton: TToolButton;
    TBCheck: TToolButton;
    procedure ExecuteToolButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MICleanupClick(Sender: TObject);
    procedure MICreateConstantClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure SaveToolButtonClick(Sender: TObject);
    procedure TBCheckClick(Sender: TObject);
  private
    { private declarations }
    FConnection:TSQLConnection;
    FISSQLScript: Boolean;
    FTransaction:TSQLTransaction;

    function CheckConnection:boolean;
    procedure CheckSQLSyntax(SQL: TStrings);
    procedure CleanupDelphiCode;
    procedure CreateConstant;
  public
    { public declarations }
    constructor Create(AOwner:TComponent);override;
  published
    property Connection: TSQLConnection   read FConnection  write FConnection;
    property Transaction:TSQLTransaction read FTransaction write FTransaction;
    Property IsSQLScript : Boolean Read FISSQLScript Write FIsSQLScript;
  end; 

implementation

uses
{$IFDEF HASSQLPARSER}
  fpsqltree,fpsqlparser,
{$ENDIF}
  strutils;

{$R *.lfm}

resourcestring
  SSQLTabCaption    = 'SQL Code';
  SResultTabCaption = 'Results';
  SSQLOK            = 'SQL OK';
  SQLSyntaxOK       = 'No syntax errors in SQL statement.';
  SSQLError         = 'SQL Error';
  SSQLSyntaxError   = 'Syntax error in SQL statement:'+slineBreak+'%s';

{ TSQLStringsPropertyEditorDlg }

//----------------------------------------------------------------//
constructor TSQLStringsPropertyEditorDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceEditorManagerIntf.GetEditorControlSettings(SQLEditor);
  SourceEditorManagerIntf.GetHighlighterSettings(SQLHighlighter);
  EditorTabSheet.Caption := SSQLTabCaption;
  ResultTabSheet.Caption := SResultTabCaption;
end;

//----------------------------------------------------------//
function TSQLStringsPropertyEditorDlg.CheckConnection:boolean;
begin
  Result := (Assigned(FConnection)) and (Assigned(FTransaction))
             and(FConnection.Connected);
end;

//------------------------------------------------------------------------//
procedure TSQLStringsPropertyEditorDlg.OpenToolButtonClick(Sender: TObject);
begin
  if(OpenDialog.Execute)then
    SQLEditor.Lines.LoadFromFile(UTF8ToSys(OpenDialog.FileName));
end;

//---------------------------------------------------------------------------//
procedure TSQLStringsPropertyEditorDlg.ExecuteToolButtonClick(Sender: TObject);
begin
  try
    SQLQuery.Close;
    SQLQuery.SQL.Text := SQLEditor.Text;
    SQLQuery.Open;
    PageControl.ActivePage := ResultTabSheet;
  except
    on e:Exception do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;
end;

//-------------------------------------------------------------//
procedure TSQLStringsPropertyEditorDlg.FormShow(Sender: TObject);

Var
  D : TSQLDialect;

begin
  {$IFDEF HASSQLPARSER}
  TBCheck.Visible:=True;
  MICheck.Visible:=True;
  {$ELSE}
  TBCheck.Visible:=False;
  MICheck.Visible:=True;
  {$ENDIF}
  D:=sqlStandard;
  If Assigned(FConnection) then
    begin
    if (copy(LowerCase(FConnection.ClassName),1,3)='tib') then
      D:=sqlinterbase6
    else if (copy(LowerCase(FConnection.ClassName),1,7)='toracle') then
      D:=sqloracle
    else if (Copy(LowerCase(FConnection.ClassName),1,6)='tmysql') then
      D:=sqlmysql;
    end;
  if (CheckConnection) then
    begin
    SQLQuery.DataBase    := FConnection;
    SQLQuery.Transaction := FTransaction;
    ResultTabSheet.TabVisible    := True;
    ExecuteToolButton.Visible := True;
    FConnection.GetTableNames(SQLHighLighter.TableNames);
    end
  else
    begin
    ResultTabSheet.TabVisible    := False;
    ExecuteToolButton.Visible := False;
    end;
  SQLHighlighter.SQLDIalect:=D;
  SQLHighlighter.Enabled:=True;
{$ifdef unix}
  {$ifndef darwin}
  SQLEditor.Font.Name:='-adobe-courier-medium-r-normal-*-8-*-*-*-m-*-iso10646-1';
  {$endif}
{$endif}
  SQLEditor.SetFocus;
end;

procedure TSQLStringsPropertyEditorDlg.MICleanupClick(Sender: TObject);

begin
  CleanupDelphiCode;
end;

procedure TSQLStringsPropertyEditorDlg.CleanupDelphiCode;

Var
  L : TStringList;
  I,J,K : Integer;
  S : String;
begin
  L:=TStringList.Create;
  try
    L.Assign(SQLEditor.Lines);
    For I:=0 to L.Count-1 do
      begin
      S:=L[i];
      K:=0;
      For J:=1 to Length(S) do
        If S[j]='''' then
          Inc(K);
      if (K<>0) and ((K mod 2)=0) then
        begin
        J:=Pos('''',S);
        Delete(S,1,J);
        J:=RPos('''',S);
        S:=Copy(S,1,J-1);
        L[i]:=S;
        end;
      end;
    SQLEditor.Lines:=L;
  finally
    L.Free;
  end;
end;

procedure TSQLStringsPropertyEditorDlg.MICreateConstantClick(Sender: TObject);
begin
  CreateConstant;
end;

procedure TSQLStringsPropertyEditorDlg.CreateConstant;

Var
  C,S : String;
  I : Integer;

begin
  For I:=0 to SQLEditor.Lines.Count-1 do
    begin
    S:=SQLEditor.Lines[i];
    If (C<>'') then
      C:=C+'+sLineBreak+'+sLineBreak;
    C:=C+''''+StringReplace(S,'''','''''',[rfReplaceAll])+'''';
    end;
  C:='SQL = '+C+';';
  Clipboard.AsText:=C;
end;

//------------------------------------------------------------------------//
procedure TSQLStringsPropertyEditorDlg.SaveToolButtonClick(Sender: TObject);
begin
  if(SaveDialog.Execute)then
    SQLEditor.Lines.SaveToFile(UTF8ToSys(SaveDialog.FileName));
end;

procedure TSQLStringsPropertyEditorDlg.TBCheckClick(Sender: TObject);
begin
  CheckSQLSyntax(SQLEditor.Lines)
end;

procedure TSQLStringsPropertyEditorDlg.CheckSQLSyntax(SQL : TStrings);
{$IFDEF HASSQLPARSER}
Var
  S : TStream;
  P : TSQLParser;
  E : TSQLElement;
  EL : TSQLElementList;
  Msg : String;
{$ENDIF}
begin
  {$IFDEF HASSQLPARSER}
  S:=TMemoryStream.Create;
  try
    SQL.SaveToStream(S);
    S.Position:=0;
    P:=TSQLParser.Create(S);
    try
      try
        E:=Nil;
        EL:=Nil;
        If IsSQLScript then
          EL:=P.ParseScript
        else
          E:=P.Parse;
        E.Free;
        EL.Free;
        MessageDLG(SSQLOK,SQLSyntaxOK,mtInformation,[mbOK],0);
      except
        On E : Exception do
          begin
          Msg:=Format(SSQLSyntaxError,[E.Message]);
          MessageDLG(SSQLError,Msg,mtError,[mbOK],0);
          end;
      end;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
  {$ENDIF}
end;
end.

