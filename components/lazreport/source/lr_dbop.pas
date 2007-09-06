
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Open table dialog            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_DBOp;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrOpenDBDialog = class(TCommonDialog)
  private
    { Private declarations }
    FPanel: TPanel;
    FLabel: TLabel;
    FComboBox: TComboBox;
    FDlghWnd: HWND;
    FNullAlias: String;
    FAliasName: String;
    FFilter: String;
    FFilterIndex: Integer;
    FFileName: String;
    FTableName: String;
    FTitle: string;
    procedure InitExtDlg;
    procedure DoneExtDlg;
    function GetAliasDirectory(A: String): String;
    procedure ComboBoxChange(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
{$IFDEF Delphi2}
    function Execute: Boolean;
{$ELSE}
    function Execute: Boolean; override;
{$ENDIF}
    property AliasName: String read FAliasName write FAliasName;
    property FileName: String read FFileName write FFileName;
    property TableName: String read FTableName write FTableName;
  published
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property Title: string read FTitle write FTitle;
  end;


implementation

{$R *.RES}

uses CommDlg, Dlgs, DB
{$IFDEF BDE}
, DBTables
{$ENDIF};

var
  OpenDBDialog: TfrOpenDBDialog;

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
begin
  GetWindowRect(Wnd, Rect);
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

function ExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
var
  s: String[255];
  i: Integer;
  b: Boolean;
begin
  Result := 0;
  if Msg = WM_NOTIFY then
    case POFNotify(LParam)^.hdr.code of
      CDN_INITDONE:
        begin
          SetParent(OpenDBDialog.FPanel.Handle, Wnd);
          OpenDBDialog.FPanel.Show;
          OpenDBDialog.FDlghWnd := GetWindowLong(Wnd, GWL_HWNDPARENT);
          CenterWindow(OpenDBDialog.FDlghWnd);
        end;
      CDN_FOLDERCHANGE:
        begin
          s[0] := Chr(GetCurrentDirectory(255, @s[1]));
          b := True;
          with OpenDBDialog.FComboBox do
          begin
            for i := 0 to Items.Count-1 do
              if AnsiCompareText(s, OpenDBDialog.GetAliasDirectory(Items[i])) = 0 then
              begin
                ItemIndex := i;
                b := False;
                break;
              end;
            if b then
              ItemIndex := Items.IndexOf(OpenDBDialog.FNullAlias);
            OpenDBDialog.FAliasName := Items[ItemIndex];
          end;
          SendMessage(GetDlgItem(OpenDBDialog.FDlghWnd, edt1), WM_SETTEXT, 0, 0);
        end;
    end;
end;

constructor TfrOpenDBDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilterIndex := 1;
end;

procedure TfrOpenDBDialog.InitExtDlg;
begin
  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Visible := False;
    Parent := Self.Owner as TWinControl;
    BevelOuter := bvNone;
    SetBounds(0, 236, 400, 30);
    FLabel := TLabel.Create(Self);
    with FLabel do
    begin
      Parent := FPanel;
      Left := 8;
      Top := 2;
      Caption := LoadStr(65000); //'&Alias:';
    end;
    FComboBox := TComboBox.Create(Self);
    with FComboBox do
    begin
      Parent := FPanel;
      Left := 81; Top := 0; Width := 234;
      Style := csDropDownList;
      Sorted := True;
{$IFDEF BDE}
      Session.GetAliasNames(Items);
{$ENDIF}
      FNullAlias := LoadStr(65001); //'None';
      Items.Add(FNullAlias);
      ItemIndex := 0;
      OnChange := ComboBoxChange;
    end;
    FLabel.FocusControl := FComboBox;
  end;
  OpenDBDialog := Self;
end;

procedure TfrOpenDBDialog.DoneExtDlg;
begin
  FComboBox.Free;
  FLabel.Free;
  FPanel.Free;
end;

function TfrOpenDBDialog.Execute: Boolean;
var
  OpenFilename: TOpenFilename;

  function AllocFilterStr(const S:string): PChar;
  var P: PChar;
  begin
    Result := nil;
    if S <> '' then
    begin
      Result := StrCopy(StrAlloc(Length(S) + 2), PChar(S));
      P := Result;
      while P^ <> #0 do
      begin
        if P^ = '|' then P^ := #0;
        Inc(P);
      end;
      Inc(P);
      P^ := #0;
    end;
  end;

begin
  InitExtDlg;
  FillChar(OpenFileName, SizeOf(OpenFileName), 0);
  OpenFilename.hInstance := hInstance;
  with OpenFilename do
  try
    lStructSize := SizeOf(TOpenFilename);
    nMaxFile := MAX_PATH;
    GetMem(lpstrFile, MAX_PATH + 2);
    FillChar(lpstrFile^, MAX_PATH + 2, 0);
    lpstrFilter := AllocFilterStr(FFilter);
    nFilterIndex := FFilterIndex;
    lpstrTitle := PChar(FTitle);
    Flags := OFN_ENABLEHOOK + OFN_EXPLORER + OFN_ENABLETEMPLATE + OFN_HIDEREADONLY;
    lpfnHook := ExplorerHook;
    lpTemplateName := 'ALIASDLGTEMPLATE';
    hWndOwner := Application.Handle;
    Result := TaskModalDialog(@GetOpenFileName, OpenFileName);
    if Result then
    begin
      FFileName := lpstrFile;
      FTableName := ExtractFileName(lpstrFile);
      if FAliasName = FNullAlias then
        FAliasName := ExtractFilePath(lpstrFile);
      FFilterIndex := nFilterIndex;
    end;
  finally
    if lpstrFile <> nil then FreeMem(lpstrFile, MAX_PATH + 2);
    if lpstrFilter <> nil then StrDispose(lpstrFilter);
  end;
  DoneExtDlg;
end;

function TfrOpenDBDialog.GetAliasDirectory(a: String): String;
var
  sl: TStringList;
begin
  Result := '';
  if a <> FNullAlias then
  begin
    sl := TStringList.Create;
{$IFDEF BDE}
    Session.GetAliasParams(a, sl);
{$ENDIF}
    Result := sl.Values['PATH'];
    sl.Free;
  end;
end;

procedure TfrOpenDBDialog.ComboBoxChange(Sender: TObject);
var
  s: String;
begin
  s := GetAliasDirectory(FComboBox.Items[FComboBox.ItemIndex]);
  SendMessage(GetDlgItem(FDlghWnd, edt1), WM_SETTEXT, 0, Integer(@s[1]));
  PostMessage(FDlghWnd, WM_KEYDOWN, vk_Return, 0);
end;

end.
