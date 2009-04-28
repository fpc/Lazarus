unit HeapTrcView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, LeakInfo, LazIDEIntf, MenuIntf, contnrs;

type
  TJumpProc = procedure (Sender: TObject; const SourceName: string; Line: integer) of object;
  { THeapTrcViewForm }

  THeapTrcViewForm = class(TForm)
    btnUpdate: TButton;
    btnBrowse: TButton;
    chkUseRaw: TCheckBox;
    chkStayOnTop: TCheckBox;
    edtTrcFileName: TEdit;
    lblTrcFile: TLabel;
    trvTraceInfo: TTreeView;
    procedure btnUpdateClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure chkStayOnTopChange(Sender: TObject);
    procedure chkStayOnTopClick(Sender: TObject);
    procedure chkUseRawChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure trvTraceInfoDblClick(Sender: TObject);
  private
    { private declarations }
    fItems  : TList;

    procedure DoUpdateLeaks;

    procedure ItemsToTree;
    procedure ChangeTreeText;

    procedure ClearItems;
    procedure DoJump;

    function GetStackTraceText(trace: TStackTrace; useRaw: Boolean): string;
    function GetStackLineText(const Line: TStackLine; useRaw: Boolean): string;

  protected
    procedure LazarusJump(Sender: TObject; const SourceFile: string; Line: Integer);
  public
    { public declarations }

    OnJumpProc : TJumpProc; //= procedure (Sender: TObject; const SourceName: string; Line: integer) of object;

  end;

var
  HeapTrcViewForm: THeapTrcViewForm = nil;

// JumpProc is the callback that is called everytime user double clicks
// on the leak line. It's legal to pass nil, then LazarusIDE is used to peform a jump
procedure ShowHeapTrcViewForm(JumpProc: TJumpProc = nil);

procedure Register;

implementation

const // resorucestring ?
  StackTraceFormat         = 'Leak: %d bytes x %d times'; // number of bytes leaked, leaks count
  StackTraceFormatSingle   = 'Leak: %d bytes';            // number of bytes leaked
  StackLineFormatWithFile  = '%s line: %d; file: %s';     // stack addr, filename (no path), line number
  StackLineFormat          = '%s';                        // stack addr

procedure ShowHeapTrcViewForm(JumpProc: TJumpProc);
begin
  if not Assigned(HeapTrcViewForm) then HeapTrcViewForm := THeapTrcViewForm.Create(nil);
  if not Assigned(JumpProc)
    then HeapTrcViewForm.OnJumpProc := @HeapTrcViewForm.LazarusJump
    else HeapTrcViewForm.OnJumpProc := JumpProc;
  HeapTrcViewForm.Show;
end;

{ THeapTrcViewForm }

procedure THeapTrcViewForm.btnUpdateClick(Sender: TObject);
begin
  DoUpdateLeaks;
end;

procedure THeapTrcViewForm.btnBrowseClick(Sender: TObject);
var
  OpenDialog : TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    if not OpenDialog.Execute then Exit;
    edtTrcFileName.Text := OpenDialog.FileName;
    DoUpdateLeaks;
  finally
    OpenDialog.Free;
  end;
end;

procedure THeapTrcViewForm.chkStayOnTopChange(Sender: TObject);
begin
  if chkStayOnTop.Checked then Self.formStyle := fsStayOnTop
  else Self.formStyle := fsNormal;
end;

procedure THeapTrcViewForm.chkStayOnTopClick(Sender: TObject);
begin
end;

procedure THeapTrcViewForm.chkUseRawChange(Sender: TObject);
begin
  ChangeTreeText;
  trvTraceInfo.Invalidate;
end;

procedure THeapTrcViewForm.FormCreate(Sender: TObject);
begin
  fItems := TList.Create;
  chkStayOnTop.Checked := FormStyle = fsStayOnTop;
end;

procedure THeapTrcViewForm.FormDestroy(Sender: TObject);
begin
  ClearItems;
  fItems.Free;
end;

procedure THeapTrcViewForm.trvTraceInfoDblClick(Sender: TObject);
begin
  DoJump;
end;

//note: to range check performed
procedure HexInt64ToStr(i64: Int64; var s: string; ofs: Integer);
var
  i : Integer;
  j : Integer;
const
  Hexes: array [0..$F] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  j := ofs + 15;
  for i := 0 to 7 do begin
    s[j] := Hexes[ i64 and $F ]; dec(j);
    s[j] := Hexes[ ((i64 and $F0) shr 4) and $F ]; dec(j);
    i64 := i64 shr 8;
  end;
end;

function GetHashString(trace: TStackTrace): string;
var
  i   : integer;
  sz  : Integer;
begin
  sz := 16 + trace.LinesCount * 16; // 8 hex digits for Size + 8 hex digits for Size
  SetLength(Result, sz);
  HexInt64ToStr(trace.BlockSize, Result, 1);
  for i := 0 to trace.LinesCount - 1 do
    HexInt64ToStr(trace.lines[i].Addr, Result, 17 + i * 16);
end;

procedure THeapTrcViewForm.ItemsToTree;
var
  i      : Integer;
  j      : Integer;
  trace  : TStackTrace;
  nd     : TTreeNode;
  hash   : TFPObjectHashTable;
  hnode  : THTObjectNode;
  list   : TFPObjectList;
  hashed : TStackTrace;
  s      : string;
  cnt    : integer;
begin
  hash := TFPObjectHashTable.Create(false);
  try
    // removing duplicates
    for i := 0 to fItems.Count - 1 do begin
      trace := TStackTrace(fItems[i]);
      s := GetHashString(trace);
      hashed := TStackTrace(hash.Items[s]);
      if Assigned(hashed) then begin
        inc(hashed.LeakCount);
        trace.Free; // remove from list
        fItems[i] := nil;
      end else
        hash.Add(s, trace)
    end;
    fItems.Pack;

    // filling the tree
    for i := 0 to fItems.Count - 1 do begin
      trace := TStackTrace(fItems[i]);
      nd := trvTraceInfo.Items.AddChildObject(nil, '+', trace);
      for j := 0 to trace.LinesCount - 1 do begin
        trvTraceInfo.Items.AddChildObject(nd, '-', Pointer(j));
      end;
    end;

    // updating tree text
    ChangeTreeText;

  finally
    hash.free;
  end;
end;

procedure THeapTrcViewForm.ClearItems;
var
  i : integer;
begin
  for i := 0 to fItems.Count - 1 do TObject(fItems[i]).Free;
  fItems.Clear;
end;

procedure THeapTrcViewForm.DoUpdateLeaks;
var
  info  : TLeakInfo;
  data  : TLeakStatus;
begin
  trvTraceInfo.BeginUpdate;
  try
    ClearItems;
    trvTraceInfo.Items.Clear;
    if not FileExists(edtTrcFileName.Text) then Exit;

    info := AllocHeapTraceInfo(edtTrcFileName.Text);
    try
      if info.GetLeakInfo(data, fItems) then ItemsToTree
      else trvTraceInfo.Items.Add(nil, 'Error while parsing trace file');
    finally
      info.Free;
    end;
  finally
    trvTraceInfo.EndUpdate;
  end;
end;

procedure THeapTrcViewForm.DoJump;
var
  nd         : TTreeNode;
  searchFile : string;
  idx        : Integer;
  trace      : TStackTrace;
begin
  if not Assigned(@OnJumpProc) then Exit;
  nd := trvTraceInfo.Selected;

  if not Assigned(nd) then Exit;
  if nd.Parent = nil then Exit;

  idx := Integer(nd.Data);
  trace := TStackTrace(nd.Parent.Data);
  if not Assigned(trace) or (idx >= trace.LinesCount) then Exit;

  searchFile := trace.Lines[idx].FileName;
  if searchFile = '' then Exit;

  idx := trace.Lines[idx].LineNum;
  OnJumpProc(Self,  searchFile, idx);
end;

procedure THeapTrcViewForm.ChangeTreeText;
var
  i, j    : Integer;
  idx     : Integer;
  useRaw  : Boolean;
  nd      : TTreeNode;
  trace   : TStackTrace;
begin
  trvTraceInfo.Items.BeginUpdate;
  try
    useRaw := chkUseRaw.Checked;
    for i := 0 to trvTraceInfo. Items.Count - 1 do begin
      nd := TTreeNode(trvTraceInfo.Items[i]);
      if Assigned(nd.Parent) or not Assigned(nd.Data) then Continue;
      trace := TStackTrace(nd.Data);
      nd.Text := GetStackTraceText(trace, useRaw);
      for j := 0 to nd.Count - 1 do begin
        idx := Integer(nd.Items[j].Data);
        nd.Items[j].Text := GetStackLineText(  trace.Lines[j], useRaw );
      end;
    end;
  finally
    trvTraceInfo.Items.EndUpdate;
  end;
end;

function THeapTrcViewForm.GetStackTraceText(trace: TStackTrace; useRaw: boolean): string;
begin
  if useRaw then begin
    Result := trace.RawStackData;
    if (Result <> '') and (trace.LeakCount > 1) then Result := Result + Format(' (%d times)', [trace.LeakCount]);
  end;

  if not useRaw or (Result = '') then begin
    if trace.LeakCount > 1
      then Result := Format(StackTraceFormat, [trace.BlockSize, trace.LeakCount])
      else Result := Format(StackTraceFormatSingle, [trace.BlockSize]);
  end;

end;

function THeapTrcViewForm.GetStackLineText(const Line: TStackLine; useRaw: boolean): string;
begin
  if useRaw then Result := Line.RawLineData;
  if not useRaw or (Result = '') then
    with Line do
      if FileName <> ''
        then Result := Format(StackLineFormatWithFile, ['$'+IntToHex(Addr, sizeof(Pointer)*2), LineNum, ExtractFileName(FileName)])
        else Result := Format(StackLineFormat, ['$'+IntToHex(Addr, sizeof(Pointer)*2)]);
end;

procedure THeapTrcViewForm.LazarusJump(Sender: TObject; const SourceFile: string; Line: Integer);
var
  nm  : string;
begin
  if not FileExists(SourceFile) then begin
    nm := LazarusIDE.FindSourceFile(SourceFile, '', [fsfUseIncludePaths] );
    if nm = '' then nm := SourceFile;
  end else
    nm := SourceFile;
  LazarusIDE.DoOpenFileAndJumpToPos(nm, Point(1, Line), -1, -1, [ofOnlyIfExists, ofRegularFile]);
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  ShowHeapTrcViewForm(nil);
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmSecondaryTools, 'mnuLeakView', 'Leak View', nil, @IDEMenuClicked);
end;


initialization
  {$I heaptrcview.lrs}

finalization
  HeapTrcViewForm.Free;

end.

