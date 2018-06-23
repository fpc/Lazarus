unit HeapTrcView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, DOM, contnrs, Clipbrd, LCLProc, LCLType,
  LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  LazIDEIntf, MenuIntf, ToolBarIntf, IDECommands,
  // LeakView
  LeakInfo;

type
  TJumpProc = procedure (Sender: TObject; const SourceName: string;
                         Line, Column: integer) of object;

  { THeapTrcViewForm }

  THeapTrcViewForm = class(TForm)
    btnUpdate: TButton;
    btnBrowse: TButton;
    btnClipboard: TButton;
    BtnResolve: TButton;
    chkUseRaw: TCheckBox;
    chkStayOnTop: TCheckBox;
    edtTrcFileName:TComboBox;
    lblTrcFile: TLabel;
    ctrlPanel: TPanel;
    memoSummary: TMemo;
    OpenDialog: TOpenDialog;
    splitter: TSplitter;
    trvTraceInfo: TTreeView;
    procedure btnClipboardClick(Sender: TObject);
    procedure BtnResolveClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure chkStayOnTopChange(Sender: TObject);
    procedure chkUseRawChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure trvTraceInfoDblClick(Sender: TObject);
  private
    Finfo  : TLeakInfo;
    fItems  : TList;

    procedure DoUpdateLeaks(FromClip: Boolean = False);

    procedure ItemsToTree;
    procedure ChangeTreeText;

    procedure ClearItems;
    procedure DoJump;

    function GetStackTraceText(trace: TStackTrace; useRaw: Boolean): string;
    function GetStackLineText(const Line: TStackLine; useRaw: Boolean): string;

    procedure SaveState(cfg: TXMLConfig);
    procedure LoadState(cfg: TXMLConfig);

    procedure AddFileToList(const FileName: AnsiString);
  protected
    procedure LazarusJump(Sender: TObject; const SourceFile: string;
                          Line, Column: Integer);
  public
    destructor Destroy; override;
  public
    OnJumpProc : TJumpProc; //= procedure (Sender: TObject; const SourceName: string; Line: integer) of object;
  end;

resourcestring
  StackTraceFormat         = 'Leak: %d bytes x %d times'; // number of bytes leaked, leaks count
  StackTraceFormatSingle   = 'Leak: %d bytes';            // number of bytes leaked
  StackLineFormatWithFile  = '%s file: %s : %d; ';        // stack addr, filename (no path), line number
  StackLineFormat          = '%s';                        // stack addr

  strTotalMemAlloc      = 'Total Mem allocated: %d';
  strLeakingMemSize     = 'Leaking Mem Size: %d';
  strLeakingBlocksCount = 'Leaking Blocks Count: %d';
  //
  rsErrorParse = 'Error while parsing trace file';
  rsDTimes = ' (%d times)';
  rsLeakView = 'Leaks and Traces';
  //
  slblTrace = '.trc file';
  sbtnUpdate = 'Update';
  sbtnClipBrd = 'Paste Clipboard';
  sbtnResolve = 'Resolve';
  schkRaw = 'Raw leak data';
  schkTop = 'Stay on top';
  sfrmCap = 'Leaks and Traces - HeapTrc and GDB backtrace output viewer';
  sfrmSelectFileWithDebugInfo = 'Select file with debug info';
  sfrmSelectTrcFile = 'Select file with trace log';

var
  HeapTrcViewForm: THeapTrcViewForm = nil;

// JumpProc is the callback that is called everytime user double clicks
// on the leak line. It's legal to pass nil, then LazarusIDE is used to peform a jump
procedure ShowHeapTrcViewForm(JumpProc: TJumpProc = nil);

procedure Register;

implementation

{$R *.lfm}

procedure ShowHeapTrcViewForm(JumpProc: TJumpProc);
begin
  if not Assigned(HeapTrcViewForm) then
    HeapTrcViewForm := THeapTrcViewForm.Create(Application);
  if Assigned(JumpProc) then
    HeapTrcViewForm.OnJumpProc := JumpProc
  else
    HeapTrcViewForm.OnJumpProc := @HeapTrcViewForm.LazarusJump;
  HeapTrcViewForm.Show;
end;

{ THeapTrcViewForm }

procedure THeapTrcViewForm.btnUpdateClick(Sender: TObject);
begin
  DoUpdateLeaks;
  AddFileToList(edtTrcFileName.Text);
end;

procedure THeapTrcViewForm.btnClipboardClick(Sender: TObject);
begin
  DoUpdateLeaks(True);
end;

procedure THeapTrcViewForm.BtnResolveClick(Sender: TObject);
begin
  if Finfo = nil then exit;

  OpenDialog.FileName := '';
  OpenDialog.Filter := '';
  OpenDialog.Title := sfrmSelectFileWithDebugInfo;
  if not OpenDialog.Execute then Exit;

  Finfo.ResolveLeakInfo(OpenDialog.FileName, fItems);
  ChangeTreeText;
end;

procedure THeapTrcViewForm.btnBrowseClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Filter := slblTrace + '|*.trc';
  OpenDialog.Title := sfrmSelectTrcFile;
  if not OpenDialog.Execute then Exit;

  edtTrcFileName.Text := OpenDialog.FileName;
  DoUpdateLeaks;
  AddFileToList(edtTrcFileName.Text);
end;

procedure THeapTrcViewForm.chkStayOnTopChange(Sender: TObject);
begin
  if chkStayOnTop.Checked then Self.formStyle := fsStayOnTop
  else Self.formStyle := fsNormal;
end;

procedure THeapTrcViewForm.chkUseRawChange(Sender: TObject);
begin
  ChangeTreeText;
  trvTraceInfo.Invalidate;
end;

var
  ConfigFileName : AnsiString = '';
function CreateXMLConfig: TXMLConfig;
begin
  Result:=TXMLConfig.Create(nil);
  Result.RootName:='config';
  if (ConfigFileName='') and Assigned(LazarusIDE) then
    ConfigFileName:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'leakview.xml';
  Result.FileName:=ConfigFileName;
end;

procedure THeapTrcViewForm.FormCreate(Sender: TObject);
var
  cfg   : TXMLConfig;
begin
  Caption:=sfrmCap;
  lblTrcFile.Caption:=slblTrace;
  btnUpdate.Caption:=sbtnUpdate;
  btnClipboard.Caption:=sbtnClipBrd;
  BtnResolve.Caption:=sbtnResolve;
  chkUseRaw.Caption:=schkRaw;
  chkStayOnTop.Caption:=schkTop;
  fItems:=TList.Create;
  try
    cfg:=CreateXMLConfig;
    try
      LoadState(cfg);
    finally
      cfg.Free;
    end;
  except
  end;
end;

procedure THeapTrcViewForm.FormDestroy(Sender: TObject);
var
  cfg : TXMLConfig;
begin
  ClearItems;
  fItems.Free;
  try
    cfg:=CreateXMLConfig;
    try
      SaveState(cfg);
    finally
      cfg.Free;
    end;
  except
  end;
  HeapTrcViewForm:=nil;
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
  sz := 32 + trace.Count * 16; // 8 hex digits for Size + 8 hex digits for Size
  SetLength(Result, sz);
  HexInt64ToStr(trace.BlockSize, Result, 1);
  HexInt64ToStr(hash(trace.RawStackData), Result, 17);
  for i := 0 to trace.Count - 1 do
    if trace.lines[i].Addr <> 0
    then HexInt64ToStr(trace.lines[i].Addr, Result, 33 + i * 16)
    else HexInt64ToStr(Hash(trace.lines[i].RawLineData), Result, 17 + i * 16);
end;

procedure THeapTrcViewForm.ItemsToTree;
var
  i      : Integer;
  j      : Integer;
  trace  : TStackTrace;
  nd     : TTreeNode;
  hash   : TFPObjectHashTable;
  hashed : TStackTrace;
  s      : string;
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
      for j := 0 to trace.Count - 1 do begin
        trvTraceInfo.Items.AddChildObject(nd, '-', {%H-}Pointer(PtrInt(j)));
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
  for i := 0 to fItems.Count - 1 do
    TObject(fItems[i]).Free;
  fItems.Clear;
end;

procedure THeapTrcViewForm.DoUpdateLeaks(FromClip: Boolean = False);
var
  data  : TLeakStatus;
  txt: String;
begin
  FreeAndNil(Finfo);
  trvTraceInfo.BeginUpdate;
  try
    ClearItems;
    trvTraceInfo.Items.Clear;
    if FromClip then begin
      txt := Clipboard.AsText;
      if txt = '' then exit;
      Finfo := AllocHeapTraceInfoFromText(txt);
    end else begin
      if (not FileExistsUTF8(edtTrcFileName.Text)) or FromClip then Exit;
      Finfo := AllocHeapTraceInfo(edtTrcFileName.Text);
    end;

    if Finfo.GetLeakInfo(data, fItems) then ItemsToTree
    else trvTraceInfo.Items.Add(nil, rsErrorParse);

    memoSummary.Clear;
    with memoSummary.Lines do begin
      Add( Format(strTotalMemAlloc, [data.TotalMem]));
      Add( Format(strLeakingMemSize, [data.LeakedMem]));
      Add( Format(strLeakingBlocksCount, [data.LeakCount]));
    end;

  finally
    trvTraceInfo.EndUpdate;
  end;
  if trvTraceInfo.Items.TopLvlCount = 1 then
    trvTraceInfo.Items.TopLvlItems[0].Expand(False);
end;

procedure THeapTrcViewForm.DoJump;
var
  nd         : TTreeNode;
  searchFile : string;
  idx        : Integer;
  trace      : TStackTrace;
  StackLine: TStackLine;
begin
  if not Assigned(@OnJumpProc) then Exit;
  nd := trvTraceInfo.Selected;

  if not Assigned(nd) then Exit;
  if nd.Parent = nil then Exit;

  idx := Integer({%H-}PtrInt(nd.Data));
  trace := TStackTrace(nd.Parent.Data);
  if not Assigned(trace) or (idx >= trace.Count) then Exit;

  searchFile := trace.Lines[idx].FileName;
  if searchFile = '' then Exit;

  StackLine:= trace.Lines[idx];
  OnJumpProc(Self,  searchFile, StackLine.LineNum, StackLine.Column);
end;

procedure THeapTrcViewForm.ChangeTreeText;
var
  i, j    : Integer;
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
    if (Result <> '') and (trace.LeakCount > 1) then Result := Result + Format(
      rsDTimes, [trace.LeakCount]);
  end;

  if not useRaw or (Result = '') then begin
    if trace.LeakCount > 1
      then Result := Format(StackTraceFormat, [trace.BlockSize, trace.LeakCount])
      else Result := Format(StackTraceFormatSingle, [trace.BlockSize]);
  end;

end;

function THeapTrcViewForm.GetStackLineText(const Line: TStackLine; useRaw: boolean): string;
begin
  if useRaw then
    Result := Line.RawLineData;

  if (not useRaw) or (Result = '') or
     ( (Pos(' ', Trim(Result)) < 1) and (Pos(':', Trim(Result)) < 1) and
       ( (copy(Trim(Result),1,1) = '$') or (copy(Trim(Result),1,2) = '0x') )
     ) // Rawdata may be address only
  then
    with Line do
      if FileName <> ''
        then Result := Format(StackLineFormatWithFile, ['$'+IntToHex(Addr, sizeof(Pointer)*2), ExtractFileName(FileName), LineNum])
        else Result := Format(StackLineFormat, ['$'+IntToHex(Addr, sizeof(Pointer)*2)]);
end;

procedure THeapTrcViewForm.SaveState(cfg:TXMLConfig);
var
  b : TRect;
  i : Integer;
begin
  cfg.SetValue('isStayOnTop',FormStyle=fsStayOnTop);
  b:=BoundsRect;
  cfg.OpenKey('bounds');
  cfg.SetValue('left', b.Left);
  cfg.SetValue('top', b.Top);
  cfg.SetValue('right', b.Right);
  cfg.SetValue('bottom', b.Bottom);
  cfg.CloseKey;
  for i:=0 to edtTrcFileName.Items.Count-1 do
    cfg.SetValue(DOMString('path'+IntToStr(i)), UTF8Decode(edtTrcFileName.Items[i]) );
end;

function PointInRect(p: TPoint; const r: TRect): Boolean;
begin
  Result:=(p.X>=r.Left) and (p.X<=r.Right) and (p.y>=r.Top) and (p.y<=r.Bottom);
end;

procedure inAnyMonitor(var b: TRect);
var
  m: TMonitor;
  mb: TRect;
const
  MinOverLap = 40;
begin
  m := Screen.MonitorFromRect(b); // Nearest Monitor
  if assigned(m)
  then mb := m.BoundsRect
  else mb := Screen.WorkAreaRect;

  // make sure top(window-bar) is visible
  if b.Top < mb.Top then OffsetRect(b, 0, mb.Top-b.Top);
  if b.Top + MinOverLap > mb.Bottom then OffsetRect(b, 0, mb.Top-b.Top-MinOverLap);
  // move left/right
  if b.Left + MinOverLap > mb.Right then OffsetRect(b, mb.Right-b.Left-MinOverLap, 0);
  if b.Right - MinOverLap < mb.Left then OffsetRect(b, mb.Left-b.Right+MinOverLap, 0);
end;

procedure THeapTrcViewForm.LoadState(cfg:TXMLConfig);
var
  b     : TRect;
  isTop : Boolean;
  st    : TStringList;
  s     : WideString;
  i     : Integer;
const
  InitFormStyle: array [Boolean] of TFormStyle = (fsNormal, fsStayOnTop);
begin
  isTop:=True;
  b:=BoundsRect;
  st:=TStringList.Create;
  try
    istop:=cfg.GetValue('isStayOnTop',isTop);
    cfg.OpenKey('bounds');
    b.Left:=cfg.GetValue('left', b.Left);
    b.Top:=cfg.GetValue('top', b.Top);
    b.Right:=cfg.GetValue('right', b.Right);
    b.Bottom:=cfg.GetValue('bottom', b.Bottom);
    cfg.CloseKey;

    if b.Right-b.Left<=0 then b.Right:=b.Left+40;
    if b.Bottom-b.Top<=0 then b.Bottom:=b.Top+40;

    for i:=0 to 7 do begin
      s:=cfg.GetValue(DOMString('path'+IntToStr(i)), '');
      if s<>'' then st.Add(UTF8Encode(s));
    end;

  except
  end;
  inAnyMonitor(b);

  FormStyle:=InitFormStyle[isTop];
  BoundsRect:=b;
  chkStayOnTop.Checked := isTop;
  if st.Count>0 then begin
    edtTrcFileName.Items.AddStrings(st);
    edtTrcFileName.ItemIndex:=0;
  end;

  st.Free;
end;

procedure THeapTrcViewForm.AddFileToList(const FileName:AnsiString);
var
  i : Integer;
begin
  i:=edtTrcFileName.Items.IndexOf(FileName);
  if (i<0) then begin
    if edtTrcFileName.Items.Count=8 then
      edtTrcFileName.Items.Delete(7);
  end else
    edtTrcFileName.Items.Delete(i);
  edtTrcFileName.Items.Insert(0, FileName);
end;

procedure THeapTrcViewForm.LazarusJump(Sender: TObject;
  const SourceFile: string; Line, Column: Integer);
var
  nm  : string;
begin
  if not FileExistsUTF8(SourceFile) then begin
    nm := LazarusIDE.FindSourceFile(SourceFile, '', [fsfUseIncludePaths] );
    if nm = '' then
      nm := SourceFile;
  end else
    nm := SourceFile;
  LazarusIDE.DoOpenFileAndJumpToPos(nm, Point(Column, Line), -1, -1, -1, [ofOnlyIfExists, ofRegularFile]);
end;

destructor THeapTrcViewForm.Destroy;
begin
  FreeAndNil(Finfo);
  inherited Destroy;
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  ShowHeapTrcViewForm(nil);
end;

procedure Register;
var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
  IDEButtonCommand: TIDEButtonCommand;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'mnuLeakView', rsLeakView, nil, @IDEMenuClicked);

  IDEShortCutX := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, 'Leaks and Traces', rsLeakView, IDEShortCutX, nil, @IDEMenuClicked);
    if IDECommand <> nil then
    begin
      IDEButtonCommand := RegisterIDEButtonCommand(IDECommand);
      if IDEButtonCommand=nil then ;
    end;
  end;
end;

end.

