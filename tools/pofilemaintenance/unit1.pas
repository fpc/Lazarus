unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, Menus, LCLIntf, LazFileUtils, PotFile;

type

  { TForm1 }

  TForm1 = class(TForm)
    chkSearchPoItemIgnoreUnit: TCheckBox;
    chkSearchPoItemIdText: TCheckBox;
    chkSearchPoItemFullWord: TCheckBox;
    edSearchPoItems: TEdit;
    lbPoFiles: TListBox;
    lbPoItems: TListBox;
    memInfo: TMemo;
    mnDupIgnCase: TMenuItem;
    mnDupIgnoreSpaceDiff: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    pnlSearch: TPanel;
    btnClearSearchPoItem: TSpeedButton;
    popMoveTo: TPopupMenu;
    popDupFinder: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    tbMovePoItems: TToolButton;
    tbSave: TToolButton;
    tbDupId: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbShowMsgIdEntries: TToolButton;
    tbShowPoFileInfo: TToolButton;
    procedure btnClearSearchPoItemClick(Sender: TObject);
    procedure chkSearchPoItemFullWordChange(Sender: TObject);
    procedure edSearchPoItemsChange(Sender: TObject);
    procedure edSearchPoItemsEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lbPoFilesClick(Sender: TObject);
    procedure lbPoItemsClick(Sender: TObject);
    procedure tbDupIdClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure tbShowMsgIdEntriesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    FPotList: TPotFileList;
    FCurPotFile: TPotFile;
    FInbPoFilesClick: Boolean;
    FDupIdPotFile: TPotFile;

    function AddFile(AFileName: String): TPotFile;
    procedure OnMoveClicked(Sender: TObject);
    procedure SetCurrentFile(ACurPotFile: TPotFile);
    procedure UpdateSaveButton;
    procedure UpdatePoItems;
    procedure UpdatePoItemFiles;
    procedure UpdatePotItems;
  public
    destructor Destroy; override;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButton1Click(Sender: TObject);
var
  f: TPotFile;
begin
  if OpenDialog1.Execute then begin
    f := AddFile(OpenDialog1.FileName);
    SetCurrentFile(f);
  end;
end;

function TForm1.AddFile(AFileName: String): TPotFile;
var
  m: TMenuItem;
  i: Integer;
begin
  for i := 0 to lbPoFiles.Count - 1 do
    if TPotFile(lbPoFiles.Items.Objects[i]).FileName = AFileName then
      exit;

  Result := TPotFile.Create(AFileName);
  lbPoFiles.AddItem(Format('%s (%d)', [Result.Name, Result.Count]), Result);

  FPotList.Add(Result);

  m := TMenuItem.Create(popMoveTo);
  m.Caption := Result.Name;
  m.Tag := PtrInt(Result);
  m.OnClick := @OnMoveClicked;
  popMoveTo.Items.Add(m);
end;

procedure TForm1.OnMoveClicked(Sender: TObject);
var
  target: TPotFile;
  i, j, e: Integer;
  itm, newItm: TPotSection;
  po, targetPo: TPoFile;
  entry, newEntry: TPoSection;
  prefix: String;
begin
  target := TPotFile(TMenuItem(Sender).Tag);

  prefix := '';
  if target.Count > 1 then
    prefix := target.Items[1].VarNameUnit;
  if prefix = '' then
    prefix := LowerCase(target.Name);
  if not InputQuery('Move items', 'Enter the Unit-name used in the target file:', prefix) then
    exit;

  for i := 0 to lbPoItems.Count - 1 do begin
    if not lbPoItems.Selected[i] then
      continue;

    itm := TPotSection(lbPoItems.Items.Objects[i]);

    newItm := TPotSection.Create;
    newItm.Text := itm.Text;
    newItm.DeleteMsgCtxt;
    newItm.ReplaceUnitPrefix(prefix);
    target.Add(newItm);

    for j := 0 to FCurPotFile.PoFiles.Count - 1 do begin
      po := FCurPotFile.PoFiles[j];
      e := po.IndexOfVar(itm.VarName);
      if e < 0 then
        Continue;

      entry := po.Items[e];
      targetPo := target.PoFiles.PoFileForLang(po.LangName, True);
      newEntry := TPoSection.Create;
      newEntry.Text := entry.Text;
      newEntry.DeleteMsgCtxt;
      newEntry.ReplaceUnitPrefix(prefix);
      targetPo.Add(newEntry);

      po.Remove(entry);
    end;

    FCurPotFile.Remove(itm);
  end;

  FCurPotFile.SetModified;
  target.SetModified;
  UpdateSaveButton;
  UpdatePotItems;
  UpdatePoItems;
end;

procedure TForm1.SetCurrentFile(ACurPotFile: TPotFile);
var
  i: Integer;
begin
  FCurPotFile := ACurPotFile;

  i := lbPoFiles.Items.IndexOfObject(ACurPotFile);
  if i > 0 then
    lbPoFiles.ItemIndex := i;
  UpdatePoItems;

  UpdateSaveButton;

  for i := 0 to popMoveTo.Items.Count - 1 do
    popMoveTo.Items[i].Enabled := popMoveTo.Items[i].Tag <> PtrInt(FCurPotFile);
end;

procedure TForm1.UpdateSaveButton;
begin
  tbSave.Enabled := (FCurPotFile <> nil) and FCurPotFile.Modified;
  if FCurPotFile <> nil then
    tbSave.Caption := format('Save %s', [FCurPotFile.Name]);
end;

procedure TForm1.UpdatePoItems;
  function HasWord(AWord, AText: String): Boolean;
  var
    i: SizeInt;
  begin
    i := Pos(AWord, AText);
    Result := (i > 0) and
      ( (i = 1) or not (AText[i-1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) ) and
      ( (i + Length(AWord) > Length(AText)) or not (AText[i + Length(AWord)] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) )
      ;
  end;
var
  i, j, CntAll, CntNoneEmpty: Integer;
  itm: TPotSection;
  MatchWords, MatchTxtId, fnd, MatchIgnUName: Boolean;
  t: String;
  FindTerms: TStringArray;
begin
  Timer1.Enabled := False;
  if tbShowPoFileInfo.Down then begin
    UpdatePoItemFiles;
    exit;
  end;

  lbPoItems.Clear;
  if FCurPotFile = nil then
    exit;

  // Start at 1, Item 0 is the header
  t := lowercase(edSearchPoItems.Text);
  FindTerms := t.Split(',');
  for j := Length(FindTerms) - 1 downto 0 do begin
    FindTerms[j] := Trim(FindTerms[j]);
    if FindTerms[j] = '' then
      Delete(FindTerms, j, 1);
  end;
  MatchWords := chkSearchPoItemFullWord.Checked;
  MatchIgnUName := chkSearchPoItemIgnoreUnit.Checked;
  MatchTxtId := chkSearchPoItemIdText.Checked;
  lbPoItems.Items.BeginUpdate;
  for i := 1 to FCurPotFile.Count - 1 do begin
    itm := FCurPotFile.Items[i];

    if Length(FindTerms) > 0 then begin
      fnd := False;
      for j := 0 to Length(FindTerms) - 1 do begin
        if MatchWords then begin
          if MatchIgnUName
          then fnd := HasWord(itm.VarNameIdent, FindTerms[j])
          else fnd := HasWord(itm.VarNameContent, FindTerms[j]);
        end
        else begin
          if MatchIgnUName
          then fnd := Pos(FindTerms[j], itm.VarNameIdent) > 0
          else fnd := Pos(FindTerms[j], itm.VarNameContent) > 0;
        end;
        if fnd then
          break;
        if MatchTxtId then begin
          if MatchWords
          then fnd := HasWord(itm.MsgIdCleanContent, FindTerms[j])
          else fnd := Pos(FindTerms[j], LowerCase(itm.MsgIdCleanContent)) > 0;
          if fnd then
            break;
        end;
      end;

      if not fnd then
        continue;
    end;

    //itm.GetTranslationyCount(CntAll, CntNoneEmpty);
    //lbPoItems.AddItem(Format('%s (%d / %d): %s', [itm.MsgIdContent, CntNoneEmpty, CntAll, itm.VarNameContent]), itm);
    lbPoItems.AddItem(Format('%s (%s)', [itm.MsgIdContent, itm.VarNameContent]), itm);
  end;
  lbPoItems.Items.EndUpdate;

  lbPoItemsClick(nil);
end;

procedure TForm1.UpdatePoItemFiles;
var
  i: Integer;
  po: TPoFile;
begin
  lbPoItems.Clear;
  if FCurPotFile = nil then
    exit;

  for i := 1 to FCurPotFile.PoFiles.Count - 1 do begin
    po := FCurPotFile.PoFiles[i];
    lbPoItems.AddItem(Format('%s (%d / %d)', [po.LangName, po.NoneEmptyCount, po.Count-1]), po);
  end;
end;

procedure TForm1.UpdatePotItems;
var
  i: Integer;
  pot: TPotFile;
begin
  for i := 0 to lbPoFiles.Count - 1 do begin
    pot := TPotFile(lbPoFiles.Items.Objects[i]);
    if pot.Modified
    then lbPoFiles.Items[i] := Format('* %s (%d)', [pot.Name, pot.Count - 1])
    else lbPoFiles.Items[i] := Format('%s (%d)', [pot.Name, pot.Count - 1]);
  end;
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
  FDupIdPotFile.Free;
  FPotList.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
var
  s: String;
  f: TPotFile;
begin
  f := nil;
  for s in FileNames do
    f := AddFile(s);
  SetCurrentFile(f);
end;

procedure TForm1.btnClearSearchPoItemClick(Sender: TObject);
begin
  if (edSearchPoItems.Text = '') and not Timer1.Enabled then
    exit;
  edSearchPoItems.Clear;
  if tbShowMsgIdEntries.Down then
    UpdatePoItems;
end;

procedure TForm1.chkSearchPoItemFullWordChange(Sender: TObject);
begin
  if (edSearchPoItems.Text = '') and not Timer1.Enabled then
    exit;
  if tbShowMsgIdEntries.Down then
    UpdatePoItems;
end;

procedure TForm1.edSearchPoItemsChange(Sender: TObject);
begin
  if tbShowPoFileInfo.Down then
    exit;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.edSearchPoItemsEditingDone(Sender: TObject);
begin
  if tbShowMsgIdEntries.Down then
    UpdatePoItems;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPotList := TPotFileList.Create;
end;

procedure TForm1.lbPoFilesClick(Sender: TObject);
var
  i: Integer;
begin
  if FInbPoFilesClick then
    exit;

  i := lbPoFiles.ItemIndex;
  if i < 0 then
    exit;
  FInbPoFilesClick := True;
  SetCurrentFile(TPotFile(lbPoFiles.Items.Objects[i]));
  FInbPoFilesClick := False;
end;

procedure TForm1.lbPoItemsClick(Sender: TObject);
var
  i, j, cnt: Integer;
  itm: TPotSection;
  s: String;
  po: TPoFile;
  e: LongInt;
begin
  if not tbShowMsgIdEntries.Down then
    exit;
  memInfo.Clear;
  cnt := 0;
  for i := 0 to lbPoItems.Count - 1 do begin
    if not lbPoItems.Selected[i] then
      continue;

    inc(cnt);
    if not (lbPoItems.Items.Objects[i] is TPotSection) then
      continue;
    itm := TPotSection(lbPoItems.Items.Objects[i]);
    //itmid := itm.MsgId;
    s := itm.VarName + LineEnding;
    s := s + itm.MsgId + LineEnding;
    s := s + itm.MsgStr + LineEnding;
    for j := 0 to FCurPotFile.PoFiles.Count - 1 do begin
      po := FCurPotFile.PoFiles[j];
      e := po.IndexOfVar(itm.VarName);
      if e >= 0 then begin
        s := s + po.LangName + ': ' + po.Items[e].MsgStr + LineEnding;
      end;
    end;

    s := s + LineEnding;
    memInfo.Append(s);
  end;

  tbMovePoItems.Enabled := cnt > 0;
  tbMovePoItems.Caption := format('Move %d items to', [cnt]);
end;

procedure TForm1.tbDupIdClick(Sender: TObject);
var
  fl: TFindDupFlags;
begin
  if FDupIdPotFile = nil then begin
    FDupIdPotFile := TPotFile.Create('', True);
    lbPoFiles.AddItem('Dups', FDupIdPotFile);
  end;

  fl := [];
  if mnDupIgnCase.Checked then
    Include(fl, fdIgnoreCase);
  if mnDupIgnoreSpaceDiff.Checked then
    Include(fl, fdIgnoreSpaceDiff);

  FPotList.FindDuplicateMsgId(FDupIdPotFile, fl);

  UpdatePotItems;
  SetCurrentFile(FDupIdPotFile);
end;

procedure TForm1.tbSaveClick(Sender: TObject);
begin
  if FCurPotFile = nil then
    exit;

  FCurPotFile.Save;
  UpdatePotItems;
  tbSave.Enabled := False;
end;

procedure TForm1.tbShowMsgIdEntriesClick(Sender: TObject);
begin
  UpdatePoItems;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdatePoItems;
end;

end.

