unit frmFileSearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Masks, LazUTF8, LazLoggerBase,
  LCLType, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls,
  FileCtrl, Buttons,
  LazIDEIntf, IDEWindowIntf,
  FileBrowserTypes, CtrlFileBrowser;

type

  { TFileSearcherForm }

  TFileSearcherForm = class(TForm)
    bpFileSearch: TButtonPanel;
    cbFilter: TFilterComboBox;
    edtSearch: TEdit;
    Label1: TLabel;
    lbStatus: TLabel;
    LBFiles: TListBox;
    SBConfigure: TSpeedButton;
    procedure cbFilterChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBFilesDblClick(Sender: TObject);
    procedure LBFilesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LBFilesSelectionChange(Sender: TObject; User: boolean);
    procedure SBConfigureClick(Sender: TObject);
  private
    FMask : TMaskList;
    FController: TFileBrowserController;
    FResults: TFileSearchResults;
    // Check whether search term is long enough
    function CheckLength: Boolean;
    // Disable listbox and show message in box
    procedure DisableListBox(const aMsg: String);
    // Actually filter list
    procedure DoFilter;
    // Callback when build of index is done
    procedure HandleIndexingDone(Sender: TObject);
  public
    // selected file items
    Function GetSelectedItems : TFileEntryArray;
  end;

var
  FileSearcherForm: TFileSearcherForm;

implementation

{$R *.lfm}

{ TFileSearcherForm }

procedure TFileSearcherForm.edtSearchChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TFileSearcherForm.DisableListBox(const aMsg : String);
begin
  FResults.Clear;
  LBFiles.Items.BeginUpdate;
  try
    LBFiles.Items.Clear;
    LBFiles.Items.Append('');
    LBFiles.Items.Append('  '+aMsg); // 2 spaces has nicer indent
    LBFiles.Enabled:=False;
  finally
    LBFiles.Items.EndUpdate;
  end;
end;

function TFileSearcherForm.CheckLength: Boolean;
begin
  //DebugLn(['"', edtSearch.Text, '": ', Length(edtSearch.Text)]);
  Result:=UTF8Length(edtSearch.Text) >= FController.MinSearchLen;
  if not Result then
    DisableListBox(Format(SWarnTermTooShort, [FController.MinSearchLen]));
end;

procedure TFileSearcherForm.DoFilter;
var
  lMatchOptions : TFilenameMatchOptions;
  Idx : Integer;
  lMatch : TFileSearchMatch;
begin
  if Not Assigned(FController) then
    begin
    DisableListBox(SWarnControllerNotAssigned);
    exit;
    end;
  //debugln(['TFileSearcherForm.DoFilter Calling CheckLength']);
  if not CheckLength then
    exit;

  FResults.Clear;
  lMatchOptions:=[];
  if (fsoMatchOnlyFileName in FController.SearchOptions) then
    Include(lMatchOptions,fmoFileNameOnly);
  if (fsoUseLetters in FController.SearchOptions) then
    Include(lMatchOptions,fmoLetters);
  //if (fsoMatchPartial in FController.SearchOptions) then
  //  Include(lMatchOptions,fmoMatchPartial);
  LBFiles.Items.BeginUpdate;
  try
    LBFiles.Items.Clear;
    FController.FindFiles(edtSearch.Text, FResults, lMatchOptions, FMask);
    for Idx:=0 to FResults.Count-1 do
      begin
      lMatch:=FResults[Idx];
      LBFiles.Items.AddObject(lMatch.FileName,lMatch);
      end;
  finally
    LBFiles.Items.EndUpdate;
  end;
  lbStatus.Caption:=Format('%d / %d matches', [FResults.Count, FController.FileCount]);;
  if LBFiles.Items.Count>0 then
    LBFiles.Enabled:=True
  else
    DisableListBox(SWarnNoMatch);
end;

procedure TFileSearcherForm.HandleIndexingDone(Sender: TObject);
begin
  edtSearch.Enabled:=True;
  //debugln(['TFileSearcherForm.HandleIndexingDone Calling CheckLength. edtSearch=', edtSearch.Text]);
  CheckLength;
end;

function TFileSearcherForm.GetSelectedItems: TFileEntryArray;
var
  Idx,I: Integer;
begin
  Result:=[];
  SetLength(Result,LBFiles.SelCount);
  idx:=0;
  For I:=0 to LBFiles.Count-1 do
    if LBFiles.Selected[I] then
      begin
      Result[Idx]:=TFileSearchMatch(LBFiles.Items.Objects[i]).Entry;
      Inc(Idx);
      end;
end;

procedure TFileSearcherForm.cbFilterChange(Sender: TObject);
begin
  FreeAndNil(FMask);
  if cBFilter.Text<>'' then
    FMask:=TMaskList.Create(cbFilter.Mask);
  DoFilter;
end;

procedure TFileSearcherForm.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,600,450);
  cbFilter.ItemIndex:=-1;  // Needed as a workaround for QTx bug #42053.
  cbFilter.ItemIndex:=0;
  if cbFilter.Mask<>'' then
    FMask:=TMaskList.Create(cbFilter.Mask);
  FResults:=TFileSearchResults.Create;
  FController:=LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  edtSearch.Hint:=Format('Search term, at least %d char', [FController.MinSearchLen]);
  edtSearch.TextHint:=edtSearch.Hint;
  bpFileSearch.OKButton.Enabled:=False;
  FController.AddOnIndexingFinishedEvent(@HandleIndexingDone);
  if FController.FillingTree then
  begin
    DisableListBox(SWarnBuildingIndex);
    edtSearch.Enabled:=False;
  end
  else begin
    //debugln(['TFileSearcherForm.FormCreate Calling CheckLength']);
    CheckLength;
  end;
 end;

procedure TFileSearcherForm.FormDestroy(Sender: TObject);
begin
  FController.RemoveOnIndexingFinishedEvent(@HandleIndexingDone);
  FreeAndNil(FMask);
  FreeAndNil(FResults);
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TFileSearcherForm.LBFilesDblClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TFileSearcherForm.LBFilesSelectionChange(Sender: TObject; User: boolean);
begin
  bpFileSearch.OKButton.Enabled:=(Sender as TListBox).ItemIndex>=0;
end;

procedure TFileSearcherForm.LBFilesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  TernW, StartW : Integer;
  FN, Term, Start : String;
  lRect : TRect;
  C : TColor;
  lCanvas : TCanvas;
  lMatch : TFileSearchMatch;
  lPositions : TMatchPositionArray;
  lPos : TMatchPosition;
begin
  lCanvas:=LBFiles.Canvas;
  if Index>=FResults.Count then
    begin
    lPositions:=[];
    if Index<LBFiles.Items.Count then
      FN:=LBFiles.Items[index]
    else
      FN:='';
    end
  else
    begin
    lMatch:=fResults[Index];
    FN:=lMatch.FileName;
    lPositions:=lMatch.MatchPositions;
    end;
  lRect:=aRect;
  if odSelected in State then begin
    lCanvas.Brush.Color:=clHighlight;
    lCanvas.FillRect(ARect);
    lCanvas.Brush.Color:=clBackground;
  end
  else begin
    c:=lCanvas.Brush.Color;
    lCanvas.Brush.Color:=clHighlight;
    for lPos in lPositions do
      begin
      Start:=Copy(FN, 1, lPos.Pos-1);
      Term:=Copy(FN, lPos.Pos, lPos.Len);
      StartW:=lCanvas.TextWidth(Start);
      TernW:=lCanvas.TextWidth(Term);
      lRect.Left:=aRect.Left+StartW;
      lRect.Right:=aRect.Left+StartW+TernW;
      if lrect.Right>aRect.Right then
        lrect.Right:=aRect.Right;
      lCanvas.FillRect(lRect);
      end;
    lCanvas.Brush.Color:=C;
  end;
  lCanvas.TextRect(aRect, aRect.Left, aRect.Top, FN);
end;

procedure TFileSearcherForm.SBConfigureClick(Sender: TObject);
begin
  FController.ShowConfig;
end;

end.

