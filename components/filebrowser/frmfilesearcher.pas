unit frmFileSearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls, FileCtrl, Buttons, filebrowsertypes,
  ctrlfilebrowser,Types, Masks;

type

  { TFileSearcherForm }

  TFileSearcherForm = class(TForm)
    bpFileSearch: TButtonPanel;
    cbFilter: TFilterComboBox;
    edtSearch: TEdit;
    Label1: TLabel;
    LBFiles: TListBox;
    SBConfigure: TSpeedButton;
    procedure cbFilterChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBFilesDblClick(Sender: TObject);
    procedure LBFilesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure SBConfigureClick(Sender: TObject);
  private
    FMask : TMaskList;
    FController: TFileBrowserController;
    FOnConfigure: TNotifyEvent;
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

uses LCLType, LazIDEIntf;

{$R *.lfm}

resourcestring
  SWarnTermTooShort = 'Search term too short (min 2 characters)';
  SWarnControllerNotAssigned = 'Controller not assigned';
  SWarnBuildingIndex = 'Building file index, please wait';
  SWarnNoMatch = 'No files match your search term';
  SSearchTerm = 'Search term, must contain at least 2 characters';

{ TFileSearcherForm }


procedure TFileSearcherForm.edtSearchChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TFileSearcherForm.DisableListBox(const aMsg : String);

begin
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
  Result:=(Length(edtSearch.Text)>=2);
  if not Result then
    DisableListBox(SWarnTermTooShort);
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
  if not CheckLength then
    exit;

  FResults.Clear;
  lMatchOptions:=[];
  if (fsoMatchOnlyFileName in FController.SearchOptions) then
    Include(lMatchOptions,fmoFileNameOnly);
  if (fsoUseLetters in FController.SearchOptions) then
    Include(lMatchOptions,fmoLetters);
  LBFiles.Items.BeginUpdate;
  try
    LBFiles.Items.Clear;
    FController.FindFiles(edtSearch.Text,FResults,lMatchOptions,FMask);
    for Idx:=0 to FResults.Count-1 do
      begin
      lMatch:=FResults[Idx];
      LBFiles.Items.AddObject(lMatch.FileName,lMatch);
      end;
  finally
    LBFiles.Items.EndUpdate;
  end;
  if LBFiles.Items.Count>0 then
    LBFiles.Enabled:=True
  else
    DisableListBox(SWarnNoMatch);
end;

procedure TFileSearcherForm.HandleIndexingDone(Sender: TObject);
begin
  edtSearch.Enabled:=True;
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
  FController:=LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if cbFilter.Mask<>'' then
    FMask:=TMaskList.Create(cbFilter.Mask);
  FResults:=TFileSearchResults.Create;
  FController.AddOnIndexingFinishedEvent(@HandleIndexingDone);
  if FController.FillingTree then
    begin
    DisableListBox(SWarnBuildingIndex);
    edtSearch.Enabled:=False;
    end
  else
    CheckLength;
 end;

procedure TFileSearcherForm.FormDestroy(Sender: TObject);
begin
  FController.RemoveOnIndexingFinishedEvent(@HandleIndexingDone);
    FreeAndNil(FMask);
  FreeAndNil(FResults);
end;

procedure TFileSearcherForm.LBFilesDblClick(Sender: TObject);
begin
  Modalresult:=mrOK;
end;

procedure TFileSearcherForm.LBFilesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

Var
  W,L : Integer;
  lRect : TRect;
  C : TColor;
  S,Term : String;
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
      S:=LBFiles.Items[index]
    else
      S:='';
    end
  else
    begin
    lMatch:=fResults[Index];
    S:=lMatch.FileName;
    lPositions:=lMatch.MatchPositions;
    end;
  lRect:=aRect;
  if not (odSelected in State) then
    begin
    c:=lCanvas.Brush.Color;
    lCanvas.Brush.Color:=clHighlight;
    for lPos in lPositions do
      begin
      Term:=Copy(S,lPos.Pos,lPos.Len);
      W:=lCanvas.TextWidth(Term);
      L:=lCanvas.TextWidth(Copy(S,1,lPos.Pos-1));
      lRect.Left:=aRect.Left+L;
      lRect.Right:=aRect.Left+L+W;
      if lrect.Right>aRect.Right then
        lrect.Right:=aRect.Right;
      lCanvas.FillRect(lRect);
      end;
    lCanvas.Brush.Color:=C;
    end;
  lCanvas.TextRect(aRect,aRect.Left,aRect.Top,S);
end;

procedure TFileSearcherForm.SBConfigureClick(Sender: TObject);
begin
  FController.ShowConfig;
end;

end.

