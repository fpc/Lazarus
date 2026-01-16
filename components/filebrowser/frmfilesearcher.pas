unit frmFileSearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls, FileCtrl, filebrowsertypes,
  ctrlfilebrowser, Types, Masks;

type

  { TFileSearcherForm }

  TFileSearcherForm = class(TForm)
    bpFileSearch: TButtonPanel;
    cbFilter: TFilterComboBox;
    edtSearch: TEdit;
    Label1: TLabel;
    LBFiles: TListBox;
    procedure cbFilterChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBFilesDblClick(Sender: TObject);
    procedure LBFilesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FMask : TMaskList;
    FController: TFileBrowserController;
    FResults: TFileSearchResults;

    procedure DoFilter;
  public
    Function GetSelectedItems : TFileEntryArray;
  end;

var
  FileSearcherForm: TFileSearcherForm;

implementation

uses LCLType, LazIDEIntf;

{$R *.lfm}

{ TFileSearcherForm }


procedure TFileSearcherForm.edtSearchChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TFileSearcherForm.DoFilter;

var
  lMatchOptions : TFilenameMatchOptions;
  Idx : Integer;
  lMatch : TFileSearchMatch;

begin
  if Not Assigned(FController) or (Length(edtSearch.Text)<2) then
    exit;
  lMatchOptions:=[];
  if (fsoMatchOnlyFileName in FController.SearchOptions) then
    Include(lMatchOptions,fmoFileNameOnly);
  if (fsoUseLetters in FController.SearchOptions) then
    Include(lMatchOptions,fmoLetters);
  FResults.Clear;
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
end;

procedure TFileSearcherForm.FormDestroy(Sender: TObject);
begin
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
  lPos : TMatchPosition;

begin
  lCanvas:=LBFiles.Canvas;
  lMatch:=fResults[Index];
  S:=lMatch.FileName;
  lRect:=aRect;
  if not (odSelected in State) then
    begin
    c:=lCanvas.Brush.Color;
    lCanvas.Brush.Color:=clHighlight;
    for lPos in lMatch.MatchPositions do
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

end.

