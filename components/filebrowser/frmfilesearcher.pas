unit frmfilesearcher;

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

begin
  if Not Assigned(FController) or (Length(edtSearch.Text)<2) then
    exit;
  LBFiles.Items.BeginUpdate;
  try
    LBFiles.Items.Clear;
    FController.FindFiles(edtSearch.Text,LBFiles.Items,(fsoMatchOnlyFileName in FController.SearchOptions),FMask);
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
      Result[Idx]:=TFileEntry(LBFiles.Items.Objects[i]);
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
end;

procedure TFileSearcherForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMask);
end;

procedure TFileSearcherForm.LBFilesDblClick(Sender: TObject);
begin
  Modalresult:=mrOK;
end;

procedure TFileSearcherForm.LBFilesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

Var
  W,L,P : Integer;
  lRect : TRect;
  C : TColor;
  ls,S,Term : String;
  lCanvas : TCanvas;

begin
  lCanvas:=LBFiles.Canvas;
  S:=LBFiles.Items[Index];
  LS:=LowerCase(S);
  Term:=LowerCase(EdtSearch.Text);
  lRect:=aRect;
  if not (odSelected in State) then
    begin
    c:=lCanvas.Brush.Color;
    lCanvas.Brush.Color:=clHighlight;
    W:=lCanvas.TextWidth(Term);
    P:=Pos(Term,LS);
    While P<>0 do
      begin
      L:=lCanvas.TextWidth(Copy(S,1,P-1));
      lRect.Left:=aRect.Left+L;
      lRect.Right:=aRect.Left+L+W;
      if lrect.Right>aRect.Right then
        lrect.Right:=aRect.Right;
      lCanvas.FillRect(lRect);
      P:=Pos(term,LS,P+Length(term));
      end;
    lCanvas.Brush.Color:=C;
    end;
  lCanvas.TextRect(aRect,aRect.Left,aRect.Top,S);
end;

end.

