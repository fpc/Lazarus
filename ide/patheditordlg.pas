{  $Id$  }
{
 /***************************************************************************
                          patheditordlg.pp
                          ----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Abstract:
   Defines the TPathEditorDialog, which is a form to edit search paths
 
}
unit PathEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit, Buttons, StdCtrls, Dialogs,
  LResources, FileUtil, LazarusIDEStrConsts;

type
  TPathEditorDialog = class(TForm)
    PathGroupBox: TGroupBox;
    PathEdit: TSynEdit;
    MoveUpButton: TButton;
    MoveDownButton: TButton;
    BrowseButton: TButton;
    DeleteButton: TButton;
    TemplateGroupBox: TGroupBox;
    TemplatesListBox: TListBox;
    AddTemplateButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    BrowseDialog: TSelectDirectoryDialog;
    procedure AddTemplateButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PathEditorDialogResize(Sender: TObject);
  private
    function GetPath: string;
    function GetTemplates: string;
    function PathToText(const APath: string): string;
    procedure SelectCurrentPath;
    procedure SetPath(const AValue: string);
    procedure SetTemplates(const AValue: string);
    procedure SetupComponents;
    function TextToPath(const AText: string): string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Path: string read GetPath write SetPath;
    property Templates: string read GetTemplates write SetTemplates;
  end;
  
  TOnPathEditorExecuted = TNotifyEvent;

  TPathEditorButton = class(TButton)
  private
    FCurrentPathEditor: TPathEditorDialog;
    FOnExecuted: TOnPathEditorExecuted;
  protected
    procedure Click; override;
    procedure DoOnPathEditorExecuted;
  public
    property CurrentPathEditor: TPathEditorDialog read FCurrentPathEditor;
    property OnExecuted: TOnPathEditorExecuted
      read FOnExecuted write FOnExecuted;
  end;

function PathEditorDialog: TPathEditorDialog;


implementation

uses Math;

var PathEditor: TPathEditorDialog;

function PathEditorDialog: TPathEditorDialog;
begin
  if PathEditor=nil then PathEditor:=TPathEditorDialog.Create(Application);
  Result:=PathEditor;
end;

{ TPathEditorDialog }

procedure TPathEditorDialog.AddTemplateButtonClick(Sender: TObject);
var i, y: integer;
begin
  y:=-1;
  for i:=0 to TemplatesListBox.Items.Count-1 do begin
    if TemplatesListBox.Selected[i] then begin
      PathEdit.Lines.Add(TemplatesListBox.Items[i]);
      y:=PathEdit.Lines.Count;
    end;
  end;
  if y>=1 then PathEdit.CaretY:=y;
  SelectCurrentPath;
end;

procedure TPathEditorDialog.BrowseButtonClick(Sender: TObject);
var y: integer;
begin
  if BrowseDialog=nil then begin
    BrowseDialog:=TSelectDirectoryDialog.Create(Self);
    BrowseDialog.Options := BrowseDialog.Options + [ofFileMustExist];
  end;
  with BrowseDialog do begin
    Title:=lisPathEditSelectDirectory;
    if (not Execute) then exit;
    y:=PathEdit.CaretY;
    if y>PathEdit.Lines.Count then y:=PathEdit.Lines.Count;
    PathEdit.Lines.Insert(y,Trim(Filename));
    PathEdit.CaretY:=y+1;
  end;
  SelectCurrentPath;
end;

procedure TPathEditorDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TPathEditorDialog.DeleteButtonClick(Sender: TObject);
var y: integer;
begin
  y:=PathEdit.CaretY-1;
  if (y>=0) and (y<PathEdit.Lines.Count) then begin
    PathEdit.Lines.Delete(y);
    SelectCurrentPath;
  end;
end;

procedure TPathEditorDialog.MoveDownButtonClick(Sender: TObject);
var y: integer;
begin
  y:=PathEdit.CaretY-1;
  if (y>=0) and (y<PathEdit.Lines.Count-1) then begin
    PathEdit.Lines.Move(y,y+1);
    PathEdit.CaretY:=y+2;
    SelectCurrentPath;
  end;
end;

procedure TPathEditorDialog.MoveUpButtonClick(Sender: TObject);
var y: integer;
begin
  y:=PathEdit.CaretY-1;
  if (y>0) and (y<PathEdit.Lines.Count) then begin
    PathEdit.Lines.Move(y,y-1);
    PathEdit.CaretY:=y;
    SelectCurrentPath;
  end;
end;

procedure TPathEditorDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TPathEditorDialog.PathEditorDialogResize(Sender: TObject);
var MaxX, MaxY, PathGroupBoxHeight: integer;
begin
  MaxX:=ClientWidth;
  MaxY:=ClientHeight;
  PathGroupBoxHeight:=((MaxY-70)*2) div 3;
  if PathGroupBoxHeight<10 then PathGroupBoxHeight:=10;

  // path groupbox
  PathGroupBox.SetBounds(6,6,MaxX-2*6,PathGroupBoxHeight);
  PathEdit.SetBounds(2,2,
                     Max(1,PathGroupBox.Width-10),Max(1,PathGroupBox.Height-57));
  MoveUpButton.SetBounds(8,PathEdit.Top+PathEdit.Height+8,
                         120,MoveUpButton.Height);
  MoveDownButton.SetBounds(MoveUpButton.Left+MoveUpButton.Width+8,
                           MoveUpButton.Top,
                           MoveUpButton.Width,MoveUpButton.Height);
  BrowseButton.SetBounds(MoveDownButton.Left+MoveDownButton.Width+8,
                         MoveDownButton.Top,
                         90,MoveDownButton.Height);
  DeleteButton.SetBounds(BrowseButton.Left+BrowseButton.Width+8,
                         BrowseButton.Top,
                         BrowseButton.Width,BrowseButton.Height);

  // template groupbox
  with TemplateGroupBox do begin
    Left:=PathGroupBox.Left;
    Top:=PathGroupBox.Top+PathGroupBox.Height+8;
    Width:=PathGroupBox.Width;
    Height:=Max(1,MaxY-50-Top);
  end;
  TemplatesListBox.SetBounds(2,2,Max(1,TemplateGroupBox.Width-10),
                             Max(1,TemplateGroupBox.Height-57));
  AddTemplateButton.SetBounds(8,TemplatesListBox.Top+TemplatesListBox.Height+8,
                              100,AddTemplateButton.Height);

  // buttons at bottom
  OkButton.SetBounds(20,Max(1,MaxY-35),100,OkButton.Height);
  CancelButton.SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,
                         OkButton.Width,OkButton.Height);
                         
  SelectCurrentPath;
end;

function TPathEditorDialog.GetPath: string;
begin
  Result:=TextToPath(PathEdit.Text);
end;

function TPathEditorDialog.GetTemplates: string;
begin
  Result:=TextToPath(TemplatesListBox.Items.Text);
end;

procedure TPathEditorDialog.SetPath(const AValue: string);
begin
  PathEdit.Text:=PathToText(AValue);
end;

procedure TPathEditorDialog.SetTemplates(const AValue: string);
var sl: TStringList;
  i: integer;
begin
  sl:=TStringList.Create;
  try
    sl.Text:=PathToText(AValue);
    with TemplatesListBox do begin
      Items.BeginUpdate;
      i:=0;
      while i<sl.Count do begin
        if Items.Count<=i then
          Items.Add(sl[i])
        else
          Items[i]:=sl[i];
        inc(i);
      end;
      while Items.Count>sl.Count do
        Items.Delete(Items.Count-1);
      Items.EndUpdate;
    end;
  finally
    sl.Free;
  end;
end;

procedure TPathEditorDialog.SetupComponents;
begin
  PathGroupBox:=TGroupBox.Create(Self);
  with PathGroupBox do begin
    Name:='PathGroupBox';
    Parent:=Self;
    Caption:=lisPathEditSearchPaths;
    Visible:=true;
  end;
  
  PathEdit:=TSynEdit.Create(Self);
  with PathEdit do begin
    Name:='PathEdit';
    Parent:=PathGroupBox;
    Options:=[eoBracketHighlight, eoHideRightMargin, eoDragDropEditing,
              eoHalfPageScroll, eoScrollByOneLess, eoScrollPastEol,
              eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces];
    Gutter.Visible:=false;
    Visible:=true;
  end;
  
  MoveUpButton:=TButton.Create(Self);
  with MoveUpButton do begin
    Name:='MoveUpButton';
    Parent:=PathGroupBox;
    Caption:=lisPathEditMovePathUp;
    OnClick:=@MoveUpButtonClick;
    Visible:=true;
  end;
  
  MoveDownButton:=TButton.Create(Self);
  with MoveDownButton do begin
    Name:='MoveDownButton';
    Parent:=PathGroupBox;
    Caption:=lisPathEditMovePathDown;
    OnClick:=@MoveDownButtonClick;
    Visible:=true;
  end;

  BrowseButton:=TButton.Create(Self);
  with BrowseButton do begin
    Name:='BrowseButton';
    Parent:=PathGroupBox;
    Caption:=lisPathEditBrowse;
    OnClick:=@BrowseButtonClick;
    Visible:=true;
  end;
  
  DeleteButton:=TButton.Create(Self);
  with DeleteButton do begin
    Name:='DeleteButton';
    Parent:=PathGroupBox;
    Caption:=dlgEdDelete;
    OnClick:=@DeleteButtonClick;
    Visible:=true;
  end;

  TemplateGroupBox:=TGroupBox.Create(Self);
  with TemplateGroupBox do begin
    Name:='TemplateGroupBox';
    Parent:=Self;
    Caption:=lisPathEditPathTemplates;
    Visible:=true;
  end;
  
  TemplatesListBox:=TListBox.Create(Self);
  with TemplatesListBox do begin
    Name:='TemplatesListBox';
    Parent:=TemplateGroupBox;
    MultiSelect:=true;
    Visible:=true;
  end;

  AddTemplateButton:=TButton.Create(Self);
  with AddTemplateButton do begin
    Name:='AddTemplateButton';
    Parent:=TemplateGroupBox;
    Caption:=lisCodeTemplAdd;
    OnClick:=@AddTemplateButtonClick;
    Visible:=true;
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Caption:=lisLazBuildOk;
    OnClick:=@OkButtonClick;
    Visible:=true;
  end;

  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Caption:=dlgCancel;
    OnClick:=@CancelButtonClick;
    Visible:=true;
  end;
end;

function TPathEditorDialog.TextToPath(const AText: string): string;
var i, j: integer;
  PathAsText: string;
begin
  PathAsText:=AText;
  Result:=PathAsText;
  // convert all line ends to semicolons, remove empty paths and trailing spaces
  i:=1;
  j:=1;
  while i<=length(PathAsText) do begin
    if PathAsText[i] in [#10,#13] then begin
      // new line -> new path
      inc(i);
      if (i<=length(PathAsText)) and (PathAsText[i] in [#10,#13])
      and (PathAsText[i]<>PathAsText[i-1]) then
        inc(i);
      // skip spaces at end of path
      while (j>1) and (Result[j-1]=' ') do
        dec(j);
      // skip empty paths
      if (j=1) or (Result[j-1]<>';') then begin
        Result[j]:=';';
        inc(j);
      end;
    end else if ord(PathAsText[i])<32 then begin
      // skip trailing spaces
      inc(i)
    end else if PathAsText[i]=' ' then begin
      // space -> skip spaces at beginning of path
      if (j>1) and (Result[j-1]<>';') then begin
        Result[j]:=PathAsText[i];
        inc(j);
      end;
      inc(i);
    end else begin
      // path char -> just copy
      Result[j]:=PathAsText[i];
      inc(j);
      inc(i);
    end;
  end;
  if (j>1) and (Result[j-1]=';') then dec(j);
  SetLength(Result,j-1);
end;

function TPathEditorDialog.PathToText(const APath: string): string;
var i: integer;
  NewPath: string;
begin
  NewPath:=APath;
  for i:=1 to length(NewPath) do
    if NewPath[i]=';' then NewPath[i]:=#13;
  Result:=NewPath;
end;

procedure TPathEditorDialog.SelectCurrentPath;
var y: integer;
begin
  y:=PathEdit.CaretY;
  if y>PathEdit.Lines.Count then exit;
  PathEdit.BlockBegin:=Point(0,y);
  PathEdit.BlockEnd:=Point(length(PathEdit.Lines[y-1])+1,y);
end;

constructor TPathEditorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=500;
    Height:=400;
    Position:=poScreenCenter;
    OnResize:=@PathEditorDialogResize;
  
    SetupComponents;
  end;
  PathEditorDialogResize(nil);
end;

destructor TPathEditorDialog.Destroy;
begin
  inherited Destroy;
end;

{ TPathEditorButton }

procedure TPathEditorButton.Click;
begin
  FCurrentPathEditor:=PathEditorDialog;
  try
    inherited Click;
    FCurrentPathEditor.ShowModal;
    DoOnPathEditorExecuted;
  finally
    FCurrentPathEditor:=nil;
  end;
end;

procedure TPathEditorButton.DoOnPathEditorExecuted;
begin
  if Assigned(OnExecuted) then OnExecuted(Self);
end;

end.

