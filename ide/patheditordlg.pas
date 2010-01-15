{  $Id$  }
{
 /***************************************************************************
                          patheditordlg.pp
                          ----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  FileUtil, ButtonPanel, MacroIntf,
  LazarusIDEStrConsts, EditorOptions;

type

  { TPathEditorDialog }

  TPathEditorDialog = class(TForm)
    AddTemplateButton: TBitBtn;
    BrowseButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    DeleteButton: TBitBtn;
    MoveDownButton: TBitBtn;
    MoveUpButton: TBitBtn;
    TemplatesListBox: TListBox;
    TemplateGroupBox: TGroupBox;
    PathGroupBox: TGroupBox;
    PathEdit: TSynEdit;
    BrowseDialog: TSelectDirectoryDialog;
    procedure AddTemplateButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure TemplatesListBoxDblClick(Sender: TObject);
  private
    FBaseDirectory: string;
    FEffectiveBaseDirectory: string;
    function GetPath: string;
    function GetTemplates: string;
    function PathToText(const APath: string): string;
    procedure SelectCurrentPath;
    procedure SetBaseDirectory(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure SetTemplates(const AValue: string);
    function TextToPath(const AText: string): string;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property EffectiveBaseDirectory: string read FEffectiveBaseDirectory;
    property Path: string read GetPath write SetPath;
    property Templates: string read GetTemplates write SetTemplates;
  end;
  
  TOnPathEditorExecuted = TNotifyEvent;

  TPathEditorButton = class(TButton)
  private
    FCurrentPathEditor: TPathEditorDialog;
    FOnExecuted: TOnPathEditorExecuted;
  protected
    procedure DoOnPathEditorExecuted;
  public
    procedure Click; override;
    property CurrentPathEditor: TPathEditorDialog read FCurrentPathEditor;
    property OnExecuted: TOnPathEditorExecuted
      read FOnExecuted write FOnExecuted;
  end;

function PathEditorDialog: TPathEditorDialog;


implementation

{$R *.lfm}

uses 
  Math;

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
  NewPath: String;
begin
  with BrowseDialog do begin
    Title:=lisPathEditSelectDirectory;
    if (not Execute) then exit;
    y:=PathEdit.CaretY;
    if y>PathEdit.Lines.Count then y:=PathEdit.Lines.Count;
    NewPath:=Filename;
    if (FEffectiveBaseDirectory<>'')
    and FilenameIsAbsolute(FEffectiveBaseDirectory) then
      NewPath:=CreateRelativePath(NewPath,FEffectiveBaseDirectory);
    PathEdit.Lines.Insert(y,Trim(NewPath));
    PathEdit.CaretY:=y+1;
  end;
  SelectCurrentPath;
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

procedure TPathEditorDialog.FormCreate(Sender: TObject);
begin
  Caption:=dlgDebugOptionsPathEditorDlgCaption;

  PathGroupBox.Caption:=lisPathEditSearchPaths;
  MoveUpButton.Caption:=lisPathEditMovePathUp;
  MoveDownButton.Caption:=lisPathEditMovePathDown;
  BrowseButton.Caption:=lisPathEditBrowse;
  DeleteButton.Caption:=dlgEdDelete;

  TemplateGroupBox.Caption:=lisPathEditPathTemplates;
  AddTemplateButton.Caption:=lisCodeTemplAdd;

  AddTemplateButton.LoadGlyphFromLazarusResource('laz_add');
  MoveUpButton.LoadGlyphFromLazarusResource('arrow_up');
  MoveDownButton.LoadGlyphFromLazarusResource('arrow_down');
  DeleteButton.LoadGlyphFromLazarusResource('laz_delete');

  PathEdit.Font.BeginUpdate;
  PathEdit.Font.Pitch := SynDefaultFontPitch;
  EditorOpts.ApplyFontSettingsTo(PathEdit);
  PathEdit.Font.EndUpdate;
  PathEdit.ExtraCharSpacing := EditorOpts.ExtraCharSpacing;
  PathEdit.ExtraLineSpacing := EditorOpts.ExtraLineSpacing;
end;

procedure TPathEditorDialog.FormResize(Sender: TObject);
var PathGroupBoxHeight: integer;
begin
  PathGroupBoxHeight:=((ClientHeight-70)*2) div 3;
  if PathGroupBoxHeight<10 then PathGroupBoxHeight:=10;

  PathGroupBox.Height:=PathGroupBoxHeight;

  SelectCurrentPath;
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

procedure TPathEditorDialog.TemplatesListBoxDblClick(Sender: TObject);
var i: integer;
begin
  i := TemplatesListBox.ItemIndex;
  if i>=0 then begin
    PathEdit.Lines.Add(TemplatesListBox.Items[i]);
    PathEdit.CaretY:=PathEdit.Lines.Count;
    SelectCurrentPath;
  end;
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
  TemplateGroupBox.Visible:=TemplatesListBox.Items.Count>0;
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

procedure TPathEditorDialog.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  FEffectiveBaseDirectory:=FBaseDirectory;
  IDEMacros.SubstituteMacros(FEffectiveBaseDirectory);
  BrowseDialog.InitialDir:=FEffectiveBaseDirectory;
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

