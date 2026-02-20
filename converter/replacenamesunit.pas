unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr, AVL_Tree,
  // LCL
  Forms, Controls, Dialogs, Grids, Menus, ButtonPanel,
  // LazUtils
  AvgLvlTree,
  // IDE, converter
  IdeIntfStrConsts, LazarusIDEStrConsts, ConverterTypes;

type

  { TStringMapUpdater }

  TStringMapUpdater = class
  private
    fStringToStringMap: TStringToStringTree;
    fMapNames: TStringList;  // Names (keys) in fStringToStringMap.
    fNoReplacementNames: TStringMap;
  public
    constructor Create(AStringToStringMap: TStringToStringTree);
    destructor Destroy; override;
    function FindReplacement(AIdent: string; out AReplacement: string): boolean;
  end;

  { TNameUpdater }

  TNameUpdater = class(TStringMapUpdater)
  public
    //constructor Create(AStringToStringMap: TStringToStringTree);
    //destructor Destroy; override;
  end;

  { TGridUpdater }

  TGridUpdater = class(TStringMapUpdater)
  private
    fGrid: TStringGrid;
    fSeenNames: TStringMap;
    fGridEndInd: Integer;
  public
    constructor Create(AStringToStringMap: TStringToStringTree; AGrid: TStringGrid);
    destructor Destroy; override;
    function AddUnique(AOldIdent: string): string;
  end;

  { TReplaceForm }

  TReplaceForm = class(TForm)
    ButtonPanel: TButtonPanel;
    InsertRow1: TMenuItem;
    DeleteRow1: TMenuItem;
    Grid: TStringGrid;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure InsertRow1Click(Sender: TObject);
    procedure DeleteRow1Click(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private

  public

  end; 


function FromMapToGrid(AMap: TStringToStringTree; AGrid: TStringGrid): boolean;
function FromGridToMap(AMap: TStringToStringTree; AGrid: TStringGrid;
                       AllowEmptyValues: boolean = true): boolean;
function EditMap(AMap: TStringToStringTree; AFormTitle: string): TModalResult;
function EditCoordOffsets(AOffs: TVisualOffsets; aTitle: string): TModalResult;

implementation

{$R *.lfm}

function FromMapToGrid(AMap: TStringToStringTree; AGrid: TStringGrid): boolean;
// Copy strings from Map to Grid.
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  Result:=true;
  List:=TStringList.Create;
  try
    AGrid.BeginUpdate;
    AMap.GetNames(List);
    for i:=1 to List.Count do begin  // Skip the fixed row in grid.
      OldIdent:=List[i-1];
      NewIdent:=AMap[OldIdent];
      if AGrid.RowCount<i+1 then
        AGrid.RowCount:=i+1;
      AGrid.Cells[0,i]:=OldIdent;
      AGrid.Cells[1,i]:=NewIdent;
    end;
    AGrid.EndUpdate;
  finally
    List.Free;
  end;
end;

function FromGridToMap(AMap: TStringToStringTree; AGrid: TStringGrid;
                       AllowEmptyValues: boolean = true): boolean;
var
  OldIdent, NewIdent: string;
  i: Integer;
begin
  Result:=true;
  AMap.Clear;
  // Collect (maybe edited) properties from StringGrid to fStringMap.
  for i:=1 to AGrid.RowCount-1 do begin // Skip the fixed row.
    OldIdent:=AGrid.Cells[0,i];
    NewIdent:=AGrid.Cells[1,i];
    if OldIdent<>'' then begin
      if AllowEmptyValues or (NewIdent<>'') then
        AMap[OldIdent]:=NewIdent;
    end;
  end;
end;

function EditMap(AMap: TStringToStringTree; AFormTitle: string): TModalResult;
var
  RNForm: TReplaceForm;
begin
  RNForm:=TReplaceForm.Create(nil);
  try
    RNForm.Caption:=AFormTitle;
    RNForm.Grid.Columns[0].Title.Caption:=lisConvDelphiName;
    RNForm.Grid.Columns[1].Title.Caption:=lisConvNewName;
    FromMapToGrid(AMap, RNForm.Grid);
    Result:=RNForm.ShowModal;
    if Result=mrOK then
      FromGridToMap(AMap, RNForm.Grid);
  finally
    RNForm.Free;
  end;
end;

// Functions for visual offsets values:

function FromListToGrid(AOffs: TVisualOffsets; AGrid: TStringGrid): boolean;
// Copy strings from coordinale list to grid.
var
  i: Integer;
begin
  Result:=true;
  AGrid.BeginUpdate;
  for i:=1 to AOffs.Count do begin  // Skip the fixed row in grid.
    if AGrid.RowCount<i+1 then
      AGrid.RowCount:=i+1;
    AGrid.Cells[0,i]:=AOffs[i-1].ParentType;
    AGrid.Cells[1,i]:=IntToStr(AOffs[i-1].Top);
    AGrid.Cells[2,i]:=IntToStr(AOffs[i-1].Left);
  end;
  AGrid.EndUpdate;
end;

function FromGridToList(AOffs: TVisualOffsets; AGrid: TStringGrid): boolean;
var
  ParentType: string;
  i, xTop, xLeft: Integer;
begin
  Result:=true;
  AOffs.Clear;
  // Collect (maybe edited) properties from StringGrid to fStringMap.
  for i:=1 to AGrid.RowCount-1 do begin // Skip the fixed row.
    ParentType:=AGrid.Cells[0,i];
    if ParentType<>'' then begin
      xTop:=0;
      try
        xTop:=StrToInt(AGrid.Cells[1,i]);
      except on EConvertError do
        ShowMessage('Top value must be a number. Now: '+AGrid.Cells[1,i]);
      end;
      xLeft:=0;
      try
        xLeft:=StrToInt(AGrid.Cells[2,i]);
      except on EConvertError do
        ShowMessage('Left value must be a number. Now: '+AGrid.Cells[2,i]);
      end;
      AOffs.Add(TVisualOffset.Create(ParentType, xTop, xLeft));
    end;
  end;
end;

function EditCoordOffsets(AOffs: TVisualOffsets; aTitle: string): TModalResult;
var
  xForm: TReplaceForm;
begin
  xForm:=TReplaceForm.Create(nil);
  try
    xForm.Caption:=aTitle;
    xForm.Grid.Columns[0].Title.Caption:=lisConvParentContainer;
    xForm.Grid.Columns[1].Title.Caption:=lisConvTopOff;
    xForm.Grid.Columns.Add.Title.Caption:=lisConvLeftOff;
    FromListToGrid(AOffs, xForm.Grid);
    Result:=xForm.ShowModal;
    if Result=mrOK then
      FromGridToList(AOffs, xForm.Grid);
  finally
    xForm.Free;
  end;
end;

procedure GetMapNames(Map: TStringToStringTree; List: TStrings);
var
  Node: TAvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=Map.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringMapItem(Node.Data);
    if EndsStr('\.(.+)', Item^.Name) then
      List.Insert(0, Item^.Name)  // Put RegExp for dotted unit names first.
    else
      List.Add(Item^.Name);
    Node:=Node.Successor;
  end;
end;

{ TStringMapUpdater }

constructor TStringMapUpdater.Create(AStringToStringMap: TStringToStringTree);
begin
  fStringToStringMap:=AStringToStringMap;
  fMapNames:=TStringList.Create;
  GetMapNames(fStringToStringMap, fMapNames);
  fNoReplacementNames:=TStringMap.Create(False);
end;

destructor TStringMapUpdater.Destroy;
begin
  fNoReplacementNames.Free;
  fMapNames.Free;
  inherited Destroy;
end;

function TStringMapUpdater.FindReplacement(AIdent: string;
                                           out AReplacement: string): boolean;
// Try to find a matching replacement using regular expression.
var
  RE: TRegExpr;
  i: Integer;
  Key: string;
  IsDotted: Boolean;
begin
  Result:=false;
  AReplacement:='';
  if fNoReplacementNames.Contains(AIdent) then  // Already tried, no replacement.
    exit;
  repeat
    IsDotted:=False;
    if fStringToStringMap.Contains(AIdent) then begin
      AReplacement:=fStringToStringMap[AIdent];
      Result:=true;
    end
    else begin                     // Not found by name, try RegExp.
      RE:=TRegExpr.Create;
      try
        RE.ModifierI:=True;
        for i:=0 to fMapNames.Count-1 do begin
          Key:=fMapNames[i]; // fMapNames has names extracted from fStringToStringMap.
          // If key contains special chars, assume it is a RegExp.
          if (Pos('(',Key)>0) or (Pos('*',Key)>0) or (Pos('+',Key)>0) then begin
            RE.Expression:=Key;
            if RE.Exec(AIdent) then begin // Match with RegExp.
              Result:=true;
              AReplacement:=RE.Substitute(fStringToStringMap[Key]);
              AIdent:=AReplacement;  // Continue with the replacement if first part
              IsDotted:=EndsStr('\.(.+)', Key);  // of dotted name was stripped.
              Break;
            end;
          end;
        end;
      finally
        RE.Free;
      end;
    end;
  until not IsDotted;
  if not Result then
    fNoReplacementNames.Add(AIdent);    // Means AIdent had no replacement.
end;

{ TNameUpdater }
{
constructor TNameUpdater.Create(AStringToStringMap: TStringToStringTree);
begin
  inherited;
end;

destructor TNameUpdater.Destroy;
begin
  inherited Destroy;
end;
}
{ TGridUpdater }

constructor TGridUpdater.Create(AStringToStringMap: TStringToStringTree; AGrid: TStringGrid);
begin
  inherited Create(AStringToStringMap);
  fGrid:=AGrid;
  fSeenNames:=TStringMap.Create(False);
  fGridEndInd:=1;
end;

destructor TGridUpdater.Destroy;
begin
  fSeenNames.Free;
  inherited Destroy;
end;

function TGridUpdater.AddUnique(AOldIdent: string): string;
// Add a new Delphi -> Lazarus mapping to the grid.
// Returns the replacement string.
begin
  if not fSeenNames.Contains(AOldIdent) then begin
    // Add only one instance of each name.
    fSeenNames.Add(AOldIdent);
    FindReplacement(AOldIdent, Result);
    if fGrid.RowCount<fGridEndInd+1 then
      fGrid.RowCount:=fGridEndInd+1;
    fGrid.Cells[0,fGridEndInd]:=AOldIdent;
    fGrid.Cells[1,fGridEndInd]:=Result;
    Inc(fGridEndInd);
  end
  else
    Result:='';
end;


{ TReplaceForm }

procedure TReplaceForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacements;
  ButtonPanel.OKButton.Caption := lisBtnOk;
  ButtonPanel.HelpButton.Caption := lisMenuHelp;
  ButtonPanel.CancelButton.Caption := lisCancel;
end;

procedure TReplaceForm.PopupMenu1Popup(Sender: TObject);
var
  ControlCoord, NewCell: TPoint;
begin
  ControlCoord := Grid.ScreenToControl(PopupMenu1.PopupPoint);
  NewCell:=Grid.MouseToCell(ControlCoord);
  Grid.Col:=NewCell.X;
  Grid.Row:=NewCell.Y;
end;

procedure TReplaceForm.InsertRow1Click(Sender: TObject);
begin
  Grid.InsertColRow(False, Grid.Row);
end;

procedure TReplaceForm.DeleteRow1Click(Sender: TObject);
begin
  Grid.DeleteColRow(False, Grid.Row);
end;

procedure TReplaceForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;


end.

