unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ExtCtrls, Menus, CodeToolsStructs, SynRegExpr,
  LazarusIDEStrConsts;

type

  { TStringMapUpdater }

  TStringMapUpdater = class
  private
    fStringMap: TStringToStringTree;
    fMapNames: TStringList;  // Names (keys) in fStringMap.
    fSeenNames: TStringList;
  public
    constructor Create(AStringsMap: TStringToStringTree);
    destructor Destroy; override;
    function FindReplacement(AIdent: string; out AReplacement: string): boolean;
  end;

  { TGridUpdater }

  TGridUpdater = class(TStringMapUpdater)
  private
    fGrid: TStringGrid;
    GridEndInd: Integer;
  public
    constructor Create(AStringsMap: TStringToStringTree; AGrid: TStringGrid);
    destructor Destroy; override;
    procedure AddUnique(AOldIdent: string);
  end;

  { TReplaceNamesForm }

  TReplaceNamesForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    InsertRow1: TMenuItem;
    DeleteRow1: TMenuItem;
    Grid: TStringGrid;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure InsertRow1Click(Sender: TObject);
    procedure DeleteRow1Click(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    IsLasRow: Boolean;
  public

  end; 

var
  ReplaceNamesForm: TReplaceNamesForm;


function FromMapToGrid(AMap: TStringToStringTree; AGrid: TStringGrid): boolean;
function FromGridToMap(AMap: TStringToStringTree; AGrid: TStringGrid;
                    AllowEmptyValues: boolean = true): boolean;
function EditMap(AMap: TStringToStringTree; ATitle: string): TModalResult;

implementation

{$R *.lfm}

function FromMapToGrid(AMap: TStringToStringTree; AGrid: TStringGrid): boolean;
// Copy strings from Map to Grid.
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  List:=TStringList.Create;
  try
    AGrid.BeginUpdate;
    AMap.GetNames(List);
    for i:=0 to List.Count-1 do begin
      OldIdent:=List[i];
      NewIdent:=AMap[OldIdent];
      if AGrid.RowCount<i+2 then
        AGrid.RowCount:=i+2;         // Leave one empty row to the end.
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

function EditMap(AMap: TStringToStringTree; ATitle: string): TModalResult;
var
  RNForm: TReplaceNamesForm;
begin
  RNForm:=TReplaceNamesForm.Create(nil);
  try
    RNForm.Caption:=ATitle;
    FromMapToGrid(AMap, RNForm.Grid);
    Result:=RNForm.ShowModal;
    if Result=mrOK then
      FromGridToMap(AMap, RNForm.Grid);
  finally
    RNForm.Free;
  end;
end;


{ TStringMapUpdater }

constructor TStringMapUpdater.Create(AStringsMap: TStringToStringTree);
begin
  fStringMap:=AStringsMap;
  fMapNames:=TStringList.Create;
  fStringMap.GetNames(fMapNames);
  fSeenNames:=TStringList.Create;
end;

destructor TStringMapUpdater.Destroy;
begin
  fSeenNames.Free;
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
begin
  if fStringMap.Contains(AIdent) then begin
    AReplacement:=fStringMap[AIdent];
    Result:=true;
  end
  else begin                     // Not found by name, try regexp.
    Result:=false;
    AReplacement:='';
    RE:=TRegExpr.Create;
    try
      for i:=0 to fMapNames.Count-1 do begin
        Key:=fMapNames[i];       // fMapNames has names extracted from fStringMap.
        // If key contains '(' assume it is a regexp.
        if Pos('(', Key)>0 then begin
          RE.Expression:=Key;
          if RE.Exec(AIdent) then begin  // Match with regexp.
            AReplacement:=RE.Substitute(fStringMap[Key]);
            Result:=true;
            Break;
          end;
        end;
      end;
    finally
      RE.Free;
    end;
  end;
end;


{ TGridUpdater }

constructor TGridUpdater.Create(AStringsMap: TStringToStringTree; AGrid: TStringGrid);
begin
  inherited Create(AStringsMap);
  fGrid:=AGrid;
  GridEndInd:=1;
end;

destructor TGridUpdater.Destroy;
begin
  inherited Destroy;
end;

procedure TGridUpdater.AddUnique(AOldIdent: string);
// Add a new Delphi -> Lazarus mapping to the grid.
var
  NewIdent: string;
begin
  // Add only one instance of each property name.
  if fSeenNames.IndexOf(AOldIdent)<0 then begin
    fSeenNames.Append(AOldIdent);
    FindReplacement(AOldIdent, NewIdent);
    if fGrid.RowCount<GridEndInd+1 then
      fGrid.RowCount:=GridEndInd+1;
    fGrid.Cells[0,GridEndInd]:=AOldIdent;
    fGrid.Cells[1,GridEndInd]:=NewIdent;
    Inc(GridEndInd);
  end;
end;


{ TReplaceNamesForm }

procedure TReplaceNamesForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacements;
  IsLasRow:=false;
end;

procedure TReplaceNamesForm.PopupMenu1Popup(Sender: TObject);
var
  ControlCoord, NewCell: TPoint;
begin
  ControlCoord := Grid.ScreenToControl(PopupMenu1.PopupPoint);
  NewCell:=Grid.MouseToCell(ControlCoord);
  Grid.Col:=NewCell.X;
  Grid.Row:=NewCell.Y;
end;

procedure TReplaceNamesForm.InsertRow1Click(Sender: TObject);
begin
  Grid.InsertColRow(False, Grid.Row);
end;

procedure TReplaceNamesForm.DeleteRow1Click(Sender: TObject);
begin
  Grid.DeleteColRow(False, Grid.Row);
end;

// Add rows automatically to the end of the grid
//  using OnSetEditText and OnEditingDone handlers and IsLasRow flag.
procedure TReplaceNamesForm.GridEditingDone(Sender: TObject);
var
  sg: TStringGrid;
begin
  if IsLasRow then begin
    sg:=Sender as TStringGrid;
    sg.RowCount:=sg.RowCount+1;
    IsLasRow:=false;
  end;
end;

procedure TReplaceNamesForm.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if ARow = (Sender as TStringGrid).RowCount-1 then
    IsLasRow:=Value<>'';
end;

procedure TReplaceNamesForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;


end.

