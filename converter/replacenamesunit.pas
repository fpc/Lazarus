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
    fSeenName: TStringList;
  public
    constructor Create(AStringMap: TStringToStringTree);
    destructor Destroy; override;
    function FindReplacement(AIdent: string; out AReplacement: string): boolean;
  end;

  { TGridUpdater }

  TGridUpdater = class(TStringMapUpdater)
  private
    fGrid: TStringGrid;
    GridEndInd: Integer;
  public
    constructor Create(AStringMap: TStringToStringTree; AGrid: TStringGrid);
    destructor Destroy; override;
    procedure AddUnique(AOldIdent: string);
    procedure MapToGrid;
    procedure GridToMap;
  end;

  { TReplaceNamesForm }

  TReplaceNamesForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    InsertRow1: TMenuItem;
    DeleteRow1: TMenuItem;
    NamePairGrid: TStringGrid;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure InsertRow1Click(Sender: TObject);
    procedure DeleteRow1Click(Sender: TObject);
    procedure NamePairGridEditingDone(Sender: TObject);
    procedure NamePairGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
  private
    IsLasRow: Boolean;
  public

  end; 

var
  ReplaceNamesForm: TReplaceNamesForm;


implementation

{$R *.lfm}

{ TStringMapUpdater }

constructor TStringMapUpdater.Create(AStringMap: TStringToStringTree);
begin
  fStringMap:=AStringMap;
  fMapNames:=TStringList.Create;
  fStringMap.GetNames(fMapNames);
  fSeenName:=TStringList.Create;
end;

destructor TStringMapUpdater.Destroy;
begin
  fSeenName.Free;
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

constructor TGridUpdater.Create(AStringMap: TStringToStringTree; AGrid: TStringGrid);
begin
  inherited Create(AStringMap);
  fGrid:=AGrid;
  GridEndInd:=1;
end;

destructor TGridUpdater.Destroy;
begin
  inherited Destroy;
end;

procedure TGridUpdater.MapToGrid;
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  // Copy properties from NameReplacements to StringGrid.
  List:=TStringList.Create;
  try
    fGrid.BeginUpdate;
    fStringMap.GetNames(List);
    for i:=0 to List.Count-1 do begin
      OldIdent:=List[i];
      NewIdent:=fStringMap[OldIdent];
      if fGrid.RowCount<i+2 then
        fGrid.RowCount:=i+2;         // Leave one empty row to the end.
      fGrid.Cells[0,i]:=OldIdent;
      fGrid.Cells[1,i]:=NewIdent;
    end;
    fGrid.EndUpdate;
  finally
    List.Free;
  end;
end;

procedure TGridUpdater.GridToMap;
var
  OldIdent, NewIdent: string;
  i: Integer;
begin
  fStringMap.Clear;
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  for i:=1 to fGrid.RowCount-1 do begin // Skip the fixed row.
    OldIdent:=fGrid.Cells[0,i];
    NewIdent:=fGrid.Cells[1,i];
    if OldIdent<>'' then
      fStringMap[OldIdent]:=NewIdent;
  end;
end;

procedure TGridUpdater.AddUnique(AOldIdent: string);
// Add a new Delphi -> Lazarus mapping to grid.
var
  NewIdent: string;
begin
  // Add only one instance of each property name.
  if fSeenName.IndexOf(AOldIdent)<0 then begin
    fSeenName.Append(AOldIdent);
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
  Caption:=lisReplacementPropTypes;
  IsLasRow:=false;
end;

procedure TReplaceNamesForm.InsertRow1Click(Sender: TObject);
begin
  NamePairGrid.InsertColRow(False, NamePairGrid.Row);
end;

procedure TReplaceNamesForm.DeleteRow1Click(Sender: TObject);
begin
  NamePairGrid.DeleteColRow(False, NamePairGrid.Row);
end;

// Add rows automatically to the end of the grid
//  using OnSetEditText and OnEditingDone handlers and IsLasRow flag.
procedure TReplaceNamesForm.NamePairGridEditingDone(Sender: TObject);
var
  sg: TStringGrid;
begin
  if IsLasRow then begin
    sg:=Sender as TStringGrid;
    sg.RowCount:=sg.RowCount+1;
    IsLasRow:=false;
  end;
end;

procedure TReplaceNamesForm.NamePairGridSetEditText(Sender: TObject; ACol,
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

