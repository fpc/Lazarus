unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ExtCtrls, CodeToolsStructs, SynRegExpr, LazarusIDEStrConsts;

type

  { TGridUpdater }

  TGridUpdater = class
  private
    fGrid: TStringGrid;
    fReplaceMap: TStringToStringTree;
    fNameList: TStringList;  // Names (keys) in fReplaceMap.
    fSeenName: TStringList;
    GridEndInd: Integer;
    function FindReplacement(AIdent: string): string;
  public
    constructor Create(AGrid: TStringGrid; AReplaceMap: TStringToStringTree);
    destructor Destroy; override;
    procedure AddUnique(AOldIdent: string);
  end;

  { TReplaceNamesForm }

  TReplaceNamesForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    NamePairGrid: TStringGrid;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end; 

var
  ReplaceNamesForm: TReplaceNamesForm;

procedure CopyFromMapToGrid(AGrid: TStringGrid; AMap: TStringToStringTree);
procedure CopyFromGridToMap(AGrid: TStringGrid; AMap: TStringToStringTree);


implementation

{$R *.lfm}

procedure CopyFromMapToGrid(AGrid: TStringGrid; AMap: TStringToStringTree);
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  List:=TStringList.Create;
  try
    AGrid.BeginUpdate;
    AMap.GetNames(List);
    for i:=0 to List.Count-1 do begin
      OldIdent:=List[i];
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

procedure CopyFromGridToMap(AGrid: TStringGrid; AMap: TStringToStringTree);
var
  OldIdent, NewIdent: string;
  i: Integer;
begin
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  for i:=1 to AGrid.RowCount-1 do begin // Skip the fixed row.
    OldIdent:=AGrid.Cells[0,i];
    NewIdent:=AGrid.Cells[1,i];
    if NewIdent<>'' then
      AMap[OldIdent]:=NewIdent;
  end;
end;

{ TGridUpdater }

constructor TGridUpdater.Create(AGrid: TStringGrid; AReplaceMap: TStringToStringTree);
begin
  fGrid:=AGrid;
  fReplaceMap:=AReplaceMap;
  fNameList:=TStringList.Create;
  fReplaceMap.GetNames(fNameList);
  fSeenName:=TStringList.Create;
  GridEndInd:=1;
  fGrid.BeginUpdate;
end;

destructor TGridUpdater.Destroy;
begin
  fGrid.EndUpdate;
  fSeenName.Free;
  fNameList.Free;
  inherited Destroy;
end;

function TGridUpdater.FindReplacement(AIdent: string): string;
// Try to find a matching replacement using regular expression.
var
  RE: TRegExpr;
  i: Integer;
  s: string;
begin
  Result:='';
  RE:=TRegExpr.Create;
  try
    for i:=0 to fNameList.Count-1 do begin
      s:=fNameList[i];           // NameList has extracted keys from fReplaceMap.
      // If key contains '(' assume it is a regexp.
      if Pos('(', s)>0 then begin
        RE.Expression:=s;
        if RE.Exec(AIdent) then begin  // Match with regexp.
          Result:=RE.Substitute(fReplaceMap[s]);
          Break;
        end;
      end;
    end;
  finally
    RE.Free;
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
    NewIdent:=fReplaceMap[AOldIdent];
    if NewIdent='' then               // Not found by name, try regexp.
      NewIdent:=FindReplacement(AOldIdent);
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
end;

procedure TReplaceNamesForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

end.

