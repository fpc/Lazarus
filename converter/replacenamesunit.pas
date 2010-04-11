unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ExtCtrls, CodeToolsStructs, SynRegExpr, LazarusIDEStrConsts;

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
    NamePairGrid: TStringGrid;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

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
        Key:=fMapNames[i];       // fMapNames has extracted keys from fStringMap.
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
  fGrid.BeginUpdate;
end;

destructor TGridUpdater.Destroy;
begin
  fGrid.EndUpdate;
  inherited Destroy;
end;

procedure TGridUpdater.MapToGrid;
var
  OldIdent, NewIdent: string;
  List: TStringList;
  i: Integer;
begin
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  List:=TStringList.Create;
  try
    fGrid.BeginUpdate;
    fStringMap.GetNames(List);
    for i:=0 to List.Count-1 do begin
      OldIdent:=List[i];
      NewIdent:=fStringMap[OldIdent];
      if fGrid.RowCount<i+1 then
        fGrid.RowCount:=i+1;
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
  // Collect (maybe edited) properties from StringGrid to NameReplacements.
  for i:=1 to fGrid.RowCount-1 do begin // Skip the fixed row.
    OldIdent:=fGrid.Cells[0,i];
    NewIdent:=fGrid.Cells[1,i];
    if NewIdent<>'' then
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
end;

procedure TReplaceNamesForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;


end.

