unit ReplaceNamesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ExtCtrls, Menus, CodeToolsStructs, SynRegExpr,
  LazarusIDEStrConsts, ConverterTypes;

type

  { TCalledFuncInfo }

  TCalledFuncInfo = class
  // Used for function replacements.
  private
    function ParseIf(var StartPos: integer): boolean;
  public
    fFuncName: string;
    fReplClause: string;
    fReplFunc: string;
    fStartPos: Integer;
    fEndPos: Integer;
    fInclSemiColon: string;
    fParams: TStringList;
    constructor Create(aFuncName, aReplacement: string);
    destructor Destroy; override;
    procedure UpdateReplacement;
  end;


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
    function AddUnique(AOldIdent: string): string;
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
  Result:=true;
  List:=TStringList.Create;
  try
    AGrid.BeginUpdate;
    AMap.GetNames(List);
    for i:=1 to List.Count do begin  // Skip the fixed row in grid.
      OldIdent:=List[i-1];
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


{ TCalledFuncInfo }

constructor TCalledFuncInfo.Create(aFuncName, aReplacement: string);
begin
  fFuncName:=aFuncName;
  fReplClause:=aReplacement;
  fParams:=TStringList.Create;
end;

destructor TCalledFuncInfo.Destroy;
begin
  fParams.Free;
  inherited Destroy;
end;

function TCalledFuncInfo.ParseIf(var StartPos: integer): boolean;
// Parse a clause starting with "if" and set fReplFunc if the condition matches.
// Example:  'if $3 match ":/" then OpenURL($3); OpenDocument($3)'
// Return true if the condition matched.
var
  RE: TRegExpr;
  ParamPos: integer;
  Str, Param: String;
  Repl: String;

  procedure ReadWhiteSpace(NewStartPos: integer);
  begin
    StartPos:=NewStartPos;
    while (StartPos<=Length(fReplClause)) and (fReplClause[StartPos]=' ') do
      inc(StartPos);
  end;

  function ParseParamNum: integer;
  var
    EndPos: Integer;
    s: String;
  begin
    if fReplClause[StartPos]<>'$' then
      raise EDelphiConverterError.Create(Format('$ expected, %s found.', [fReplClause[StartPos]]));
    Inc(StartPos);      // Skip $
    EndPos:=StartPos;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos] in ['0'..'9']) do
      Inc(EndPos);
    s:=Copy(fReplClause, StartPos, EndPos-StartPos);
    Result:=StrToInt(s);
    ReadWhiteSpace(EndPos);
  end;

  procedure ParseString(aStr: string);
  var
    EndPos: Integer;
    s: String;
  begin
    EndPos:=StartPos;
    while (EndPos<=Length(fReplClause)) and
          (fReplClause[EndPos] in ['a'..'z','A'..'Z','_']) do
      Inc(EndPos);
    s:=Copy(fReplClause, StartPos, EndPos-StartPos);
    if s<>aStr then
      raise EDelphiConverterError.Create(Format('%s expected, %s found.', [aStr, s]));
    ReadWhiteSpace(EndPos);
  end;

  function ParseDoubleQuoted: string;
  var
    EndPos: Integer;
  begin
    if fReplClause[StartPos]<>'"' then
      raise EDelphiConverterError.Create(Format('" expected, %s found.', [fReplClause[StartPos]]));
    Inc(StartPos);      // Skip "
    EndPos:=StartPos;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos]<>'"') do
      inc(EndPos);
    Result:=Copy(fReplClause, StartPos, EndPos-StartPos);
    ReadWhiteSpace(EndPos+1);
  end;

  function GetReplacement: string;
  var
    EndPos: Integer;
  begin
    EndPos:=StartPos;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos]<>';') do
      inc(EndPos);
    Result:=Copy(fReplClause, StartPos, EndPos-StartPos);
    StartPos:=EndPos+1;    // Skip ';'
  end;

begin
  // "if " is already skipped when coming here.
  ReadWhiteSpace(StartPos);            // Possible space in the beginning.
  ParamPos:=ParseParamNum;
  ParseString('match');
  Str:=ParseDoubleQuoted;
  ParseString('then');
  Repl:=GetReplacement;

  Result:=False;
  if ParamPos<=fParams.Count then begin
    Param:=fParams[ParamPos-1];
    RE:=TRegExpr.Create;
    try
      RE.Expression:=Str;
      if RE.Exec(Param) then begin
        fReplFunc:=Repl;
        Result:=True;
      end;
    finally
      RE.Free;
    end;
  end;
end;

procedure TCalledFuncInfo.UpdateReplacement;
// Parse fReplClause and set fReplFunc, maybe conditionally based on parameters.
var
  StartPos, EndPos: Integer;
begin
  StartPos:=1;
  while true do begin // StartPos<=Length(fReplClause)
    // "If" condition can match or not. Continue if it didn't match.
    if Copy(fReplClause, StartPos, 3) = 'if ' then begin
      Inc(StartPos, 3);
      if ParseIf(StartPos) then
        Break;
    end
    else begin
      // Replacement without conditions. Copy it and stop.
      EndPos:=StartPos;
      while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos]<>';') do
        inc(EndPos);
      fReplFunc:=Copy(fReplClause, StartPos, EndPos-StartPos);
      Break;
    end;
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

function TGridUpdater.AddUnique(AOldIdent: string): string;
// Add a new Delphi -> Lazarus mapping to the grid.
// Returns the replacement string.
begin
  if fSeenNames.IndexOf(AOldIdent)<0 then begin
    // Add only one instance of each name.
    fSeenNames.Append(AOldIdent);
    FindReplacement(AOldIdent, Result);
    if fGrid.RowCount<GridEndInd+1 then
      fGrid.RowCount:=GridEndInd+1;
    fGrid.Cells[0,GridEndInd]:=AOldIdent;
    fGrid.Cells[1,GridEndInd]:=Result;
    Inc(GridEndInd);
  end;
end;


{ TReplaceNamesForm }

procedure TReplaceNamesForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacements;
  btnOK.Caption:=lisOk;
  HelpButton.Caption:=lisMenuHelp;
  btnCancel.Caption:=dlgCancel;
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

