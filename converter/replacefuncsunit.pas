unit ReplaceFuncsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ButtonPanel, ComCtrls, Grids, CheckLst, Menus, SynRegExpr,
  LazarusIDEStrConsts, ConverterTypes;

type

  { TFuncReplacement }

  TFuncReplacement = class
  private
    // Defined in UI:
    fCategory: string;
    fFuncName: string;
    fReplClause: string;
    fPackageName: string;
    fUnitName: string;
    // Calculated for each actual replacement:
    fReplFunc: string;        // May be extracted from a conditional expression.
    fStartPos: Integer;       // Start and end positions of original func+params.
    fEndPos: Integer;
    fInclSemiColon: string;   // Ending semiColon is included in the replacement.
    fParams: TStringList;     // Parameters of the original function call.
    function ParseIf(var aStart: integer): boolean;
  public
    constructor Create(const aCategory, aFuncName, aReplacement, aPackageName, aUnitName: string);
    constructor Create(aFuncRepl: TFuncReplacement);
    destructor Destroy; override;
    procedure UpdateReplacement;
  public
    property Category: string read fCategory;
    property FuncName: string read fFuncName;
    property ReplClause: string read fReplClause;
    property ReplFunc: string read fReplFunc;       // The actual replacement.
    property PackageName: string read fPackageName;
    property UnitName: string read fUnitName;
    property StartPos: Integer read fStartPos write fStartPos;
    property EndPos: Integer read fEndPos write fEndPos;
    property InclSemiColon: string read fInclSemiColon write fInclSemiColon;
    property Params: TStringList read fParams;
  end;


  { TReplaceFuncsForm }

  TReplaceFuncsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    CheckListBox1: TCheckListBox;
    DeleteRow1: TMenuItem;
    Grid: TStringGrid;
    InsertRow1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure InsertRow1Click(Sender: TObject);
    procedure DeleteRow1Click(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure OKButtonClick(Sender: TObject);
  private
    IsLasRow: Boolean;
  public

  end; 

var
  ReplaceFuncsForm: TReplaceFuncsForm;

procedure ClearFuncList(aFuncs: TStringList);
procedure AddReplaceFunc(aFuncs: TStringList;
                 aCategory, aDelphiFunc, aReplaceFunc, aPackage, aUnitName: string);
function FromFuncListToUI(aFuncs: TStringList; aGrid: TStringGrid): boolean;
function FromUIToFuncList(aFuncs: TStringList; aGrid: TStringGrid): boolean;
function EditFuncReplacements(aFuncs: TStringList; aTitle: string): TModalResult;


implementation

{$R *.lfm}

procedure ClearFuncList(aFuncs: TStringList);
var
  i: Integer;
begin
  for i := 0 to aFuncs.Count-1 do
    aFuncs.Objects[i].Free;
  aFuncs.Clear;
end;

procedure AddReplaceFunc(aFuncs: TStringList;
                 aCategory, aDelphiFunc, aReplaceFunc, aPackage, aUnitName: string);
var
  FuncRepl: TFuncReplacement;
  x: integer;
begin
  if not aFuncs.Find(aDelphiFunc, x) then begin
    FuncRepl:=TFuncReplacement.Create(aCategory,
                                  aDelphiFunc, aReplaceFunc, aPackage, aUnitName);
    aFuncs.AddObject(aDelphiFunc, FuncRepl);
  end;
end;

function FromFuncListToUI(aFuncs: TStringList; aGrid: TStringGrid): boolean;
// Copy strings from Map to Grid.
var
  i: Integer;
  FuncRepl: TFuncReplacement;
begin
  Result:=true;
  aGrid.BeginUpdate;
  for i:=1 to aFuncs.Count do begin     // Skip the fixed row in grid.
    if aGrid.RowCount<i+2 then
      aGrid.RowCount:=i+2;              // Leave one empty row to the end.
    FuncRepl:=TFuncReplacement(aFuncs.Objects[i-1]);
    aGrid.Cells[0,i]:=FuncRepl.fCategory;
    aGrid.Cells[1,i]:=aFuncs[i-1];      // Delphi function name
    aGrid.Cells[2,i]:=FuncRepl.fReplClause;
    aGrid.Cells[3,i]:=FuncRepl.PackageName;
    aGrid.Cells[4,i]:=FuncRepl.fUnitName;
  end;
  aGrid.EndUpdate;
end;

function FromUIToFuncList(aFuncs: TStringList; aGrid: TStringGrid): boolean;
var
  i: Integer;
begin
  Result:=true;
  ClearFuncList(aFuncs);
  // Collect (maybe edited) properties from StringGrid to fStringMap.
  for i:=1 to aGrid.RowCount-1 do     // Skip the fixed row.
    if aGrid.Cells[1,i]<>'' then      // Delphi function name must have something.
      AddReplaceFunc(aFuncs, aGrid.Cells[0,i],
                             aGrid.Cells[1,i],
                             aGrid.Cells[2,i],
                             aGrid.Cells[3,i],
                             aGrid.Cells[4,i]);
end;

function EditFuncReplacements(aFuncs: TStringList; aTitle: string): TModalResult;
var
  RFForm: TReplaceFuncsForm;
begin
  RFForm:=TReplaceFuncsForm.Create(nil);
  try
    RFForm.Caption:=aTitle;
    FromFuncListToUI(aFuncs, RFForm.Grid);
    Result:=RFForm.ShowModal;
    if Result=mrOK then
      FromUIToFuncList(aFuncs, RFForm.Grid);
  finally
    RFForm.Free;
  end;
end;


{ TFuncReplacement }

constructor TFuncReplacement.Create(const aCategory,
                            aFuncName, aReplacement, aPackageName, aUnitName: string);
begin
  inherited Create;
  fCategory:=aCategory;
  fFuncName:=aFuncName;
  fReplClause:=aReplacement;
  fPackageName:=aPackageName;
  fUnitName:=aUnitName;
  fParams:=TStringList.Create;
end;

constructor TFuncReplacement.Create(aFuncRepl: TFuncReplacement);
// Copy constructor.
begin
  Create(aFuncRepl.fCategory,
         aFuncRepl.fFuncName,
         aFuncRepl.fReplClause,
         aFuncRepl.fPackageName,
         aFuncRepl.fUnitName);
end;

destructor TFuncReplacement.Destroy;
begin
  fParams.Free;
  inherited Destroy;
end;

function TFuncReplacement.ParseIf(var aStart: integer): boolean;
// Parse a clause starting with "if" and set fReplFunc if the condition matches.
// Example:  'if $3 match ":/" then OpenURL($3); OpenDocument($3)'
// Return true if the condition matched.

  procedure ReadWhiteSpace(NewStartPos: integer);
  begin
    aStart:=NewStartPos;
    while (aStart<=Length(fReplClause)) and (fReplClause[aStart]=' ') do
      inc(aStart);
  end;

  function ParseParamNum: integer;
  var
    EndPos: Integer;
    s: String;
  begin
    if fReplClause[aStart]<>'$' then
      raise EDelphiConverterError.Create(Format('$ expected, %s found.', [fReplClause[aStart]]));
    Inc(aStart);      // Skip $
    EndPos:=aStart;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos] in ['0'..'9']) do
      Inc(EndPos);
    s:=Copy(fReplClause, aStart, EndPos-aStart);
    Result:=StrToInt(s);
    ReadWhiteSpace(EndPos);
  end;

  procedure ParseString(aStr: string);
  var
    EndPos: Integer;
    s: String;
  begin
    EndPos:=aStart;
    while (EndPos<=Length(fReplClause)) and
          (fReplClause[EndPos] in ['a'..'z','A'..'Z','_']) do
      Inc(EndPos);
    s:=Copy(fReplClause, aStart, EndPos-aStart);
    if s<>aStr then
      raise EDelphiConverterError.Create(Format('%s expected, %s found.', [aStr, s]));
    ReadWhiteSpace(EndPos);
  end;

  function ParseDoubleQuoted: string;
  var
    EndPos: Integer;
  begin
    if fReplClause[aStart]<>'"' then
      raise EDelphiConverterError.Create(Format('" expected, %s found.', [fReplClause[aStart]]));
    Inc(aStart);      // Skip "
    EndPos:=aStart;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos]<>'"') do
      inc(EndPos);
    Result:=Copy(fReplClause, aStart, EndPos-aStart);
    ReadWhiteSpace(EndPos+1);
  end;

  function GetReplacement: string;
  var
    EndPos: Integer;
  begin
    EndPos:=aStart;
    while (EndPos<=Length(fReplClause)) and (fReplClause[EndPos]<>';') do
      inc(EndPos);
    Result:=Copy(fReplClause, aStart, EndPos-aStart);
    aStart:=EndPos+1;    // Skip ';'
  end;

var
  ParamPos: integer;
  RE: TRegExpr;
  Str, Param: String;
  Repl: String;
begin
  // "if " is already skipped when coming here.
  ReadWhiteSpace(aStart);            // Possible space in the beginning.
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

procedure TFuncReplacement.UpdateReplacement;
// Parse fReplClause and set fReplFunc, maybe conditionally based on parameters.
var
  xStart, xEnd: Integer;
begin
  xStart:=1;
  while true do begin // xStart<=Length(fReplClause)
    // "If" condition can match or not. Continue if it didn't match.
    if Copy(fReplClause, xStart, 3) = 'if ' then begin
      Inc(xStart, 3);
      if ParseIf(xStart) then
        Break;
    end
    else begin
      // Replacement without conditions. Copy it and stop.
      xEnd:=xStart;
      while (xEnd<=Length(fReplClause)) and (fReplClause[xEnd]<>';') do
        inc(xEnd);
      fReplFunc:=Copy(fReplClause, xStart, xEnd-xStart);
      Break;
    end;
  end;
end;


{ TReplaceFuncsForm }

procedure TReplaceFuncsForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacementFuncs;
  IsLasRow:=false;
end;

procedure TReplaceFuncsForm.PopupMenu1Popup(Sender: TObject);
var
  ControlCoord, NewCell: TPoint;
begin
  ControlCoord := Grid.ScreenToControl(PopupMenu1.PopupPoint);
  NewCell:=Grid.MouseToCell(ControlCoord);
  Grid.Col:=NewCell.X;
  Grid.Row:=NewCell.Y;
end;

procedure TReplaceFuncsForm.InsertRow1Click(Sender: TObject);
begin
  Grid.InsertColRow(False, Grid.Row);
end;

procedure TReplaceFuncsForm.DeleteRow1Click(Sender: TObject);
begin
  Grid.DeleteColRow(False, Grid.Row);
end;

// Add rows automatically to the end of the grid
//  using OnSetEditText and OnEditingDone handlers and IsLasRow flag.
procedure TReplaceFuncsForm.GridEditingDone(Sender: TObject);
var
  sg: TStringGrid;
begin
  if IsLasRow then begin
    sg:=Sender as TStringGrid;
    sg.RowCount:=sg.RowCount+1;
    IsLasRow:=false;
  end;
end;

procedure TReplaceFuncsForm.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if ARow = (Sender as TStringGrid).RowCount-1 then
    IsLasRow:=Value<>'';
end;

procedure TReplaceFuncsForm.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;


end.

