unit ReplaceFuncsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ButtonPanel, ComCtrls, Grids, CheckLst, Menus, StdCtrls, SynRegExpr,
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

  { TFuncsAndCategories }

  TFuncsAndCategories = class
  private
    // Delphi func names, objects property has TFuncReplacement items.
    fFuncs: TStringList;
    // Category names, objects property has boolean info Used/Not used.
    fCategories: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddFunc(aCategory, aDelphiFunc, aReplaceFunc, aPackage, aUnitName: string): integer;
    function FuncAtInd(Ind: integer): TFuncReplacement;
    function AddCategory(aCategory: string; aUsed: Boolean): integer;
    function CategoryIsUsed(Ind: integer): Boolean;
  public
    property Funcs: TStringList read fFuncs;
    property Categories: TStringList read fCategories;
  end;

  { TReplaceFuncsForm }

  TReplaceFuncsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    CategoryListBox: TCheckListBox;
    DeleteRow1: TMenuItem;
    Grid: TStringGrid;
    InsertRow1: TMenuItem;
    CategoriesLabel: TLabel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    procedure CategoryListBoxClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure InsertRow1Click(Sender: TObject);
    procedure DeleteRow1Click(Sender: TObject);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure GridEditingDone(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure OKButtonClick(Sender: TObject);
  private
    IsLasRow: Boolean;
  public
    function FromFuncListToUI(aFuncsAndCateg: TFuncsAndCategories): boolean;
    function FromUIToFuncList(aFuncsAndCateg: TFuncsAndCategories): boolean;
  end;


function EditFuncReplacements(aFuncsAndCateg: TFuncsAndCategories;
                              aTitle: string): TModalResult;


implementation

{$R *.lfm}

function EditFuncReplacements(aFuncsAndCateg: TFuncsAndCategories;
                              aTitle: string): TModalResult;
var
  RFForm: TReplaceFuncsForm;
begin
  RFForm:=TReplaceFuncsForm.Create(nil);
  try
    RFForm.Caption:=aTitle;
    RFForm.CategoriesLabel.Caption:=lisConvDelphiCategories;
    RFForm.Grid.Columns[0].Title.Caption:=lisCEOModeCategory;
    RFForm.Grid.Columns[1].Title.Caption:=lisConvDelphiFunc;
    RFForm.Grid.Columns[2].Title.Caption:=lisReplacement;
    RFForm.Grid.Columns[3].Title.Caption:=lisPackage;
    RFForm.Grid.Columns[4].Title.Caption:=lisUIDUnit;
    RFForm.FromFuncListToUI(aFuncsAndCateg);
    Result:=RFForm.ShowModal;
    if Result=mrOK then
      RFForm.FromUIToFuncList(aFuncsAndCateg);
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


{ TFuncsAndCategories }

constructor TFuncsAndCategories.Create;
begin
  inherited Create;
  fFuncs:=TStringList.Create;
  fFuncs.Sorted:=True;
  fFuncs.CaseSensitive:=False;
  fCategories:=TStringList.Create;
  fCategories.Sorted:=True;
  fCategories.Duplicates:=dupIgnore;
end;

destructor TFuncsAndCategories.Destroy;
begin
  fCategories.Free;
  fFuncs.Free;
  inherited Destroy;
end;

procedure TFuncsAndCategories.Clear;
var
  i: Integer;
begin
  for i := 0 to fFuncs.Count-1 do
    fFuncs.Objects[i].Free;
  fFuncs.Clear;
  fCategories.Clear;
end;

function TFuncsAndCategories.AddFunc(
      aCategory, aDelphiFunc, aReplaceFunc, aPackage, aUnitName: string): integer;
// This is called when settings are read or when user made changes in GUI.
// Returns index for the added func replacement, or -1 if not added (duplicate).
var
  FuncRepl: TFuncReplacement;
  x: integer;
begin
  Result:=-1;
  if (aDelphiFunc<>'') and not fFuncs.Find(aDelphiFunc, x) then begin
    FuncRepl:=TFuncReplacement.Create(aCategory,
                                  aDelphiFunc, aReplaceFunc, aPackage, aUnitName);
    Result:=fFuncs.AddObject(aDelphiFunc, FuncRepl);
  end;
end;

function TFuncsAndCategories.FuncAtInd(Ind: integer): TFuncReplacement;
begin
  Result:=nil;
  if fFuncs[Ind]<>'' then
    Result:=TFuncReplacement(fFuncs.Objects[Ind]);
end;

function TFuncsAndCategories.AddCategory(aCategory: string; aUsed: Boolean): integer;
var
  CategUsed: PtrInt;
begin
  CategUsed:=0;
  if aUsed then
    CategUsed:=1;
  Result:=fCategories.AddObject(aCategory, TObject(CategUsed));
end;

function TFuncsAndCategories.CategoryIsUsed(Ind: integer): Boolean;
var
  CategUsed: PtrInt;
begin
  CategUsed:=0;
  if fCategories[Ind]<>'' then
    CategUsed:=PtrInt(fCategories.Objects[Ind]);
  Result:=CategUsed=1;
end;


{ TReplaceFuncsForm }

procedure TReplaceFuncsForm.FormCreate(Sender: TObject);
begin
  Caption:=lisReplacementFuncs;
  ButtonPanel.OKButton.Caption := lisOk;
  ButtonPanel.HelpButton.Caption := lisMenuHelp;
  ButtonPanel.CancelButton.Caption := dlgCancel;
  IsLasRow:=false;
end;

procedure TReplaceFuncsForm.CategoryListBoxClickCheck(Sender: TObject);
begin
  Grid.Invalidate; // Update;
end;

procedure TReplaceFuncsForm.PopupMenu1Popup(Sender: TObject);
var
  ControlCoord, NewCell: TPoint;
begin
  ControlCoord:=Grid.ScreenToControl(PopupMenu1.PopupPoint);
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

procedure TReplaceFuncsForm.GridPrepareCanvas(sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
var
  SGrid: TStringGrid;
  Categ: string;
  i: integer;
begin
  SGrid:=Sender as TStringGrid;
  Categ:=SGrid.Cells[0, aRow];  // Column 0 = category.
  i:=CategoryListBox.Items.IndexOf(Categ);
  if (i<>-1) and not CategoryListBox.Checked[i] then
    SGrid.Canvas.Font.Color:= clGrayText;
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

function TReplaceFuncsForm.FromFuncListToUI(aFuncsAndCateg: TFuncsAndCategories): boolean;
// Copy strings from Map to Grid.
var
  FuncRepl: TFuncReplacement;
  i: Integer;
begin
  Result:=true;
  Grid.BeginUpdate;                          // Skip the fixed row in grid.
  for i:=1 to aFuncsAndCateg.fFuncs.Count do begin
    if Grid.RowCount<i+2 then
      Grid.RowCount:=i+2;                    // Leave one empty row to the end.
    FuncRepl:=TFuncReplacement(aFuncsAndCateg.fFuncs.Objects[i-1]);
    Grid.Cells[0,i]:=FuncRepl.fCategory;
    Grid.Cells[1,i]:=aFuncsAndCateg.fFuncs[i-1];      // Delphi function name
    Grid.Cells[2,i]:=FuncRepl.fReplClause;
    Grid.Cells[3,i]:=FuncRepl.PackageName;
    Grid.Cells[4,i]:=FuncRepl.fUnitName;
  end;
  for i:=0 to aFuncsAndCateg.fCategories.Count-1 do begin
    CategoryListBox.Items.Add(aFuncsAndCateg.fCategories[i]);
    CategoryListBox.Checked[i]:=aFuncsAndCateg.CategoryIsUsed(i);
  end;
  Grid.EndUpdate;
end;

function TReplaceFuncsForm.FromUIToFuncList(aFuncsAndCateg: TFuncsAndCategories): boolean;
var
  i: Integer;
begin
  Result:=true;
  aFuncsAndCateg.Clear;
  // Collect (maybe edited) properties from StringGrid to fStringMap.
  for i:=1 to Grid.RowCount-1 do     // Skip the fixed row.
    if Grid.Cells[1,i]<>'' then      // Delphi function name must have something.
      aFuncsAndCateg.AddFunc(Grid.Cells[0,i],
                     Grid.Cells[1,i],
                     Grid.Cells[2,i],
                     Grid.Cells[3,i],
                     Grid.Cells[4,i]);
  // Copy checked (used) categories.
  for i:=0 to CategoryListBox.Count-1 do
    aFuncsAndCateg.AddCategory(CategoryListBox.Items[i], CategoryListBox.Checked[i]);
end;


end.

