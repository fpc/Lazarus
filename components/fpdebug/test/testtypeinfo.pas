unit TestTypeInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpPascalParser, FpDbgDwarf, FpDbgInfo, FpDbgLoader, FpPascalBuilder,
  FpDbgUtil, FpDbgDwarfConst, FileUtil, LazLoggerBase, LazUTF8, fpcunit, testutils,
  testregistry, TestHelperClasses;

const
  TESTPROG1_FUNC_BAR_LINE = 185;

type

  { TTestPascalExpression }

  TTestPascalExpression = class(TFpPascalExpression)
  private
    FContext: TDbgInfoAddressContext;
  protected
    function GetDbgSymbolForIdentifier(AnIdent: String): TDbgSymbol; override;
  public
    constructor Create(ATextExpression: String; AContext: TDbgInfoAddressContext);
  end;

  { TTestMemReader }

  TTestMemReader = class(TFpDbgMemReaderBase)
  public
    RegisterValues: array[0..30] of TDbgPtr;
    function ReadMemory(AnAddress: FpDbgInfo.TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx({%H-}AnAddress, {%H-}AnAddressSpace: FpDbgInfo.TDbgPtr; {%H-}ASize: Cardinal; {%H-}ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Integer; out AValue: FpDbgInfo.TDbgPtr): Boolean; override;
  end;


  { TTestTypInfo }

  TTestTypInfo = class(TTestCase)
  protected
    FDwarfInfo: TDbgDwarf;
  published
    Procedure New1;
    procedure X;
  end;

implementation

{ TTestMemReader }

function TTestMemReader.ReadMemory(AnAddress: FpDbgInfo.TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
begin
  Result := True;
  Move(Pointer(AnAddress)^, ADest^, ASize);
end;

function TTestMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: FpDbgInfo.TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Result := False;
end;

function TTestMemReader.ReadRegister(ARegNum: Integer; out AValue: FpDbgInfo.TDbgPtr): Boolean;
begin
  Result := True;
  AValue := RegisterValues[ARegNum];
end;

{ TTestPascalExpression }

function TTestPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TDbgSymbol;
begin
  Result := nil;
  if (AnIdent <> '') and (FContext <> nil) then
    Result := FContext.FindSymbol(AnIdent);
end;

constructor TTestPascalExpression.Create(ATextExpression: String;
  AContext: TDbgInfoAddressContext);
begin
  FContext := AContext;
  inherited Create(ATextExpression);
end;

procedure TTestTypInfo.X;
var
  s1, s2,s3: String;
begin
  s1 := '_vptr$TOBJECT';
  s2 := UTF8UpperCase( '_vptr$TOBJECT');
  s3 := UTF8LowerCase( '_vptr$TOBJECT');
  DebugLn (dbgs(  CompareUtf8BothCase(@s2[1],@s3[1],@s1[1]) ));
end;

procedure TTestTypInfo.New1;
var
  ImageLoader: TTestDummyImageLoader;
  SectionDbgInfo: TTestDummySectionInfoEntries;
  CompUnit, Prog1, Prog2,
  GlobVar1, TypeInt, TypeIntDecl: TTestDwarfInfoEntry;
  Ctx: TDbgInfoAddressContext;
  sym: TDbgSymbol;
  MemReader: TTestMemReader;

  TestStackFrame: record
    AVal: Integer; // -8
    AVal2: Integer; // -4
    EndPoint: Cardinal;
  end;
  Expression: TTestPascalExpression;
begin
  ImageLoader := TTestDummyImageLoader.Create;
  SectionDbgInfo := ImageLoader.TestImgReader.TestSection['.debug_info'] as TTestDummySectionInfoEntries;

  {%region}
  CompUnit := SectionDbgInfo.GetFirstInfoEntryObj;
  CompUnit.Tag := DW_TAG_compile_unit;
  CompUnit.Children := 1; //DW_CHILDREN_yes
  CompUnit.Add(DW_AT_name, DW_FORM_string, 'testprog1.pas'+#0);
  CompUnit.Add(DW_AT_producer, DW_FORM_string, 'Free Pascal 2.6.2 2013/02/16'+#0);
  CompUnit.Add(DW_AT_comp_dir, DW_FORM_string, 'B:/lazarus_latest/components/fpdebug/test/testapps/'+#0);
  CompUnit.Add(DW_AT_language, DW_FORM_data1, [$09]);
  CompUnit.Add(DW_AT_identifier_case, DW_FORM_data1, [$03]);
  CompUnit.Add(DW_AT_stmt_list, DW_FORM_data4, [$00, $00, $00, $00]);
  CompUnit.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  CompUnit.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00501A50);

  Prog1 := CompUnit.GetNewChild;
  Prog1.Tag := DW_TAG_subprogram;
  Prog1.Children := 1;
  Prog1.Add(DW_AT_name, DW_FORM_string, 'BAR'+#0);
  Prog1.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Prog1.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Prog1.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Prog1.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00401000);
  Prog1.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00402000);

  Prog2 := CompUnit.GetNewChild;
  Prog2.Tag := DW_TAG_subprogram;
  Prog2.Children := 0;
  Prog2.Add(DW_AT_name, DW_FORM_string, 'BAR2'+#0);
  Prog2.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Prog2.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Prog2.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Prog2.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00403000);
  Prog2.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00404000);


  TypeInt := CompUnit.GetNewChild;
  TypeInt.Tag := DW_TAG_base_type;
  TypeInt.Children := 0;
  TypeInt.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeInt.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeInt.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  TypeIntDecl := CompUnit.GetNewChild;
  TypeIntDecl.Tag := DW_TAG_typedef;
  TypeIntDecl.Children := 0;
  TypeIntDecl.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeIntDecl.AddRef(DW_AT_type, DW_FORM_ref4, TypeInt); // $7A, $06, $00, $00

  GlobVar1 := CompUnit.GetNewChild;
  GlobVar1.Tag := DW_TAG_variable;
  GlobVar1.Children := 0;
  GlobVar1.Add(DW_AT_name, DW_FORM_string, 'INT1'+#0);
  GlobVar1.Add(DW_AT_location, DW_FORM_block1, [$02, $75, $78]);  // DW_OP_breg5-8
  GlobVar1.AddRef(DW_AT_type, DW_FORM_ref4, TypeIntDecl);

  {%endregion}

  TestStackFrame.AVal := -299;


  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
  MemReader := TTestMemReader.Create;
  MemReader.RegisterValues[5] := TDbgPtr(@TestStackFrame.EndPoint);

  //////////////////////////////////////////////////////////

  FDwarfInfo := TDbgDwarf.Create(ImageLoader);
  FDwarfInfo.LoadCompilationUnits;
  FDwarfInfo.MemReader := MemReader;

  Ctx := FDwarfInfo.FindContext($00401010);
  AssertTrue('got ctx', Ctx <> nil);

  sym := Ctx.FindSymbol('Int1');
  AssertTrue('got sym',  sym <> nil);
  sym.ReleaseReference();

  Expression := TTestPascalExpression.Create('Int1', Ctx);
  AssertTrue(Expression.Valid);
  AssertTrue(Expression.ResultValue <> nil);
  AssertEquals(Expression.ResultValue.AsInteger, -299 );
  Expression.Free;

  Expression := TTestPascalExpression.Create('@Int1', Ctx);
  AssertTrue(Expression.Valid);
  AssertTrue(Expression.ResultValue <> nil);
  AssertEquals(Expression.ResultValue.AsInteger, PtrInt(@TestStackFrame.AVal));
  Expression.Free;

  Expression := TTestPascalExpression.Create('@Int1^', Ctx);
  AssertTrue(Expression.Valid);
  AssertTrue(Expression.ResultValue <> nil);
  AssertEquals(Expression.ResultValue.AsInteger, -299 );
  Expression.Free;

  Expression := TTestPascalExpression.Create('(@Int1)^', Ctx);
  AssertTrue(Expression.Valid);
  AssertTrue(Expression.ResultValue <> nil);
  AssertEquals(Expression.ResultValue.AsInteger, -299 );
  Expression.Free;


  Ctx.ReleaseReference;

  ///////////////////////////
  FDwarfInfo.Free;
  ImageLoader.Free;
  MemReader.Free;
end;



initialization

  RegisterTest(TTestTypInfo);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} )^.Enabled := True;
end.

