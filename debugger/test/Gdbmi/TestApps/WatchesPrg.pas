// Do not add/remove lines
// TestWatches.pas expects hardcoded lines for breakpoints

(* Struture
  program WatchesPrg;

  type
   {$DEFINE Global_Types}

  {$DEFINE Global_Implementation}

  procedure FooFunc(  {$DEFINE FooFunc_Param}  }
    type
      {$DEFINE FooFunc_LocalType}
    var
      {$DEFINE FooFunc_Local}

    function SubFoo()():Integer;
      type
        {$DEFINE SubFooFunc_LocalType}
      var
        {$DEFINE SubFooFunc_Local}
      begin
        {$DEFINE SubFooFunc_Body}
      end;

    begin
      {$DEFINE FooFunc_Body}
    end;

  var
    {$DEFINE Global_Var}

  begin
    {$DEFINE Global_Body}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
    {$DEFINE Global_Body_NIL}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
  end;

*)

program WatchesPrg;
{$H-}

uses sysutils, variants, Classes {$IFDEF USE_W1} , unitw1 {$ENDIF};

type
{$DEFINE Global_Types}

  { class/record/object }
  {$I WatchesPrgStruct.inc}
  { strings }
  {$I WatchesPrgString.inc}
  { simple }
  {$I WatchesPrgSimple.inc}
  { enum/set }
  {$I WatchesPrgEnum.inc}
  { Array }
  {$I WatchesPrgArray.inc}
  { variants }
  {$I WatchesPrgVariant.inc}
  { procedure/function/method }
  {$I WatchesPrgProc.inc}


{$UNDEF Global_Types}

{$DEFINE Global_Implementation}
  { class/record/object }
  {$I WatchesPrgStruct.inc}
  { strings }
  {$I WatchesPrgString.inc}
  { simple }
  {$I WatchesPrgSimple.inc}
  { enum/set }
  {$I WatchesPrgEnum.inc}
  { Array }
  {$I WatchesPrgArray.inc}
  { variants }
  {$I WatchesPrgVariant.inc}
  { procedure/function/method }
  {$I WatchesPrgProc.inc}

{$UNDEF Global_Implementation}


procedure FooFunc(
  (***  parameter and var-param  ***)
  {$DEFINE FooFunc_Param}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

    Dummy: Integer
  {$UNDEF FooFunc_Param}
);
type
  (***  local type  ***)
  {$DEFINE FooFunc_LocalType}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_LocalType}

var
  (***  local var  ***)
  {$DEFINE FooFunc_Local}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_Local}

  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  type
    (***  local type  ***)
    {$DEFINE SubFooFunc_LocalType}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { Array }
      {$I WatchesPrgArray.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
      DummySubFooType12345 = Integer;
    {$UNDEF SubFooFunc_LocalType}

  var
    (***  local var  ***)
    {$DEFINE SubFooFunc_Local}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { Array }
      {$I WatchesPrgArray.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
      DummySubFooVar12345: Integer;
    {$UNDEF SubFooFunc_Local}
  begin
    {$DEFINE SubFooFunc_Body}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { Array }
      {$I WatchesPrgArray.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
    {$UNDEF SubFooFunc_Body}
    writeln(1); // nested break
  end;

begin
  {$DEFINE FooFunc_Body}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_Body}

  SubFoo(VarInt, ArgInt);
  // break on next line
  writeln(1);
end;


var
  (***  global var (to feed var-param)-***)
  {$DEFINE Global_Var}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF Global_Var}

begin
  {$DEFINE Global_Body}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF Global_Body}

  FooFunc(
    {$DEFINE Global_Call_FooFunc}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { Array }
      {$I WatchesPrgArray.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}

      0
    {$UNDEF Global_Call_FooFunc}
  );


  // same with nil
  {$DEFINE Global_Body_NIL}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}
    { Array }
    {$I WatchesPrgArray.inc}
    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF Global_Body_NIL}

  FooFunc(
    {$DEFINE Global_Call_FooFunc}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { Array }
      {$I WatchesPrgArray.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}

      0
    {$UNDEF Global_Call_FooFunc}
  );

  // not bother to free mem
end.
