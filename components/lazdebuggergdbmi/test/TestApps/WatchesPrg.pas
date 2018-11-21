// Do not add/remove lines
// TestWatches.pas expects hardcoded lines for breakpoints

(* Struture
  program WatchesPrg;

  type
   {$DEFINE Global_Type}

  {$DEFINE Global_Implementation}

  procedure FooFunc(  {$DEFINE FooFunc_Param}  }
    type
      {$DEFINE FooFunc_LocalType}
    var
      {$DEFINE FooFunc_LocalVar}

    function SubFoo()():Integer;
      type
        {$DEFINE Sub_FooFunc_LocalType}
      var
        {$DEFINE Sub_FooFunc_LocalVar}
      begin
        {$DEFINE Sub_FooFunc_Body}
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
var i, j, k, l: integer;
type
{$DEFINE Global_Type}

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


{$UNDEF Global_Type}

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


    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_LocalType}

var
  (***  local var  ***)
  {$DEFINE FooFunc_LocalVar}
    { class/record/object }
    {$I WatchesPrgStruct.inc}
    { strings }
    {$I WatchesPrgString.inc}
    { simple }
    {$I WatchesPrgSimple.inc}
    { enum/set }
    {$I WatchesPrgEnum.inc}


    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_LocalVar}

  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  type
    (***  local type  ***)
    {$DEFINE Sub_FooFunc_LocalType}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}


      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
      DummySubFooType12345 = Integer;
    {$UNDEF Sub_FooFunc_LocalType}

  var
    (***  local var  ***)
    {$DEFINE Sub_FooFunc_LocalVar}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}


      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
      DummySubFooVar12345: Integer;
    {$UNDEF Sub_FooFunc_LocalVar}
  begin
    {$DEFINE Sub_FooFunc_Body}
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}


      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}
    {$UNDEF Sub_FooFunc_Body}
    Freemem(GetMem(1)); // nested break
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


    { variants }
    {$I WatchesPrgVariant.inc}
    { procedure/function/method }
    {$I WatchesPrgProc.inc}

  {$UNDEF FooFunc_Body}

  SubFoo(VarInt, ArgInt);
  // break on next line
  Freemem(GetMem(1));
  {$IFDEF WITH_SLEEP} sleep(50);sleep(50);sleep(50);sleep(50);sleep(50);sleep(50);sleep(50);sleep(50);sleep(50);sleep(50); {$ENDIF};
end;




procedure FooFuncArray(
  (***  parameter and var-param  ***)
  {$DEFINE FooFunc_Param}
    {$I WatchesPrgArray.inc}    { Array }
    Dummy: Integer
  {$UNDEF FooFunc_Param}
);
  (***  local var/type  ***)
  {$DEFINE FooFunc_Local}
    {$I WatchesPrgArray.inc}    { Array }
  {$UNDEF FooFunc_Local}
begin
  {$DEFINE FooFunc_Body}
    {$I WatchesPrgArray.inc}    { Array }
  {$UNDEF FooFunc_Body}

  // break on next line
  Freemem(GetMem(1));
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

  {$DEFINE Global_Call_FooFunc}
    FooFunc(
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}

      0
    );

    FooFuncArray(
      { Array }
      {$I WatchesPrgArray.inc}
      0
    );
  {$UNDEF Global_Call_FooFunc}

  {$DEFINE Global_Body_Extra}
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

  {$UNDEF Global_Body_Extra}




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

  {$DEFINE Global_Call_FooFunc}
    FooFunc(
      { class/record/object }
      {$I WatchesPrgStruct.inc}
      { strings }
      {$I WatchesPrgString.inc}
      { simple }
      {$I WatchesPrgSimple.inc}
      { enum/set }
      {$I WatchesPrgEnum.inc}
      { variants }
      {$I WatchesPrgVariant.inc}
      { procedure/function/method }
      {$I WatchesPrgProc.inc}

      0
    );

    FooFuncArray(
      { Array }
      {$I WatchesPrgArray.inc}
      0
    );
  {$UNDEF Global_Call_FooFunc}

  // not bother to free mem
end.
