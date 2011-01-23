program WatchesPrg;
{$H-}

uses sysutils, variants;

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
  begin
    writeln(1);
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
