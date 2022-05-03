{%identcomplincludekeywords:on}
unit                   {completion:+5=!do,!label,!repeat,!else,!inherited,!and,!not}
tcompletion_keywords   {completion:+21=!do,!label,!repeat,!else,!inherited,!and,!not}
;   {completion:+2=!do,!label,!repeat,!else,!inherited,!and,!not}

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}
{$ModeSwitch advancedrecords}

interface   {completion:+11=!do,!label,!repeat,!else,!inherited,!and,!not}

uses     {completion:+5=!do,!label,!repeat,!else,!inherited,!and,!not}
system   {completion:+7=!do,!label,!repeat,!else,!inherited,!and,!not}
;

const   {completion:+6=!do,!label,!repeat,!else,!inherited,!and,!not}
  MYCONST
   =    {completion:+2=!do,!label,!repeat,!else,!inherited,!and,!not}
   1    {completion:+2=!do,!label,!repeat,!else,!inherited,!and,!not}
   ;    {completion:+2=!do,!label,!repeat,!else,!inherited,!and,!not}
  MYCONST_B = 'test' ;   {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}

var    {completion:+4=!do,!label,!repeat,!else,!inherited,!and,!not}
  MyVar     {completion:+6=!do,!label,!repeat,!else,!inherited,!and,!not}
  :         {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  Integer   {completion:+8=!do,!label,!repeat,!else,!inherited,!and,!not}
  ;         {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  MyVar2: TObject ;   {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not,!case}
  // TODO: "case" in record
  MyVar3: record    {completion:+7=!do,!label,!repeat,!else,!inherited,!and,!not}
    a: Integer ;    {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
    a2              {completion:+3=!do,!label,!repeat,!else,!inherited,!and,!not}
    :               {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
    Integer         {completion:+8=!do,!label,!repeat,!else,!inherited,!and,!not}
    ;               {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
    case
    boolean   {completion:+8=!do,!label,!repeat,!else,!inherited,!and,!not} // TODO: "of"
    of
      true: (
        b: TObject;
      );
      false: (
        c: array [0..1] of integer;
      );
  end;

type
  TFoo    {completion:+5=!do,!label,!repeat,!else,!inherited,!and,!not}
  =       {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  class   {completion:+6=!do,!label,!repeat,!else,!inherited,!and,!not}
  ;

  TIntA =    {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not} // TODO: "array"
  array      {completion:+6=!do,!label,!repeat,!else,!inherited,!and,!not} // TODO: "of"
  of
  integer;

  TIntB = array
  [ 0..9 ]   {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not} // TODO: "of"
  of
  integer;

  TFoo = class   {completion:+6=!do,!label,!repeat,!else,!inherited,!and,!not}
  (          {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  TObject    {completion:+8=!do,!label,!repeat,!else,!inherited,!and,!not}
  )          {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
    a        {completion:+2=!do,!label,!repeat,!else,!inherited,!and,!not}
    :        {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
    Integer  {completion:+8=!do,!label,!repeat,!else,!inherited,!and,!not}
    ;        {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  private
    procedure     {completion:+10=!do,!label,!repeat,!else,!inherited,!and,!not}
    Bar           {completion:+5=!do,!label,!repeat,!else,!inherited,!and,!not}
    ;             {completion:+1=!do,!label,!repeat,!else,!inherited,!and,!not}
  end;

procedure SomeProc(val: Integer; var v2, v3: TIntA);
function f1: TObject;

implementation
var
  bVal: Boolean;
  iVal: integer;
  ia: TIntA;

procedure SomeProc(val: Integer);
begin  {completion:+6=!do,!label,repeat,!else,inherited,!and,!not}
  ;    {completion:+1=!do,!label,repeat,!else,inherited,!and,!not}
end;

procedure SomeProc(val: Integer; var v2, v3: TIntA);
begin

end;

function f1: TObject;
begin

end;

procedure TFoo.Bar;
begin

end;

begin
  if  {completion:+3=!and,not,inherited} // +3 test in space after if
    iVal  {completion:!and,!in,not,inherited;+1=!and,!in,!not,inherited;+5=and,in,!not,!inherited} // + 5 in spaces after iVal
end.

