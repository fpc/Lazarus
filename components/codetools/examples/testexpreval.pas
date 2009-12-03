{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Demonstrating the TExpressionEvaluator, used for FPC conditionals.
}
program TestExprEval;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileProcs, ExprEval;
  
var
  e: TExpressionEvaluator;

procedure Test(Expr, ExpectedResult: string);
var
  OldResult, NewResult: String;
begin
  DebugLn(['Test Expression="',expr,'"']);
  OldResult:=e.EvalOld(Expr);
  NewResult:=e.Eval(Expr);
  DebugLn(['Test eval2 OldResult="',OldResult,'" NewResult="',NewResult,'"']);
  if NewResult<>ExpectedResult then
    DebugLn(['FATAL: Expression="',expr,'" ExpectedResult="',ExpectedResult,'" <> NewResult="',NewResult,'"']);
end;

begin
  e:=TExpressionEvaluator.Create;
  e.Variables['A']:='123';
  Test('defined A','1');
  Test('defined(A)','1');
  Test('undefined(A)','0');
  Test('not undefined(A)','1');
  Test('not defined A','0');
  Test('A or B','1');
  Test('defined(B)','0');
  Test('B or A','1');
  Test('defined(B) or defined(A)','1');
  Test('defined(B) or not defined(A)','0');
  Test('not defined(B) or not defined(A)','1');
  Test('not defined(B) or defined(A)','1');
  Test('1 << 2','4');
  Test('4 >> 1','2');
  Test('4 = ''4''','1');
  Test('(1+1)=2','1');
  Test('(1+(2+4))=7','1');
  Test('(1+2*3)=7','1');
  e.Free;
end.

