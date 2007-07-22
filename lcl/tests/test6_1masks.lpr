{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  LCL Test 6_1

  Mask creating and matching test.
}
program test6_1masks;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, Masks,
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestMask }

  TTestMask = class(TTestCase)
  private
    FS, FMask: String;
    procedure Test;
  protected
    procedure TestMask(const S, Mask: String; Result: Boolean);
    procedure TestMaskException(const S, Mask: String; AFail: Boolean);
  published
    procedure TestNil;
    procedure TestAnyText;
    procedure TestAnyChar;
    procedure TestCharSet;
    procedure TestMaskSyntax;
  end;

procedure TTestMask.Test;
begin
  MatchesMask(FS, FMask);
end;

procedure TTestMask.TestMask(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesMask(S, Mask));
end;

procedure TTestMask.TestMaskException(const S, Mask: String; AFail: Boolean);
begin
  FS := S;
  FMask := Mask;
  if AFail then
    AssertException('Invalid syntax: ' + S + ' match ' + Mask + ': ', EConvertError, @Test)
  else
    try
      Test;
    except
      Fail('Invalid syntax: ' + S + ' match ' + Mask);
    end;
end;

procedure TTestMask.TestMaskSyntax;
begin
  TestMaskException('', '', False);
  TestMaskException('', 'a', False);
  TestMaskException('', '?', False);
  TestMaskException('', '*', False);
  TestMaskException('', '[a]', False);
  TestMaskException('', '[a-b]', False);
  TestMaskException('', '[!a-b]', False);
  TestMaskException('', '[abc]', False);
  TestMaskException('', '[abc-fgh]', False);
  TestMaskException('', '[a------h]', False);
  TestMaskException('', '**', False);
  TestMaskException('', 'aa', False);
  TestMaskException('', 'a*', False);
  TestMaskException('', '*a', False);
  TestMaskException('', '*?', False);

  TestMaskException('', '[', True);
  TestMaskException('', '[a', True);
  TestMaskException('', '[]', True);
  TestMaskException('', '[!]', True);
  TestMaskException('', '[-]', True);
  TestMaskException('', '[a-]', True);
  TestMaskException('', '[-a]', True);
  TestMaskException('', '[--a]', True);
end;

procedure TTestMask.TestNil;
begin
  TestMask('', '', True);
  TestMask('', '*', True);
  TestMask('', '?', False);
  TestMask('', 'a', False);
  TestMask('', '[a]', False);
end;

procedure TTestMask.TestAnyText;
begin
  TestMask('abc', '*', True);
  TestMask('abc', 'a*', True);
  TestMask('abc', '*c', True);
  TestMask('abc', '*a*', True);
  TestMask('abc', '*b*', True);
  TestMask('abc', '*c*', True);
  TestMask('abc', 'a*c', True);
  TestMask('abc', '*bc', True);
  TestMask('abc', 'ab*', True);

  TestMask('abcde', '*', True);
  TestMask('abcde', 'a*e', True);
  TestMask('abcde', 'a*b*e', True);
  TestMask('abcde', 'a*d*e', True);
  TestMask('abcde', 'a*c*e', True);
  TestMask('abcde', 'a*b*e', True);

  TestMask('abc', '*b', False);
  TestMask('abc', 'b*', False);
  TestMask('abc', '*a', False);
  TestMask('abc', 'c*', False);
  TestMask('abc', 'ab*d', False);

  TestMask('abcde', 'a*d', False);
  TestMask('abcde', 'a*c*d', False);
  TestMask('abcde', 'b*d*e', False);
end;

procedure TTestMask.TestAnyChar;
begin
  TestMask('abc', '?bc', True);
  TestMask('abc', '?b?', True);
  TestMask('abc', '???', True);

  TestMask('abc', '?*?', True);
  TestMask('abc', '?*??', True);
  TestMask('abc', '?*?*?', True);

  TestMask('abc', 'a?', False);
  TestMask('abc', 'abc?', False);
  TestMask('abc', '?abc', False);
  TestMask('abc', '??*??', False);
  TestMask('abc', '?*?*??', False);
end;

procedure TTestMask.TestCharSet;
begin
  TestMask('c', '[c]', True);
  TestMask('c', '[!b]', True);
  TestMask('c', '[a-c]', True);
  TestMask('c', '[a-d]', True);
  TestMask('c', '[!a-b]', True);
  TestMask('c', '[abc]', True);

  TestMask('c', '[a]', False);
  TestMask('c', '[!c]', False);
  TestMask('c', '[a-b]', False);
  TestMask('c', '[abd]', False);
end;

begin
  RegisterTest(TTestMask);
  
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

