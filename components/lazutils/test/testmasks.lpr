{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the LazUtils                                        *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  LazUtils Test
  Mask creating and matching test.
}
program TestMasks;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  Interfaces, Forms,
  GuiTestRunner,
  Masks;

type

  { TTestMask }

  TTestMask = class(TTestCase)
  private
    FS, FMask: String;
    procedure Test;
    procedure TestMask(const S, Mask: String; Result: Boolean);
    procedure TestMaskDisableRange(const S, Mask: String; Result: Boolean);
    procedure TestMaskLegacy(const S, Mask: String; Result: Boolean);
    procedure TestMaskWindows(const S, Mask: String; Result: Boolean);
    procedure TestMaskException(const S, Mask: String; AFail: Boolean);
  published
    procedure TestMaskSyntax;
    procedure TestNil;
    procedure TestAnyText;
    procedure TestAnyChar;
    procedure TestCharSet;
    procedure TestDisableRange;
    procedure TestCase;
    procedure TestLegacy;
    procedure TestSpecial;
    procedure TestWindows;
  end;

procedure TTestMask.Test;
begin
  MatchesMask(FS, FMask);
end;

procedure TTestMask.TestMask(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesMask(S, Mask));
end;

procedure TTestMask.TestMaskDisableRange(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result,
               MatchesMask(S, Mask, False, MaskOpCodesDisableRange));
end;

procedure TTestMask.TestMaskLegacy(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesMaskLegacy(S, Mask));
end;

procedure TTestMask.TestMaskWindows(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesWindowsMask(S, Mask));
end;

procedure TTestMask.TestMaskException(const S, Mask: String; AFail: Boolean);
begin
  FS := S;
  FMask := Mask;
  if AFail then
    AssertException('Invalid syntax: ' + S + ' match ' + Mask + ': ', EMaskError, @Test)
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
  //TestMaskException('', '[!]', True);
  //TestMaskException('', '[-]', True);
  TestMaskException('', '[a-]', True);
  //TestMaskException('', '[-a]', True);
  //TestMaskException('', '[--a]', True);
end;

procedure TTestMask.TestNil;
begin
  TestMask('', '', True);
  TestMask('', '*', True);

  TestMask('', '?', False);
  TestMask('', 'a', False);
  TestMask('', '[a]', False);
  TestMask('', 'ä', False);
  TestMask('', '[ä]', False);
end;

procedure TTestMask.TestAnyText;
begin
  TestMask('abc', '*', True);              // ASCII
  TestMask('abc', 'a*', True);
  TestMask('abc', '*c', True);
  TestMask('abc', '*a*', True);
  TestMask('abc', '*b*', True);
  TestMask('abc', '*c*', True);
  TestMask('abc', 'a*c', True);
  TestMask('abc', '*bc', True);
  TestMask('abc', 'ab*', True);

  TestMask('äöæ', '*', True);              // Unicode
  TestMask('äöæ', 'ä*', True);
  TestMask('äöæ', '*æ', True);
  TestMask('äöæ', '*ä*', True);
  TestMask('äöæ', '*ö*', True);
  TestMask('äöæ', '*æ*', True);
  TestMask('äöæ', 'ä*æ', True);
  TestMask('äöæ', '*öæ', True);
  TestMask('äöæ', 'äö*', True);

  TestMask('abcde', '*', True);            // ASCII
  TestMask('abcde', 'a*e', True);
  TestMask('abcde', 'a*b*e', True);
  TestMask('abcde', 'a*d*e', True);
  TestMask('abcde', 'a*c*e', True);
  TestMask('abcde', 'a*b*e', True);
  TestMask('abc.pas.bak', '*.bak', True);

  TestMask('äöæ獵豹☺', '*', True);         // Unicode
  TestMask('äöæ獵豹☺', 'ä*☺', True);
  TestMask('äöæ獵豹☺', 'ä*ö*☺', True);
  TestMask('äöæ獵豹☺', 'ä*獵豹*☺', True);
  TestMask('äöæ獵豹☺', 'ä*æ*☺', True);
  TestMask('äöæ獵豹☺', 'ä*ö*☺', True);

  TestMask('abc', '*b', False);            // ASCII
  TestMask('abc', 'b*', False);
  TestMask('abc', '*a', False);
  TestMask('abc', 'c*', False);
  TestMask('abc', 'ab*d', False);

  TestMask('äöæ', '*ö', False);            // Unicode
  TestMask('äöæ', 'ö*', False);
  TestMask('äöæ', '*ä', False);
  TestMask('äöæ', 'æ*', False);
  TestMask('äöæ', 'äö*ũ', False);

  TestMask('abcde', 'a*d', False);         // ASCII
  TestMask('abcde', 'a*c*d', False);
  TestMask('abcde', 'b*d*e', False);
  TestMask('abc.txt', '.*', False);
  TestMask('abc.txt', '*.', False);
  TestMask('abc', '*.', False);
  TestMask('abc.pas.bak', '*.pas', False);

  TestMask('äöæ獵豹☺', 'ä*獵豹', False);   // Unicode
  TestMask('äöæ獵豹☺', 'ä*æ*獵豹', False);
  TestMask('äöæ獵豹☺', 'ö*獵豹*☺', False);
end;

procedure TTestMask.TestAnyChar;
begin
  TestMask('abc', '?bc', True);            // ASCII
  TestMask('abc', '?b?', True);
  TestMask('abc', '???', True);

  TestMask('äöæ', '?öæ', True);            // Unicode
  TestMask('äöæ', '?ö?', True);
  TestMask('äöæ', '???', True);

  TestMask('abc', '?*?', True);            // ASCII
  TestMask('abc', '?*??', True);
  TestMask('abc', '?*?*?', True);

  TestMask('äöæ', '?*?', True);            // Unicode
  TestMask('äöæ', '?*??', True);
  TestMask('äöæ', '?*?*?', True);

  TestMask('abc', 'a?', False);            // ASCII
  TestMask('abc', 'abc?', False);
  TestMask('abc', '?abc', False);
  TestMask('abc', '??*??', False);
  TestMask('abc', '?*?*??', False);

  TestMask('äöæ', 'ä?', False);            // Unicode
  TestMask('äöæ', 'äöæ?', False);
  TestMask('äöæ', '?äöæ', False);
  TestMask('äöæ', '??*??', False);
  TestMask('äöæ', '?*?*??', False);
end;

procedure TTestMask.TestCharSet;
begin
  TestMask('c', '[c]', True);              // ASCII
  TestMask('c', '[!b]', True);
  TestMask('c', '[a-c]', True);
  TestMask('c', '[a-d]', True);
  TestMask('c', '[d-a]', True);  // Reverse range
  TestMask('c', '[!a-b]', True);
  TestMask('c', '[abc]', True);

  TestMask('ö', '[ö]', True);              // Unicode
  TestMask('ö', '[!ä]', True);
  TestMask('ö', '[ä-ũ]', True);
  TestMask('է', '[ՠ-կ]', True);
  TestMask('ö', '[!☺-☂]', True);
  TestMask('ö', '[äũö]', True);

  TestMask('c', '[a]', False);             // ASCII
  TestMask('c', '[!c]', False);
  TestMask('c', '[a-b]', False);
  TestMask('c', '[z-d]', False);  // Reverse range
  TestMask('c', '[abd]', False);

  TestMask('ö', '[ä]', False);             // Unicode
  TestMask('ö', '[!ö]', False);
  TestMask('ö', '[ՠ-կ]', False);
  TestMask('ö', '[äũæ]', False);
end;

procedure TTestMask.TestDisableRange;
begin
  TestMaskDisableRange('a[b]c', 'a[b]c', True); // [] is now literal.
  // Wildcard syntax should still work.
  TestMaskDisableRange('a[b]c', '?[b]?', True);
  TestMaskDisableRange('abc', 'a*', True);
  TestMaskDisableRange('abc', '?b?', True);

  TestMaskDisableRange('abc', '?[b]?', False);
  TestMaskDisableRange('c', '[c]', False);
end;

procedure TTestMask.TestCase;
begin
  TestMask('aBc', '?b?', True);
  TestMask('äÖæ', 'Äö?', True);
  TestMask('abcÖ', '*[äũö]', True);
end;

procedure TTestMask.TestLegacy;
begin
  TestMaskLegacy('a?c', '?[?]?', True);
  TestMaskLegacy('C:\x', 'C:\x', True);

  TestMaskLegacy('a?c', '?\??', False);
  TestMaskLegacy('ab*.x', '??\*.x', False);
  TestMaskLegacy('x \ y', '? \\ ?', False);
  TestMaskLegacy('abc', '?[?]?', False);
  TestMaskLegacy('a??d', '?[?]?', False);
end;

procedure TTestMask.TestSpecial;
begin
  TestMask('a?c', '?[?]?', True);
  TestMask('abc', '?[?]?', True);
  TestMask('ac', '?[?]?', True);
  TestMask('a?c', '?\??', True);
  TestMask('ab*.x', '??\*.x', True);
  TestMask('a[c]d', '?\[*', True);
  TestMask('x \ y', '? \\ ?', True);
  TestMask('abcd', 'a[??]d', True);
  TestMask('abd', 'a[??]d', True);
  TestMask('ad', 'a[??]d', True);

  TestMask('C:\x', 'C:\x', False);
  TestMask('abcd', '?[?]?', False);
end;

procedure TTestMask.TestWindows;
begin
  TestMaskWindows('abc.txt', '*.*', True);
  TestMaskWindows('abc', '*.*', True);
  TestMaskWindows('abc.txt', '*', True);
  TestMaskWindows('abc', '*', True);
  TestMaskWindows('abc', '*.', True);
  TestMaskWindows('abcd.txt', 'abc???.*', True);
  TestMaskWindows('abcd.txt', 'abc???.txt?', True);
  TestMaskWindows('abcd.txt', 'abc*', True);
  TestMaskWindows('abc.pas.bak', '*.bak', True);
  TestMaskWindows('C:\x', 'C:\x', True);
  TestMaskWindows('C:\ab[c]d', 'C:*[*]*', True);
  TestMaskWindows('', '*', True);
  TestMaskWindows('', '?', True);

  TestMaskWindows('abcd.txt', '*.txtx', False);
  TestMaskWindows('abc.txt', '*.', False);
  TestMaskWindows('abc.txt', '.*', False);
  TestMaskWindows('abc.pas.bak', '*.pas', False);
  TestMaskWindows('abc', '.*', False);
  TestMaskWindows('x \ y', '? \\ ?', False);
  TestMaskWindows('', 'a', False);
  TestMaskWindows('', '[a]', False);
end;

begin
  RegisterTest(TTestMask);
  
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

