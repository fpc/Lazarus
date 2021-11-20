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
    procedure TestMaskAdvanced(const S, Mask: String; Result: Boolean);
    procedure TestMaskWindows(const S, Mask: String; Result: Boolean);
    procedure TestMaskWindowsNonDefaultQuirks(const S, Mask: String; Result: Boolean);
    procedure TestMaskException(const S, Mask: String; AFail: Boolean);
  published
    procedure TestMaskSyntax;
    procedure TestNil;
    procedure TestAnyText;
    procedure TestAnyChar;
    procedure TestCharSet;
    procedure TestDisableRange;
    procedure TestCase;
    procedure TestDefault;
    procedure TestAdvanced;
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

procedure TTestMask.TestMaskAdvanced(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result,
               MatchesMask(S, Mask, False, AllMaskOpCodes));
end;

procedure TTestMask.TestMaskWindows(const S, Mask: String; Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesWindowsMask(S, Mask));
end;

procedure TTestMask.TestMaskWindowsNonDefaultQuirks(const S, Mask: String;
  Result: Boolean);
begin
  AssertEquals(S + ' match ' + Mask + ': ', Result, MatchesWindowsMask(S, Mask, False, DefaultMaskOpCodes, [wqFilenameEnd,wqExtension3More,wqAllByExtension]));
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

procedure TTestMask.TestDefault;
begin
  TestMask('a?c', '?[?]?', True);
  TestMask('C:\x', 'C:\x', True);

  TestMask('a?c', '?\??', False);
  TestMask('ab*.x', '??\*.x', False);
  TestMask('x \ y', '? \\ ?', False);
  TestMask('abc', '?[?]?', False);
  TestMask('a??d', '?[?]?', False);
end;

procedure TTestMask.TestAdvanced;
begin
  TestMaskAdvanced('a?c', '?[?]?', True);
  TestMaskAdvanced('abc', '?[?]?', True);
  TestMaskAdvanced('ac', '?[?]?', True);
  TestMaskAdvanced('a?c', '?\??', True);
  TestMaskAdvanced('ab*.x', '??\*.x', True);
  TestMaskAdvanced('a[c]d', '?\[*', True);
  TestMaskAdvanced('x \ y', '? \\ ?', True);
  TestMaskAdvanced('abcd', 'a[??]d', True);
  TestMaskAdvanced('abd', 'a[??]d', True);
  TestMaskAdvanced('ad', 'a[??]d', True);

  TestMaskAdvanced('C:\x', 'C:\x', False);
  TestMaskAdvanced('abcd', '?[?]?', False);
end;

procedure TTestMask.TestWindows;
begin
  TestMaskWindows('abc.txt', '*.*', True);
  TestMaskWindows('abc', '*.*', True);
  TestMaskWindows('abc.txt', '*', True);
  TestMaskWindows('abc', '*', True);
  TestMaskWindows('abc', '*.', True);
  TestMaskWindows('abcd.txt', 'abc???.*', False);
  TestMaskWindows('abcd.txt', 'abc???.txt?', False);
  TestMaskWindowsNonDefaultQuirks('abcd.txt', 'abc???.*', True);
  TestMaskWindowsNonDefaultQuirks('abcd.txt', 'abc???.txt?', True);
  TestMaskWindows('abcd.txt', 'abc*', True);
  TestMaskWindows('abc.pas.bak', '*.bak', True);
  TestMaskWindows('C:\x', 'C:\x', True);
  TestMaskWindows('C:\ab[c]d', 'C:*[*]*', False);  //sets and ranges are enabled by default on TWindowsMask as well
  TestMaskWindows('', '*', True);
  TestMaskWindows('', '?', False);
  TestMaskWindowsNonDefaultQuirks('', '?', True); //requires wqFileNameEnd
  TestMaskWindows('abcd.txt', '*.txtx', False);
  TestMaskWindows('abc.txt', '*.', False);
  TestMaskWindows('abc.txt', '.*', False);
  TestMaskWindows('abc.pas.bak', '*.pas', False);
  TestMaskWindows('abc', '.*', False);
  TestMaskWindows('x \ y', '? \\ ?', False);
  TestMaskWindows('', 'a', False);
  TestMaskWindows('', '[a]', False);
  TestMaskWindows('foo','foo.*',True);
end;

{$R *.res}

begin
  RegisterTest(TTestMask);
  
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

