{$assertions on}

{ Test LazUtf8.UTF8WrapText with various tests.
  Uncomment Writeln lines to see the output,
  by default this code only makes assertions and the output will be empty
  on success.

  Build this like "fpc test_UTF8WrapText.lpr -Fu..".

  This test should pass on all systems (Windows and Linux tested),
  remember that Length(LineEnding) is OS-specific.
}

{$apptype CONSOLE}

uses LazUtf8;
var
  I: Integer;
begin
  //Writeln(UTF8WrapText('123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', LineEnding, [' ', #9], 40, 16));
  Assert(UTF8WrapText('123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', LineEnding, [' ', #9], 40, 16) =
    '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789');

  //Writeln(UTF8WrapText('Secondary config directory where Lazarus searches for config template files. Default is "C:\cygwin64\home\michalis\installed\fpclazarus\latest_2025_11_30_test_fpcupdeluxe_win\lazarus".', LineEnding, [' ', #9], 80, 16));
  Assert(UTF8WrapText('Secondary config directory where Lazarus searches for config template files. Default is "C:\cygwin64\home\michalis\installed\fpclazarus\latest_2025_11_30_test_fpcupdeluxe_win\lazarus".', LineEnding, [' ', #9], 80, 16) =
    'Secondary config directory where Lazarus searches for config ' + LineEnding +
    StringOfChar(' ', 16) + 'template files. Default is ' + LineEnding +
    StringOfChar(' ', 16) + '"C:\cygwin64\home\michalis\installed\fpclazarus\latest_2025_11_30_test_fpcupdeluxe_win\lazarus".');

  //Writeln(UTF8WrapText('blah 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', LineEnding, [' ', #9], 80, 16));
  Assert(UTF8WrapText('blah 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', LineEnding, [' ', #9], 80, 16) =
    'blah ' + LineEnding +
    StringOfChar(' ', 16) + '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789');

  for I := 1 to 200 do
  begin
    // Writeln(I);
    // Writeln(UTF8WrapText('blah ' + StringOfChar('a', I), LineEnding, [' ', #9], 80, 16));

    if I < 80 - 16 - Length('blah ') then
      { No line break in this case }
      Assert(UTF8WrapText('blah ' + StringOfChar('a', I), LineEnding, [' ', #9], 80, 16) =
        'blah ' + StringOfChar('a', I))
    else
      { One line break in this case, between blah and aaaa.... }
      Assert(UTF8WrapText('blah ' + StringOfChar('a', I), LineEnding, [' ', #9], 80, 16) =
        'blah ' + LineEnding +
        StringOfChar(' ', 16) + StringOfChar('a', I));
  end;
end.
