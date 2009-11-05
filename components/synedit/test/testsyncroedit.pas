unit TestSyncroEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, LCLProc, TestBase,
  SynPluginSyncroEdit;

type

  { TTestSyncroEdit }

  TTestSyncroEdit = class(TTestBase)
  protected
    procedure Dump(hsh: TSynPluginSyncroEditWordsHash);
  published
    procedure TestWordsHash;
  end;

implementation

procedure TTestSyncroEdit.Dump(hsh: TSynPluginSyncroEditWordsHash);
var
  he: TSynPluginSyncroEditWordsHashEntry;
  i, j: Integer;
begin
  debugln(['Dump ', hsh.HashSize]);
  for i := 0 to hsh.HashSize - 1 do begin
    he := hsh.HashEntry[i, 0];
    if he.Count > 0 then begin
      debugln(['hash for ', i,', ', 0, '   Cnt=', he.Count, ' hsh=', he.Hash, ' nxt=', he.Next, ' y=', he.LineIdx, ' x=', he.BytePos, ' ln=', he.Len]);
      j := 1;
      he := hsh.HashEntry[i, j];
      while he.Count > 0 do begin
        debugln(['  hash for ', i,', ', j, ' Cnt=', he.Count, ' hsh=', he.Hash, ' nxt=', he.Next, ' y=', he.LineIdx, ' x=', he.BytePos, ' ln=', he.Len]);
        inc(j);
        he := hsh.HashEntry[i, j];
      end;
    end;
  end;
end;

procedure TTestSyncroEdit.TestWordsHash;
var
  hsh: TSynPluginSyncroEditWordsHash;

  function Check(Msg, Wrd: String; Cnt: Integer): TSynPluginSyncroEditWordsHashEntry;
  begin
    Result:= hsh.GetWord(@Wrd[1], length(Wrd));
    AssertEquals(Msg + ' ' + Wrd + ' Cnt', Cnt, Result.Count);
    if Cnt > 0 then
      AssertEquals(Msg + ' ' + Wrd + ' Len', length(Wrd), Result.Len);
  end;
  procedure Add(Wrd: String; Y, X: Integer);
  begin
    hsh.AddWord(y, x, length(Wrd), @Wrd[1]);
    //Dump(hsh);
  end;
  procedure Del(Wrd: String);
  begin
    hsh.RemoveWord(length(Wrd), @Wrd[1]);
    //Dump(hsh);
  end;

var
  lwl: TSynPluginSyncroEditLowerLineCache;
  s: String;
  i: Integer;
begin
  lwl := TSynPluginSyncroEditLowerLineCache.Create;
  lwl.Lines := SynEdit.ViewedTextBuffer;
  try
    hsh := TSynPluginSyncroEditWordsHash.Create;
    hsh.LowerLines := lwl;

    SynEdit.Lines.Add('Test abc');
    SynEdit.Lines.Add('atdesktop before2 252'); // supposed to have the same hash on a 4096 table
    SynEdit.Lines.Add('mk_equal ecpageup'); // same big hash

    Add('test', 0, 1);  Check('one word', 'test', 1);
    Add('test', 0, 1);  Check('one word twice', 'test', 2);
    Del('test');        Check('one word one down again', 'test', 1);
    Del('test');        Check('one word gone', 'test', 0);

    s:= 'atdesktop before2 252';
    AssertEquals('clash for atdesktop before2', hsh.GetWordModHash(@s[1], 9), hsh.GetWordModHash(@s[11], 7));
    AssertEquals('clash for atdesktop 252',     hsh.GetWordModHash(@s[1], 9), hsh.GetWordModHash(@s[19], 3));
    (* repeat test, but with double entry*)
    // add word with bad pointer, so we can create a clash

    Add('atdesktop', 1, 1); s:='2W  1'; Check(s, 'atdesktop', 1); Check(s, 'before2', 0); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  2'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Del('atdesktop');       s:='2W  3'; Check(s, 'atdesktop', 0); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W  4'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W  5'; Check(s, 'atdesktop', 2); Check(s, 'before2', 1); Check(s, '252', 0);
    Del('atdesktop');       s:='2W  6'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  7'; Check(s, 'atdesktop', 1); Check(s, 'before2', 2); Check(s, '252', 0);
    Del('before2');         s:='2W  8'; Check(s, 'atdesktop', 1); Check(s, 'before2', 1); Check(s, '252', 0);
    Add('before2', 1, 11);  s:='2W  9'; Check(s, 'atdesktop', 1); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W 10'; Check(s, 'atdesktop', 2); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('atdesktop', 1, 1); s:='2W 11'; Check(s, 'atdesktop', 3); Check(s, 'before2', 2); Check(s, '252', 0);
    Add('252', 1, 19);      s:='2W 12'; Check(s, 'atdesktop', 3); Check(s, 'before2', 2); Check(s, '252', 1);
    Del('before2');         s:='2W 13'; Check(s, 'atdesktop', 3); Check(s, 'before2', 1); Check(s, '252', 1);
    Del('before2');         s:='2W 14'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 1);
    Del('before2');         s:='2W 15'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 1); // none to del
    Del('252');             s:='2W 16'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 0);
    Del('252');             s:='2W 17'; Check(s, 'atdesktop', 3); Check(s, 'before2', 0); Check(s, '252', 0); // none to del
    Del('atdesktop');       s:='2W 18'; Check(s, 'atdesktop', 2); Check(s, 'before2', 0); Check(s, '252', 0);
    hsh.Clear;              s:='2W 19'; Check(s, 'atdesktop', 0); Check(s, 'before2', 0); Check(s, '252', 0);

    Add('mk_equal', 2, 1);  s:='3W  1'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 0);
    Add('ecpageup', 2, 10); s:='3W  2';
    AssertEquals('same hash', Check(s, 'mk_equal', 1).Hash, Check(s, 'ecpageup', 1).Hash);

    Add('ecpageup', 2, 1);  s:='3W  3'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 2);
    Add('mk_equal', 2, 1);  s:='3W  4'; Check(s, 'mk_equal', 2); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  5'; Check(s, 'mk_equal', 1); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  6'; Check(s, 'mk_equal', 0); Check(s, 'ecpageup', 2);
    Del('mk_equal');        s:='3W  7'; Check(s, 'mk_equal', 0); Check(s, 'ecpageup', 2);
    hsh.Clear;

    // resize test
    for i := 0 to 5000 do begin
  //if i = 2200 then  Dump(hsh);
      SynEdit.Lines.Add(IntToStr(i));
      Add(inttostr(i), 3+i, 1);
      if i mod 7 = 0 then
        Add(inttostr(i), 3+i, 1);
    end;
    for i := 0 to 5000 do
      if i mod 7 = 0 then
        Check(inttostr(i), inttostr(i), 2)
      else
        Check(inttostr(i), inttostr(i), 1);

  finally
    hsh.free;
    lwl.Free;
  end;
end;

initialization

  RegisterTest(TTestSyncroEdit); 
end.

