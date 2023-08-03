{
 Test all with:
     ./runtests --format=plain --suite=TTestSearchPathProcs

 Test specific with:
     ./runtests --format=plain --suite=TTestSearchPathProcs.TestRelateDirectoryMasks
}
unit TestSearchPathProcs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testglobals, LazLogger, LazFileUtils,
  SearchPathProcs, LazUTF8;

type

  { TTestSearchPathProcs }

  TTestSearchPathProcs = class(TTestCase)
  published
    procedure TestRelateDirectoryMasks;
    procedure TestFileIsInSPDirectory;
    procedure TestTrimSearchPath;
  end;

implementation

{ TTestSearchPathProcs }

procedure TTestSearchPathProcs.TestRelateDirectoryMasks;

  procedure t(const Paths: TStringDynArray; Left, Right: integer; Expected: TSPFileMaskRelation);
  var
    SearchPath: String;
    i, LeftStart, RightStart: Integer;
    Actual: TSPFileMaskRelation;
  begin
    SearchPath:='';
    LeftStart:=10000;
    RightStart:=10000;
    for i:=0 to length(Paths)-1 do begin
      if i>0 then
        SearchPath+=';';
      if i=Left then
        LeftStart:=length(SearchPath)+1;
      if i=Right then
        RightStart:=length(SearchPath)+1;
      SearchPath+=SetDirSeparators(Paths[i]);
    end;

    Actual:=RelateDirectoryMasks(SearchPath,LeftStart,SearchPath,RightStart);
    if Actual<>Expected then begin
      Fail('SearchPath="'+SearchPath+'" LeftStart='+IntToStr(LeftStart)+' RightStart='+IntToStr(RightStart)+' Actual='+dbgs(Actual)+' Expected='+dbgs(Expected));
    end;

    // try the other way round
    case Expected of
      TSPFileMaskRelation.LeftMoreGeneral: Expected:=TSPFileMaskRelation.RightMoreGeneral;
      TSPFileMaskRelation.RightMoreGeneral: Expected:=TSPFileMaskRelation.LeftMoreGeneral;
    end;
    Actual:=RelateDirectoryMasks(SearchPath,RightStart,SearchPath,LeftStart);
    if Actual<>Expected then begin
      Fail('SearchPath="'+SearchPath+'" LeftStart='+IntToStr(LeftStart)+' RightStart='+IntToStr(RightStart)+' Actual='+dbgs(Actual)+' Expected='+dbgs(Expected));
    end;
  end;

begin
  t([''],1,1,TSPFileMaskRelation.None);
  t([''],0,0,TSPFileMaskRelation.None);
  t(['a'],0,0,TSPFileMaskRelation.Equal);
  t(['foo'],0,0,TSPFileMaskRelation.Equal);
  {$IFDEF MSWindows}
  t(['C:\foo'],0,0,TSPFileMaskRelation.Equal);
  t(['C:\foo\bar'],0,0,TSPFileMaskRelation.Equal);
  t(['C:\foo\bar','C:\foo\bar\'],0,1,TSPFileMaskRelation.Equal);
  {$ELSE}
  t(['/foo'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/bar'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/bar','/foo/bar/'],0,1,TSPFileMaskRelation.Equal);
  {$ENDIF}

  // star
  t(['*'],0,0,TSPFileMaskRelation.Equal);
  t(['*','a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['*','.'],0,1,TSPFileMaskRelation.None);
  t(['*','..'],0,1,TSPFileMaskRelation.None);
  {$IFDEF MSWindows}
  t(['C:\*'],0,0,TSPFileMaskRelation.Equal);
  t(['C:\foo\*'],0,0,TSPFileMaskRelation.Equal);
  t(['*','a\'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['C:\*','C:\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['C:\*','a'],0,1,TSPFileMaskRelation.None);
  t(['C:\foo\*','C:\foo\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['C:\foo\*','C:\foo\a\'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['foo\*','foo\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['C:\foo\*','C:\a'],0,1,TSPFileMaskRelation.None);
  t(['foo\*','a'],0,1,TSPFileMaskRelation.None);
  t(['C:\foo\*','C:\foo\bar\a'],0,1,TSPFileMaskRelation.None);
  t(['foo\*','foo\bar\a'],0,1,TSPFileMaskRelation.None);
  t(['C:\*','C:\'],0,1,TSPFileMaskRelation.None);
  {$ELSE}
  t(['/*'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/*'],0,0,TSPFileMaskRelation.Equal);
  t(['*','a/'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/*','/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/*','a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/*','/foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/foo/*','/foo/a/'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['foo/*','foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/foo/*','/a'],0,1,TSPFileMaskRelation.None);
  t(['foo/*','a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/*','/foo/bar/a'],0,1,TSPFileMaskRelation.None);
  t(['foo/*','foo/bar/a'],0,1,TSPFileMaskRelation.None);
  t(['/*','/'],0,1,TSPFileMaskRelation.None);
  {$ENDIF}

  // star star
  t(['**'],0,0,TSPFileMaskRelation.Equal);
  t(['**','a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','.'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','..'],0,1,TSPFileMaskRelation.None);
  t(['**','foo\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  {$IFDEF MSWindows}
  t(['C:\**'],0,0,TSPFileMaskRelation.Equal);
  t(['C:\bar\**'],0,0,TSPFileMaskRelation.Equal);
  t(['**','C:\foo\a'],0,1,TSPFileMaskRelation.None);
  t(['C:\**','foo\a'],0,1,TSPFileMaskRelation.None);
  t(['C:\foo\**','foo\a'],0,1,TSPFileMaskRelation.None);
  t(['C:\foo\**','C:\foo\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['C:\foo\**','C:\foo\bar\a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  {$ELSE}
  t(['/**'],0,0,TSPFileMaskRelation.Equal);
  t(['/bar/**'],0,0,TSPFileMaskRelation.Equal);
  t(['**','foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','/foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/**','foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/**','foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/**','/foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/foo/**','/foo/bar/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  {$ENDIF}
end;

procedure TTestSearchPathProcs.TestFileIsInSPDirectory;

  procedure t(Filename, Directory: string; MaskType: TSPMaskType; Expected: boolean);
  var
    Actual: Boolean;
  begin
    Filename:=SetDirSeparators(Filename);
    Directory:=SetDirSeparators(Directory);
    Actual:=FileIsInSPDirectory(Filename,Directory,MaskType);
    if Actual=Expected then exit;
    Fail('Filename="'+Filename+'" Directory="'+Directory+'" MaskType='+dbgs(MaskType)+' Expected='+dbgs(Expected)+', but was '+dbgs(Actual));
  end;

begin
  t('','',TSPMaskType.None,false);
  t('.','',TSPMaskType.None,false);
  t('..','',TSPMaskType.None,false);
  t('../','',TSPMaskType.None,false);
  t('a','',TSPMaskType.None,true);
  t('a/b','',TSPMaskType.None,false);
  t('','foo',TSPMaskType.None,false);
  t('a','foo',TSPMaskType.None,false);
  t('foo/','foo',TSPMaskType.None,true);
  t('foo/a','foo',TSPMaskType.None,true);
  t('foo/bar/a','foo',TSPMaskType.None,false);
  {$IFDEF MSWindows}
  t('C:\a','',TSPMaskType.None,false);
  t('C:\a\b','',TSPMaskType.None,false);
  t('C:\','foo',TSPMaskType.None,false);
  t('C:\','C:\foo',TSPMaskType.None,false);
  t('C:\a','C:\foo',TSPMaskType.None,false);
  t('C:\foo\','C:\foo',TSPMaskType.None,true);
  t('C:\foo\a','C:\foo',TSPMaskType.None,true);
  t('C:\foo\bar\a','C:\foo',TSPMaskType.None,false);
  {$ELSE}
  t('/a','',TSPMaskType.None,false);
  t('/a/b','',TSPMaskType.None,false);
  t('/','foo',TSPMaskType.None,false);
  t('/','/foo',TSPMaskType.None,false);
  t('/a','/foo',TSPMaskType.None,false);
  t('/foo/','/foo',TSPMaskType.None,true);
  t('/foo/a','/foo',TSPMaskType.None,true);
  t('/foo/bar/a','/foo',TSPMaskType.None,false);
  {$ENDIF}

  t('','',TSPMaskType.Star,false);
  t('.','',TSPMaskType.Star,false);
  t('..','',TSPMaskType.Star,false);
  t('../','',TSPMaskType.Star,false);
  t('a','',TSPMaskType.Star,false);
  t('a/b','',TSPMaskType.Star,true);
  t('a/b/c','',TSPMaskType.Star,false);
  t('','foo',TSPMaskType.Star,false);
  t('a','foo',TSPMaskType.Star,false);
  t('foo/','foo',TSPMaskType.Star,false);
  t('foo/bar','foo',TSPMaskType.Star,false);
  t('foo/bar/a','foo',TSPMaskType.Star,true);
  t('foo/bar/a/b','foo',TSPMaskType.Star,false);
  {$IFDEF MSWindows}
  t('C:\a','',TSPMaskType.Star,false);
  t('C:\a','C:\',TSPMaskType.Star,false);
  t('C:\a\b','C:\',TSPMaskType.Star,true);
  t('C:\a\b\c','C:\',TSPMaskType.Star,false);
  t('C:\','C:\foo',TSPMaskType.Star,false);
  t('C:\a','C:\foo',TSPMaskType.Star,false);
  t('C:\foo\','C:\foo',TSPMaskType.Star,false);
  t('C:\foo\bar','C:\foo',TSPMaskType.Star,false);
  t('C:\foo\bar\a','C:\foo',TSPMaskType.Star,true);
  t('C:\foo\bar\a\b','C:\foo',TSPMaskType.Star,false);
  {$ELSE}
  t('/a','',TSPMaskType.Star,false);
  t('/a','/',TSPMaskType.Star,false);
  t('/a/b','/',TSPMaskType.Star,true);
  t('/a/b/c','/',TSPMaskType.Star,false);
  t('/','/foo',TSPMaskType.Star,false);
  t('/a','/foo',TSPMaskType.Star,false);
  t('/foo/','/foo',TSPMaskType.Star,false);
  t('/foo/bar','/foo',TSPMaskType.Star,false);
  t('/foo/bar/a','/foo',TSPMaskType.Star,true);
  t('/foo/bar/a/b','/foo',TSPMaskType.Star,false);
  {$ENDIF}

  t('','',TSPMaskType.StarStar,false);
  t('.','',TSPMaskType.StarStar,true);
  t('..','',TSPMaskType.StarStar,false);
  t('../','',TSPMaskType.StarStar,false);
  t('a','',TSPMaskType.StarStar,true);
  t('a/b','',TSPMaskType.StarStar,true);
  t('a/b/c','',TSPMaskType.StarStar,true);
  t('','foo',TSPMaskType.StarStar,false);
  t('a','foo',TSPMaskType.StarStar,false);
  t('foo/','foo',TSPMaskType.StarStar,true);
  t('foo/bar','foo',TSPMaskType.StarStar,true);
  t('foo/bar','foo/bar',TSPMaskType.StarStar,false);
  t('foo/bar/a','foo',TSPMaskType.StarStar,true);
  t('foo/bar/a','foo/bar',TSPMaskType.StarStar,true);
  {$IFDEF MSWindows}
  t('foo\','C:\foo',TSPMaskType.StarStar,false);
  t('C:\a','C:\',TSPMaskType.StarStar,true);
  t('C:\a\b','C:\',TSPMaskType.StarStar,true);
  t('C:\a\b\c','C:\',TSPMaskType.StarStar,true);
  t('C:\','C:\foo',TSPMaskType.StarStar,false);
  t('C:\a','C:\foo',TSPMaskType.StarStar,false);
  t('C:\foo\','C:\foo',TSPMaskType.StarStar,true);
  t('C:\foo\bar','C:\foo',TSPMaskType.StarStar,true);
  t('C:\foo\bar','C:\foo\bar',TSPMaskType.StarStar,false);
  t('C:\foo\bar\a','C:\foo',TSPMaskType.StarStar,true);
  t('C:\foo\bar\a','C:\foo\bar',TSPMaskType.StarStar,true);
  {$ELSE}
  t('foo/','/foo',TSPMaskType.StarStar,false);
  t('/a','/',TSPMaskType.StarStar,true);
  t('/a/b','/',TSPMaskType.StarStar,true);
  t('/a/b/c','/',TSPMaskType.StarStar,true);
  t('/','/foo',TSPMaskType.StarStar,false);
  t('/a','/foo',TSPMaskType.StarStar,false);
  t('/foo/','/foo',TSPMaskType.StarStar,true);
  t('/foo/bar','/foo',TSPMaskType.StarStar,true);
  t('/foo/bar','/foo/bar',TSPMaskType.StarStar,false);
  t('/foo/bar/a','/foo',TSPMaskType.StarStar,true);
  t('/foo/bar/a','/foo/bar',TSPMaskType.StarStar,true);
  {$ENDIF}
end;

procedure TTestSearchPathProcs.TestTrimSearchPath;

  procedure t(SearchPath, BaseDirectory: string;
              DeleteDoubles, ExpandPaths: boolean; Expected: string);
  var
    Actual: String;
  begin
    SearchPath:=SetDirSeparators(SearchPath);
    BaseDirectory:=SetDirSeparators(BaseDirectory);
    Actual:=TrimSearchPath(SearchPath,BaseDirectory,DeleteDoubles,ExpandPaths);
    if Actual=Expected then exit;
    Fail('SearchPath="'+SearchPath+'" BaseDirectory="'+BaseDirectory+'" DeleteDoubles='+dbgs(DeleteDoubles)+' ExpandPaths='+dbgs(ExpandPaths)+' expected "'+Expected+'", but was "'+Actual+'"');
  end;

begin
  t('/a','',true,false,'/a');
  t('/foo/*;/foo/*','',true,false,'/foo/*');
  t('/foo/*;/foo/bar','',true,false,'/foo/*');
  t('/foo/bar;/foo/*','',true,false,'/foo/bar;/foo/*');
  t('/a;/foo/*;/b;/foo/bar','',true,false,'/a;/foo/*;/b');
  t('/a;/foo/*;/b;/foo/**','',true,false,'/a;/foo/*;/b;/foo/**');
  t('/a;/foo/**;/b;/foo/*','',true,false,'/a;/foo/**;/b');
end;

initialization
  AddToIDEIntfTestSuite(TTestSearchPathProcs);
end.

