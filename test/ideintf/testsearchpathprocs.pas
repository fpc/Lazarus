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
  t(['/foo'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/bar'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/bar','/foo/bar/'],0,1,TSPFileMaskRelation.Equal);

  // star
  t(['*'],0,0,TSPFileMaskRelation.Equal);
  t(['/*'],0,0,TSPFileMaskRelation.Equal);
  t(['/foo/*'],0,0,TSPFileMaskRelation.Equal);
  t(['*','a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
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
  t(['*','.'],0,1,TSPFileMaskRelation.None);
  t(['*','..'],0,1,TSPFileMaskRelation.None);
  t(['/*','/'],0,1,TSPFileMaskRelation.None);

  // star star
  t(['**'],0,0,TSPFileMaskRelation.Equal);
  t(['/**'],0,0,TSPFileMaskRelation.Equal);
  t(['/bar/**'],0,0,TSPFileMaskRelation.Equal);
  t(['**','a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','/foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/**','foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/**','foo/a'],0,1,TSPFileMaskRelation.None);
  t(['/foo/**','/foo/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['/foo/**','/foo/bar/a'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','.'],0,1,TSPFileMaskRelation.LeftMoreGeneral);
  t(['**','..'],0,1,TSPFileMaskRelation.None);
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
  t('/a','',TSPMaskType.None,false);
  t('/a/b','',TSPMaskType.None,false);
  t('/','foo',TSPMaskType.None,false);
  t('/','/foo',TSPMaskType.None,false);
  t('/a','/foo',TSPMaskType.None,false);
  t('/foo/','/foo',TSPMaskType.None,true);
  t('/foo/a','/foo',TSPMaskType.None,true);
  t('/foo/bar/a','/foo',TSPMaskType.None,false);

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
end;

initialization
  AddToIDEIntfTestSuite(TTestSearchPathProcs);
end.

