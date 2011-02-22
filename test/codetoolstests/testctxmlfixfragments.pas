{
 Test all with:
     ./runtests --format=plain --suite=TTestCTXMLFixFragment

 Test individually:
     ./runtests --format=plain --suite=TestFixXMLFragmentComment
}
unit TestCTXMLFixFragments;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, Classes, SysUtils, FileProcs, contnrs, testglobals, CTXMLFixFragment;

type

  { TTestCTXMLFixFragment }

  TTestCTXMLFixFragment = class(TTestCase)
  protected
    function Test(Title, Fragment, FixedFragment: string): boolean;
  published
    procedure TestFixXMLFragmentComment;
    procedure TestFixXMLFragmentInvalidCharacters;
    procedure TestFixXMLFragmentOpenTag;
    procedure TestFixXMLFragmentAttribute;
    procedure TestFixXMLFragmentCloseTag;
    procedure TestFixXMLFragmentBugReports;
  end;

implementation

{ TTestCTXMLFixFragment }

function TTestCTXMLFixFragment.Test(Title, Fragment, FixedFragment: string
  ): boolean;
var
  s: String;
  ErrorList: TObjectList;
begin
  Result:=true;
  try
    s:=Fragment;
    FixFPDocFragment(s,true,true,ErrorList,false);
    AssertEquals(Title+' fragment: '+DbgStr(Fragment),dbgstr(FixedFragment),dbgstr(s));
  finally
    ErrorList.Free;
  end;
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentComment;
begin
  Test('close comment','<!--','<!---->');
  Test('close comment and delete invalid char','<!--null'#0#1#2'comment','<!--nullcomment-->');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentInvalidCharacters;
begin
  Test('delete special characters','A'#0'B'#1#127,'AB');
  Test('replace tag characters','LT< GT>AMP&','LT&lt; GT&gt;AMP&amp;');
  Test('lower case special characters','&LT;','&lt;');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentOpenTag;
begin
  Test('valid short tag','<link/>','<link/>');
  Test('valid short with empty attribute tag','<link id=""/>','<link id=""/>');
  Test('missing tag name','<>','&lt;&gt;');
  Test('lower case tag name','<A></a>','<a></a>');
  Test('invalid character in tag','<a "></a>','<a >"&gt;</a>');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentAttribute;
begin
  Test('lower case attribute name','<a Name=""></a>','<a name=""></a>');
  Test('missing attribute equal','<a name ""></a>','<a name =""></a>');
  Test('missing attribute value','<a name=></a>','<a name=""></a>');
  Test('missing attribute quotes','<a name=1></a>','<a name="1"></a>');
  Test('missing attribute ending quote','<a name="1></a>','<a name="1"></a>');
  Test('invalid character in attribute value','<a name="&"></a>','<a name="&amp;"></a>');
  Test('amp attribute value','<a name="&amp;"></a>','<a name="&amp;"></a>');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentCloseTag;
begin
  Test('lower case close tag name','<a></A>','<a></a>');
  Test('close open tag','<a>','<a/>');
  Test('close open sub tag','<p><a></p>','<p><a/></p>');
  Test('disable invalid close tag','</p>','&lt;/p&gt;');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentBugReports;
begin
  Test('15120','operator <(TPoint, TPoint): Boolean',
                 'operator &lt;(TPoint, TPoint): Boolean');
  Test('16671','<br>',
               '<br/>');
  Test('18800','<link id="foo"/>','<link id="foo"/>');
end;

initialization
  AddToCodetoolsTestSuite(TTestCTXMLFixFragment);

end.

