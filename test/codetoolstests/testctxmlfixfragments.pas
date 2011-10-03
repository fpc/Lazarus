{
 Test all with:
     ./runtests --format=plain --suite=TTestCTXMLFixFragment

 Test individually:
     ./runtests --format=plain --suite=TestFixXMLFragmentComment
     ./runtests --format=plain --suite=TestFixXMLValue
}
unit TestCTXMLFixFragments;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, Classes, SysUtils, FileProcs, testglobals, CTXMLFixFragment;

type

  { TTestCTXMLFixFragment }

  TTestCTXMLFixFragment = class(TTestCase)
  protected
    function TestFrag(Title, Fragment, FixedFragment: string): boolean;
    function TestAttr(Title, Value, FixedValue: string): boolean;
  published
    procedure TestFixXMLFragmentComment;
    procedure TestFixXMLFragmentInvalidCharacters;
    procedure TestFixXMLFragmentOpenTag;
    procedure TestFixXMLFragmentAttribute;
    procedure TestFixXMLFragmentCloseTag;
    procedure TestFixXMLFragmentBugReports;
    // attribute value
    procedure TestFixXMLValue;
  end;

implementation

{ TTestCTXMLFixFragment }

function TTestCTXMLFixFragment.TestFrag(Title, Fragment, FixedFragment: string
  ): boolean;
var
  s: String;
begin
  Result:=true;
  s:=Fragment;
  FixFPDocFragment(s,true,true,nil,false);
  AssertEquals(Title+' fragment: '+DbgStr(Fragment),dbgstr(FixedFragment),dbgstr(s));
end;

function TTestCTXMLFixFragment.TestAttr(Title, Value, FixedValue: string
  ): boolean;
var
  s: String;
begin
  Result:=true;
  s:=Value;
  FixFPDocAttributeValue(s);
  AssertEquals(Title+' value: '+DbgStr(Value),dbgstr(FixedValue),dbgstr(s));
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentComment;
begin
  TestFrag('close comment','<!--','<!---->');
  TestFrag('close comment and delete invalid char','<!--null'#0#1#2'comment','<!--nullcomment-->');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentInvalidCharacters;
begin
  TestFrag('delete special characters','A'#0'B'#1#127,'AB');
  TestFrag('replace tag characters','LT< GT>AMP&','LT&lt; GT&gt;AMP&amp;');
  TestFrag('lower case special characters','&LT;','&lt;');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentOpenTag;
begin
  TestFrag('valid short tag','<link/>','<link/>');
  TestFrag('valid short with empty attribute tag','<link id=""/>','<link id=""/>');
  TestFrag('missing tag name','<>','&lt;&gt;');
  TestFrag('lower case tag name','<A></a>','<a></a>');
  TestFrag('invalid character in tag','<a "></a>','<a >"&gt;</a>');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentAttribute;
begin
  TestFrag('lower case attribute name','<a Name=""></a>','<a name=""></a>');
  TestFrag('missing attribute equal','<a name ""></a>','<a name =""></a>');
  TestFrag('missing attribute value','<a name=></a>','<a name=""></a>');
  TestFrag('missing attribute quotes','<a name=1></a>','<a name="1"></a>');
  TestFrag('missing attribute ending quote','<a name="1></a>','<a name="1"></a>');
  TestFrag('invalid character in xml fragment attribute value','<a name="&"></a>','<a name="&amp;"></a>');
  TestFrag('amp attribute value','<a name="&amp;"></a>','<a name="&amp;"></a>');
  TestFrag('lt attribute value','<a name="operator&lt;"></a>','<a name="operator&lt;"></a>');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentCloseTag;
begin
  TestFrag('lower case close tag name','<a></A>','<a></a>');
  TestFrag('close open tag','<a>','<a/>');
  TestFrag('close open sub tag','<p><a></p>','<p><a/></p>');
  TestFrag('disable invalid close tag','</p>','&lt;/p&gt;');
end;

procedure TTestCTXMLFixFragment.TestFixXMLFragmentBugReports;
begin
  TestFrag('15120','operator <(TPoint, TPoint): Boolean',
                 'operator &lt;(TPoint, TPoint): Boolean');
  TestFrag('16671','<br>',
               '<br/>');
  TestFrag('18800','<link id="foo"/>','<link id="foo"/>');
end;

procedure TTestCTXMLFixFragment.TestFixXMLValue;
begin
  TestAttr('invalid character in xml attribute value','operator<','operator&lt;');
  TestAttr('correct character in xml attribute value','&amp;','&amp;');
  TestAttr('lower case character name in attribute value','&AMP;','&amp;');
  TestAttr('" in attribute value','"','&quot;');
  TestAttr(''' in attribute value','''','&apos;');
  TestAttr('< in attribute value','<','&lt;');
  TestAttr('> in attribute value','>','&gt;');
end;

initialization
  AddToCodetoolsTestSuite(TTestCTXMLFixFragment);

end.

