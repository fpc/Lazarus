unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  contnrs, CTXMLFixFragment;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FVerbose: boolean;
  public
    procedure TestComment;
    procedure TestInvalidCharacters;
    procedure TestOpenTag;
    procedure TestAttribute;
    procedure TestCloseTag;
    procedure TestBugReports;
    function Test(Title, Fragment, FixedFragment: string): boolean;
    property Verbose: boolean read FVerbose write FVerbose;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Verbose:=false;
  TestComment;
  TestInvalidCharacters;
  TestOpenTag;
  TestAttribute;
  TestCloseTag;
  TestBugReports;
end;

procedure TForm1.TestComment;
begin
  Test('close comment','<!--','<!---->');
  Test('close comment and delete invalid char','<!--null'#0#1#2'comment','<!--nullcomment-->');
end;

procedure TForm1.TestInvalidCharacters;
begin
  Test('delete special characters','A'#0'B'#1#127,'AB');
  Test('replace tag characters','LT< GT>AMP&','LT&lt; GT&gt;AMP&amp;');
  Test('lower case special characters','&LT;','&lt;');
end;

procedure TForm1.TestOpenTag;
begin
  Test('missing tag name','<>','&lt;&gt;');
  Test('lower case tag name','<A></a>','<a></a>');
  Test('invalid character in tag','<a "></a>','<a >"&gt;</a>');
end;

procedure TForm1.TestAttribute;
begin
  Test('lower case attribute name','<a Name=""></a>','<a name=""></a>');
  Test('missing attribute equal','<a name ""></a>','<a name =""></a>');
  Test('missing attribute value','<a name=></a>','<a name=""></a>');
  Test('missing attribute quotes','<a name=1></a>','<a name="1"></a>');
  Test('missing attribute ending quote','<a name="1></a>','<a name="1"></a>');
  Test('invalid character in attribute value','<a name="&"></a>','<a name="&amp;"></a>');
  Test('amp attribute value','<a name="&amp;"></a>','<a name="&amp;"></a>');
end;

procedure TForm1.TestCloseTag;
begin
  Test('lower case close tag name','<a></A>','<a></a>');
  Test('close open tag','<a>','<a/>');
  Test('close open sub tag','<p><a></p>','<p><a/></p>');
  Test('disable invalid close tag','</p>','&lt;/p&gt;');
end;

procedure TForm1.TestBugReports;
begin
  Test('15120','operator <(TPoint, TPoint): Boolean',
                 'operator &lt;(TPoint, TPoint): Boolean');
  Test('16671','<br>',
               '<br/>');
end;

function TForm1.Test(Title, Fragment, FixedFragment: string): boolean;
var
  s: String;
  ErrorList: TObjectList;
begin
  Result:=true;
  try
    s:=Fragment;
    FixFPDocFragment(s,true,true,ErrorList,Verbose);
    if s<>FixedFragment then begin
      Result:=false;
      debugln(['failed: ',Title]);
      debugln(['  fragment: '+DbgStr(Fragment)]);
      debugln(['  should:   '+DbgStr(FixedFragment)]);
      debugln(['  result:   '+DbgStr(s)]);
    end;
  finally
    ErrorList.Free;
  end;
end;

end.

