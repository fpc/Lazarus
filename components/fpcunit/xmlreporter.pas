{
  Copyright (C) 2006 Graeme Geldenhuys <graemeg@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  

  Purpose:
    This unit contains a XML TestListener for use with the fpcUnit testing
    framework.  It uses the XMLWrite unit, which is part of FPC, to generate
    the XML document. The benefit of using the XMLWrite unit, is that the
    data generated is valid XML, with resevered characters correctly escaped.
    This allows the XML document to be further processed with XSTL etc without
    any issues.

  Note:
    This unit will possibly move at a later stage to the FPC repository.
    It can't now (pre FPC 2.0.3) otherwise nobody will be able to use the new
    XML funtionality in the GUITestRunner.
}

unit xmlreporter;

{$mode objfpc}{$H+}

interface
uses
  Classes
  ,SysUtils
  ,fpcUnit
  ,TestUtils
  ,dom
  ,XMLWrite
  ;
  

type
  { XML Test Listner }
  TXMLResultsWriter = class(TNoRefCountObject, ITestListener)
  private
    FDoc: TXMLDocument;
    { These TDOMNodes are for easy access and a bit of optimization }
    FResults: TDOMNode;
    FListing: TDOMNode;
    FFailures: TDOMNode;
    FErrors: TDOMNode;
    { Converts the actual test results into XML nodes. This gets called
      by the public method WriteResult. }
    procedure   TestResultAsXML(pTestResult: TTestResult);
    { This gets called in the class constructor and sets up the starting nodes }
    procedure   WriteHeader;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   WriteResult(aResult: TTestResult);

    { ITestListener interface requirements }
    procedure   AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure   AddError(ATest: TTest; AError: TTestFailure);
    procedure   StartTest(ATest: TTest);
    procedure   EndTest(ATest: TTest);
    
    { A public property to the internal XML document }
    property    Document: TXMLDocument read FDoc;
  end;


implementation


{ TXMLResultsWriter }

procedure TXMLResultsWriter.TestResultAsXML(pTestResult: TTestResult);
var
  i: longint;
  n, lResults: TDOMNode;
begin
  lResults := FDoc.FindNode('TestResults');
  n := FDoc.CreateElement('NumberOfRunnedTests');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(pTestResult.RunTests)));
  lResults.AppendChild(n);

  n := FDoc.CreateElement('NumberOfErrors');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(pTestResult.NumberOfErrors)));
  lResults.AppendChild(n);

  n := FDoc.CreateElement('NumberOfFailures');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(pTestResult.NumberOfFailures)));
  lResults.AppendChild(n);
  
  if pTestResult.NumberOfErrors <> 0 then
  begin
    for i := 0 to pTestResult.Errors.Count - 1 do
      AddError(nil, TTestFailure(pTestResult.Errors.Items[i]));
  end;

  if pTestResult.NumberOfFailures <> 0 then
  begin
    for i := 0 to pTestResult.Failures.Count - 1 do
      AddFailure(nil, TTestFailure(pTestResult.Failures.Items[i]));
  end;
end;


procedure TXMLResultsWriter.WriteHeader;
begin
  FResults := FDoc.CreateElement('TestResults');
  FResults.AppendChild(FDoc.CreateComment(' Generated using FPCUnit on '
      + FormatDateTime('yyyy-mm-dd hh:mm ', Now) ));
  FDoc.AppendChild(FResults);
  FListing := FDoc.CreateElement('TestListing');
  FResults.AppendChild(FListing);
end;


constructor TXMLResultsWriter.Create;
begin
  FDoc        := TXMLDocument.Create;
  FResults    := nil;
  FFailures   := nil;
  FErrors     := nil;
  WriteHeader;
end;


destructor TXMLResultsWriter.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;


procedure TXMLResultsWriter.WriteResult(aResult: TTestResult);
begin
  TestResultAsXML(aResult);
end;


procedure TXMLResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  n: TDOMElement;
begin
  { Try and find the node first }
  if not Assigned(FFailures) then
    FFailures := FDoc.FindNode('ListOfFailures');
  { If we couldn't find it, create it }
  if not Assigned(FFailures) then
  begin
    FFailures := FDoc.CreateElement('ListOfFailures');
    FResults.AppendChild(FFailures);
  end;
  
  n := FDoc.CreateElement('Failure');
  n.AppendChild(FDoc.CreateElement('Message')         ).AppendChild(FDoc.CreateTextNode(AFailure.AsString));
  n.AppendChild(FDoc.CreateElement('ExceptionClass')  ).AppendChild(FDoc.CreateTextNode(AFailure.ExceptionClassName));
  n.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild(FDoc.CreateTextNode(AFailure.ExceptionMessage));
  FFailures.AppendChild(n);
end;


procedure TXMLResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
var
  n: TDOMElement;
begin
  { Try and find the node first }
  if not Assigned(FErrors) then
    FErrors := FDoc.FindNode('ListOfErrors');
  { If we couldn't find it, create it }
  if not Assigned(FErrors) then
  begin
    FErrors := FDoc.CreateElement('ListOfErrors');
    FResults.AppendChild(FErrors);
  end;

  n := FDoc.CreateElement('Error');
  n.AppendChild(FDoc.CreateElement('Message')         ).AppendChild(FDoc.CreateTextNode(AError.AsString));
  n.AppendChild(FDoc.CreateElement('ExceptionClass')  ).AppendChild(FDoc.CreateTextNode(AError.ExceptionClassName));
  n.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild(FDoc.CreateTextNode(AError.ExceptionMessage));
  n.AppendChild(FDoc.CreateElement('SourceUnitName')  ).AppendChild(FDoc.CreateTextNode(AError.SourceUnitName));
  n.AppendChild(FDoc.CreateElement('LineNumber')      ).AppendChild(FDoc.CreateTextNode(IntToStr(AError.LineNumber)));
  n.AppendChild(FDoc.CreateElement('FailedMethodName')).AppendChild(FDoc.CreateTextNode(AError.FailedMethodName));
  FErrors.AppendChild(n);
end;


procedure TXMLResultsWriter.StartTest(ATest: TTest);
var
  n: TDOMElement;
begin
  if not Assigned(FListing) then
    exit;
  n := FDoc.CreateElement('Test');
  n['Name'] := ATest.TestSuiteName + '.' + ATest.TestName;
  FListing.AppendChild(n);
end;


procedure TXMLResultsWriter.EndTest(ATest: TTest);
begin
  { do nothing }
end;


end.

