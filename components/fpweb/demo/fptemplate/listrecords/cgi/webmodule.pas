unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb; 

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure func1callRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    procedure func1callReplaceTag(Sender: TObject; const TagString:String; 
      TagParams: TStringList; Out ReplaceText: String);
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{ TFPWebModule1 }

procedure TFPWebModule1.func1callRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin     //Template:TFPTemplate is a property of the web module
  Template.FileName := 'mytemplate3.html';{template file with 
the template tag -> REPORTRESULT}
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+';
  Template.EndDelimiter := '+}';
  Template.OnReplaceTag := @func1callReplaceTag;

  AResponse.Content := Template.GetContent;

  Handled := true;
end;

procedure TFPWebModule1.func1callReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
var
   header, footer, onerow:String;
   NoRecordsToShow:Boolean;
   ColumnHeaders:array[1..2] of String;
   ColumnValues:array[1..3,1..2]of String;
   I:Integer;
begin//HTML template tag handling for an html template file

  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else 

  //Replace the REPORTRESULT html tag using it's tag parameters
  if AnsiCompareText(TagString, 'REPORTRESULT') = 0 then
  begin
    //fill up some arrays with data (could come from a SQL query)
    NoRecordsToShow := false;

    ColumnHeaders[1] := 'Amount';
    ColumnHeaders[2] := 'Percentage';

    ColumnValues[1,1] := '10.00';
    ColumnValues[1,2] := '5';

    ColumnValues[2,1] := '15.00';
    ColumnValues[2,2] := '4';

    ColumnValues[3,1] := '20.00';
    ColumnValues[3,2] := '3';

    //NoRecordsToShow could be something like SQL1.IsEmpty , etc.
    if NoRecordsToShow then
    begin  //if there's nothing to list, just replace the whole tag with the 
           //"Not Found" message that the template contains
      ReplaceText := TagParams.Values['NOTFOUND'];
      Exit;
    end;

    header := TagParams.Values['HEADER'];
    //insert header parameters
    header := StringReplace(header, '~Column1', ColumnHeaders[1], []);
    header := StringReplace(header, '~Column2', ColumnHeaders[2], []);

    ReplaceText := header;//done with the header (could have been looping 
			  //through table field names also)
    //insert the rows
    onerow := TagParams.Values['ONEROW'];//template for 1 row
    //loop through the rows, it could be someting like "while not SQL1.EOF do"
    for I := 1 to 3 do
    begin
      ReplaceText := ReplaceText + StringReplace(StringReplace(onerow
                       ,'~Column1Value', '$' + ColumnValues[I, 1], [])
                       ,'~Column2value', ColumnValues[I, 2] + '%', []) + #13#10;
    end;

    //insert the footer
    footer := TagParams.Values['FOOTER'];
    //replace footer parameters if needed
    //...

    ReplaceText := ReplaceText + footer;
  end else begin

//Not found value for tag -> TagString
    ReplaceText := 'Template tag {+' + TagString + '+} is not implemented yet.';
  end;
end;

initialization
  {$I webmodule.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
