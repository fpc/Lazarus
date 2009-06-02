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
begin
  Template.FileName := 'mytemplate1.html';
  Template.AllowTagParams := true;
  Template.OnReplaceTag := @func1callReplaceTag;

  AResponse.Content := Template.GetContent;

  Handled := true;
end;

procedure TFPWebModule1.func1callReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  if AnsiCompareText(TagString, 'TagName1') = 0 then
  begin
    ReplaceText := 'Here I am from the web module!';
  end else begin

//Not found value for tag -> TagString
    ReplaceText := 'Template tag {' + TagString + '} is not implemented yet.';
  end;
end;

initialization
  {$I webmodule.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
