unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons;

type

  { TMyComponent }

  TMyComponent = class(TCheckBox)
  private
    FDefaultText: WideString;
    FWideStr1: widestring;
    procedure SetDefaultText(const AValue: WideString);
    procedure SetWideStr1(const AValue: widestring);
    function WideStr1IsStored: boolean;
    procedure ReadText(Reader: TReader);
    procedure WriteText(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property WideStr1: widestring read FWideStr1 write SetWideStr1 stored WideStr1IsStored;
    property DefaultText: WideString read FDefaultText write SetDefaultText stored False;
  end;

  { TStreamDemoForm }

  TStreamDemoForm = class(TForm)
    AGroupBox: TGroupBox;
    StreamAsLFMCheckBox: TCheckBox;
    Note2Label: TLabel;
    Note1Label: TLabel;
    ReadStreamButton: TButton;
    StreamMemo: TMemo;
    StreamGroupBox: TGroupBox;
    WriteToStreamButton: TButton;
    SourceGroupBox: TGroupBox;
    DestinationGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure ReadStreamButtonClick(Sender: TObject);
    procedure StreamAsLFMCheckBoxChange(Sender: TObject);
    procedure WriteToStreamButtonClick(Sender: TObject);
  public
    StreamAsString: string;
    procedure ShowStreamInMemo;
    procedure SaveStreamAsString(AStream: TStream);
    procedure ReadStreamFromString(AStream: TStream);
    function ReadStringFromStream(AStream: TStream): string;
    procedure ClearDestinationGroupBox;
    procedure OnFindClass(Reader: TReader; const AClassName: string;
                          var ComponentClass: TComponentClass);
  end;

var
  StreamDemoForm: TStreamDemoForm;

implementation

{ TStreamDemoForm }

procedure TStreamDemoForm.WriteToStreamButtonClick(Sender: TObject);
var
  AStream: TMemoryStream;
begin
  AStream:=TMemoryStream.Create;
  try
    WriteComponentAsBinaryToStream(AStream,AGroupBox);
    SaveStreamAsString(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TStreamDemoForm.ReadStreamButtonClick(Sender: TObject);
var
  NewComponent: TComponent;
  AStream: TMemoryStream;
begin
  ClearDestinationGroupBox;

  AStream:=TMemoryStream.Create;
  try
    ReadStreamFromString(AStream);
    NewComponent:=nil;
    ReadComponentFromBinaryStream(AStream,NewComponent,
                                  @OnFindClass,DestinationGroupBox);
    if NewComponent is TControl then
      TControl(NewComponent).Parent:=DestinationGroupBox;
  finally
    AStream.Free;
  end;
end;

procedure TStreamDemoForm.FormCreate(Sender: TObject);
var
  MyComponent: TMyComponent;
begin
  // create a checkbox with Owner = AGroupBox
  // because TWriter writes all components owned by AGroupBox
  MyComponent:=TMyComponent.Create(AGroupBox);
  with MyComponent do begin
    Name:='MyComponent';
    Parent:=AGroupBox;
  end;
end;

procedure TStreamDemoForm.StreamAsLFMCheckBoxChange(Sender: TObject);
begin
  ShowStreamInMemo;
end;

procedure TStreamDemoForm.ShowStreamInMemo;
var
  LRSStream: TMemoryStream;
  LFMStream: TMemoryStream;
begin
  if StreamAsLFMCheckBox.Checked then begin
    // convert the stream to LFM
    LRSStream:=TMemoryStream.Create;
    LFMStream:=TMemoryStream.Create;
    try
      ReadStreamFromString(LRSStream);
      LRSObjectBinaryToText(LRSStream,LFMStream);
      StreamMemo.Lines.Text:=ReadStringFromStream(LFMStream);
    finally
      LRSStream.Free;
      LFMStream.Free;
    end;
  end else begin
    // the stream is in binary format and contains characters, that can not be
    // shown in the memo. Convert all special characters to hexnumbers.
    StreamMemo.Lines.Text:=DbgStr(StreamAsString);
  end;
end;

procedure TStreamDemoForm.SaveStreamAsString(AStream: TStream);
begin
  StreamAsString:=ReadStringFromStream(AStream);
  ShowStreamInMemo;
end;

procedure TStreamDemoForm.ReadStreamFromString(AStream: TStream);
begin
  AStream.Size:=0;
  if StreamAsString<>'' then
    AStream.Write(StreamAsString[1],length(StreamAsString));
  AStream.Position:=0;
end;

function TStreamDemoForm.ReadStringFromStream(AStream: TStream): string;
begin
  AStream.Position:=0;
  SetLength(Result,AStream.Size);
  if Result<>'' then
    AStream.Read(Result[1],length(Result));
end;

procedure TStreamDemoForm.ClearDestinationGroupBox;
{ free all components owned by DestinationGroupBox
  Do not confuse 'Owner' and 'Parent';
  The 'Owner' of a TComponent is responsible for freeing the component.
  All components owned by a component can be found in its 'Components'
  property.
  The 'Parent' of a TControl is the visible container. For example
  DestinationGroupBox has as Parent the form (StreamDemoForm).
  All controls with the same parent are gathered in Parent.Controls.
  
  In this simple example the created component has as Owner and Parent the
  DestinationGroupBox.
}
begin
  while DestinationGroupBox.ComponentCount>0 do
    DestinationGroupBox.Components[0].Free;
end;

procedure TStreamDemoForm.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName,'TGroupBox')=0 then
    ComponentClass:=TGroupBox
  else if CompareText(AClassName,'TCheckBox')=0 then
    ComponentClass:=TCheckBox
  else if CompareText(AClassName,'TMyComponent')=0 then
    ComponentClass:=TMyComponent;
end;

{ TMyComponent }

procedure TMyComponent.SetWideStr1(const AValue: widestring);
begin
  if FWideStr1=AValue then exit;
  FWideStr1:=AValue;
end;

procedure TMyComponent.SetDefaultText(const AValue: WideString);
begin
  if FDefaultText=AValue then exit;
  FDefaultText:=AValue;
end;

function TMyComponent.WideStr1IsStored: boolean;
begin
  Result:=true;
end;

procedure TMyComponent.ReadText(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetDefaultText(Reader.ReadString);
  else
    SetDefaultText(Reader.ReadWideString);
  end;
end;

procedure TMyComponent.WriteText(Writer: TWriter);
begin
  Writer.WriteWideString(FDefaultText);
end;

procedure TMyComponent.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('WideDefaultText', @ReadText, @WriteText, FDefaultText <> 'Node');
end;

constructor TMyComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FWideStr1:='';
end;

initialization
  {$I mainunit.lrs}

end.

