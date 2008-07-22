{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Example for RTTI controls.
    Demonstrates, how to define a custom class with custom types and connecting
    it to RTTI controls.
}
unit Example2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, TypInfo, Forms, Controls, Graphics,
  Dialogs, RTTICtrls, StdCtrls;

type
  TMyEnum = (MyEnum1,MyEnum2,MyEnum3);
  TMyRange = 3..7;

  TMyClass = class(TPersistent)
  private
    FMyEnum: TMyEnum;
    FMyRange: TMyRange;
    FMyString: string;
    procedure SetMyEnum(const AValue: TMyEnum);
    procedure SetMyRange(const AValue: TMyRange);
    procedure SetMyString(const AValue: string);
  published
    property MyString: string read FMyString write SetMyString;
    property MyEnum: TMyEnum read FMyEnum write SetMyEnum;
    property MyRange: TMyRange read FMyRange write SetMyRange;
  end;

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    TIEdit1: TTIEdit;
    TIRadioGroup1: TTIRadioGroup;
    TITrackBar1: TTITrackBar;
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    MyObject: TMyClass;
  end; 

var
  Form1: TForm1; 
  
procedure Log(const Msg: string);

implementation

procedure Log(const Msg: string);
begin
  Form1.Memo1.Lines.Add(Msg);
end;

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
begin
  // create MyObject
  MyObject:=TMyClass.Create;
  // link properties
  TIEdit1.Link.SetObjectAndProperty(MyObject,'MyString');
  TIRadioGroup1.Link.SetObjectAndProperty(MyObject,'MyEnum');
  TITrackBar1.Link.SetObjectAndProperty(MyObject,'MyRange');
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  // unlink properties
  TIEdit1.Link.TIObject:=nil;
  TIRadioGroup1.Link.TIObject:=nil;
  TITrackBar1.Link.TIObject:=nil;
  // free MyObject
  MyObject.Free;
end;

{ TMyClass }

procedure TMyClass.SetMyEnum(const AValue: TMyEnum);
begin
  if AValue=MyEnum then exit;
  FMyEnum:=AValue;
  Log('TMyClass.SetMyEnum '+GetEnumProp(Self,'MyEnum'));
end;

procedure TMyClass.SetMyRange(const AValue: TMyRange);
begin
  if AValue=MyRange then exit;
  FMyRange:=AValue;
  Log('TMyClass.SetMyRange '+IntToStr(MyRange));
end;

procedure TMyClass.SetMyString(const AValue: string);
begin
  if AValue=MyString then exit;
  FMyString:=AValue;
  Log('TMyClass.SetMyString '+MyString);
end;

initialization
  {$I example2.lrs}

end.

