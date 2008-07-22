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
    Demonstrates how to write your own property editors to access readonly
    properties.
}
unit Example3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTTICtrls,
  StdCtrls, PropEdits;

type
  { TBall - a class with some readonly properties and a procedure to set the
    properties. }

  TBall = class(TPersistent)
  private
    FX: integer;
    FY: integer;
    FSize: word;
  public
    procedure SetBall(const NewX, NewY: integer; const NewSize: word);
  published
    // published readonly properties
    property X: integer read FX;
    property Y: integer read FY;
    property Size: word read FSize;
  end;
  
  { TBallPropertyEditor - a property editor for the TBall properties }
  
  TBallPropertyEditor = class(TIntegerPropertyEditor)
  public
    procedure SetValue(const NewValue: string);  override;
  end;

  
  { TForm1 }

  TForm1 = class(TForm)
    SizeTIEdit: TTIEdit;
    XLabel: TLabel;
    YLabel: TLabel;
    SizeLabel: TLabel;
    XTIEdit: TTIEdit;
    YTIEdit: TTIEdit;
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
    procedure Form1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Ball1: TBall;
  end; 

var
  Form1: TForm1; 

implementation

{ TBall }

procedure TBall.SetBall(const NewX, NewY: integer; const NewSize: word);
begin
  if (FX=NewX) and (FY=NewY) and (FSize=NewSize) then exit;
  FX:=NewX;
  FY:=NewY;
  FSize:=NewSize;
  Form1.Invalidate;
end;

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
begin
  Ball1:=TBall.Create;
  Ball1.SetBall(200,100,20);
  
  XTIEdit.Link.SetObjectAndProperty(Ball1,'X');
  YTIEdit.Link.SetObjectAndProperty(Ball1,'Y');
  SizeTIEdit.Link.SetObjectAndProperty(Ball1,'Size');
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  // unlink properties
  XTIEdit.Link.TIObject:=nil;
  YTIEdit.Link.TIObject:=nil;
  SizeTIEdit.Link.TIObject:=nil;

  Ball1.Free;
end;

procedure TForm1.Form1Paint(Sender: TObject);
begin
  with Canvas do begin
    Brush.Color:=clBlue;
    Ellipse(Ball1.X-Ball1.Size,Ball1.Y-Ball1.Size,
            Ball1.X+Ball1.Size,Ball1.Y+Ball1.Size);
  end;
end;

{ TBallPropertyEditor }

procedure TBallPropertyEditor.SetValue(const NewValue: string);
var
  L: integer;
  Ball: TBall;
  X: integer;
  Y: integer;
  Size: word;
  PropName: String;
begin
  L := StrToIntDef(NewValue,0);
  Ball:=GetComponent(0) as TBall;
  PropName:=GetName;
  if CompareText(PropName,'X')=0 then X:=L else X:=Ball.X;
  if CompareText(PropName,'Y')=0 then Y:=L else Y:=Ball.Y;
  if CompareText(PropName,'Size')=0 then Size:=Word(L) else Size:=Ball.Size;
  Ball.SetBall(X,Y,Size);
end;

initialization
  {$I example3.lrs}
  RegisterPropertyEditor(TypeInfo(integer),TBall,'X',TBallPropertyEditor);
  RegisterPropertyEditor(TypeInfo(integer),TBall,'Y',TBallPropertyEditor);
  RegisterPropertyEditor(TypeInfo(word),TBall,'Size',TBallPropertyEditor);

end.

