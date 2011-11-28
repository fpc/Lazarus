library androidlcltest;

{$mode objfpc}{$H+}
{$define Android}

uses
  {$ifdef Android}
  cmem,
  customdrawnint,
  {$endif}
  Interfaces,
  Forms,
  mainform;

{$ifdef Android}
exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap name 'Java_com_pascal_lcltest_LCLActivity_LCLDrawToBitmap',
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

