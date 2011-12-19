library androidlcltest;

{$mode objfpc}{$H+}
{$define Android}

uses
  {$ifdef Android}
  customdrawnint,
  {$endif}
  Interfaces,
  Forms,
  mainform, secondform;

{$ifdef Android}
exports
  Java_com_pascal_lclproject_LCLActivity_LCLOnTouch name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTouch',
  Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap name 'Java_com_pascal_lcltest_LCLActivity_LCLDrawToBitmap',
  Java_com_pascal_lclproject_LCLActivity_LCLOnCreate name 'Java_com_pascal_lcltest_LCLActivity_LCLOnCreate',
  Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished name 'Java_com_pascal_lcltest_LCLActivity_LCLOnMessageBoxFinished',
  Java_com_pascal_lclproject_LCLActivity_LCLOnKey name 'Java_com_pascal_lcltest_LCLActivity_LCLOnKey',
  Java_com_pascal_lclproject_LCLActivity_LCLOnTimer name 'Java_com_pascal_lcltest_LCLActivity_LCLOnTimer',
  Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged name 'Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged',
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
end.

