unit RichMemoFactory; 

{$mode objfpc}{$H+}

interface

{$define NoRichMemo}
{$ifdef LCLWin32}{$undef NoRichMemo}{$endif}
{$ifdef LCLCarbon}{$undef NoRichMemo}{$endif}
{$ifdef LCLGtk2}{$undef NoRichMemo}{$endif}
{$ifdef LCLCocoa}{$undef NoRichMemo}{$endif}
{$ifdef LCLQt}{$undef NoRichMemo}{$endif}
{$ifdef LCLQt5}{$undef NoRichMemo}{$endif}
{$ifdef LCLQt6}{$undef NoRichMemo}{$endif}

uses
  WSLCLClasses,
  RichMemo
  {$ifdef NoRichMemo},WSRichMemo{$endif}
  {$ifdef LCLWin32},Win32RichMemo{$endif}
  {$ifdef LCLCarbon},CarbonRichMemo{$endif}

  {$ifdef LCLGtk2}
  //Since Gtk doesn't have a native RTF loader, RichMemo provides
  //a widgetset independent loader. It's registered by default for
  //Gtk. The registration can be prevented by compiling
  //with RichMemoNoDefaultRTFLoader defined.
  //or setting RTFLoadStream to nil (or any other routine) in runtime.
  ,RichMemoRTF, Gtk2RichMemo
  {$endif}
  {$ifdef LCLCocoa},CocoaRichMemo{$endif}
  {$ifdef LCLQt},QtRichMemo{$endif}
  {$ifdef LCLQt5},Qt5RichMemo{$endif}
  {$ifdef LCLQt6},Qt6RichMemo{$endif}
  ;

function RegisterCustomRichMemo: Boolean;

implementation

function RegisterCustomRichMemo: Boolean; alias : 'WSRegisterCustomRichMemo';
begin
  Result := True;
  {$ifdef LCLWin32}RegisterWSComponent(TCustomRichMemo, TWin32WSCustomRichMemo);{$endif}
  {$ifdef LCLCarbon}RegisterWSComponent(TCustomRichMemo, TCarbonWSCustomRichMemo);{$endif}
  {$ifdef LCLGtk2}
  RegisterWSComponent(TCustomRichMemo, TGtk2WSCustomRichMemo);
  {$ifndef RichMemoNoDefaultRTFLoader}
  RegisterRTFLoader;
  RegisterRTFSaver;
  {$endif}
  {$endif}
  {$ifdef LCLCocoa}RegisterWSComponent(TCustomRichMemo, TCocoaWSCustomRichMemo);{$endif}
  {$ifdef LCLQt}RegisterWSComponent(TCustomRichMemo, TQtWSCustomRichMemo);{$endif}
  {$ifdef LCLQt5}RegisterWSComponent(TCustomRichMemo, TQtWSCustomRichMemo);{$endif}
  {$ifdef LCLQt6}RegisterWSComponent(TCustomRichMemo, TQtWSCustomRichMemo);{$endif}
  {$ifdef NoRichMemo}RegisterWSComponent(TCustomRichMemo, TWSCustomRichMemo);{$endif}
end;

initialization
 // initialization is here just to prevent compiler warning about not being used
 // the unit is actually used by providing an implementation for WSREgisterCustomRichMemo class
 // thus it shouldn't be smart-linked out by a smartlinker. hmm

end.

