{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jcfidelazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  JcfIdeMain, JcfIdeRegister, frFiles, frObfuscateSettings, frClarify, 
  frClarifySpaces, frClarifyIndent, frBlankLines, frClarifyAlign, 
  frClarifyLongLineBreaker, frClarifyReturns, frCompilerDirectReturns, 
  frClarifyBlocks, frClarifyCaseBlocks, frComments, frWarnings, 
  frReservedCapsSettings, frAnyCapsSettings, frIdentifierCapsSettings, 
  frNotIdentifierCapsSettings, frUnitCaps, frReplace, frUses, frTransform, 
  frAsm, frPreProcessor, Diff, diffmerge, EditorConverter, Delay, JcfHelp, 
  JcfUiToolsGUI, fAbout, fJcfErrorDisplay, JcfUIConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JcfIdeRegister', @JcfIdeRegister.Register);
end;

initialization
  RegisterPackage('jcfidelazarus', @Register);
end.
