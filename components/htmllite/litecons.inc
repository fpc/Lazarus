{Version 7.25}
{Do not use this file with C++Builder 5}

{$IFDEF HL_LAZARUS}

{$mode ObjFPC}{$H+}
{$Define ver100_plus}
{$Define ver120_plus}
{$Define NoGIF}
{$Define NoThreads}

{$ELSE}

{$A+,B-,F-,G+,I+,P+,T-,V+,X+,R-}

{$ifdef Win32}
  {$J+}  {typed constants are modifiable}
  {$H+}  {LongStrings On}
{$endif}

{$ifndef DebugIt}
  {$W-}  {Stack frames off}
  {$Q-}  {overflow checking off}
  {$S-}  {stack checking off}
  {$C-}  {Assertions off}
  {$ifdef Win32}
    {$O+}  {optimization on}
  {$endif}
{$else}
  {$W+}  {Stack frames on}
  {$Q+}  {overflow checking on}
  {$S+}  {stack checking on}
  {$C+}  {Assertions on}
  {$ifdef Win32}
    {$O-}  {optimization off}
  {$endif}
{$endif}

{$Define Delphi6_Plus}

{$ifdef ver100}   {Delphi 3}
{$Define ver100_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver110}    {C++Builder 3}
{$ObjExportAll On}
{$Define CppBuilder}
{$Define ver100_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef Ver120}   {Delphi 4}
{$Define ver100_plus}
{$Define ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver125}    {C++Builder 4}
{$ObjExportAll On}
{$Define CppBuilder}
{$Define ver100_plus}
{$Define ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef Ver130}   {Delphi 5}
{$Define ver100_plus}
{$Define ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef Ver140}   {Delphi 6}
{$Define ver100_plus}
{$Define ver120_plus}
  {$Warn Symbol_Platform Off}
{$endif}

{$ifdef Ver150}   {Delphi 7}
{$Define ver100_plus}
{$Define ver120_plus}
  {$Warn Symbol_Platform Off}
{$endif}


{.$Define NoGIF} {To eliminate GIF image capability, define "NoGIF" by
           removing the '.'.}
           
{$DEFINE HL_INTERFACE}
{$DEFINE HL_IMPLEMENTATION}

{$ENDIF}
