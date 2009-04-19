{$I qtdefines.inc}
{$IFDEF USE_QT_45}
{$i qt45.pas}
{$ELSE}
{$IFDEF USE_QT_44}
{$i qt44.pas}
{$ELSE}
{$i qt43.pas}
{$ENDIF}
{$ENDIF}
