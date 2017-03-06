{$IF FPC_VERSION = 3}
  {$IF FPC_RELEASE = 0}
    {$IF FPC_PATCH > 1}
      {$DEFINE FPC302}
    {$ENDIF}
  {$ELSE}
    {$IF FPC_PATCH > 0}
      {$DEFINE FPC311}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
