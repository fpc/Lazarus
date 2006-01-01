{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

const
   GXID_CLAIM_DEVICE = 1;
   GXID_RELEASE_DEVICE = 2;
   GXID_RETURN_OK = 0;
   GXID_RETURN_ERROR = -(1);

type
   PGxidMessage = ^TGxidMessage;

   PGxidU32 = ^TGxidU32;
   TGxidU32 = dword;

   PGxidI32 = ^TGxidI32;
   TGxidI32 = longint;
   TGxidClaimDevice = record
        _type : TGxidU32;
        length : TGxidU32;
        device : TGxidU32;
        window : TGxidU32;
        exclusive : TGxidU32;
     end;

   TGxidReleaseDevice = record
        _type : TGxidU32;
        length : TGxidU32;
        device : TGxidU32;
        window : TGxidU32;
     end;

   TGxidMessageAny = record
        _type : TGxidU32;
        length : TGxidU32;
     end;

   TGxidMessage = record
       case longint of
          0 : ( any : TGxidMessageAny );
          1 : ( claim : TGxidClaimDevice );
          2 : ( release : TGxidReleaseDevice );
       end;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF read_implementation}

