{ TImage

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

constructor TImage.Create(AOwner: TComponent);
begin
   inherited;
   fCompStyle := csImage;
   FPicture := TPicture.Create;
   FPicture.OnChange := @PictureChanged;
   Setbounds(0,0,100,100);
end;

procedure TImage.SetPicture(const AValue: TPicture);
begin
   FPicture.Assign(AValue);  //the onchange of the picture gets called and notifies that something changed.
end;

Procedure TImage.PictureChanged(Sender : TObject);
begin
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

