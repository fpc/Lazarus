
Important:

  This example uses the JPEGForLazarus package (see in the directory above).
  You must first open once the package jpegforlazarus.lpk in the IDE, so that
  the IDE knows, where to find the lpk file.


To use jpeg in your own applications:

 - Add JPEGForLazarus to the dependencies of your project.

   Project Menu -> Project Inspector -> Add -> New Requirement -> Package Name:
     JPEGForLazarus
   Click OK.
   
 - Add "lazjpeg" to the uses section of any unit, where jpeg is needed.
   Add this unit at least once, so that jpeg is registered and
   Open/SavePictureDialogs and TPicture can open jpeg.
     

The code is under LGPL2. That means you can use it freely in your
applications, even commercial ones.
  

For further questions:
  lazarus@miraclec.com
