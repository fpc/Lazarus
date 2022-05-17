
Following comment contributed by Forum user mgc in May 2022

The problems in the example code are:
- Opensslsockets is needed in the Uses clause;
- Error in these lines:
   Add(BoolToStr(Entry.capabilities.canEdit,'Yes','No'));
   Add(Entry.lastModifyingUser.displayName);

After compiling it worked, but not always. Many, many times I got the error “google.ini is being used for another process”. Obviously I am using Windows. Then I vanished the Inifiles unit and incorporated the ClientID and ClientSecret into the code and the problem disappeared. However, the performance is still very poor. Sometimes the program freezes, sometimes get fatal errors. Sometimes the program takes forever to close using the X button or Alt+F4.

Going to Linux has not make things better.  The performance is still very poor.

I watched the request.log file and notice that its size keeps growing on minutes after the request to the API. Meanwhile, sometimes it has an “Internal server error”. I concluded that this problem happens due to the large number of folders and sub-folders in my drive. Each folder is a call to the API which makes the process very slow and inefficient.
Least but not last, any sub-folder request results in a “bad request”.

I hope this contribution helped to update the API’s FP client.

Thanks.

Mauricio Camargo
