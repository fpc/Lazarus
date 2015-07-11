These example demonstrates the usage of PostMessage, SendMessage and the message methods in Lazarus and FreePascal and its widgetset independent counterpart, the direct method call and QueueAsyncCall

SendMessage and PostMessage
===========================
PostMessage and SendMessage are not fully compatible with windows. For example passing windows messages to 
controls will not do anything except pass the message along to a message handler (for example if you pass
WM_PAINT to a control it will not paint itself).

The only use of PostMessage and SendMessage outside of Windows is to call your own custom message handlers.

SendMessage calls the message handler and does not return until the message is processed by the message handler.
PostMessage adds your message to the message queue and returns immediatly without waiting for the message to
be processed.

Please do not use any messages in the Windows system range. Use only messages >= LM_USER.

Direct method call and QueueAsyncCall
=====================================
If all you are interested it to delay the execution of a method until after an event handler has finished, then Application.QueueAsyncCall is a good alternative. More information about QueueAsyncCall can be found at http://wiki.lazarus.freepascal.org/Asynchronous_Calls

The AsyncCall example shows the use of QueueAsyncCall.

For more info read the comments in the code.
