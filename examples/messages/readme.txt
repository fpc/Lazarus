This example demonstrates the usage of PostMessage, SendMessage and the message methods in Lazarus and FreePascal.

PostMessage and SendMessage are not fully compatible with windows. For example passing windows messages to 
controls will not do anything except pass the message along to a message handler (for example if you pass
WM_PAINT to a control it will not paint itself).

The only use of PostMessage and SendMessage outside of Windows is to call your own custom message handlers.

SendMessage calls the message handler and does not return until the message is processed by the message handler.
PostMessage adds your message to the message queue and returns immediatly without waiting for the message to
be processed.

Please do not use any messages in the Windows system range. Use only messages >= LM_USER.

For more info read the comments in the code.
