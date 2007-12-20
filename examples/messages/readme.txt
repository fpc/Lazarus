This example demonstrates usage of PostMessage, SendMessage and message methods in Lazarus and FreePascal.

PostMessage and SendMessage are not fully compatible with windows. For example passing windows messages to 
controls will not do anything except return of this message to message handler (for example if you pass
WM_PAINT to control - it will not paint itself).

The only use of PostMessage and SendMessage outside windows consist in ability to call your message handlers.

SendMessage calls message handler and doesnot return until message become processed by message handler.
PostMessage adds message to the message queue and doenot wait while message become processed.

Please do not use message from windows system range. Use only messages >= LM_USER.

For more info read comments in code.