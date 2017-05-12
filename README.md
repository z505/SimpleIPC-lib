# SimpleIPC-lib
Simple IPC (interprocess communication) for any program shipped as a dll/so

Why is IPC so difficult and why do so many people end up reinventing their own IPC mechanisms?

This is an attempt to make IPC simple. Any language (C/C++/Delphi/Rust/Python/etc.) should be able to simply ship a DLL with the application that handles all the IPC mechanisms... Then you just send a string, integer, boolean with a simple function. The receiving program also has the DLL loaded and can communicate with any program that has the SimpleIPC dll loaded into it.

So it is similar to say a program that has a sqlite dll can communicate with an sqlite database.. Why not have something similar for IPC (inter process communication)? All the programmer needs to do is load the SimpleIPC DLL into his exe, then he can communicate with any other exe/elf that also has the simpleipc dll loaded.

More advanced IPC: google protocol buffers in combination with simpleipc means you can send standardized data using the well known google protocol buffer mechanism (GPB). The GPB does not handle IPC for you, so when you need IPC mechanism to actually send and receive the google protocol buffer, why not use SimpleIPC?

Cool things about IPC: 
* could make a plugin system that allows you to compile separate exe's as your plugins (really modular) instead of using DLL's as your plugins.
* reduces the chance of the main program crashing if you have multiple exe's. Each exe is it's own process, so if one fails, the main exe does not fail
* multiple processes could theoretically be used across multiple CPU's if this is of any performance benefit to you
* less need for threads and dangerous/risky programming tactics

Alternative to: zeromq, corba, windows SendMessage/PostMessage, DLL based plugin systems, and a million other ipc mechanisms
