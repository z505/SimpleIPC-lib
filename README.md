# SimpleIPC-lib
Simple IPC (interprocess communication) for any program shipped as a dll/so, or possibly linked in statically with .a/.h files

Why is IPC so difficult and why do so many people end up reinventing their own IPC mechanisms?

This is an attempt to make IPC simple.
* Any language (C/C++/Delphi/Rust/Python/etc.) should be able to simply ship a DLL with the application that handles all the IPC mechanisms.
* Then you just send a string, integer, boolean with a simple function.
* The receiving program also has the DLL loaded and can communicate with any program that has the SimpleIPC dll loaded into it.

First goals of this project:
* show a Golang exe communicating with a Lazarus/FPC exe
* show a Golang exe communicating with a C++/C exe
* show a Python program communicating with a Golang/FPC exe
* show some more languages
* show that it works on linux/bsd/macOS/windows (all the popular platforms)

Analogy: any program that has a sqlite dll loaded, can communicate with an sqlite database..
Why not have something similar for IPC (inter process communication)?
All the programmer needs to do is load the SimpleIPC DLL into his exe/elf, then he can communicate with any other exe/elf that also has the simpleipc dll loaded.
Or, alternatively, one does not have to load a DLL, and can link the SimpleIPC code in statically with .a/.h files possibly in the future, once again similar to sqlite.

More advanced IPC: string with records or structs and their types, or tuples. This is in addtion to the simple ones like OnInteger OnString ipc messages.

Adcantages and great uses of IPC:
* programmer can make a plugin system that allows you to compile separate exe's as your plugins (really modular) instead of using DLL's.
* reduces the chance of the main program crashing. Each exe is it's own process, so if one fails, the main exe does not fail and just the IPC exe fails
* multiple processes could theoretically be used across multiple CPU's if this is of any performance benefit to you
* less need for threads and dangerous/risky programming tactics
* allows exceptions to be caught in each executable instead of exceptions in a dll
* allows multiple programming languages to be used for each executable if there is an existing codebase out there that you need to use for one executable, while you still want to program another executabe in another language.  This is complex and a disadvatage compared to writing in one language, but sometimes the problem in software development that is addressed by having the tool available for it since codebases out there exist in other languages that one wants to incorporate into your project.

Alternative to: zeromq, corba, windows SendMessage/PostMessage, DLL based plugin systems, remote procedure calls, and many other more complicated ipc mechanisms

Thanks to: SimpleIpc FPC unit authors (Michael Van Canneyt and others) since this code  wraps that code they wrote for one compiler purpose
