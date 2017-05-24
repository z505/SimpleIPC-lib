{ Project to make SimpleIPC features of fpc inside a DLL so that any language
  can use simpleipc facilities for inter process communication

  Example: load fpc simpleipc dll into golang and communicate with fpc between
  two exe's. Then no DLL plugin is needed for a plugin system, separate exe's
  can be used.

  Todo: rename to dynsIpc.lpr

  Notes:
    -try also AdvancedIPC unit for more features not available in simpleipc
    -From fpc docs: "a SimpleIPC client in a desktop application cannot connect
     to a SimpleIPC server in a service application", so this will need to be
     noted if DLL is ever being used in a service
    -Exceptions - catch all common exceptions in DLL and return errors other
     ways (codes), do not let exception be handled in the exe as
     c/c++/delphi/other languages do not have a comptable exception mechanism
     or shared memory manager with fpc.

 Copyright Z505 Software

 License: bsd/mit }

library dynsimpleipc;
{$mode objfpc} {$H+}

uses
  mainipc,   // todo: rename to sIpcServ
  sIpcClient;

begin
// leave empty, this is a DLL
end.

