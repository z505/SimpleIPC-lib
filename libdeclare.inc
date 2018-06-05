{ Include file for common declarations used in DLL and executable code

  Copyright Z505 Software

  License: bsd/mit }

type
  TCallbackString = procedure(s: pansichar); cdecl;
  // TODO
  // TCallbackStringW = procedure(s: pwidechar); cdecl;
  TCallbackInt32 = procedure(x: int32); cdecl;
  TCallbackXY = procedure(x,y: int32); cdecl;
  TCallbackInts = procedure(x1,x2,x3,x4: int32); cdecl;
  TCallbackIntStr = procedure(i: int32; s: pansichar); cdecl;
  // TODO
  // TCallbackIntStrW = procedure(i: int32; s: pwidechar); cdecl;
const
  MSG_NO_READ = 0;
  MSG_READ = 1;

const
  OPT_NO_THREAD = 0; // todo: document these constants
  OPT_THREAD = 1;

const
  //Message types
  mtUnknown = 0;  // unknown message, const taken from simpleipc unit
  mtString = 1;   // string message, const taken from simpleipc unit
  mtInt32 = 2;    // integer, long
  mtXY = 3;       // x and y integer pair
  mtInts = 4;     // four integers
  mtIntStr = 5;   // integer and string combination
{ Todo: ----------------------------------------------
  mtStringW // widechar
  mtIntStrW // ..
  mtXYStr        // 2 integers and a string
  mtIntsStr      // four integers and a string
  mtServCmd      // server command: i.e. stop server
  ------------------------------------------------------- }

const
  TEST_SERVER_ID = '12345test';

