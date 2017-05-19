type
  TCallbackString = procedure(s: pchar); cdecl;
  TCallbackInt32 = procedure(x: int32); cdecl;
  TCallbackXY = procedure(x,y: int32); cdecl;
  TCallbackX4 = procedure(x1,x2,x3,x4: int32); cdecl;
  TCallbackIntStr = procedure(i: int32; s: pchar); cdecl;

const
  MSG_NO_READ = 0;
  MSG_READ = 1;

const
  OPT_NO_THREAD = 0; // todo: document these constants
  OPT_THREAD = 1;

const
  //Message types
  mtUnknown = 0; // unknown message, const taken from simpleipc unit
  mtString = 1;  // string message, const taken from simpleipc unit
  mtInt32 = 2;   // integer, long
  mtXY = 3;      // x and y integer pair
  mtX4 = 4;      // four integers
  mtIntStr = 5;  // integer and string combination



