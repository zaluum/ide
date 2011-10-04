

typedef u_int64_t ViUInt64;
typedef int64_t ViInt64;
typedef ViUInt64 * ViPUInt64;
typedef ViUInt64 * ViAUInt64;
typedef ViInt64 * ViPInt64;
typedef ViInt64 * ViAInt64;



typedef unsigned int ViUInt32;
typedef signed int ViInt32;





typedef ViUInt32 * ViPUInt32;
typedef ViUInt32 * ViAUInt32;
typedef ViInt32 * ViPInt32;
typedef ViInt32 * ViAInt32;

typedef unsigned short ViUInt16;
typedef ViUInt16 * ViPUInt16;
typedef ViUInt16 * ViAUInt16;

typedef signed short ViInt16;
typedef ViInt16 * ViPInt16;
typedef ViInt16 * ViAInt16;

typedef unsigned char ViUInt8;
typedef ViUInt8 * ViPUInt8;
typedef ViUInt8 * ViAUInt8;

typedef signed char ViInt8;
typedef ViInt8 * ViPInt8;
typedef ViInt8 * ViAInt8;

typedef char ViChar;
typedef ViChar * ViPChar;
typedef ViChar * ViAChar;

typedef unsigned char ViByte;
typedef ViByte * ViPByte;
typedef ViByte * ViAByte;

typedef void * ViAddr;
typedef ViAddr * ViPAddr;
typedef ViAddr * ViAAddr;

typedef float ViReal32;
typedef ViReal32 * ViPReal32;
typedef ViReal32 * ViAReal32;

typedef double ViReal64;
typedef ViReal64 * ViPReal64;
typedef ViReal64 * ViAReal64;

typedef ViPByte ViBuf;
typedef ViPByte ViPBuf;
typedef ViPByte * ViABuf;

typedef ViPChar ViString;
typedef ViPChar ViPString;
typedef ViPChar * ViAString;

typedef ViString ViRsrc;
typedef ViString ViPRsrc;
typedef ViString * ViARsrc;

typedef ViUInt16 ViBoolean;
typedef ViBoolean * ViPBoolean;
typedef ViBoolean * ViABoolean;

typedef ViInt32 ViStatus;
typedef ViStatus * ViPStatus;
typedef ViStatus * ViAStatus;

typedef ViUInt32 ViVersion;
typedef ViVersion * ViPVersion;
typedef ViVersion * ViAVersion;

typedef ViUInt32 ViObject;
typedef ViObject * ViPObject;
typedef ViObject * ViAObject;

typedef ViObject ViSession;
typedef ViSession * ViPSession;
typedef ViSession * ViASession;

typedef ViUInt32 ViAttr;


typedef const ViChar * ViConstString;
typedef ViObject ViEvent;
typedef ViEvent * ViPEvent;
typedef ViObject ViFindList;
typedef ViFindList * ViPFindList;


typedef ViUInt64 ViBusAddress;
typedef ViUInt64 ViBusSize;
typedef ViUInt64 ViAttrState;







typedef ViUInt64 ViBusAddress64;
typedef ViBusAddress64 * ViPBusAddress64;


typedef ViUInt32 ViEventType;
typedef ViEventType * ViPEventType;
typedef ViEventType * ViAEventType;
typedef void * ViPAttrState;
typedef ViAttr * ViPAttr;
typedef ViAttr * ViAAttr;

typedef ViString ViKeyId;
typedef ViPString ViPKeyId;
typedef ViUInt32 ViJobId;
typedef ViJobId * ViPJobId;
typedef ViUInt32 ViAccessMode;
typedef ViAccessMode * ViPAccessMode;
typedef ViBusAddress * ViPBusAddress;
typedef ViUInt32 ViEventFilter;

//typedef va_list ViVAList;

typedef ViStatus ( * ViHndlr)
   (ViSession vi, ViEventType eventType, ViEvent event, ViAddr userHandle);



ViStatus viOpenDefaultRM (ViPSession vi);
ViStatus viFindRsrc (ViSession sesn, ViString expr, ViPFindList vi,
                                    ViPUInt32 retCnt, ViChar desc[]);
ViStatus viFindNext (ViFindList vi, ViChar desc[]);
ViStatus viParseRsrc (ViSession rmSesn, ViRsrc rsrcName,
                                    ViPUInt16 intfType, ViPUInt16 intfNum);
ViStatus viParseRsrcEx (ViSession rmSesn, ViRsrc rsrcName, ViPUInt16 intfType,
                                    ViPUInt16 intfNum, ViChar rsrcClass[],
                                    ViChar expandedUnaliasedName[],
                                    ViChar aliasIfExists[]);
ViStatus viOpen (ViSession sesn, ViRsrc name, ViAccessMode mode,
                                    ViUInt32 timeout, ViPSession vi);



ViStatus viClose (ViObject vi);
ViStatus viSetAttribute (ViObject vi, ViAttr attrName, ViAttrState attrValue);
ViStatus viGetAttribute (ViObject vi, ViAttr attrName, void * attrValue);
ViStatus viStatusDesc (ViObject vi, ViStatus status, ViChar desc[]);
ViStatus viTerminate (ViObject vi, ViUInt16 degree, ViJobId jobId);

ViStatus viLock (ViSession vi, ViAccessMode lockType, ViUInt32 timeout,
                                    ViKeyId requestedKey, ViChar accessKey[]);
ViStatus viUnlock (ViSession vi);
ViStatus viEnableEvent (ViSession vi, ViEventType eventType, ViUInt16 mechanism,
                                    ViEventFilter context);
ViStatus viDisableEvent (ViSession vi, ViEventType eventType, ViUInt16 mechanism);
ViStatus viDiscardEvents (ViSession vi, ViEventType eventType, ViUInt16 mechanism);
ViStatus viWaitOnEvent (ViSession vi, ViEventType inEventType, ViUInt32 timeout,
                                    ViPEventType outEventType, ViPEvent outContext);
ViStatus viInstallHandler(ViSession vi, ViEventType eventType, ViHndlr handler,
                                    ViAddr userHandle);
ViStatus viUninstallHandler(ViSession vi, ViEventType eventType, ViHndlr handler,
                                      ViAddr userHandle);



ViStatus viRead (ViSession vi, ViPBuf buf, ViUInt32 cnt, ViPUInt32 retCnt);
ViStatus viReadAsync (ViSession vi, ViPBuf buf, ViUInt32 cnt, ViPJobId jobId);
ViStatus viReadToFile (ViSession vi, ViConstString filename, ViUInt32 cnt,
                                    ViPUInt32 retCnt);
ViStatus viWrite (ViSession vi, ViBuf buf, ViUInt32 cnt, ViPUInt32 retCnt);
ViStatus viWriteAsync (ViSession vi, ViBuf buf, ViUInt32 cnt, ViPJobId jobId);
ViStatus viWriteFromFile (ViSession vi, ViConstString filename, ViUInt32 cnt,
                                    ViPUInt32 retCnt);
ViStatus viAssertTrigger (ViSession vi, ViUInt16 protocol);
ViStatus viReadSTB (ViSession vi, ViPUInt16 status);
ViStatus viClear (ViSession vi);



ViStatus viSetBuf (ViSession vi, ViUInt16 mask, ViUInt32 size);
ViStatus viFlush (ViSession vi, ViUInt16 mask);

ViStatus viBufWrite (ViSession vi, ViBuf buf, ViUInt32 cnt, ViPUInt32 retCnt);
ViStatus viBufRead (ViSession vi, ViPBuf buf, ViUInt32 cnt, ViPUInt32 retCnt);

ViStatus viPrintf (ViSession vi, ViString writeFmt, ...);
//ViStatus viVPrintf (ViSession vi, ViString writeFmt, ViVAList params);
ViStatus viSPrintf (ViSession vi, ViPBuf buf, ViString writeFmt, ...);
//ViStatus viVSPrintf (ViSession vi, ViPBuf buf, ViString writeFmt,
//                                    ViVAList parms);

ViStatus viScanf (ViSession vi, ViString readFmt, ...);
//ViStatus viVScanf (ViSession vi, ViString readFmt, ViVAList params);
ViStatus viSScanf (ViSession vi, ViBuf buf, ViString readFmt, ...);
//ViStatus viVSScanf (ViSession vi, ViBuf buf, ViString readFmt,
//                                    ViVAList parms);

ViStatus viQueryf (ViSession vi, ViString writeFmt, ViString readFmt, ...);
//ViStatus viVQueryf (ViSession vi, ViString writeFmt, ViString readFmt,
//                                    ViVAList params);



ViStatus viIn8 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViPUInt8 val8);
ViStatus viOut8 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViUInt8 val8);
ViStatus viIn16 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViPUInt16 val16);
ViStatus viOut16 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViUInt16 val16);
ViStatus viIn32 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViPUInt32 val32);
ViStatus viOut32 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViUInt32 val32);


ViStatus viIn64 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViPUInt64 val64);
ViStatus viOut64 (ViSession vi, ViUInt16 space,
                                    ViBusAddress offset, ViUInt64 val64);

ViStatus viIn8Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViPUInt8 val8);
ViStatus viOut8Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViUInt8 val8);
ViStatus viIn16Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViPUInt16 val16);
ViStatus viOut16Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViUInt16 val16);
ViStatus viIn32Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViPUInt32 val32);
ViStatus viOut32Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViUInt32 val32);
ViStatus viIn64Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViPUInt64 val64);
ViStatus viOut64Ex (ViSession vi, ViUInt16 space,
                                    ViBusAddress64 offset, ViUInt64 val64);


ViStatus viMoveIn8 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt8 buf8);
ViStatus viMoveOut8 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt8 buf8);
ViStatus viMoveIn16 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt16 buf16);
ViStatus viMoveOut16 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt16 buf16);
ViStatus viMoveIn32 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt32 buf32);
ViStatus viMoveOut32 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt32 buf32);


ViStatus viMoveIn64 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt64 buf64);
ViStatus viMoveOut64 (ViSession vi, ViUInt16 space, ViBusAddress offset,
                                    ViBusSize length, ViAUInt64 buf64);

ViStatus viMoveIn8Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt8 buf8);
ViStatus viMoveOut8Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt8 buf8);
ViStatus viMoveIn16Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt16 buf16);
ViStatus viMoveOut16Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt16 buf16);
ViStatus viMoveIn32Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt32 buf32);
ViStatus viMoveOut32Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt32 buf32);
ViStatus viMoveIn64Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt64 buf64);
ViStatus viMoveOut64Ex (ViSession vi, ViUInt16 space, ViBusAddress64 offset,
                                    ViBusSize length, ViAUInt64 buf64);


ViStatus viMove (ViSession vi, ViUInt16 srcSpace, ViBusAddress srcOffset,
                                    ViUInt16 srcWidth, ViUInt16 destSpace,
                                    ViBusAddress destOffset, ViUInt16 destWidth,
                                    ViBusSize srcLength);
ViStatus viMoveAsync (ViSession vi, ViUInt16 srcSpace, ViBusAddress srcOffset,
                                    ViUInt16 srcWidth, ViUInt16 destSpace,
                                    ViBusAddress destOffset, ViUInt16 destWidth,
                                    ViBusSize srcLength, ViPJobId jobId);


ViStatus viMoveEx (ViSession vi, ViUInt16 srcSpace, ViBusAddress64 srcOffset,
                                    ViUInt16 srcWidth, ViUInt16 destSpace,
                                    ViBusAddress64 destOffset, ViUInt16 destWidth,
                                    ViBusSize srcLength);
ViStatus viMoveAsyncEx (ViSession vi, ViUInt16 srcSpace, ViBusAddress64 srcOffset,
                                    ViUInt16 srcWidth, ViUInt16 destSpace,
                                    ViBusAddress64 destOffset, ViUInt16 destWidth,
                                    ViBusSize srcLength, ViPJobId jobId);


ViStatus viMapAddress (ViSession vi, ViUInt16 mapSpace, ViBusAddress mapOffset,
                                    ViBusSize mapSize, ViBoolean access,
                                    ViAddr suggested, ViPAddr address);
ViStatus viUnmapAddress (ViSession vi);


ViStatus viMapAddressEx (ViSession vi, ViUInt16 mapSpace, ViBusAddress64 mapOffset,
                                    ViBusSize mapSize, ViBoolean access,
                                    ViAddr suggested, ViPAddr address);


void viPeek8 (ViSession vi, ViAddr address, ViPUInt8 val8);
void viPoke8 (ViSession vi, ViAddr address, ViUInt8 val8);
void viPeek16 (ViSession vi, ViAddr address, ViPUInt16 val16);
void viPoke16 (ViSession vi, ViAddr address, ViUInt16 val16);
void viPeek32 (ViSession vi, ViAddr address, ViPUInt32 val32);
void viPoke32 (ViSession vi, ViAddr address, ViUInt32 val32);


void viPeek64 (ViSession vi, ViAddr address, ViPUInt64 val64);
void viPoke64 (ViSession vi, ViAddr address, ViUInt64 val64);




ViStatus viMemAlloc (ViSession vi, ViBusSize size, ViPBusAddress offset);
ViStatus viMemFree (ViSession vi, ViBusAddress offset);


ViStatus viMemAllocEx (ViSession vi, ViBusSize size, ViPBusAddress64 offset);
ViStatus viMemFreeEx (ViSession vi, ViBusAddress64 offset);




ViStatus viGpibControlREN(ViSession vi, ViUInt16 mode);
ViStatus viGpibControlATN(ViSession vi, ViUInt16 mode);
ViStatus viGpibSendIFC (ViSession vi);
ViStatus viGpibCommand (ViSession vi, ViBuf cmd, ViUInt32 cnt, ViPUInt32 retCnt);
ViStatus viGpibPassControl(ViSession vi, ViUInt16 primAddr, ViUInt16 secAddr);

ViStatus viVxiCommandQuery(ViSession vi, ViUInt16 mode, ViUInt32 cmd,
                                     ViPUInt32 response);
ViStatus viAssertUtilSignal(ViSession vi, ViUInt16 line);
ViStatus viAssertIntrSignal(ViSession vi, ViInt16 mode, ViUInt32 statusID);
ViStatus viMapTrigger (ViSession vi, ViInt16 trigSrc, ViInt16 trigDest,
                                    ViUInt16 mode);
ViStatus viUnmapTrigger (ViSession vi, ViInt16 trigSrc, ViInt16 trigDest);
ViStatus viUsbControlOut (ViSession vi, ViInt16 bmRequestType, ViInt16 bRequest,
                                    ViUInt16 wValue, ViUInt16 wIndex, ViUInt16 wLength,
                                    ViBuf buf);
ViStatus viUsbControlIn (ViSession vi, ViInt16 bmRequestType, ViInt16 bRequest,
                                    ViUInt16 wValue, ViUInt16 wIndex, ViUInt16 wLength,
                                    ViPBuf buf, ViPUInt16 retCnt);
ViStatus viVxiServantResponse(ViSession vi, ViInt16 mode, ViUInt32 resp);
