 #define Min(a, b)     ((a) < (b) ? (a) : (b))

static HANDLE LocalReAllocDS(HANDLE hMem, WORD wBytes, WORD wFlags,
                                                                 WORD TheDS)
{
  WORD SavedDS;
  HANDLE ReturnValue;

  SavedDS = _DS;
  _DS = TheDS;
  ReturnValue = LocalReAlloc(hMem, wBytes, wFlags);
  _DS = SavedDS;
  return ReturnValue;
}

static LPSTR LocalLockDS(HANDLE hMem, WORD TheDS)
{
  WORD SavedDS;
  LPSTR ReturnValue;

  SavedDS = _DS;
  _DS = TheDS;
  ReturnValue = (LPSTR)LocalLock(hMem);
  _DS = SavedDS;
  return ReturnValue;
}

static BOOL LocalUnlockDS(HANDLE hMem, WORD TheDS)
{
  WORD SavedDS;
  BOOL ReturnValue;

  SavedDS = _DS;
  _DS = TheDS;
  ReturnValue = LocalUnlock(hMem);
  _DS = SavedDS;
  return ReturnValue;
}
