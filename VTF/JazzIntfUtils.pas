unit JazzIntfUtils;

interface

uses
  Classes,
  {$IFNDEF LINUX}
  ComObj,
  {$ENDIF}
  SysUtils,
  TypInfo;

type
  TGuidArray = array of TGuid;

function InterfaceToObject(const Instance: IInterface): TObject;
function GetInterfaceEntry(const Instance: IInterface): PInterfaceEntry;
function GetInterfaceIID(const Instance: IInterface; var IID: TGUID): boolean; overload;
function GetInterfaceIID(const ObjectClass: TClass; var IID: TGUID): boolean; overload;
function GetPIMTOffSet(const Instance: IInterface): Integer;
procedure GetInterfaces(ObjectClass: TClass; var IIDList: TGuidArray); overload;
procedure GetInterfaces(const Instance: IInterface; var IIDList: TGuidArray); overload;

implementation

uses
  Variants;

function InterfaceToObject(const Instance: IInterface): TObject;
var
  OffSet: Integer;
begin
  OffSet:= GetPIMTOffSet(Instance);
  if OffSet > 0 then
    Result:= TObject(PAnsiChar(Instance) - OffSet)
  else
    Result:= nil;
end;

function GetPIMTOffSet(const Instance: IInterface): Integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: LongInt of
      AddByte : (AdjustmentByte: ShortInt);
      AddLong : (AdjustmentLong: LongInt);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result:= -1;
  if Assigned(Pointer(Instance)) then
    try
      QueryInterfaceThunk:= TInterfaceRef(Instance)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Result:= -QueryInterfaceThunk.AdjustmentByte;
        AddLong: Result:= -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;

function GetInterfaceIID(const ObjectClass: TClass; var IID: TGUID): boolean;
var
  LInterfaceTable: PInterfaceTable;
  LInterfaceEntry: PInterfaceEntry;
begin
  Result:= False;
  if Assigned(ObjectClass) then
  begin
    LInterfaceTable:= ObjectClass.GetInterfaceTable;
    if Assigned(LInterfaceTable) then
    begin
      if LInterfaceTable.EntryCount > 0 then
      begin
        Result:= True;
        LInterfaceEntry:= @LInterfaceTable.Entries[0];
        IID:= LInterfaceEntry.IID;
      end;
    end;
  end;
end;
                                              
procedure GetInterfaces(ObjectClass: TClass; var IIDList: TGuidArray);
var
  I: Integer;
  LCount: Integer;
  LInterfaceTable: PInterfaceTable;
  LInterfaceEntry: PInterfaceEntry;
begin
  LCount:= 0; 
  while Assigned(ObjectClass) do
  begin
    LInterfaceTable:= ObjectClass.GetInterfaceTable;
    if Assigned(LInterfaceTable) then
    begin
      for I:= 0 to LInterfaceTable.EntryCount -1 do
      begin
        Inc(LCount);
        SetLength(IIDList, LCount);
        LInterfaceEntry:= @LInterfaceTable.Entries[I];
        IIDList[LCount -1]:= LInterfaceEntry.IID;
      end;
    end;
    ObjectClass:= ObjectClass.ClassParent;
  end;
end;

procedure GetInterfaces(const Instance: IInterface; var IIDList: TGuidArray);
begin
  GetInterfaces(InterfaceToObject(Instance).ClassType, IIDList);
end;

function GetInterfaceEntry(const Instance: IInterface): PInterfaceEntry;
var
  OffSet: Integer;
  LObject: TObject;
  InterfaceTable: PInterfaceTable;
  I: Integer;
  CurrentClass: TClass;
begin
  OffSet:= GetPIMTOffSet(Instance);
  LObject:= InterfaceToObject(Instance);
  if (OffSet >= 0) and Assigned(LObject) then
  begin
    CurrentClass:= LObject.ClassType;
    while Assigned(CurrentClass) do
    begin
      InterfaceTable:= CurrentClass.GetInterfaceTable;
      if Assigned(InterfaceTable) then
        for I:= 0 to InterfaceTable.EntryCount -1 do
        begin
          Result:= @InterfaceTable.Entries[I];
          if Result.IOffSet = OffSet then Exit;
        end;
      CurrentClass:= CurrentClass.ClassParent
    end;
  end;
  Result:= nil;
end;

function GetInterfaceIID(const Instance: IInterface; var IID: TGUID): boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  InterfaceEntry:= GetInterfaceEntry(Instance);
  Result:= Assigned(InterfaceEntry);
  if Result then IID:= InterfaceEntry.IID;
end;

end.
