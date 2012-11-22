unit JazzTypeInfo;

interface

uses
  Classes,
  IniFiles,
  SysUtils,
  JazzClasses,
  JazzValueTypeIntf;

type
  ITypeRegister = interface;
  ITypeInfo = interface(IInterface)
    ['{C5F70D61-019B-4FFD-8B11-C9EA9AF53117}']
    function GetIID: TGUID;
    function GetItemClass: TClass;
    function GetItems: ITypeRegister;
    function GetOwner: ITypeRegister;
    function GetRegisterSuperLater: boolean;
    function GetTypeClass: TClass;
    function GetTypeClassParent: TClass;
    function GetTypeName: string;
    procedure SetIID(const Value: TGUID);
    procedure SetItemClass(const Value: TClass);
    procedure SetRegisterSuperLater(const Value: boolean);
    procedure SetTypeClass(const Value: TClass);
    procedure SetTypeName(const Value: string);

    function GetTypeInfo(const Name: string): ITypeInfo;
    function Add(const IID: TGUID; const TypeName: string; const ItemClass: TClass = nil): ITypeInfo;
    property Items: ITypeRegister read GetItems;

    property IID: TGUID read GetIID write SetIID;
    property TypeName: string read GetTypeName write SetTypeName;
    property TypeClass: TClass read GetTypeClass write SetTypeClass;
    property TypeClassParent: TClass read GetTypeClassParent;
    property ItemClass: TClass read GetItemClass write SetItemClass;
    property Owner: ITypeRegister read GetOwner;
    property RegisterSuperLater: boolean read GetRegisterSuperLater write SetRegisterSuperLater;
  end;

  ITypeRegister = interface(IInterface)
    ['{EA3C15DB-3F8F-44D6-9B3C-E18A9FEA9750}']
    procedure AddMembersFromSuper(const TypeInfo: ITypeInfo);
    function Add(const TypeInfo: ITypeInfo): Integer;
    function Count: Integer;
    function GetItems(const Index: Integer): ITypeInfo;

    function FindTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function FindTypeInfo(const Instance: IInterface): ITypeInfo; overload;
    function FindTypeInfo(const Name: string; const ClassName: boolean = False): ITypeInfo; overload;
    function FindTypeInfo(const TypeClass: TClass): ITypeInfo; overload;
    function FindTypeInfo(const ListInstance: IInterface; const MemberName: string): ITypeInfo; overload;

    function GetTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function GetTypeInfo(const Instance: IInterface): ITypeInfo; overload;
    function GetTypeInfo(const Name: string; const ClassName: boolean = False): ITypeInfo; overload;
    function GetTypeInfo(const TypeClass: TClass): ITypeInfo; overload;

    function FindClass(const ListInstance: IInterface; const MemberName: string): TClass; 
    function FindIID(const TypeClass: TClass; var IID: TGUID): boolean;
    function GetIID(const TypeClass: TClass; var IID: TGUID): boolean;

    function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const ItemClass: TClass = nil): ITypeInfo;
    property Items[const Index: Integer]: ITypeInfo read GetItems; default;
  end;

  TTypeInfo = class(TInterfacedObject, ITypeInfo)
  private
    FIID: TGUID;
    FItems: ITypeRegister;
    FTypeName: string;
    FTypeClass: TClass;
    FItemClass: TClass;
    FOwner: Pointer;
    FRegisterSuperLater: boolean;
  protected
    function GetIID: TGUID;
    function GetItemClass: TClass;
    function GetItems: ITypeRegister;
    function GetOwner: ITypeRegister;
    function GetRegisterSuperLater: boolean;
    function GetTypeClass: TClass;
    function GetTypeClassParent: TClass;
    function GetTypeName: string;
    procedure SetIID(const Value: TGUID);
    procedure SetItemClass(const Value: TClass);
    procedure SetRegisterSuperLater(const Value: boolean);
    procedure SetTypeClass(const Value: TClass);
    procedure SetTypeName(const Value: string);

    function GetTypeInfo(const Name: string): ITypeInfo;
    function Add(const IID: TGUID; const TypeName: string; const ItemClass: TClass = nil): ITypeInfo;
    property Items: ITypeRegister read GetItems;

    property TypeClassParent: TClass read GetTypeClassParent;
    property IID: TGUID read GetIID write SetIID;
    property TypeName: string read GetTypeName write SetTypeName;
    property TypeClass: TClass read GetTypeClass write SetTypeClass;
    property ItemClass: TClass read GetItemClass write SetItemClass;
    property Owner: ITypeRegister read GetOwner;
    property RegisterSuperLater: boolean read GetRegisterSuperLater write SetRegisterSuperLater;
  public
    constructor Create(const AOwner: ITypeRegister); virtual;
  end;

  TTypeRegister = class(TInterfacedObject, ITypeRegister)
  private
    FItems: IInterfaceList;
    FHashName: TStringHash;
    FHashClassName: TStringHash;
    FHashIID: TStringHash;
    procedure AddMembersFromSuper(const TypeInfo: ITypeInfo);
    function IndexOf(const Name: string; const ClassName: boolean): Integer;
  protected
    function Add(const TypeInfo: ITypeInfo): Integer;
    function Count: Integer;
    function GetItems(const Index: Integer): ITypeInfo;

    function FindTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function FindTypeInfo(const Instance: IInterface): ITypeInfo; overload;
    function FindTypeInfo(const Name: string; const ClassName: boolean = False): ITypeInfo; overload;
    function FindTypeInfo(const TypeClass: TClass): ITypeInfo; overload;
    function FindTypeInfo(const Instance: IInterface; const MemberName: string): ITypeInfo; overload;

    function GetTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function GetTypeInfo(const Instance: IInterface): ITypeInfo; overload;
    function GetTypeInfo(const Name: string; const ClassName: boolean = False): ITypeInfo; overload;
    function GetTypeInfo(const TypeClass: TClass): ITypeInfo; overload;

    function FindClass(const ListInstance: IInterface; const MemberName: string): TClass; 
    function FindIID(const TypeClass: TClass; var IID: TGUID): boolean;
    function GetIID(const TypeClass: TClass; var IID: TGUID): boolean;

    function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const ItemClass: TClass = nil): ITypeInfo;
    property Items[const Index: Integer]: ITypeInfo read GetItems; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  ITypeInfoLoader = interface(IInterface)
    ['{6F823925-C7B7-4055-BAE7-B788F104F4C0}']
    procedure Execute;
  end;

  TTypeInfoLoaderClass = class of TTypeInfoLoader;
  TTypeInfoLoader = class(TInterfacedObject, ITypeInfoLoader)
  protected
    procedure Execute; virtual; abstract;
  end;

  ITypeInfoRegisterClass = interface(IRegisterClass)
    ['{92C4F741-EDAA-426E-B5D4-16C76D17A96B}']
    procedure LoadTypeInfoRegister(const AClass: TTypeInfoLoaderClass);
  end;
  
  TTypeInfoRegisterClass = class(TJazzRegisterClass, ITypeInfoRegisterClass)
  protected
    procedure RegisterClass(const AClass: TClass); override;
    procedure LoadTypeInfoRegister(const AClass: TTypeInfoLoaderClass);
  end;

  ETypeRegisterException = class(EJazz)
  protected
    function GetDefaultMessage: string; override;
  end;

function RegisterTypeLoader(AClass: TClass): ITypeInfoRegisterClass;
function TypeRegister: ITypeRegister;
function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const ItemClass: TClass = nil): ITypeInfo;

function AutoRegisterClass(const AClass: TClass): ITypeInfo;
procedure AutoRegisterService(const Instance: IInterface);

implementation

uses
  JazzConsts,
  JazzIntfUtils,
  JazzValueType;

var
  FTypeRegister: ITypeRegister = nil;
  FTypeClassRegister: ITypeInfoRegisterClass = nil;

function RegisterTypeLoader(AClass: TClass): ITypeInfoRegisterClass;
begin
  if FTypeClassRegister = nil then
    FTypeClassRegister:= TTypeInfoRegisterClass.Create;
    
  Result:= FTypeClassRegister;
  Result.RegisterClass(AClass); 
end;

function TypeRegister: ITypeRegister;
begin
  if FTypeRegister = nil then FTypeRegister:= TTypeRegister.Create;
  Result:= FTypeRegister;
end;

function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass:
  TClass; const ItemClass: TClass): ITypeInfo;
begin
  Result:= TypeRegister.RegisterType(IID, TypeName, TypeClass, ItemClass);
end;

procedure __UpdateIID(const Instace: IInterface; const ATypeInfo: ITypeInfo);
var
  LIID: TGUID;
begin
  if GetInterfaceIID(Instace, LIID) then ATypeInfo.IID:= LIID;
end;

function __AutoRegisterObject(const Instance: IInterface): ITypeInfo;
var
  LClass: TClass;
begin
 LClass:= InterfaceToObject(Instance).ClassType;
 Result:= AutoRegisterClass(LClass);
  __UpdateIID(Instance, Result);
end;

function __AutoRegisterList(const Instance: IInterface): ITypeInfo;
var
  LObject: IObjectType;
  LClass: TClass;
begin
  __AutoRegisterObject(Instance);
  LClass:= (Instance as IObjectListType).ItemClass; 
  AutoRegisterClass(LClass);

  if not AutoRegisterObject then
  begin
    LObject:= TObjectTypeClass((Instance as IObjectListType).ItemClass).Create;
    AutoRegisterService(LObject); 
  end;
end;

function AutoRegisterClass(const AClass: TClass): ITypeInfo;
var
  LName: string;
  LIID: TGUID;
  LTypeInfo: ITypeInfo;
  LClass: TClass;
begin
  Assert(AClass <> nil, SClassCantBeNil);
  
  LClass:= AClass;
  while LClass <> nil do
  begin
    LTypeInfo:= TypeRegister.FindTypeInfo(LClass);

    if LTypeInfo = nil then
    begin
      LName:= Copy(LClass.ClassName, 2, Length(LClass.ClassName) -1);
      LIID:= StringToGUID(GUID_NULL);
      GetInterfaceIID(LClass, LIID);

      if Result = nil then
        Result:= RegisterType(LIID, LName, LClass)
      else
        RegisterType(LIID, LName, LClass);

      if IsValueTypeClass(LClass.ClassParent) then
        LClass:= nil
      else
        LClass:= LClass.ClassParent
    end
    else
    begin
      if Result = nil then Result:= LTypeInfo;
      LClass:= nil;
    end;
  end;
end;

procedure AutoRegisterService(const Instance: IInterface);
var
  LObject: IObjectType;
  LObjectList: IObjectListType;
begin
  if Supports(Instance, IObjectListType, LObjectList) then
    __AutoRegisterList(LObjectList)
  else
  if Supports(Instance, IObjectType, LObject) then
    __AutoRegisterObject(LObject);
end;

{ TTypeInfo }

function TTypeInfo.GetTypeClass: TClass;
begin
  Result:= FTypeClass;
end;

function TTypeInfo.GetIID: TGUID;
begin
  Result:= FIID;
end;

function TTypeInfo.GetTypeName: string;
begin
  Result:= FTypeName;
end;

procedure TTypeInfo.SetTypeClass(const Value: TClass);
begin
  FTypeClass:= Value;
end;

procedure TTypeInfo.SetIID(const Value: TGUID);
begin
  FIID:= Value;
end;

procedure TTypeInfo.SetTypeName(const Value: string);
begin
  FTypeName:= Value;
end;

function TTypeInfo.GetOwner: ITypeRegister;
begin
  Result:= ITypeRegister(FOwner);
end;

constructor TTypeInfo.Create(const AOwner: ITypeRegister);
begin
  inherited Create;
  FRegisterSuperLater:= False;
  FOwner:= Pointer(AOwner);
end;

function TTypeInfo.GetItemClass: TClass;
begin
  Result:= FItemClass; 
end;

procedure TTypeInfo.SetItemClass(const Value: TClass);
begin
  FItemClass:= Value;
end;

function TTypeInfo.Add(const IID: TGUID; const TypeName: string;
  const ItemClass: TClass): ITypeInfo;
var
  LTypeInfo: ITypeInfo;
  LTypeClass: TClass;
begin
  LTypeInfo:= TypeRegister.GetTypeInfo(IID);
  LTypeClass:= LTypeInfo.TypeClass;

  Assert(TypeClass <> nil, SClassCantBeNil);
  Result:= Items.RegisterType(IID, TypeName, LTypeClass, ItemClass);
end;

function TTypeInfo.GetItems: ITypeRegister;
begin
  if FItems = nil then FItems:= TTypeRegister.Create;
  Result:= FItems;
end;

function TTypeInfo.GetTypeInfo(const Name: string): ITypeInfo;
begin
  Result:= Items.GetTypeInfo(Name);
end;

function TTypeInfo.GetTypeClassParent: TClass;
begin
  if TypeClass = nil then
    Result:= nil
  else
  begin
    if RegisterSuperLater then
    begin
      FRegisterSuperLater:= False;
//      TypeRegister.AddMembersFromSuper(Self);
    end;

    Result:= TypeClass.ClassParent;
    if IsValueTypeClass(Result) then Result:= nil;
  end;
end;

function TTypeInfo.GetRegisterSuperLater: boolean;
begin
  Result:= FRegisterSuperLater;
end;

procedure TTypeInfo.SetRegisterSuperLater(const Value: boolean);
begin
  FRegisterSuperLater:= Value;
end;

{ TTypeRegister }

procedure TTypeRegister.AddMembersFromSuper(const TypeInfo: ITypeInfo);
var
  I: Integer;
  LSuperList: TStrings;
  LSuperClass: TClass;
  LTypeInfo: ITypeInfo;
begin
  LSuperList:= TStringList.Create;
  try
    LSuperClass:= TypeInfo.TypeClassParent;
    while LSuperClass <> nil do
    begin
      LTypeInfo:= TypeRegister.FindTypeInfo(LSuperClass);

      if LTypeInfo = nil then
      begin
        TypeInfo.RegisterSuperLater:= True;
        Exit;
      end;
      LSuperClass:= nil;
      
      if LTypeInfo <> nil then
      begin
        LSuperList.AddObject(LTypeInfo.TypeName, Pointer(LTypeInfo));
        LSuperClass:= LTypeInfo.TypeClassParent;
      end;
    end;

    while LSuperList.Count > 0 do
    begin
      Pointer(LTypeInfo):= LSuperList.Objects[LSuperList.Count -1];
      for I:= 0 to LTypeInfo.Items.Count -1 do
        TypeInfo.Items.Add(LTypeInfo.Items[I]);
      LSuperList.Delete(LSuperList.Count -1);
    end;
  finally
    LSuperList.Free;
  end;
end;

constructor TTypeRegister.Create;
begin
  inherited Create;
  FItems:= TInterfaceList.Create;
  FHashClassName:= TStringHash.Create;
  FHashName:= TStringHash.Create;
  FHashIID:= TStringHash.Create;
end;

function TTypeRegister.GetTypeInfo(const IID: TGUID): ITypeInfo;
begin
  Result:= FindTypeInfo(IID);
  if not Assigned(Result) then
    raise ETypeRegisterException.CreateFmt([GuidToString(IID)]);
end;

function TTypeRegister.GetTypeInfo(const Name: string; const ClassName: boolean): ITypeInfo;
begin
  Result:= FindTypeInfo(Name, ClassName);

  if not Assigned(Result) then
    raise ETypeRegisterException.CreateFmt([Name]);
end;

function TTypeRegister.GetIID(const TypeClass: TClass; var IID: TGUID): boolean;
begin
  Result:= FindIID(TypeClass, IID);
  if not Result then
    raise ETypeRegisterException.CreateFmt([TypeClass.ClassName]);
end;

function TTypeRegister.GetItems(const Index: Integer): ITypeInfo;
begin
  Result:= FItems[Index] as ITypeInfo;
end;

function TTypeRegister.GetTypeInfo(const TypeClass: TClass): ITypeInfo;
begin
  Result:= FindTypeInfo(TypeClass);

  if not Assigned(Result) then
    raise ETypeRegisterException.CreateFmt([TypeClass.ClassName]);
end;

function TTypeRegister.GetTypeInfo(const Instance: IInterface): ITypeInfo;
begin
  Result:= FindTypeInfo(Instance);

  if not Assigned(Result) then
    raise ETypeRegisterException.CreateFmt([GUIDToString(IInterface)]);
end;

function TTypeRegister.RegisterType(const IID: TGUID; const TypeName: string;
  const TypeClass: TClass; const ItemClass: TClass): ITypeInfo;
begin
  Result:= TTypeInfo.Create(Self);

  Result.IID:= IID;
  Result.TypeName:= TypeName;
  Result.TypeClass:= TypeClass;
  Result.ItemClass:= ItemClass;

  Add(Result);
//  AddMembersFromSuper(Result);
end;

function TTypeRegister.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TTypeRegister.Add(const TypeInfo: ITypeInfo): Integer;
begin
  Result:= FItems.Add(TypeInfo);
  FHashName.Add(AnsiUpperCase(TypeInfo.TypeName), Result);
  FHashClassName.Add(AnsiUpperCase(TypeInfo.TypeClass.ClassName), Result);
  FHashIID.Add(GUIDToString(TypeInfo.IID), Result);
end;

function TTypeRegister.FindTypeInfo(const IID: TGUID): ITypeInfo;
var
  LIndex: Integer;
begin
  Result:= nil;
  LIndex:= FHashIID.ValueOf(GUIDToString(IID));
  if LIndex <> NotFound then Result:= (FItems[LIndex] as ITypeInfo);
end;

function TTypeRegister.FindTypeInfo(const TypeClass: TClass): ITypeInfo;
var
  LIndex: Integer;
begin
  Result:= nil;
  LIndex:= IndexOf(TypeClass.ClassName, True);
  if LIndex <> NotFound then Result:= (FItems[LIndex] as ITypeInfo);
end;

function TTypeRegister.IndexOf(const Name: string; const ClassName: boolean): Integer;
begin
  if ClassName then
    Result:= FHashClassName.ValueOf(AnsiUpperCase(Name))
  else
    Result:= FHashName.ValueOf(AnsiUpperCase(Name))
end;

function TTypeRegister.FindTypeInfo(const Instance: IInterface): ITypeInfo;
var
  LObject: TObject;
begin
  LObject:= InterfaceToObject(Instance);
  if LObject <> nil then Result:= FindTypeInfo(LObject.ClassType);
end;

function TTypeRegister.FindTypeInfo(const Name: string;
  const ClassName: boolean): ITypeInfo;
var
  LIndex: Integer;
begin
  Result:= nil;
  LIndex:= IndexOf(Name, ClassName);
  if (LIndex <> NotFound) then Result:= (FItems[Lindex] as ITypeInfo);
end;

destructor TTypeRegister.Destroy;
begin
  FreeAndNil(FHashClassName);
  FreeAndNil(FHashName);
  FreeAndNil(FHashIID);
  inherited;
end;

function TTypeRegister.FindTypeInfo(const Instance: IInterface;
  const MemberName: string): ITypeInfo;
var
  LTypeInfo: ITypeInfo;
begin
  Result:= nil;
  LTypeInfo:= TypeRegister.FindTypeInfo(Instance);
  if (LTypeInfo <> nil) then
  begin
    if (LTypeInfo.ItemClass = nil) then
      Result:= LTypeInfo.GetTypeInfo(MemberName)
    else
    begin
      LTypeInfo:= TypeRegister.FindTypeInfo(LTypeInfo.ItemClass);

      if LTypeInfo <> nil then
        Result:= LTypeInfo.GetTypeInfo(MemberName);
    end;
  end;
end;

function TTypeRegister.FindClass(const ListInstance: IInterface;
  const MemberName: string): TClass;
var
  LTypeInfo: ITypeInfo;
begin
  LTypeInfo:= FindTypeInfo(ListInstance, MemberName);
  if LTypeInfo <> nil then
    Result:= LTypeInfo.TypeClass
  else
    Result:= nil;
end;

function TTypeRegister.FindIID(const TypeClass: TClass; var IID: TGUID): boolean;
var
  LTypeInfo: ITypeInfo;
begin
  LTypeInfo:= FindTypeInfo(TypeClass);
  Result:= LTypeInfo <> nil;
  if Result then IID:= LTypeInfo.IID;  
end;

{ TTypeInfoRegisterClass }

procedure TTypeInfoRegisterClass.LoadTypeInfoRegister(const AClass: TTypeInfoLoaderClass);
var
  LTypeInfo: ITypeInfoLoader;
begin
 LTypeInfo:= AClass.Create;
 LTypeInfo.Execute;
end;

procedure TTypeInfoRegisterClass.RegisterClass(const AClass: TClass);
begin
  LoadTypeInfoRegister(TTypeInfoLoaderClass(AClass));
end;

{ ETypeRegisterException }

function ETypeRegisterException.GetDefaultMessage: string;
begin
  Result:= STypeInfoNotFound;
end;

end.

