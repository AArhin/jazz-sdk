unit JazzClasses;

interface

uses
  Classes,
  IniFiles,
  SyncObjs,
  SysUtils;

type
  EJazz = class(Exception)
  protected
    function GetDefaultMessage: string; virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
    constructor CreateFmt(const Args: array of const); overload;
    constructor CreateFmt(const Msg: string; const Args: array of const); overload;
  end;

  TCommandNotifyEvent = procedure(const Param: IInterface = nil) of object;

  IStreamable = interface(IInterface)
    ['{FDC30388-68E8-4262-A408-3ED964D5035E}']
    procedure LoadFromStream(const Reader: TReader);
    procedure SaveToStream(const Writer: TWriter);
  end;

  ICustomObject = interface(IInterface)
    ['{5258B873-F0A0-4D81-8DD9-2AA7FA0C8A7F}']
    function Implementor: TObject;
    function GetClassName: string;
    function GetOwner: IInterface;
    procedure SetOwner(const Value: IInterface);
    property Owner: IInterface read GetOwner write SetOwner;
  end;

  TCustomObject = class(TInterfacedObject, ICustomObject)
  private
    FOwner: Pointer;
  protected
    function GetOwner: IInterface;
    procedure SetOwner(const Value: IInterface);

    function Implementor: TObject;
    function GetClassName: string;
    property Owner: IInterface read GetOwner write SetOwner;
  public
    constructor Create(const AOwner: IInterface = nil); virtual;
    destructor Destroy; override;
  end;

  PInterfaceItem = ^TInterfaceItem;
  TInterfaceItem = record
    Item: IInterface;
    FirstName: string;
    SecondName: string;
  end;

  INamedInterfaceList = interface(IInterface)
    ['{DC34DD05-2AF4-48B6-8584-2E8834720C94}']
    function GetCaseSensitive: Boolean;
    function GetCount: Integer;
    function GetItem(const Name: string): IInterface;
    function GetItems(const Index: Integer): IInterface;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetItem(const Name: string; const Value: IInterface);
    procedure SetItems(const Index: Integer; const Value: IInterface);

    function Add(const Name: string; const Item: IInterface): Integer; overload;
    function Add(const FirstName, SecondName: string; const Item: IInterface): Integer; overload;
    procedure Insert(const Index: Integer; const Name: string; const Item: IInterface); overload;
    procedure Insert(const Index: Integer; const FirstName, SecondName: string; const Item: IInterface); overload;

    function IndexOf(const Item: IInterface): Integer; overload;
    function IndexOf(const Name: string; const SecondName: boolean = False): Integer; overload;
    procedure Exchange(const Index1, Index2: Integer);

    procedure Clear;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string; const SecondName: boolean = False); overload;
    procedure Remove(const Item: IInterface);

    procedure Lock;
    procedure Unlock;

    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Count: Integer read GetCount;
    property Item[const Name: string]: IInterface read GetItem write SetItem;
    property Items[const Index: Integer]: IInterface read GetItems write SetItems; default;
  end;

  TNamedInterfaceList = class(TInterfacedObject, INamedInterfaceList)
  private
    FCapacity: Integer;
    FDoubleNamed: boolean;
    FCaseSensitive: Boolean;
    FHashList1: TStringHash;
    FHashList2: TStringHash;
    FItems: TList;
    FLock: TCriticalSection;
    procedure ReHash;
  protected
    function GetCaseSensitive: Boolean;
    function GetCount: Integer;
    function GetItem(const Name: string): IInterface;
    function GetItems(const Index: Integer): IInterface;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetItem(const Name: string; const Value: IInterface);
    procedure SetItems(const Index: Integer; const Value: IInterface);

    function Add(const Name: string; const Item: IInterface): Integer; overload;
    function Add(const FirstName, SecondName: string; const Item: IInterface): Integer; overload;
    procedure Insert(const Index: Integer; const Name: string; const Item: IInterface); overload;
    procedure Insert(const Index: Integer; const FirstName, SecondName: string; const Item: IInterface); overload;

    function IndexOf(const Item: IInterface): Integer; overload;
    function IndexOf(const Name: string; const SecondName: boolean = False): Integer; overload;
    procedure Exchange(const Index1, Index2: Integer);

    procedure Clear;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string; const SecondName: boolean = False); overload;
    procedure Remove(const Item: IInterface);
  public
    constructor Create(const DoubleNamed: boolean = False; const HashSize: Integer = 389);
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Count: Integer read GetCount;
    property Item[const Name: string]: IInterface read GetItem write SetItem;
    property Items[const Index: Integer]: IInterface read GetItems write SetItems; default;
  end;

  IRegister = interface(IInterface)
    ['{D29F7011-6CAF-49CA-8836-D573208835ED}']
    function Add(const Name: string; const AObject: TObject): Integer;
    function Count: Integer;
    function Delete(const Index: Integer): boolean;
    function GetItem(const Name: string): TObject;
    function GetItems(const Index: Integer): TObject;
    function IndexOf(const Name: string): Integer;
    function IndexOfObject(const AObject: TObject): Integer;
    property Item[const Name: string]: TObject read GetItem;
    property Items[const Index: Integer]: TObject read GetItems; default;
  end;

  IRegisterClass = interface(IInterface)
    ['{53DF764E-61E6-424B-9199-7076B8A614E0}']
    function GetItems(const Index: Integer): TClass;
    function Count: Integer;
    function FindClass(const ClassName: string): TClass;
    procedure RegisterClass(const AClass: TClass);
    procedure UnregisterClass(const AClass: TClass);
    property Items[const Index: Integer]: TClass read GetItems; default;
  end;

  TRegister = class(TInterfacedObject, IRegister)
  private
    FCapacity: Integer;
    FList: TList;
    FHash: TStringHash;
    FLock: TCriticalSection;
  protected
    procedure Lock;
    procedure Unlock;
    function GetItem(const Name: string): TObject;
    function GetItems(const Index: Integer): TObject;

    function Add(const Name: string; const AObject: TObject): Integer;
    function Count: Integer; virtual;
    function IndexOf(const Name: string): Integer;
    function IndexOfObject(const AObject: TObject): Integer;
    function Delete(const Index: Integer): boolean;
    procedure Clear;
    property Item[const Name: string]: TObject read GetItem;
    property Items[const Index: Integer]: TObject read GetItems; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TJazzRegisterClass = class(TRegister, IRegisterClass)
  protected
    function Count: Integer; override;
    function GetItems(const Index: Integer): TClass; reintroduce;
    function FindClass(const ClassName: string): TClass; virtual;
    procedure RegisterClass(const AClass: TClass); virtual;
    procedure UnregisterClass(const AClass: TClass); virtual;
    property Items[const Index: Integer]: TClass read GetItems; default;
  end;

function ApplicationFindClass(const ClassName: string): TClass;
procedure ApplicationRegisterClass(const AClass: TClass);
procedure ApplicationRegisterClasses(const AClasses: array of TClass);
procedure ApplicationUnregisterClass(const AClass: TClass);

implementation

uses
  JazzConsts;

var
  _ClassRegister: IRegisterClass;

function ApplicationFindClass(const ClassName: string): TClass;
begin
  Result := nil;
  if _ClassRegister = nil then Exit;
  Result := _ClassRegister.FindClass(ClassName);
end;

procedure ApplicationRegisterClass(const AClass: TClass);
begin
  if _ClassRegister = nil then _ClassRegister := TJazzRegisterClass.Create;
  _ClassRegister.RegisterClass(AClass);
end;

procedure ApplicationRegisterClasses(const AClasses: array of TClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    ApplicationRegisterClass(AClasses[I]);
end;

procedure ApplicationUnregisterClass(const AClass: TClass);
begin
  if _ClassRegister = nil then Exit;
  _ClassRegister.UnregisterClass(AClass);
  if (_ClassRegister as IRegister).Count = 0 then _ClassRegister := nil;
end;

{ TRegister }

function TRegister.Add(const Name: string; const AObject: TObject): Integer;
var
  LItem: PStringItem;
begin
  Lock;
  try
    Result := IndexOf(Name);
    if Result <> NotFound then Exit;
    New(LItem);
    LItem.FString := Uppercase(Name);
    LItem.FObject := AObject;
    Result := FList.Add(LItem);
    FHash.Add(LItem.FString, Result);
  finally
    Unlock;
  end;
end;

procedure TRegister.Clear;
var
  I: Integer;
  LItem: PStringItem;
begin
  for I := 0 to FList.Count - 1 do
  begin
    LItem := PStringItem(FList[I]);
    Finalize(LItem^);
    Dispose(FList[I]);
  end;
  FList.Clear;
  FHash.Clear;
end;

function TRegister.Count: Integer;
begin
  Lock;
  try
    Result := FList.Count;
  finally
    Unlock;
  end;
end;

constructor TRegister.Create;
begin
  inherited;
  FCapacity := 256;
  FLock := TCriticalSection.Create;
  FList := TList.Create;
  FHash := TStringHash.Create(FCapacity);
end;

function TRegister.Delete(const Index: Integer): boolean;
var
  LItem: PStringItem;
begin
  Result := False;
  Lock;
  try
    if Index < Count then
    begin
      LItem := PStringItem(FList[Index]);
      FHash.Remove(LItem.FString);
      Finalize(LItem^);
      Dispose(FList[Index]);
      FList.Delete(Index);
      Result := True;
    end;
  finally
    Unlock;
  end;
end;

destructor TRegister.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  FreeAndNil(FHash);
  FreeAndNil(FLock);
  inherited;
end;

function TRegister.GetItem(const Name: string): TObject;
var
  LIndex: Integer;
begin
  LIndex := FHash.ValueOf(Uppercase(Name));
  if LIndex <> NotFound then Result := GetItems(LIndex) else Result := nil;
end;

function TRegister.GetItems(const Index: Integer): TObject;
var
  LItem: PStringItem;
begin
  Lock;
  try
    if (Index <> NotFound) and (Index < Count) then
    begin
      LItem := PStringItem(FList[Index]);
      Result := LItem.FObject;
    end
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

function TRegister.IndexOf(const Name: string): Integer;
begin
  Result := FHash.ValueOf(Uppercase(Name));
end;

function TRegister.IndexOfObject(const AObject: TObject): Integer;
var
  I: Integer;
  LItem: PStringItem;
begin
  Lock;
  try
    Result := NotFound;
    for I := 0 to FList.Count - 1 do
    begin
      LItem := PStringItem(FList[I]);
      if LItem.FObject = AObject then
      begin
        Result := I;
        Break;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TRegister.Lock;
begin
  FLock.Enter;
end;

procedure TRegister.Unlock;
begin
  FLock.Leave;
end;

{ TJazzRegisterClass }

function TJazzRegisterClass.Count: Integer;
begin
  Result := inherited Count;
end;

function TJazzRegisterClass.FindClass(const ClassName: string): TClass;
begin
  Result := TClass(Item[ClassName]);
end;

function TJazzRegisterClass.GetItems(const Index: Integer): TClass;
begin
  Result := TClass(inherited GetItems(Index));
end;

procedure TJazzRegisterClass.RegisterClass(const AClass: TClass);
begin
  Add(AClass.ClassName, Pointer(AClass));
end;

procedure TJazzRegisterClass.UnregisterClass(const AClass: TClass);
begin
  if FindClass(AClass.ClassName) <> nil then
    Delete(IndexOfObject(Pointer(AClass)));
end;

constructor TNamedInterfaceList.Create(const DoubleNamed: boolean; const HashSize: Integer);
begin
  inherited Create;
  FCapacity := HashSize;
  FLock := TCriticalSection.Create;
  FItems := TList.Create;

  FHashList1 := TStringHash.Create(FCapacity);
  FDoubleNamed := DoubleNamed;
  if FDoubleNamed then FHashList2 := TStringHash.Create;
end;

destructor TNamedInterfaceList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FHashList1);
  if FDoubleNamed then FreeAndNil(FHashList2);
  FreeAndNil(FLock);
  inherited;
end;

function TNamedInterfaceList.Add(const Name: string; const Item: IInterface): Integer;
var
  LItem: PInterfaceItem;
begin
  Assert(not FDoubleNamed, SAddDoubleNamed);
  Result := IndexOf(Name);
  if Result = NotFound then
  begin
    New(LItem);
    LItem.Item:= Item as IInterface;
    LItem.FirstName := Name;
    Result := FItems.Add(LItem);
    if CaseSensitive then
      FHashList1.Add(Name, Result)
    else
      FHashList1.Add(AnsiUpperCase(Name), Result);
  end;
end;

procedure TNamedInterfaceList.Clear;
var
  I: Integer;
  LItem: PInterfaceItem;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    LItem := PInterfaceItem(FItems[I]);
    Finalize(LItem^);
    Dispose(FItems[I]);
  end;
  FItems.Clear;
  FHashList1.Clear;
  if FDoubleNamed then
    FHashList2.Clear;
end;

procedure TNamedInterfaceList.Delete(const Index: Integer);
var
  LName: string;
  LItem: PInterfaceItem;
begin
  LItem := PInterfaceItem(FItems[Index]);
  LName := LItem.FirstName;

  if CaseSensitive then
  begin
    FHashList1.Remove(LName);
    if FDoubleNamed then FHashList2.Remove(LName);
  end
  else
  begin
    FHashList1.Remove(AnsiUpperCase(LName));
    if FDoubleNamed then FHashList2.Remove(AnsiUpperCase(LName));
  end;

  Finalize(LItem^);
  Dispose(LItem);
  FItems.Delete(Index);
end;

procedure TNamedInterfaceList.Delete(const Name: string; const SecondName: boolean);
var
  LIndex: Integer;
  LItem: PInterfaceItem;
begin
  LIndex := IndexOf(Name, SecondName);
  if LIndex = NotFound then
    Exit;
  if CaseSensitive then
  begin
    FHashList1.Remove(Name);
    if FDoubleNamed then FHashList2.Remove(Name);
  end
  else
  begin
    FHashList1.Remove(AnsiUpperCase(Name));
    if FDoubleNamed then FHashList2.Remove(AnsiUpperCase(Name));
  end;
  LItem := FItems[LIndex];
  Finalize(LItem^);
  Dispose(LItem);
  FItems.Delete(LIndex);
end;

function TNamedInterfaceList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TNamedInterfaceList.GetItem(const Name: string): IInterface;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(Name);
  if LIndex = NotFound then
    Result := nil
  else
    Result := PInterfaceItem(FItems[LIndex]).Item;
end;

function TNamedInterfaceList.GetItems(const Index: Integer): IInterface;
begin
  Result := nil;
  Result := PInterfaceItem(FItems[Index]).Item;
end;

function TNamedInterfaceList.IndexOf(const Name: string; const SecondName: boolean): Integer;
var
  LHash: TStringHash;
begin
  if SecondName and FDoubleNamed then
    LHash := FHashList2
  else
    LHash := FHashList1;

  if CaseSensitive then
    Result := LHash.ValueOf(Name)
  else
    Result := LHash.ValueOf(AnsiUpperCase(Name));
end;

procedure TNamedInterfaceList.ReHash;
var
  I: Integer;
begin
  FHashList1.Clear;
  if FDoubleNamed then FHashList2.Clear;
  if CaseSensitive then
    for I := 0 to FItems.Count - 1 do
    begin
      FHashList1.Add(PInterfaceItem(FItems[I]).FirstName, I);
      if FDoubleNamed then
        FHashList2.Add(PInterfaceItem(FItems[I]).SecondName, I);
    end
  else
    for I := 0 to FItems.Count - 1 do
    begin
      FHashList1.Add(AnsiUpperCase(PInterfaceItem(FItems[I]).FirstName), I);
      if FDoubleNamed then
        FHashList2.Add(AnsiUpperCase(PInterfaceItem(FItems[I]).SecondName), I);
    end;
end;

procedure TNamedInterfaceList.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    ReHash;
  end;
end;

procedure TNamedInterfaceList.SetItem(const Name: string; const Value:
  IInterface);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(Name);
  if LIndex <> NotFound then PInterfaceItem(FItems[LIndex]).Item := Value;
end;

procedure TNamedInterfaceList.SetItems(const Index: Integer; const Value: IInterface);
begin
  PInterfaceItem(FItems[Index]).Item := Value;
end;

function TNamedInterfaceList.IndexOf(const Item: IInterface): Integer;
var
  I: Integer;
begin
  Result := NotFound;

  for I := 0 to FItems.Count - 1 do
    if PInterfaceItem(FItems[I]).Item = (Item as IInterface) then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TNamedInterfaceList.Exchange(const Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
  ReHash;
end;

procedure TNamedInterfaceList.Lock;
begin
  FLock.Enter;
end;

procedure TNamedInterfaceList.Unlock;
begin
  FLock.Leave;
end;

function TNamedInterfaceList.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TNamedInterfaceList.Add(const FirstName, SecondName: string;
  const Item: IInterface): Integer;
var
  LItem: PInterfaceItem;
begin
  Assert(FDoubleNamed, SAddNotDoubleNamed);

  Result := IndexOf(FirstName);
  if Result = NotFound then
  begin
    New(LItem);
    LItem.Item := Item as IInterface;
    LItem.FirstName := FirstName;
    LItem.SecondName := SecondName;
    Result := FItems.Add(LItem);
    if CaseSensitive then
    begin
      FHashList1.Add(FirstName, Result);
      FHashList2.Add(SecondName, Result);
    end
    else
    begin
      FHashList1.Add(AnsiUpperCase(FirstName), Result);
      FHashList2.Add(AnsiUpperCase(SecondName), Result);
    end;
  end;
end;

{ TCustomObject }

function TCustomObject.GetClassName: string;
begin
  Result := Implementor.ClassName;
end;

constructor TCustomObject.Create(const AOwner: IInterface);
begin
  inherited Create;
  if FOwner = nil then
    FOwner := Pointer(AOwner);
end;

destructor TCustomObject.Destroy;
begin
  FOwner := nil;
  inherited;
end;

function TCustomObject.GetOwner: IInterface;
begin
  Result := IInterface(FOwner);
end;

function TCustomObject.Implementor: TObject;
begin
  Result := Self;
end;

procedure TCustomObject.SetOwner(const Value: IInterface);
begin
  FOwner := Pointer(Value);
end;

procedure TNamedInterfaceList.Remove(const Item: IInterface);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(Item);
  if LIndex <> NotFound then Delete(LIndex);
end;

procedure TNamedInterfaceList.Insert(const Index: Integer;
  const Name: string; const Item: IInterface);
var
  LItem: PInterfaceItem;
begin
  Assert(not FDoubleNamed, SAddDoubleNamed);
  if IndexOf(Name) = NotFound then
  begin
    New(LItem);
    LItem.Item := Item as IInterface;
    LItem.FirstName := Name;
    FItems.Insert(Index, LItem);
    if CaseSensitive then
      FHashList1.Add(Name, Index)
    else
      FHashList1.Add(AnsiUpperCase(Name), Index);
    ReHash;
  end;
end;

procedure TNamedInterfaceList.Insert(const Index: Integer; const FirstName,
  SecondName: string; const Item: IInterface);
var
  LItem: PInterfaceItem;
begin
  Assert(FDoubleNamed, SAddNotDoubleNamed);

  if IndexOf(FirstName) = NotFound then
  begin
    New(LItem);
    LItem.Item := Item as IInterface;
    LItem.FirstName := FirstName;
    LItem.SecondName := SecondName;
    FItems.Insert(Index, LItem);
    if CaseSensitive then
    begin
      FHashList1.Add(FirstName, Index);
      FHashList2.Add(SecondName, Index);
    end
    else
    begin
      FHashList1.Add(AnsiUpperCase(FirstName), Index);
      FHashList2.Add(AnsiUpperCase(SecondName), Index);
    end;
    ReHash;
  end;
end;

{ EJazz }

constructor EJazz.Create;
begin
  Self.Message:= GetDefaultMessage; 
end;

constructor EJazz.Create(const Msg: string);
begin
  Self.Message:= Msg;
end;

constructor EJazz.CreateFmt(const Args: array of const);
begin
  CreateFmt(GetDefaultMessage, Args);
end;

constructor EJazz.CreateFmt(const Msg: string; const Args: array of const);
begin
  if Pos('%', Msg) > -1 then
    Self.Message:= Format(Msg, Args)
  else
    Self.Message:= Msg; 
end;

end.

