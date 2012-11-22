unit JazzDataFactory;

interface

uses
  Classes,
  SyncObjs,
  JazzClasses,
  JazzMappingIntf,
  JazzValueTypeIntf;

type
  TDataGeneratorClass = class of TDataGenerator;

  TGeneratorContext = (
    gcClass,
    gcClassMember,
    gcApplication
  );

  TGeneratorOrder = (
    goNone,
    goAscending,
    goDescending
  );

  IDataGenerator = interface(IInterface)
    ['{4B02C83E-3493-4AA4-A37F-8654B10C8099}']
    function GetClassContext: string;
    function GetOrder: TGeneratorOrder;
    procedure SetClassContext(const Value: string);
    procedure SetOrder(const Value: TGeneratorOrder);

    function Next(const Member: IMemberType): boolean;
    property ClassContext: string read GetClassContext write SetClassContext;
    property Order: TGeneratorOrder read GetOrder write SetOrder;
  end;

  TDataGenerator = class(TInterfacedObject, IDataGenerator)
  private
    FClassContext: string;
    FOrder: TGeneratorOrder;
    FLock: TCriticalSection;
  protected
    procedure Lock;
    procedure Unlock;
    function GetClassContext: string;
    function GetOrder: TGeneratorOrder;
    procedure SetClassContext(const Value: string);
    procedure SetOrder(const Value: TGeneratorOrder);

    function Next(const Member: IMemberType): boolean; virtual; abstract;
    property ClassContext: string read GetClassContext write SetClassContext;
    property Order: TGeneratorOrder read GetOrder write SetOrder;
  public
    class function GetContext: TGeneratorContext; virtual; 
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TGuidGenerator = class(TDataGenerator)
  protected
    function Next(const Member: IMemberType): boolean; override;
  public
    class function GetContext: TGeneratorContext; override;
  end;

  IIntegerGenerator = interface(IDataGenerator)
    ['{C347E1E0-0270-48F6-8FFE-5CDA1B99B1EC}']
    function GetLast: Int64;
    procedure SetLast(const Value: Int64);
    procedure GenerateNext;
    property Last: Int64 read GetLast write SetLast;
  end;
  
  TIntegerGenerator = class(TDataGenerator, IIntegerGenerator)
  private
    FLast: Int64;
  protected
    function GetLast: Int64;
    procedure SetLast(const Value: Int64);
    procedure GenerateNext; virtual; abstract;

    function Next(const Member: IMemberType): boolean; override;
    property Last: Int64 read GetLast write SetLast;
  public
    constructor Create; override;
    class function GetContext: TGeneratorContext; override;
  end;

  TAscendingGenerator = class(TIntegerGenerator)
  protected
    procedure GenerateNext; override;
  public
    constructor Create; override;
  end;

  TDescendingGenerator = class(TIntegerGenerator)
  protected
    procedure GenerateNext; override;
  public
    constructor Create; override;
  end;

  IDataFactory = interface(ICustomObject)
    ['{5FD74EA9-CB6E-4651-B3CE-C0616EB96BF7}']
    function GetMapping: IMapping;
    property Mapping: IMapping read GetMapping;

    procedure Next(const AObject: IObjectType); overload;
    procedure Next(const AMember: IMemberType; const ObjectMeta: IObjectMeta = nil; const MemberMeta: IMemberMeta = nil); overload;
  end;

  IGeneratorList = interface(INamedInterfaceList)
    ['{9CB13E24-9C7C-49A6-A843-A9828E4D4040}']
  end;
  TGeneratorList = class(TNamedInterfaceList, IGeneratorList);

  TDataFactory = class(TCustomObject, IDataFactory)
  private
    FItems: IGeneratorList;
    function GetGeneratorClass(const AClass: TClass): TDataGeneratorClass;
    function GetGenerator(const Name: string; const GeneratorClass: TDataGeneratorClass): IDataGenerator;
    function GetMemberMeta(const ValueType: IMemberType; const Meta: IObjectMeta = nil): IMemberMeta;
    function GetObjectMeta(const ValueType: IMemberType): IObjectMeta;
  protected
    function GetName(const Member: IMemberMeta; const Meta: IObjectMeta): string; overload;
    function GetName(const Member: IMemberType; const Meta: IObjectMeta): string; overload;

    function GetMapping: IMapping;
    property Mapping: IMapping read GetMapping;

    procedure Next(const AObject: IObjectType); overload;
    procedure Next(const AMember: IMemberType; const ObjectMeta: IObjectMeta = nil; const MemberMeta: IMemberMeta = nil); overload;
  public
    destructor Destroy; override;
  end;

var
  DefaultGeneratorClass: TDataGeneratorClass = TGuidGenerator;

implementation

uses
  SysUtils,
  JazzConsts,
  JazzPersisterConsts,
  JazzMapping,
  JazzUtils,
  JazzSessionIntf,
  JazzValueType;

function TDataFactory.GetMapping: IMapping;
begin
  Result:= IMapping(Owner);
end;

procedure TDataFactory.Next(const AObject: IObjectType);
var
  I: Integer;
  LOIDMeta: IMemberMeta;
  LObjectMeta: IObjectMeta;
begin
  LObjectMeta:= GetObjectMeta(AObject);
  if LObjectMeta <> nil then
  begin
    for I:= 0 to LObjectMeta.OID.Count -1 do
    begin
      LOIDMeta:= LObjectMeta.OID[I];
      Next(AObject.Member[LOIDMeta.MemberName], LObjectMeta, LOIDMeta);
    end;
  end;
end;

function TDataFactory.GetName(const Member: IMemberType;
  const Meta: IObjectMeta): string;
var
  LMemberMeta: IMemberMeta;
begin
  Result:= '';
  if Supports(Meta.FindMember(Member.Name), IMemberMeta, LMemberMeta) then
    Result:= GetName(LMemberMeta, Meta);
end;

procedure TDataFactory.Next(const AMember: IMemberType;
  const ObjectMeta: IObjectMeta; const MemberMeta: IMemberMeta);
var
  LGenerator: IDataGenerator;
  LObjectMeta: IObjectMeta;
  LMemberMeta: IMemberMeta;
begin
  if not Supports(ObjectMeta, IObjectMeta, LObjectMeta) then
    LObjectMeta:= GetObjectMeta(AMember);

  if not Supports(MemberMeta, IMemberMeta, LMemberMeta) then
    LMemberMeta:= GetMemberMeta(AMember, LObjectMeta); 

  LGenerator:= GetGenerator(
    GetName(LMemberMeta, LObjectMeta),
    GetGeneratorClass(LMemberMeta.GeneratorClass)
  );
  LGenerator.Next(AMember);
end;

function TDataFactory.GetObjectMeta(const ValueType: IMemberType): IObjectMeta;
var
  LObject: IObjectType;
  LOwner: IValueType;
begin
  Result:= nil;

  if Supports(ValueType, IObjectType, LObject) then
    Result:= Mapping.Find(LObject.GetClassName)
  else
  if Supports(ValueType.Owner, IValueType, LOwner) then
    Result:= Mapping.Find(LOwner.GetClassName);
end;

function TDataFactory.GetName(const Member: IMemberMeta;
  const Meta: IObjectMeta): string;
var
  LClass: TDataGeneratorClass;
begin
  LClass:= GetGeneratorClass(Member.GeneratorClass);
  
  case LClass.GetContext of
    gcApplication:
      Result:= LClass.ClassName;
    gcClass:
      Result:= LClass.ClassName + '.' + Meta.ObjectClassName;
    gcClassMember:
      Result:= LClass.ClassName + '.' + Meta.ObjectClassName + '.' + Member.GetClassName;
  else
    Result:= '';
  end;
end;

function TDataFactory.GetGenerator(const Name: string;
  const GeneratorClass: TDataGeneratorClass): IDataGenerator;
var
  LIndex: Integer;
begin
  if FItems = nil then FItems:= TGeneratorList.Create;
  LIndex:= FItems.IndexOf(Name);
  
  if LIndex = NotFound then
  begin
    Result:= GeneratorClass.Create;
    FItems.Add(Name, Result);
  end
  else
    Result:= FItems[LIndex] as IDataGenerator;
end;

destructor TDataFactory.Destroy;
begin
  if FItems <> nil then FItems.Clear;
  inherited;
end;

{ TDataGenerator }

constructor TDataGenerator.Create;
begin
  inherited Create;
  FLock:= TCriticalSection.Create;
end;

destructor TDataGenerator.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TDataGenerator.GetClassContext: string;
begin
  Result:= FClassContext;
end;

class function TDataGenerator.GetContext: TGeneratorContext;
begin
  Result:= gcClass;
end;

function TDataGenerator.GetOrder: TGeneratorOrder;
begin
  Result:= FOrder; 
end;

procedure TDataGenerator.Lock;
begin
  FLock.Enter;
end;

procedure TDataGenerator.SetClassContext(const Value: string);
begin
  FClassContext:= Value;
end;

procedure TDataGenerator.SetOrder(const Value: TGeneratorOrder);
begin
  FOrder:= Value;
end;

class function TGuidGenerator.GetContext: TGeneratorContext;
begin
  Result:= gcApplication;
end;

function TGuidGenerator.Next(const Member: IMemberType): boolean;
begin
  Lock;
  try
    if Member.IsNull then
    begin
      Member.AsString:= JazzUtils.GenerateGUID;
      Result:= True;
    end
    else
      Result:= False;
  finally
    Unlock;
  end;
end;

{ TNegativeIntegerGenerator }

constructor TIntegerGenerator.Create;
begin
  inherited;
  FLast:= 0;
end;

class function TIntegerGenerator.GetContext: TGeneratorContext;
begin
  Result:= gcClassMember;
end;

function TIntegerGenerator.GetLast: Int64;
begin
  Result:= FLast;
end;

function TIntegerGenerator.Next(const Member: IMemberType): boolean;
begin
  Lock;
  try
    Result:= False;
    if Member.IsNull and IsNumericType(Member) then
    begin
      GenerateNext;
      (Member as INumericType).AsInteger:= Last;
      Result:= True;
    end;
  finally
    Unlock;
  end;
end;

procedure TDataGenerator.Unlock;
begin
  FLock.Release;
end;

{ TAscendingGenerator }

constructor TAscendingGenerator.Create;
begin
  inherited;
  Order:= goAscending;
end;

procedure TIntegerGenerator.SetLast(const Value: Int64);
begin
  FLast:= Value;
end;

procedure TAscendingGenerator.GenerateNext;
begin
  inherited;
  Inc(FLast);
end;

{ TDescendingGenerator }

constructor TDescendingGenerator.Create;
begin
  inherited;
  Order:= goDescending;
end;

procedure TDescendingGenerator.GenerateNext;
begin
  inherited;
  Dec(FLast);
end;

function TDataFactory.GetGeneratorClass(const AClass: TClass): TDataGeneratorClass;
begin
  if AClass <> nil then
  begin
    if AClass.InheritsFrom(TDataGenerator) then
      Result:= TDataGeneratorClass(AClass)
    else
      raise Exception.CreateFmt(SNotDataGeneratorClass, [AClass.ClassName]);
  end
  else
    Result:= DefaultGeneratorClass;
end;

function TDataFactory.GetMemberMeta(const ValueType: IMemberType;
  const Meta: IObjectMeta): IMemberMeta;
var
  LObjectMeta: IObjectMeta; 
begin
  if Meta <> nil then
    LObjectMeta:= Meta
  else
    LObjectMeta:= GetObjectMeta(ValueType);

  Result:= LObjectMeta.FindMember(ValueType.Name); 
end;

end.

