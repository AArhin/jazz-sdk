unit JazzValueType;

{$DEFINE AUTO_REGISTER_SERVICE}
interface
{ TODO -oCesar -cRefactory : Weak reference in object relationship }
{ TODO -oCesar -cRefactory : add an EnumeratedType }
{ TODO -oCesar -cRefactory : Default Values in AddMember }
{ TODO -oCesar -cRefactory : Check options for attributes }
{ TODO -oCesar -cRefactory : Check Min/Max Values in Numeric Types }

uses
  Classes,
  SqlTimSt,
  SyncObjs,
  SysUtils,
  JazzClasses,
  JazzConsts,
  JazzNotify,
  JazzNotifyIntf,
  JazzObserverIntf,
  JazzSubjectIntf,
  JazzTypeInfo,
  JazzTypes,
  JazzValueTypeIntf;

type
  EJazzObjectClassNotDefined = class(EJazz)
  protected
    function GetDefaultMessage: string; override;
  end;

  EJazzMemberNotFound = class(EJazz)
  protected
    function GetDefaultMessage: string; override;
  end;

  EJazzInsert = class(EJazz)
  protected
    function GetDefaultMessage: string; override;
  end;

  TValueTypeClass = class of TValueType;
  TValueType = class(TCustomObject, IValueType, ISubject)
  private
    FSubject: ISubject;
    FLock: TCriticalSection;
    FLoading: boolean;
    FDestroyng: boolean;
    procedure Lock;
    procedure Unlock;
    function CanNotifySubject: boolean;
  protected
    // IValueType
    function Clone: IValueType; virtual;
    function GetSubject: ISubject;
    function GetValueTypeKind: TValueTypeKind; virtual;
    procedure Assign(const Source: IValueType); virtual; abstract;
    property Subject: ISubject read GetSubject;
    property ValueTypeKind: TValueTypeKind read GetValueTypeKind;

    // ISubject
    function GetNotifying: boolean;
    procedure Attach(const Observer: IObserver);
    procedure Detach(const Observer: IObserver);
    procedure DetachAll;
    procedure Notify(const Notification: IObjectEvent); virtual;
    procedure NotifySubject(const Notification: IObjectEvent);

    // Notification - updating
    function GetLoading: boolean;
    procedure SetLoading(const Value: boolean);
    function GetDestroying: Boolean;
    procedure SetDestroying(const Value: Boolean);

    property Notifying: boolean read GetNotifying;
    property Loading: boolean read GetLoading write SetLoading;
    property Destroying: Boolean read GetDestroying write SetDestroying;
  public
    class function NewList: IObjectListType;
    constructor Create(const AOwner: IInterface = nil); override;
    destructor Destroy; override;
  end;

  TMemberTypeClass = class of TMemberType;
  TMemberType = class(TValueType, IMemberType, IMemberState, IPersisterState,
    IStreamable, IMemberStreamable)
  private
    FIsNull: boolean;
    FName: string;
    FDataProvider: Pointer;
    FState: IState;
  protected
    // IStreamable
    procedure LoadFromStream(const Reader: TReader); virtual;
    procedure SaveToStream(const Writer: TWriter); virtual;

    // IMemberStreamable
    function LoadNullFromStream(const Reader: TReader): boolean; virtual;
    function SaveNullToStream(const Writer: TWriter): boolean; virtual;

    // IMemberType
    function GetAsString: string; virtual; abstract;
    function GetName: string;
    procedure SetAsString(const Value: string); virtual; abstract;

    function Clone: IValueType; override;
    function DataProvider: IDataProvider;
    procedure SetProvider(const Provider: IDataProvider);

    function IsNull: boolean; virtual;
    procedure Clear; virtual;
    procedure LoadFromDataProvider(LoadCascade: boolean = False); virtual;

    property AsString: string read GetAsString write SetAsString;
    property Name: string read GetName;

    // IMemberState
    function GetState: IState;
    procedure Modified;
    procedure Loaded(const DataProvider: IInterface = nil);
    procedure Persisted(const DataProvider: IInterface = nil);
    property State: IState read GetState;

    // override
    procedure Assign(const Source: IValueType); override;
    procedure Notify(const Notification: IObjectEvent); override;

  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); reintroduce; virtual;
  end;

  TObjectTypeClass = class of TObjectType;
  TObjectType = class(TMemberType, IObjectType, IObjectState, IStreamable)
  private
    FMemberList: IMemberList;
    FObjectList: Pointer;
    FObjectListAssigned: boolean;
    procedure CheckAutoLoadMember(const Member: IMemberType);
    function IsObjectMember(MemberName: string): Boolean;
  protected
    function GetObjectList: IObjectListType;

    // IObjectState and IObjectType
    procedure Delete;
    procedure Deleted;
    procedure CancelDelete;

    function GetMember(const Name: string): IMemberType;
    function GetMembers(const Index: Integer): IMemberType;
    function GetMembersCount: Integer;
    function GetMemberList: IMemberList;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; IID: TGUID); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; const ItemClass: TClass); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; const ItemClass: TClass; IID: TGUID); overload;
    procedure InitInstance; virtual;

    property Member[const Name: string]: IMemberType read GetMember;
    property Members[const Index: Integer]: IMemberType read GetMembers; default;
    property MembersCount: Integer read GetMembersCount;
    property MemberList: IMemberList read GetMemberList;

    // overrided
    function Clone: IValueType; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    procedure SetAsString(const Value: string); override;
    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure Notify(const Notification: IObjectEvent); override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string); overload; override;
    constructor Create(const AOwner: IValueType = nil); reintroduce; overload; virtual;
  end;

  PDeletingItem = ^TDeletingItem;
  TDeletingItem = record
    Index: Integer;
    Instance: IInterface;
  end;

  TObjectListTypeClass = class of TObjectListType;
  TObjectListType = class(TMemberType, IObjectListType, IObjectListState, IStreamable)
  private
    FDeletingItems: TList;
    FItemClass: TClass;
    FItems: IInterfaceList;
    FUpdating: Integer;
  protected
    // Deleting
    function GetDeletingCount: Integer;
    function GetDeletingItems(const Index: Integer): IObjectType;
    function IndexOfDeleting(const AObject: IObjectType): Integer;
    procedure AddDeleting(const AObject: IObjectType);
    procedure InternalCancelDeleting(const AObject: IObjectType = nil; CanNotify: Boolean = True);
    procedure CancelDeleting(const AObject: IObjectType = nil);
    procedure RemoveDeleting(const AObject: IObjectType); overload;
    procedure RemoveDeleting(const Index: Integer); overload;
    property DeletingCount: Integer read GetDeletingCount;
    property DeletingItems[const Index: Integer]: IObjectType read GetDeletingItems;

    // Items
    function Add(const Item: IObjectType): Integer; overload; virtual;
    function Add: IObjectType; overload; virtual;
    function New: IObjectType; virtual;
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Item: IInterface);
    procedure Remove(Index: Integer);
    procedure RemoveObject(const AObject: IObjectType);

    function FindObject(const Names: array of string; Values: array of string): IObjectType;
    function FindObjectIndex(const Names: array of string; Values: array of string): Integer;
    function GetCount: Integer;
    function GetItemClass: TClass;
    function GetItems(const Index: Integer): IObjectType;
    function IndexOf(const AObject: IObjectType): Integer;
    property Count: Integer read GetCount;
    property ItemClass: TClass read GetItemClass;
    property Items[const Index: Integer]: IObjectType read GetItems; default;

    function IsUpdating: boolean;
    procedure BeginUpdate;
    procedure EndUpdate(const CallNotify: boolean = True);
    // override
    procedure Clear; override;
    function Clone: IValueType; override;
    procedure Notify(const Notification: IObjectEvent); override;

    procedure Assign(const Source: IValueType); override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;

    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetValueTypeKind: TValueTypeKind; override;
  public
    constructor Create(const ItemClass: TClass); reintroduce; overload; virtual;
    constructor Create(const AOwner: IValueType; const ItemClass: TClass); reintroduce; overload; virtual;
    constructor Create(const AOwner: IValueType; const ItemClass: TClass; const Name: string); reintroduce; overload; virtual;
    destructor Destroy; override;
    function GetEnumerator: IObjectListTypeEnumerator;
  end;

  TObjectListTypeEnumerator = class(TInterfacedObject, IObjectListTypeEnumerator)
  private
    FIndex: Integer;
    FList: IObjectListType;
  public
    constructor Create(AList: IObjectListType);
    function GetCurrent: IObjectType;
    function MoveNext: Boolean;
    property Current: IObjectType read GetCurrent;
  end;

  TBlobType = class(TMemberType, IBlobType)
  private
    FValue: TStream;
    FOnGetValue: TOnGetBlobValue;
    FOnSetValue: TOnSetBlobValue;
  protected
    function GetOnGetValue: TOnGetBlobValue;
    function GetOnSetValue: TOnSetBlobValue;
    procedure SetOnGetValue(Value: TOnGetBlobValue);
    procedure SetOnSetValue(Value: TOnSetBlobValue);

    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: TStream;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: TStream);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); overload; override;
    procedure SaveToStream(Stream: TStream); reintroduce; overload;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    destructor Destroy; override;
    property Value: TStream read GetValue write SetValue;
    property OnGetValue: TOnGetBlobValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetBlobValue read GetOnSetValue write SetOnSetValue;
  end;

  TBooleanType = class(TMemberType, IBooleanType)
  private
    FValue: boolean;
    FOnGetValue: TOnGetBooleanValue;
    FOnSetValue: TOnSetBooleanValue;
  protected
    function GetOnGetValue: TOnGetBooleanValue;
    function GetOnSetValue: TOnSetBooleanValue;
    procedure SetOnGetValue(Value: TOnGetBooleanValue);
    procedure SetOnSetValue(Value: TOnSetBooleanValue);

    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: boolean;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: boolean);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    property Value: boolean read GetValue write SetValue;
    property OnGetValue: TOnGetBooleanValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetBooleanValue read GetOnSetValue write SetOnSetValue;
  end;

  TCharType = class(TMemberType, ICharType)
  private
    FValue: Char;
    FOnGetValue: TOnGetCharValue;
    FOnSetValue: TOnSetCharValue;
  protected
    function GetOnGetValue: TOnGetCharValue;
    function GetOnSetValue: TOnSetCharValue;
    procedure SetOnGetValue(Value: TOnGetCharValue);
    procedure SetOnSetValue(Value: TOnSetCharValue);
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: Char;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Char);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: Char read GetValue write SetValue;
    property OnGetValue: TOnGetCharValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetCharValue read GetOnSetValue write SetOnSetValue;
  end;

  TNumericType = class(TMemberType, INumericType)
  protected
    function GetAsFloat: Double; virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    function GetValueTypeKind: TValueTypeKind; override;
    procedure SetAsFloat(const Value: Double); virtual; abstract;
    procedure SetAsInteger(const Value: Integer); virtual; abstract;

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  TCurrencyType = class(TNumericType, ICurrencyType)
  private
    FValue: Currency;
    FOnGetValue: TOnGetCurrencyValue;
    FOnSetValue: TOnSetCurrencyValue;
  protected
    function GetOnGetValue: TOnGetCurrencyValue;
    function GetOnSetValue: TOnSetCurrencyValue;
    procedure SetOnGetValue(Value: TOnGetCurrencyValue);
    procedure SetOnSetValue(Value: TOnSetCurrencyValue);

    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: Currency;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Currency);

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: Currency read GetValue write SetValue;
    property OnGetValue: TOnGetCurrencyValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetCurrencyValue read GetOnSetValue write SetOnSetValue;
  end;

  TDateType = class(TMemberType, IDateType)
  private
    FValue: TDateTime;
    FOnGetValue: TOnGetDateValue;
    FOnSetValue: TOnSetDateValue;
  protected
    function GetAsString: string; override;
    function GetOnGetValue: TOnGetDateValue;
    function GetOnSetValue: TOnSetDateValue;
    function GetValue: TDateTime;
    function GetValueTypeKind: TValueTypeKind; override;
    procedure SetAsString(const Value: string); override;
    procedure SetOnGetValue(Value: TOnGetDateValue);
    procedure SetOnSetValue(Value: TOnSetDateValue);
    procedure SetTime(const Value: TDateTime);
    procedure SetTimeStamp(const Value: TTimeStamp);
    procedure SetValue(const Value: TDateTime); virtual;

    function AsSQLTimeStamp: TSQLTimeStamp;
    function AsTimeStamp: TTimeStamp;
    function AsTimeStampString: string;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: TDateTime read GetValue write SetValue;
    property OnGetValue: TOnGetDateValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetDateValue read GetOnSetValue write SetOnSetValue;
  end;

  TTimeType = class(TDateType, ITimeType)
  protected
    function GetValueTypeKind: TValueTypeKind; override;
    procedure SetValue(const Value: TDateTime); override;
  end;

  TFloatType = class(TNumericType, IFloatType)
  private
    FValue: Double;
    FOnGetValue: TOnGetFloatValue;
    FOnSetValue: TOnSetFloatValue;
  protected
    function GetOnGetValue: TOnGetFloatValue;
    function GetOnSetValue: TOnSetFloatValue;
    procedure SetOnGetValue(Value: TOnGetFloatValue);
    procedure SetOnSetValue(Value: TOnSetFloatValue);

    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: Double;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Double); virtual;

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: Double read GetValue write SetValue;
    property OnGetValue: TOnGetFloatValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetFloatValue read GetOnSetValue write SetOnSetValue;
  end;

  TIntegerType = class(TNumericType, IIntegerType)
  private
    FValue: Integer;
    FOnGetValue: TOnGetIntegerValue;
    FOnSetValue: TOnSetIntegerValue;
  protected
    function GetOnGetValue: TOnGetIntegerValue;
    function GetOnSetValue: TOnSetIntegerValue;
    procedure SetOnGetValue(Value: TOnGetIntegerValue);
    procedure SetOnSetValue(Value: TOnSetIntegerValue);
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: Integer;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: Integer);

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: Integer read GetValue write SetValue;
    property OnGetValue: TOnGetIntegerValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetIntegerValue read GetOnSetValue write SetOnSetValue;
  end;

  TLongIntType = class(TNumericType, ILongIntType)
  private
    FValue: LongInt;
    FOnGetValue: TOnGetLongIntValue;
    FOnSetValue: TOnSetLongIntValue;
  protected
    function GetOnGetValue: TOnGetLongIntValue;
    function GetOnSetValue: TOnSetLongIntValue;
    procedure SetOnGetValue(Value: TOnGetLongIntValue);
    procedure SetOnSetValue(Value: TOnSetLongIntValue);
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: LongInt;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: LongInt);

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: LongInt read GetValue write SetValue;
    property OnGetValue: TOnGetLongIntValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetLongIntValue read GetOnSetValue write SetOnSetValue;
  end;

  TMemoType = class(TMemberType, IMemoType)
  private
    FValue: TStrings;
    FOnGetValue: TOnGetMemoValue;
    FOnSetValue: TOnSetMemoValue;
    FLocked: boolean;
    function InternalLocked: boolean;
    procedure InternalLock;
    procedure InternalUnlock;
    procedure MemoChanged(Sender: TObject); virtual;
  protected
    function GetOnGetValue: TOnGetMemoValue;
    function GetOnSetValue: TOnSetMemoValue;
    procedure SetOnGetValue(Value: TOnGetMemoValue);
    procedure SetOnSetValue(Value: TOnSetMemoValue);
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: TStrings;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: TStrings);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    destructor Destroy; override;
    property Value: TStrings read GetValue write SetValue;
    property OnGetValue: TOnGetMemoValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetMemoValue read GetOnSetValue write SetOnSetValue;
  end;

  TSmallIntType = class(TNumericType, ISmallIntType)
  private
    FValue: SmallInt;
    FOnGetValue: TOnGetSmallIntValue;
    FOnSetValue: TOnSetSmallIntValue;
  protected
    function GetOnGetValue: TOnGetSmallIntValue;
    function GetOnSetValue: TOnSetSmallIntValue;
    procedure SetOnGetValue(Value: TOnGetSmallIntValue);
    procedure SetOnSetValue(Value: TOnSetSmallIntValue);
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: SmallInt;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: SmallInt);

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    constructor Create(const AOwner: IValueType; const Name: string = EmptyStr); override;
    property Value: SmallInt read GetValue write SetValue;
    property OnGetValue: TOnGetSmallIntValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetSmallIntValue read GetOnSetValue write SetOnSetValue;
  end;

  TStringType = class(TMemberType, IStringType)
  private
    FOnGetValue: TOnGetStringValue;
    FOnSetValue: TOnSetStringValue;
    FValue: string;
  protected
    function GetAsString: string; override;
    function GetValue: string;
    function GetValueTypeKind: TValueTypeKind; override;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: string);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    function GetOnGetValue: TOnGetStringValue;
    function GetOnSetValue: TOnSetStringValue;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
    procedure SetOnGetValue(Value: TOnGetStringValue);
    procedure SetOnSetValue(Value: TOnSetStringValue);
  public
    property Value: string read GetValue write SetValue;
    property OnGetValue: TOnGetStringValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetStringValue read GetOnSetValue write SetOnSetValue;
  end;

  TWideStringType = class(TMemberType, IWideStringType)
  private
    FValue: WideString;
    FOnGetValue: TOnGetWideStringValue;
    FOnSetValue: TOnSetWideStringValue;
  protected
    function GetOnGetValue: TOnGetWideStringValue;
    function GetOnSetValue: TOnSetWideStringValue;
    procedure SetOnGetValue(Value: TOnGetWideStringValue);
    procedure SetOnSetValue(Value: TOnSetWideStringValue);
    function GetAsString: string; override;
    function GetValueTypeKind: TValueTypeKind; override;
    function GetValue: WideString;
    procedure SetAsString(const Value: string); override;
    procedure SetValue(const Value: WideString);

    procedure Assign(const Source: IValueType); override;
    procedure Clear; override;
    procedure LoadFromStream(const Reader: TReader); override;
    procedure SaveToStream(const Writer: TWriter); override;
  public
    property Value: WideString read GetValue write SetValue;
    property OnGetValue: TOnGetWideStringValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetWideStringValue read GetOnSetValue write SetOnSetValue;
  end;

  TState = class(TInterfacedObject, IState, IStreamable)
  private
    FController: Pointer;
    FDeleting: boolean;
    FDeleted: boolean;
    FLoaded: Boolean;
    FModified: Boolean;
    FPersisted: Boolean;
    function GetLoaded: boolean;
    function GetDeleting: boolean;
    function GetDeleted: boolean;
    function GetModified: boolean;
    function GetPersisted: boolean;
  protected
    function GetController: IInterface;
    procedure Assign(const Source: IState);
    procedure LoadFromStream(const Reader: TReader);
    procedure SaveToStream(const Writer: TWriter); virtual;
    procedure UpdateState(const Notification: IStateChangedEvent);
    procedure Reset;

    property Loaded: boolean read GetLoaded;
    property Deleting: boolean read GetDeleting;
    property Deleted: boolean read GetDeleted;
    property Modified: boolean read GetModified;
    property Persisted: boolean read GetPersisted;
  public
    constructor Create(const Controller: IInterface); reintroduce;
  end;

  // Object/Member State
  TStateChangedEvent = class(TObjectEvent, IStateChangedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TLoadedEvent = class(TStateChangedEvent, ILoadedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TDeletingEvent = class(TStateChangedEvent, IDeletingEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TDeleteEvent = class(TDeletingEvent, IDeleteEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TCancelDeletingEvent = class(TStateChangedEvent, ICancelDeletingEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TDeletedEvent = class(TStateChangedEvent, IDeletedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TModifiedEvent = class(TStateChangedEvent, IModifiedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TPersistedEvent = class(TStateChangedEvent, IPersistedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  { for internal use in TBlobType }
  TStreamType = class(TMemoryStream)
  private
    FOwner: Pointer;
  protected
    procedure InternalClear; virtual;
  public
    constructor Create(const AOwner: IMemberType); virtual;
    function Write(const Buffer; Count: Integer): Longint; override;
    procedure Clear; virtual;
  end;

  TValueTypeFactory = class(TInterfacedObject, IValueTypeFactory)
  private
    function NewValueType(const TypeClass: TMemberTypeClass; const Owner: IInterface): IValueType;
  public
    function NewBlob(const Value: TStream; const Owner: IInterface = nil): IBlobType;
    function NewBoolean(const Value: boolean; const Owner: IInterface = nil): IBooleanType;
    function NewChar(const Value: Char; const Owner: IInterface = nil): ICharType;
    function NewCurrency(const Value: Currency; const Owner: IInterface = nil): ICurrencyType;
    function NewDate(const Value: TDateTime; const Owner: IInterface = nil): IDateType;
    function NewFloat(const Value: Double; const Owner: IInterface = nil): IFloatType;
    function NewInteger(const Value: Integer; const Owner: IInterface = nil): IIntegerType;
    function NewLongInt(const Value: LongInt; const Owner: IInterface = nil): ILongIntType;
    function NewMemo(const Value: string; const Owner: IInterface = nil): IMemoType; overload;
    function NewMemo(const Value: TStrings; const Owner: IInterface = nil): IMemoType; overload;
    function NewSmallInt(const Value: SmallInt; const Owner: IInterface = nil): ISmallIntType;
    function NewString(const Value: string; const Owner: IInterface = nil): IStringType;
    function NewWideString(const Value: WideString; const Owner: IInterface = nil): IWideStringType;
  end;

var
{$IFDEF AUTO_REGISTER_SERVICE}
  AutoRegisterObject: boolean = False;
  AutoRegisterObjectList: boolean = True;
{$ELSE}
  AutoRegisterObject: boolean = False;
  AutoRegisterObjectList: boolean = False;
{$ENDIF}

  ValueTypes: array[1..16] of TClass = (
    TBlobType,
    TCharType,
    TCurrencyType,
    TDateType,
    TFloatType,
    TIntegerType,
    TLongIntType,
    TMemoType,
    TSmallIntType,
    TStringType,
    TTimeType,
    TWideStringType,
    TValueType,
    TMemberType,
    TObjectType,
    TObjectListType
    );

const
  FirstMemberType = 1; // TBlobType
  LastMemberType = 11; // TWideStringType

function IsNumericType(const Member: IMemberType): boolean;
function IsValueTypeClass(const TypeClass: TClass): boolean;
function IsMemberTypeClass(const TypeClass: TClass): boolean;
function TypeFactory: IValueTypeFactory;

function TypeRegister: ITypeRegister;
function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const ItemClass: TClass = nil): ITypeInfo;
function GetTypeClass(ClassName: string): TClass;

implementation

uses
{$IFDEF AUTO_REGISTER_SERVICE}
  JazzIntfUtils,
{$ENDIF}
  Math,
  Variants,
  JazzCoreConsts,
  JazzSubject,
  JazzUtils;

var
  _ValueTypeFactory: IValueTypeFactory;

function TypeFactory: IValueTypeFactory;
begin
  if _ValueTypeFactory = nil then
    _ValueTypeFactory := TValueTypeFactory.Create;
  Result := _ValueTypeFactory;
end;

function IsNumericType(const Member: IMemberType): boolean;
begin
  Result := Member.ValueTypeKind in [vtCurrency, vtFloat, vtInteger, vtLongInt,
    vtSmallInt, vtNumeric];
end;

function IsValueTypeClass(const TypeClass: TClass): boolean;
var
  I: Integer;
begin
  Result := False;
  if TypeClass = nil then
    Exit;
  for I := Low(ValueTypes) to High(ValueTypes) do
  begin
    if ValueTypes[I] = TypeClass then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IsMemberTypeClass(const TypeClass: TClass): boolean;
var
  I: Integer;
begin
  Result := False;
  if TypeClass = nil then
    Exit;

  for I := FirstMemberType to LastMemberType do
  begin
    if ValueTypes[I] = TypeClass then
    begin
      Result := True;
      Break;
    end;
  end;
end;

constructor TValueType.Create(const AOwner: IInterface);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FSubject := TSubject.Create(Self);
end;

{ TValueType }

function TValueType.Clone: IValueType;
begin
  Result := TValueTypeClass(ClassType).Create(Owner) as IValueType;
  Result.Assign(Self);
end;

function TypeRegister: ITypeRegister;
begin
  Result := JazzTypeInfo.TypeRegister;
end;

function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass:
  TClass; const ItemClass: TClass = nil): ITypeInfo;
begin
  Result := JazzTypeInfo.RegisterType(IID, TypeName, TypeClass, ItemClass);
end;

function GetTypeClass(ClassName: string): TClass;
begin
  Result:= TypeRegister.GetTypeInfo(ClassName, True).TypeClass;
end;

{ TJazzValueTypeMember }

procedure TMemberType.Assign(const Source: IValueType);
var
  LSource: IMemberType;
begin
  Assert(Supports(Source, IMemberType, LSource), SIntfNotSupported);
  State.Assign((LSource as IMemberState).State);
  FIsNull := LSource.IsNull;
  FName := LSource.Name;
end;

constructor TMemberType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited Create(AOwner);
  FState := TState.Create(Self);
  FIsNull := True;
  FName := Name;
end;

function TMemberType.GetName: string;
begin
  Result := FName;
end;

function TMemberType.IsNull: boolean;
begin
  Result := FIsNull;
end;

procedure TMemberType.Clear;
begin
  if (DataProvider <> nil) and not IsNull then
    Modified;
  FIsNull := True;
  Notify(NewEvent(TClearEvent, Self));
end;

function TMemberType.Clone: IValueType;
begin
  Result := TMemberTypeClass(ClassType).Create(Owner as IValueType) as IValueType;
  Result.Assign(Self);
end;

function TValueType.GetSubject: ISubject;
begin
  Result := FSubject;
end;

procedure TValueType.Attach(const Observer: IObserver);
begin
  Subject.Attach(Observer);
end;

procedure TValueType.Detach(const Observer: IObserver);
begin
  Subject.Detach(Observer);
end;

function TValueType.GetNotifying: boolean;
begin
  Result := Subject.Notifying;
end;

procedure TValueType.Notify(const Notification: IObjectEvent);
begin
  Lock;
  try
    if CanNotifySubject then
      NotifySubject(Notification);
  finally
    Unlock;
  end;
end;

class function TValueType.NewList: IObjectListType;
begin
  Result := TObjectListType.Create(Self);
end;

procedure TValueType.Lock;
begin
  FLock.Enter;
end;

procedure TValueType.Unlock;
begin
  FLock.Release;
end;

destructor TValueType.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TValueType.CanNotifySubject: boolean;
begin
  Result := not Loading;
end;

constructor TObjectType.Create(const AOwner: IValueType);
begin
  Create(AOwner, EmptyStr);
end;

constructor TObjectType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited Create(AOwner, Name);
{$IFDEF AUTO_REGISTER_SERVICE}
  if AutoRegisterObject then
    AutoRegisterService(Self);
{$ENDIF}
  FMemberList := TNamedInterfaceList.Create;
  Loading := True;
  try
    InitInstance;
  finally
    Loading := False;
  end;
end;

function TObjectType.IsObjectMember(MemberName: string): Boolean;
begin
  Result := Pos('.', MemberName) > 0;
end;

function TObjectType.GetMember(const Name: string): IMemberType;
var
  LIndex: Integer;
  LMember: IMemberType;
  LName: string;
  LObject: IObjectType;
begin
  Result:= nil;
  if not IsObjectMember(Name) then
    Result := MemberList.Item[Name] as IMemberType
  else
  begin
    LIndex := 1;
    LName := StrGetToken(Name, '.', LIndex);
    LMember := MemberList.Item[LName] as IObjectType;

    while (LIndex <> -1) and (LMember <> nil) do
    begin
      LName := StrGetToken(Name, '.', LIndex);
      if Supports(LMember, IObjectType, LObject) then
        LMember := LObject.Member[LName] as IMemberType;
    end;

    Result:= LMember;
  end;
  if Result = nil then
    raise EJazzMemberNotFound.CreateFmt([Name, Self.GetClassName])
  else
    CheckAutoLoadMember(Result);
end;

procedure TObjectType.CheckAutoLoadMember(const Member: IMemberType);
var
  LObject: IObjectState;
begin
  if Supports(Member, IObjectState, LObject) and not LObject.State.Loaded then
    Member.LoadFromDataProvider;
end;

function TObjectType.GetMembers(const Index: Integer): IMemberType;
begin
  Result := MemberList[Index] as IMemberType;
  CheckAutoLoadMember(Result);
end;

procedure TObjectType.Assign(const Source: IValueType);
var
  I: Integer;
begin
  Assert(ClassName = Source.Implementor.ClassName, SIntfNotSupported);
  inherited Assign(Source);

  for I := 0 to (Source as IObjectType).MemberList.Count - 1 do
    Members[I].Assign((Source as IObjectType)[I]);
end;

procedure TObjectType.Clear;
var
  I: Integer;
begin
  for I := 0 to MemberList.Count - 1 do
    Members[I].Clear;
end;

function TObjectType.Clone: IValueType;
begin
  Result := TObjectTypeClass(ClassType).Create(Owner as IValueType) as IValueType;
  Result.Assign(Self);
end;

function TObjectType.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to MemberList.Count - 1 do
    Result := Result + Members[I].AsString + EOL;
end;

procedure TObjectType.LoadFromStream(const Reader: TReader);
var
  LName: string;
  LClassName: string;
  LMember: IMemberType;
begin
  with Reader do
  begin
    ReadListBegin;
    inherited LoadFromStream(Reader);
    while not Reader.EndOfList do
    begin
      LName := ReadString;
      LClassName := ReadString;
      LMember := Member[LName];
      if LMember <> nil then
        (LMember as IMemberStreamable).LoadFromStream(Reader);
    end;
    ReadListEnd;
  end;
end;

procedure TObjectType.SaveToStream(const Writer: TWriter);
var
  I: Integer;
  LMember: IMemberType;
  LStreamable: IMemberStreamable;
begin
  with Writer do
  begin
    WriteListBegin;
    inherited SaveToStream(Writer);

    for I := 0 to MemberList.Count - 1 do
    begin
      LMember := Members[I];
      WriteString(LMember.Name);
      WriteString(LMember.Implementor.ClassName);
      if Supports(LMember, IMemberStreamable, LStreamable) then
        LStreamable.SaveToStream(Writer);
    end;
    WriteListEnd;
  end;
end;

procedure TObjectListType.Assign(const Source: IValueType);
var
  I: Integer;
  LSourceList: IObjectListType;
begin
  Assert(Supports(Source, IObjectListType, LSourceList), SIntfNotSupported);
  Clear;
  inherited Assign(Source);
  for I := 0 to LSourceList.Count - 1 do
    Add.Assign(LSourceList[I]);

  for I := 0 to LSourceList.DeletingCount - 1 do
    AddDeleting(LSourceList.DeletingItems[I]);
end;

constructor TObjectListType.Create(const AOwner: IValueType;
  const ItemClass: TClass);
begin
  Create(AOwner, ItemClass, Name);
end;

function TObjectListType.GetItemClass: TClass;
begin
  Result := FItemClass;
end;

function TObjectListType.GetItems(const Index: Integer): IObjectType;
begin
  Result := FItems.Items[Index] as IObjectType;
end;

function TObjectListType.Add: IObjectType;
begin
  Result := New;
  Add(Result);
end;

function TObjectListType.GetCount: Integer;
begin
  if FItems <> nil then
  begin
    if (FItems.Count = 0) and not State.Loaded then
    begin
      BeginUpdate;
      LoadFromDataProvider(True);
      EndUpdate(False);
    end;

    Result := FItems.Count;
  end
  else
    Result := 0;
end;

function TObjectListType.Add(const Item: IObjectType): Integer;
var
  LObject: IObjectType;
begin
  if Supports(Item, IObjectType, LObject) then
  begin
    Result := FItems.Add(LObject);
    Notify(NewEvent(TAddEvent, Self));
  end
  else
    Result:= -1;
end;

procedure TObjectListType.Remove(Index: Integer);
begin
  FItems.Delete(Index);
  Notify(NewEvent(TRemoveEvent, Self));
end;

procedure TObjectListType.Insert(Index: Integer; const Item: IInterface);
var
  LObject: IObjectType;
begin
  if Index < 0 then
    Index := 0
  else if Index > Count then
    Index := Count;

  if Supports(Item, IObjectType, LObject) then
  begin
    FItems.Insert(Index, LObject);
    Notify(NewEvent(TAddEvent, Self));
  end
  else
    raise EJazzInsert.Create;
end;

procedure TObjectListType.InternalCancelDeleting(const AObject: IObjectType;
  CanNotify: Boolean);
  procedure RestoreItem(const AObject: IObjectType; Index: Integer; PItem: PDeletingItem = nil);
  var
    LItem: PDeletingItem;
  begin
    if CanNotify then
      (AObject as IObjectState).CancelDelete;

    if PItem <> nil then
      LItem := PItem
    else
      LItem := PDeletingItem(FDeletingItems[Index]);

    if FItems.IndexOf(LItem.Instance) = NotFound then
      Insert(LItem.Index, LItem.Instance);
    RemoveDeleting(Index);
  end;

var
  LIndex: Integer;
  LItem: PDeletingItem;
begin
  if AObject <> nil then
  begin
    LIndex := IndexOfDeleting(AObject);
    if LIndex <> NotFound then
      RestoreItem(AObject, LIndex);
  end
  else
  begin
    while FDeletingItems.Count > 0 do
    begin
      LIndex := FDeletingItems.Count - 1;
      LItem := PDeletingItem(FDeletingItems[LIndex]);
      RestoreItem(LItem.Instance as IObjectType, LIndex, LItem);
    end;
  end;
end;

procedure TObjectListType.LoadFromStream(const Reader: TReader);
var
  LItem: IObjectType;
  LStream: IStreamable;
  LObjectClass: TObjectTypeClass;
  LClassName: string;
begin
  BeginUpdate;
  try
    with Reader do
    begin
      ReadSignature;
      ReadListBegin;
      inherited LoadFromStream(Reader);
      ReadInteger; // Count, just in case
      ReadListBegin;
      while not Reader.EndOfList do
      begin
        LClassName := ReadString;
        if ItemClass.ClassName = LClassName then
          LItem := Add
        else
        begin
          LObjectClass := TObjectTypeClass(TypeRegister.GetTypeInfo(LClassName, True));
          if LObjectClass <> nil then
            LItem := LObjectClass.Create(Self);
        end;
        if Supports(LItem, IStreamable, LStream) then
          LStream.LoadFromStream(Reader);
      end;
      ReadListEnd;
      ReadListEnd;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TObjectListType.SaveToStream(const Writer: TWriter);
var
  I: Integer;
  LItem: IStreamable;
begin
  with Writer do
  begin
    WriteSignature;
    WriteListBegin;
    inherited SaveToStream(Writer);
    WriteInteger(Count); // just for information - beginlist provide loop
    WriteListBegin;
    for I := 0 to Count - 1 do
    begin
      WriteString(Items[I].Implementor.ClassName);
      if Supports(Items[I], IStreamable, LItem) then
        LItem.SaveToStream(Writer);
    end;
    WriteListEnd;
    WriteListEnd;
  end;
end;

procedure TObjectListType.Notify(const Notification: IObjectEvent);
var
  LObject: IObjectType;
  LSubject: ISubject;
begin
  case Notification.NotifyType of
    ntDeleting:
      begin
        if Supports(Notification.Sender, IObjectType, LObject) then
          AddDeleting(LObject);
      end;
    ntCancelDeleting:
      begin
        if Supports(Notification.Sender, IObjectType, LObject) then
          CancelDeleting(LObject)
        else
          CancelDeleting;
      end;
    ntDeleted:
      begin
        if Supports(Notification.Sender, IObjectType, LObject) then
          RemoveDeleting(LObject);
      end;
  end;
  if not Loading then
  begin
    if Supports(Owner, ISubject, LSubject) then
      LSubject.Notify(Notification);
    if not IsUpdating then
      NotifySubject(Notification);
  end;
end;

procedure TObjectListType.Clear;
var
  LItem: PDeletingItem;
  LIndex: Integer;
begin
  if FItems = nil then Exit;
  BeginUpdate;
  try
    FItems.Clear;
    while FDeletingItems.Count > 0 do
    begin
      LIndex := FDeletingItems.Count - 1;
      LItem := PDeletingItem(FDeletingItems[LIndex]);
      Finalize(LItem^);
      Dispose(FDeletingItems[LIndex]);
      FDeletingItems.Delete(LIndex);
    end;
    FDeletingItems.Clear;
  finally
    EndUpdate(False);
    if not Destroying then
      Notify(NewEvent(TClearEvent, Self));
  end;
end;

function TObjectListType.Clone: IValueType;
begin
  Result := TObjectListTypeClass(ClassType).Create(
    Owner as IValueType, ItemClass) as IValueType;
  Result.Assign(Self);
end;

procedure TObjectListType.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TObjectListType.EndUpdate(const CallNotify: boolean);
begin
  if FUpdating > 0 then
    Dec(FUpdating);
  if (FUpdating = 0) and CallNotify then
    Notify(NewEvent(TEndUpdateEvent, Self));
end;

function TObjectListType.IsUpdating: boolean;
begin
  Result := FUpdating > 0;
end;

function TObjectListType.FindObject(const Names: array of string;
  Values: array of string): IObjectType;
var
  LIndex: Integer;
begin
  LIndex := FindObjectIndex(Names, Values);
  if LIndex <> NotFound then
    Result := Items[LIndex]
  else
    Result := nil;
end;

procedure TObjectListType.RemoveObject(const AObject: IObjectType);
var
  LObject: IObjectType;
begin
  if Supports(AObject, IObjectType, LObject) then
  begin
    FItems.Remove(LObject);
    Notify(NewEvent(TRemoveEvent, Self));
  end;
end;

procedure TObjectType.SetAsString(const Value: string);
begin
  raise Exception.Create(SObjectAsString);
end;

function TObjectType.GetObjectList: IObjectListType;
var
  LObjectList: IObjectListType;
begin
  if not FObjectListAssigned then
  begin
    FObjectListAssigned := True;
    if Supports(Owner, IObjectListType, LObjectList) then
      FObjectList := Pointer(LObjectList);
  end;

  Result := IObjectListType(FObjectList);
end;

procedure TObjectType.AddMember(out Instance; const Name: string;
  const MemberClass: TClass; IID: TGUID);
var
  LMember: IMemberType;
begin
  LMember := nil;
  if MemberClass.InheritsFrom(TObjectType) then
    LMember := TObjectTypeClass(MemberClass).Create(Self, Name)
  else
    LMember := TMemberTypeClass(MemberClass).Create(Self, Name);

  if LMember <> nil then
  begin
    MemberList.Add(Name, LMember);
    LMember.QueryInterface(IID, Instance);
  end;
end;

procedure TObjectType.AddMember(out Instance; const Name: string;
  const MemberClass: TClass);
var
  LIID: TGUID;
begin
  LIID := StringToGUID(GUID_NULL);
  if not TypeRegister.GetIID(MemberClass, LIID) then
    Exit;
  AddMember(Instance, Name, MemberClass, LIID);
end;

procedure TObjectType.AddMember(out Instance; const Name: string;
  const MemberClass: TClass; const ItemClass: TClass);
var
  LIID: TGUID;
begin
  LIID := StringToGUID(GUID_NULL);
  if not TypeRegister.GetIID(MemberClass, LIID) then
    Exit;
  AddMember(Instance, Name, MemberClass, ItemClass, LIID);
end;

procedure TObjectType.AddMember(out Instance; const Name: string;
  const MemberClass, ItemClass: TClass; IID: TGUID);
var
  LMember: IMemberType;
begin
  LMember := TObjectListTypeClass(MemberClass).Create(Self, ItemClass, Name);

  if LMember <> nil then
  begin
    MemberList.Add(Name, LMember);
    LMember.QueryInterface(IID, Instance);
  end;
end;

procedure TObjectType.CancelDelete;
var
  LEvent: IStateChangedEvent;
begin
  LEvent := NewEvent(TCancelDeletingEvent, Self) as IStateChangedEvent;
  LEvent.StartNotification;
  try
    State.UpdateState(LEvent);
  finally
    LEvent.EndNotification;
  end;
end;

procedure TObjectType.Delete;
var
  LEvent: IStateChangedEvent;
begin
  LEvent := NewEvent(TDeletingEvent, Self) as IStateChangedEvent;
  LEvent.StartNotification;
  try
    State.UpdateState(LEvent);
    Notify(LEvent);
  finally
    LEvent.EndNotification;
  end;
end;

function TObjectType.GetMemberList: IMemberList;
begin
  Result := FMemberList;
end;

procedure TValueType.NotifySubject(const Notification: IObjectEvent);
begin
  Subject.Notify(Notification);
end;

procedure TValueType.DetachAll;
begin
  Subject.DetachAll;
end;

function TValueType.GetDestroying: Boolean;
begin
  Result := FDestroyng;
end;

function TValueType.GetLoading: boolean;
begin
  Result := FLoading;
end;

procedure TValueType.SetDestroying(const Value: Boolean);
begin
  FDestroyng := Value;
end;

procedure TValueType.SetLoading(const Value: boolean);
begin
  FLoading := Value;
end;

function TValueType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtValueType;
end;

{ TStringType }

procedure TStringType.Assign(const Source: IValueType);
var
  LSource: IStringType;
begin
  inherited Assign(Source);
  if Supports(Source, IStringType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TStringType.Clear;
begin
  inherited;
  FValue := EmptyStr;
end;

function TStringType.GetAsString: string;
begin
  Result := GetValue;
end;

function TStringType.GetOnGetValue: TOnGetStringValue;
begin
  Result := FOnGetValue;
end;

function TStringType.GetOnSetValue: TOnSetStringValue;
begin
  Result := FOnSetValue;
end;

function TStringType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtString;
end;

function TStringType.GetValue: string;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TStringType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadString);
end;

procedure TStringType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteString(Value);
end;

procedure TStringType.SetAsString(const Value: string);
begin
  SetValue(Value);
end;

procedure TStringType.SetOnGetValue(Value: TOnGetStringValue);
begin
  FOnGetValue := Value;
end;

procedure TStringType.SetOnSetValue(Value: TOnSetStringValue);
begin
  FOnSetValue := Value;
end;

procedure TStringType.SetValue(const Value: string);
var
  LNewValue: string;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

{ TJazzBoolean }

procedure TBooleanType.Assign(const Source: IValueType);
var
  LSource: IBooleanType;
begin
  inherited Assign(Source);
  if Supports(Source, IBooleanType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TBooleanType.Clear;
begin
  inherited;
  FValue := False;
end;

function TBooleanType.GetAsString: string;
begin
  Result := BoolToStr(Value, True);
end;

function TBooleanType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtBoolean;
end;

function TBooleanType.GetValue: boolean;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TBooleanType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadBoolean);
end;

procedure TBooleanType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteBoolean(Value);
end;

procedure TBooleanType.SetAsString(const Value: string);
var
  LValue: boolean;
begin
  if TryStrToBool(Value, LValue) then
    SetValue(LValue)
end;

procedure TBooleanType.SetValue(const Value: boolean);
var
  LNewValue: boolean;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TCurrencyType.Assign(const Source: IValueType);
var
  LSource: ICurrencyType;
begin
  inherited Assign(Source);
  if Supports(Source, ICurrencyType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TCurrencyType.Clear;
begin
  inherited;
  FValue := 0;
end;

function TCurrencyType.GetAsFloat: Double;
begin
  Result := Value;
end;

function TCurrencyType.GetAsInteger: Integer;
begin
  Result := Trunc(Value);
end;

function TCurrencyType.GetAsString: string;
begin
  Result := CurrToStrF(FValue, ffCurrency, CurrencyDecimals);
end;

function TCurrencyType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtCurrency;
end;

function TCurrencyType.GetValue: Currency;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TCurrencyType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadCurrency);
end;

procedure TCurrencyType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteCurrency(Value);
end;

procedure TCurrencyType.SetAsFloat(const Value: Double);
begin
  Self.Value := Value;
end;

procedure TCurrencyType.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TCurrencyType.SetAsString(const Value: string);
var
  LValue: Currency;
begin
  if TryStrToCurr(Value, LValue) then
    SetValue(LValue);
end;

procedure TCurrencyType.SetValue(const Value: Currency);
var
  LNewValue: Currency;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TIntegerType.Assign(const Source: IValueType);
var
  LSource: IIntegerType;
begin
  inherited Assign(Source);
  if Supports(Source, IIntegerType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TIntegerType.Clear;
begin
  inherited;
  FValue := 0;
end;

function TIntegerType.GetAsFloat: Double;
begin
  Result := Value;
end;

function TIntegerType.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TIntegerType.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TIntegerType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtInteger;
end;

function TIntegerType.GetValue: Integer;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TIntegerType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadInteger);
end;

procedure TIntegerType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteInteger(Value);
end;

procedure TIntegerType.SetAsFloat(const Value: Double);
begin
  Self.Value := Round(Value);
end;

procedure TIntegerType.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TIntegerType.SetAsString(const Value: string);
var
  LValue: Integer;
begin
  if TryStrToInt(Value, LValue) then
    SetValue(LValue);
end;

procedure TIntegerType.SetValue(const Value: Integer);
var
  LNewValue: Integer;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TFloatType.Assign(const Source: IValueType);
var
  LSource: IFloatType;
begin
  inherited Assign(Source);
  if Supports(Source, IFloatType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TFloatType.Clear;
begin
  inherited;
  FValue := 0;
end;

function TFloatType.GetAsFloat: Double;
begin
  Result := Value;
end;

function TFloatType.GetAsInteger: Integer;
begin
  Result := Trunc(FValue);
end;

function TFloatType.GetAsString: string;
begin
  Result := FloatToStr(FValue);
end;

function TFloatType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtFloat;
end;

function TFloatType.GetValue: Double;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TFloatType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadFloat);
end;

procedure TFloatType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteFloat(Value);
end;

procedure TFloatType.SetAsFloat(const Value: Double);
begin
  Self.Value := Value;
end;

procedure TFloatType.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TFloatType.SetAsString(const Value: string);
var
  LValue: Double;
begin
  if TryStrToFloat(value, LValue) then
    SetValue(LValue);
end;

procedure TFloatType.SetValue(const Value: Double);
var
  LNewValue: Double;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

{ TJazzBoolean }

procedure TDateType.Assign(const Source: IValueType);
var
  LSource: IDateType;
begin
  inherited Assign(Source);
  if Supports(Source, IDateType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

function TDateType.GetAsString: string;
begin
  if IsNull then
    Result := EmptyStr
  else
    Result := DateTimeToStr(Value);
end;

function TDateType.AsSQLTimeStamp: TSQLTimeStamp;
begin
  Result := DateTimeToSQLTimeStamp(FValue);
end;

function TDateType.GetValue: TDateTime;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TDateType.SetValue(const Value: TDateTime);
begin
  if FValue <> Value then
    SetTime(Value);
end;

function TDateType.AsTimeStamp: TTimeStamp;
begin
  Result := DateTimeToTimeStamp(FValue);
end;

procedure TBlobType.Assign(const Source: IValueType);
var
  LSource: IBlobType;
begin
  inherited Assign(Source);
  if Supports(Source, IBlobType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

function TBlobType.GetAsString: string;
begin
  SetLength(Result, FValue.Size);
  Move(FValue, PAnsiChar(@Result[1])^, FValue.Size);
end;

function TBlobType.GetValue: TStream;
begin
  if Assigned(FOnGetValue) then FOnGetValue(FValue);
  FValue.Position := 0;
  Result := FValue;
end;

procedure TBlobType.SetAsString(const Value: string);
var
  LValue: TStringStream;
begin
  LValue := TStringStream.Create(Value);
  try
    SetValue(LValue);
  finally
    LValue.Free;
  end;
end;

procedure TBlobType.SetValue(const Value: TStream);
begin
  if Value.Size <> FValue.Size then
  begin
    if Value.Size = 0 then
      Clear
    else
    begin
      Value.Position := 0;
      FValue.CopyFrom(Value, Value.Size);
    end;
    Modified;
  end;
end;

procedure TMemoType.Assign(const Source: IValueType);
var
  LSource: IMemoType;
begin
  inherited Assign(Source);
  if Supports(Source, IMemoType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

function TMemoType.GetAsString: string;
begin
  Result := FValue.Text;
end;

function TMemoType.GetValue: TStrings;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TMemoType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    FValue.Text := Reader.ReadString;
end;

procedure TMemoType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteString(AsString);
end;

procedure TMemoType.SetValue(const Value: TStrings);
begin
  if (FValue <> Value) or IsNull then
  begin
    FValue.Assign(Value);
    Modified;
  end;
end;

procedure TSmallIntType.Assign(const Source: IValueType);
var
  LSource: ISmallIntType;
begin
  inherited Assign(Source);
  if Supports(Source, ISmallIntType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TSmallIntType.Clear;
begin
  inherited;
  FValue := 0;
end;

function TSmallIntType.GetAsFloat: Double;
begin
  Result := Value;
end;

function TSmallIntType.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TSmallIntType.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TSmallIntType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtSmallInt;
end;

function TSmallIntType.GetValue: SmallInt;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TSmallIntType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadInteger);
end;

procedure TSmallIntType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteInteger(Value);
end;

procedure TSmallIntType.SetAsFloat(const Value: Double);
begin
  Self.Value := Round(Value);
end;

procedure TSmallIntType.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TSmallIntType.SetAsString(const Value: string);
var
  LValue: Integer;
begin
  if TryStrToInt(Value, LValue) then
    SetValue(LValue);
end;

procedure TSmallIntType.SetValue(const Value: SmallInt);
var
  LNewValue: SmallInt;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TLongIntType.Assign(const Source: IValueType);
var
  LSource: IIntegerType;
begin
  inherited Assign(Source);
  if Supports(Source, IIntegerType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TLongIntType.Clear;
begin
  inherited;
  FValue := 0;
end;

function TLongIntType.GetAsFloat: Double;
begin
  Result := Value;
end;

function TLongIntType.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TLongIntType.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TLongIntType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtLongInt;
end;

function TLongIntType.GetValue: LongInt;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TLongIntType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadInteger);
end;

procedure TLongIntType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteInteger(Value);
end;

procedure TLongIntType.SetAsFloat(const Value: Double);
begin
  Self.Value := Trunc(Value);
end;

procedure TLongIntType.SetAsInteger(const Value: Integer);
begin
  Self.Value := Value;
end;

procedure TLongIntType.SetAsString(const Value: string);
var
  LValue: Integer;
begin
  if TryStrToInt(Value, LValue) then
    SetValue(LValue);
end;

procedure TLongIntType.SetValue(const Value: LongInt);
var
  LNewValue: LongInt;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

{ TStringType }

procedure TWideStringType.Assign(const Source: IValueType);
var
  LSource: IStringType;
begin
  inherited Assign(Source);
  if Supports(Source, IStringType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

function TWideStringType.GetAsString: string;
begin
  Result := GetValue;
end;

function TWideStringType.GetValue: WideString;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TWideStringType.SetValue(const Value: WideString);
var
  LNewValue: WideString;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TWideStringType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadString);
end;

procedure TWideStringType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteString(Value);
end;

procedure TCharType.Assign(const Source: IValueType);
var
  LSource: ICharType;
begin
  inherited Assign(Source);
  if Supports(Source, ICharType, LSource) then
    if not LSource.IsNull then
      Value := LSource.Value;
end;

procedure TCharType.Clear;
begin
  inherited;
  FValue := CharSpace;
end;

function TCharType.GetAsString: string;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

function TCharType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtChar;
end;

function TCharType.GetValue: Char;
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(FValue);
  Result := FValue;
end;

procedure TCharType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadChar);
end;

procedure TCharType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteChar(Value);
end;

procedure TCharType.SetAsString(const Value: string);
begin
  SetValue(Value[1]);
end;

procedure TCharType.SetValue(const Value: Char);
var
  LNewValue: Char;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);

  if (FValue <> LNewValue) or IsNull then
  begin
    FValue := LNewValue;
    Modified;
  end;
end;

procedure TBlobType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetAsString(Reader.ReadString);
end;

procedure TBlobType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteString(AsString);
end;

procedure TDateType.LoadFromStream(const Reader: TReader);
begin
  inherited LoadFromStream(Reader);
  if not LoadNullFromStream(Reader) then
    SetValue(Reader.ReadDate);
end;

procedure TDateType.SaveToStream(const Writer: TWriter);
begin
  inherited SaveToStream(Writer);
  if not SaveNullToStream(Writer) then
    Writer.WriteDate(Value);
end;

function TDateType.AsTimeStampString: string;
begin
  if IsNull then
    Result := EmptyStr
  else
    Result := FormatDateTime(STimeStampFormat, Value);
end;

procedure TState.Assign(const Source: IState);
begin
  FLoaded := Source.Loaded;
  FDeleting := Source.Deleting;
  FDeleted := Source.Deleted;
  FModified := Source.Modified;
  FPersisted := Source.Persisted;
end;

constructor TState.Create(const Controller: IInterface);
begin
  inherited Create;
  FController := Pointer(Controller);
  Reset;
end;

function TState.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TState.GetDeleting: boolean;
begin
  Result := FDeleting;
end;

function TState.GetDeleted: boolean;
begin
  Result := FDeleted;
end;

function TState.GetLoaded: Boolean;
begin
  Result := FLoaded;
end;

function TState.GetModified: Boolean;
begin
  Result := FModified;
end;

function TState.GetPersisted: Boolean;
begin
  Result := FPersisted;
end;

constructor TCharType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited;
  FValue := CharSpace;
end;

{ TState }

procedure TState.LoadFromStream(const Reader: TReader);
begin
  with Reader do
  begin
    FLoaded := ReadBoolean;
    FDeleting := ReadBoolean;
    FDeleted := ReadBoolean;
    FModified := ReadBoolean;
    FPersisted := ReadBoolean;
  end
end;

procedure TState.Reset;
begin
  FLoaded := False;
  FDeleting := False;
  FDeleted := False;
  FModified := False;
  FPersisted := False;
end;

procedure TState.SaveToStream(const Writer: TWriter);
begin
  with Writer do
  begin
    WriteBoolean(FLoaded);
    WriteBoolean(FDeleting);
    WriteBoolean(FDeleted);
    WriteBoolean(FModified);
    WriteBoolean(FPersisted);
  end;
end;

procedure TState.UpdateState(const Notification: IStateChangedEvent);
var
  LObject: IValueType;
  LOwner: IMemberState;
begin
  Notification.StartNotification;
  try
    case Notification.NotifyType of
      ntLoaded:
        begin
          FLoaded := True;
          FModified := False;
        end;
      ntDeleting: FDeleting := True;
      ntCancelDeleting: FDeleting := False;
      ntDeleted:
        begin
          FDeleting := False;
          FDeleted := True;
        end;
      ntModified:
        begin
          FModified := True;
          FPersisted := False;
          LObject := GetController as IValueType;
          if not LObject.Loading and Supports(LObject.Owner, IMemberState, LOwner) then
            LOwner.State.UpdateState(Notification);
        end;
      ntPersisted:
        begin
          FLoaded := True;
          FModified := False;
          FPersisted := True;
        end;
    end
  finally
    Notification.EndNotification;
  end;
end;

procedure TMemberType.Notify(const Notification: IObjectEvent);
var
  LOwner: ISubject;
begin
  if (State.Modified or State.Deleting) and
    Supports(Owner, ISubject, LOwner) then
    LOwner.Notify(Notification);

  if CanNotifySubject then
    NotifySubject(Notification);
end;

constructor TObjectListType.Create(const AOwner: IValueType;
  const ItemClass: TClass; const Name: string);
begin
  inherited Create(AOwner, Name);
  FItemClass := ItemClass;
{$IFDEF AUTO_REGISTER_SERVICE}
  if AutoRegisterObjectList then
    AutoRegisterService(Self);
{$ENDIF}
  FItems := TInterfaceList.Create;
  FDeletingItems := TList.Create;
end;

function TObjectListType.IndexOf(const AObject: IObjectType): Integer;
var
  LObject: IObjectType;
begin
  if Supports(AObject, IObjectType, LObject) then
    Result:= FItems.IndexOf(LObject)
  else
    Result:= -1;
end;

procedure TDateType.SetAsString(const Value: string);
var
  LValue: TDateTime;
begin
  if TryStrToDateTime(Value, LValue) then
    SetValue(LValue);
end;

procedure TMemoType.SetAsString(const Value: string);
begin
  if (FValue.Text <> Value) or IsNull then
  begin
    FValue.Text := Value;
    Modified;
  end;
end;

procedure TWideStringType.SetAsString(const Value: string);
begin
  SetValue(Value);
end;

function TMemberType.DataProvider: IDataProvider;
var
  LOwner: IMemberType;
begin
  if (FDataProvider = nil) and Supports(Owner, IMemberType, LOwner) then
  begin
    if LOwner.DataProvider <> nil then
      FDataProvider := Pointer(LOwner.DataProvider);
  end;
  Result := IDataProvider(FDataProvider);
end;

procedure TMemberType.SetProvider(const Provider: IDataProvider);
begin
  FDataProvider := Pointer(Provider);
end;

function TObjectListType.GetDeletingCount: Integer;
begin
  if FDeletingItems <> nil then
    Result := FDeletingItems.Count
  else
    Result:= 0;
end;

constructor TObjectListType.Create(const ItemClass: TClass);
begin
  Create(nil, ItemClass);
end;

function TObjectListType.GetAsString: string;
begin
  Result := Format(SItemsCount, [Count]);
end;

procedure TObjectListType.SetAsString(const Value: string);
begin
  raise Exception.Create(SListCantBeString);
end;

procedure TMemoType.MemoChanged(Sender: TObject);
begin
  if not InternalLocked then
    Modified;
end;

procedure TBlobType.Clear;
begin
  Lock;
  try
    inherited;
    TStreamType(FValue).InternalClear;
  finally
    Unlock;
  end;
end;

constructor TStreamType.Create(const AOwner: IMemberType);
begin
  inherited Create;
  FOwner := Pointer(AOwner);
end;

procedure TStreamType.Clear;
begin
  inherited Clear;
  IMemberType(FOwner).Clear;
end;

function TStreamType.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  (IMemberType(FOwner) as IMemberState).Modified;
end;

procedure TDateType.Clear;
begin
  inherited;
  FValue := 0;
end;

procedure TMemoType.Clear;
begin
  inherited;
  InternalLock;
  try
    FValue.Clear;
  finally
    InternalUnlock;
  end;
end;

procedure TWideStringType.Clear;
begin
  inherited;
  FValue := EmptyStr;
end;

procedure TMemberType.LoadFromDataProvider(LoadCascade: boolean);
var
  LEvent: ILoadedEvent;
  LOwner: IObjectType;
begin
  if Supports(Owner, IObjectType, LOwner) then
  begin
    if (LOwner.DataProvider <> nil) and
      (LOwner as IMemberState).State.Loaded then
    begin
      LOwner.DataProvider.Load(Self, not LoadCascade, True);
      LEvent := NewEvent(TLoadedEvent, Self) as ILoadedEvent;
      LEvent.StartNotification;
      try
        State.UpdateState(LEvent);
        Notify(LEvent);
      finally
        LEvent.EndNotification;
      end;
    end;
  end;
end;

procedure TMemoType.InternalLock;
begin
  FLocked := True;
end;

function TMemoType.InternalLocked: boolean;
begin
  Result := FLocked;
end;

procedure TMemoType.InternalUnlock;
begin
  FLocked := False;
end;

function TMemberType.LoadNullFromStream(const Reader: TReader): boolean;
begin
  Result := Reader.ReadString = NullStr;
  if Result then
    Clear;
end;

function TMemberType.SaveNullToStream(const Writer: TWriter): boolean;
var
  LValue: string;
begin
  Result := IsNull;
  if Result then
    LValue := NullStr
  else
    LValue := EmptyStr;
  Writer.WriteString(LValue);
end;

function TMemberType.GetState: IState;
begin
  Result := FState;
end;

procedure TMemberType.Loaded(const DataProvider: IInterface);
var
  LEvent: IStateChangedEvent;
  LDataProvider: IDataProvider;
begin
  LEvent := NewEvent(TLoadedEvent, Self) as IStateChangedEvent;
  LEvent.StartNotification;
  try
    State.UpdateState(LEvent);
    Notify(LEvent);
  finally
    Loading := False;
    LEvent.EndNotification;
  end;

  if Supports(DataProvider, IDataProvider, LDataProvider) then
    SetProvider(LDataProvider);
end;

procedure TMemberType.Modified;
var
  LEvent: IStateChangedEvent;
begin
  FIsNull := False;
  if Loading then
    Exit;
  LEvent := NewEvent(TModifiedEvent, Self) as IStateChangedEvent;
  LEvent.StartNotification;
  try
    State.UpdateState(LEvent);
    Notify(LEvent);
  finally
    LEvent.EndNotification;
  end;
end;

procedure TMemberType.Persisted(const DataProvider: IInterface);
var
  LDataProvider: IDataProvider;
begin
  State.UpdateState(NewEvent(TPersistedEvent, Self) as IStateChangedEvent);
  if Supports(DataProvider, IDataProvider, LDataProvider) then
    SetProvider(LDataProvider);
end;

procedure TMemberType.LoadFromStream(const Reader: TReader);
var
  LState: IStreamable;
begin
  if Supports(State, IStreamable, LState) then
  begin
    Reader.ReadListBegin;
    LState.LoadFromStream(Reader);
    Reader.ReadListEnd;
  end;
end;

procedure TMemberType.SaveToStream(const Writer: TWriter);
var
  LState: IStreamable;
begin
  if Supports(State, IStreamable, LState) then
  begin
    Writer.WriteListBegin;
    LState.SaveToStream(Writer);
    Writer.WriteListEnd;
  end;
end;

procedure TObjectListType.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
  Notify(NewEvent(TEndUpdateEvent, Self));
end;

function TObjectListType.FindObjectIndex(const Names: array of string; Values:
  array of string): Integer;
  function CheckValues(const AObject: IObjectType): boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Low(Names) to High(Names) do
    begin
      if AObject.Member[Names[I]].AsString <> Values[I] then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  I: Integer;
begin
  Result := NotFound;
  if (SizeOf(Names) <> SizeOf(Values)) or (SizeOf(Values) = 0) then
    Exit;
  for I := 0 to Count - 1 do
  begin
    if CheckValues(Items[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TObjectListType.AddDeleting(const AObject: IObjectType);
var
  LItem: PDeletingItem;
  LObject: IObjectType;
begin
  if Supports(AObject, IObjectType, LObject) then
  begin
    System.New(LItem);
    LItem.Instance:= LObject;
    LItem.Index:= IndexOf(LObject);
    FDeletingItems.Add(LItem);
    RemoveObject(LObject);
  end;
end;

procedure TObjectListType.RemoveDeleting(const AObject: IObjectType);
var
  LIndex: Integer;
begin
  LIndex := IndexOfDeleting(AObject);
  if LIndex <> NotFound then
    RemoveDeleting(LIndex);
end;

procedure TObjectListType.RemoveDeleting(const Index: Integer);
var
  LItem: PDeletingItem;
begin
  LItem := PDeletingItem(FDeletingItems[Index]);
  Finalize(LItem^);
  Dispose(FDeletingItems[Index]);
  FDeletingItems.Delete(Index);
end;

function TObjectListType.GetDeletingItems(const Index: Integer): IObjectType;
begin
  if (Index > DeletingCount) then
    Result := nil
  else
    Result := PDeletingItem(FDeletingItems[Index]).Instance as IObjectType;
end;

function TObjectListType.GetEnumerator: IObjectListTypeEnumerator;
begin
  Result := TObjectListTypeEnumerator.Create(Self);
end;

procedure TObjectListType.CancelDeleting(const AObject: IObjectType);
begin
  BeginUpdate;
  try
    InternalCancelDeleting(AObject);
  finally
    EndUpdate;
  end;
end;

function TObjectListType.IndexOfDeleting(const AObject: IObjectType): Integer;
var
  I: Integer;
  LItem: PDeletingItem;
  LObject: IObjectType;
begin
  Result:= -1;
  if Supports(AObject, IObjectType, LObject) then
  begin
    Result := -1;
    for I := 0 to FDeletingItems.Count - 1 do
    begin
      LItem := PDeletingItem(FDeletingItems[I]);
      if LItem.Instance = LObject then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

destructor TObjectListType.Destroy;
begin
  Destroying := True;
  Clear;
  FreeAndNil(FDeletingItems);
  inherited;
end;

procedure TObjectType.Notify(const Notification: IObjectEvent);
var
  LOwner: ISubject;
begin
  if Supports(Owner, ISubject, LOwner) then LOwner.Notify(Notification);
  if CanNotifySubject then NotifySubject(Notification);
end;

procedure TObjectType.Deleted;
var
  LEvent: IStateChangedEvent;
begin
  LEvent := NewEvent(TDeletedEvent, Self) as IStateChangedEvent;
  LEvent.StartNotification;
  try
    State.UpdateState(LEvent);
    Notify(LEvent);
  finally
    LEvent.EndNotification;
  end;
end;

procedure TStreamType.InternalClear;
begin
  inherited Clear;
end;

{ TValueTypeFactory }

function TValueTypeFactory.NewBlob(const Value: TStream;
  const Owner: IInterface): IBlobType;
begin
  Result := NewValueType(TBlobType, Owner) as IBlobType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewBoolean(const Value: boolean;
  const Owner: IInterface): IBooleanType;
begin
  Result := NewValueType(TBooleanType, Owner) as IBooleanType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewChar(const Value: Char;
  const Owner: IInterface): ICharType;
begin
  Result := NewValueType(TCharType, Owner) as ICharType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewCurrency(const Value: Currency;
  const Owner: IInterface): ICurrencyType;
begin
  Result := NewValueType(TCurrencyType, Owner) as ICurrencyType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewDate(const Value: TDateTime;
  const Owner: IInterface): IDateType;
begin
  Result := NewValueType(TDateType, Owner) as IDateType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewFloat(const Value: Double;
  const Owner: IInterface): IFloatType;
begin
  Result := NewValueType(TFloatType, Owner) as IFloatType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewInteger(const Value: Integer;
  const Owner: IInterface): IIntegerType;
begin
  Result := NewValueType(TIntegerType, Owner) as IIntegerType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewLongInt(const Value: LongInt;
  const Owner: IInterface): ILongIntType;
begin
  Result := NewValueType(TLongIntType, Owner) as ILongIntType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewMemo(const Value: TStrings;
  const Owner: IInterface): IMemoType;
begin
  Result := NewValueType(TMemoType, Owner) as IMemoType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewMemo(const Value: string;
  const Owner: IInterface): IMemoType;
begin
  Result := NewValueType(TMemoType, Owner) as IMemoType;
  Result.Value.Text := Value;
end;

function TValueTypeFactory.NewSmallInt(const Value: SmallInt;
  const Owner: IInterface): ISmallIntType;
begin
  Result := NewValueType(TSmallIntType, Owner) as ISmallIntType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewString(const Value: string;
  const Owner: IInterface): IStringType;
begin
  Result := NewValueType(TStringType, Owner) as IStringType;
  Result.Value := Value;
end;

function TValueTypeFactory.NewValueType(const TypeClass: TMemberTypeClass;
  const Owner: IInterface): IValueType;
begin
  Result := TypeClass.Create(Owner as IValueType);
end;

function TValueTypeFactory.NewWideString(const Value: WideString;
  const Owner: IInterface): IWideStringType;
begin
  Result := NewValueType(TWideStringType, Owner) as IWideStringType;
  Result.Value := Value;
end;

{ TStateChangedEvent }

function TStateChangedEvent.GetNotifyType: TNotifyType;
begin
  Result := ntStateChanged;
end;

{ TLoadedEvent }

function TLoadedEvent.GetNotifyType: TNotifyType;
begin
  Result := ntLoaded;
end;

{ TDeletingEvent }

function TDeletingEvent.GetNotifyType: TNotifyType;
begin
  Result := ntDeleting;
end;

{ TDeleteEvent }

function TDeleteEvent.GetNotifyType: TNotifyType;
begin
  Result := ntDeleting;
end;

{ TCancelDeletingEvent }

function TCancelDeletingEvent.GetNotifyType: TNotifyType;
begin
  Result := ntCancelDeleting;
end;

{ TDeletedEvent }

function TDeletedEvent.GetNotifyType: TNotifyType;
begin
  Result := ntDeleted;
end;

{ TModifiedEvent }

function TModifiedEvent.GetNotifyType: TNotifyType;
begin
  Result := ntModified;
end;

{ TPersistedEvent }

function TPersistedEvent.GetNotifyType: TNotifyType;
begin
  Result := ntPersisted;
end;

function TBlobType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtBlob;
end;

function TDateType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtDate;
end;

function TMemoType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtMemo;
end;

function TWideStringType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtWideString;
end;

function TObjectType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtObject;
end;

function TObjectListType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtObjectList;
end;

function TObjectListType.New: IObjectType;
begin
  Result := TObjectTypeClass(ItemClass).Create(Self);
end;

constructor TBlobType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited;
  FValue := TStreamType.Create(Self);
end;

destructor TBlobType.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

constructor TCurrencyType.Create(const AOwner: IValueType;
  const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TDateType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TFloatType.Create(const AOwner: IValueType;
  const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TIntegerType.Create(const AOwner: IValueType;
  const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TSmallIntType.Create(const AOwner: IValueType;
  const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TLongIntType.Create(const AOwner: IValueType;
  const Name: string);
begin
  inherited;
  FValue := 0;
end;

constructor TMemoType.Create(const AOwner: IValueType; const Name: string);
begin
  inherited;
  FLocked := False;
  FValue := TStringList.Create;
  TStringList(FValue).OnChange := MemoChanged;
end;

destructor TMemoType.Destroy;
begin
  TStringList(FValue).OnChange := nil;
  FreeAndNil(FValue);
  inherited;
end;

procedure TObjectType.InitInstance;
begin

end;

function TObjectType.GetMembersCount: Integer;
begin
  Result := MemberList.Count;
end;

function TBlobType.GetOnGetValue: TOnGetBlobValue;
begin
  Result := FOnGetValue;
end;

function TBlobType.GetOnSetValue: TOnSetBlobValue;
begin
  Result := FOnSetValue;
end;

procedure TBlobType.SetOnGetValue(Value: TOnGetBlobValue);
begin
  FOnGetValue := Value;
end;

procedure TBlobType.SetOnSetValue(Value: TOnSetBlobValue);
begin
  FOnSetValue := Value;
end;

function TBooleanType.GetOnGetValue: TOnGetBooleanValue;
begin
  Result := FOnGetValue;
end;

function TBooleanType.GetOnSetValue: TOnSetBooleanValue;
begin
  Result := FOnSetValue;
end;

procedure TBooleanType.SetOnGetValue(Value: TOnGetBooleanValue);
begin
  FOnGetValue := Value;
end;

procedure TBooleanType.SetOnSetValue(Value: TOnSetBooleanValue);
begin
  FOnSetValue := Value;
end;

function TCharType.GetOnGetValue: TOnGetCharValue;
begin
  Result := FOnGetValue;
end;

function TCharType.GetOnSetValue: TOnSetCharValue;
begin
  Result := FOnSetValue;
end;

procedure TCharType.SetOnGetValue(Value: TOnGetCharValue);
begin
  FOnGetValue := Value;
end;

procedure TCharType.SetOnSetValue(Value: TOnSetCharValue);
begin
  FOnSetValue := Value;
end;

function TCurrencyType.GetOnGetValue: TOnGetCurrencyValue;
begin
  Result := FOnGetValue;
end;

function TCurrencyType.GetOnSetValue: TOnSetCurrencyValue;
begin
  Result := FOnSetValue;
end;

procedure TCurrencyType.SetOnGetValue(Value: TOnGetCurrencyValue);
begin
  FOnGetValue := Value;
end;

procedure TCurrencyType.SetOnSetValue(Value: TOnSetCurrencyValue);
begin
  FOnSetValue := Value;
end;

function TDateType.GetOnGetValue: TOnGetDateValue;
begin
  Result := FOnGetValue;
end;

function TDateType.GetOnSetValue: TOnSetDateValue;
begin
  Result := FOnSetValue;
end;

procedure TDateType.SetOnGetValue(Value: TOnGetDateValue);
begin
  FOnGetValue := Value;
end;

procedure TDateType.SetOnSetValue(Value: TOnSetDateValue);
begin
  FOnSetValue := Value;
end;

procedure TDateType.SetTime(const Value: TDateTime);
var
  LNewValue: TDateTime;
begin
  LNewValue := Value;
  if Assigned(FOnSetValue) then
    FOnSetValue(FValue, LNewValue);
  FValue := LNewValue;
  Modified;
end;

procedure TDateType.SetTimeStamp(const Value: TTimeStamp);
begin
  SetTime(TimeStampToDateTime(Value));
end;

function TFloatType.GetOnGetValue: TOnGetFloatValue;
begin
  Result := FOnGetValue;
end;

function TFloatType.GetOnSetValue: TOnSetFloatValue;
begin
  Result := FOnSetValue;
end;

procedure TFloatType.SetOnGetValue(Value: TOnGetFloatValue);
begin
  FOnGetValue := Value;
end;

procedure TFloatType.SetOnSetValue(Value: TOnSetFloatValue);
begin
  FOnSetValue := Value;
end;

function TIntegerType.GetOnGetValue: TOnGetIntegerValue;
begin
  Result := FOnGetValue;
end;

function TIntegerType.GetOnSetValue: TOnSetIntegerValue;
begin
  Result := FOnSetValue;
end;

procedure TIntegerType.SetOnGetValue(Value: TOnGetIntegerValue);
begin
  FOnGetValue := Value;
end;

procedure TIntegerType.SetOnSetValue(Value: TOnSetIntegerValue);
begin
  FOnSetValue := Value;
end;

function TLongIntType.GetOnGetValue: TOnGetLongIntValue;
begin
  Result := FOnGetValue;
end;

function TLongIntType.GetOnSetValue: TOnSetLongIntValue;
begin
  Result := FOnSetValue;
end;

procedure TLongIntType.SetOnGetValue(Value: TOnGetLongIntValue);
begin
  FOnGetValue := Value;
end;

procedure TLongIntType.SetOnSetValue(Value: TOnSetLongIntValue);
begin
  FOnSetValue := Value;
end;

function TMemoType.GetOnGetValue: TOnGetMemoValue;
begin
  Result := FOnGetValue;
end;

function TMemoType.GetOnSetValue: TOnSetMemoValue;
begin
  Result := FOnSetValue;
end;

procedure TMemoType.SetOnGetValue(Value: TOnGetMemoValue);
begin
  FOnGetValue := Value;
end;

procedure TMemoType.SetOnSetValue(Value: TOnSetMemoValue);
begin
  FOnSetValue := Value;
end;

function TSmallIntType.GetOnGetValue: TOnGetSmallIntValue;
begin
  Result := FOnGetValue;
end;

function TSmallIntType.GetOnSetValue: TOnSetSmallIntValue;
begin
  Result := FOnSetValue;
end;

procedure TSmallIntType.SetOnGetValue(Value: TOnGetSmallIntValue);
begin
  FOnGetValue := Value;
end;

procedure TSmallIntType.SetOnSetValue(Value: TOnSetSmallIntValue);
begin
  FOnSetValue := Value;
end;

function TWideStringType.GetOnGetValue: TOnGetWideStringValue;
begin
  Result := FOnGetValue;
end;

function TWideStringType.GetOnSetValue: TOnSetWideStringValue;
begin
  Result := FOnSetValue;
end;

procedure TWideStringType.SetOnGetValue(Value: TOnGetWideStringValue);
begin
  FOnGetValue := Value;
end;

procedure TWideStringType.SetOnSetValue(Value: TOnSetWideStringValue);
begin
  FOnSetValue := Value;
end;

procedure TBlobType.SaveToStream(Stream: TStream);
begin
  if Value.Size > 0 then
  begin
    Value.Position := 0;
    Stream.CopyFrom(Value, Value.Size);
  end;
end;

{ TNumericType }

function TNumericType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtNumeric;
end;

{ TTimeType }

function TTimeType.GetValueTypeKind: TValueTypeKind;
begin
  Result := vtTime;
end;

procedure TTimeType.SetValue(const Value: TDateTime);
begin
  SetTime(Value);
end;

{ TObjectListTypeEnumerator }

constructor TObjectListTypeEnumerator.Create(AList: IObjectListType);
begin
  FIndex := -1;
  FList := AList;
end;

function TObjectListTypeEnumerator.GetCurrent: IObjectType;
begin
  Result := FList[FIndex];
end;

function TObjectListTypeEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ EJazzMemberNotFound }

function EJazzMemberNotFound.GetDefaultMessage: string;
begin
  Result:= SMemberNotFound;
end;

{ EJazzObjectClassNotDefined }

function EJazzObjectClassNotDefined.GetDefaultMessage: string;
begin
  Result:= SObjectClassNotDefined;
end;

{ EJazzInsert }

function EJazzInsert.GetDefaultMessage: string;
begin
  Result := SItemCannotBeInserted;
end;

initialization
  RegisterType(IObjectType, 'Object', TObjectType);
  RegisterType(IObjectListType, 'ObjectList', TObjectListType);
  RegisterType(IBlobType, 'Blob', TBlobType);
  RegisterType(IBooleanType, 'Boolean', TBooleanType);
  RegisterType(ICharType, 'Char', TCharType);
  RegisterType(ICurrencyType, 'Currency', TCurrencyType);
  RegisterType(IFloatType, 'Float', TFloatType);
  RegisterType(IIntegerType, 'Integer', TIntegerType);
  RegisterType(ILongIntType, 'LongInt', TLongIntType);
  RegisterType(ISmallIntType, 'SmallInt', TSmallIntType);
  RegisterType(IStringType, 'String', TStringType);
  RegisterType(IDateType, 'Date', TDateType);
  RegisterType(ITimeType, 'Time', TTimeType);
  RegisterType(IMemoType, 'Memo', TMemoType);
  RegisterType(IWideStringType, 'WideString', TWideStringType);

end.

