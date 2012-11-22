unit JazzValueTypeIntf;

interface

uses
  Classes,
  SqlTimSt,
  SysUtils,
  JazzClasses,
  JazzNotifyIntf,
  JazzSubjectIntf,
  JazzTypes;

type
  IValueType = interface;
  IMemberType = interface;
  IObjectListType = interface;

  IDataProvider = interface(IInterface)
    ['{581DEB88-29B5-4546-B992-BE1660841643}']
    function Load(const Member: IMemberType; const LoadAllMembers: boolean = True; const AutoLoad: boolean = False): boolean;
  end;

  IStateChangedEvent = interface(IObjectEvent)
    ['{8B9E7F99-9C6A-4E8C-9212-6ECC260EDC98}']
  end;

  ILoadedEvent = interface(IStateChangedEvent)
    ['{9765D85F-6E78-49E4-ABC1-AA7461B60CE3}']
  end;

  IDeletingEvent = interface(IStateChangedEvent)
    ['{082E2DC5-3FB4-45F7-A411-EDF41F2D205A}']
  end;

  IDeleteEvent = IDeletingEvent;

  ICancelDeletingEvent = interface(IStateChangedEvent)
    ['{2CEA319D-E640-4630-9836-2FD5C2CE39FF}']
  end;

  IDeletedEvent = interface(IStateChangedEvent)
    ['{EC53E8B6-7B53-43D7-BBB2-2AB0FD3E998B}']
  end;

  IModifiedEvent = interface(IStateChangedEvent)
    ['{109CD7EC-F656-4AF3-8B74-9A81CDFFD85A}']
  end;

  IPersistedEvent = interface(IStateChangedEvent)
    ['{D025A078-D34A-43D4-91C5-B1A1CADF6E58}']
  end;

  IState = interface;
  IMemberState = interface(IInterface)
    ['{6C1776F9-0239-4D0F-AAC7-617E323C8177}']
    function GetState: IState;
    procedure Modified;
    property State: IState read GetState;
  end;

  IPersisterState = interface(IMemberState)
    ['{C15365E7-8FD5-4B32-BA25-19BFF491BBF6}']
    procedure Loaded(const DataProvider: IInterface = nil);
    procedure Persisted(const DataProvider: IInterface = nil);
  end;

  IObjectState = interface(IPersisterState)
    ['{F56DECB4-A8AC-444A-9DB2-1D16BBF68BC1}']
    procedure Delete;
    procedure Deleted;
    procedure CancelDelete;
  end;

  IObjectListState = interface(IPersisterState)
    ['{99D65F10-6E60-4B4D-905E-DA2800E172D3}']
  end;

  IState = interface(IInterface)
    ['{60042746-3305-46E7-9EA3-AC1E5E8B50B5}']
    function GetLoaded: boolean;
    function GetDeleting: boolean;
    function GetDeleted: boolean;
    function GetModified: boolean;
    function GetPersisted: boolean;

    procedure Assign(const Source: IState);
    procedure UpdateState(const Notification: IStateChangedEvent);
    procedure Reset;
    
    property Loaded: boolean read GetLoaded;       // Loaded from persister
    property Deleting: boolean read GetDeleting;   // Marked to delete
    property Deleted: boolean read GetDeleted;     // Deleted from persister
    property Modified: boolean read GetModified;   // Member or Object Changed
    property Persisted: boolean read GetPersisted; // Saved to persister
  end;

  TValueTypeKind = (
    vtValueType,
    vtObject,
    vtObjectList,

    vtBlob,
    vtBoolean,
    vtChar,
    vtCurrency,
    vtDate,
    vtFloat,
    vtInteger,
    vtLongInt,
    vtMemo,
    vtNumeric,
    vtSmallInt,
    vtString,
    vtTime,
    vtWideString
  );

  IValueType = interface(ICustomObject)
    ['{24A7139B-A70B-4974-9137-75A32FCDC2F7}']
    function Clone: IValueType;
    function GetDestroying: Boolean;
    function GetLoading: boolean;
    function GetSubject: ISubject;
    function GetValueTypeKind: TValueTypeKind;
    procedure Assign(const Source: IValueType);
    procedure NotifySubject(const Notification: IObjectEvent);
    procedure SetDestroying(const Value: Boolean);
    procedure SetLoading(const Value: boolean);
    property Destroying: Boolean read GetDestroying write SetDestroying;
    property Loading: boolean read GetLoading write SetLoading;
    property Subject: ISubject read GetSubject;
    property ValueTypeKind: TValueTypeKind read GetValueTypeKind;
  end;

  IMemberStreamable = interface(IStreamable)
    ['{45631370-9D50-42AA-BD98-9562180F6D4B}']
    function LoadNullFromStream(const Reader: TReader): boolean;
    function SaveNullToStream(const Writer: TWriter): boolean; 
  end;

  IMemberType = interface(IValueType)
    ['{FB6FC339-3C7F-46ED-9754-11AF6DFB90D3}']
    function GetAsString: string; 
    function GetName: string;
    procedure SetAsString(const Value: string);

    function DataProvider: IDataProvider;
    function IsNull: boolean;
    procedure Clear;
    procedure LoadFromDataProvider(LoadCascade: boolean = False);
    procedure SetProvider(const Provider: IDataProvider);
    property AsString: string read GetAsString write SetAsString;
    property Name: string read GetName;
  end;

  IMemberList = INamedInterfaceList;

  IObjectType = interface(IMemberType)
    ['{827094A3-8B53-469D-8046-048EC1A8C766}']
    function GetMember(const Name: string): IMemberType;
    function GetMembers(const Index: Integer): IMemberType;
    function GetMembersCount: Integer;
    function GetMemberList: IMemberList;

    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; IID: TGUID); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; const ItemClass: TClass); overload;
    procedure AddMember(out Instance; const Name: string; const MemberClass: TClass; const ItemClass: TClass; IID: TGUID); overload;
    procedure InitInstance;

    procedure Delete;
    procedure CancelDelete;

    property Member[const Name: string]: IMemberType read GetMember;
    property Members[const Index: Integer]: IMemberType read GetMembers; default;
    property MembersCount: Integer read GetMembersCount;
    property MemberList: IMemberList read GetMemberList;
  end;

  IObjectListTypeEnumerator = interface
  ['{9F8FE829-D7DC-4767-AF30-C56D50E25508}']
    function GetCurrent: IObjectType;
    function MoveNext: Boolean;
    property Current: IObjectType read GetCurrent;
  end;

  IObjectListType = interface(IMemberType)
    ['{65842477-8515-4B26-93C4-6516C54447E5}']
    // Deleting
    function GetDeletingCount: Integer;
    function GetDeletingItems(const Index: Integer): IObjectType;
    function IndexOfDeleting(const AObject: IObjectType): Integer;
    procedure AddDeleting(const AObject: IObjectType);
    procedure CancelDeleting(const AObject: IObjectType = nil);
    procedure RemoveDeleting(const AObject: IObjectType); overload;
    procedure RemoveDeleting(const Index: Integer); overload;
    property DeletingCount: Integer read GetDeletingCount;
    property DeletingItems[const Index: Integer]: IObjectType read GetDeletingItems;

    // Items
    function Add(const Item: IObjectType): Integer; overload;
    function Add: IObjectType; overload;
    function New: IObjectType;
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Item: IInterface);
    procedure Remove(Index: Integer);
    procedure RemoveObject(const AObject: IObjectType);

    function FindObject(const Names: array of string; Values: array of string): IObjectType;
    function FindObjectIndex(const Names: array of string; Values: array of string): Integer;
    function GetCount: Integer;
    function GetItems(const Index: Integer): IObjectType;
    function IndexOf(const AObject: IObjectType): Integer;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IObjectType read GetItems; default;

    function IsUpdating: boolean;
    procedure BeginUpdate;
    procedure EndUpdate(const CallNotify: boolean = True);
    function GetEnumerator: IObjectListTypeEnumerator;
    function GetItemClass: TClass;
    property ItemClass: TClass read GetItemClass;
  end;

  TOnGetBlobValue = procedure(var Value: TStream) of Object;
  TOnSetBlobValue = procedure(OldValue: TStream; var NewValue: TStream) of Object;

  TOnGetBooleanValue = procedure(var Value: Boolean) of Object;
  TOnSetBooleanValue = procedure(OldValue: Boolean; var NewValue: Boolean) of Object;

  TOnGetCharValue = procedure(var Value: Char) of Object;
  TOnSetCharValue = procedure(OldValue: Char; var NewValue: Char) of Object;

  TOnGetDateValue = procedure(var Value: TDateTime) of Object;
  TOnSetDateValue = procedure(OldValue: TDateTime; var NewValue: TDateTime) of Object;

  TOnGetMemoValue = procedure(var Value: TStrings) of Object;
  TOnSetMemoValue = procedure(OldValue: TStrings; var NewValue: TStrings) of Object;

  TOnGetCurrencyValue = procedure(var Value: Currency) of Object;
  TOnSetCurrencyValue = procedure(OldValue: Currency; var NewValue: Currency) of Object;

  TOnGetFloatValue = procedure(var Value: Double) of Object;
  TOnSetFloatValue = procedure(OldValue: Double; var NewValue: Double) of Object;

  TOnGetIntegerValue = procedure(var Value: Integer) of Object;
  TOnSetIntegerValue = procedure(OldValue: Integer; var NewValue: Integer) of Object;

  TOnGetLongIntValue = procedure(var Value: LongInt) of Object;
  TOnSetLongIntValue = procedure(OldValue: LongInt; var NewValue: LongInt) of Object;

  TOnGetSmallIntValue = procedure(var Value: SmallInt) of Object;
  TOnSetSmallIntValue = procedure(OldValue: SmallInt; var NewValue: SmallInt) of Object;

  TOnGetStringValue = procedure(var Value: string) of Object;
  TOnSetStringValue = procedure(OldValue: string; var NewValue: string) of Object;

  TOnGetWideStringValue = procedure(var Value: WideString) of Object;
  TOnSetWideStringValue = procedure(OldValue: WideString; var NewValue: WideString) of Object;

  IBlobType = interface(IMemberType)
    ['{D7C2EECC-3920-4AF9-972C-1E9E7AEB3A84}']
    function GetOnGetValue: TOnGetBlobValue;
    function GetOnSetValue: TOnSetBlobValue;
    procedure SetOnGetValue(Value: TOnGetBlobValue);
    procedure SetOnSetValue(Value: TOnSetBlobValue);
    procedure SaveToStream(Stream: TStream); overload;

    function GetValue: TStream;
    procedure SetAsString(const Value: string);
    procedure SetValue(const Value: TStream);
    property Value: TStream read GetValue write SetValue;
    property OnGetValue: TOnGetBlobValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetBlobValue read GetOnSetValue write SetOnSetValue;
  end;

  IBooleanType = interface(IMemberType)
    ['{EA9BF744-927D-44DA-81B0-AFDF853E8FFD}']
    function GetOnGetValue: TOnGetBooleanValue;
    function GetOnSetValue: TOnSetBooleanValue;
    procedure SetOnGetValue(Value: TOnGetBooleanValue);
    procedure SetOnSetValue(Value: TOnSetBooleanValue);
    function GetValue: boolean;
    procedure SetValue(const Value: boolean);
    property Value: boolean read GetValue write SetValue;
    property OnGetValue: TOnGetBooleanValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetBooleanValue read GetOnSetValue write SetOnSetValue;
  end;

  ICharType = interface(IMemberType)
    ['{EA9BF744-927D-44DA-81B0-AFDF853E8FFD}']
    function GetOnGetValue: TOnGetCharValue;
    function GetOnSetValue: TOnSetCharValue;
    procedure SetOnGetValue(Value: TOnGetCharValue);
    procedure SetOnSetValue(Value: TOnSetCharValue);
    function GetValue: Char;
    procedure SetValue(const Value: Char);
    property Value: Char read GetValue write SetValue;
    property OnGetValue: TOnGetCharValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetCharValue read GetOnSetValue write SetOnSetValue;
  end;

  IDateType = interface(IMemberType)
    ['{009A3502-D2A2-4BEF-B6FC-2660AB9AB17C}']
    function AsSQLTimeStamp: TSQLTimeStamp;
    function AsTimeStamp: TTimeStamp;
    function AsTimeStampString: string;
    function GetOnGetValue: TOnGetDateValue;
    function GetOnSetValue: TOnSetDateValue;
    function GetValue: TDateTime;
    procedure SetOnGetValue(Value: TOnGetDateValue);
    procedure SetOnSetValue(Value: TOnSetDateValue);
    procedure SetTime(const Value: TDateTime);
    procedure SetTimeStamp(const Value: TTimeStamp);
    procedure SetValue(const Value: TDateTime);

    property Value: TDateTime read GetValue write SetValue;
    property OnGetValue: TOnGetDateValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetDateValue read GetOnSetValue write SetOnSetValue;
  end;

  ITimeType = interface(IDateType)
    ['{AF4424E9-4F9D-44AD-84A3-7E7B84CC0F93}'] 
  end;

  INumericType = interface(IMemberType)
    ['{5F6687CF-0EB4-40B0-A913-650A4BCC3D71}']
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Integer);

    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  ICurrencyType = interface(INumericType)
    ['{C2B913ED-B39E-4A3C-97E0-0A99EC856C3F}']
    function GetOnGetValue: TOnGetCurrencyValue;
    function GetOnSetValue: TOnSetCurrencyValue;
    procedure SetOnGetValue(Value: TOnGetCurrencyValue);
    procedure SetOnSetValue(Value: TOnSetCurrencyValue);
    function GetValue: Currency;
    procedure SetValue(const Value: Currency);
    property Value: Currency read GetValue write SetValue;
    property OnGetValue: TOnGetCurrencyValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetCurrencyValue read GetOnSetValue write SetOnSetValue;
  end;

  IFloatType = interface(INumericType)
    ['{AF498BD9-EDEF-4B41-8AEF-D3607DFAECF1}']
    function GetOnGetValue: TOnGetFloatValue;
    function GetOnSetValue: TOnSetFloatValue;
    procedure SetOnGetValue(Value: TOnGetFloatValue);
    procedure SetOnSetValue(Value: TOnSetFloatValue);
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    property Value: Double read GetValue write SetValue;
    property OnGetValue: TOnGetFloatValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetFloatValue read GetOnSetValue write SetOnSetValue;
  end;

  IIntegerType = interface(INumericType)
    ['{337B8BCA-C0EB-4E09-883A-C98A1CED064F}']
    function GetOnGetValue: TOnGetIntegerValue;
    function GetOnSetValue: TOnSetIntegerValue;
    procedure SetOnGetValue(Value: TOnGetIntegerValue);
    procedure SetOnSetValue(Value: TOnSetIntegerValue);
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    property Value: Integer read GetValue write SetValue;
    property OnGetValue: TOnGetIntegerValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetIntegerValue read GetOnSetValue write SetOnSetValue;
  end;

  ILongIntType = interface(INumericType)
    ['{337B8BCA-C0EB-4E09-883A-C98A1CED064F}']
    function GetOnGetValue: TOnGetLongIntValue;
    function GetOnSetValue: TOnSetLongIntValue;
    procedure SetOnGetValue(Value: TOnGetLongIntValue);
    procedure SetOnSetValue(Value: TOnSetLongIntValue);
    function GetValue: LongInt;
    procedure SetValue(const Value: LongInt);
    property Value: LongInt read GetValue write SetValue;
    property OnGetValue: TOnGetLongIntValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetLongIntValue read GetOnSetValue write SetOnSetValue;
  end;

  IMemoType = interface(IMemberType)
    ['{5CB0DD2B-C4C9-4C13-8009-A14D2018046F}']
    function GetOnGetValue: TOnGetMemoValue;
    function GetOnSetValue: TOnSetMemoValue;
    procedure SetOnGetValue(Value: TOnGetMemoValue);
    procedure SetOnSetValue(Value: TOnSetMemoValue);
    function GetValue: TStrings;
    procedure SetValue(const Value: TStrings);
    procedure MemoChanged(Sender: TObject);
    property Value: TStrings read GetValue write SetValue;
    property OnGetValue: TOnGetMemoValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetMemoValue read GetOnSetValue write SetOnSetValue;
  end;

  ISmallIntType = interface(INumericType)
    ['{7597DB5D-AB71-4EF4-BA39-55C4BEB0C140}']
    function GetOnGetValue: TOnGetSmallIntValue;
    function GetOnSetValue: TOnSetSmallIntValue;
    procedure SetOnGetValue(Value: TOnGetSmallIntValue);
    procedure SetOnSetValue(Value: TOnSetSmallIntValue);
    function GetValue: SmallInt;
    procedure SetValue(const Value: SmallInt);
    property Value: SmallInt read GetValue write SetValue;
    property OnGetValue: TOnGetSmallIntValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetSmallIntValue read GetOnSetValue write SetOnSetValue;
  end;

  IStringType = interface(IMemberType)
    ['{8E57F685-7EF1-4C3E-A8E0-28F7E2E045D1}']
    function GetOnGetValue: TOnGetStringValue;
    function GetOnSetValue: TOnSetStringValue;
    procedure SetOnGetValue(Value: TOnGetStringValue);
    procedure SetOnSetValue(Value: TOnSetStringValue);
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Value: string read GetValue write SetValue;
    property OnGetValue: TOnGetStringValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetStringValue read GetOnSetValue write SetOnSetValue;
  end;

  IWideStringType = interface(IMemberType)
    ['{99DBA182-A2D1-4675-919C-50830B53FB17}']
    function GetOnGetValue: TOnGetWideStringValue;
    function GetOnSetValue: TOnSetWideStringValue;
    procedure SetOnGetValue(Value: TOnGetWideStringValue);
    procedure SetOnSetValue(Value: TOnSetWideStringValue);

    function GetValue: WideString;
    procedure SetValue(const Value: WideString);
    property Value: WideString read GetValue write SetValue;
    property OnGetValue: TOnGetWideStringValue read GetOnGetValue write SetOnGetValue;
    property OnSetValue: TOnSetWideStringValue read GetOnSetValue write SetOnSetValue;
  end;

  IValueTypeFactory = interface(IInterface)
    ['{E690CD36-AD13-4A4F-B194-6DB91B59CBBC}']
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

implementation

end.



