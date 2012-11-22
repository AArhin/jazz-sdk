unit ValueTypeTestObjectIntf;

interface

uses JazzValueTypeIntf, OneToOneObjectIntf, classes, Graphics;

type
  IValueTypeTestObject = interface(IObjectType)
  ['{6DF045E5-D10C-4F42-84F1-F2741B0AD9F9}']
    function GetBlobMember: TStream;
    function GetBooleanMember: Boolean;
    function GetCharMember: Char;
    function GetCurrencyMember: Currency;
    function GetDateMember: TDateTime;
    function GetFloatMember: Double;
    function GetIntegerMember: Integer;
    function GetLongIntMember: LongInt;
    function GetMemoMember: TStrings;
    function GetOneToManyMember: IObjectListType;
    function GetOneToOneMember: IOneToOneObject;
    function GetSmallintMember: SmallInt;
    function GetStringMember: string;
    function GetTimeMember: TDateTime;
    function GetWideStringMember: string;
    procedure SetBlobMember(const Value: TStream);
    procedure SetBooleanMember(const Value: Boolean);
    procedure SetCharMember(const Value: Char);
    procedure SetCurrencyMember(const Value: Currency);
    procedure SetDateMember(const Value: TDateTime);
    procedure SetFloatMember(const Value: Double);
    procedure SetIntegerMember(const Value: Integer);
    procedure SetLongIntMember(const Value: LongInt);
    procedure SetMemoMember(const Value: TStrings);
    procedure SetOneToManyMember(const Value: IObjectListType);
    procedure SetOneToOneMember(const Value: IOneToOneObject);
    procedure SetSmallintMember(const Value: SmallInt);
    procedure SetStringMember(const Value: string);
    procedure SetTimeMember(const Value: TDateTime);
    procedure SetWideStringMember(const Value: string);

    property BlobMember: TStream read GetBlobMember write SetBlobMember;
    property BooleanMember: Boolean read GetBooleanMember write SetBooleanMember;
    property CharMember: Char read GetCharMember write SetCharMember;
    property CurrencyMember: Currency read GetCurrencyMember write SetCurrencyMember;
    property DateMember: TDateTime read GetDateMember write SetDateMember;
    property FloatMember: Double read GetFloatMember write SetFloatMember;
    property IntegerMember: Integer read GetIntegerMember write SetIntegerMember;
    property LongIntMember: LongInt read GetLongIntMember write SetLongIntMember;
    property MemoMember: TStrings read GetMemoMember write SetMemoMember;
    property OneToManyMember: IObjectListType read GetOneToManyMember write SetOneToManyMember;
    property OneToOneMember: IOneToOneObject read GetOneToOneMember write SetOneToOneMember;
    property SmallintMember: SmallInt read GetSmallintMember write SetSmallintMember;
    property StringMember: string read GetStringMember write SetStringMember;
    property TimeMember: TDateTime read GetTimeMember write SetTimeMember;
    property WideStringMember: string read GetWideStringMember write SetWideStringMember;
  end;

implementation

end.

