unit BOAddressIntf;

interface

uses JazzValueTypeIntf;

type
  IAddress = interface(IObjectType)
    ['{F2BCC842-B9AA-4E30-85F5-0F4F434C509D}']
    function GetAddress1: string;
    function GetAddress2: string;
    function GetAddressState: string;
    function GetCity: string;
    function GetCountry: string;
    function GetID: string;
    function GetIDOwner: string;
    function GetPostalCode: string;
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: string);
    procedure SetAddressState(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIDOwner(const Value: string);
    procedure SetPostalCode(const Value: string);

    property ID: string read GetID write SetID;
    property IDOwner: string read GetIDOwner write SetIDOwner;
    property Address1: string read GetAddress1 write SetAddress1;
    property Address2: string read GetAddress2 write SetAddress2;
    property City: string read GetCity write SetCity;
    property AddressState: string read GetAddressState write SetAddressState;
    property PostalCode: string read GetPostalCode write SetPostalCode;
    property Country: string read GetCountry write SetCountry;
  end;
  
implementation

end.

