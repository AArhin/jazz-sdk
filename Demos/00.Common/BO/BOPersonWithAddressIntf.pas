unit BOPersonWithAddressIntf;

interface

uses JazzValueTypeIntf, BOPersonIntf;

type
  IPersonWithAddress = interface(IPerson)
    ['{A6A5BB0F-4171-4D08-BD50-CA8E13A19E9A}']
    function GetAddressList: IObjectListType;
    function GetCredit: Currency;
    function GetIsActive: boolean;
    function GetLastUpdate: TDateTime;
    procedure SetCredit(const Value: Currency);
    procedure SetIsActive(const Value: boolean);
    procedure SetLastUpdate(const Value: TDateTime);

    property AddressList: IObjectListType read GetAddressList;
    property Credit: Currency read GetCredit write SetCredit;
    property IsActive: boolean read GetIsActive write SetIsActive;
    property LastUpdate: TDateTime read GetLastUpdate write SetLastUpdate;
  end;

implementation

end.
