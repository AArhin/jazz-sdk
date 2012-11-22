unit BOPersonIntf;

interface

uses Classes, JazzValueTypeIntf;

type
  IPerson = interface(IObjectType)
    ['{19DC87CD-584E-4F15-A191-D05E8FB2F6D3}']
    function GetBirthDate: TDateTime;
    function GetDocument: string;
    function GetID: string;
    function GetName: string;
    function GetPicture: TStream;
    procedure SetBirthDate(const Value: TDateTime);
    procedure SetDocument(const Value: string);
    procedure SetID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPicture(const Value: TStream);

    property ID: string read GetID write SetID;
    property Name: string read GetName write SetName;
    property BirthDate: TDateTime read GetBirthDate write SetBirthDate;
    property Document: string read GetDocument write SetDocument;
    property Picture: TStream read GetPicture write SetPicture;
  end;

implementation

end.
