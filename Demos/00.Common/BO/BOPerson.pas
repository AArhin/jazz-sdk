unit BOPerson;

interface

uses
  (* delphi *)
  Classes,
  (* jazz *)
  JazzValueTypeIntf, JazzValueType, JazzTypeInfo,
  (* application *)
  BOPersonIntf;

type
  TPerson = class(TObjectType, IPerson)
  private
    FID: IStringType;
    FName: IStringType;
    FBirthDate: IDateType;
    FDocument: IStringType;
    FPicture: IBlobType;
  protected
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
  public
    procedure InitInstance; override;
  end;

implementation

function TPerson.GetDocument: string;
begin
 Result:= FDocument.Value;
end;

function TPerson.GetID: string;
begin
  Result:= FID.Value;
end;

function TPerson.GetName: string;
begin
  Result:= FName.Value;
end;

function TPerson.GetPicture: TStream;
begin
  Result:= FPicture.Value;
end;

procedure TPerson.SetDocument(const Value: string);
begin
  FDocument.Value:= Value;
end;

procedure TPerson.SetID(const Value: string);
begin
  FID.Value:= Value;
end;

procedure TPerson.SetName(const Value: string);
begin
  FName.Value:= Value
end;

procedure TPerson.SetPicture(const Value: TStream);
begin
  FPicture.Value:= Value;
end;

function TPerson.GetBirthDate: TDateTime;
begin
  Result:= FBirthDate.Value;
end;

procedure TPerson.SetBirthDate(const Value: TDateTime);
begin
  FBirthDate.Value:= Value;
end;

procedure TPerson.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType);
  AddMember(FName, 'Name', TStringType);
  AddMember(FBirthDate, 'BirthDate', TDateType);
  AddMember(FDocument, 'Document', TStringType);
  AddMember(FPicture, 'Picture', TBlobType);
end;

end.


