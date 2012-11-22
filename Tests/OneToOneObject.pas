unit OneToOneObject;

interface

uses JazzValueType,
  JazzValueTypeIntf,
  JazzMapping,
  JazzMappingIntf,

  OneToOneObjectIntf;

type
  TOneToOneObject = class(TObjectType, IOneToOneObject)
  private
    FID: IStringType;
    FIDOwner: IStringType;
  protected
    function GetID: string;
    function GetIDOwner: string;
    procedure SetID(const Value: string);
    procedure SetIDOwner(const Value: string);
  public
    procedure InitInstance; override;
    property ID: string read GetID write SetID;
    property IDOwner: string read GetIDOwner write SetIDOwner;
  end;

  TOneToOneMapping = class(TMappingLoader)
  public
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

function TOneToOneObject.GetID: string;
begin
  Result := FID.Value;
end;

function TOneToOneObject.GetIDOwner: string;
begin
  Result := FIDOwner.Value;
end;

procedure TOneToOneObject.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType);
  AddMember(FIDOwner, 'IDOwner', TStringType);
end;

procedure TOneToOneObject.SetID(const Value: string);
begin
  FID.Value := Value;
end;

procedure TOneToOneObject.SetIDOwner(const Value: string);
begin
  FIDOwner.Value := Value;
end;

procedure TOneToOneMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TOneToOne', 'OneToOne') do
    begin
      with Add('ID', 'ID') do
      begin
        IsOid := True;
        ColumnType := itString;
      end;
      Add('IDOwner', 'IDOwner', itString, 38);
    end;
  end;
end;

initialization
  RegisterMapping(TOneToOneMapping);

end.

