unit AddressManager;

interface

uses Classes, ObjectLinkManager, JazzValueTypeIntf, JazzSessionIntf,
  PersonManager;

type
  TAddressManager = class(TObjectManager)
  private
    FOwner: Pointer;
  protected
    procedure CreateColumns; override;
    procedure LinkComponent(const Component: TComponent); override;
    procedure Populate(const Count: Integer); override;
    procedure PopulateDetail(const Owner: IObjectType; const Count: Integer);
  public
    constructor Create(const AOwner: IObjectManager; const Session: ISession;
      const ClassToManager: TClass); reintroduce; virtual;
  end;

  TPersonWithAddressManager = class(TPersonManager)
  protected
    procedure Populate(const Count: Integer); override;
  end;

implementation

uses
  RandomData, BOAddressIntf, BOPersonWithAddressIntf, JazzUtils,
  ComCtrls, ViewEditores;

  { TAddressManager }

constructor TAddressManager.Create(const AOwner: IObjectManager;
  const Session: ISession; const ClassToManager: TClass);
begin
  inherited Create(Session, ClassToManager);
  FOwner:= Pointer(AOwner);
  AOwner.PopulateDetail:= Self.PopulateDetail;
end;

procedure TAddressManager.CreateColumns;
begin
  inherited;
  with ListViewObserver do
  begin
    Clear;
    if CreateIDColumns then
      Add('ID', 'Identification', 300);

    Add('Address1', 'Address1', 180);
    Add('Address2', 'Address2', 100);
    Add('City', 'City', 100);
    if CreateIDColumns then
      Add('IDOwner', 'Owner Identification', 300);

    if CreateDebugColumns then
    begin
      Add('Loaded', 'Loaded', DebugColumnWidth);
      Add('Persisted', 'Persisted', DebugColumnWidth);
      Add('Modified', 'Modified', DebugColumnWidth);
      Add('Deleting', 'Deleting', DebugColumnWidth);
      Add('Deleted', 'Deleted', DebugColumnWidth);
    end;
  end;
end;

procedure TAddressManager.LinkComponent(const Component: TComponent);
begin
  inherited;
  if Component is TListView then
  begin
    IObjectManager(FOwner).ListViewObserver.AddDetail(Self.ListViewObserver);
    CreateColumns;    
  end;
end;

procedure TAddressManager.Populate(const Count: Integer);
begin
  PopulateDetail(nil, Count);
end;

procedure TAddressManager.PopulateDetail(const Owner: IObjectType;
  const Count: Integer);
var
  I: Integer;
  LAddress: IAddress;
  LObjectList: IObjectListType;
begin
  if Owner = nil then Exit;
  // Populate ObjectList
  // ValueObjectList.BeginUpdate prevent visual notifications
  LObjectList:= (Owner as IPersonWithAddress).AddressList as IObjectListType;

  LObjectList.BeginUpdate;
  try
    for I:= 1 to Count do
    begin
      LAddress:= LObjectList.Add as IAddress;

      LAddress.ID:= GenerateGUID;
      LAddress.Address1:= RandomStreet;
      LAddress.Address2:= RandomStreet;
      LAddress.City:= RandomCity;
      LAddress.AddressState:= RandomState;
      LAddress.Country:= 'Brazil'; 
      if Owner <> nil then
        LAddress.IDOwner:= (Owner as IPersonWithAddress).ID;
    end;
  finally
    LObjectList.EndUpdate;
  end;
end;


{ TPersonWithAddressManager }

procedure TPersonWithAddressManager.Populate(const Count: Integer);
var
  I: Integer;
  LPerson: IPersonWithAddress;
begin
  // Populate ObjectList
  // ValueObjectList.BeginUpdate prevent visual notifications
  ObjectList.BeginUpdate;
  try
    for I:= 1 to Count do
    begin
      LPerson:= ObjectList.Add as IPersonWithAddress;

//      LPerson.ID:= GenerateGUID;
      LPerson.Name:= RandomPersonName;
      LPerson.Document:= RandomNumber(11);
      if Assigned(FPopulateDetail) then
      begin
        Randomize;
        FPopulateDetail(LPerson, Random(10));
      end;
    end;
  finally
    ObjectList.EndUpdate;
  end;
end;

end.
